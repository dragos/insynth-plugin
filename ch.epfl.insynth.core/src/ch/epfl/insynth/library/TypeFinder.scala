package ch.epfl.insynth.library

import scala.tools.nsc.interactive.Global
import ch.epfl.insynth.library.definitions._

trait TypeFinders extends Definitions { 
  self:ISynth =>
    
  import compiler._
 
  class TypeFinder(typeSystem:CompositeTypeSystem) {

    private var currentVariables:List[String] = null

    private var makeVarConst:Symbol => Term = null

    private def setCurrentVariables(currentVariables:List[String]){this.currentVariables = currentVariables}

    private def isVariable(variableName:String):Boolean = currentVariables.exists(variableName.equals)

    def getTypeSystem = this.typeSystem

    def setUserDefAbstType(ud:UserDefinition){
      val tpe = Const(this.typeSystem.abstConstant(SugarFree(ud.realTypeName)))
      if(ud.isString){
	tpe.setSubtypes(List(this.typeSystem.abstConstant("scala.Any")))
      }
      ud.setAbstType(tpe)
    }

    def setQueryAndLocalAbstType(definition:ClauseDefinition):Boolean = {
      //a method called inside findType
      //we set it to "makeGroundVarConst" when variabels need to be ground 
      makeVarConst = makeGroundVarConst

      this.typeSystem.pushVariableRenamer()
      this.setCurrentVariables(definition.realPolys)

      findType(definition.realType) match {
	case Some(defType) => 
	  definition.setAbstType(defType) 
	  definition.setVariableRenamer(this.typeSystem.topVariableRenamer)
	true
	case _ => {
	  this.typeSystem.popVariableRenamer()
	  false
	}
      }
    }

    def setAbstType(definition:ClauseDefinition):Boolean = {
      //a method called inside findType
      //we set it to "makeNonGroundVarConst" when variabels need to be non-ground
      makeVarConst = makeNonGroundVarConst

      //this.typeSystem.pushVariableRenamer()
      this.typeSystem.pushVariableRenamer(definition.realPolys)
      this.setCurrentVariables(definition.realPolys)

      if (definition.needReceiver) {
	val receiver = definition.receiver
	val receiverType = findType(receiver.realType)
	if (receiverType.isDefined){
	  findType(definition.realType) match {
	    case Some(defType) => 
	      definition.setAbstType(Arrow(receiverType.get, defType))
	      definition.setVariableRenamer(this.typeSystem.topVariableRenamer)
	      true
	    case _ => {
	      this.typeSystem.popVariableRenamer()
	      false
	    }
	  }
	}
	else {
	  this.typeSystem.popVariableRenamer()
	  false
	}

      } else {
	findType(definition.realType) match {
	  case Some(defType) => 
	    definition.setAbstType(defType) 
	    definition.setVariableRenamer(this.typeSystem.topVariableRenamer)
	    true
	  case _ => {
	    this.typeSystem.popVariableRenamer()
	    false
	  }
	}
      }
    }

    def setInheritanceFunction(definition:InheritanceInfo):Boolean = {
      makeVarConst = makeInhNonGroundVarConst
      this.typeSystem.pushVariableRenamer(definition.realPolys)
      this.setCurrentVariables(definition.realPolys)

      val receiverType = findType(definition.getSubType)
      if (receiverType.isDefined){
	findType(definition.getSuperType) match {
	  case Some(defType) => 
	    definition.setAbstType(Arrow(receiverType.get, defType))
	    definition.setVariableRenamer(this.typeSystem.topVariableRenamer)
	    true
	  case _ => {
	    this.typeSystem.popVariableRenamer()
	    false
	  }
	}
      }
      else {
	this.typeSystem.popVariableRenamer()
	false
      }
    }

    private def findType(tpe:Type):Option[Term] = {
      try{
	Some(traverse(tpe))
      } catch {
	case _ => None
      }
    }

    protected def traverse(tpe:Type):Term = {
      def makeArrow(args:List[Type]):Term = args match {
	case Nil => throw new Exception("Impossible case in \"makeArrow\"!")
	case head :: Nil => traverse(head)
	case head :: tail => Arrow(traverse(head), makeArrow(tail))
      }

      def isFunctionLiteral(sym:Symbol) = {
	val nameBeginsWith = scala.Function.getClass.getName.replace("$", "")
	val name = sym.fullName
	name.startsWith(nameBeginsWith) && isDigit(name.drop(nameBeginsWith.length))
      }

      def isDigit(str:String) = 
	try {
	  str.toInt
	  true
	} catch {
	  case _ : java.lang.NumberFormatException => false 
	}

      def isRepeated(sym:Symbol) = sym.tpe.typeSymbol.fullName == "scala.<repeated>"

      def isImplicit(sym:Symbol) = sym.isImplicit

     //TODO: add support for "def foo{}" <--- Nullable method. 
      
      tpe match {

	//Method
	case MethodType(params: List[Symbol], resultType: Type) =>
	  val newParams = params.filterNot(x => isRepeated(x) || isImplicit(x))
	  makeArrow(newParams.map(_.tpe) ::: List(resultType))
	
	//Polymorphic
	case PolyType(typeParams: List[Symbol], resultType: Type) =>
	  traverse(resultType)

	//Function type
	//(Int, Boolean) => Int
	case TypeRef(pre: Type, sym: Symbol, args: List[Type])
	  if (isFunctionLiteral(sym) && !args.isEmpty) =>
	  makeArrow(args)

	//Polymorphic instantiated types
	//TODO: Make a better check, this one is ugly
	case TypeRef(pre: Type, sym: Symbol, args: List[Type])
	  if (!sym.isMonomorphicType && !args.isEmpty)=>
	  val typList = for (arg <- args) yield traverse(arg)
	  Instance(this.typeSystem.abstInstance(SugarFree(sym.fullName)),typList)

	//Base types
	//TODO: Somehow "=> Any" falls here, check this
	case TypeRef(pre: Type, sym: Symbol, args: List[Type]) =>
	  makeVarConst(sym)

	case _ => throw new Exception("Type still not suported. "+tpe.getClass.getName)
      }
    }

    private def makeInhNonGroundVarConst(symbol:Symbol):Term = {
      val name = symbol.fullName
      if (isVariable(name)){
	Variable(this.typeSystem.abstVariable(name))
      } else {
	Const(this.typeSystem.abstConstant(SugarFree(name)))
      }
    }


    private def makeNonGroundVarConst(symbol:Symbol):Term = {
      val name = symbol.fullName
      if (isVariable(name)){
	Variable(this.typeSystem.abstVariable(name))
      } else {
	//Const(this.typeSystem.abstConstant(SugarFree(name)))
	val newName = this.typeSystem.abstConstant(SugarFree(name))

	val const = Const(newName)
	const.setSubtypes(findSuperClasses(symbol, 2))
	const
      }
    }

    private def makeGroundVarConst(symbol:Symbol):Term = {
      val name = symbol.fullName

      if (isVariable(name)){
	Const(this.typeSystem.abstGroundVariable(name))
      } else {
//	Const(this.typeSystem.abstConstant(SugarFree(name)))

	val newName = this.typeSystem.abstConstant(SugarFree(name))

	val const = Const(newName)
	const.setSubtypes(findSuperClasses(symbol, 2))
	const
      }
    }

    private def findSuperClasses(symbol:Symbol, depth:Int):List[String] = {
      val supertypes = symbol.tpe.parents.map(x => x.typeSymbol)
      var result = List[String]()
      supertypes.foreach{
	sClass => 
	  if(depth > 0 && sClass != null && sClass.isMonomorphicType){
	    result ++= this.typeSystem.abstConstant(SugarFree(sClass.fullName)) :: findSuperClasses(sClass, depth-1)
	  }
      }
      result
    }
  }

  object SugarFree {
    private val map = Map("scala.Predef.String" -> "java.lang.String", "scala.Predef.Set" -> collection.immutable.Set.getClass.getName.replace("$",""), "scala.Predef.Map" -> collection.immutable.Map.getClass.getName.replace("$",""), "scala.Predef.Manifest" -> scala.reflect.Manifest.getClass.getName.replace("$",""),"scala.Predef.ClassManifest" -> scala.reflect.ClassManifest.getClass.getName.replace("$",""), "scala.Predef.Pair" -> scala.Tuple2.getClass.getName.replace("$",""), "scala.Predef.Triple" -> scala.Tuple3.getClass.getName.replace("$",""), "scala.Predef.Class" -> "java.lang.Class")

    def apply(name:String):String = {
      if (map.contains(name)) map(name)
      else name
    }
  }


  class InheritanceTypeMaker(typeSystem:CompositeTypeSystem) {

    private var currentVariables:List[String] = null

    private def setCurrentVariables(currentVariables:List[String]){this.currentVariables = currentVariables}

    private def isVariable(variableName:String):Boolean = currentVariables.exists(variableName.equals)

    def getTypeSystem = this.typeSystem

    def setAbstType(definition:InheritanceInfo):Boolean = {

      this.typeSystem.pushVariableRenamer(definition.realPolys)
      this.setCurrentVariables(definition.realPolys)

      val receiverType = findType(definition.getSuperType)
      if (receiverType.isDefined){
	findType(definition.getSubType) match {
	  case Some(defType) => 
	    definition.setAbstType(Arrow(receiverType.get, defType))
	    definition.setVariableRenamer(this.typeSystem.topVariableRenamer)
	    true
	  case _ => {
	    this.typeSystem.popVariableRenamer()
	    false
	  }
	}
      }
      else {
	this.typeSystem.popVariableRenamer()
	false
      }
    }

    private def findType(tpe:Type):Option[Term] = {
      try{
	Some(traverse(tpe))
      } catch {
	case _ => None
      }
    }

    protected def traverse(tpe:Type):Term = {
      def makeArrow(args:List[Type]):Term = args match {
	case Nil => throw new Exception("Impossible case in \"makeArrow\"!")
	case head :: Nil => traverse(head)
	case head :: tail => Arrow(traverse(head), makeArrow(tail))
      }

      def isFunctionLiteral(sym:Symbol) = {
	val nameBeginsWith = scala.Function.getClass.getName.replace("$", "")
	val name = sym.fullName
	name.startsWith(nameBeginsWith) && isDigit(name.drop(nameBeginsWith.length))
      }

      def isDigit(str:String) = 
	try {
	  str.toInt
	  true
	} catch {
	  case _ : java.lang.NumberFormatException => false 
	}

      def isRepeated(sym:Symbol) = sym.tpe.typeSymbol.fullName == "scala.<repeated>"

      def isImplicit(sym:Symbol) = sym.isImplicit

      tpe match {

	//Method
	case MethodType(params: List[Symbol], resultType: Type) =>
	  val newParams = params.filterNot(x => isRepeated(x) || isImplicit(x))
	  makeArrow(newParams.map(_.tpe) ::: List(resultType))
	
	//Polymorphic
	case PolyType(typeParams: List[Symbol], resultType: Type) =>
	  traverse(resultType)

	//Function type
	//(Int, Boolean) => Int
	case TypeRef(pre: Type, sym: Symbol, args: List[Type])
	  if (isFunctionLiteral(sym) && !args.isEmpty) =>
	  makeArrow(args)

	//Polymorphic instantiated types
	//TODO: Make a better check, this one is ugly
	case TypeRef(pre: Type, sym: Symbol, args: List[Type])
	  if (!sym.isMonomorphicType && !args.isEmpty)=>
	  val typList = for (arg <- args) yield traverse(arg)
	  Instance(this.typeSystem.abstInstance(SugarFree(sym.fullName)),typList)

	//Base types
	//TODO: Somehow "=> Any" falls here, check this
	case TypeRef(pre: Type, sym: Symbol, args: List[Type]) =>
	  makeNonGroundVarConst(sym)

	case _ => throw new Exception("Type still not suported. "+tpe.getClass.getName)
      }
    }

    private def makeNonGroundVarConst(symbol:Symbol):Term = {
      val name = symbol.fullName
      if (isVariable(name)){
	Variable(this.typeSystem.abstVariable(name))
      } else {
	Const(this.typeSystem.abstConstant(SugarFree(name)))
      }
    }

  }

}
