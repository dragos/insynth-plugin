package ch.epfl.insynth.typetransformations

import scala.tools.nsc.interactive.Global
import ch.epfl.scala.trees.{Const => ScalaConst, Method => ScalaMethod, Function => ScalaFunction, Instance => ScalaInstance, ScalaType}
import ch.epfl.insynth.InSynth

trait TExtractor{
  self:InSynth =>

  import compiler._  
    
object ScalaTypeExtractor {
    
  def getLocalType(tpe:Type):Option[ScalaType] = {
    assert(tpe != null)
    try{
      Some(traverse(tpe))
    } catch {
      case ex =>
        None
    }
  }

  def getType(tpe:Type):Option[ScalaType] = getType(null, tpe)
  
  def getType(receiverType:Type, tpe:Type):Option[ScalaType] = {
    assert(tpe != null)
    try {
      Some(ScalaMethod(if (receiverType != null) 
        traverse(receiverType) else null, 
        getParamList(tpe), 
        getReturnType(tpe)))
    } catch {
      case ex =>
        None
    }
  }   
  
  private def isRepeated(sym:Symbol) = sym.tpe.typeSymbol.fullName == "scala.<repeated>"  
  
  //TODO: What to do with implicit params and type* ?
  private def getParamList(tpe:Type): List[List[ScalaType]] = {
    val paramss = tpe.paramss
    paramss.map(params => params.filterNot(param => param.isImplicit || isRepeated(param)).map(param => traverse(param.tpe)))
  }
  
  private def onlyReturnType(rawReturn:Type):Type = rawReturn match {
    case MethodType(_, resultType) => onlyReturnType(resultType)
    case t => t
  }
  
  private def getReturnType(tpe:Type): ScalaType = {
    val returnType = onlyReturnType(tpe.resultType)
      
    traverse(returnType)
  }

  private def traverse(tpe:Type):ScalaType = {
    tpe match {
	  //Polymorphic
	  case PolyType(typeParams: List[Symbol], resultType: Type) =>
        traverse(resultType)
	    
      //Function type
	  case TypeRef(pre: Type, sym: Symbol, args: List[Type])
	    if(definitions.isFunctionType(tpe)) =>
	    val list = args.init.map(traverse)
	    val result = traverse(args.last)
	      
	    ScalaFunction(list, result)
	      
	  //TODO: => Type  
	  /*case TypeRef(pre: Type, sym: Symbol, args: List[Type])
	    if (!sym.isMonomorphicType && args.length == 1 && sym.fullName == "scala.<byname>")=>
	      traverse(args(0))
	  */
	    
	  //Polymorphic instantiated types
	  case TypeRef(pre: Type, sym: Symbol, args: List[Type])
	    if (!sym.isMonomorphicType && !args.isEmpty)=>
	      ScalaInstance(SugarFree(sym.fullName), args.map(traverse))
	     
	  //Base types
	  case TypeRef(pre: Type, sym: Symbol, args: List[Type]) =>
	    if (!sym.isTypeParameter) ScalaConst(SugarFree(sym.fullName))	      
	      else throw new Exception("<<Parametrized types not supported: "+tpe.getClass.getName+">>")
	    
	  case _ => throw new Exception("<<Not supported: "+tpe.getClass.getName+">>") 
    }
  }  
}
  
  object SugarFree {
    private val map = Map("scala.Predef.String"        -> "java.lang.String",
                          "scala.Predef.Set"           -> collection.immutable.Set.getClass.getName.replace("$",""),
                          "scala.Predef.Map"           -> collection.immutable.Map.getClass.getName.replace("$",""),
                          "scala.Predef.Manifest"      -> scala.reflect.Manifest.getClass.getName.replace("$",""),
                          "scala.Predef.ClassManifest" -> scala.reflect.ClassManifest.getClass.getName.replace("$",""),
                          "scala.Predef.Pair"          -> scala.Tuple2.getClass.getName.replace("$",""),
                          "scala.Predef.Triple"        -> scala.Tuple3.getClass.getName.replace("$",""),
                          "scala.Predef.Class"         -> "java.lang.Class")

    def apply(name:String):String = {
      if (map.contains(name)) map(name)
      else name
    }
  }
    
  
  
}