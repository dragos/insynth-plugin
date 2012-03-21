package ch.epfl.insynth.library.definitions

import scala.tools.nsc.interactive.Global
import ch.epfl.insynth.library.prover.trees._
import ch.epfl.insynth.library.types._

import scala.collection.mutable.Queue
import ch.epfl.insynth.library.ISynth

trait Definitions extends Types with Trees {
  self: ISynth =>
    
  import compiler._
 
    
    
  class Clause(val fsym: List[String],  val level:Int, val action:Action, val definition:ClauseDefinition, val query:Boolean, val term:Term){

    private var createdTime:Int = 0
    private var resolvedTime:Int = -1
    private var innerWeight:Weight = null  

    //Initial clauses
    def this(fsym:List[String], definition:ClauseDefinition, query:Boolean, term:Term) = this(fsym, 0, null, definition, query,term)

    //Non-initial clauses
    def this(fsym:List[String], level:Int, action:Action, query:Boolean, term:Term) = this(fsym, level, action, null, query, term)

    //Empty clause
    def this(fsym:List[String],  action:Action) = this(fsym, -1, action, null, false, null)

    def created_=(time:Int){this.createdTime = time}
    def created = createdTime
    def resolved_=(time:Int){this.resolvedTime = time}
    def resolved = resolvedTime

    def weight_=(weight1:Weight){this.innerWeight = weight1}
    def weight = innerWeight

    //Only initial clauses have a function
    def fun():DefFunction = if (definition != null) definition.function else null

    def isQuery = query

    override def toString = fsym +" : "+term

    //Debugging -----------------------------------------------------------------------------------------//

    var chosen:Long = 0

    def incChosen(){chosen+=1}
    def getChosen = chosen
    
    //Debugging -----------------------------------------------------------------------------------------//
  }

  /*
   *
   * The abstract meta-class that represents a definition: a class, a object, a method, a field, a local val or var.
   *
   */
  abstract class Definition(
    protected val symbol:Symbol,
    protected val parent:Definition
  ){

    def this(symbol:Symbol) = this(symbol, null)
    
    def getParent = this.parent

    /*
     *
     * Scala AST type. No receiver type attached.
     * Used by Type Finder.
     *
     */
    def realType:Type = this.symbol.tpe

    /*
     * 
     * Returns full realName. In the case of locals, it attaches a word "local" in order to avoid ambiguity. 
     * 
     */
    def realName:String

    /*
     * 
     * Returns a name with a receiver attached. 
     * Used by a Reconstructor.
     * 
     */
    def prettyName:String 

    def realPolys:List[String] 

  }

  /*
   *
   * The meta-class that represents a class or an object.
   *
   */
  class ClassInfo(symbol:Symbol) extends Definition(symbol) {
    /*
     * 
     * Returns true if the symbol represents an object.
     * 
     */
    def isObject = this.symbol.isModule

    /*
     * 
     * Returns true if the symbol represents an object.
     * 
     */
    def isClass = this.symbol.isClass

    def realPolys:List[String] = this.symbol.tpe.typeArgs.map(x => x.typeSymbol.fullName)

    def realName = "class "+symbol.fullName

    def prettyName = this.symbol.simpleName.toString.replace(" ","")

    def decls = this.symbol.tpe.decls.toList

    def nameString = this.symbol.nameString

    def isTuple = this.symbol.fullName.startsWith(scala.Tuple1.getClass.getName.replace("1$",""))

    def fullName = symbol.fullName

    def hasTest = this.symbol.tpe.decls.exists(x => x.isPublic && x.isMethod && x.tpe.paramTypes.isEmpty && x.simpleName.toString().equals("test"))
  }

  /*
   *
   * The abstract meta-class that represents a definition that can be wrapped in a clause.
   * We pass the clause to a prover.
   *
   */
  abstract class ClauseDefinition(
    symbol:Symbol, 
    parent:Definition, 
    isExternal:Boolean,  //All defs that are not in the "top Class or Object"
    needThis:Boolean   //Only defs that belong to the "top Class or Object" can have this
  ) extends Definition(symbol, parent) {

    def this(symbol:Symbol, parent:Definition) = this(symbol, parent, false, false)

    def this(parent:Definition) = this(null, parent)

    def this() = this(null)

    protected var abstractType:Term = null

    /*
     * TODO: Check if we really need this.
     * The name that "realName" maps to. Used by Prover and WeightsManager. 
     *
     */ 
    private var abstractName:String = null

    /*
     *
     *
     *
     */
    protected var variablesRenamer:Renamer[String, String] = null

    /*
     * 
     * This will be set by TypeFinder.
     *
     * 
     */
    def setVariableRenamer(variablesRenamer:Renamer[String, String]) {this.variablesRenamer = variablesRenamer}

    /*
     *
     * Receiver is an object that helps us to access the definition. We do not need it if a definition is local, if it can be accessed by "this". 
     * 
     */
    def needReceiver:Boolean = this.isExternal && this.parent.asInstanceOf[ClassInfo].isClass && !this.symbol.isConstructor

    /*
     *
     * Mostly the receiver is the parent. If not, one can override this method.
     *
     */
    def receiver:Definition = if (needReceiver) parent else null

    /*
     *
     * ISynth type. If a receiver exists it will be attached.
     * Used by Prover.
     *
     */
    def abstType:Term = abstractType //r=2
    //this.abstractType

    /*
     *
     * Type Finder sets ISynth type. ISynth type. If a receiver exists it will be attached.
     * Set by TypeFinder.
     * 
     */
    def setAbstType(abstractType:Term){this.abstractType = abstractType}
    
    /*
     *
     * Set by Loader.
     *
     */
    def setAbstName(abstractName:String){this.abstractName = abstractName}

    /*
     * 
     * Returns a full abstract name.
     * Used by WeightsManager and Prover.
     * 
     */
    def abstName:String = this.abstractName

    /*
     *
     * The method that creates a clause. 
     * We associate ClauseDefinition to its Clause. 
     *
     */
    def clause:Clause = new Clause(List[String](this.abstName), this, isQuery, this.abstType)

    /*
     * TODO: Check if we need to move this to Clause class. 
     * Creates a function that helps us to reconstruct an expression from a proof. 
     * 
     */
    def function:DefFunction = new DefFunction(this)

    def prettyName:String 

    def abstPolys:List[String]

    /*
     *
     * Used by Reconstructor to set children in a function.
     *
     */ 
    def length:Int

    def isQuery:Boolean

    def printArgs(types:Map[String, String], s:StringBuffer)(args: => Unit):Unit  

    override def toString = prettyName

    def isTupleConstructor:Boolean

    def isApply:Boolean = false

     def isInvisible = false
  }

  /*
   *
   * The meta-class that represents a method.
   *
   */
  class MethodInfo(
    symbol:Symbol,
    clazz:ClassInfo,
    isExternal:Boolean,
    needThis:Boolean
  ) extends ClauseDefinition(symbol, clazz, isExternal, needThis){

    private val params:List[Type] = this.symbol.tpe.paramTypes // or symbol.tpe.paramss.reduceLeft(_++_)
    private val retType:Type = this.symbol.tpe.resultType

    private val polys:List[Symbol] = this.symbol.tpe.typeParams

    /*
     *
     * Names of polymorphic params.
     * Used by TypeFinder and children Definitions.
     *
     */
    def realPolys:List[String] = (if (this.polys != null) this.polys.map(x => x.fullName) else List[String]()) ++ (if (this.parent != null) this.parent.realPolys else Nil)

    /*
     *
     * Returns polys of this definition.
     * If it is a constructor returns only its parent polys.
     *
     */
    def abstPolys:List[String] = {
      if (this.variablesRenamer != null && this.polys != null)
	if (this.isConstructor && !this.isObjectMethod){
	  this.parent.realPolys.map(x => this.variablesRenamer.abstName(x))
	} else this.polys.map(x => this.variablesRenamer.abstName(x.fullName))
      else
	List[String]()
    }

    //TODO
    private def lastParamRepeats:Boolean = if (!this.params.isEmpty) this.params.last.typeSymbol.fullName == MethodInfo.repeatTypeName else false

    /*
     * TODO: What to do with overloaded methods? 
     * Returns full realName. In the case of locals, it attaches a word "local" in order to avoid ambiguity. 
     * 
     */
    //TODO: we should traverse "params" instead of asking for their symbol's fullname
    def realName:String = "method " + symbol.fullName + params.map(x => x.typeSymbol.fullName).mkString(" ",",","")

    /*
     * 
     * Returns a name with a receiver attached. 
     * Used by a Reconstructor.
     * 
     */
    def prettyName:String = {
      if (this.isExternal){
	if (this.isConstructor){
	  if (!this.isTupleConstructor) "new " + this.parent.prettyName
	  else ""
	} else 
	    if (this.isObjectMethod) {
	      if (this.isApply) this.parent.prettyName
	      else this.parent.prettyName + "." + this.symbol.simpleName.toString.replace(" ","")
	    } else {
	      if (!this.isApply) this.symbol.simpleName.toString.replace(" ","")
	      else ""
	    }
      } else {
	if(this.isConstructor) "new " + this.parent.prettyName
	  else 
	    if (this.isApply) "this"
	      else if (this.needThis) "this." + this.symbol.simpleName.toString.replace(" ","")
		   else this.symbol.simpleName.toString.replace(" ","")
      }
    }

    private def isConstructor = this.symbol.isConstructor

    override def isApply = this.symbol.simpleName.toString.equals(MethodInfo.applyMethodName)

    private def isObjectMethod = this.parent.asInstanceOf[ClassInfo].isObject
 
    def isTupleConstructor = this.isConstructor && this.parent.asInstanceOf[ClassInfo].isTuple 
 
    /*
     *
     * Used by Reconstructor to set children in a function.
     *
     */ 
    def length:Int = this.params.length + (if (this.needReceiver) 1 else 0) + (if (this.lastParamRepeats) -1 else 0) 

    def printArgs(types:Map[String, String], s:StringBuffer)(print: => Unit){
      if (this.needParentheses){
	s.append("(")
	print
	s.append(")")
      }
    }

    def isDollarName = prettyName.contains("$")

    def isQuery:Boolean = false    

    private def needParentheses:Boolean = this.symbol.tpe.paramSectionCount != 0
  }

  object MethodInfo {
    final val repeatTypeName = "scala.<repeated>" 
    final val applyMethodName = "apply" 
  }

  /*
   *
   * The meta-class that represents a field or a class parameters.
   *
   */
  class FieldInfo(
    symbol:Symbol,
    clazz:ClassInfo,
    isExternal:Boolean,
    needThis:Boolean
  ) extends ClauseDefinition(symbol, clazz, isExternal, needThis){

    /*
     * TODO: What to do with overloaded methods? 
     * Returns full realName. In the case of locals, it attaches a word "local" in order to avoid ambiguity. 
     * 
     */
    def realName:String = "field " + this.symbol.fullName+" "

    /*
     * 
     * Returns a name with a receiver attached. 
     * Used by a Reconstructor.
     * 
     */
    def prettyName:String = {
      if (this.isExternal){
	if (this.isObjectField) this.parent.prettyName + "." + this.symbol.simpleName.toString.replace(" ","")
	    else this.symbol.simpleName.toString.replace(" ","")
      } else {
	if (this.needThis) "this." + this.symbol.simpleName.toString.replace(" ","")
	    else this.symbol.simpleName.toString.replace(" ","")
      }
    }

    private def isObjectField = this.parent.asInstanceOf[ClassInfo].isObject

    def realPolys:List[String] = if (parent != null) parent.realPolys else Nil

    /*
     *
     * Returns only polys of this definition, not its parent polys.
     *
     */
    def abstPolys:List[String] = List[String]()
 
    /*
     *
     * Used by Reconstructor to set children in a function.
     *
     */ 
    def length:Int = if (this.needReceiver) 1 else 0

    def printArgs(types:Map[String, String], s:StringBuffer)(print: => Unit){
      print
    }

    def isQuery:Boolean = false

    def isTupleConstructor = false
  }

  /*
   *
   * The meta-class that represents a local val or var, a method parameters.
   *
   */  
  class LocalInfo(
    symbol:Symbol,
    parent:ClauseDefinition
  ) extends ClauseDefinition(symbol, parent) {

    /*
     * 
     * Returns full realName. In the case of locals, it attaches a word "local" in order to avoid ambiguity. 
     * 
     */
    def realName:String = "local " + this.symbol.fullName

    /*
     * 
     * Returns a name with a receiver attached. 
     * Used by a Reconstructor.
     * 
     */
    def prettyName:String = this.symbol.simpleName.toString.replace(" ","")

    def realPolys:List[String] = Nil//if (parent != null) parent.realPolys else Nil

    /*
     *
     * Returns only polys of this definition, not its parent polys.
     *
     */
    def abstPolys:List[String] = List[String]()
 
    /*
     *
     * Used by Reconstructor to set children in a function.
     *
     */ 
    def length:Int = 0 

    def printArgs(types:Map[String, String], s:StringBuffer)(print: => Unit){
      print
    }

    def isQuery:Boolean = false

    def isTupleConstructor = false
  }

  class QueryInfo(tpe:Type, parent:Definition) extends ClauseDefinition(parent) {

    /*
     *
     * Scala AST type. No receiver type attached.
     * Used by Type Finder.
     *
     */
    override def realType:Type = tpe

    /*
     * 
     * Returns full realName. In the case of locals, it attaches a word "local" in order to avoid ambiguity. 
     * 
     */
    def realName:String = QueryInfo.REAL_NAME

    /*
     * 
     * Returns a name with a receiver attached. 
     * Used by a Reconstructor.
     * 
     */
    def prettyName:String = QueryInfo.PRETTY_NAME

    def realPolys:List[String] = Nil//if (parent != null) parent.realPolys else Nil

    /*
     *
     * Returns only polys of this definition, not its parent polys.
     *
     */
    def abstPolys:List[String] = List[String]()

    /*
     *
     * Used by Reconstructor to set children in a function.
     *
     */ 
    def length:Int = 1 

    def printArgs(types:Map[String, String], s:StringBuffer)(print: => Unit){
      print
    }

    def isQuery:Boolean = true

    def isTupleConstructor = false
  }

  class InheritanceInfo(subtype:Type, supertype:Type) extends ClauseDefinition(null) {
    def getSuperType = supertype 
    def getSubType = subtype

    //has realPolys
    //has abstPolys
    //has abstractType
    //has variableRenamers
    //has Clause
    //has DefFunction
    //has abstractName; for weights
    //has length = 1

    //no parent
    //no real type
    //no real name
    //no prettyName
    //noReceiver

    private var polys:List[Symbol] = this.subtype.typeArgs.map(x => x.typeSymbol)

    def realPolys:List[String] = this.polys.map(x => x.fullName)

    /*
     *
     * Returns only polys of this definition, not its parent polys.
     *
     */
    def abstPolys:List[String] = if (this.variablesRenamer != null && this.polys != null) this.polys.map(x => this.variablesRenamer.abstName(x.fullName)) else List[String]()

    /*
     *
     * Used by Reconstructor to set children in a function.
     *
     */ 
    def length:Int = 1

    def printArgs(types:Map[String, String], s:StringBuffer)(print: => Unit){
      print
    }

    def isQuery:Boolean = false

    def isTupleConstructor = false

    /*
     *
     * Scala AST type. No receiver type attached.
     * Used by Type Finder.
     *
     */
    override def realType:Type = null


    /*
     * 
     * Returns a name with a receiver attached. 
     * Used by a Reconstructor.
     * 
     */
    def prettyName:String = ""

    /*
     * 
     * Returns full realName. In the case of locals, it attaches a word "local" in order to avoid ambiguity.
     * 
     */
    def realName:String = "InteritanceDef"

    override def isInvisible = true

  }

  abstract class UserDefinition extends ClauseDefinition(null) {

    def realPolys:List[String] = Nil

    /*
     *
     * Returns only polys of this definition, not its parent polys.
     *
     */
    def abstPolys:List[String] = Nil

    /*
     *
     * Used by Reconstructor to set children in a function.
     *
     */ 
    def length:Int = 0

    def printArgs(types:Map[String, String], s:StringBuffer)(print: => Unit){
      print
    }

    def isQuery:Boolean = false

    def isTupleConstructor = false

    /*
     *
     * Scala AST type. No receiver type attached.
     * Used by Type Finder.
     *
     */
    override def realType:Type = null

    def realTypeName:String 

    def isString:Boolean
  }

  class IntUD extends UserDefinition {

    /*
     * 
     * Returns a name with a receiver attached. 
     * Used by a Reconstructor.
     * 
     */
    def prettyName:String = "0"

    /*
     * 
     * Returns full realName. In the case of locals, it attaches a word "local" in order to avoid ambiguity.
     * 
     */
    def realName:String = "IntUserDefined"

    def realTypeName:String = "scala.Int"

    def isString = false
  }

  class BooleanUD extends UserDefinition {

    /*
     * 
     * Returns a name with a receiver attached. 
     * Used by a Reconstructor.
     * 
     */
    def prettyName:String = "false"

    /*
     * 
     * Returns full realName. In the case of locals, it attaches a word "local" in order to avoid ambiguity. 
     * 
     */
    def realName:String = "BooleanUserDefined"

    def realTypeName:String = "scala.Boolean"

    def isString = false
  }

  class StringUD extends UserDefinition {

    /*
     * 
     * Returns a name with a receiver attached. 
     * Used by a Reconstructor.
     * 
     */
    def prettyName:String = "\"?\""

    /*
     * 
     * Returns full realName. In the case of locals, it attaches a word "local" in order to avoid ambiguity. 
     * 
     */
    def realName:String = "StringUserDefined"

    def realTypeName:String = "java.lang.String"
 
    def isString = true
 }


  object QueryInfo {
    final val REAL_NAME = "<query>"
    final val PRETTY_NAME = ""
  }

  class DefinitionFactory {



    def makeClass(symbol:Symbol):ClassInfo = new ClassInfo(symbol)
    def makeQuery(tpe:Type, parent:Definition):QueryInfo = new QueryInfo(tpe, parent)
    def makeLocal(symbol:Symbol, parent:ClauseDefinition):LocalInfo = new LocalInfo(symbol, parent)
    def makeExternalDef(symbol:Symbol, parent:ClassInfo):ClauseDefinition = {
      if (symbol.isMethod) new MethodInfo(symbol, parent, true, false)
      else new FieldInfo(symbol, parent, true, false)
    }

    def makeInternalDef(symbol:Symbol, parent:ClassInfo, needThis:Boolean):ClauseDefinition = {
      if (symbol.isMethod) new MethodInfo(symbol, parent, false, needThis)
      else new FieldInfo(symbol, parent, false, needThis)
    }

    def makeBooleanUD = new BooleanUD()
    def makeIntUD = new IntUD()
    def makeStringUD = new StringUD()

    def makeInheritance(subtype:Type, supertype:Type):InheritanceInfo = new InheritanceInfo (subtype, supertype)
  }


class DefFunction(val definition:ClauseDefinition){ // extends AbstFunction {
  protected var children:Array[DefFunction] = setChildren()
  protected var variables:Array[Term] = setVariables()
  protected var vector:Array[(DefFunction,Int)] = setVector()

  def apply(that:DefFunction, index:Int, subs:Queue[Sub]):DefFunction = index match {
    case 0 => if (checkVect(1, that)) {
		if (that.vector.length == 1){
		  vector(0)._1.children(vector(0)._2) = that
		  vector = Array(that.vector(0))
		} else {
		  throw new Exception("//TODO: support partial order functions 0: ")
		}
	      } else {
		throw new Exception("//TODO: how to create a function")
	      }
	      apply(subs)
	      this

    case 1 => if (checkVect(0, that)) {
		if (that.vector.length == 0){
		  vector(0)._1.children(vector(0)._2) = that
		  vector = null
		} else {
		  throw new Exception("//TODO: support partial order functions 1: ")
		}
	      } else {
		throw new Exception("//TODO: how to create a function")
	      }
	      apply(subs)
	      this

    case 2 => vector(0)._1.children(vector(0)._2) = that
	      vector = null
	      apply(subs)
	      this


    case 3 => if (checkVect(1, that)) {
		if (that.vector.length == 1){
		  vector(0)._1.children(vector(0)._2) = that
		  vector(0) = that.vector(0)	
		} else {
		  //TODO: support partial order functions
		  throw new Exception("//TODO: support partial order functions 1: ")
		}	
	      } else {
		//TODO: how to create a function
		throw new Exception("//TODO: how to create a function")
	      }
	      apply(subs)
	      this

    case 4 => if (checkVect(0, that)) {
		if (that.vector.length == 0){
		  vector(0)._1.children(vector(0)._2) = that
		  val newVector = new Array[(DefFunction, Int)](vector.length-1)
		  for (i <- 0 until (vector.length-1)){
	            newVector(i) = vector(i+1)
		  }
		  vector = newVector
		} else {
		  //TODO: support partial order functions
		  throw new Exception("//TODO: support partial order functions 1: ")
		}
	      } else {
		//TODO: how to create a function
		throw new Exception("//TODO: how to create a function")
	      }
	      apply(subs)
	      this

    case 5 => vector(0)._1.children(vector(0)._2) = that
	      val newVector = new Array[(DefFunction, Int)](vector.length-1)
	      for (i <- 0 until (vector.length-1)){
	        newVector(i) = vector(i+1)
	      }
	      vector = newVector
	      apply(subs)
	      this

    case _ => null
  }

/*
  def apply(that:DefFunction, index:Int, subs:Queue[Sub]):DefFunction = index match {
    case 0 => if (checkVect(1, that)) {
		if (that.vector.length == 1){
		  vector(0)._1.children(vector(0)._2) = that
		  vector = Array(that.vector(0))
		} else {
		  throw new Exception("//TODO: support partial order functions 0: ")
		}
	      } else {
		throw new Exception("//TODO: how to create a function")
	      }
	      apply(subs)
	      this

    case 1 => if (checkVect(0, that)) {
		if (that.vector.length == 0){
		  vector(0)._1.children(vector(0)._2) = that
		  vector = null
		} else {
		  throw new Exception("//TODO: support partial order functions 1: ")
		}
	      } else {
		throw new Exception("//TODO: how to create a function")
	      }
	      apply(subs)
	      this

    case 2 => vector(0)._1.children(vector(0)._2) = that
	      vector = null
	      apply(subs)
	      this

    case 3 => vector(0)._1.children(vector(0)._2) = that
	      val newVector = new Array[(DefFunction, Int)](vector.length-1)
	      for (i <- 0 until (vector.length-1)){
	        newVector(i) = vector(i+1)
	      }
	      vector = newVector
	      apply(subs)
	      this

    case _ => null
  }
*/
  def apply(subs:Queue[Sub]){
    for(i <- 0 until variables.length){
      variables(i) = Sub.apply(variables(i), subs)
    }

    for(i <- 0 until children.length){
      val child = children(i)
      if (child != null) child.apply(subs)
    }
  }

  private def checkVect(first:Int, fun:DefFunction) = {
    val vector = fun.vector
    var cond = true
    var i = first
    while(cond && i < vector.length){
      cond = vector(i)._1 == fun
      i+=1
    }
    cond
  }

  private def setVariables() = {
    val varNames = this.definition.abstPolys
    val variables = new Array[Term](varNames.size)
    for(i <- 0 until varNames.size){
      variables(i) = Variable(varNames(i))
    }
    variables
  }

  private def setChildren() = new Array[DefFunction](this.definition.length)

  private def setVector() = {
    val vector = new Array[(DefFunction, Int)](children.length)
    for(i <- 0 until children.length){
      vector(i) = (this, i)
    }
    vector
  }

  def print(types:Map[String, String], s:StringBuffer){
    if (this.definition.needReceiver){
      children(0).print(types, s)
      if (!this.definition.isApply) appendNameToReceiver(this.definition.prettyName, s)
      printPolys(types, s)
      this.definition.printArgs(types, s){
	for (i <- 1 until children.length) {
	  if(i != 1) s.append(",")
	  children(i).print(types, s)
	}
      }
    } else {
      if (this.definition.isInvisible){
	this.definition.printArgs(types, s){
	  for (i <- 0 until children.length) {
	    if(i != 0) s.append(",")
	    children(i).print(types, s)
	  }
	}
      } else {
	appendName(this.definition.prettyName, s)
	printPolys(types, s)
	this.definition.printArgs(types, s){
	  for (i <- 0 until children.length) {
	    if(i != 0) s.append(",")
	    children(i).print(types, s)
	  }
	}
      }
    }
  }

  def appendName(name:String, s:StringBuffer){
    if (!isDollarName(name)) { 
      s.append(name)
    } else {
      s.append(dollarFreeName(name))
    }  
  }

  def appendNameToReceiver(name:String, s:StringBuffer){
    if (!isDollarName(name)) { 
      s.append(".")
      s.append(name)
    } else {
      s.append(".")
      s.append(dollarFreeName(name))
    }
  }

  def isDollarName(name:String) = name.contains("$")

  def dollarFreeName(name:String) = {
    name.replace("$colon", ":").replace("$amp", "&").replace("$amp", "&").replace("$bar", "&"). replace("$plus","+").replace("$minus", "-").replace("$div", "/").replace("$less"," <").replace("=", "$eq").replace("$greater",">").replace("$percent","%").replace("$times","*").replace("$up", "^").replace("$tilde","~").replace("$hash","#").replace("$bang","!").replace("$bslash","\\").replace("$qmark", "?")

  }

  def printPolys(types:Map[String, String], s:StringBuffer){
    if(variables.length > 0 && !definition.isTupleConstructor) {
      s.append("[")
      for (i <- 0 until variables.length){
	if(i != 0) s.append(",")
	DefFunction.printPoly(variables(i), types, s)
      }
      s.append("]")
    }
  }
}

object DefFunction {
  def printPoly(term:Term, types:Map[String, String], s:StringBuffer):Unit = term match{
    case Const(name) => s.append(niceName(types(name)))
    case Instance(name, terms) => 
      WithSugar(name, types, s){
	for (i <- 0 until terms.size){
	  if(i != 0) s.append(",")
	  printPoly(terms(i), types, s)
	}      
      }
    case Arrow(l1, l2) =>
      printPoly(l1, types, s)
      s.append("=>")
      printPoly(l2, types, s)
//    case _ => s.append("Int")
//TODO: Fix this bug
    case _ => throw new Exception("No all variables are instantiatied.")
  }

  def niceName(s:String) = {
    val index = s.lastIndexOf(".")
    if (index == -1) s
    else s.substring(index + 1)
  }
}

object WithSugar{
  def apply(name:String, types:Map[String, String], s:StringBuffer)(print: =>Unit): Unit = {
    val realName = types(name)
    val cond = realName.startsWith(scala.Tuple1.getClass.getName.replace("1$",""))
    if(!cond) s.append(DefFunction.niceName(realName))
    s.append(if (cond) "(" else "[")
    print
    s.append(if (cond) ")" else "]")
  }
}

  class Action(val base:Clause, val index:Int, val applied:Clause, val subs:Queue[Sub])
}
