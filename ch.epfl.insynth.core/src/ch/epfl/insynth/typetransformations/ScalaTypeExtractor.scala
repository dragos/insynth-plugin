package ch.epfl.insynth.typetransformations

import scala.tools.nsc.interactive.Global
import ch.epfl.scala.trees.{Const => ScalaConst, Method => ScalaMethod, Function => ScalaFunction, Instance => ScalaInstance, ScalaType}
import ch.epfl.insynth.InSynth

trait TExtractor{
  self:InSynth =>

  import compiler._  
    
object ScalaTypeExtractor {

  def apply(tpe:Type) = getType(null, tpe)
    
  def apply(receiverType:Type, tpe:Type) = getType(receiverType, tpe)
    
  def getLocalType(tpe:Type):Option[ScalaType] = {
    assert(tpe != null)
    try{
      Some(traverse(tpe))
    } catch {
      case ex =>
        None
    }
  }
  
  private def getType(receiverType:Type, tpe:Type):Option[ScalaType] = {
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
	      ScalaInstance(sym.fullName, args.map(traverse))
	     
	  //Base types
	  case TypeRef(pre: Type, sym: Symbol, args: List[Type]) =>
	    if (!sym.isTypeParameter) ScalaConst(sym.fullName)	      
	      else throw new Exception("<<Parametrized types not supported: "+tpe.getClass.getName+">>")
	    
	  case _ => throw new Exception("<<Not supported: "+tpe.getClass.getName+">>") 
    }
  }  
}
}