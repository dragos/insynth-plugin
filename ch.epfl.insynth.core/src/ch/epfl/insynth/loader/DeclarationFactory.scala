package ch.epfl.insynth.loader

import ch.epfl.insynth.InSynth
import ch.epfl.insynth.trees.TypeTransformer
import ch.epfl.insynth.env.Declaration

trait TDeclarationFactory extends TData {
  self:InSynth =>
    
  import compiler._  
  
  object DeclarationFactory {

    def getOwnerClassDecl(sdecl:SimpleDecl):Option[Declaration] = {
      val declOption = if (sdecl.needReceiver) makeDecl(sdecl.getSymbol.fullName, sdecl.getReceiver.tpe, sdecl.getSymbol.tpe)
                 else makeDecl(sdecl.getSymbol.fullName, sdecl.getSymbol.tpe)
      
      declOption match {
        case Some(decl) =>
          decl.setIsConstructor(sdecl.isConstructor)
          decl.setHasParentheses(sdecl.needParentheses)
          decl.setHasThis(sdecl.needThis)
          decl.setIsApply(sdecl.isApply)
          decl.setBelongsToObject(sdecl.isInObject)
          
          //We do not care if something is field or method
          
          Some(decl)
        case None => None
      }
    }
    
    def getThisDecl(tpe:Type):Option[Declaration]  = {
      val thisOption = makeDecl("this", tpe)
      thisOption match {
        case Some(_this) =>
          _this.setIsThis(true)
          Some(_this)
        case None => None
      }
    }
  
    def getLocalDecl(sym:Symbol):Option[Declaration] = {
      val name = sym.fullName
      val tpe = sym.tpe
      val localOption = makeDecl(name, tpe)
      localOption match {
        case Some(local) =>
          local.setIsLocal(true)
          Some(local)
        case None => None
      }
    }
    
    def makeDecl(name:String, tpe:Type):Option[Declaration] = makeDecl(name, null, tpe)
    
    def makeDecl(name:String, receiverType:Type, tpe:Type):Option[Declaration] = {
      val scalaTypeOption = ScalaTypeExtractor(receiverType, tpe)
      scalaTypeOption match {
        case Some(scalaType) =>
          val inSynthType = TypeTransformer.transform(scalaType)
          Some(Declaration(name, inSynthType, scalaType))
        case None => None //throw new Exception("No type found for decl in: "+ this.getClass.getName)
      }     
    }
  
    def getCoerctionDecl() {
    
    }
  
  }
}