package ch.epfl.insynth.loader

import scala.tools.nsc.interactive.Global
import ch.epfl.insynth.env.InitialEnvironmentBuilder
import ch.epfl.insynth.InSynth
import ch.epfl.insynth.typetransformations.TExtractor
import ch.epfl.insynth.trees.TypeTransformer
import ch.epfl.insynth.trees.{Type => InSynthType}

trait TLoader extends TCollector with TExtractor with TDeclarationFactory {
  self:InSynth =>
  
  import compiler._
  
  class Loader {
    
    private var collector = new Collector()
    
    def load(pos:Position, tree:Tree):(InSynthType, InitialEnvironmentBuilder) = {
      val builder = new InitialEnvironmentBuilder()
   
      val queryType = load(pos, tree, builder)
    
      (queryType, builder)
    }
  
    def load(pos:Position, tree:Tree, builder:InitialEnvironmentBuilder):InSynthType = {
      val data = collector.gather(tree, pos)
      
      if (data.hasDesiredType) {

        if (data.hasThisType) {
          println("This type: "+ data.getThisType)
          
          val thisOption = DeclarationFactory.getThisDecl(data.getThisType)
          
          thisOption match {
            case Some(_this) => builder.addDeclaration(_this)
            case None => 
          }
        }
        
        println("Locals: ")
        data.getLocals.foreach {
          localSym =>
            println(localSym.fullName)
            
            val localOption = DeclarationFactory.getLocalDecl(localSym)
          
            localOption match {
              case Some(local) => builder.addDeclaration(local)
              case None => 
            }
        }
        
        println("Type decls:")
        data.getMostNestedOwnerTypeDecls.foreach {
          sdecl =>
            println(sdecl.getSymbol.fullName)
 
            val declOption = DeclarationFactory.getOwnerClassDecl(sdecl)
          
            declOption match {
              case Some(decl) => builder.addDeclaration(decl)
              case None => 
            }
        }

        println("Desired type: "+data.getDesiredType)
                
        val desiredTypeOption = ScalaTypeExtractor(data.getDesiredType)
        
        desiredTypeOption match {
          case Some(desiredType) => 
            TypeTransformer.transform(desiredType)
          case None => throw new Exception("Desired Type not found in: "+this.getClass.getName)
        }
      } else throw new Exception("Desired Type not found in: "+this.getClass.getName)
    }
  }
}