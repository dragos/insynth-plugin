package ch.epfl.insynth.loader

import scala.tools.nsc.interactive.Global
import ch.epfl.insynth.env.InitialEnvironmentBuilder
import ch.epfl.insynth.InSynth
import ch.epfl.insynth.typetransformations.TExtractor
import ch.epfl.insynth.trees.TypeTransformer
import ch.epfl.insynth.trees.{Type => InSynthType}
import ch.epfl.scala.trees.ScalaType

trait TLoader extends TCollector with TExtractor with TDeclarationFactory {
  self:InSynth =>
  
  import compiler._
  
  class Loader {
    
    private var collector = new Collector()
    
    def load(pos:Position, tree:Tree):(ScalaType, InitialEnvironmentBuilder) = {
      val builder = new InitialEnvironmentBuilder()
   
      val queryType = load(pos, tree, builder)
    
      (queryType, builder)
    }
  
    //TODO: 
    //(1) Load weights
    //(2) Load coerctions
    //(3) Loas subtypes
    def load(pos:Position, tree:Tree, builder:InitialEnvironmentBuilder):ScalaType = {
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
 
            val declOption = DeclarationFactory.getDecl(sdecl)
          
            declOption match {
              case Some(decl) => builder.addDeclaration(decl)
              case None => 
            }
        } 
        
        loadDecls(data.getPackageTypes, builder)
        
        loadDecls(data.getImportedTypes, builder)
        
        println("Desired type: "+data.getDesiredType)
                
        val desiredTypeOption = ScalaTypeExtractor.getLocalType(data.getDesiredType)
        
        desiredTypeOption match {
          case Some(desiredType) => 
            //TypeTransformer.transform(desiredType)
            desiredType
          case None => throw new Exception("Desired Type not found in: "+this.getClass.getName)
        }
      } else throw new Exception("Desired Type not found in: "+this.getClass.getName)
    }
    
    private def loadDecls(types:List[Symbol], builder:InitialEnvironmentBuilder){
       for {
         tpe <- types
         decl <- tpe.tpe.decls
         if(!decl.nameString.contains("$") && 
	        decl.exists &&
	        decl.isPublic &&
	        !decl.isSynthetic &&
	        !(tpe.isModule && decl.isConstructor) &&
	        !decl.isSetter &&
	        decl.isValue &&  //What was this? I guess with this we get rid of type defs and other junk.
	        !TData.returnsUnit(decl))
        } {
          val declOption = DeclarationFactory.getDecl(new SimpleDecl(decl, tpe, tpe.isModule, false, decl.isConstructor))
          
          declOption match {
            case Some(decl) => builder.addDeclaration(decl)
            case None => 
          }
        }
    }
  }
}