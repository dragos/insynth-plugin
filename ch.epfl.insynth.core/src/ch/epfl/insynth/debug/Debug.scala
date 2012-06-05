package ch.epfl.insynth.debug

import ch.epfl.insynth.trees.Type

object Debug{
  final val ON = true
  
  import ch.epfl.insynth.env._
  
  def apply(ta:TypeAssignment){
    if(ON){
      printTA(ta)
    }
  }
  
  
  def apply(decl:Declaration){
    if(ON){
      printDecl(decl)
    }
  }
  
  def apply(msg:String){
    if(ON){
      print(msg+" ")
    }
  }
  
  def apply(tpe:Type){
    if(ON){
      printType(tpe)
    }
  }
  
  def apply(msg:String, ta:TypeAssignment){
    Debug(msg)
    Debug(ta)
  }
  
  def apply(msg:String, decl:Declaration){
    Debug(msg)
    Debug(decl)
  }
  
  def apply(msg:String, tpe:Type){
    Debug(msg)
    Debug(tpe)
  }

  private def printType(tpe:Type){
    println("Type: "+tpe)
  }
  
  private def printTA(ta:TypeAssignment){
    ta.getDeclarations.foreach(printDecl)
  }
  
  private def printDecl(decl:Declaration){
    println("Decl["+decl+"   "+decl.hashCode+"]")
  }
}