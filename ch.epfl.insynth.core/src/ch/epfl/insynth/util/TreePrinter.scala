package ch.epfl.insynth.util
import ch.epfl.insynth.env._
import ch.epfl.scala.trees._
import java.io.PrintWriter
import java.io.FileWriter

object TreePrinter {
 
  def apply(fileName:String, msg:String){
    val out = new PrintWriter(new FileWriter(fileName))
    out.println
    out.println(msg)
    out.flush
    out.close
  }
  
  def apply(fileName:String, msg:String, decls:List[Declaration]){
    val out = new PrintWriter(new FileWriter(fileName))
    
    out.println("Initial decls: ")
    decls.foreach{
      decl => out.println(decl)
    }
    
    out.println
    out.println(msg)
    out.flush
    out.close
  }
  
  def apply(fileName:String, answer:ContainerNode, decls:List[Declaration]){
    val out = new PrintWriter(new FileWriter(fileName))
    
    out.println("Initial decls: ")
    decls.foreach{
      decl => out.println(decl)
    }
    
    out.println    
    printAnswer(out, answer)
    out.flush
    out.close
  }
  
  def apply(fileName:String, answer:ContainerNode, decls:List[Declaration], depth:Int){
    val out = new PrintWriter(new FileWriter(fileName))
    
    out.println("Initial decls: ")
    decls.foreach{
      decl => out.println(decl)
    }
    
    out.println    
    printAnswerWithDepth(out, answer, depth)
    out.flush
    out.close
  }  
  
  def apply(fileName:String, msg:String, answer:ContainerNode, decls:List[Declaration]){
    val out = new PrintWriter(new FileWriter(fileName))
    
    out.println("Initial decls: ")
    decls.foreach{
      decl => out.println(decl)
    }
    
    out.println
    out.println(msg)
    out.println    
    printAnswer(out, answer)
    out.flush
    out.close
  }  
  
  def apply(fileName:String, msg:String, answer:ContainerNode, decls:List[Declaration], depth:Int){
    val out = new PrintWriter(new FileWriter(fileName))
    
    out.println("Initial decls: ")
    decls.foreach{
      decl => out.println(decl)
    }
    
    out.println
    out.println(msg)
    out.println    
    printAnswerWithDepth(out, answer, depth)
    out.flush
    out.close
  }
  
  def apply(answer:ContainerNode, decls:List[Declaration]){
    val out = new PrintWriter(System.out)
    
    out.println("Initial decls: ")
    decls.foreach{
      decl => out.println(decl)
    }
    
    out.println
    
    printAnswer(out, answer)
    out.flush
  }
 
  private def printAnswer(out:PrintWriter, answer:ContainerNode) {
    out.println
    out.println("Solution:")
    
    printAnswer(out, answer, Set.empty[SimpleNode], 0)
  }
  
  private def printAnswerWithDepth(out:PrintWriter, answer:ContainerNode, depth:Int) {
    out.println
    out.println("Solution:")
    
    printAnswerWithDebth(out, answer, Set.empty[SimpleNode], 0, depth)
  }  
  
  private def printAnswer(out:PrintWriter, answer:ContainerNode, set:Set[SimpleNode], length:Int) {
      answer.getNodes.foreach{
        simpleNode =>
          if (!set.contains(simpleNode)){          
            printlnDeclsWithIndention(out, simpleNode.getDecls, length)
            for (val (tpe, container) <- simpleNode.getParams) {
              val tpeName = "["+tpe.toString+ "]"
              printlnWithIndention(out, tpeName, length)
              printlnWithIndention(out, "|", length + 4)
              printlnWithIndention(out, "V", length + 4)
              printAnswer(out, container, set + simpleNode, length + 4)
            }
          } else {
            printWithIndention(out, "Visited ", length)
            printlnDeclNamesWithIndention(out, simpleNode.getDecls, 0)
          }
      }
  }
  
  private def printAnswerWithDebth(out:PrintWriter, answer:ContainerNode, set:Set[SimpleNode], length:Int, depth:Int) {
    if(depth > 0){
      answer.getNodes.foreach{
        simpleNode =>
          if (!set.contains(simpleNode)){          
            printlnDeclNamesWithIndention(out, simpleNode.getDecls, length)
            for (val (tpe, container) <- simpleNode.getParams) {
              val tpeName = "["+tpe.toString+ "]"
              printlnWithIndention(out, tpeName, length)
              printlnWithIndention(out, "|", length + 4)
              printlnWithIndention(out, "V", length + 4)
              printAnswerWithDebth(out, container, set + simpleNode, length + 4, depth-1)
            }
          } else {
            printWithIndention(out, "Visited ", length)
            printlnDeclNamesWithIndention(out, simpleNode.getDecls, 0)
          }
      }
    }
  }
  
  private def printlnWithIndention(out:PrintWriter, text:String, length:Int){
    for(i <- 0 until length) out.print(" ")
    out.println(text)
  }
  
  private def printWithIndention(out:PrintWriter, text:String, length:Int){
    for(i <- 0 until length) out.print(" ")
    out.print(text)
  }
  
  private def printlnDeclsWithIndention(out:PrintWriter, decls:List[Declaration], length:Int){
    printlnWithIndention(out, decls.mkString("Decls[",",","]"), length)
  }
  
  private def printlnDeclNamesWithIndention(out:PrintWriter, decls:List[Declaration], length:Int){
    printlnWithIndention(out, decls.map(_.getSimpleName).mkString("Decls[",",","]"), length)
  }
}