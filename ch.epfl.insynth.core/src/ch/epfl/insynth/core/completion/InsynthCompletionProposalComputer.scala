package ch.epfl.insynth.core.completion

import scala.collection.JavaConverters._
import org.eclipse.jdt.ui.text.java.IJavaCompletionProposalComputer
import org.eclipse.jdt.ui.text.java.ContentAssistInvocationContext
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.jface.text.contentassist.ICompletionProposal
import org.eclipse.jface.text.contentassist.IContextInformation
import org.eclipse.jdt.ui.text.java.JavaContentAssistInvocationContext
import scala.tools.eclipse.javaelements.ScalaCompilationUnit
import org.eclipse.jface.text.contentassist.CompletionProposal
import java.io.FileWriter
import java.io.BufferedWriter

import scala.tools.nsc.interactive.Global
import scala.tools.nsc.util.SourceFile
import java.io.BufferedReader
import java.io.InputStreamReader
import scala.tools.nsc.util.Position
import java.io.OutputStreamWriter

//import ch.epfl.insynth.library.ISynth
import ch.epfl.insynth.InSynth

/* 
TODO:
4. predef.exit(1)

*/

class InsynthCompletionProposalComputer extends IJavaCompletionProposalComputer {

  def sessionStarted() {}
  def sessionEnded() {}
  def getErrorMessage() = null

  /** No context information for the moment. */
  def computeContextInformation(context: ContentAssistInvocationContext, monitor: IProgressMonitor) =
    List[IContextInformation]().asJava  
    
  /** Return InSynth completion proposals. */
  def computeCompletionProposals(context: ContentAssistInvocationContext, monitor: IProgressMonitor): java.util.List[ICompletionProposal] = {
    import java.util.Collections.{ emptyList => javaEmptyList }

    val position = context.getInvocationOffset()
    
    context match {
      case jc: JavaContentAssistInvocationContext => jc.getCompilationUnit match {
        case scu: ScalaCompilationUnit =>
          
          var oldContent:Array[Char] = scu.getContents
          
          scu.withSourceFile {
            (sourceFile, compiler) =>
              if(compiler != InSynthWrapper.compiler){
            	  InSynthWrapper.compiler = compiler
                  InSynthWrapper.insynth = new InSynth(compiler)
              } else {
            	  if (InSynthWrapper.insynth == null){
            		  InSynthWrapper.insynth = new InSynth(compiler)
            	  }
              }
              
              compiler.askReload(scu, getNewContent(position, oldContent))


              val results = InSynthWrapper.insynth.getSnippets(sourceFile.position(position))
              
              val list1:java.util.List[ICompletionProposal] = new java.util.LinkedList[ICompletionProposal]()
              
              results.foreach(x => list1.add(new InSynthCompletitionProposal(x)))
              
              list1
             }(javaEmptyList())
             
        case _ => javaEmptyList()
      }
      case _ => javaEmptyList()
    }
  }
  
  private def getNewContent(position: Int, oldContent: Array[Char]): Array[Char] = {
    val (cont1, cont2) = oldContent.splitAt(position)
    
    val mark = ";{  ;exit()};".toCharArray
      
    val newContent = Array.ofDim[Char](oldContent.length + mark.length)
    
    System.arraycopy(cont1, 0, newContent, 0, cont1.length)
    System.arraycopy(mark, 0, newContent, cont1.length, mark.length)
    System.arraycopy(cont2, 0, newContent, cont1.length + mark.length, cont2.length)
    
    println("New content:")
    newContent.foreach{print}
    //println()
    newContent
  }
}


object InSynthWrapper{
  
  var insynth:InSynth = null;
  var compiler:Global = null;
  
}