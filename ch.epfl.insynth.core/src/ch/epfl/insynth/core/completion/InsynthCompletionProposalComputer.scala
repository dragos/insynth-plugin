package ch.epfl.insynth.core.completion

import scala.collection.JavaConverters._
import org.eclipse.jdt.ui.text.java.IJavaCompletionProposalComputer
import org.eclipse.jdt.ui.text.java.ContentAssistInvocationContext
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.jface.text.contentassist.ICompletionProposal
import org.eclipse.jface.text.contentassist.IContextInformation
import org.eclipse.jdt.ui.text.java.JavaContentAssistInvocationContext

import scala.tools.eclipse.javaelements.ScalaCompilationUnit

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
          scu.withSourceFile { (sourceFile, compiler) =>

            // add calls to InSynth here, using the compiled and the source file (scala.tools.nsc)
            println("InSynth plugin called at pos: %d".format(position))
            
            javaEmptyList[ICompletionProposal]()
          }(javaEmptyList())

        case _ => javaEmptyList()
      }
      case _ => javaEmptyList()
    }
  }
}
