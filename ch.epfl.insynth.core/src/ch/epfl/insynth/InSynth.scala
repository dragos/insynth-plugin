package ch.epfl.insynth

import scala.tools.nsc.interactive.Global
import ch.epfl.insynth.loader.TLoader
import scala.tools.nsc.util.SourceFile
import ch.epfl.insynth.engine.Engine
import ch.epfl.insynth.util.TimeOut
import ch.epfl.insynth.scheduler.BFSScheduler
import ch.epfl.insynth.util.TreePrinter
import ch.epfl.insynth.reconstruction.Reconstructor
import ch.epfl.insynth.debug.Debug

class InSynth(val compiler: Global) extends TLoader {
  
  import compiler._
  
  private val config = new Config()
  
  private val loader = new Loader()
  
  def getSnippets(pos:Position):List[String] = {
    //TreePrinter(config.errorFileName, "")    
    
    try {
      var tree = wrapTypedTree(pos.source, false)
      val (desiredType, builder) = loader.load(pos, tree)
      
      val engine = new Engine(builder, desiredType, new BFSScheduler(), TimeOut(config.getTimeOutSlot))
  
      val initialDecls = builder.getAllDeclarations
      
      val time = System.currentTimeMillis
      
      val solution = engine.run()
/*      
      if(solution != null){
        TreePrinter(config.outputFileName, "Solution found in: "+ (System.currentTimeMillis - time)+" ms", solution, initialDecls)
        //TreePrinter(config.outputFileName, "Solution found in: "+ (System.currentTimeMillis - time)+" ms", initialDecls)
      }
      else TreePrinter(config.outputFileName, "No solution found in: "+ (System.currentTimeMillis - time)+" ms", initialDecls)
*/
      if (solution != null){
        Reconstructor(solution.getNodes.head).map(_.getSnippet)
        //List("Found!")
      } else Nil
      
    } catch {
      case ex =>
//        TreePrinter(config.errorFileName, ex.getMessage +"\n"+ ex.getStackTraceString)
      Nil
    }
  }

  private def wrapTypedTree(source: SourceFile, forceReload: Boolean): Tree =
    wrap[Tree](r => new AskTypeItem(source, forceReload, r).apply(), t => throw t)

  private def wrap[A](compute: Response[A] => Unit, handle: Throwable => A): A = {
    val result = new Response[A]
    compute(result)
    result.get.fold(o => o, handle)
  }  
}