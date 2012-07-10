package ch.epfl.insynth

import scala.tools.nsc.interactive.Global
import ch.epfl.insynth.loader.TLoader
import scala.tools.nsc.util.SourceFile
import ch.epfl.insynth.engine.Engine
import ch.epfl.insynth.util.TimeOut
import ch.epfl.insynth.scheduler.BFSScheduler
import ch.epfl.insynth.util.TreePrinter
import ch.epfl.insynth.reconstruction.Reconstructor
import ch.epfl.insynth.env.InitialEnvironmentBuilder
import ch.epfl.insynth.loader.TPreLoder
import ch.epfl.insynth.reconstruction.Output
import ch.epfl.insynth.scheduler.WeightScheduler

class InSynth(val compiler: Global) extends TLoader with TPreLoder {
  
  import compiler._
  
  private val loader = new Loader()
  
  def getPredefDecls() = {
    val preloader = new PreLoader()
    preloader.load()
  }
  
  def getSnippets(pos:Position, builder:InitialEnvironmentBuilder):List[Output] = {   
    
    try {
      
      ScalaTypeExtractor.clear()
      var tree = wrapTypedTree(pos.source, false)
      val desiredType = loader.load(pos, tree, builder)

      val engine = new Engine(builder, desiredType, new WeightScheduler(), TimeOut(Config.getTimeOutSlot))
      
      val solution = engine.run()
      
      if (solution != null) Reconstructor(solution.getNodes.head) else Nil
    } catch {
      case ex =>
      Nil
    }
  }

  private def wrapTypedTree(source: SourceFile, forceReload: Boolean): Tree =
  {
    val response = new Response[Tree]
    askType(source, forceReload, response)
    val typed = response.get
    typed.fold(identity, throw _)
  }  
}