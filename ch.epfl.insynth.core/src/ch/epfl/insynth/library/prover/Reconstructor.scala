package ch.epfl.insynth.library.prover

import scala.tools.nsc.interactive.Global
import ch.epfl.insynth.library.definitions._
import ch.epfl.insynth.library.ISynth

trait Reconstructors extends Definitions {
 self: ISynth =>

object Reconstructor{
  def reconstruct(clause:Clause):DefFunction = {
    val action = clause.action
    if (action == null){
      clause.fun()
    } else {
      reconstruct(action.base).apply(reconstruct(action.applied), action.index, action.subs)
    }
  }
}

object ReconstructonPrinter{
  def print(clause:Clause):String = {
    val action = clause.action
    if (action != null){
      "(index = "+action.index +"  subs = "+ action.subs.mkString(",")+" base = "+print(action.base)+"  appl = "+print(action.applied)+")"
    } else "null"
  }
}
}
