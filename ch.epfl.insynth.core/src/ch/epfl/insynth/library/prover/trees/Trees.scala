package ch.epfl.insynth.library.prover.trees

import scala.tools.nsc.interactive.Global
import scala.math.Ordered
import scala.collection.mutable.{ListBuffer, PriorityQueue, Queue, HashSet}
import ch.epfl.insynth.library.ISynth

trait Trees {
  self: ISynth =>

  sealed abstract class Term

  case class Const(val name: String) extends Term{
    private var subtypes = List[String]()
    def isSubtype(name:String) = subtypes.contains(name)
    def setSubtypes(subt:List[String]){subtypes = subt}
  }

  case class Variable(val name:String) extends Term
  case class Instance(val name: String, val t: List[Term]) extends Term
  case class Arrow(val l1:Term, val l2:Term) extends Term

  //TODO: This should be more efficient
  //Should implement our own priority queue
  class Weight(var value:Double) extends Ordered[Weight] {
    def compare(that:Weight) = {
      val thisVal = this.value
      val thatVal = that.value
      if (thisVal < thatVal) -1
      else if (thisVal > thatVal) 1
      else 0
    }

    def halve(){value = (2. * value)/5.}

    override def toString = "Weight: "+ value
  }

  abstract class Sub {
    def apply(term: Term):Term
  }

  object Sub{
    def apply(term: Term, subs:Queue[Sub]):Term = {
      var newTerm = term
      subs.foreach{sub => newTerm = sub.apply(newTerm)}
      newTerm
    }
  }

  class RenameSub(oldName:String, newName:String) extends Sub {
    def apply(term: Term):Term = term match {
      case Variable(name) if (name == oldName) => Variable(newName)
      case Instance(name, terms) => Instance(name, terms.map(x => apply(x)))
      case Arrow(l1, l2) => Arrow(apply(l1), apply(l2))
      case t => t
    }

    override def toString = "RenameSub("+oldName+","+newName+")"
  }

  class VarTermSub(var1:String, term:Term) extends Sub {
    def apply(term1: Term):Term = term1 match {
      case Variable(name) if (name == var1) => term
      case Instance(name, terms) => Instance(name, terms.map(x => apply(x)))
      case Arrow(l1, l2) => Arrow(apply(l1), apply(l2))
      case t => t
    }

    override def toString = "VarTermSub("+var1+","+term+")"
  }

}
