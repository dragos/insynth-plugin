package ch.epfl.insynth.library.prover

import scala.tools.nsc.interactive.Global
import ch.epfl.insynth.library.definitions._
import ch.epfl.insynth.library.ISynth

trait WeightManagers extends Definitions {
  self: ISynth =>

  class WeightManager(m:  Map[String, Double]) {

    def computeInitialValues(initClauses: List[Clause]){
      initClauses.foreach{
	clause => clause.weight = computeWeight(clause)
      }
    }

    def computeWeight(c: Clause):Weight = {
      val w = computeWeightofFSymbols(c.fsym) + computeWeightofTerm(c.term) 
      new Weight(w)
    }
    
    private def computeWeightofFSymbols(fsym: List[String]): Double = fsym.map(m(_)).reduceLeft(_+_)

    private def computeWeightofTerm(t: Term): Double  = t match {
      case Const(n) => m(n)
      case Variable(v) => m("Var")
      case Instance(n, l) => m(n) + l.map(computeWeightofTerm(_)).reduceLeft(_+_)
      case Arrow(t1, t2) => m("Arrow") + computeWeightofTerm(t1) + computeWeightofTerm(t2)
      case _=> 0.
    }
  }
}
/*
trait WeightManagers {
  self: RichPresentationCompiler =>

class WeightManager(map:  Map[String, Double], userPref: List[String]) {

  private var alpha:Double = 0
  private var beta:Double = 0

//========================

  def computeInitialValues(initClauses: List[Clause]){
    var importantClauses: List[(Clause, Double)] = Nil
    var importantMin = 100000. //Double.MaxValue
    var importantMax: Double = 0
    var otherClauses: List[(Clause, Double)] = Nil
    var otherMin = 100000. //Double.MaxValue
    initClauses.foreach(c => {
       val n = computeWeight(c, map)
       if (isImportant(c, userPref)) {
         importantClauses = (c, n) :: importantClauses
         if (n < importantMin) importantMin = n
         if (n > importantMax) importantMax = n
       } else {
         otherClauses = (c, n) :: otherClauses
         if (n < otherMin) otherMin = n
       }
    })

    val p = scale(importantClauses, importantMin, importantMax)
    val clauses = p._1 ::: otherClauses

    clauses.foreach{
      x => x._1.weight = new Weight(x._2)
    }

    alpha = p._2
    beta = p._3
  }



// ================================

  def computeWeight(c: Clause):Weight = {

    val w = computeWeight(c, map)
    val pn = if (isImportant(c, userPref)) scaleClauseWeight((c,w), alpha, beta) else (c,w)
    new Weight(pn._2)
  }

  private def computeWeightofFSymbols(fsym: List[String], m: Map[String, Double]): Double = {
    var i: Double = 0
    fsym.foreach(f => {
      val fv = m(f)
      i = i + fv
    })
    i
  }


  private def computeWeightofTerm(t: Term, m: Map[String, Double]): Double  = t match {
    case Const(n) => m(n)
    case Variable(v) => m("Var")
    case Instance(n, l) => {
      var i: Double = 0
      l.foreach(le => i = i + computeWeightofTerm(le, m))
      m(n) + i
    }
    case Arrow(t1, t2) => {
      val i1 = computeWeightofTerm(t1, m)
      val i2 = computeWeightofTerm(t2, m)
      m("Arrow") + i1 + i2
    }
    //Empty clause
    case _=> 0.
  }


  private def computeWeight(c: Clause, m: Map[String, Double]): Double = {
    val i1 = computeWeightofFSymbols(c.fsym, m)
    val i2 = computeWeightofTerm(c.term, m)
    val i3 = i1 + i2
    i3
  }


// =================


   private def doFSymContainImportant(fsym: List[String], u: List[String]): Boolean = {
     val l = fsym.intersect(u)
     ! l.isEmpty
   }


  private def doTermContainImortant(t: Term, u: List[String]): Boolean = t match {
    case Const(n) => u.contains(n)
    case Variable(v) => false
    case Instance(n, l) => {
      val nb = u.contains(n)
      var bf = false
      l.foreach(le => bf = bf || doTermContainImortant(le, u))
      bf || nb
    }
    case Arrow(t1, t2) => {
      val b1 = doTermContainImortant(t1, u)
      val b2 = doTermContainImortant(t2, u)
      b1 || b2
    }
    //Empty clause
    case _ => false
  }

  private def isImportant(c: Clause, u: List[String]): Boolean = {
      val b1 = doFSymContainImportant(c.fsym, u)
      val b2 = doTermContainImortant(c.term, u)
      b1 || b2
  }


// ====================


  private def scaleClauseWeight(p:(Clause, Double), alpha: Double, beta: Double): (Clause, Double) = {
    val n = p._2 * alpha + beta
    (p._1, n)
  }


  //TODO: This should be fixed
  private def scale(l: List[(Clause, Double)], im: Double, iM: Double): (List[(Clause, Double)], Double, Double)  = {
    val alpha = 0.000001 //1.0 / (2.0 * (iM - im))
    val beta = 0.5 //(iM - 2 *im) /  (2.0 * (iM - im))
    val l1 = l.map(e => scaleClauseWeight(e, alpha, beta))
    (l1, alpha, beta)
  }
}
}
*/
