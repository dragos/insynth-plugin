package ch.epfl.insynth.library.prover

import scala.tools.nsc.interactive.Global
import scala.math._

import scala.math.Ordered
import scala.collection.mutable.{ListBuffer, PriorityQueue, Queue, HashSet}

import scala.collection.immutable.SortedSet
import ch.epfl.insynth.library.prover._
import ch.epfl.insynth.library.prover.util._
import ch.epfl.insynth.library.ISynth

trait Provers extends Reconstructors with WeightManagers {
  self: ISynth =>

class Prover(pos: List[Clause],
	     neg: Clause,
	     unifier: Unifier,
	     var emptyClsNum: Int,
	     timeout: Long,
	     critical: Int => Int,
	     map:  Map[String, Double],
	     userPref: List[String],
	     polyMap: Map[String, String],
	     BackwardSearch_ON:Boolean) {

  private final val weighting = new Weighting[Clause]()
  private final val resolving = new Resolving[Clause]()
  private final val creating = new Creating[Clause]()

  //This must come before initFree such that we set weights first
  private val wManager = initWeightManager() 

  val free: DenseArray[PriorityQueue[Clause]] = initFree()
  
  val captured: DenseArray[PriorityQueue[Clause]] = new DenseArray[PriorityQueue[Clause]]()

  //This should be dense or sparse array 
  val positive: DenseArray[PriorityQueue[Clause]] = initPositive()
  val negative: DenseArray[PriorityQueue[Clause]] = initNegative()

  //all levels smaller than this one should have 0 free clauses
  private var minLevel: Int = 0
  private var maxLevel: Int = 0
  private var now: Int = 0
  private var startTime: Long = 0

  private val generatedPos = initGeneratedPos()

  private val solutionSet = new HashSet[String]() 
  private var solutions = List.empty[(Weight, String)]

  private def initWeightManager() = {
    val manager = new WeightManager(map)
    manager.computeInitialValues(neg :: pos)
   
    pos.foreach{
      x => 
      val realName = x.definition.realName
    }

    manager
  }

  private def initGeneratedPos():DenseArray[DenseArray[Int]] = {
    val array = new DenseArray[DenseArray[Int]]()
    //or we can simply omit this line
    array(now) = new DenseArray[Int]()
    array(now)(minLevel) = pos.size
    array
  }

  private val generatedNeg = initGeneratedNeg()

  private def initGeneratedNeg():DenseArray[DenseArray[Int]] = {
    val array = new DenseArray[DenseArray[Int]]()
    //or we can simply omit this line
    array(now) = new DenseArray[Int]()
    array(now)(minLevel) = 1
    array
  }

  private def initFree():DenseArray[PriorityQueue[Clause]] = {
    val queue = new PriorityQueue[Clause]()(weighting)
    queue ++= pos
    queue += neg
    val array = new DenseArray[PriorityQueue[Clause]]()
    array(0) = queue
    array
  }

  private def initPositive():DenseArray[PriorityQueue[Clause]] = {
    val queue = new PriorityQueue[Clause]()(creating)
    queue ++= pos
    val array = new DenseArray[PriorityQueue[Clause]]()
    array(0) = queue
    array
  }

  private def initNegative():DenseArray[PriorityQueue[Clause]] = {
    val queue = new PriorityQueue[Clause]()(creating)
    queue += neg
    val array = new DenseArray[PriorityQueue[Clause]]()
    array(0) = queue
    array
  }

  private def putIfNewSolution(clause:Clause) = try{
    val s = new StringBuffer()
    val fun = Reconstructor.reconstruct(clause).print(polyMap, s)
    val solution = s.toString

    if (!solutionSet.contains(solution)) {
            
      //println("Solution founded:  "+ solution)
      //println(ReconstructonPrinter.print(clause))

      solutions = (clause.weight, solution) :: solutions
      solutionSet.add(solution)
    }
  } catch {
    case e =>
  }

  var chosenNum = 0

  def run():ProverOutput = {
    var proofs: List[Proof] = Nil
    var finished = false

    startTime = System.currentTimeMillis
    lastTime = System.currentTimeMillis

    while(!finished && maxLevel >= minLevel) {
      val clause = nextClause()
      if (clause == null) {
	finished = true
      } else {
	resolve(clause)
	finished = isTimeOut
      }
      steps+=1
    }

    val sorted = solutions.sortWith((x, y) => (x._1 < y._1)).map(z => z._2)

    val list = if (emptyClsNum == -1 || emptyClsNum > sorted.size) sorted else sorted.take(emptyClsNum)

    new ProverOutput(list, (System.currentTimeMillis - startTime).toString + "ms", pos.size)
  }

// Debugging --------------------------------------------------------------------------------//

  private var lastTime:Long = 0
  private var timeCnt = 1

  private def printEvery(time:Long){
    val localTime = System.currentTimeMillis
    if (localTime - lastTime > time) {
      println("------------------------------------------------------------------------")
      println("Time: " + (time * timeCnt)+ "ms")
      println("MinLevel: "+ minLevel)
      println("MaxLevel: "+ maxLevel)
      println("Now: "+ now)

      println()
      for (i <- 0 to maxLevel){
	print("Level "+i)
	print(" free = "+(if(free(i)!= null) free(i).size else 0))
	print(" captured = "+(if(captured(i)!= null) captured(i).size else 0))
	print(" positive = "+(if(positive(i)!= null) positive(i).size else 0))
	print(" negative = "+(if(negative(i)!= null) negative(i).size else 0))
	println()
	println()
      }
      println()
      println("------------------------------------------------------------------------")
      
      lastTime = localTime
      timeCnt+=1

      printGenerated()
   }
  }

  private def printGenerated(){

    println("Generated: -------------------------------------------------------------")
    println("Positive [Now, Elements]:")
    for(i <- 0 to now){
      print(i)
      for(j <- 0 to maxLevel){
	print("  "+generatedPos(i)(j))
      }
      println
    }

    println("Negative [Now, Elements]:")
    for(i <- 0 to now){
      print(i)
      for(j <- 0 to maxLevel){
	print("  "+generatedNeg(i)(j))
      }
      println
    }

    println("------------------------------------------------------------------------")
  }  

  private def sortByChosen:List[Clause] = {
    val pq = positive.elements ++ negative.elements

    val elements = ListBuffer[Clause]()
    pq.foreach {x => elements ++= x}
    elements.sortWith((x,y) => x.getChosen > y.getChosen).toList
  }

  private def printTopChosen(length:Int){
    var list:List[Clause] = null
    val sorted = this.sortByChosen
    if (length == -1 || length > sorted.size){
      list = sorted
    } else {
      list = sorted.take(length)
    }
    println("Most used clauses are:")
    for (i <- 0 until list.size){
      val cls = list(i)
      println(i+".  times="+cls.getChosen+"  level="+cls.level+"  names ="+findNames(cls).mkString(", "))
    }
  }

  private def findNames(clause:Clause):List[String] = {
    val action = clause.action
    if (action == null){
      List(clause.definition.prettyName)
    } else {
      findNames(action.base) ++ findNames(action.applied)
    }  
  }

  var steps = 0
  var resolveTime:Long = 0
  var resolveNum:Long = 0
  var setWeigthTime:Long = 0
  var unificationTime:Long = 0
  var subsApplicationTime:Long = 0
  var dequeueTime:Long = 0

  val PROFILING_NUM = 0

  private def printStatistics(){
    println("//---------------------------------------- Statistics -----------------------------------------//")
    println("#Steps:"+steps +"          #resolved:"+resolveNum)
    println("- Resolve time:"+(resolveTime.toDouble/1000000)+"ms         average:"+(resolveTime.toDouble/1000000/resolveNum))
    println("   - Set. weight time:"+(setWeigthTime.toDouble/1000000)+"ms")
    println("   - Unification time:"+(unificationTime.toDouble/1000000)+"ms")
    println("   - App. subs. time:"+(subsApplicationTime.toDouble/1000000)+"ms")
    println("   - Dequeue time:"+(dequeueTime.toDouble/1000000)+"ms")
    println("Overal time: "+(System.currentTimeMillis - startTime).toString + "ms")
    println("//---------------------------------------------------------------------------------------------//")
  }

//-------------------------------------------------------------------------------------------//  

  var nextChoice = 0

  //Fairness choice
  private def nextClause():Clause = {
    val clause = nextChoice match {
      case 0 => nextBestCluase()
      case 1 => nextBestCluaseInLevel()
      case 2 => nextBestCluaseInLastLevel()
    }
    nextChoice = (nextChoice + 1) % 3
    clause

//    nextBestCluase()
  }

  private def nextBestCluase():Clause = {
    var min:Clause = null
    var level = -1
    for (i <- minLevel to maxLevel) {
      val queue = free(i)
      if (queue != null && !queue.isEmpty) {
	val value = queue.head
	if (min == null || value.weight < min.weight) {
	  min = value
	  level = i
	}
      }
    }

    if (level == -1)
      null
    else
      free(level).dequeue()
  }

  private def nextBestCluaseInLastLevel():Clause = {
    var level = -1
    var i = maxLevel
    var cond = false
    while (!cond && i >= minLevel){
      val queue = free(i)
      cond = queue != null && !queue.isEmpty
      if (cond) level = i
      i -=1
    }

    if (cond) free(level).dequeue() else null
  }

  var nextLevel = minLevel

  //This technique might return "null" in the case that there is no free clause in nextLevel
  private def nextBestCluaseInLevel():Clause = {
    if(nextLevel < minLevel) nextLevel = minLevel
    val queue = free(nextLevel)
    if(queue != null && !queue.isEmpty){
      nextLevel = minLevel + (nextLevel - minLevel + 1) % (maxLevel - minLevel + 1)
      queue.dequeue()
    } else nextBestCluase()
  }

  private def nextBestNegCluase():Clause = {
    var min:Clause = null
    var level = -1
    for (i <- minLevel to maxLevel) {
      val queue = free(i)
      if (queue != null && !queue.isEmpty) {
	val list = queue.toList
	val value = list.filter(x => x.isQuery).sortWith((x,y) => x.weight < y.weight).head
	if (value != null && (min == null || value.weight < min.weight)) {
	  min = value
	  level = i
	}
      }
    }

    if (level == -1)
      null
    else {
      val list = free(level).toList.filterNot(_.equals(min))
      free(level).clear
      free(level) ++= list
      min
    }
  }

  private def resolve(clause:Clause){
    val level = clause.level
    if(clause.isQuery) {
      var time = System.nanoTime()
      val clauses = backward(clause)
      resolveTime += System.nanoTime() - time

      putInCaptured(clause)

      val cond = !clauses.isEmpty

      if(cond) now+=1
      
      putAsNegative(clauses, level+1)
      
      if (cond){
	updateGenerated(0, clauses.size, level+1)
	maxLevel = max(level+1, maxLevel)
      }

      val minUpdated = updateMinLevel(level)
      if (cond) 
	freeCaptured(level+1)
      else //TODO: check what happens when you remove this "else"
	if (minUpdated) freeAllCapturedOnlyIn(level + 1)
    } else {
      var time = System.nanoTime()
      val clauses = forward(clause)
      val clauses2 = backward2(clause)
      resolveTime += System.nanoTime() - time
      putInCaptured(clause)

      val clausesNoTau = Prover.removeTau(clauses)

      val cond = !clausesNoTau.isEmpty || !clauses2.isEmpty

      if (cond) now+=1

      putAsPositive(clausesNoTau, level+1)
      putAsNegative(clauses2, level+1)

      if (cond){
	updateGenerated(clausesNoTau.size, clauses2.size, level+1)
	maxLevel = max(level+1, maxLevel)
      }

      val minUpdated = updateMinLevel(level)
      if (cond) 
	freeCaptured(level+1)
      else //TODO: check what happens when you remove this "else"
	if (minUpdated) freeAllCapturedOnlyIn(level + 1)
    }
  }

  private def setWeight(clause:Clause){
    clause.weight = wManager.computeWeight(clause)
  }

  // direction "<-"
  private def backward(clause:Clause): List[Clause] = {
    var newClauses = List.empty[Clause]
    var i = 0

    while (i <= clause.level && i < positive.length) {
      val queue = positive(i)
      if (queue != null) {
      var j = 0
      var queueArray = queue.toArray[Clause]
      var cond = true
      while(j < queueArray.length && cond){

	val pos = queueArray(j)
	
	if (clause.resolved < pos.created){

	  pos.term match {
	    case Arrow(l1, l2) =>
	      resolveNum+=1

	      var subs = new Queue[Sub]()

	      if (BackwardSearch_ON) {
		if (unifier.unify(l2, clause.term, subs)) {

		  val newL1 = Sub.apply(l1, subs)
		  
		  //rename variables
		  val rnmSubs  = renameSubs(findVariables(newL1))
		  subs ++= rnmSubs
		  val newCls = Sub.apply(newL1, rnmSubs)
		  
		  val newClause = new Clause(pos.fsym ++ clause.fsym, clause.level + 1, new Action(clause, 0, pos, subs), true, newCls)
		  
		  setWeight(newClause)
		  
		  newClauses = newClause :: newClauses		
		}
	      
		subs  = new Queue[Sub]()
	      }

	      if (unifier.unify(pos.term, clause.term, subs)) {
		val newClause = new Clause(pos.fsym ++ clause.fsym, new Action(clause, 1, pos, subs))

		setWeight(newClause)

		putIfNewSolution(newClause)
	      }
	    case term =>
	      resolveNum+=1

	      val subs = new Queue[Sub]()
	      if (unifier.unify(term, clause.term, subs)) {
		val newClause = new Clause(pos.fsym ++ clause.fsym,  new Action(clause, 2, pos, subs))  
		setWeight(newClause)
		
		putIfNewSolution(newClause)
	      }
	  }
	} else cond = false
	j+=1
      }
      }
      i+=1
    }

    newClauses
  }

  private def weightSmallerThan(value:Double, clause:Clause) = true//clause.weight.value < value

  private final val MAX_WEIGHT:Double = 1000.

  // direction "->"
  private def backward2(clause:Clause):List[Clause] = {
    var newClauses = List.empty[Clause]
    var emptyClauses = List.empty[Clause]
    var i = 0
    
    while (i < clause.level && i < negative.length) {
      val queue = negative(i)
      if (queue != null) {
      var j = 0
      var queueArray = queue.toArray[Clause]
      var cond = true
      while(j < queueArray.length && cond){

	val neg = queueArray(j)
	if (clause.resolved < neg.created){

	  clause.term match {
	    case Arrow(l1, l2) =>
	      resolveNum+=1

	      var subs = new Queue[Sub]()

	      if (BackwardSearch_ON) {
		if (unifier.unify(l2, neg.term, subs)) {

		  val newL1 = Sub.apply(l1, subs)

		  //rename variables
		  val rnmSubs  = renameSubs(findVariables(newL1))
		  subs ++= rnmSubs
		  val newCls = Sub.apply(newL1, rnmSubs)

		  val newClause = new Clause(neg.fsym ++ clause.fsym, clause.level + 1, new Action(neg, 0, clause, subs), true, newCls)
		  setWeight(newClause)

		  newClauses = newClause :: newClauses
		}
		subs  = new Queue[Sub]()
	      }

	      if (unifier.unify(clause.term, neg.term, subs)) {
		val newClause = new Clause(neg.fsym ++ clause.fsym, new Action(neg, 1, clause, subs))  
		setWeight(newClause)
		putIfNewSolution(newClause)

	      }
	    case term =>
	      resolveNum+=1

	      val subs = new Queue[Sub]()
	      if (unifier.unify(term, neg.term, subs)) {
		val newClause = new Clause(neg.fsym ++ clause.fsym, new Action(neg, 2, clause, subs))
		setWeight(newClause)
		putIfNewSolution(newClause)

	      }
	  }
	} else cond = false
	j+=1

      }
      }
      i+=1
    }
    
    newClauses
  }


  // direction "<-" 
  private def forward(clause:Clause):List[Clause] = {
    var newClauses = List.empty[Clause]
    var i = 0

    while (i <= clause.level && i < positive.length) {
      val queue = positive(i)

      if (queue != null) {
	var j = 0
	var queueArray = queue.toArray[Clause]
	var cond = true
	while (j < queueArray.length && cond) {
	  val pos = queueArray(j)
          if (clause.resolved < pos.created) {
	    pos.term match {
	      case Arrow(l11, l12) =>
		clause.term match {
		  case Arrow(l21, l22) =>
		    var subs = new Queue[Sub]()
		    if (unifier.unify(l12, l21, subs)) {
		      val newL11 = Sub.apply(l11, subs)
		      val newL22 = Sub.apply(l22, subs)

		      //rename variables
		      val rnmSubs1  = renameSubs(findVariables(newL11))
		      val newCls1 = Sub.apply(newL11, rnmSubs1)
		      subs ++= rnmSubs1

		      val rnmSubs2  = renameSubs(findVariables(newL22))
		      val newCls2 = Sub.apply(newL22, rnmSubs2)
		      subs ++= rnmSubs2

	              val newCls = Arrow(newCls1, newCls2)

		      val newClause = new Clause(pos.fsym ++ clause.fsym, clause.level + 1, new Action(clause, 3 , pos, subs), false, newCls)
		      setWeight(newClause)
		      newClauses = newClause :: newClauses
		    }
		    subs = new Queue[Sub]()
		    if (unifier.unify(pos.term, l21, subs)){
		      val newL22 = Sub.apply(l22, subs)

		      val rnmSubs  = renameSubs(findVariables(newL22))
		      val newCls = Sub.apply(newL22, rnmSubs)
		      subs ++= rnmSubs
		      
	              val newClause = new Clause(pos.fsym ++ clause.fsym, clause.level + 1, new Action(clause, 4 , pos, subs), false, newCls)
		      setWeight(newClause)
		      newClauses = newClause :: newClauses
		    }
		case _ =>
	      }
	    case term =>
	      clause.term match {
		case Arrow(l21, l22) =>
		  var subs = new Queue[Sub]()
  		  if (unifier.unify(term, l21, subs)) {
		    val newL22 = Sub.apply(l22, subs)

		    //rename variables
		    val rnmSubs = renameSubs(findVariables(newL22))
		    val newCls = Sub.apply(newL22, rnmSubs)
		    subs ++= rnmSubs

		    val newClause = new Clause(pos.fsym ++ clause.fsym, clause.level + 1, new Action(clause, 5 , pos, subs), false, newCls)
		    setWeight(newClause)
		    newClauses = newClause :: newClauses
		  }
		case _ =>
	      }
	    }
	} else cond = false
	j+=1

      }
      }
      i+=1
    }
    
    newClauses
  }



  // direction "<-" 
/*  private def forward(clause:Clause):List[Clause] = {
    var newClauses = List.empty[Clause]
    var i = 0

    while (i <= clause.level && i < positive.length) {
      val queue = positive(i)

      if (queue != null) {
	var j = 0
	var queueArray = queue.toArray[Clause]
	var cond = true
	while (j < queueArray.length && cond) {
	  val pos = queueArray(j)
          if (clause.resolved < pos.created) {
	    val term = pos.term
	    clause.term match {
	      case Arrow(l1, l2) =>
		resolveNum+=1
		var subs = new Queue[Sub]()
 		if (unifier.unify(term, l1, subs)) {  

		  val newL2 = Sub.apply(l2, subs)

		  //rename variables
		  val rnmSubs = renameSubs(findVariables(newL2))
		  val newCls = Sub.apply(newL2, rnmSubs)
		  subs ++= rnmSubs

		  val newClause = new Clause(pos.fsym ++ clause.fsym, clause.level + 1, new Action(clause, 5 , pos, subs), false, newCls)
		  setWeight(newClause)
		  newClauses = newClause :: newClauses
		}
	      case _ =>
	    }
	} else cond = false
	j+=1

      }
      }
      i+=1
    }
    
    newClauses
  }
*/
  private def isSortedInc(a:Array[Clause]):Boolean = {
    if (a.length > 1){
      var created = a(0).created
      for(i <- 1 until a.length){
	if (created > a(i).created) return false
	else created = a(i).created
      }
    }
    true
  }

  private def isSortedDec(a:Array[Clause]):Boolean = {
    if (a.length > 1){
      var created = a(0).created
      for(i <- 1 until a.length){
	if (created < a(i).created) return false
	else created = a(i).created
      }
    }
    true
  }

  private var varCnt:Long = 0

  private def newName():String = {
    varCnt +=1
    "VP"+varCnt.toString
  }

  private def renameSub(oldName:String): RenameSub = new RenameSub(oldName, newName())
  private def renameSubs(oldNames:Queue[String]):Queue[Sub] = {
    var queue = new Queue[Sub]()
    oldNames.foreach {
      x => queue += renameSub(x)
    }
    queue
  }

  private def findVariables(term:Term): Queue[String] = {
    val set = new HashSet[String]()
    findVariables(term, set)
    var queue = new Queue[String]()
    queue ++= set
    queue
  }

  private def findVariables(term:Term, set:HashSet[String]): Unit = term match {
    case Variable(t1) => set.add(t1)
    case Instance(_, terms) =>
      terms.foreach(x => findVariables(x, set))
    case Arrow(l1, l2) =>
      findVariables(l1, set)
      findVariables(l2, set)
    case _ =>
  }

  private def freeCaptured(level:Int):Unit = {
    for(i <- level to maxLevel){
      val queue = captured(i)
      if (queue != null) {
	while(!queue.isEmpty && checkCritical(queue.head)){
	  free(i) += queue.dequeue()
	}
      }
    }
  }

  private def freeAllCapturedOnlyIn(level:Int):Unit = {
    val queue = captured(level)
    if (queue != null){
      while (!queue.isEmpty){
	free(level) += queue.dequeue()
      }
    }
  }

  private def checkCritical(clause:Clause):Boolean = {
    if (clause.isQuery){
      critical(clause.level - minLevel) <= generatedPos(now)(clause.level) - generatedPos(clause.resolved)(clause.level)
    } else {
      critical(clause.level - minLevel) <= generatedPos(now)(clause.level) - generatedPos(clause.resolved)(clause.level) + generatedNeg(now)(clause.level) - generatedNeg(clause.resolved)(clause.level)
    }
  }

  private def putInCaptured(clause:Clause):Unit = {
    clause.resolved = now
    var queue = captured(clause.level)
    if (queue == null) {
      queue = new PriorityQueue[Clause]()(resolving)
      captured(clause.level) = queue
    }
    queue += clause
  }

  private def updateMinLevel(level:Int):Boolean = {
    val cond = level == minLevel && free(level).isEmpty
    if (cond) minLevel+=1
    cond
  }

  private def putAsNegative(clauses:List[Clause], level:Int):Unit = {
    if (!clauses.isEmpty) {

      clauses.foreach {
	clause =>
	  
	  //free clauses
	  var queue = free(level)
	  if (queue == null) {
	    queue = new PriorityQueue[Clause]()(weighting)
	    free(level) = queue
	  }
	  queue += clause
	  	
	  //positive
	  queue = negative(level)
	  if (queue == null) {
	    queue = new PriorityQueue[Clause]()(creating)
	    negative(level) = queue
	  }
	  clause.created = now
	  queue += clause

      }
    }
  }

  private def putAsPositive(clauses:List[Clause], level:Int):Unit = {
    if (!clauses.isEmpty) {

      clauses.foreach {
	clause =>
	  
	  //free clauses
          var queue = free(level)
	  if (queue == null) {
	    queue = new PriorityQueue[Clause]()(weighting)
	    free(level) = queue
	  }
	  queue += clause
	
	  //positive
	  queue = positive(level)
	  if (queue == null) {
	    queue = new PriorityQueue[Clause]()(creating)
	    positive(level) = queue
	  }
	  clause.created = now
	  queue += clause
      }
    }
  }

  def updateGenerated(posSize:Int, negSize:Int, level:Int){
      //Pos generated
      var genPosNow = generatedPos(now)
      if (genPosNow == null) {
	genPosNow = generatedPos(now - 1).clone()
	generatedPos(now) = genPosNow
      }

      //level can be greater than maxLevel
      if (level == maxLevel+1) {
	genPosNow(level) = genPosNow(maxLevel) + posSize 
      } else {
	for (i <- level to maxLevel)
	  genPosNow(i) = genPosNow(i) + posSize 
      }

      //Neg generated
      var genNegNow = generatedNeg(now)
      if (genNegNow == null) {
	genNegNow = generatedNeg(now - 1).clone()
	generatedNeg(now) = genNegNow
      }

      //level can be greater than maxLevel
      if (level == maxLevel+1) {
	genNegNow(level) = genNegNow(maxLevel) + negSize 
      } else {
	for (i <- level to maxLevel)
	  genNegNow(i) = genNegNow(i) + negSize 
      }

    //TODO: update maxLevel
  }

  private def isTimeOut():Boolean = (timeout *(PROFILING_NUM+1)) <= (System.currentTimeMillis - startTime)

}

trait Unifier {

  def unify(tpe1:Term, tpe2:Term, subs:Queue[Sub]):Boolean

  protected def occurs(v:String, tpe:Term):Boolean = tpe match {
    case Variable(t1) => t1 == v
    case Instance(_, terms) => 
      terms.exists(x => occurs(v, x))
    case Arrow(l1, l2) =>
      occurs(v, l1) || occurs(v, l2)
    case _ => false
  }

  protected def unify(list: List[(Term, Term)], subs:Queue[Sub]):Boolean = {
    var cond = true
    var workingList = new ListBuffer[(Term, Term)]()
    workingList ++= list
    while (cond && !workingList.isEmpty) {
      val (t1, t2) = workingList.remove(0)
      cond = unify(Sub.apply(t1, subs), Sub.apply(t2, subs), subs)
    }
    cond
  }
}

class SimpleUnifier extends Unifier {

  def unify(tpe1:Term, tpe2:Term, subs:Queue[Sub]):Boolean = (tpe1, tpe2) match {
    case (Const(t1), Const(t2)) if(t1 == t2) => true
    case (Variable(t1), Variable(t2)) => 
      if (t1 != t2) subs += new RenameSub(t1, t2)
      true
    case (Variable(t1), t2:Term) if(!occurs(t1, t2)) => 
      subs += new VarTermSub(t1, t2)
      true
    case (t2:Term, Variable(t1)) if(!occurs(t1, t2)) =>
      subs += new VarTermSub(t1, t2)
      true
    case (Instance(name1, terms1), Instance(name2, terms2)) 
      if (name1 == name2 && terms1.length == terms2.length) =>
      unify(terms1 zip terms2, subs)

    case (Arrow(l11, l12), Arrow(l21, l22)) =>
      val newL11 = Sub.apply(l11, subs)
      val newL21 = Sub.apply(l21, subs)
      if (unify(newL11, newL21, subs)){
	val newL12 = Sub.apply(l12, subs)
	val newL22 = Sub.apply(l22, subs)
	unify(newL12, newL22, subs)
      } else false

    case _ => false
  }
}

class SubtypingUnifier extends Unifier {

  val simple = new SimpleUnifier()

  //We apply to the second one. This means that we need to check if t2 is in subtypes of t1
  def unify(tpe1:Term, tpe2:Term, subs:Queue[Sub]):Boolean = (tpe1, tpe2) match {
    case (c @ Const(t1), Const(t2)) if(t1.equals(t2) || c.isSubtype(t2)) => true
    case (Variable(t1), Variable(t2)) => 
      if (t1 != t2) subs += new RenameSub(t1, t2)
      true
    case (Variable(t1), t2:Term) if(!occurs(t1, t2)) => 
      subs += new VarTermSub(t1, t2)
      true
    case (t2:Term, Variable(t1)) if(!occurs(t1, t2)) =>
      subs += new VarTermSub(t1, t2)
      true
    case (Instance(name1, terms1), Instance(name2, terms2)) 
      if (name1 == name2 && terms1.length == terms2.length) =>
      unify(terms1 zip terms2, subs)   //TODO:here we should have "simple.unify" 

    case (Arrow(l11, l12), Arrow(l21, l22)) =>

      val newL11 = Sub.apply(l11, subs)
      val newL21 = Sub.apply(l21, subs)
      if (simple.unify(newL11, newL21, subs)){

	val newL12 = Sub.apply(l12, subs)
	val newL22 = Sub.apply(l22, subs)
	simple.unify(newL12, newL22, subs)
      } else false

    case _ => false
  }

}


object Prover {

  def removeTau(clauses:List[Clause]):List[Clause] = clauses.filterNot(x => isTau(x))

  private def isTau(clause:Clause):Boolean = {
    val last = findLastTerm(clause.term)
    checkSame(clause.term, last)
  }

  private def checkSame(term:Term, last:Term): Boolean = term match {
    case Arrow(l1, l2) =>  same(l1, last) || checkSame(l2, last) 
    case _ => false
  }

  private def same(term1:Term, term2:Term):Boolean = (term1, term2) match {
    case (Const(t1), Const(t2)) => t1 == t2
    case (Variable(t1), Variable(t2)) => t1 == t2
    case (Instance(name1, terms1), Instance(name2, terms2)) 
      if(name1 == name2 && terms1.length == terms2.length) =>
	same(terms1 zip terms2)
    case (Arrow(l11,l12), Arrow(l21,l22)) =>
      same(l11,l21) && same(l12,l22)
    case _=> false
  }

  private def same(terms:List[(Term, Term)]):Boolean = {
    var cond = true
    var workingList = new ListBuffer[(Term, Term)]()
    workingList ++= terms
    while (cond && !workingList.isEmpty) {
      val (t1, t2) = workingList.remove(0)
      cond = same(t1, t2)
    }
    cond
  }

  private def findLastTerm(term:Term): Term = term match {
    case Arrow(_, l2) => findLastTerm(l2)
    case t => t
  }
}

class Proof(val clause:Clause){
  //TODO:Implement
  def reconstruct():String = null
}

//Increasing order
class Weighting[T <: Clause] extends Ordering[T] {
  def compare(x: T, y: T): Int = y.weight.compare(x.weight)
}

//Increasing order
class Resolving[T <: Clause] extends Ordering[T] {
  def compare(x: T, y: T): Int = y.resolved - x.resolved
}

//Decreasing order
class Creating[T <: Clause] extends Ordering[T] {
  def compare(x: T, y: T): Int = x.created - y.created
}

//TODO: Put all relevant info in here
class ProverOutput(val solutions:List[String], val time:String, val declNum:Int)

}
