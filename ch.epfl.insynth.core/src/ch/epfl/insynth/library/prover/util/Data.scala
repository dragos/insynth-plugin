package ch.epfl.insynth.library.prover.util

import scala.math._
import scala.collection.mutable.HashMap

class SparseArray[T] {

  private var map = new HashMap[Long, T]()

  private var minIndex:Long = Long.MaxValue
  private var maxIndex:Long = -1

  def apply(key:Long):T = map(key)

  def update(key:Long, t:T) {
    if (key < 0) throw new Exception("Negative index in SparseArray: "+ key)

    maxIndex = max(key, maxIndex)
    minIndex = min(key, minIndex)
    map(key) = t
  }

  def isEmpty = map.isEmpty

  override def clone(): SparseArray[T] = {
    var array = new SparseArray[T]()
    array.map = map.clone()
    array
  }
  
  def lastIndex = maxIndex 
  def firstIndex = minIndex

  def clear = {
    map = new HashMap[Long, T]()
    maxIndex = -1
    minIndex = Long.MaxValue
  }

  def indexes(first:Long, last:Long): List[Long] = {
    val list = map.keys.filter(x => (x >= first && x <= last)).toList
    list.sortWith((x,y) => x < y)
  }

  def indexesFrom(first:Long) = indexes(first, lastIndex)

  def indexesTo(last:Long) = indexes(firstIndex, last)

  def indexes():List[Long] = indexes(firstIndex, lastIndex)

}

class DenseArray[T: Manifest] {

  private final val INIT_LENGTH:Int = 100

  private var array = new Array[T](INIT_LENGTH)

  def apply(key:Int):T = {
    if (key < 0) throw new Exception("Negative index in DenseArray: "+ key)
    ensureSize(key)
    array(key)
  }

  def update(key:Int, t:T) {
    if (key < 0) throw new Exception("Negative index in DenseArray: "+ key)
    ensureSize(key)
    array(key) = t
  }

  private def ensureSize(key:Int){
    if (key >= array.length) {
      val newArray = new Array[T](2 * array.length)
      System.arraycopy(array, 0, newArray, 0, array.length)
      array = newArray
    }
  }

  def isEmpty = array.isEmpty

  override def clone(): DenseArray[T] = {
    var newArray = new DenseArray[T]()
    newArray.array = array.clone()
    newArray
  }
  
  def length = array.length

  def clear = {
    array = new Array[T](INIT_LENGTH)
  }

  def mkString = array.mkString

  override def toString = mkString 

  def data = array

  def elements:List[T] = {
    var list = List[T]()
    for(i <- 0 until array.length){
      if (array(i) != null){
	list = array(i) :: list
      }
    }
    list
  }
}
