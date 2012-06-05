package ch.epfl.insynth.scheduler

import ch.epfl.insynth.env.TypeAssignment
import ch.epfl.insynth.env.Environment
import scala.collection.mutable.Queue
import ch.epfl.insynth.debug.Debug

trait Scheduler extends Listener {
  
  def hasFinished():Boolean
  
  def add(ta:TypeAssignment)
  
  def next():TypeAssignment
  
}

class BFSScheduler extends Scheduler {
  
  private var queue = Queue.empty[TypeAssignment]
  
  def hasFinished():Boolean = queue.isEmpty
  
  def add(ta:TypeAssignment){
    val active = ta.getProperties.isActive
    if (!active) {
      ta.getProperties.activate()
      queue.enqueue(ta)
    }
  }
  
  def next():TypeAssignment = {
    Debug("In Scheduler: queue.size = "+queue.size)
    
    val ta = queue.dequeue()
    ta.getProperties.deactivate()
    ta
  }
  
  def notify(ta:TypeAssignment){
    val properties = ta.getProperties
    if (!properties.isActive) add(ta)
  }
}