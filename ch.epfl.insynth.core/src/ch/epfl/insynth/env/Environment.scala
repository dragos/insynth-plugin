package ch.epfl.insynth.env

import ch.epfl.insynth.trees.Type
import ch.epfl.insynth.scheduler.Scheduler

abstract class Environment {
  
  protected var clusters = Map.empty[Type, Cluster]
  
  def getCluster(tpe:Type) = {
    clusters.get(tpe) match {
      case Some(cluster) => cluster
      case None => null
    }
  }
  
  def addCluster(cluster:Cluster) {
    assert(cluster != null)    
    clusters += (cluster.getType -> cluster) 
  }
  
  def addTypeAssignment(ta:TypeAssignment)
  
  def getTypeAssignments(tpe:Type):List[TypeAssignment]

  def getTypeSet:Set[Type]
  
}

class InitialEnvironment extends Environment {
  
  override def addTypeAssignment(ta:TypeAssignment) {
    val tpe = ta.getReturnType
    val cluster = this.getCluster(tpe)
    if(cluster != null){
      cluster.addTypeAssignment(ta)
    } else {
      val newCluster = new InitialCluster(tpe)
      newCluster.addTypeAssignment(ta)
      this.addCluster(newCluster)
    }
  }
  
  override def getTypeAssignments(tpe:Type) = {
    val cluster = this.getCluster(tpe)
    if (cluster != null) cluster.getTypeAssignments
    else List.empty[TypeAssignment]
  }
  
  override def getTypeSet = Set.empty[Type]
}

class DeltaEnvironment(parent:Environment) extends Environment {
  assert(parent != null)
  
  private var typeSet = parent.getTypeSet
  
  override def addTypeAssignment(ta:TypeAssignment) {
    val tpe = ta.getType
    if (!typeSet.contains(tpe)){
      typeSet += tpe
      
      val retType = ta.getReturnType
    
      //get a cluster from a child
      val cluster = this.getCluster(retType)
      if(cluster != null){
        cluster.addTypeAssignment(ta)
      } else {
      
        //get a cluster from a parent 
        val parentCluster = parent.getCluster(retType)
      
        //anyways create new cluster that may or may not point to the parent cluster
        val newCluster = if (parentCluster != null) new DeltaCluster(retType, parentCluster)
                                             else new InitialCluster(retType)

        //add new cluster
        this.addCluster(newCluster)
        newCluster.addTypeAssignment(ta)
      }
    }
  }

  override def getTypeSet = typeSet
  
  override def getTypeAssignments(tpe:Type) = {
    val cluster = this.getCluster(tpe)
    if (cluster != null) {
      cluster.getTypeAssignments
    } else {
      val parentCluster = parent.getCluster(tpe)
      if (parentCluster != null) {
        parentCluster.getTypeAssignments
      } else List.empty[TypeAssignment]
    }
  }
}