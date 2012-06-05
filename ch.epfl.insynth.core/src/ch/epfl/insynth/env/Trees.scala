package ch.epfl.insynth.env

import ch.epfl.insynth.trees._

trait Node

case class SimpleNode(decls:List[Declaration], params:Map[Type, ContainerNode]) extends Node {
  def getDecls = decls
  def getParams = params
}

/**
 * container for tree nodes
 */
case class ContainerNode(var nodes:Set[SimpleNode]) extends Node {
  
  def this() = this(Set.empty)
  
  def addNode(node:SimpleNode){
    nodes += node
  }
  
  def getNodes = nodes
}