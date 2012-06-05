package ch.epfl.insynth.env

import ch.epfl.insynth.trees._

class QueryBuilder(tpe:Type) {
  assert(tpe != null)
  
  private val inSynthRetType = Const("*Query*") 
  private val inSynthType = Arrow(TSet(tpe), inSynthRetType)
  
  def getQuery = Query(new Declaration("query", inSynthType), inSynthRetType, new InitialSender())
  
}

case class Query(decl:Declaration, inSynthRetType:Type, sender:InitialSender) {
  
  def getSolution = sender.getAnswers
  
  def getDeclaration = decl
  
  def getReturnType = inSynthRetType
  
  def getSender = sender
}