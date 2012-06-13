package ch.epfl.insynth.env.tests

import ch.epfl.insynth.env._
import ch.epfl.insynth.trees._
import ch.epfl.scala.trees.{Instance => ScalaInstance, Const => ScalaConst, _}

import org.junit.Test
import org.junit.Assert._

class DeclarationTest {

  private def makeDecl(name:String, scalaType:ScalaType) = new Declaration(name, TypeTransformer.transform(scalaType), scalaType)
  
  @Test
  def testDeclaration1() {
    val decl = makeDecl("ch.epfl.insynth.A.m1", ScalaConst("Int"))
    assertEquals(decl.getSimpleName, "m1")
  }
  
  @Test
  def testDeclaration2() {
    val decl = makeDecl("ch.epfl.insynth.A.m1", ScalaConst("Int"))
    assertEquals(decl.getParamSetTypes, TSet(List.empty))
  }
  
  @Test
  def testDeclaration3() {
    val decl = makeDecl("ch.epfl.insynth.A.m1", Method(null, List.empty[List[ScalaType]],ScalaConst("Int")))
    assertEquals(decl.getParamSetTypes, TSet(List.empty))
  }
  
  @Test
  def testDeclaration4() {
    val decl = makeDecl("ch.epfl.insynth.A.m1", Method(ScalaConst("T1"), List(List(ScalaConst("Boolean"))),ScalaConst("Int")))
    assertEquals(decl.getParamSetTypes, TSet(List(Const("T1"), Const("Boolean"))))
  }
  
  @Test
  def testDeclaration5() {
    val decl = makeDecl("ch.epfl.insynth.A.m1", Method(ScalaConst("T1"), List(List(ScalaConst("Boolean"))),ScalaConst("Int")))
    assertEquals(decl.getParamTypes, List(Const("T1"), Const("Boolean")))
  }
  
  @Test
  def testDeclaration6() {
    val decl = makeDecl("ch.epfl.insynth.A.m1", Method(ScalaConst("T1"), List(List(ScalaConst("Boolean"))),ScalaConst("Int")))
    assertEquals(decl.getReturnType, Const("Int"))
  }
  
  @Test
  def testDeclaration7() {
    val decl = makeDecl("ch.epfl.insynth.A.m1", Method(ScalaConst("T1"), List(List(ScalaConst("Boolean"))),ScalaConst("Int")))
    assertEquals(decl.getType, Arrow(TSet(List(Const("T1"), Const("Boolean"))), Const("Int")))
  }
}