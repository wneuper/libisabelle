package edu.tum.cs.isabelle

import scala.collection.JavaConverters._

import isabelle._

object Operation {

  def implicitly[I : Codec, O : Codec](name: String): Operation[I, O] =
    Operation(name, Codec[I], Codec[O])

  // for Test_PIDE.java
  val Hello = implicitly[String, String]("hello")
  val Teststr = implicitly[String, String]("teststr")
  val Testit = implicitly[scala.math.BigInt, XML.Tree]("testit")
  
  // Protoco.thy operation_setup for all isabisac's Math_Engine --------
  val autoCalculate = implicitly[XML.Tree, XML.Tree]("autocalculate") 
  val CalcTree = implicitly[XML.Tree, XML.Tree]("calctree")
  val DEconstrCalcTree = implicitly[scala.math.BigInt, XML.Tree]("deconstrcalctree")
  val getFormulaeFromTo = implicitly[XML.Tree, XML.Tree]("getformulaefromto")
  val Iterator = implicitly[scala.math.BigInt, XML.Tree]("iterator")
  val moveActiveRoot = implicitly[scala.math.BigInt, XML.Tree]("moveactiveroot")
  val refFormula = implicitly[XML.Tree, XML.Tree]("refformula")

  //-------------------------------------------------------------------
  val UseThys = implicitly[List[String], Unit]("use_thys")

  protected[isabelle] val UseThys_Java =
    Operation("use_thys",
      Codec[List[String]].transform[java.util.List[String]](_.asJava, _.asScala.toList),
      Codec[Unit].transform[Void](_ => null, _ => ()))

}

case class Operation[I, O](name: String, toProver: Codec[I], fromProver: Codec[O]) {
  def encode(i: I): XML.Tree = toProver.encode(i)
  def decode(xml: XML.Tree): Result[O] =
    Codec.exnResult(fromProver).decode(xml).right.map(Exn.release)
}
