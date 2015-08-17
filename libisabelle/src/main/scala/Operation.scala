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
  val appendFormula = implicitly[XML.Tree, XML.Tree]("append_form") 
  val autoCalculate = implicitly[XML.Tree, XML.Tree]("autocalculate")  
  val applyTactic = implicitly[XML.Tree, XML.Tree]("apply_tac")
  val CalcTree = implicitly[XML.Tree, XML.Tree]("calctree")
  val checkContext = implicitly[XML.Tree, XML.Tree]("check_ctxt")

  val DEconstrCalcTree = implicitly[scala.math.BigInt, XML.Tree]("deconstrcalctree")
  val fetchApplicableTactics = implicitly[XML.Tree, XML.Tree]("fetch_applicable_tacs")
  val fetchProposedTactic = implicitly[XML.Tree, XML.Tree]("fetch_proposed_tac")
  val findFillpatterns = implicitly[XML.Tree, XML.Tree]("find_fill_patts")
  val getAccumulatedAsms = implicitly[XML.Tree, XML.Tree]("get_accumulated_asms")

  val getActiveFormula = implicitly[XML.Tree, XML.Tree]("get_active_form")
  val getAssumptions = implicitly[XML.Tree, XML.Tree]("get_asms")
  val getFormulaeFromTo = implicitly[XML.Tree, XML.Tree]("getformulaefromto")
  val getTactic = implicitly[XML.Tree, XML.Tree]("get_tac")
  val initContext = implicitly[XML.Tree, XML.Tree]("init_ctxt")

  val inputFillFormula = implicitly[XML.Tree, XML.Tree]("input_fill_form")
  val interSteps = implicitly[XML.Tree, XML.Tree]("inter_steps")
  val Iterator = implicitly[scala.math.BigInt, XML.Tree]("iterator")
  val modelProblem = implicitly[scala.math.BigInt, XML.Tree]("model_pbl")
  val modifyCalcHead = implicitly[XML.Tree, XML.Tree]("modify_calchead")

  val moveActiveCalcHead = implicitly[XML.Tree, XML.Tree]("move_active_calchead")
  val moveActiveDown = implicitly[XML.Tree, XML.Tree]("move_active_down")
  val moveActiveFormula = implicitly[XML.Tree, XML.Tree]("move_active_form")
  val moveActiveLevelDown = implicitly[XML.Tree, XML.Tree]("move_active_levdown")
  val moveActiveLevelUp = implicitly[XML.Tree, XML.Tree]("move_active_levup")

  val moveActiveRoot = implicitly[scala.math.BigInt, XML.Tree]("moveactiveroot")
  val moveActiveUp = implicitly[XML.Tree, XML.Tree]("move_active_up")
  val moveCalcHead = implicitly[XML.Tree, XML.Tree]("move_calchead")
  val moveDown = implicitly[XML.Tree, XML.Tree]("move_down")
  val moveLevelDown = implicitly[XML.Tree, XML.Tree]("move_levdn")

  val moveLevelUp = implicitly[XML.Tree, XML.Tree]("move_levup")
  val moveRoot = implicitly[XML.Tree, XML.Tree]("move_root")
  val moveUp = implicitly[XML.Tree, XML.Tree]("move_up")
  val refFormula = implicitly[XML.Tree, XML.Tree]("refformula")
  val refineProblem = implicitly[XML.Tree, XML.Tree]("refine_pbl")

  val replaceFormula = implicitly[XML.Tree, XML.Tree]("replace_form")
  val requestFillformula = implicitly[XML.Tree, XML.Tree]("request_fill_form")
  val resetCalcHead = implicitly[XML.Tree, XML.Tree]("reset_calchead")
  val setContext = implicitly[XML.Tree, XML.Tree]("set_ctxt")
  val setMethod = implicitly[XML.Tree, XML.Tree]("set_met")

  val setNextTactic = implicitly[XML.Tree, XML.Tree]("set_next_tac")
  val setProblem = implicitly[XML.Tree, XML.Tree]("set_pbl")
  val setTheory = implicitly[XML.Tree, XML.Tree]("set_thy")
  
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
