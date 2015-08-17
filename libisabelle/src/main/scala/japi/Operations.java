package edu.tum.cs.isabelle.japi;

import edu.tum.cs.isabelle.*;
import java.math.BigInteger;
import isabelle.XML;

public class Operations {

  private Operations() {}

  public static <I, O> Operation<I, O> fromCodecs(String name, Codec<I> enc, Codec<O> dec) {
    return new Operation<I, O>(name, enc, dec);
  }

  // for Test_PIDE.java
  public static final Operation<String, String> HELLO =
    Operation$.MODULE$.Hello();
  
  public static final Operation<String, String> TESTSTR =
    Operation$.MODULE$.Teststr();
  
  public static final Operation<scala.math.BigInt, XML.Tree> TESTIT =
    Operation$.MODULE$.Testit();

  // Protoco.thy operation_setup for all isabisac's Math_Engine --------
  public static final Operation<XML.Tree, XML.Tree> APPEND_FORM =
    Operation$.MODULE$.appendFormula();
  public static final Operation<XML.Tree, XML.Tree> AUTO_CALC =
    Operation$.MODULE$.autoCalculate();
  public static final Operation<XML.Tree, XML.Tree> APPLY_TAC =
    Operation$.MODULE$.applyTactic();
  public static final Operation<XML.Tree, XML.Tree> CALC_TREE =
    Operation$.MODULE$.CalcTree();
  public static final Operation<XML.Tree, XML.Tree> CHECK_CTXT =
    Operation$.MODULE$.checkContext();
  public static final Operation<scala.math.BigInt, XML.Tree> DEL_CALC =
    Operation$.MODULE$.DEconstrCalcTree();
  public static final Operation<XML.Tree, XML.Tree> FETCH_APPL_TACS =
    Operation$.MODULE$.fetchApplicableTactics();
  public static final Operation<XML.Tree, XML.Tree> FETCH_PROP_TAC =
    Operation$.MODULE$.fetchProposedTactic();
  public static final Operation<XML.Tree, XML.Tree> FIND_FILL_PATTS =
    Operation$.MODULE$.findFillpatterns();
  public static final Operation<XML.Tree, XML.Tree> GET_ACC_ASMS =
    Operation$.MODULE$.getAccumulatedAsms();
  public static final Operation<XML.Tree, XML.Tree> GET_ACTIVE_FORM =
    Operation$.MODULE$.getActiveFormula();
  public static final Operation<XML.Tree, XML.Tree> GET_ASMS =
    Operation$.MODULE$.getAssumptions();
  public static final Operation<XML.Tree, XML.Tree> GET_FORMULAE =
    Operation$.MODULE$.getFormulaeFromTo();
  public static final Operation<XML.Tree, XML.Tree> GET_TAC =
    Operation$.MODULE$.getTactic();
  public static final Operation<XML.Tree, XML.Tree> INIT_CTXT =
    Operation$.MODULE$.initContext();
  public static final Operation<XML.Tree, XML.Tree> INPUT_FILL_FORM =
    Operation$.MODULE$.inputFillFormula();
  public static final Operation<XML.Tree, XML.Tree> INTER_STEPS =
    Operation$.MODULE$.interSteps();
  public static final Operation<scala.math.BigInt, XML.Tree> ITERATOR =
    Operation$.MODULE$.Iterator();
  public static final Operation<scala.math.BigInt, XML.Tree> MODEL_PBL =
    Operation$.MODULE$.modelProblem();
  public static final Operation<XML.Tree, XML.Tree> MODIFY_CALCHEAD =
    Operation$.MODULE$.modifyCalcHead();
  public static final Operation<XML.Tree, XML.Tree> MOVE_ACTIVE_CALCHEAD =
    Operation$.MODULE$.moveActiveCalcHead();
  public static final Operation<XML.Tree, XML.Tree> MOVE_ACTIVE_DOWN =
    Operation$.MODULE$.moveActiveDown();
  public static final Operation<XML.Tree, XML.Tree> MOVE_ACTIVE_FORM =
    Operation$.MODULE$.moveActiveFormula();
  public static final Operation<XML.Tree, XML.Tree> MOVE_ACTIVE_LEVDN =
    Operation$.MODULE$.moveActiveLevelDown();
  public static final Operation<XML.Tree, XML.Tree> MOVE_ACTIVE_LEVUP =
    Operation$.MODULE$.moveActiveLevelUp();
  public static final Operation<scala.math.BigInt, XML.Tree> MOVE_ACTIVE_ROOT =
    Operation$.MODULE$.moveActiveRoot();
  public static final Operation<XML.Tree, XML.Tree> MOVE_ACTIVE_UP =
    Operation$.MODULE$.moveActiveUp();
  public static final Operation<XML.Tree, XML.Tree> MOVE_CALCHEAD =
    Operation$.MODULE$.moveCalcHead();
  public static final Operation<XML.Tree, XML.Tree> MOVE_DOWN =
    Operation$.MODULE$.moveDown();
  public static final Operation<XML.Tree, XML.Tree> MOVE_LEVDN =
    Operation$.MODULE$.moveLevelDown();
  public static final Operation<XML.Tree, XML.Tree> MOVE_LEVUP =
    Operation$.MODULE$.moveLevelUp();
  public static final Operation<XML.Tree, XML.Tree> MOVE_ROOT =
    Operation$.MODULE$.moveRoot();
  public static final Operation<XML.Tree, XML.Tree> MOVE_UP =
    Operation$.MODULE$.moveUp();
  public static final Operation<XML.Tree, XML.Tree> REF_FORMULA =
    Operation$.MODULE$.refFormula();
  public static final Operation<XML.Tree, XML.Tree> REFINE_PBL =
    Operation$.MODULE$.refineProblem();
  public static final Operation<XML.Tree, XML.Tree> REPLACE_FORM =
    Operation$.MODULE$.replaceFormula();
  public static final Operation<XML.Tree, XML.Tree> REQUEST_FILL_FORM =
    Operation$.MODULE$.requestFillformula();
  public static final Operation<XML.Tree, XML.Tree> RESET_CALCHEAD =
    Operation$.MODULE$.resetCalcHead();
  public static final Operation<XML.Tree, XML.Tree> SET_CTXT =
    Operation$.MODULE$.setContext();
  public static final Operation<XML.Tree, XML.Tree> SET_MET =
    Operation$.MODULE$.setMethod();
  public static final Operation<XML.Tree, XML.Tree> SET_NEXT_TAC =
    Operation$.MODULE$.setNextTactic();
  public static final Operation<XML.Tree, XML.Tree> SET_PBL =
    Operation$.MODULE$.setProblem();
  public static final Operation<XML.Tree, XML.Tree> SET_THY =
    Operation$.MODULE$.setTheory();


  //-------------------------------------------------------------------
  public static final Operation<java.util.List<String>, Void> USE_THYS =
    Operation$.MODULE$.UseThys_Java();

}
