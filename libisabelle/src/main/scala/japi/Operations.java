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
  public static final Operation<XML.Tree, XML.Tree> AUTO_CALC =
		    Operation$.MODULE$.autoCalculate();
  public static final Operation<XML.Tree, XML.Tree> CALC_TREE =
    Operation$.MODULE$.CalcTree();
  public static final Operation<scala.math.BigInt, XML.Tree> DEL_CALC =
		    Operation$.MODULE$.DEconstrCalcTree();
  public static final Operation<XML.Tree, XML.Tree> GET_FORMULAE =
		    Operation$.MODULE$.getFormulaeFromTo();
  public static final Operation<scala.math.BigInt, XML.Tree> ITERATOR =
    Operation$.MODULE$.Iterator();
  public static final Operation<scala.math.BigInt, XML.Tree> MOVE_ACTIVE_ROOT =
    Operation$.MODULE$.moveActiveRoot();
  public static final Operation<XML.Tree, XML.Tree> REF_FORMULA =
    Operation$.MODULE$.refFormula();

  //-------------------------------------------------------------------
  public static final Operation<java.util.List<String>, Void> USE_THYS =
    Operation$.MODULE$.UseThys_Java();

}
