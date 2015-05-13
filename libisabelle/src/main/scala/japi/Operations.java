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

  // for Mini_Test.java -----------------------------------------------
  public static final Operation<scala.math.BigInt, XML.Tree> ITERATOR =
		    Operation$.MODULE$.Iterator();
  public static final Operation<scala.math.BigInt, XML.Tree> MOVEACTIVEROOT =
		    Operation$.MODULE$.moveActiveRoot();
  public static final Operation<scala.math.BigInt, XML.Tree> DELCALC =
		    Operation$.MODULE$.DEconstrCalcTree();


  //-------------------------------------------------------------------
  public static final Operation<java.util.List<String>, Void> USE_THYS =
    Operation$.MODULE$.UseThys_Java();

}
