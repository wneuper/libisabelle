package edu.tum.cs.isabelle.japi;

import edu.tum.cs.isabelle.*;

public class Operations {

  private Operations() {}

  public static <I, O> Operation<I, O> fromCodecs(String name, Codec<I> enc, Codec<O> dec) {
    return new Operation<I, O>(name, enc, dec);
  }

  public static final Operation<String, String> HELLO =
    Operation$.MODULE$.Hello();
  
  public static final Operation<String, String> TESTSTR =
    Operation$.MODULE$.Teststr();
  
  public static final Operation<String, String> TESTINT =
    Operation$.MODULE$.Testint();

  public static final Operation<java.util.List<String>, Void> USE_THYS =
    Operation$.MODULE$.UseThys_Java();

}
