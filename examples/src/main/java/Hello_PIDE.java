package examples.src.main.java;

import edu.tum.cs.isabelle.japi.*;

import java.io.File;
import java.util.Arrays;
import java.math.BigInteger;

import scala.math.BigInt;

public class Hello_PIDE {

  public static void main(String args[]) {
     
	System.out.println("Hello I'm Hello_PIDE!");

    isabelle.Isabelle_System.init("/usr/local/isabisac", "");
    JSystem sys = JSystem.instance(new File("/home/wneuper/proto4/libisabelle/."), "Protocol");
    System.out.println(sys.invoke(Operations.HELLO, "europa"));
    System.out.println(sys.invoke(Operations.TESTSTR, "xxx"));
    int i = 1;
    System.out.println(sys.invoke(Operations.TESTIT, 
      new scala.math.BigInt(BigInteger.valueOf(i))));
    
    sys.dispose();
    //   $ java -cp full/target/scala-2.11/libisabelle-full.jar Hello_PIDE

  }

}
