package src.main.java;

import java.io.File;
import java.math.BigInteger;

import edu.tum.cs.isabelle.japi.JSystem;
import edu.tum.cs.isabelle.japi.Operations;

public class Mini_Test {

	  public static void main(String args[]) {
		     
			System.out.println("begin of mini-test cf. ~~/doc/test--isac-Java--isac-kernel.txt");

		    JSystem sys = JSystem.instance(new File("."), "Protocol");
		    int i = 1;
		    System.out.println(sys.invoke(Operations.ITERATOR, 
		      new scala.math.BigInt(BigInteger.valueOf(i))));
		    
		    sys.dispose();
		    //$ java -cp full/target/scala-2.11/libisabelle-full.jar Mini_Test
		  }

}
