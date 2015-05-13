import java.io.File;
import java.math.BigInteger;
import scala.math.BigInt;

import edu.tum.cs.isabelle.japi.JSystem;
import edu.tum.cs.isabelle.japi.Operations;

public class Mini_Test {

  public static void main(String args[]) {
		     
    System.out.println("begin of mini-test cf. ~~/doc/test--isac-Java--isac-kernel.txt");

    JSystem sys = JSystem.instance(new File("."), "Protocol");
    int i = 1;
    System.out.println(sys.invoke(Operations.ITERATOR,       //step 2
      new scala.math.BigInt(BigInteger.valueOf(i))));		    
    System.out.println(sys.invoke(Operations.MOVEACTIVEROOT, //step 3
      new scala.math.BigInt(BigInteger.valueOf(i))));
    System.out.println(sys.invoke(Operations.AUTOCALC,       //step 7
      new scala.math.BigInt(                                 BigInteger.valueOf(i))));
    System.out.println(sys.invoke(Operations.DELCALC,        //step 13
      new scala.math.BigInt(BigInteger.valueOf(i))));
				    
    sys.dispose();
    //$ ./sbt full/assembly
    //$ java -cp full/target/scala-2.11/libisabelle-full.jar Mini_Test
    }

}
