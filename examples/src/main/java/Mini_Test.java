import java.io.File;
import java.math.BigInteger;

import scala.math.BigInt;
import scala.collection.immutable.List;
import scala.collection.immutable.List$;
import scala.collection.immutable.$colon$colon;

import edu.tum.cs.isabelle.japi.JSystem;
import edu.tum.cs.isabelle.japi.Operations;

public class Mini_Test {

  //http://stackoverflow.com/questions/4524868/can-i-use-scala-list-directly-in-java
  public static <T> List<T> list(T ... ts) {
      List<T> result = List$.MODULE$.empty();
      for(int i = ts.length; i > 0; i--) {
          result = new $colon$colon(ts[i - 1], result);
      }
      return result;
  }

  public static void main(String args[]) {
		     
    System.out.println("----- begin of mini-test cf. ~~/doc/test--isac-Java--isac-kernel.txt");

    JSystem sys = JSystem.instance(new File("."), "Protocol");

    List items = list("equality (x+1=(2::real))", "solveFor x", "solutions L");
    List pbl = list("sqroot-test","univariate","equation","test");
    List met = list("Test","squ-equ-test-subpbl1");
    System.out.println(sys.invoke(Operations.CALC_TREE,        //step 1
      ConvertXML.calc_tree(items, "Test", pbl, met)));		    
    int calcid = 1;
    System.out.println(sys.invoke(Operations.ITERATOR,         //step 2
      new scala.math.BigInt(BigInteger.valueOf(calcid))));		    
    System.out.println(sys.invoke(Operations.MOVE_ACTIVE_ROOT, //step 3
      new scala.math.BigInt(BigInteger.valueOf(calcid))));
    System.out.println(sys.invoke(Operations.GET_FORMULAE,     //step 4
      ConvertXML.get_formulae(new scala.math.BigInt(BigInteger.valueOf(calcid)), 
      "Pbl", "Pbl", new scala.math.BigInt(BigInteger.valueOf(0)), "false")));

    //step 6
    
    String auto = "CompleteCalc";
    System.out.println(sys.invoke(Operations.AUTO_CALC,        //step 7
      ConvertXML.auto_calculate(new scala.math.BigInt(BigInteger.valueOf(calcid)), auto)));

    //step 10
    
    System.out.println(sys.invoke(Operations.DEL_CALC,         //step 13
      new scala.math.BigInt(BigInteger.valueOf(calcid))));
    
    System.out.println("----- end of mini-test cf. ~~/doc/test--isac-Java--isac-kernel.txt");
				    
    sys.dispose();
    //$ ./sbt full/assembly
    //$ java -cp full/target/scala-2.11/libisabelle-full.jar Mini_Test
    }

}
