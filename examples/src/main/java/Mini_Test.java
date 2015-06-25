import java.io.File;
import java.math.BigInteger;

import scala.math.BigInt;
import scala.collection.immutable.List;
import scala.collection.immutable.List$;
import scala.collection.immutable.$colon$colon;

import edu.tum.cs.isabelle.japi.JSystem;
import edu.tum.cs.isabelle.japi.Operations;
import isabelle.XML;


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
    // Isac comes in by: Protocol imports ".../Frontend"

    List items = list("equality (x+1=(2::real))", "solveFor x", "solutions L");
    List pbl = list("sqroot-test","univariate","equation","test");
    List met = list("Test","squ-equ-test-subpbl1");
    XML.Tree CALC_TREE_out = sys.invoke(Operations.CALC_TREE,               //step 1
      ConvertXML.calc_tree(items, "Test", pbl, met));
    System.out.println("# 1 # " + CALC_TREE_out);

    int calcid = 1;
    XML.Tree ITERATOR_out = sys.invoke(Operations.ITERATOR,                 //step 2
      new scala.math.BigInt(BigInteger.valueOf(calcid)));
    System.out.println("# 2 # " + ITERATOR_out);

    XML.Tree MOVE_ACTIVE_ROOT_out = sys.invoke(Operations.MOVE_ACTIVE_ROOT, //step 3
      new scala.math.BigInt(BigInteger.valueOf(calcid)));
    System.out.println("# 3 # " + MOVE_ACTIVE_ROOT_out);

    XML.Tree GET_FORMULAE_out = sys.invoke(Operations.GET_FORMULAE,         //step 4
      ConvertXML.get_formulae(new scala.math.BigInt(BigInteger.valueOf(calcid)), 
      "Pbl", "Pbl", new scala.math.BigInt(BigInteger.valueOf(0)), "false"));
    System.out.println("# 4 # " + GET_FORMULAE_out);

    XML.Tree REF_FORMULA_out = sys.invoke(Operations.REF_FORMULA,           //step 6
      ConvertXML.ref_formula(new scala.math.BigInt(BigInteger.valueOf(calcid)), "Pbl"));
    System.out.println("# 6 # " + REF_FORMULA_out);

    String auto = "CompleteCalc";
    XML.Tree AUTO_CALC_out = sys.invoke(Operations.AUTO_CALC,               //step 7
      ConvertXML.auto_calculate(new scala.math.BigInt(BigInteger.valueOf(calcid)), auto));
    System.out.println("# 7 # " + AUTO_CALC_out);

    REF_FORMULA_out = sys.invoke(Operations.REF_FORMULA,                    //step 10
      ConvertXML.ref_formula(new scala.math.BigInt(BigInteger.valueOf(calcid)), "Res"));
    System.out.println("# 10 # " + REF_FORMULA_out);

    XML.Tree DEL_CALC_out = sys.invoke(Operations.DEL_CALC,                 //step 13
      new scala.math.BigInt(BigInteger.valueOf(calcid)));
    System.out.println("# 13 # " + DEL_CALC_out);
    
    System.out.println("----- end of mini-test cf. ~~/doc/test--isac-Java--isac-kernel.txt");
				    
    sys.dispose();
    //$ ./sbt full/assembly
    //$ java -cp full/target/scala-2.11/libisabelle-full.jar Mini_Test
    }

}
