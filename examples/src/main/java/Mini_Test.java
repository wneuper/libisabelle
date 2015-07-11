package examples.src.main.java;

import java.io.File;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Vector;

import scala.math.BigInt;

import edu.tum.cs.isabelle.japi.JSystem;
import edu.tum.cs.isabelle.japi.Operations;
import isabelle.XML;

import examples.src.main.java.ConvertXML; //IMPORT-PROBLEM150705

public class Mini_Test {

  public static void main(String args[]) {
		     
    System.out.println("----- begin of mini-test cf. ~~/doc/test--isac-Java--isac-kernel.txt");

    // 2 obstacles for development within Eclipse:
    // A: ISABELLE_HOME needs to be set before JSystem.instance is called
    // B: dependencies between the many paths (1..5) below with respect to Java, sbt, isabelle build, Isabelle/Isac, Eclipse,
    //    where (9) is still open: ...
  ////---------------------------------------------------------------------------------------\\
    isabelle.Isabelle_System.init("/usr/local/isabisac", "");
    JSystem sys = JSystem.instance(new File("/home/wneuper/proto4/libisabelle/."), "Protocol");
  //\\---------------------------------------------------------------------------------------//
    //                                      \_______________(1)________________/   \__(2)___/
    //                                                       |                         |
    //                    ROOT of (2) made relative to (1)   +------- by ./sbt --------+   packed into libisabelle-full.jar
    // we run Mini_Test with:
    // xxxxxxx$ java -cp /home/wneuper/proto4/libisabelle/full/target/scala-2.11/libisabelle-full.jar examples.src.main.java.Mini_Test
    // \_(3)_/            \___________________________________(4)__________________________________/   \____________(5)_____________/
    //    |                                                    |                                                     |
    // partly (see (9)) freed for Eclipse by (1)         found by java                         relative to (1) by libisabelle-full.jar
    //                                                                                                             \_____(1..2)_____/
    // (6) Isabelle/Isac comes in by: Protocol imports ".../Frontend".                                                     /
    // (7) Embedding into Eclipse (in the future by Run Configurations): packed into isac-java.jar by Export in Eclipse __/
    // (8) we run all packed into isac-java.jar (with main BridgeMain):
    // libisabelle$ java -jar /home/wneuper/proto4/dist/isac-java.jar /home/wneuper/proto4/repos/isac-java/src/java/properties/BridgeMain.properties
    //
    // ... works, but the following still raises an error (where "/home/wneuper/proto4" is an example for "xxxxxxx"):
    // (9) xxxxxxx$ java -jar /home/wneuper/proto4/dist/isac-java.jar /home/wneuper/proto4/repos/isac-java/src/java/properties/BridgeMain.properties
    // Exception in thread "main" java.lang.RuntimeException: Bad session root directory: "/home/wneuper/proto4"
	//   at isabelle.Library$ERROR$.apply(library.scala:20)
    //                                               \__isac-java.jar__/
    //                                                       |
    //                                   contains libisabelle-full.jar as a library, 
    // where (2) is made relative to (1), 
    // but something concerning session management in libisabelle is still not relative to the directory libisabelle.
    // This refers to https://github.com/larsrh/libisabelle/commit/29bce1cdb2efe7d9f21e1a5d87f710299600c75f
    //         and to https://github.com/wneuper/libisabelle ...the changeset creating these updates.
    //
    // ?!?!?!?!?!?!?!?!?!?!?!?!?!?!? SO THERE ARE STILL UNRESOLVED DEPENDENCIES ?!?!?!?!?!?!?!?!?!?!?!?!?!?!?
    
    //List items = list("equality (x+1=(2::real))", "solveFor x", "solutions L");
    ArrayList<String> items = new ArrayList<>();
    items.add("equality (x+1=(2::real))");
    items.add("solveFor x");
    items.add("solutions L");
    //List pbl = list("sqroot-test","univariate","equation","test");
    ArrayList<String> pbl = new ArrayList<>();
    pbl.add("sqroot-test");
    pbl.add("univariate");
    pbl.add("equation");
    pbl.add("test");
    //List met = list("Test","squ-equ-test-subpbl1");
    ArrayList<String> met = new ArrayList<>();
    met.add("Test");
    met.add("squ-equ-test-subpbl1");
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

    Vector<Integer> from_path = new Vector<>();
    String from_kind = "Pbl";
    Vector<Integer> to_path = new Vector<>();
    String to_kind = "Pbl";
    int level = 0;
    XML.Tree GET_FORMULAE_out = sys.invoke(Operations.GET_FORMULAE,         //step 4
      ConvertXML.get_formulae(new scala.math.BigInt(BigInteger.valueOf(calcid)), 
      from_path, from_kind, to_path, to_kind, new scala.math.BigInt(BigInteger.valueOf(level)), "false"));
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
