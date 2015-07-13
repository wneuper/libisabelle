package examples.src.main.java;

import java.io.File;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Vector;

import scala.math.BigInt;
import edu.tum.cs.isabelle.japi.JSystem;
import edu.tum.cs.isabelle.japi.Operations;
import isabelle.XML;
import examples.src.main.java.ConvertXML;
import examples.src.main.java.IntIntCompound;
import examples.src.main.java.IntPosCompound;
import examples.src.main.java.IntCalcChangedCompound;

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
    
    //----- step 1 ----------------------------------------------------------------
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
    XML.Tree CALC_TREE_out = sys.invoke(Operations.CALC_TREE,
      ConvertXML.calc_tree(items, "Test", pbl, met));
    int calcid = (ConvertXML.calc_tree_out(CALC_TREE_out)).intValue();
    System.out.println("# 1 # " + CALC_TREE_out);

    //----- step 2 ----------------------------------------------------------------
    XML.Tree ITERATOR_out = sys.invoke(Operations.ITERATOR,
      new scala.math.BigInt(BigInteger.valueOf(calcid)));
    IntIntCompound int_int = ConvertXML.iterator_out(ITERATOR_out);
    calcid = int_int.get_calcid().intValue();
    int userid = int_int.get_userid().intValue();
    System.out.println("# 2 # " + ITERATOR_out);

    //----- step 3 ----------------------------------------------------------------
    XML.Tree MOVE_ACTIVE_ROOT_out = sys.invoke(Operations.MOVE_ACTIVE_ROOT,
      new scala.math.BigInt(BigInteger.valueOf(calcid)));
    IntPosCompound calcid_pos = ConvertXML.move_active_root_out(MOVE_ACTIVE_ROOT_out);
    calcid = calcid_pos.get_calcid();
    Vector<Integer> ints = calcid_pos.get_ints();
    String kind = calcid_pos.get_kind();
    System.out.println("# 3 # " + MOVE_ACTIVE_ROOT_out);

    //----- step 4 ----------------------------------------------------------------
    Vector<Integer> from_path = new Vector<>();
    String from_kind = "Pbl";
    Vector<Integer> to_path = new Vector<>();
    String to_kind = "Pbl";
    int level = 0;
    XML.Tree GET_FORMULAE_out = sys.invoke(Operations.GET_FORMULAE,
      ConvertXML.get_formulae(new scala.math.BigInt(BigInteger.valueOf(calcid)), 
      from_path, from_kind, to_path, to_kind, new scala.math.BigInt(BigInteger.valueOf(level)), "false"));
    System.out.println("# 4 # " + GET_FORMULAE_out);

    //----- step 6 ----------------------------------------------------------------
    Vector<Integer> pos_path = new Vector<>();
    String pos_kind = "Pbl";
    XML.Tree REF_FORMULA_out = sys.invoke(Operations.REF_FORMULA,
      ConvertXML.ref_formula(new scala.math.BigInt(BigInteger.valueOf(calcid)), pos_path, pos_kind));
    System.out.println("# 6 # " + REF_FORMULA_out);

    //----- step 7 ----------------------------------------------------------------
    String auto = "CompleteCalc";
    XML.Tree AUTO_CALC_out = sys.invoke(Operations.AUTO_CALC,
      ConvertXML.auto_calculate(new scala.math.BigInt(BigInteger.valueOf(calcid)), auto));
    IntCalcChangedCompound calcid_cc = ConvertXML.auto_calc_out(AUTO_CALC_out);
    Vector<Integer> unc_ints = calcid_cc.get_unc_ints();
    String unc_kind = calcid_cc.get_unc_kind();
    //...
    System.out.println("# 7 # " + AUTO_CALC_out);

    //----- step 10 ---------------------------------------------------------------
    pos_path = new Vector<>();
    pos_kind = "Res";
    REF_FORMULA_out = sys.invoke(Operations.REF_FORMULA,
      ConvertXML.ref_formula(new scala.math.BigInt(BigInteger.valueOf(calcid)), pos_path, pos_kind));
    System.out.println("# 10 # " + REF_FORMULA_out);

    //----- step 13 ---------------------------------------------------------------
    XML.Tree DEL_CALC_out = sys.invoke(Operations.DEL_CALC,
      new scala.math.BigInt(BigInteger.valueOf(calcid)));
    calcid = ConvertXML.del_calc_out(DEL_CALC_out).intValue();
    System.out.println("# 13 # " + DEL_CALC_out);
    
    System.out.println("----- end of mini-test cf. ~~/doc/test--isac-Java--isac-kernel.txt");
				    
    sys.dispose();
    //$ ./sbt full/assembly
    //$ java -cp full/target/scala-2.11/libisabelle-full.jar Mini_Test
    }

}
