import java.io.File;
import java.util.Arrays;
import edu.tum.cs.isabelle.japi.*;

public class Hello_PIDE {

  public static void main(String args[]) {
     
	System.out.println("Hello I'm Hello_PIDE!");

    JSystem sys = JSystem.instance(new File("."), "Protocol");
    System.out.println(sys.invoke(Operations.HELLO, "europa"));
    System.out.println(sys.invoke(Operations.TESTSTR, "xxx"));
    System.out.println(sys.invoke(Operations.TESTINT, 1));
    sys.dispose();
  }

}
