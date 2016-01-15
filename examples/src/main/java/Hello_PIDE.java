import java.nio.file.Paths;
import edu.tum.cs.isabelle.api.*;
import edu.tum.cs.isabelle.japi.*;
import edu.tum.cs.isabelle.setup.*;

public class Hello_PIDE {

  public static void main(String args[]) {
    Environment env = JSetup.makeEnvironment(
      Paths.get(System.getenv("/usr/local/isabisac")), JPlatform.guess(), new Version("2015"));
    Configuration config = Configuration.fromPath(
      Paths.get("/home/wneuper/proto4/libisabelle-0.2.2"), "libisabelle_Isac");
    JSystem sys = JSystem.create(env, config);
    System.out.println(sys.invoke(Operations.HELLO, "world of libisabelle_Isac"));
    sys.dispose();
  }

}
