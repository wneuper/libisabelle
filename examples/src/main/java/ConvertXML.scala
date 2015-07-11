package examples.src.main.java

import isabelle.XML;
import isabelle.Markup;

import java.util.ArrayList;
import java.util.Vector;
import java.math.BigInteger;

import scala.math.BigInt;
import scala.collection.JavaConverters._

//import isac.interfaces.ICalcIterator;  //NOT RESOLVED BY sbt
//import isac.bridge.CalcIterator;       //NOT RESOLVED BY sbt

object ConvertXML {

  //===== conversions in both directions: Java <--> XML(Scala)
  //compare isabisac/src/../datatypes.sml
  
  def xml_of_int (i: scala.math.BigInt): XML.Tree = {
    XML.Elem(Markup("INT", Nil), List(XML.Text(i.toString())))
  }  
  def xml_to_int (t: XML.Tree): scala.math.BigInt = t match {
    case XML.Elem(Markup("INT", Nil), List(XML.Text(i))) => BigInt(i)
    case _ => throw new IllegalArgumentException("xml_to_int exn")
  }
  def xml_of_ints (is: List[scala.math.BigInt]): XML.Tree = {
    XML.Elem(Markup("INTLIST", Nil), is map xml_of_int)
  }
  def xml_to_ints (t: XML.Tree): List[scala.math.BigInt] = t match {
    case XML.Elem(Markup("INTLIST", Nil), is) => is map xml_to_int
    case _ => throw new IllegalArgumentException("xml_to_ints exn")
  }
  def xml_of_pos (ints: List[scala.math.BigInt], kind: String ): XML.Tree = {
    XML.Elem(Markup("POSITION", Nil), List(
      xml_of_ints(ints),
      XML.Elem(Markup("POS", Nil), List(XML.Text(kind)))))
  }
  def xml_to_pos (t: XML.Tree): (List[scala.math.BigInt], String) = t match {
    case XML.Elem(Markup("POSITION", Nil), List(
        is, XML.Elem(Markup("POS", Nil), List(XML.Text(kind))))) => (xml_to_ints(is), kind)
    case _ => throw new IllegalArgumentException("xml_to_pos exn")
  } 
  def xml_of_str (s: String): XML.Tree = {
    XML.Elem(Markup("STRING", Nil), List(XML.Text(s)))
  }  
  def xml_to_str (t: XML.Tree): String = t match {
    case XML.Elem(Markup("STRING", Nil), List(XML.Text(s))) => s
    case _ => throw new IllegalArgumentException("xml_to_str exn")
  }
  def xml_of_strs (ss: List[String]): XML.Tree = {
    XML.Elem(Markup("STRINGLIST", Nil), ss map xml_of_str)
  }
  def xml_to_strs (t: XML.Tree): List[String] = t match {
    case XML.Elem(Markup("STRINGLIST", Nil), ss) => ss map xml_to_str
    case _ => throw new IllegalArgumentException("xml_to_strs exn")
  } 
  def xml_of_spec (thy: String, pbl: ArrayList[String], met: ArrayList[String]): XML.Tree = {
    XML.Elem(Markup("SPECIFICATION", Nil), List(
      XML.Elem(Markup("THEORYID", Nil), List(XML.Text(thy))),
      XML.Elem(Markup("PROBLEMID", Nil), List(xml_of_strs(pbl.asScala.toList))),
      XML.Elem(Markup("METHODID", Nil), List(xml_of_strs(met.asScala.toList)))))
  }
  def xml_to_spec (t: XML.Tree) = t match {
    case XML.Elem(Markup("SPECIFICATION", Nil), List(
      XML.Elem(Markup("THEORYID", Nil), List(XML.Text(thy))),
      XML.Elem(Markup("PROBLEMID", Nil), List(pbl)),
      XML.Elem(Markup("METHODID", Nil), List(met)))) => (thy, xml_to_strs(pbl), xml_to_strs(met))
    case _ => throw new IllegalArgumentException("xml_to_spec exn")
  } 
  
  
  //===== convert arguments of JSystem.invoke(Operations.*
  // As long as libisabelle cannot import isac.* we use Java primitive types as arguments.
  // Conversions are all done in Scala (has specific methods), not in Java.
  // Note: java.int-->scala.BigInt not done here, because "int" is unknown in Scala.

  /* conversion java.lang.Integer --> scala.math.BigInt */
  def Integer_to_BigInt (i: java.lang.Integer): scala.math.BigInt = {
    new scala.math.BigInt(new BigInteger(i.toString())) //TODO: improve conversion ?
  }

  //----- step 1 -----------------------
  def calc_tree(items: ArrayList[String], thy: String, pbl: ArrayList[String], met: ArrayList[String]): XML.Tree = {
    XML.Elem(Markup("FORMALIZATION", Nil), List(
      xml_of_strs(items.asScala.toList),
      xml_of_spec(thy, pbl, met)))
  }
  /*scala> val items = List("equality (x+1=(2::real))", "solveFor x", "solutions L")
           val (thy, pbl, met) = ("Test", 
                       List("sqroot-test","univariate","equation","test"), 
                       List("Test","squ-equ-test-subpbl1"))
   *scala> calc_tree(items, thy, pbl, met)
   *output for comparison with doc/test--isac-java--isac-kernel.txt
    + manual linefeed with indentation:
      isabelle.XML.Tree = 
      <FORMALIZATION>
        <STRINGLIST>
          <STRING>equality (x+1=(2::real))</STRING>
          <STRING>solveFor x</STRING>
          <STRING>solutions L</STRING>
        </STRINGLIST>
        <SPECIFICATION>
          <THEORYID>Test</THEORYID>
          <PROBLEMID>
            <STRINGLIST>
              <STRING>sqroot-test</STRING>
              <STRING>univariate</STRING>
              <STRING>equation</STRING>
              <STRING>test</STRING></STRINGLIST>
          </PROBLEMID>
          <METHODID>
            <STRINGLIST>
              <STRING>Test</STRING>
              <STRING>squ-equ-test-subpbl1</STRING>
            </STRINGLIST>
          </METHODID>
        </SPECIFICATION>
      </FORMALIZATION>
   */
   
  //----- step 4 -----------------------
  //def get_formulae(calcid: scala.math.BigInt, from: ICalcIterator,                           to: ICalcIterator,  //NOT RESOLVED BY sbt
  def   get_formulae(calcid: scala.math.BigInt, from_path: Vector[Integer], from_kind: String, to_path: Vector[Integer], to_kind: String, 
    level: scala.math.BigInt, rules/*?*/: String): XML.Tree =
  { XML.Elem(Markup("GETFORMULAEFROMTO", Nil), List(
      XML.Elem(Markup("CALCID", Nil), List(XML.Text(calcid.toString()))),
      xml_of_pos (from_path.asScala.toList map Integer_to_BigInt, from_kind),
      xml_of_pos (to_path.asScala.toList map Integer_to_BigInt, to_kind),
      XML.Elem(Markup("INT", Nil), List(XML.Text(level.toString()))),
      XML.Elem(Markup("BOOL", Nil), List(XML.Text(rules)))))
  }  
  
  //----- step 6 -----------------------
  //def ref_formula(calcid: scala.math.BigInt, pos: ICalcIterator,  //NOT RESOLVED BY sbt  
  def   ref_formula(calcid: scala.math.BigInt, pos: String): XML.Tree =
  { 
    /*scala> val (calcid, pos) = (1:BigInt, "Pbl")
     */
    XML.Elem(Markup("REFFORMULA", Nil), List(
      XML.Elem(Markup("CALCID", Nil), List(XML.Text(calcid.toString()))),
      xml_of_pos (Nil, pos)))
  }  
    
  //----- step 7 -----------------------
  def auto_calculate(calcid: scala.math.BigInt, auto: String): XML.Tree =
  {    
    XML.Elem(Markup("AUTOCALC", Nil), List(
      XML.Elem(Markup("CALCID", Nil), List(XML.Text(calcid.toString()))),
      XML.Elem(Markup("AUTO", Nil), List(XML.Text(auto)))))
  }

  //===== prepare for spec. constructors  Java <--  XML(Scala)..libisabelle
  


}