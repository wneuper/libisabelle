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

  // conversions in all directions: Java <--> XML(Scala) <--> Isabelle/ML
  //compare isabisac/src/../datatypes.sml
  //see "conversions XML <--> ML" below: xml_of_* concerning          XML(Scala) <--  Isabelle/ML in Protocol.thy
  //see "conversions XML <--> ML" below: xml_to_* concerning          XML(Scala)  --> Isabelle/ML in Protocol.thy
  //see "convert results of JSystem.invoke(Operations.*"   below concerning Java <--  XML(Scala) in Mini_Test.java
  //see "convert arguments of JSystem.invoke(Operations.*" below concerning Java  --> XML(Scala) in Mini_Test.java
  //  here some xml_of_* are reused due to native Scala conversions
  
  //===== conversions XML(Scala) <--> Isabelle/ML
  def xml_of_int (i: scala.math.BigInt): XML.Tree = {
    XML.Elem(Markup("INT", Nil), List(XML.Text(i.toString())))
  }  
  def xml_to_int (t: XML.Tree): scala.math.BigInt = t match {
    case XML.Elem(Markup("INT", Nil), List(XML.Text(i))) => BigInt(i)
    case _ => throw new IllegalArgumentException("xml_to_int exn")
  }
  def xml_of_ints (is: List[scala.math.BigInt]): XML.Tree = {               //XML(Scala) <-- Isabelle/ML
    XML.Elem(Markup("INTLIST", Nil), is map xml_of_int)
  }
  def xml_to_ints (t: XML.Tree): List[scala.math.BigInt] = t match {        //XML(Scala) --> Isabelle/ML
    case XML.Elem(Markup("INTLIST", Nil), is) => is map xml_to_int
    case _ => throw new IllegalArgumentException("xml_to_ints exn")
  }
  def xml_to_VectorInteger (t: XML.Tree): Vector[java.lang.Integer] = t match {   //Java <-- XML(Scala)
    case XML.Elem(Markup("INTLIST", Nil), is) => {
      val v = new java.util.Vector[java.lang.Integer];
      is.foreach { case (XML.Elem(Markup("INT", Nil), List(XML.Text(i)))) => v.add(new java.lang.Integer(i)) }
      v
    }
    case _ => throw new IllegalArgumentException("xml_to_VectorInteger exn")
  }
  def xml_to_VectorString (t: XML.Tree): Vector[java.lang.String] = t match {     //Java <-- XML(Scala)
    case XML.Elem(Markup("STRINGLIST", Nil), ss) => {
      val v = new java.util.Vector[java.lang.String];
      ss.foreach { case (XML.Elem(Markup("STRING", Nil), List(XML.Text(str)))) => v.add(str) }
      v
    }
    case _ => throw new IllegalArgumentException("xml_to_VectorInteger exn")
  }
  def xml_to_Items (t: XML.Tree): Vector[java.lang.String] = t match {     //Java <-- XML(Scala)
    case XML.Elem(Markup("GIVEN", Nil), its) => {
      val v = new java.util.Vector[java.lang.String];
      its.foreach { case (XML.Elem(Markup("ITEM", Nil), List(XML.Text(str)))) => v.add(str) }
//         status = "incorrect" INSTEAD OF.......^^^^                
      v
    //case "WHERE"
    //case "FIND"
    //case "RELATE"
    }
    case _ => throw new IllegalArgumentException("xml_to_VectorInteger exn")
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
  
  
  //===== convert arguments of JSystem.invoke(Operations.* using XML(Scala) <-- Isabelle/ML
  // As long as libisabelle cannot import isac.* we use Java primitive types as arguments.
  // Conversions are all done in Scala (has specific methods), not in Java.
  // Note: java.int-->scala.BigInt not done here, because "int" is unknown in Scala.

  // conversion java.lang.Integer --> scala.math.BigInt
  def Integer_to_BigInt (i: java.lang.Integer): scala.math.BigInt = {
    new scala.math.BigInt(new BigInteger(i.toString())) //TODO: improve conversion ?
  }
  // UNUSED conversion scala.math.BigInt --> java.lang.Integer
  def BigInt_to_Integer (i: scala.math.BigInt): java.lang.Integer = {
    new java.lang.Integer(i.toString()) //TODO: improve conversion ?
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
  def   ref_formula(calcid: scala.math.BigInt, pos_path: Vector[Integer], pos_kind: String): XML.Tree =
  { XML.Elem(Markup("REFFORMULA", Nil), List(
      XML.Elem(Markup("CALCID", Nil), List(XML.Text(calcid.toString()))),
      xml_of_pos (pos_path.asScala.toList map Integer_to_BigInt, pos_kind)))
  }  
    
  //----- step 7 -----------------------
  def auto_calculate(calcid: scala.math.BigInt, auto: String): XML.Tree =
  {    
    XML.Elem(Markup("AUTOCALC", Nil), List(
      XML.Elem(Markup("CALCID", Nil), List(XML.Text(calcid.toString()))),
      XML.Elem(Markup("AUTO", Nil), List(XML.Text(auto)))))
  }

  //===== convert results of JSystem.invoke(Operations.* using Java <-- XML(Scala) =============
  //----- step 1 -----------------------
  def calc_tree_out(t: XML.Tree): java.lang.Integer = t match {
    case
       XML.Elem(Markup("CALCTREE", Nil), List(
         XML.Elem(Markup("CALCID", Nil), List(XML.Text(i)))))
      => new Integer(i)
    case _ => throw new IllegalArgumentException("calc_tree_out exn")
  }

  //----- step 2 -----------------------
  def iterator_out(t: XML.Tree): IntIntCompound = t match {
    case
      XML.Elem(Markup("ADDUSER", Nil), List(
        XML.Elem(Markup("CALCID", Nil), List(XML.Text(calcid))),
        XML.Elem(Markup("USERID", Nil), List(XML.Text(userid)))))
      => new IntIntCompound(new java.lang.Integer(calcid), new java.lang.Integer(userid))
    case _ => throw new IllegalArgumentException("iterator_out exn")
  }

  //----- step 3 -----------------------
  def move_active_root_out(t: XML.Tree): IntPosCompound = t match {
    case 
      XML.Elem (Markup("CALCITERATOR", Nil), List(
        XML.Elem (Markup("CALCID", Nil), List(
          XML.Text (calcid))),
          XML.Elem(Markup("POSITION", Nil), List(
            is, XML.Elem(Markup("POS", Nil), List(XML.Text(kind)))))))
      => new IntPosCompound(new java.lang.Integer(calcid), xml_to_VectorInteger(is), kind)
    case _ => throw new IllegalArgumentException("move_active_root_out exn")
  }

  //----- step 4 ----------------------------------------------------------------
  //----- step 6 ----------------------------------------------------------------
  //----- step 7 ----------------------------------------------------------------
  def auto_calc_out(t: XML.Tree): IntCalcChangedCompound = t match {
    case
      XML.Elem(Markup("AUTOCALC", Nil), List(
        XML.Elem(Markup("CALCID", Nil), List(XML.Text (calcid))),
        XML.Elem(Markup("CALCCHANGED", Nil), List(
          XML.Elem(Markup("UNCHANGED", Nil), List(
            unc_is, XML.Elem(Markup("POS", Nil), List(XML.Text(unc_kind))))),
          XML.Elem(Markup("DELETED", Nil), List(
            del_is, XML.Elem(Markup("POS", Nil), List(XML.Text(del_kind))))),
          XML.Elem(Markup("GENERATED", Nil), List(
            gen_is, XML.Elem(Markup("POS", Nil), List(XML.Text(gen_kind)))))))))
      => new IntCalcChangedCompound(new java.lang.Integer(calcid), xml_to_VectorInteger(unc_is), unc_kind,
          xml_to_VectorInteger(del_is), del_kind, xml_to_VectorInteger(gen_is), gen_kind)
    case _ => throw new IllegalArgumentException("auto_calc_out exn")
  }
  //----- step 10 ---------------------------------------------------------------
  def ref_formula_out(t: XML.Tree): java.lang.Object = t match {
    case
      XML.Elem(Markup("REFFORMULA", Nil), List(
        XML.Elem(Markup("CALCID", Nil), List(XML.Text (calcid))),
        XML.Elem(Markup("CALCFORMULA", Nil), List(
          XML.Elem(Markup("POSITION", Nil), List(
            form_ints, XML.Elem(Markup("POS", Nil), List(XML.Text(form_kind))))),
          XML.Elem(Markup("FORMULA", Nil), List(
            XML.Elem(Markup("MATHML", Nil), List(
              XML.Elem(Markup("ISA", Nil), List(XML.Text(form_isa)))))))))))
      => new IntCalcFormCompound(new java.lang.Integer(calcid), 
          xml_to_VectorInteger(form_ints), form_kind, form_isa)
//case for step 6 
//this becomes cumbersome without import CalcHead .. Model .. ModelItemList
//thus we shift ConvertXML to isac-java
//    case
//      XML.Elem(Markup("REFFORMULA", Nil), List(
//        XML.Elem(Markup("CALCID", Nil), List(XML.Text (calcid))),
//        XML.Elem(Markup("CALCHEAD", !!!!!!!!!!!!!!!!!!!!!!!!), List(
//          XML.Elem(Markup("POSITION", Nil), List(
//            form_ints, XML.Elem(Markup("POS", Nil), List(XML.Text(form_kind))))),
//          XML.Elem(Markup("HEAD", Nil), List(
//            XML.Elem(Markup("MATHML", Nil), List(
//              XML.Elem(Markup("ISA", Nil), List(XML.Text(form_isa)))))))))))
//          :
//          :
//      => new IntCalcHeadCompound(new java.lang.Integer(calcid), 
//          head_status, xml_to_VectorInteger(head_ints), head_kind, form_isa, head_isa,
//          xml_to_VectorString(givens), xml_to_VectorString(wheres), xml_to_VectorString(finds), xml_to_VectorString(relates),
//          belongsto, xml_to_VectorString(thy), xml_to_VectorString(pbl), xml_to_VectorString(met))
    case _ => throw new IllegalArgumentException("ref_formula_out exn")
  }
  
  //----- step 13 ---------------------------------------------------------------
  def del_calc_out(t: XML.Tree): java.lang.Integer = t match {
    case
      XML.Elem(Markup("DELCALC", Nil), List(
        XML.Elem(Markup("CALCID", Nil), List(XML.Text(calcid)))))
      => new java.lang.Integer(calcid)
    case _ => throw new IllegalArgumentException("del_calc_out exn")
  }

}