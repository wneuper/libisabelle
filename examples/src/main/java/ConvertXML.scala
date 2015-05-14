//package src.main.java  ...THIS CAUSES AN ERROR IN sbt

import isabelle.XML;
import isabelle.Markup;

//import isac.interfaces.ICalcIterator;  //NOT RESOLVED BY sbt
//import isac.bridge.CalcIterator;       //NOT RESOLVED BY sbt

object ConvertXML {

  //===== conversions in both directions: Java <--> XML(Scala)..libisabelle
  //compare isabisac/src/../datatypes.sml
  //!as long as there are no imports, we preliminarily use Scala!
  
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
      XML.Text(kind)))
  }
  /* HERE IS SOME ERROR:
  def xml_to_pos (t: XML.Tree): (List[scala.math.BigInt], String) = t match {
    case XML.Elem(Markup("POSITION", Nil), List(
        is, XML.Elem(Markup("POS", Nil), List(XML.Text(kind))))) => (xml_to_ints is, kind)
    case _ => throw new IllegalArgumentException("xml_to_pos exn")
  } */ 
  
  //===== convert arguments of methods calling  --> XML(Scala)..libisabelle
   
  //----- step 4 -----------------------
  //def get_formulae(calcid: scala.math.BigInt, from: ICalcIterator, to: ICalcIterator,  //NOT RESOLVED BY sbt
  def   get_formulae(calcid: scala.math.BigInt, from: String,        to: String, 
    level: scala.math.BigInt, rules/*?*/: String): XML.Tree =
  { 
    /* THIS RAISES ERROR isabelle.System$ProverException: decoding failed
    XML.Elem(Markup("GETFORMULAEFROMTO", Nil), List(
      XML.Elem(Markup("CALCID", Nil), List(XML.Text(calcid.toString()))),
      xml_of_pos (Nil, from),
      xml_of_pos (Nil, to),
      XML.Elem(Markup("INT", Nil), List(XML.Text(level.toString()))),
      XML.Elem(Markup("BOOL", Nil), List(XML.Text(rules)))))
    */
    XML.Elem(Markup("GETFORMULAEFROMTO", Nil), List(
      XML.Elem(Markup("CALCID", Nil), List(XML.Text(calcid.toString()))),
      XML.Elem(Markup("POSITION", Nil), List(
        XML.Elem(Markup("INTLIST", Nil), Nil),
        XML.Elem(Markup("POS", Nil), List(XML.Text(from))))),
      XML.Elem(Markup("POSITION", Nil), List(
        XML.Elem(Markup("INTLIST", Nil), Nil),
        XML.Elem(Markup("POS", Nil), List(XML.Text(from))))),
      XML.Elem(Markup("INT", Nil), List(XML.Text(level.toString()))),
      XML.Elem(Markup("BOOL", Nil), List(XML.Text(rules)))))
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