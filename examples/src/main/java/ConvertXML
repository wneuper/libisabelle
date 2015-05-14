//package src.main.java  ...THIS CAUSES AN ERROR IN sbt

import isabelle.XML;
import isabelle.Markup;

//import isac.interfaces.ICalcIterator;  //NOT RESOLVED BY sbt
//import isac.bridge.CalcIterator;       //NOT RESOLVED BY sbt

object ConvertXML {
  
  //----- step 4 -----------------------
  //def get_formulae(calcid: scala.math.BigInt, from: ICalcIterator, to: ICalcIterator,  //NOT RESOLVED BY sbt
  def   get_formulae(calcid: scala.math.BigInt, from: String,        to: String, 
    level: scala.math.BigInt, rules/*?*/: String): XML.Tree =
  { 
    XML.Elem(Markup("GETFORMULAEFROMTO", Nil), List(
      XML.Elem(Markup("CALCID", Nil), List(XML.Text(calcid.toString()))),
      XML.Elem(Markup("POSITION", Nil), List(
        XML.Elem(Markup("INTLIST", Nil), Nil),
        XML.Elem(Markup("POS", Nil), List(XML.Text(from))))),
      XML.Elem(Markup("POSITION", Nil), List(
        XML.Elem(Markup("INTLIST", Nil), Nil),
        XML.Elem(Markup("POS", Nil), List(XML.Text(from))))),
      XML.Elem(Markup("INT", Nil), List(XML.Text(level.toString()))),
      XML.Elem(Markup("BOOL", Nil), List(XML.Text(rules))
    )))
  }  
  //----- step 7 -----------------------
  def auto_calculate(calcid: scala.math.BigInt, auto: String): XML.Tree =
  {    
    XML.Elem(Markup("AUTOCALC", Nil), List(
      XML.Elem(Markup("CALCID", Nil), List(XML.Text(calcid.toString()))),
      XML.Elem(Markup("AUTO", Nil), List(XML.Text(auto)))))
  }
}