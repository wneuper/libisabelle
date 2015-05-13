//package src.main.java

import isabelle.XML;
import isabelle.Markup;

object Mini_Test_INPUT {
  
  //----- step 7 -----------------------
  def auto_calculate(calcid: scala.math.BigInt, auto: String): XML.Tree =
  {    
    XML.Elem(Markup("AUTOCALC", Nil), List(
      XML.Elem(Markup("CALCID", Nil), List(XML.Text(calcid.toString()))),
      XML.Elem(Markup("AUTO", Nil), List(XML.Text(auto)))))
  }
  
}