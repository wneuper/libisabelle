/* 
 * @author Walther Neuper
 * Copyright (c) due to license terms
 * Created on Jul 14, 2015
 * Institute for Softwaretechnology, Graz University of Technology, Austria.
 */
package examples.src.main.java;

import java.util.Vector;
import java.util.ArrayList;

/**
 * compounds calcid and members of isac.util.formulae.CalcFormula
 * for transport from libisabelle/PIDE to isac-java.
 * 
 * Isackernel#Frontend#refFormula either returns CalcFormula or CalcHead,
 * so type cast is required from Object to CalcFormula or CalcHead.
 * @author Walther Neuper
 */
public class IntCalcFormCompound {

    private Integer calcid_;
    private Vector<Integer> form_ints_; //Pos-Vector ?? path-ArrayList
    private String form_kind_;
    private String form_isa_;
    
    public IntCalcFormCompound(Integer calcid, 
      Vector<Integer> form_ints, String form_kind, String form_isa) {
    	
    	calcid_ = calcid;
        //CalcFormula    	
    	form_ints_ = form_ints;
    	form_kind_ = form_kind;
    	form_isa_ = form_isa;
    }
    
    public Integer get_calcid() {
        return calcid_;
    }  
    public Vector<Integer> get_form_ints() {
        return form_ints_;
    }  
    public String get_form_kind() {
        return form_kind_;
    }  
    public String get_form_isa() {
        return form_isa_;
    }  
    
}