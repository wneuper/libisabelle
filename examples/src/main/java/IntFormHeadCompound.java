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
 * and isac.util.formulae.CalcHead for transport from libisabelle/PIDE to isac-java.
 * 
 * Isackernel#Frontend#refFormula either returns CalcFormula or CalcHead,
 * so here is one part of member variables null and the other unequal null.
 * @author Walther Neuper
 */
public class IntFormHeadCompound {

    private Integer calcid_;
    //CalcFormula
    private Vector<Integer> form_ints_; //Pos-Vector ?? path-ArrayList
    private String form_kind_;
    private String form_isa_;
    //CalcHead
    private String head_status_;
    private Vector<Integer> head_ints_; //Pos-Vector ?? path-ArrayList
    private String head_kind_;
    private String head_isa_;
    private Vector<String> givens_;  //Pos-Vector ?? path-ArrayList
    private Vector<String> wheres_;  //Pos-Vector ?? path-ArrayList
    private Vector<String> finds_;   //Pos-Vector ?? path-ArrayList
    private Vector<String> relates_; //Pos-Vector ?? path-ArrayList
    private String belongsto_;
    private String thy_;
    private ArrayList<String> pbl_;
    private ArrayList<String> met_;
    
    public IntFormHeadCompound(Integer calcid, 
      Vector<Integer> form_ints, String form_kind, String form_isa,
      String head_status, Vector<Integer> head_ints, String head_kind, String head_isa,
        Vector<String> givens, Vector<String> wheres, Vector<String> finds, Vector<String> relates,
        String belongsto, String thy, ArrayList<String> pbl, ArrayList<String> met) {
    	
    	calcid_ = calcid;
        //CalcFormula    	
    	form_ints_ = form_ints;
    	form_kind_ = form_kind;
    	form_isa_ = form_isa;
        //CalcHead
    	head_status_ = head_status;
    	head_ints_ = head_ints;
    	head_kind_ = head_kind;
    	head_isa_ = head_isa;
    	givens_ = givens;
    	wheres_ = wheres;
    	finds_ = finds;
    	relates_ = relates;
    	thy_ = thy;
    	pbl_ = pbl;
    	met_ = met;
    }
    
    public Integer get_calcid() {
        return calcid_;
    }  
    //Isackernel#Frontend#refFormula either returns CalcFormula or CalcHead,
    //  the other is null in all member variables.
    //It is considered sufficient to check two simple analogous member variables.
    public boolean is_calchead() throws Exception {
    	if (form_kind_ == null) {
    		if (head_kind_ == null) {
    			throw new Exception("libisabelle: IntFormHeadCompound has too many null");
    		} else { return true; }
    	} else { return false; }    		
    }
    //CalcFormula
    public Vector<Integer> get_form_ints() {
        return form_ints_;
    }  
    public String get_form_kind() {
        return form_kind_;
    }  
    public String get_form_isa() {
        return form_isa_;
    }  
    //CalcHead
    public String get_form_isa_() {
        return head_status_;
    }  
    public Vector<Integer> get_head_ints() {
        return head_ints_;
    }  
    public String get_head_kind() {
        return head_kind_;
    }  
    public String get_head_isa() {
        return head_isa_;
    }  
    public Vector<String> get_givens() {
        return givens_;
    }  
    public Vector<String> get_wheres() {
        return wheres_;
    }  
    public Vector<String> get_finds() {
        return finds_;
    }  
    public Vector<String> get_relates() {
        return relates_;
    }  
    public String get_belongsto() {
        return belongsto_;
    }  
    public String get_thy() {
        return thy_;
    }  
    public ArrayList<String> get_pbl() {
        return pbl_;
    }  
    public ArrayList<String> get_met() {
        return met_;
    }  
    
}