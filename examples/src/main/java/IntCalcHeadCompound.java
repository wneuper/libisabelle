/* 
 * @author Walther Neuper
 * Copyright (c) due to license terms
 * Created on Jul 14, 2015
 * Institute for Softwaretechnology, Graz University of Technology, Austria.
 */
package examples.src.main.java;

import java.util.Vector;

/**
 * compounds calcid and members of isac.util.formulae.CalcHead
 * for transport from libisabelle/PIDE to isac-java.
 * 
 * Isackernel#Frontend#refFormula either returns CalcFormula or CalcHead,
 * so type cast is required from Object to CalcFormula or CalcHead.
 * @author Walther Neuper
 */
public class IntCalcHeadCompound {

    private Integer calcid_;
    private String head_status_;
    private Vector<Integer> head_ints_;
    private String head_kind_;
    private String head_isa_;
    private Vector<String> givens_;
    private Vector<String> wheres_;
    private Vector<String> finds_;
    private Vector<String> relates_;
    private String belongsto_;
    private String thy_;
    private Vector<String> pbl_;
    private Vector<String> met_;
    
    public IntCalcHeadCompound(Integer calcid, 
      String head_status, Vector<Integer> head_ints, String head_kind, String head_isa,
        Vector<String> givens, Vector<String> wheres, Vector<String> finds, Vector<String> relates,
        String belongsto, String thy, Vector<String> pbl, Vector<String> met) {
    	
    	calcid_ = calcid;
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
    public Vector<String> get_pbl() {
        return pbl_;
    }  
    public Vector<String> get_met() {
        return met_;
    }  
    
}