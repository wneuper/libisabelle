/* 
 * @author Walther Neuper
 * Copyright (c) due to license terms
 * Created on Jul 12, 2015
 * Institute for Softwaretechnology, Graz University of Technology, Austria.
 */

package examples.src.main.java;

import java.util.Vector;

/**
 * compounds calcid and members of isac.util.CalcChanged
 * for transport from libisabelle/PIDE to isac-java.
 * @author Walther Neuper
 */
public class IntCalcChangedCompound {

    private Vector<Integer> unc_ints_;
    private String unc_kind_;
    private Vector<Integer> del_ints_;
    private String del_kind_;
    private Vector<Integer> gen_ints_;
    private String gen_kind_;

    public IntCalcChangedCompound(Integer calcid, Vector<Integer> unc_ints, String unc_kind,
    		Vector<Integer> del_ints, String del_kind, Vector<Integer> gen_ints, String gen_kind) {
    	unc_ints_ = unc_ints;
    	unc_kind_ = unc_kind;
    	del_ints_ = del_ints;
    	del_kind_ = del_kind;
    	gen_ints_ = gen_ints;
    	gen_kind_ = gen_kind;
    }
    
    public Vector<Integer> get_unc_ints() {
        return unc_ints_;
    }  
    public String get_unc_kind() {
        return unc_kind_;
    }  
    public Vector<Integer> get_del_ints() {
        return del_ints_;
    }  
    public String get_del_kind() {
        return del_kind_;
    }  
    public Vector<Integer> get_gen_ints() {
        return gen_ints_;
    }  
    public String get_gen_kind() {
        return gen_kind_;
    }  
}