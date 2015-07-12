/* 
 * @author Walther Neuper
 * Copyright (c) due to license terms
 * Created on Jul 1, 2015
 * Institute for Softwaretechnology, Graz University of Technology, Austria.
 */
package examples.src.main.java;

import examples.src.main.java.IntPosCompound;

import java.util.Vector;

/**
 * compounds calcid and isac.util.formulae.Position
 * for transport from libisabelle/PIDE to isac-java.
 * @author Walther Neuper
 */
public class IntPosCompound {

    private Integer calcid_;
    //see Position#int_list_ and Position#kind_ integrated here 
    //because type seems(?!?) cast impossible:
    //  PIDEPosition ppos = IntPosCompound.get_pos();
    //  Position pos = ( Position ) ppos;    
    private Vector<Integer> int_list_;
    private String kind_;

    public IntPosCompound(Integer calcid, Vector<Integer> int_list, String kind) {
    	calcid_ = calcid;
    	int_list_ = int_list;
    	kind_ = kind;
    }
    
    public Integer get_calcid() {
        return calcid_;
    }  
    public Vector<Integer> get_ints() {
        return int_list_;
    }  
    public String get_kind() {
        return kind_;
    }  
}