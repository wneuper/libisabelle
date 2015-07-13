/* 
 * @author Walther Neuper
 * Copyright (c) due to license terms
 * Created on Jul 12, 2015
 * Institute for Softwaretechnology, Graz University of Technology, Austria.
 */

package examples.src.main.java;

/**
 * compounds calcid and isac.util.formulae.Position
 * for transport from libisabelle/PIDE to isac-java.
 * @author Walther Neuper
 */
public class IntIntCompound {

    private Integer calcid_;
    private Integer userid_;

    public IntIntCompound(Integer calcid, Integer userid) {
    	calcid_ = calcid;
    	userid_ = userid;
    }
    
    public Integer get_calcid() {
        return calcid_;
    }  
    public Integer get_userid() {
        return userid_;
    }  
}