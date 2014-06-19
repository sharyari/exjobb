package model;

import java.util.Vector;

public class Synchronize {
	
	private Vector<Transition> t = new Vector<Transition>();
	
	public Synchronize(Transition t1, Transition t2){
		t1.synchronize();
		t2.synchronize();
		t.add(t1);
		t.add(t2);
	}
	
	public Synchronize(Transition t1, Transition t2, Transition t3){
		t1.synchronize();
		t2.synchronize();
		t3.synchronize();
		t.add(t1);
		t.add(t2);
		t.add(t3);
	}
	
	public String toString(){
		return "";
	}
	
}
