package model;

import java.util.Vector;

public class Synchronize {
	
	private Vector<Transition> t = new Vector<Transition>();
	private String name;
	private int number;
	private static int counter;
	
	public Synchronize(Transition t1, Transition t2){
		number=counter++;
		t1.synchronize();
		t2.synchronize();
		t.add(t1);
		t.add(t2);
	}
	
	public void setName (String name){
		this.name = name;
	}
	
	public String getName() {
		return name+number;
	}
	
	public Synchronize(Transition t1, Transition t2, Transition t3){
		t1.synchronize();
		t2.synchronize();
		t3.synchronize();
		t.add(t1);
		t.add(t2);
		t.add(t3);
	}
	
	public String toHaskell(){
		String str = name+number+" = ([";
		for (int i=0;i<t.size();i++)
			str+=t.elementAt(i).helpHaskell()+",";
		str = str.substring(0, str.length()-1);
		str+="], [";

		String str2 = "";
		for (int i=0;i<t.size();i++){
			str2=t.elementAt(i).helpHaskell2();
			if (str2 != "")
				str += str2+",";
		}
		str.substring(0, str.length()-1);
		str+="])\n";
		return str;
	}
	
	public String toString(){
		return "";
	}
	
}
