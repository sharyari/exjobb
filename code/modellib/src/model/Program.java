package model;

import java.util.Vector;

public class Program {
	private Vector<State> states = new Vector<State>();
	private Vector<Transition> transitions = 
			new Vector<Transition>();
	private int counter = 0;
	private int initial = 0;
	
	private String name;
	
	public String getName(){
		return name;
	}
	
	public Program(String name){
		this.name= name;
	}
	public void addState(State s){
		if (s.getType() == StateType.INITIAL)
			initial = s.getNum();
		states.add(s);
	}
	
	public int getInitial(){
		return initial;
	}
	
	public void addTransition(Transition t){
		transitions.add(t);
		t.setName(name+counter);
		counter++;
	}
	
	public void addState(Vector<State> s){
		states.addAll(s);
	}
	
	public void addTransition(Vector<Transition> t){
		transitions.addAll(t);
	}
	
	public Vector<Transition> getTransitions() {
		return transitions;
	}

	public String toString(){
		String str = "";
		for (int i = 0; i< states.size();i++)
			str += states.elementAt(i) +", ";
		str += "\n";
		for (int i = 0; i< transitions.size();i++)
			str += transitions.elementAt(i)+"\n";
		return str;
	}
	
}
