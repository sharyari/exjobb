package model;

import java.util.Vector;

public class Program {
	private Vector<State> states = new Vector<State>();
	private Vector<Transition> transitions = 
			new Vector<Transition>();

	private State initial;
	
	private int counter = 0;
	private int number;
	
	private String name;
	
	public String getName(){
		return name;
	}
	
	public Program(String name){
		this.name= name;
	}
	public void addState(State s){
		if (s.getType() == StateType.INITIAL)
			initial = s;
		states.add(s);
	}
	
	public State getInitial() {
		return initial;
	}
	public void setNumber(int n) {
		number = n;
		for(int i=0;i<states.size();i++){
			states.elementAt(i).setInProgram(number);
		}
	}
	
	public int getNumber(){
		return number;
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
	
	public String toHaskell() {
		String str = "";
		for (int i=0;i<transitions.size();i++){
			str+=transitions.elementAt(i).toHaskell();
		}
		return str;
	}

	public String getTransitionNames() {
		String str= "";
		for (int i=0;i< transitions.size();i++){
			str+= transitions.elementAt(i).getName()+",";
		}
		str = str.substring(0, str.length()-1);

		return str;
	}
	
}
