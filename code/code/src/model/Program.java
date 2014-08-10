package model;

import java.util.Vector;

public class Program {
	private Vector<State> states = new Vector<State>();
	private Vector<Transition> transitions = new Vector<Transition>();
	private State initial;
	private int counter = 0;
	private int number;
	private String name;
	
	public Program(String name){
		this.name= name; //Name is used to create function names
	}
	
	public String getName(){
		return name; 
	}
	
	public void addState(State s){
		if (s.getType() == StateType.INITIAL || states.size() == 0)
			initial = s; // The first state is assumed to be initial, unless one is later specified.
		states.add(s);
	}
	
	public State getInitial() {
		return initial;
	}
	
	// Programs are enumerated when added to a model, all states must be updated
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
	
	public void addTransition (int a, int b){
		State s1=null,s2=null;
		for (int i=0;i<states.size();i++){
			if (states.elementAt(i).getNum() == a){
				s1 = states.elementAt(i);
			}
			if (states.elementAt(i).getNum() == b){
				s2 = states.elementAt(i);
			}
		}
		Transition t = new Transition(s1,s2,Actions.NONE); // catch throw null!
		transitions.add(t);
		t.setName(name+counter);
		counter++;
	}
	
	public void addTransition (int a, int b, Actions act){
		State s1=null,s2=null;
		for (int i=0;i<states.size();i++){
			if (states.elementAt(i).getNum() == a){
				s1 = states.elementAt(i);
			}
			if (states.elementAt(i).getNum() == b){
				s2 = states.elementAt(i);
			}
		}
		Transition t = new Transition(s1,s2, act); // catch throw null!
		transitions.add(t);
		t.setName(name+counter);
		counter++;
	}

	public void addTransition (int a, int b, Channel ch, String op, int j){
		State s1=null,s2=null;
		for (int i=0;i<states.size();i++){
			if (states.elementAt(i).getNum() == a){
				s1 = states.elementAt(i);
			}
			if (states.elementAt(i).getNum() == b){
				s2 = states.elementAt(i);
			}
		}
		Transition t = new Transition(s1,s2, ch, op, j); // catch throw null!
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
			if (transitions.elementAt(i).sync == false){
				str+= transitions.elementAt(i).getName()+",";
			}
		} 
		if (str != "")
			str = str.substring(0, str.length()-1);

		return str;
	}
	
}
