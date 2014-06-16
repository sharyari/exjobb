package model;

class modellib {
	public static void main(String[] args) {

		Model mod = new Model();

		Channel m = new Channel("M");
		Channel a = new Channel("A");

		mod.addChannel(m);
		mod.addChannel(a);

		Program sender = new Program("s");

		State s1 = new State(1, StateType.INITIAL);
		State s2 = new State(2);
		State s3 = new State(3);
		State s4 = new State(4);

		Transition t1 = new Transition(s1, s2);
		Transition t2 = new Transition(s2, s2, m, "!", "a");
		Transition t3 = new Transition(s2, s2, a, "?", "b");
		Transition t4 = new Transition(s2, s3, a, "?", "a");
		Transition t5 = new Transition(s3, s4);
		Transition t6 = new Transition(s4, s4, m, "!", "b");
		Transition t7 = new Transition(s4, s4, a, "?", "a");
		Transition t8 = new Transition(s4, s1, a, "?", "b");

		sender.addState(s1);
		sender.addState(s2);
		sender.addState(s3);
		sender.addState(s4);

		sender.addTransition(t1);
		sender.addTransition(t2);
		sender.addTransition(t3);
		sender.addTransition(t4);
		sender.addTransition(t5);
		sender.addTransition(t6);
		sender.addTransition(t7);
		sender.addTransition(t8);

		Program receiver = new Program("r");
		State r1 = new State(1, StateType.INITIAL);
		State r2 = new State(2);
		State r3 = new State(3);
		State r4 = new State(4);

		Transition tr1 = new Transition(r1, r2, m, "?", "a");
		Transition tr2 = new Transition(r1, r1, m, "?", "b");
		Transition tr3 = new Transition(r1, r2, a, "!", "b");
		Transition tr4 = new Transition(r2, s3);
		Transition tr5 = new Transition(r3, r4, m, "?", "b");
		Transition tr6 = new Transition(r3, r3, m, "?", "a");
		Transition tr7 = new Transition(r3, r3, a, "!", "a");
		Transition tr8 = new Transition(r4, r1);
	

		receiver.addState(s1);
		receiver.addState(s2);
		receiver.addState(s3);
		receiver.addState(s4);

		receiver.addTransition(tr1);
		receiver.addTransition(tr2);
		receiver.addTransition(tr3);
		receiver.addTransition(tr4);
		receiver.addTransition(tr5);
		receiver.addTransition(tr6);
		receiver.addTransition(tr7);
		receiver.addTransition(tr8);

		
		mod.addProgram(sender);
		mod.addProgram(receiver);

		System.out.println(mod.toHaskell());
		
	}
}