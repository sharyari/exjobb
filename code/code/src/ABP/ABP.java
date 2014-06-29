package ABP;


import model.Channel;
import model.Model;
import model.Program;
import model.State;
import model.StateType;
import model.Actions;

class ABP {
	
	public static void main(String[] args) {

		Model mod = new Model();

		Channel m = new Channel();
		Channel a = new Channel();

		mod.addChannel(m);
		mod.addChannel(a);

		m.addSymbol("a");
		m.addSymbol("b");
		a.addSymbol("a");
		a.addSymbol("b");
		
		Program sender = new Program("s");
		Program receiver = new Program("r");
		Program observer = new Program("o");

		sender.addState(new State(1, StateType.INITIAL));
		sender.addState(new State(2));
		sender.addState(new State(3));
		sender.addState(new State(4));

		
		sender.addTransition(1, 2,Actions.SND);
		sender.addTransition(2, 2, m, "!", "a");
		sender.addTransition(2, 2, a, "?", "b");
		sender.addTransition(2, 3, a, "?", "a");
		sender.addTransition(3, 4,Actions.SND);
		sender.addTransition(4, 4, m, "!", "b");
		sender.addTransition(4, 4, a, "?", "a");
		sender.addTransition(4, 1, a, "?", "b");

		

		receiver.addState(new State(1, StateType.INITIAL));
		receiver.addState(new State(2));
		receiver.addState(new State(3));
		receiver.addState(new State(4));

		receiver.addTransition(1, 2, m, "?", "a");
		receiver.addTransition(1, 1, m, "?", "b");
		receiver.addTransition(1, 1, a, "!", "b");
		receiver.addTransition(2, 3,Actions.RCV);
		receiver.addTransition(3, 4, m, "?", "b");
		receiver.addTransition(3, 3, m, "?", "a");
		receiver.addTransition(3, 3, a, "!", "a");
		receiver.addTransition(4, 1,Actions.RCV);
	


		observer.addState(new State (1,StateType.INITIAL));
		observer.addState(new State (2));
		observer.addState(new State (3, StateType.BAD));
		
		observer.addTransition(1, 2,Actions.SND);
		observer.addTransition(2, 1,Actions.RCV);
		observer.addTransition(1, 3,Actions.RCV);
		observer.addTransition(2, 3,Actions.SND);



		mod.addProgram(sender);
		mod.addProgram(receiver);
		mod.addProgram(observer);

		mod.synchronize(sender, observer, Actions.SND);
		mod.synchronize(receiver, observer, Actions.RCV);

		
		
		System.out.println(mod.toHaskell());
	}
	
}