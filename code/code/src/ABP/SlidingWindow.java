package ABP;



import model.Channel;
import model.Model;
import model.Program;
import model.State;
import model.StateType;
import model.Actions;

public class SlidingWindow {
	public static void main(String[] args) {

		// CREATE MODEL
		Model mod = new Model();

		// CREATE CHANNELS
		Channel m = new Channel();
		Channel a = new Channel();

		// ADD CHANNELS TO MODEL
		mod.addChannel(m);
		mod.addChannel(a);
		
		m.addSymbol(0);
		m.addSymbol(1);
		m.addSymbol(2);
		a.addSymbol(0);
		a.addSymbol(1);
		a.addSymbol(2);

		// CREATE PROGRAMS
		Program sender = new Program("s");
		Program receiver = new Program("r");
		Program observer = new Program("o");
		
		// ADD PROGRAMS TO MODEL

		
		// DEFINE SENDER		
		sender.addState(new State(1, StateType.INITIAL));
		sender.addState(new State(2));
		sender.addState(new State(3));
		sender.addState(new State(4));
		sender.addState(new State(5));
		sender.addState(new State(6));
		sender.addState(new State(7));
		sender.addState(new State(8));
		sender.addState(new State(9));
		
		sender.addTransition(1, 2, Actions.SND);
		sender.addTransition(2, 3, Actions.SND);
		sender.addTransition(2, 2, m, "!", 0);
		sender.addTransition(3, 3, m, "!", 10);
		sender.addTransition(2, 4, a, "?", 0);
		sender.addTransition(3, 5, a, "?", 0);

		sender.addTransition(4, 5, Actions.SND);
		sender.addTransition(5, 6, Actions.SND);
		sender.addTransition(5, 5, m, "!", 1);
		sender.addTransition(6, 6 ,m, "!", 21);
		sender.addTransition(5, 7, a, "?", 1);
		sender.addTransition(6, 8, a, "?", 1);

		sender.addTransition(7, 8, Actions.SND);
		sender.addTransition(8, 9, Actions.SND);
		sender.addTransition(8, 8, m, "!", 2);
		sender.addTransition(9, 9, m, "!", 12);
		sender.addTransition(8, 1, a, "?", 2);
		sender.addTransition(9, 2, a, "?", 2);
		

		// DEFINE RECEIVER
		receiver.addState(new State(1, StateType.INITIAL));
		receiver.addState(new State(2));
		receiver.addState(new State(3));
		receiver.addState(new State(4));
		receiver.addState(new State(5));
		receiver.addState(new State(6));

		
		receiver.addTransition(1, 1, a, "!",2);
		receiver.addTransition(1, 2, m, "?", 0);
		receiver.addTransition(2, 3,Actions.RCV);
		receiver.addTransition(3, 3, a, "!", 0);
		receiver.addTransition(3, 4, m, "?", 1);
		receiver.addTransition(4, 5,Actions.RCV);
		receiver.addTransition(5, 5, a, "!", 1);
		receiver.addTransition(5, 6, m, "?", 2);
		receiver.addTransition(6, 1,Actions.RCV);

		
		// DEFINE OBSERVER
		observer.addState(new State(1, StateType.INITIAL));
		observer.addState(new State(2));
		observer.addState(new State(3));
		observer.addState(new State(4, StateType.BAD));
		
		observer.addTransition(1, 2,Actions.SND);
		observer.addTransition(2, 3,Actions.SND);
		observer.addTransition(3, 4,Actions.SND);
		observer.addTransition(2, 1,Actions.RCV);
		observer.addTransition(3, 2,Actions.RCV);
		observer.addTransition(1, 4,Actions.RCV);

		
		mod.addProgram(sender);
		mod.addProgram(receiver);
		mod.addProgram(observer);

		// SYNCHRONIZE PROGRAMS OVER ACTIONS
		mod.synchronize(sender, observer, Actions.SND);
		mod.synchronize(receiver, observer, Actions.RCV);

		System.out.println(mod.toHaskell());
	}
}