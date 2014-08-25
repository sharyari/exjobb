package ABP;



import model.Channel;
import model.Model;
import model.Program;
import model.State;
import model.StateType;
import model.Actions;

public class SlidingWindow5 {
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
		m.addSymbol(3);
		m.addSymbol(4);
		a.addSymbol(0);
		a.addSymbol(1);
		a.addSymbol(2);
		a.addSymbol(3);
		a.addSymbol(4);
		
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
		sender.addState(new State(10));
		sender.addState(new State(11));
		sender.addState(new State(12));
		sender.addState(new State(13));
		sender.addState(new State(14));
		sender.addState(new State(15));
		sender.addState(new State(16));
		sender.addState(new State(17));
		sender.addState(new State(18));
		sender.addState(new State(19));
		sender.addState(new State(20));
		sender.addState(new State(21));
		sender.addState(new State(22));
		sender.addState(new State(23));
		sender.addState(new State(24));
		sender.addState(new State(25));

		
		sender.addTransition(1, 2, Actions.SND);
		sender.addTransition(2, 3, Actions.SND);
		sender.addTransition(3, 4, Actions.SND);
		sender.addTransition(4, 5, Actions.SND);
		sender.addTransition(2, 2, m, "!", 0);
		sender.addTransition(3, 3, m, "!", 10);
		sender.addTransition(4, 4, m, "!", 210);
		sender.addTransition(5, 5, m, "!", 3210);
		sender.addTransition(2, 6, a, "?", 0);
		sender.addTransition(3, 7, a, "?", 0);
		sender.addTransition(4, 8, a, "?", 0);
		sender.addTransition(5, 9, a, "?", 0);

		sender.addTransition(6, 7, Actions.SND);
		sender.addTransition(7, 8, Actions.SND);
		sender.addTransition(8, 9, Actions.SND);
		sender.addTransition(9, 10, Actions.SND);
		sender.addTransition(7, 7, m, "!", 1);
		sender.addTransition(8, 8 ,m, "!", 21);
		sender.addTransition(9, 9, m, "!", 321);
		sender.addTransition(10, 10, m, "!", 4321);
		sender.addTransition(7, 11, a, "?", 1);
		sender.addTransition(8, 12, a, "?", 1);
		sender.addTransition(9, 13, a, "?", 1);
		sender.addTransition(10, 14, a, "?", 1);

		sender.addTransition(11, 12, Actions.SND);
		sender.addTransition(12, 13, Actions.SND);
		sender.addTransition(13, 14, Actions.SND);
		sender.addTransition(14, 15, Actions.SND);
		sender.addTransition(12, 12, m, "!", 2);
		sender.addTransition(13, 13 ,m, "!", 32);
		sender.addTransition(14, 14, m, "!", 432);
		sender.addTransition(15, 15, m, "!", 0432);
		sender.addTransition(12, 16, a, "?", 2);
		sender.addTransition(13, 17, a, "?", 2);
		sender.addTransition(14, 18, a, "?", 2);
		sender.addTransition(15, 19, a, "?", 2);

		sender.addTransition(16, 17, Actions.SND);
		sender.addTransition(17, 18, Actions.SND);
		sender.addTransition(18, 19, Actions.SND);
		sender.addTransition(19, 20, Actions.SND);
		sender.addTransition(17, 17, m, "!", 3);
		sender.addTransition(18, 18 ,m, "!", 43);
		sender.addTransition(19, 19, m, "!", 043);
		sender.addTransition(20, 20, m, "!", 1043);
		sender.addTransition(17, 21, a, "?", 3);
		sender.addTransition(18, 22, a, "?", 3);
		sender.addTransition(19, 23, a, "?", 3);
		sender.addTransition(20, 24, a, "?", 3);

		sender.addTransition(21, 22, Actions.SND);
		sender.addTransition(22, 23, Actions.SND);
		sender.addTransition(23, 24, Actions.SND);
		sender.addTransition(24, 25, Actions.SND);
		sender.addTransition(22, 22, m, "!", 4);
		sender.addTransition(23, 23 ,m, "!", 04);
		sender.addTransition(24, 24, m, "!", 104);
		sender.addTransition(25, 25, m, "!", 2104);
		sender.addTransition(22, 1, a, "?", 4);
		sender.addTransition(23, 2, a, "?", 4);
		sender.addTransition(24, 3, a, "?", 4);
		sender.addTransition(25, 4, a, "?", 4);		

		// DEFINE RECEIVER
		receiver.addState(new State(1, StateType.INITIAL));
		receiver.addState(new State(2));
		receiver.addState(new State(3));
		receiver.addState(new State(4));
		receiver.addState(new State(5));
		receiver.addState(new State(6));
		receiver.addState(new State(7));
		receiver.addState(new State(8));
		receiver.addState(new State(9));
		receiver.addState(new State(10));
		
		receiver.addTransition(1, 1, a, "!", 4);
		receiver.addTransition(1, 2, m, "?", 0);
		receiver.addTransition(2, 3, Actions.RCV);
		receiver.addTransition(3, 3, a, "!", 0);
		receiver.addTransition(3, 4, m, "?", 1);
		receiver.addTransition(4, 5, Actions.RCV);
		receiver.addTransition(5, 5, a, "!", 1);
		receiver.addTransition(5, 6, m, "?", 2);
		receiver.addTransition(6, 7, Actions.RCV);
		receiver.addTransition(7, 7, a, "!", 2);
		receiver.addTransition(7, 8, m, "?", 3);
		receiver.addTransition(8, 9, Actions.RCV);
		receiver.addTransition(9, 9, a, "!", 3);
		receiver.addTransition(9, 10, m, "?", 4);
		receiver.addTransition(10, 1, Actions.RCV);

		// DEFINE OBSERVER
		observer.addState(new State(1, StateType.INITIAL));
		observer.addState(new State(2));
		observer.addState(new State(3));
		observer.addState(new State(4));
		observer.addState(new State(5));
		observer.addState(new State(6, StateType.BAD));
		
		observer.addTransition(1, 2,Actions.SND);
		observer.addTransition(2, 3,Actions.SND);
		observer.addTransition(3, 4,Actions.SND);
		observer.addTransition(4, 5,Actions.SND);
		observer.addTransition(5, 6,Actions.SND);
		observer.addTransition(2, 1,Actions.RCV);
		observer.addTransition(3, 2,Actions.RCV);
		observer.addTransition(4, 3,Actions.RCV);
		observer.addTransition(5, 4,Actions.RCV);
		observer.addTransition(1, 6,Actions.RCV);

		
		mod.addProgram(sender);
		mod.addProgram(receiver);
		mod.addProgram(observer);

		// SYNCHRONIZE PROGRAMS OVER ACTIONS
		mod.synchronize(sender, observer, Actions.SND);
		mod.synchronize(receiver, observer, Actions.RCV);

		System.out.println(mod.toHaskell());
	}
}