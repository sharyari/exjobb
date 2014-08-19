package ABP;



import model.Channel;
import model.Model;
import model.Program;
import model.State;
import model.StateType;
import model.Actions;

public class BoundedRetransmission {
	public static void main(String[] args) {

		// CREATE MODEL
		Model mod = new Model();

		// CREATE CHANNELS
		Channel m = new Channel();
		Channel a = new Channel();

		// ADD CHANNELS TO MODEL
		mod.addChannel(m);
		mod.addChannel(a);
		
		m.addSymbol("f");
		m.addSymbol("l");
		m.addSymbol("1");
		m.addSymbol("2");
		a.addSymbol("f");
		a.addSymbol("l");
		a.addSymbol("1");
		a.addSymbol("2");

		
		
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
		
		sender.addTransition(1,2,Actions.REQ);
		sender.addTransition(2,3,m,"!","f");
		sender.addTransition(3, 3, m, "!", "f");		
		sender.addTransition(3, 4, a, "?", "f");
		sender.addTransition(4, 5, m, "!", "0");
		sender.addTransition(4, 8, m, "!", "l");
		sender.addTransition(5, 5, m, "!", "0");
		sender.addTransition(5, 6, a, "?", "0");
		sender.addTransition(6, 8, m, "!", "l");
		sender.addTransition(6, 7, m, "!", "1");
		sender.addTransition(7, 7, m, "!", "1");
		sender.addTransition(7, 8, a, "?", "1");
		sender.addTransition(7,1,Actions.SNOK);
		sender.addTransition(3,1,Actions.SNOK);
		sender.addTransition(5,1,Actions.SNOK);
		sender.addTransition(8, 8, m, "!", "l");
		sender.addTransition(8,1,Actions.SOK);
		sender.addTransition(8,1,Actions.SDNK);

		
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

		
		
		receiver.addTransition(1, 2, Actions.RFST);
		receiver.addTransition(2, 3, m, "?", "f");
		receiver.addTransition(3, 3, a, "!", "f");
		receiver.addTransition(3, 4, Actions.RINC);
		receiver.addTransition(3, 1, Actions.RNOK);
		receiver.addTransition(3, 8, Actions.ROK);
		receiver.addTransition(4, 5, m, "?", "0");
		receiver.addTransition(5, 5, a, "!", "0");
		receiver.addTransition(5, 1, Actions.RNOK);
		receiver.addTransition(5, 5, a, "!", "0");
		receiver.addTransition(5, 6, Actions.RINC);
		receiver.addTransition(5, 8, Actions.ROK);
		receiver.addTransition(6, 7, m, "?", "1");
		receiver.addTransition(7, 7, a, "!", "1");	
		receiver.addTransition(7, 1, Actions.RNOK);	
		receiver.addTransition(7, 5, m, "?", "0");	
		receiver.addTransition(7, 8, Actions.ROK);
		receiver.addTransition(8, 9, m, "?","l");
		receiver.addTransition(9, 9, a, "!", "l");
		receiver.addTransition(9, 1);
		
		// DEFINE OBSERVER
		observer.addState(new State(1, StateType.INITIAL));
		observer.addState(new State(2));
		observer.addState(new State(3, StateType.BAD));
		
		observer.addTransition(1, 3,Actions.SNOK);
		observer.addTransition(1, 3,Actions.SOK);
		observer.addTransition(1, 3,Actions.SDNK);
		observer.addTransition(1, 2,Actions.REQ);
		observer.addTransition(2, 3,Actions.REQ);
		observer.addTransition(2, 1,Actions.SDNK);
		observer.addTransition(2, 1,Actions.SOK);
		observer.addTransition(2, 1,Actions.SNOK);
		
		mod.addProgram(sender);
		mod.addProgram(receiver);
		mod.addProgram(observer);

		// SYNCHRONIZE PROGRAMS OVER ACTIONS
		mod.synchronize(sender, observer, Actions.SOK);
		mod.synchronize(sender, observer, Actions.REQ);
		mod.synchronize(sender, observer, Actions.SDNK);
		mod.synchronize(sender, observer, Actions.SNOK);

		
		System.out.println(mod.toHaskell());
	}
}