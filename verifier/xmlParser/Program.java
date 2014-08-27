package xmlParser;
import java.util.Vector;

public class Program {
	String friendlyName;
	Vector<State> states;
	Vector<Rule> rules;
	Vector<Channel> channels;
	Vector<Symbol> symbols;
	Vector<Action> actions;
	int number;
	
	Program(String name, int number, Vector <State>states, Vector<Rule> rules, Vector<Action> actions, Vector<Channel> channels, Vector<Symbol> symbols){
		friendlyName = name;
		this.rules = rules;
		this.channels = channels;
		this.states = states;
		this.symbols = symbols;
		this.number=number;
		this.actions=actions;
	}

	State getInitial(){
		for (int i=0;i<states.size();i++){
			State temp = states.elementAt(i);
			if (temp.isInitial())
				return states.elementAt(i);
		}
		return states.elementAt(0);
	}
	
	Vector<Rule> getRules() {
		return rules;
	}
	
	public String getFriendlyName(){
		return friendlyName;
	}
	
	int getNumber() {
		return number;
	}
	
	public int getNumStates(){
		return states.size();
	}
	
	public String getBad(){
		String str="";
		for (int i=0;i<states.size();i++){
			if (states.elementAt(i).isBad())
				str+="("+number+","+states.elementAt(i).getNumber()+"),";
		}
		return str;
	}
	
	Vector<Action> synchronize(String action){
		Vector<Action> found = new Vector<Action>();
		for (int i=0;i<actions.size();i++){
			Action temp = actions.elementAt(i);
			if (temp.getAction().equals(action)) {
				found.add(temp);
				temp.sync();
			}
		}
		return found;
	}
	
}
