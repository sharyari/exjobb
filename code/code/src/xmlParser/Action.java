package xmlParser;
import java.util.Vector;
public class Action {
	String friendlyName;
	String action;
	int s1, s2;
	Boolean sync;
	int processNumber;
	Action(String friendlyName, Vector<State> states, String state1, String state2, String action, int processNumber){
		sync = false;
		this.action = action;
		for (int i=0;i<states.size();i++){
			if (states.elementAt(i).getFriendlyName().equals(state1))
				s1=states.elementAt(i).getNumber();
			if (states.elementAt(i).getFriendlyName().equals(state2))
				s2=states.elementAt(i).getNumber();
		}
		this.action = action;
		this.friendlyName = friendlyName;
		this.processNumber = processNumber;
	}
	
	String toHaskell(int programNum){
		String str=friendlyName+" = [(";
		str+=programNum+","+s1+","+s2;
		str+=")],(0, \"_\", 2))";
		return str;
	}
	
	public String getFriendlyName() {
		return friendlyName;
	}
	
	public String getAction() {
		return action;
	}
	
	public String helpHaskell() {
		return "("+processNumber+","+s1+","+s2+")";
	}
	
	void sync(){
		sync=true;
	}
	
	
}
