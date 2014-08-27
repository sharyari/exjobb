package xmlParser;

public class State {

	private String friendlyName; // name of the node
	private int number; // Nodes are enumerated internally
	private int belongsTo; // The number of the program this state belongs to
	private Boolean bad;
	private Boolean initial;
	
	State (int number, String friendlyName, int belongsTo, Boolean initial, Boolean bad){
		this.friendlyName = friendlyName;
		this.number = number;
		this.belongsTo = belongsTo;
		this.bad = bad;
		this.initial = initial;
	}
	
	Boolean isInitial() {
		return initial;
	}

	Boolean isBad() {
		return bad;
	}

	
	int getNumber () {
		return number;
	}
	
	String getFriendlyName() {
		return friendlyName;
	}
	
}
