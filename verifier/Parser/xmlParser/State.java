package xmlParser;

public class State {

	private String friendlyName; // name of the node
	private int number; // Nodes are enumerated internally
	private int belongsTo; // The number of the program this state belongs to
	private Boolean bad;
	private Boolean initial;
	
	public State (int number, String friendlyName, int belongsTo, Boolean initial, Boolean bad){
		this.friendlyName = friendlyName;
		this.number = number;
		this.belongsTo = belongsTo;
		this.bad = bad;
		this.initial = initial;
	}
	
	public Boolean isInitial() {
		return initial;
	}

	public Boolean isBad() {
		return bad;
	}

	
	public int getNumber () {
		return number;
	}
	
	public String getFriendlyName() {
		return friendlyName;
	}
	
}
