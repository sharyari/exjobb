package xmlParser;

public class Channel {
	String friendlyName;
	int number;
	
	public Channel (String name, int number){
		friendlyName = name;
		this.number = number;
	}
	
	public String getFriendlyName() {
		return friendlyName;
	}
	
	int getNumber() {
		return number;
	}
	
}
