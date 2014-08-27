package xmlParser;

public class Channel {
	String friendlyName;
	int number;
	
	Channel (String name, int number){
		friendlyName = name;
		this.number = number;
	}
	
	String getFriendlyName() {
		return friendlyName;
	}
	
	int getNumber() {
		return number;
	}
	
}
