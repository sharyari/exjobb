package xmlParser;

public class Symbol {
	String friendlyName;
	int number;
	
	Symbol(String name, int number){
		friendlyName = name;
		this.number = number;
	}
	
	public int getNumber() {
		return number;
	}
	
	public String getFriendlyName(){
		return friendlyName;
	}
	
}
