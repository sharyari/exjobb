package model;

public class Channel {
	private String name;
	
	public Channel(String name){
		this.name = name;
	}

	public Channel(){
		this.name = "channel";
	}
	
	public String toString() {
		return name;
	}
	
	public String getName(){
		return name;
	}
	
}
