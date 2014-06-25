package model;

public class Channel {
	private Integer num;
	static Integer counter = 0;
		
	public Channel(){
		num = counter++;
	}
	
	public String toString() {
		return num+"";
	}
	
	public Integer getNum(){
		return num;
	}
	
}
