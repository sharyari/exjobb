package model;

import java.util.Vector;

public class Channel {
	private Integer num;
	static Integer counter = 0;
	Vector<String> symbols = new Vector<String>();

	public void addSymbol(String s){
		symbols.add(s);
	}
	
	public String getSymbols() {
		String str = "";
		for (int i=0;i<symbols.size();i++)
			str+="\""+symbols.elementAt(i)+"\",";
		str = str.substring(0, str.length()-1);

		return str;
	}
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
