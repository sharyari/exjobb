package model;

public class Transition {
	boolean sync = false;
	
	private State s1;
	private State s2;
	private Channel ch;
	private String symbol;
	private String effect;
	private String name;
	
	public String toHaskell(){
		if (sync)
			return "";
		String str = name+" = Rule [";
		str+=helpHaskell();
		if (ch == null) {
			str+="] []\n";
		} else {
			str+= "] ["+helpHaskell2()+"]\n";
		}
		return str;
	}

	public String helpHaskell(){
		String str = "";
		str+="("+s1.getInProgram()+","+s1.getNum()+","+s2.getNum()+")";
		return str;
	}
	public String helpHaskell2(){
		String str = "";
		if (ch != null)
		str+="("+ch.getNum()+",\""+effect+"\",\""+symbol+"\")";
		return str;
	}
	
	public Transition (State s1, State s2){
		this.s1 = s1;
		this.s2 = s2;
		ch = null; symbol = ""; effect="";
	}

	
	public void setName(String name){
		this.name = name;
	}
	
	public String getName(){
		return name;
	}
	
	public Transition (State s1, State s2, Channel ch, String effect, String symbol){
		this.s1 = s1;
		this.s2 = s2;
		this.ch = ch;
		this.symbol = symbol;
		this.effect = effect;
	}
	public void synchronize(){
		sync = true;
	}
	
	public String toString() {
		return (s1 + " " + ch + " " + s2);
	}
	
	public int getS1(){
		return s1.getNum();
	}
	
	public int getS2(){
		return s2.getNum();
	}
	
	public String getSymbol(){
		return symbol;
	}
	
	public String getEffect(){
		return effect;
	}

	public String getChannel(){
		if (ch == null)
			return null;
		return ""+ch.getNum();
	}
}
