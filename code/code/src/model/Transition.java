package model;

public class Transition {
	boolean sync = false;
	
	private State s1;
	private State s2;
	private Channel ch;
	private int symbol;
	private String effect;
	private String name;
	private Actions action;
	
	//////////////////////////////////////////////
	/////////////// CONSTRUCTORS /////////////////
	//////////////////////////////////////////////
	
	public Transition (State s1, State s2, Channel ch, String effect, int symbol){
		this.s1 = s1;
		this.s2 = s2;
		this.ch = ch;
		this.symbol = symbol;
		this.effect = effect;
		this.action = Actions.NONE;
	}
	
	public Transition (State s1, State s2){
		this.s1 = s1;
		this.s2 = s2;
		this.action = Actions.NONE;
		ch = null; symbol = 0; effect="";
	}

	public Transition (State s1, State s2, Actions a){
		this.s1 = s1;
		this.s2 = s2;
		this.action = a;
		sync = false;
		ch = null; symbol = 0; effect="";
	}
	
	//////////////////////////////////////////////
	////////////// GET DEPARTMENT ////////////////
	//////////////////////////////////////////////

	
	public int getS1(){
		return s1.getNum();
	}
	
	public int getS2(){
		return s2.getNum();
	}
	
	public String getSymbol(){
		return symbol+"";
	}
	
	public String getEffect(){
		return effect;
	}
	
	public String getChannel(){
		if (ch == null)
			return null;
		return ""+ch.getNum();
	}
	
	public boolean isSynchronized(){
		return sync;
	}
	
	public Actions getAction() {
		return action;
	}

	public String getName(){
		return name;
	}


	//////////////////////////////////////////////
	////////////// SET DEPARTMENT ////////////////
	//////////////////////////////////////////////
	
	public void setName(String name){
		this.name = name;
	}
	
	//////////////////////////////////////////////
	/////////////// MANIPULATION (////////////////
	//////////////////////////////////////////////

	public void synchronize(){
		sync = true;
	}
	
	
	//////////////////////////////////////////////
	//////////// STRING MANIPULATION /////////////
	//////////////////////////////////////////////
	
	public String toString() {
		return (s1 + " " + ch + " " + s2);
	}

	public String helpHaskell(){
		String str = "";
		str+="("+s1.getInProgram()+","+s1.getNum()+","+s2.getNum()+")";
		return str;
	}
	public String helpHaskell2(){
		int temp = symbol;
		String str = "";
		String tempStr = "";
		if (ch != null)
		str+="("+ch.getNum()+",\""+effect+"\",[";
		while (temp > 0){
			tempStr+= temp%10+",";
			temp=temp/10;
		}
		if (symbol != 0)
			tempStr = tempStr.substring(0, tempStr.length()-1);
		str+= new StringBuilder(tempStr).reverse().toString();
		str+="])";
		return str;
	}
	
	public String toHaskell(){
		if (sync)
			return "";
		String str = name+" = ([";
		str+=helpHaskell();
		if (ch == null) {
			str+="], (1, \"_\", []))\n";
		} else {
			str+= "], "+helpHaskell2()+")\n";
		}
		return str;
	}


}

