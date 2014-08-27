package xmlParser;

public class Synchronized {
	Action role1, role2;
	String friendlyName;
	
	public Synchronized (Action a, Action b){
		role1=a;role2=b;
		friendlyName=role1.getFriendlyName()+role2.getFriendlyName();
	}
	
	public String toHaskell(){
		String str=friendlyName+" = ([";
			str+=role1.helpHaskell()+","+role2.helpHaskell()+"],(1,\"_\",[1]))\n";
		return str;
	}
	
	public String getFriendlyName(){
		return friendlyName;
	}
	
}
