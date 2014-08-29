package xmlParser;
import java.util.ArrayList;
import java.util.List;
import java.util.Vector;

public class Rule {
	int s1,s2, chan;
	List<Integer> symbol;
	String type;
	String friendlyName;
	
	public Rule (String friendlyname, Vector<Symbol> symbols, Vector<State> states, Vector<Channel> channels,
			String state1, String state2, String type, String symbol, String channel){
		s1 = -1;
		s2 = -1;
		this.type = type;
		this.friendlyName = friendlyname;
		this.symbol = new ArrayList<Integer>();
		String[] sym = symbol.split(",");
		for (int j=0;j<sym.length;j++){
			String s = sym[j];
			for (int i=0;i<symbols.size();i++){
				if (symbols.elementAt(i).getFriendlyName().equals(s))
					this.symbol.add(symbols.elementAt(i).getNumber());
			}
		}
		if (states==null)
			throw new RuntimeException("States missing.");
		for (int i=0;i<states.size();i++){
			if (states.elementAt(i).getFriendlyName().equals(state1))
				s1=states.elementAt(i).getNumber();
			if (states.elementAt(i).getFriendlyName().equals(state2))
				s2=states.elementAt(i).getNumber();
		}
		for (int i=0;i<channels.size();i++){
			if (channels.elementAt(i).getFriendlyName().equals(channel))
				chan=channels.elementAt(i).getNumber();
		}
		if (this.symbol.size() == 0)
			throw new RuntimeException("Message not known \""+symbol+"\". States "+state1+ " and "+state2+".");
		if (s1==-1)
			throw new RuntimeException("Unknown state "+ state1);
		if (s2==-1)
			throw new RuntimeException("Unknown state "+ state2);
		
	}
	
	public String getFriendlyName() {
		return friendlyName;
	}
	
	public String toHaskell(int programNum){
		String str=friendlyName+" = ([(";
		str+=programNum+","+s1+","+s2;
		str+=")],(";
		if (type == null)
			str+=chan+",\"_\","+symbol+")";
		else if (type.equals("send"))
			str+=chan+",\"!\","+symbol+")";
		else if (type.equals("receive"))
			str+=chan+",\"?\","+symbol+")";
		str+=")\n";
		return str;
	}
	
}
