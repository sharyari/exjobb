package model;

import java.util.Vector;

public class Model {
	Vector<Program> programs = new Vector<Program>();
	Vector<Synchronize> sync = new Vector<Synchronize>();
	Vector<Channel> channels = new Vector<Channel>();

	public Model(){
	}
	
	public void addChannel(Channel ch) {
		channels.add(ch);
	}

	public void addProgram(Program p){
		programs.add(p);
	}
	
	public String toString(){
		String str = "";
		for (int i = 0; i< programs.size();i++){
			str += "\n\nProgram " + i + ": \n";
			str += programs.elementAt(i);
		}
		return str;
	}

	public String toHaskell(){
		String str = "";
		for(int i=0;i<programs.size();i++){
			Program p = programs.elementAt(i);
			Vector <Transition> transitions = p.getTransitions();
			for (int j=0;j<transitions.size();j++){
				Transition t = transitions.elementAt(j);
				String rule = t.getName() + " (Conf ";
				for(int k=0;k<programs.size();k++){
					if (k==i){
						rule += t.getS1()+ " ";
					} else {
						rule += programs.elementAt(k).getName() + " ";
					}
				}
				
				for (int k=0; k < channels.size();k++){
					if (t.getChannel() != null && t.getChannel() == channels.elementAt(k).getName() &&
							t.getEffect() == "?"){
							rule += "('"+t.getSymbol()+"':"+t.getChannel()+") ";
					} else {
						rule += channels.elementAt(k).getName()+" ";
					}
				}
				rule = rule.substring(0,rule.length()-1);
				rule +=") = \n	(Conf ";
				
				for(int k=0;k<programs.size();k++){
					if (k==i){
						rule += t.getS2()+ " ";
					} else {
						rule += programs.elementAt(k).getName() + " ";
					}
				}
				
				for (int k=0; k < channels.size();k++){
					if (t.getChannel() != null && t.getChannel() == channels.elementAt(k).getName() &&
							t.getEffect() == "!"){
							rule += "("+(t.getChannel()+"++['"+t.getSymbol()+"']) ");
					} else {
						rule += channels.elementAt(k).getName()+" ";
					}
				}
				rule = rule.substring(0,rule.length()-1);

				rule +=")\n";
				rule += t.getName() + " _ = Null\n";

				
				
				str += rule+"\n\n";
			}
		}
	
		str += "transitions = [";
		for (int i=0;i<programs.size();i++){
			Program p = programs.elementAt(i);
			for(int j=0;j<p.getTransitions().size();j++){
				str += p.getTransitions().elementAt(j).getName()+",";
			}
		}
		str = str.substring(0,str.length()-1);
		str += "]\n\n";
		
		str += "initial = [(Conf ";
		for(int i=0;i<programs.size();i++){
			str+= programs.elementAt(i).getInitial()+" ";
		}

		for(int i=0;i<channels.size();i++){
			str+= channels.elementAt(i).getInitial()+" ";
		}

		str +=")]\n\n";

		
		
		return str;
		}
		
	}
	
