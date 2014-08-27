package xmlParser;

import java.util.Vector;

public class Model {
	Vector<Program> programs;
	Vector<Channel> channels;
	Vector<Symbol> symbols;
	Vector<Synchronized> sync;
	
	public Model (Vector<Program> programs, Vector<Channel> channels, Vector<Symbol> symbols, Vector<Synchronized> sync){
		this.programs = programs;
		this.channels = channels;
		this.symbols = symbols;
		this.sync = sync;
	}
	
	public String toHaskell(){
		String str = "module ProblemFormulation where\n";
		str+="import Data.Word\n";
		str+="import DataTypes\n\n\n";
		
		str += "symbols :: Symbols";
		str += "\nsymbols = [";
		for (int i=0;i< channels.size();i++){
			str+="[";
			for (int j=0;j< symbols.size();j++)
				str+=symbols.elementAt(j).getNumber()+",";
			str = str.substring(0, str.length()-1);
			str+="],";
		}
		str = str.substring(0, str.length()-1);
		str+="]";
		
		str += "\n\ninitial :: [Word8]";
		str += "\ninitial = [";
		for (int i=0;i<programs.size();i++){
			str+= programs.elementAt(i).getInitial().getNumber()+","; 
		}
		str = str.substring(0, str.length()-1);
		str +="]\n";
		
		str += "\n\nbad :: [(Int, Word8)]";
		str += "\nbad = [";
		for (int i=0;i<programs.size();i++){
			str+= programs.elementAt(i).getBad(); 
		}
		str = str.substring(0, str.length()-1);
		str +="]\n";
		
		str+= "\ntransitions :: [([(Int,Int,Int)], (Int,String,CWord))]";
		str+="\ntransitions = [";
		for (int i=0;i<programs.size();i++){
			Vector<Rule> rules = programs.elementAt(i).getRules();
			for (int j=0;j<rules.size();j++)
				str+= rules.elementAt(j).getFriendlyName()+",";
		}
		for (int j=0;j<sync.size();j++)
			str+= sync.elementAt(j).getFriendlyName()+",";

		str = str.substring(0, str.length()-1);
		str +="]\n\n";
		
		for (int i=0;i<programs.size();i++){
			Vector<Rule> rules = programs.elementAt(i).getRules();
			for (int j=0;j<rules.size();j++)
				str+= rules.elementAt(j).toHaskell(programs.elementAt(i).getNumber());
		}
		
		for (int i=0;i<sync.size();i++){
			str+=sync.elementAt(i).toHaskell();
		}

		str+= "\nnumStates :: [Int]\nnumStates = [";
		for (int i=0;i<programs.size();i++){
			str+=programs.elementAt(i).getNumStates()+",";
		}
		str = str.substring(0, str.length()-1);
		str +="]\n\n";
		return str;
	}
	
}
