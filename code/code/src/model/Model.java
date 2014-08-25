	package model;

	import java.util.Vector;

	public class Model {
		Vector<Program> programs = new Vector<Program>();
		Vector<Synchronize> sync = new Vector<Synchronize>();
		Vector<Channel> channels = new Vector<Channel>();
		
		private int counter=0;
		
		//////////////////////////////////////////////
		/////////////// CONSTRUCTORS /////////////////
		//////////////////////////////////////////////
				
		public Model(){
		}
		
		//////////////////////////////////////////////
		/////////////// MANIPULATION /////////////////
		//////////////////////////////////////////////

		public void addChannel(Channel ch) {
			channels.add(ch);
		}
		
		public void addProgram(Program p){
			p.setNumber(counter++);
			programs.add(p);
		}
		
		//////////////////////////////////////////////
		//////////// STRING MANIPULATION /////////////
		//////////////////////////////////////////////
		
		public String toString(){
			String str = "";
			for (int i = 0; i< programs.size();i++){
				str += "\n\nProgram " + i + ": \n";
				str += programs.elementAt(i);
			}
			return str;
		}
		
		public void synchronize(Program p1, Program p2, Actions a){
			Vector<Transition> t1 = p1.getTransitions();
			Vector<Transition> t2 = p2.getTransitions();
			
			for (int i=0;i<t1.size();i++){
				Transition e1 = t1.elementAt(i);
					for (int j=0;j<t2.size();j++){
						Transition e2 = t2.elementAt(j);
							if (e1.getAction() == e2.getAction() && e1.getAction() == a){
								Synchronize s = new Synchronize (t1.elementAt(i), t2.elementAt(j));
								s.setName("sync");
								sync.add(s);
		}	}	}	}

		public String toHaskell(){
			String str = "module ProblemFormulation where\n";
			str+="import Data.Word\n";
			str+="import DataTypes\n\n\n";
			
			str += "symbols :: Symbols";
			str += "\nsymbols = [";
			for (int i=0;i< channels.size();i++){
				str+="["+channels.elementAt(i).getSymbols()+"],";
			}
			str = str.substring(0, str.length()-1);
			str+="]";
			
			str += "\n\ninitial :: [Word8]";
			str += "\ninitial = [";
			for (int i=0;i<programs.size();i++){
				str+= programs.elementAt(i).getInitial().getNum()+","; 
			}
			str = str.substring(0, str.length()-1);
			str +="]\n";

			str+= "\ntransitions :: [([(Int,Int,Int)], (Int,String,CWord))]";
			str+="\ntransitions = [";
			String str2 = "";
			for (int i=0;i<programs.size();i++){
				str2= programs.elementAt(i).getTransitionNames();
				if (str2!="")
					str+=str2+",";
			}
			for (int i=0;i<sync.size();i++){
				str+= sync.elementAt(i).getName()+",";
			}
			str = str.substring(0, str.length()-1);
			str+="]\n";

			str+="\n";
			for (int i=0;i<programs.size();i++){
				str+= programs.elementAt(i).toHaskell();
			}
			for (int i=0;i<sync.size();i++){
				str+= sync.elementAt(i).toHaskell();
			}
			str+= "bad :: [(Int, Word8)]";
			str+= "\nbad = [";
			for (int i=0;i<programs.size();i++)
				str+=programs.elementAt(i).printBad();
			str = str.substring(0, str.length()-1);
			str+= "]";
			
			
		return str;
		}
			
		}
		
