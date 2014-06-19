	package model;

	import java.util.Vector;

	public class Model {
		Vector<Program> programs = new Vector<Program>();
		Vector<Synchronize> sync = new Vector<Synchronize>();
		Vector<Channel> channels = new Vector<Channel>();

		private int counter=0;
		
		public Model(){
		}
		
		public void addChannel(Channel ch) {
			channels.add(ch);
		}

		public void addProgram(Program p){
			p.setNumber(++counter);
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
			for (int i=0;i<programs.size();i++){
				str+= programs.elementAt(i).toHaskell();
			}
			
			str += "\ninitial = [Conf [";
			for (int i=0;i<programs.size();i++){
				str+= "(" + programs.elementAt(i).getInitial().getInProgram()+","+programs.elementAt(i).getInitial().getNum()+"),"; 
			}
			str = str.substring(0, str.length()-1);
			str +="] [";
			
			for (int i=0;i<channels.size();i++){
				str+= "\"\",";
			}
			str = str.substring(0, str.length()-1);
			str +="]]";

		
			str+="\ntransitions = [";
			for (int i=0;i<programs.size();i++){
				str+= programs.elementAt(i).getTransitionNames();
				str+=",";
			}
			str = str.substring(0, str.length()-1);
			
			str+="]\n";
		return str;
		}
			
		}
		
