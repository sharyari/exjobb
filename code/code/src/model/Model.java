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
			p.setNumber(counter++);
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
		
		public void synchronize(Transition t1, Transition t2){
			Synchronize s = new Synchronize (t1, t2);
			s.setName("sync");
			sync.add(s);
		}

		public String toHaskell(){
			String str = "";
			for (int i=0;i<programs.size();i++){
				str+= programs.elementAt(i).toHaskell();
			}
			for (int i=0;i<sync.size();i++){
				str+= sync.elementAt(i).toHaskell();
			}
			
			str += "\ninitial = [Conf [";
			for (int i=0;i<programs.size();i++){
				str+= programs.elementAt(i).getInitial().getNum()+","; 
			}
			str = str.substring(0, str.length()-1);
			str +="] [";
			
			for (int i=0;i<channels.size();i++){
				str+= "\"\",";
			}
			str = str.substring(0, str.length()-1);
			str +="]]";

		
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
		return str;
		}
			
		}
		
