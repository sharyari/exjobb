package model;

	public class State {
		private	int num;
		private StateType type;
		private int inProgram;
		
		public State(int num){
			this.num = num;
			type = StateType.NORMAL;
		}
		
		public int getInProgram() {
			return inProgram;
		}
		
		public void setInProgram(int in) {
			inProgram = in;
		}
		
		public StateType getType(){
			return type;
		}
		
		public State(int num, StateType type){
			this.num = num;
			this.type = type;
		}
		
		public String toString(){
			if (type != StateType.NORMAL)
				return num+ " " + type;
			else
				return num+"";
		}
		
		public int getNum(){
			return num;
		}
		
	}
