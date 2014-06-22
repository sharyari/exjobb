package model;

	public class State {
		private	int num;
		private int inProgram;
		private StateType type;
		
		public State(int num){
			this.num = num;
			type = StateType.NORMAL; //If no state-type defined, assume normal
		}
		
		public State(int num, StateType type){
			this.num = num;
			this.type = type;
		}
		
		// Returns the number of the states associated program
		public int getInProgram() {
			return inProgram; 
		}
		
		// Sets the number of its associated program
		public void setInProgram(int in) {
			inProgram = in;
		}
		
		// Returns the type of the state
		public StateType getType(){
			return type;
		}

		public int getNum(){
			return num;
		}
		
	}
