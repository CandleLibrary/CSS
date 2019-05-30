export default class pseudoClassSelector{
	constructor(sym,env){
		this.val = sym[1];
	}

	get type(){
		return "pseudoClass"
	}

	match(element){
		return true;
	}

	toString(){

	}
}
