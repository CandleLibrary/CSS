export default class pseudoClassSelector{
	constructor(sym,env){
		this.val = sym[1];
	}

	get type(){
		return "pseudoClass"
	}

	matchReturnElement(element){
		return element;
	}

	toString(){

	}
}
