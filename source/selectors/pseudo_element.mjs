export default class pseudoElementSelector{
	constructor(sym,env){
		this.val = sym[1].val;
	}

	get type(){
		return "pseudoElement"
	}

	match(element, result){
		result.match = true;
		return element;
	}

	toString(){

	}
}
