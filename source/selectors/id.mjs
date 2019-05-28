export default class idSelector{
	constructor(sym,env){
		this.val = sym[1];
	}

	get type(){
		return "id"
	}

	match(element, result){
		result.match = element.id == this.val;
		return element;
	}

	toString(){

	}
}
