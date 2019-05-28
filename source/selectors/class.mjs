export default class classSelector{
	constructor(sym,env){
		this.val = sym[1];
	}

	get type(){
		return "class"
	}

	match(element, result){
		result.match = element.classList.contains(this.val);
		return element;
	}

	toString(){

	}
}
