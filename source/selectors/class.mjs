export default class classSelector{
	constructor(sym,env){
		this.val = sym[1];
	}

	get type(){
		return "class"
	}

	matchReturnElement(element, window){
		return element.classList.contains(this.val) ? element : null;
	}

	toString(){
		return "."+this.val;
	}
}
