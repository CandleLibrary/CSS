export default class classSelector{
	constructor(sym,env){
		this.val = sym[1];
	}

	get type(){
		return "class"
	}

	match(element, result){
		return element.classList.contains(this.val);
	}

	toString(){
		return "."+this.val;
	}
}
