export default class idSelector{
	constructor(sym,env){
		this.val = sym[1];
	}

	get type(){
		return "id"
	}

	match(element){
		return element.id == this.val;
	}

	toString(){
		return "#"+ this.val;
	}
}
