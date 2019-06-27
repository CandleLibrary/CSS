export default class idSelector{
	constructor(sym,env){
		this.val = sym[1];
	}

	get type(){
		return "id"
	}

	matchReturnElement(element){
		return element.id == this.val ? element : null;
	}

	toString(){
		return "#"+ this.val;
	}
}
