export default class selector{
	constructor(sym,env){
		if(sym.len > 1)
			this.namespace = sym[0];
		this.val = ((sym.len > 1) ? sym[2] : sym[0]).toLowerCase();
	}

	get type(){
		return "type"
	}

	match(element, result){
		return element.tagName.toLowerCase() == this.val;
	}

	toString(){

	}
}
