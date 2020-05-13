export default class attribSelector{
	constructor(sym,env){
		this.key = sym[1];
		this.val = "";
		this.op = "";
		this.mod = "";

		if(sym.length > 3){
			this.val = sym[3];
			this.op = sym[2];
			this.mod = sym.length > 5 ? sym[4] : "";
		}

	}

	get type(){
		return "attrib"
	}

	matchReturnElement(element, result){
		
		let attr = element.getAttribute(this.key);

		if(!attr)
			return null
		if(this.val && attr !== this.val)
			return null;
		
		return element;
	}

	toString(){
		return `[${this.key+this.op+this.val+this.mod}]`;
	}
}
