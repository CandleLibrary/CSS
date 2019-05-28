export default class comboSelector{
	constructor(sym,env){
		if(sym.length > 1){
			this.op = sym[0];
			this.selector = sym[1];
		}else
			return sym[0]
	}

	get type(){
		return "basic"
	}

	match(element, result){
		this.selector.match(element, result);
		
		if(result.match){
			//return pool of candidates
			switch(this.op){
				case ">":
					return element.parentElement;
				case "+":
					return element.prevSibling;
				case "~":
					return element.parentElement.children.slice(0, element.index);
			}
		}
		
		return element;
	}

	toString(){

	}
}
