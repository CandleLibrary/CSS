class UIMaster {
	constructor(css){
		this.css = css;
		this.rule_sets = [];

		this.element = document.createElement("div");
	}	

	render(){
		for(let i = 0; i < this.rule_sets.length; i++)
			this.rule_sets.render(this.element);
	}
}

class UIRuleSet{
	constructor(){
		this.hash = 0;
		this.rules = [];
		this.selectors = [];
	}

	generateHash(){

	}
}

class UIRule{
	constructor(){
		this.hash = 0;
		this.type = null;

		this.setupElement();
	}

	setupElement(){
		this.element = document.createElement("div");
		this.element.classList.add("cfw_css_ui_rule");
	}

	generateHash(){
		
	}
}

class UIValue{
	constructor(parent){
		this.parent = parent;
	}

	update(value){

	}

	setupElement(){
		this.element = document.createElement("input");
		this.element.classList.add("cfw_css_ui_rule");
	}

	setupElement(){
		this.element = document.createElement("div");
		this.element.classList.add("cfw_css_ui_value");
	}
}