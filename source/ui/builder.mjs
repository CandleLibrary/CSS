import { UIValue } from "./ui_value.mjs";
export default class UIMaster {
	constructor(css){
		this.css = css;
		this.rule_sets = [];
		this.selectors = [];
		this.element = document.createElement("div");
	}

	build(css = this.css){

		this.css = css;

		let children = css.children;

		
		for(let i = 0; i < children.length; i++){
			let r = new UIRuleSet(children[i], this);
		}
	}	

	render(){
		for(let i = 0; i < this.rule_sets.length; i++)
			this.rule_sets.render(this.element);
	}

	mount(element){
		if(element instanceof HTMLElement)
			element.appendChild(this.element);
	}

	unmount(){
		if(this.element.parentElement)
			this.element.parentElement.removeChild(this.element);
	}
}

class UIRuleSet{
	constructor(rule_body, parent){
		this.parent = parent;
		this.hash = 0;
		this.rules = [];
		this.selectors = [];

		this.element = document.createElement("div");

		this.build(rule_body._sel_a_[0].r);
		this.mount(this.parent.element)
	}

	mount(element){
		if(element instanceof HTMLElement)
			element.appendChild(this.element);
	}

	unmount(){
		if(this.element.parentElement)
			this.element.parentElement.removeChild(this.element);
	}

	build(rule_body = this.rule_body){
		this.rule_body = rule_body;

		for(let a in rule_body.props){
			let rule = new UIRule(a, rule_body.props[a], this);
		}
	}

	generateHash(){

	}
}

class UIRule{
	constructor(type, value, parent){
		this.hash = 0;
		this.type = type;
		this.parent = parent;
		this.setupElement();

		this.element.innerHTML = `${type}`
		
		this.value = new UIValue(type, value, this);

		this.mount(this.parent.element)
	}

	mount(element){
		if(element instanceof HTMLElement)
			element.appendChild(this.element);
	}

	unmount(){
		if(this.element.parentElement)
			this.element.parentElement.removeChild(this.element);
	}

	setupElement(){
		this.element = document.createElement("div");
		this.element.classList.add("cfw_css_ui_rule");
	}

	generateHash(){
		
	}
}

