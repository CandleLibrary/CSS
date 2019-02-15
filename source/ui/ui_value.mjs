import {  property_definitions } from "../properties/property_and_type_definitions.mjs";
import {  getPropertyParser } from "../properties/parser.mjs";
import * as ui_productions from "./ui_productions.mjs";

const props = Object.assign({}, property_definitions);

export class UIValue{

	constructor(type, value, parent){
		console.log(ui_productions)
		this.parent = parent;

		let pp = getPropertyParser(type, undefined, props, ui_productions)
		this.setupElement(pp);
		this.mount(this.parent.element)
		debugger;
	}

	mount(element){
		if(element instanceof HTMLElement)
			element.appendChild(this.element);
	}

	update(value){

	}

	setupElement(pp){
		this.element = pp.buildInput();
	}
}