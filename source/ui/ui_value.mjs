import whind from "@candlefw/whind";
import { property_definitions } from "../properties/property_and_type_definitions.mjs";
import { getPropertyParser } from "../properties/parser.mjs";
import * as ui_productions from "./ui_productions.mjs";
import { NR, AND, OR, ONE_OF } from "../properties/productions.mjs";


const props = Object.assign({}, property_definitions);
const productions = { NR, AND, OR, ONE_OF };


export class UIValue {

    constructor(type, value, parent) {
        this.type = type;

        this.parent = parent;

        let pp = getPropertyParser(type, undefined, props, ui_productions);
        
        this.setupElement(pp, value);

        this.mount(this.parent.element)
    }

    mount(element) {
        if (element instanceof HTMLElement)
            this.element.mount(element);
    }

    update(value) {
        this.parent.update(this.type, value.toString());
    }

    setupElement(pp, value) {
        this.element = pp.buildInput(1, whind(value));
        console.log(this.element)
        this.element.parent = this;
    }
}
