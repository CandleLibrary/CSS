import whind from "@candlefw/whind";
import UISelector from "./ui_selectors.mjs";
import * as ui_productions from "./ui_productions.mjs";
import {
    property_definitions,
    media_feature_definitions,
    types
} from "../properties/property_and_type_definitions.mjs";
//import { CSSRule as R, CSSSelector as S } from "../nodes.mjs";
import { getPropertyParser } from "../properties/parser.mjs";

const props = Object.assign({}, property_definitions);

var dragee = null;

function dragstart(e){
    event.dataTransfer.setData('text/plain',null)
    UIProp.dragee = this;
}

export default class UIProp {
    constructor(type,  parent) {
        this.hash = 0;
        this.type = type;
        this.parent = parent;
        this.setupElement();
        this._value = null;
    }

    build(type, value){
        this.element.innerHTML = `${type}:`
        let pp = getPropertyParser(type, undefined, props, ui_productions);
        this._value = pp.buildInput(1, whind(value));
        this._value.parent = this;
        this._value.mount(this.element);
    }

    update(value) {
        this.parent.update(this.type, value.toString());
    }

    mount(element) {
        if (element instanceof HTMLElement)
            element.appendChild(this.element)
    }

    unmount() {
        if (this.element.parentElement)
            this.element.parentElement.removeChild(this.element);
    }

    setupElement() {
        this.element = document.createElement("div");
        this.element.setAttribute("draggable", "true")
        this.element.classList.add("cfw_css_ui_rule");
        this.element.addEventListener("dragstart", dragstart.bind(this))
    }

    get value(){
        return this._value.toString();
    }
}
