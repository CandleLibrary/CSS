import whind from "@candlefw/whind";
import UISelector from "./ui_selectors.mjs";
import * as ui_productions from "./ui_productions.mjs";
import createCache from "./create_cache.mjs";
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

class UIProp {
    constructor(type,  parent) {
        // Predefine all members of this object.
        this.hash = 0;
        this.type = "";
        this.parent = null;
        this._value = null;
        this.setupElement(type);
        this.init(type, parent)
    }

    init(type,  parent){
        this.type = type;
        this.parent = parent;
    }

    destroy(){
        this.hash = 0;
        this.type = "";
        this.parent = null;
        this._value = null;
        this.type = null;
        this.parent = null;
        this.unmount();
    }

    build(type, value){
        this.element.innerHTML =""
        this.element.appendChild(this.label)
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

    setupElement(type) {
        this.element = document.createElement("div");
        this.element.setAttribute("draggable", "true")
        this.element.classList.add("prop");
        this.element.addEventListener("dragstart", dragstart.bind(this));
        this.label = document.createElement("span")
        this.label.classList.add("prop")
        this.label.innerHTML = `${type.replace(/[\-\_]/g, " ")}`;
    }

    get value(){
        return this._value.toString();
    }
}

UIProp = createCache(UIProp);

console.log(UIProp.prototype)

export default UIProp
