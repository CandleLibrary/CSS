import whind from "@candlefw/whind";
import * as ui_productions from "./ui_productions.mjs";
import {
    property_definitions,
    media_feature_definitions,
    types
} from "../properties/property_and_type_definitions.mjs";
//import { CSSRule as R, CSSSelector as S } from "../nodes.mjs";
import { getPropertyParser } from "../properties/parser.mjs";


class UISelectorPart{
    constructor(name){
        this.txt = name;

        this.element = document.createElement("span");
        this.element.classList.add("selector");
        this.element.innerHTML = this.txt;

    }

    mount(element){
        if (element instanceof HTMLElement)
            element.appendChild(this.element);
    }

    unmount(){
        if (this.element.parentElement)
            this.element.parentElement.removeChild(this.element);
    }

}

export default class UISelector {
    constructor(selector) {
        this.selector = selector;
        this.parts = [];
        selector.v.forEach(e => {
            this.parts.push(new UISelectorPart(e))
        })
        this.text = selector.v.join();
    }

    update(type, value) {
        console.log(`${type}:${value};`)
        this.parent.update(type, value);
    }

    mount(parent) {
        this.parent = parent;
        let selector_div = parent.selector_space;

        this.parts.forEach(e=>e.mount(selector_div));
    }

    unmount() {
        if (this.element.parentElement)
            this.element.parentElement.removeChild(this.element);
    }


    rebuild(selector){
        this.parts.forEach(e=>e.unmount())
        this.parts.length = 0;
        selector.v.forEach(e => {
            this.parts.push(new UISelectorPart(e))
        })
        this.mount(this.parent);

    }

    setupElement() {
        this.element = document.createElement("div");
        this.element.classList.add("cfw_css_ui_rule");
    }
}