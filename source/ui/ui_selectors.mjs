import whind from "@candlefw/whind";
import * as ui_productions from "./ui_productions.mjs";
import {
    property_definitions,
    media_feature_definitions,
    types
} from "../properties/property_and_type_definitions.mjs";
//import { CSSRule as R, CSSSelector as S } from "../nodes.mjs";
import { getPropertyParser } from "../properties/parser.mjs";

function dragstart(e){
    event.dataTransfer.setData('text/plain',null)
    UISelectorPart.dragee = this;
}

function dragover(e){
    e.preventDefault();
}

class UISelectorPart{

    constructor(name, index){
        this.txt = name;
        this.index = index;
        this.element = document.createElement("span");
        this.element.classList.add("selector");
        this.element.innerHTML = this.txt;
        this.element.setAttribute("draggable", true)
        this.parent = null;
        this.element.addEventListener("dragstart",dragstart.bind(this))
    }

    mount(element, parent){
        this.parent = parent;
        if (element instanceof HTMLElement)
            element.appendChild(this.element);
    }

    unmount(){
        this.parent = null;
        if (this.element.parentElement)
            this.element.parentElement.removeChild(this.element);
    }

    compare(other_part){
        return other_part.txt === this.txt
    }

    toString(){
        return this.txt;
    }

};


function drop(e){
    if(UISelectorPart.dragee){
        const part = UISelectorPart.dragee;
        const parent = part.parent;

        loop:
        while(parent != this){

            //Ignore if part is already present in the selector area
            for(let i = 0; i < this.parts.length; i++)
                if(this.parts[i].compare(part)) break loop;

            part.unmount();
            let d = parent.remove(part);
            this.add(part, ...d);
            part.mount(this.element, this);
            break;
        }
    }
    UISelectorPart.dragee = null;
    e.preventDefault();
    e.stopPropagation();
    e.stopImmediatePropagation();
    return false;
}

export default class UISelector {
    constructor(selector) {
        this.selector = selector;
        this.parts = [];
        
        selector.v.forEach((e, i) => {
            this.parts.push(new UISelectorPart(e, i))
        })
        
        this.text = selector.v.join();
    }

    update() {
        this.parent.update();
    }

    mount(parent) {
        this.element = parent.selector_space;
        this.element.ondrop = drop.bind(this);
        this.element.ondragover = dragover;
        
        this.parent = parent;

        this.parts.forEach(e=>e.mount(this.element, this));
    }

    unmount() {
        if (this.element.parentElement)
            this.element.parentElement.removeChild(this.element);
    }

    remove(part){
        let index = part.index;
        this.parts.splice(index,1);
        this.parts.forEach((e,i)=>e.index = i);
        const a = this.selector.a.splice(index,1)[0];
        const v = this.selector.v.splice(index,1)[0];
        this.update();
        return [a,v]
    }

    add(part, a, v){
        this.parts.push(part)
        this.selector.a.push(a);
        this.selector.v.push(v);
        this.parts.forEach((e,i)=>e.index = i);
        this.update();
    }

    rebuild(selector){
        this.parts.forEach(e=>e.unmount(false))
        this.parts.length = 0;
        selector.v.forEach((e,i) => {
            this.parts.push(new UISelectorPart(e, i))
        })
        this.mount(this.parent);

    }

    setupElement() {
        this.element = document.createElement("div");
        this.element.classList.add("cfw_css_ui_rule");
    }
}