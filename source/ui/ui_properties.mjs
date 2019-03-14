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

const UIPropCache = null;

class UIProp {
    constructor(type,  parent) {
        debugger
        // Predefine all members of this object.
        this.hash = 0;
        this.type = "";
        this.parent = null;
        this._value = null;
        this.next = null;

        this.type = type;
        this.parent = parent;
        
        if(!this.CACHED)
            this.setupElement(type);
    }

    build(type, value){
        this.element.innerHTML =""
        this.element.appendChild(this.label)
        let pp = getPropertyParser(type, undefined, props, ui_productions);
        this._value = pp.buildInput(1, whind(value));
        this._value.parent = this;
        this._value.mount(this.element);
    }
    destroy(){

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

(function(cacher){
    const cache = null;

    let constr = cacher.constructor.bind(cacher);
    console.log(cacher, constr)
    cacher.constructor = function(...args){
        debugger
            let r
        if(cache){
            r = cache;
            cache = cache.next_cached;
            r.next_cached = null;
            constr.call(r,...args);
        }else{
            r = new constr(...args);
            r.next_cached = null;
            r.CACHED = true;
        }
        return r;
    };

    let destroy = cacher.prototype.destroy;

    cacher.prototype.destroy = function(...args){

        if(destroy)
            destroy.call(this, ...args);

        this.next_cached = cache;
        cache = this;
    }
})(UIProp);

console.log(UIProp.prototype)

export default UIProp