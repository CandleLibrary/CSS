//import { UIValue } from "./ui_value.mjs";


import whind from "@candlefw/whind";
import * as ui_productions from "./ui_productions.mjs";
import {
    property_definitions,
    media_feature_definitions,
    types
} from "../properties/property_and_type_definitions.mjs";
//import { CSSRule as R, CSSSelector as S } from "../nodes.mjs";
import { getPropertyParser } from "../properties/parser.mjs";


const props = Object.assign({}, property_definitions);


export default class UIMaster {
    constructor(css) {
        css.addObserver(this);
        this.css = css;
        this.rule_sets = [];
        this.selectors = [];
        this.element = document.createElement("div");


        this.rule_map = new Map();
    }

    // Builds out the UI elements from collection of rule bodies and associated selector groups. 
    // css - A CandleFW_CSS object. 
    // meta - internal 
    build(css = this.css) {

        //Extract rule bodies and set as keys for the rule_map. 
        //Any existing mapped body that does not have a matching rule should be removed. 
        
        const rule_sets = css.children;

        for(let i= 0; i < rule_sets.length; i++){
            let rule_set = rule_sets[i];

            for(let i = 0; i < rule_set.rules.length; i++){

                let rule = rule_set.rules[i];
                console.log(i, rule)

                if(!this.rule_map.get(rule))
                    this.rule_map.set(rule, new UIPropSet(rule, this));
                else {
                    this.rule_map.get(rule).rebuild(rule);
                }
            }

        
            const selector_array = rule_set._sel_a_;

            for(let i = 0; i < selector_array.length; i++){
                let selector = selector_array[i];
                let rule_ref = selector.r;

                let rule_ui = this.rule_map.get(rule_ref);

                rule_ui.addSelector(selector);
            }
        }


        this.css = css;

        let children = css.children;

        this.rule_sets = [];
        this.selectors = [];
    }

    updatedCSS(css) {
        if(this.UPDATE_MATCHED) return void (this.UPDATE_MATCHED = false);      
        //this.element.innerHTML = "";
        this.build(css);
        //this.render();
    }

    render() {
        for (let i = 0; i < this.rule_sets.length; i++)
            this.rule_sets.render(this.element);
    }

    mount(element) {
        if (element instanceof HTMLElement)
            element.appendChild(this.element);
    }

    unmount() {
        if (this.element.parentElement)
            this.element.parentElement.removeChild(this.element);
    }

    update(){
        this.UPDATE_MATCHED = true;
    	this.css.updated();
    }
}

class UIPropSet {
    constructor(rule_body, parent) {

        this.parent = parent;
        this.hash = 0;
        this.rules = [];
        this.selectors = null;

        this.element = document.createElement("div");
        this.selector_space = document.createElement("div");
        this.rule_space = document.createElement("div");

        this.element.addEventListener("dragover", dragover)
        this.element.addEventListener("drop", (e)=>{
            
            let parent = dragee.parent;
            let value = dragee.value;
            let type = dragee.type;

            if(parent === this)
                return;

            this.addProp(type, value);
            parent.removeProp(type)

            //move the dragee's data into this ruleset
        })

        this.element.appendChild(this.selector_space);
        this.element.appendChild(this.rule_space);

        this.build(rule_body);
        this.mount(this.parent.element);

        this.ver = rule_body;
    }

    addData(){

    }
    addSelector(selector){

        //Add to list of selectors and update UI
        if(!this.selectors){

            this.selectors = new UISelector(selector);

            this.selectors.mount(this);
        }else{
            this.selectors.rebuild(selector);
        }
    }

    mount(element) {
        if (element instanceof HTMLElement)
            element.appendChild(this.element);
    }

    unmount() {
        if (this.element.parentElement)
            this.element.parentElement.removeChild(this.element);
    }

    build(rule_body = this.rule_body) {

        this.rule_body = rule_body;

        let i = -1;

        for (let a in rule_body.props) {
            let rule;
            
            //Reuse Existing Rule Bodies
            if(++i < this.rules.length){
                rule = this.rules[i];
            }else{
                rule = new UIProp(a,  this);
                this.rules.push(rule);
            }
            console.log(rule_body.toString(0, a))
            rule.build(a, rule_body.toString(0, a));
            rule.mount(this.rule_space)
        }
    }

    rebuild(rule_body){
        if(this.ver !== rule_body.ver){
            this.rule_space.innerHTML = "";
            this.rules.length = 0;
            this.build(rule_body);
            this.ver = this.rule_body.ver;
        }
    }

    update(type, value) {
        
        let lexer = whind(value);
        
        const IS_VIRTUAL = {
            is: false
        };
        
        const parser = getPropertyParser(type, IS_VIRTUAL, property_definitions);
        const rule = this.rule_body;
        if (parser && !IS_VIRTUAL.is) {
            if (!rule.props) rule.props = {};
            parser.parse(lexer, rule.props);
        }

        this.parent.update();
    }

    addProp(type, value){
        this.update(type, value);
        this.rebuild(this.rule_body);
    }

    removeProp(type){
        const rule = this.rule_body;
        if(rule.props[type]){
            delete rule.props[type];
            this.parent.update();
            this.rebuild(this.rule_body);
        }
    }

    generateHash() {}
}


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
class UISelector {
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



class UIProp {
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
        console.log(`${this.type}:${value};`)
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
        this.element.addEventListener("dragstart", drag.bind(this))
    }

    get value(){
        return this._value.toString();
    }
}

var dragee = null;
function drag(e){
        event.dataTransfer.setData('text/plain',null)
    dragee = this;
}

function dragover(e){
    e.preventDefault();
}