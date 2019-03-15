
import whind from "@candlefw/whind";
import UISelector from "./ui_selectors.mjs";
import * as ui_productions from "./ui_productions.mjs";
import UIProp from "./ui_properties.mjs";
import {
    property_definitions,
    media_feature_definitions,
    types
} from "../properties/property_and_type_definitions.mjs";
//import { CSSRule as R, CSSSelector as S } from "../nodes.mjs";
import { getPropertyParser } from "../properties/parser.mjs";


const props = Object.assign({}, property_definitions);
export default class UIRuleSet {
    constructor(rule_body, parent) {

        this.parent = parent;
        this.hash = 0;
        this.rules = [];
        this.selectors = null;

        this.element = document.createElement("div");
        this.element.classList.add("rule")
        this.selector_space = document.createElement("div");
        this.selector_space.classList.add("rule_selectors")
        this.rule_space = document.createElement("div");
        this.rule_space.classList.add("rule_body")

        this.element.addEventListener("dragover", dragover)
        this.element.addEventListener("drop", (e)=>{
            
            let prop = UIProp.dragee;
            let parent = prop.parent;
            let value = prop.value;
            let type = prop.type;

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

    updateSelectors(obj){
        if(obj.parts.length < 1){
            //remove selector from the rule set.
        }
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

        if(type && value){

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
        }

        this.parent.update();
    }

    addProp(type, value){
        this.update(type, value);
        //Increment the version of the rule_body
        this.rule_body.ver++;
       
        this.rebuild(this.rule_body);
    }

    removeProp(type){
        const rule = this.rule_body;
        if(rule.props[type]){
            delete rule.props[type];


            //Increment the version of the rule_body
            this.rule_body.ver++;

            this.parent.update();
            this.rebuild(this.rule_body);
        }
    }

    generateHash() {}
}

function dragover(e){
    e.preventDefault();
}