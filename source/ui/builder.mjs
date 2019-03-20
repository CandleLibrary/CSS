//import { UIValue } from "./ui_value.mjs";


import whind from "@candlefw/whind";
import * as ui_productions from "./ui_productions.mjs";
import UIRuleSet from "./ui_ruleset.mjs";
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
        this.element.classList.add("cfw_css");


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

                if(!this.rule_map.get(rule))
                    this.rule_map.set(rule, new UIRuleSet(rule, this));
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
