import whind from "@candlefw/whind";
import cached_factory from "@candlefw/cached_factory"

import ui_stylerule from "./ui_stylerule.js";

class UI_stylesheet {

    constructor(css) {

        this.css = null;
        
        this.rule_map = null;

        this.element = document.createElement("div");
        
        this.element.classList.add("cfw_css");

        this.update_mod = 0;
    }

    initializer(css){

        this.css = css;

        this.rule_map = new Map();

        if (css) {
            css.addObserver(this);
            this.build();
        }
    }

    destructor(){

        this.unmount();

        this.css && this.css.removeObserver(this);
        
        for(const rule of this.rule_map.values())
            rule.destroy();

        this.rule_map = null;

        this.css = null;

        this.update_mod = 0;
    }

    destroy(){
        cached_factory.collect(this);
    }

    // Builds out the UI elements from collection of rule bodies and associated selector groups. 
    // css - A CandleFW_CSS object. 
    // meta - internal 
    build(css = this.css) {
        if (this.update_mod++ % 3 !== 0) return;

        //Extract rule bodies and set as keys for the rule_map. 
        //Any existing mapped body that does not have a matching rule should be removed. 

        const rule_set = css.ruleset;

        for (const rule of rule_set.rules) {

            if (!this.rule_map.get(rule))
                this.rule_map.set(rule, new ui_stylerule(rule, this));
            else {
                this.rule_map.get(rule).rebuild(rule);
            }
        }

        this.css = css;
    }

    updatedCSS(css) {
        if (this.UPDATE_MATCHED) return void(this.UPDATE_MATCHED = false);
        this.build(css);
    }

    mount(element) {
        if (element instanceof HTMLElement)
            element.appendChild(this.element);
    }

    unmount() {
        if (this.element.parentElement)
            this.element.parentElement.removeChild(this.element);
    }

    update() {
        this.UPDATE_MATCHED = true;
        this.css.updated();
    }
}

export default cached_factory(UI_stylesheet);
