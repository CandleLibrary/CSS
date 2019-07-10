import whind from "@candlefw/whind";
import ui_stylerule from "./ui_stylerule.mjs";

export default class UIMaster {
    constructor(css) {


        this.css = css;
        this.rule_sets = [];

        this.element = document.createElement("div");
        this.element.classList.add("cfw_css");
        this.update_mod = 0;

        this.rule_map = new Map();

        if (css) {
            css.addObserver(this);
            this.build();
        }
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
        this.rule_sets = [];
    }

    updatedCSS(css) {
        if (this.UPDATE_MATCHED) return void(this.UPDATE_MATCHED = false);
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

    update() {
        this.UPDATE_MATCHED = true;
        this.css.updated();
    }
}
