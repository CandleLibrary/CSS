import whind from "@candlefw/whind";
import UI_selector from "./ui_selectors.js";
import cached_factory from "@candlefw/cached_factory"
import ui_prop from "./ui_property.js";
import * as ui_productions from "./ui_productions.js";

class ui_stylerule {

    constructor(stylerule, parent) {

        this.parent = null;
        this.stylerule = null;
        this.hash = 0;
        this.props = [];
        this.selectors = [];


        //HTML
        this.element = document.createElement("div");
        this.element.classList.add("rule")
        this.selector_space = document.createElement("div");
        this.selector_space.classList.add("rule_selectors")
        this.rule_space = document.createElement("div");

        this.rule_space.classList.add("stylerule");
        this.element.addEventListener("dragover", dragover)
        this.element.addEventListener("drop", (e) => {

            let prop = ui_prop.dragee;
            let parent = prop.parent;
            let value = prop.value;
            let type = prop.type;

            if (parent === this)
                return;

            this.addProp(type, value);
            parent.removeProp(type)

            //move the dragee's data into this propset
        })

        this.element.appendChild(this.selector_space);
        this.element.appendChild(this.rule_space);
    }

    initializer(stylerule, parent){
        this.stylerule = stylerule;
        this.stylerule.addObserver(this);
        this.parent = parent;
        this.build(stylerule);
        this.mount(this.parent.element);
    }

    destructor(){
        cached_factory.collect(this);
    }

    destroy() {
        this.stylerule.removeObserver(this);
        this.stylerule = null;
        this.parent = null;

        if (this.element.parentElement)
            this.element.parentElement.removeChild(this.element);

        for (const prop of this.props)
            prop.destroy();

        for (const selector of this.selectors)
            selector.destroy();

        this.props.lenth = 0;
        this.selectors.lenth = 0;
    }

    addSelector(selector) {

        const ui_selector = new UI_selector(selector);

        this.selectors.push(ui_selector);

        ui_selector.mount(this);
    }

    mount(element) {
        if (element instanceof HTMLElement)
            element.appendChild(this.element);
    }

    unmount() {
        if (this.element.parentElement)
            this.element.parentElement.removeChild(this.element);
    }

    build(stylerule = this.stylerule) {

        this.stylerule = stylerule;
        this.ver = this.stylerule.ver;

        const 
            properties = stylerule.properties,
            present_rules = new WeakSet();
        
        let i = -1;

        //Update own properties
        for (let j = 0; j < this.props.length; j++) {
            const ui_prop = this.props[j];

            if (properties.has(ui_prop.name)) {
                const prop = properties.get(ui_prop.name);
                ui_prop.updatedCSSStyleProperty(prop);
                present_rules.add(prop);
            } else 
                this.props.splice(j--, 1)[0].destroy();
        }

        for (const prop of properties.values()) 
            if (!present_rules.has(prop)) {

                const own_prop = new ui_prop(prop, this);
                //Reuse Existing Rule Bodies
                this.props.push(own_prop);
                own_prop.mount(this.rule_space)
            }
    }

    rebuild(stylerule) {
        if (this.ver !== stylerule.ver) {
            this.build(stylerule);
            this.ver = stylerule.ver;
        }
    }

    updatedCSSStyleRule(stylerule) {
        if (!this.GUARD_UPDATE_LOOP)
            for (const prop of this.props)
                prop.updatedCSSStyleProperty();
    }

    update(type, value) {

        if (type && value) {
            this.GUARD_UPDATE_LOOP = true;
            this.stylerule.addProp(`${type}:${value}`);
            this.stylerule.update();
            this.GUARD_UPDATE_LOOP = false;
        }

        this.parent.update(this);
    }

    addProp(type, value) {
        this.update(type, value);
        //Increment the version of the stylerule
        this.stylerule.ver++;

        this.rebuild(this.stylerule);
    }

    removeProp(type) {
        const rule = this.stylerule;
        
        if (rule.props[type]) {

            rule.properties.delete(type);

            //Increment the version of the stylerule
            this.stylerule.ver++;

            this.parent.update();
            
            this.rebuild(this.stylerule);
        }
    }
}

function dragover(e) {
    e.preventDefault();
}

export default cached_factory(ui_stylerule);
