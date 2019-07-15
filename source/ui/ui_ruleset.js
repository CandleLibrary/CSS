import cached_factory from "@candlefw/cached_factory"

import UISelector from "./ui_selectors.js";
import UIProp from "./ui_properties.js";

class ui_stylerule {

    constructor(stylerule, parent) {
        this.parent = parent;
        this.props = null;
        this.selectors = null;
        this.element = null;
        this.selector_space = null;
        this.rule_space = null;
        this.ver = null;
    }

    initializer(stylerule, parent) {
        this.ver = stylerule;
        this.parent = parent;

        this.props = [];
        this.selectors = [];
        
        this.element = document.createElement("div");
        this.element.classList.add("rule")
        this.selector_space = document.createElement("div");
        this.selector_space.classList.add("rule_selectors")
        this.rule_space = document.createElement("div");
        this.rule_space.classList.add("stylerule")

        this.element.appendChild(this.selector_space);
        this.element.appendChild(this.rule_space);
        
        this.element.addEventListener("dragover", dragover)
        this.element.addEventListener("drop", (e) => {

            let prop = UIProp.dragee;
            let parent = prop.parent;
            let value = prop.value;
            let type = prop.type;

            if (parent === this)
                return;

            this.addProp(type, value);
            parent.removeProp(type)

            //move the dragee's data into this propset
        })
        
        this.build(stylerule);
        this.mount(this.parent.element);
        this.ver.addObserver(this);
    }

    destructor() {
        this.unmount();
        
        for(const prop of this.props)
            prop.destroy();

        for(const selector of this.selectors)
            selector.destroy();

        this.ver.removeObserver(this);

        this.element = null;
        this.selector_space = null;
        this.rule_space = null;
        this.parent = parent;
        this.props = null;
        this.selectors = null;
        this.ver = null;
    }

    destroy() {
        cached_factory.collect(this);
    }

    updateSelectors(obj) {
        if (obj.parts.length < 1) {
            //remove selector from the rule set.
        }
    }

    addSelector(selector) {

        const ui_selector = new UISelector(selector);

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

        let i = -1;

        for (const prop of stylerule.properties.values()) {
            let own_prop;

            //Reuse Existing Rule Bodies
            if (++i < this.props.length) {
                own_prop = this.props[i];
            } else {
                own_prop = new UIProp(prop.name, this);
                this.props.push(own_prop);
            }
            own_prop.build(prop.name, prop.value_string);
            own_prop.mount(this.rule_space)
        }

        for (const selector of stylerule.selectors) {
            this.addSelector(selector);
        }
    }

    rebuild(stylerule) {
        if (true || this.ver !== stylerule.ver) {
            this.rule_space.innerHTML = "";
            this.props.length = 0;
            this.build(stylerule);
            this.ver = this.stylerule.ver;
        }
    }

    update(type, value) {

        if (type && value) {
            this.stylerule.addProp(`${type}:${value}`);
            this.stylerule.update();
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
            //delete rule.props[type];


            //Increment the version of the stylerule
            this.stylerule.ver++;

            this.parent.update();
            this.rebuild(this.stylerule);
        }
    }

    generateHash() {}
}

function dragover(e) {
    e.preventDefault();
}

export default cached_factory(ui_stylerule);
