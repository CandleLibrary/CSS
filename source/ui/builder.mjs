import { UIValue } from "./ui_value.mjs";
import {
    property_definitions,
    media_feature_definitions,
    types
} from "../properties/property_and_type_definitions";
import { CSSRule as R, CSSSelector as S } from "../nodes";
import { getPropertyParser } from "../properties/parser";
import whind from "@candlefw/whind";

export default class UIMaster {
    constructor(css) {
        css.addObserver(this);
        this.css = css;
        this.rule_sets = [];
        this.selectors = [];
        this.element = document.createElement("div");
    }

    build(css = this.css) {

        this.css = css;

        let children = css.children;

        this.rule_sets = [];
        this.selectors = [];

        for (let i = 0; i < children.length; i++) {
            let r = new UIRuleSet(children[i], this);
        }
    }

    updatedCSS(css) {
        this.element.innerHTML = "";
        this.build(css);
        this.render();
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
    	this.css.updated();
    }
}

class UIRuleSet {
    constructor(rule_body, parent) {
        this.parent = parent;
        this.hash = 0;
        this.rules = [];
        this.selectors = [];

        this.element = document.createElement("div");

        this.build(rule_body._sel_a_[0].r);
        this.mount(this.parent.element)
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

        for (let a in rule_body.props) {
            let rule = new UIRule(a, rule_body.toString(0, a), this);
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

    generateHash() {}
}

class UIRule {
    constructor(type, value, parent) {
        this.hash = 0;
        this.type = type;
        this.parent = parent;
        this.setupElement();

        this.element.innerHTML = `${type}:`

        this.value = new UIValue(type, value, this);

        this.mount(this.parent.element)
    }

    update(type, value) {
        this.parent.update(type, value);
    }

    mount(element) {
        if (element instanceof HTMLElement)
            element.appendChild(this.element);
    }

    unmount() {
        if (this.element.parentElement)
            this.element.parentElement.removeChild(this.element);
    }

    setupElement() {
        this.element = document.createElement("div");
        this.element.classList.add("cfw_css_ui_rule");
    }

    generateHash() {

    }
}
