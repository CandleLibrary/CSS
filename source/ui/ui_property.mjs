import whind from "@candlefw/whind";
import cached_factory from "@candlefw/cached_factory"
import * as ui_productions from "./ui_productions.mjs";
import {
    property_definitions,
    media_feature_definitions,
    types
} from "../properties/property_and_type_definitions.mjs";
import { getPropertyParser } from "../properties/parser.mjs";
const props = Object.assign({}, property_definitions);

var dragee = null;

function dragstart(e) {
    event.dataTransfer.setData('text/plain', null)
    UIProp.dragee = this;
}

class UI_property {
    constructor(prop, parent) {
        // Predefine all members of this object.
        this.type = "";

        this.prop = null;
        this.parent = null;
        this._value = null;

        this.hash = 0;
        this.ver = 0;
    }

    initializer(prop, parent) {
        this.prop = prop;
        this.type = prop.name;
        this.parent = parent;

        this.setupElement(this.type);
        this.prop.addObserver(this);
        this.build();
    }

    destructor() {

        this._value && this._value.destroy();
        this.prop && this.prop.removeObserver(this);

        this.hash = 0;
        this.ver = 0;
        this.type = "";

        this._value = null;
        this.type = null;
        this.parent = null;
        this.unmount();
    }

    destroy() {
        cached_factory.collect(this);
    }

    setupElement(type) {
        this.element = document.createElement("div");
        this.element.setAttribute("draggable", "true")
        this.element.classList.add("prop");
        this.element.addEventListener("dragstart", dragstart.bind(this));
        this.label = document.createElement("span")
        this.label.classList.add("prop_label")
        this.label.innerHTML = `${type.replace(/[\-\_]/g, " ")}`;
    }

    build() {
        const type = this.prop.name;
        const value = this.prop.value_string;

        this.element.innerHTML = ""
        this.element.appendChild(this.label)
        let pp = getPropertyParser(type, undefined, props, ui_productions);
        this._value = pp.buildInput(1, whind(value));

        if (this._value) {
            this._value.parent = this;
            this._value.mount(this.element);
        }
    }

    mount(element) {
        if (element instanceof HTMLElement)
            element.appendChild(this.element)
    }

    unmount() {
        if (this.element.parentElement)
            this.element.parentElement.removeChild(this.element);
    }

    get value() {
        return this._value.toString();p
p
    }

    update(value) {
        this.UPDATE_LOOP_GAURD = true;
        this.prop.setValueFromString(value.toString());
        this.UPDATE_LOOP_GAURD = false;
        //this.parent.update(this.type, );
    }

    updatedCSSStyleProperty(prop = this.prop) {

        if (prop == this.prop && this.ver == prop.ver)
            return;

        // this.ver = prop.ver;

        if (!this.UPDATE_LOOP_GAURD)
            this._value.setValue(prop.value_string);
        this.UPDATE_LOOP_GAURD = false;
    }
}

export default cached_factory(UI_property);
