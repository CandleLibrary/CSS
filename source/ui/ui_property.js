import whind from "@candlefw/whind";
import cached_factory from "@candlefw/cached_factory"
import * as ui_productions from "./ui_productions.js";
import {
    property_definitions,
    media_feature_definitions,
    types
} from "../properties/property_and_type_definitions.js";
import { getPropertyParser } from "../properties/parser.js";
const props = Object.assign({}, property_definitions);

var dragee = null;

function dragstart(e) {
    event.dataTransfer.setData('text/plain', null)
    UIProp.dragee = this;
}

class UI_property {
    constructor(prop, parent) {
        // Predefine all members of this object.
        this.name = "";

        this.prop = null;
        this.parent = null;
        this._value = null;

        this.hash = 0;
        this.ver = -1;
    }

    initializer(prop, parent) {
        this.prop = prop;
        this.name = prop.name;
        this.parent = parent;

        this.setupElement(this.name);
        this.prop.addObserver(this);
        this.build();
    }

    destructor() {

        this._value && this._value.destroy();
        this.prop && this.prop.removeObserver(this);

        this.hash = 0;
        this.ver = 0;
        this.name = "";

        this._value = null;
        this.name = null;
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

    mount(element) {
        if (element instanceof HTMLElement)
            element.appendChild(this.element)
    }

    unmount() {
        if (this.element.parentElement)
            this.element.parentElement.removeChild(this.element);
    }

    get value() {
        return this._value.toString();
    }

    update(value) {

        this.UPDATE_LOOP_GAURD = true;

        this.prop.setValueFromString(value.toString());

        this.UPDATE_LOOP_GAURD = false;
    }


    build() {

        if (this.ver == this.prop.ver)
            return;

        if (this._value)
            this._value.destroy();

        const type = this.prop.name;
        const value = this.prop.value_string;

        this.element.innerHTML = "";
        this.element.appendChild(this.label)

        let pp = getPropertyParser(type, undefined, props, ui_productions);
        this._value = pp.buildInput(1, whind(value));

        if (this._value) {
            this._value.parent = this;
            this._value.mount(this.element);
        }

        this.ver = this.prop.ver;
    }
    updatedCSSStyleProperty(prop = this.prop) {

        if (this.ver == prop.ver)
            return;

        if (prop !== this.prop) {
            this.prop = prop;
            return this.build()
        } else {

            // this.ver = prop.ver;

            //if (!this.UPDATE_LOOP_GAURD) {

                const val = this._value.setValue(prop.value_string);

                if (val !== this._value) {
                    this._value = val;
                    this._value.mount(this.element);
                }
            // /}

            this.ver = this.prop.ver;
        }
    }
}

export default cached_factory(UI_property);
