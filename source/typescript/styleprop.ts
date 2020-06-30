import parseDeclaration from "./properties/parse_declaration.js";
import observer from "@candlefw/observer";

export default class styleprop {

    constructor(name, original_value, val) {
        cfw.harness.inspect(val);
        this.val = val;
        this.name = name.replace(/\-/g, "_");
        this.original_value = original_value;
        this.rule = null;
        this.ver = 0;
    }
    destroy() {
        this.val = null;
        this.name = "";
        this.original_value = "";
        this.rule = null;
        observer.destroy(this);
    }

    get css_type() {
        return "styleprop";
    }

    updated() {
        this.updateObservers();

        if (this.parent)
            this.parent.update();
    }

    get value() {
        return this.val.length > 1 ? this.val : this.val[0];
    }

    get value_string() {
        return this.val.join(" ");
    }

    toString(offset = 0) {
        const
            str = [],
            off = ("    ").repeat(offset);

        return `${off + this.name.replace(/\_/g, "-")}:${this.value_string}`;
    }

    setValueFromString(value) {
        const result = parseDeclaration([this.name, null, value]);

        if (result)
            this.setValue(...result.prop);
    }

    setValue(...values) {

        let i = 0;

        for (const value of values) {
            const own_val = this.val[i];


            if (own_val && value instanceof own_val.constructor)
                this.val[i] = value;
            else
                this.val[i] = value;
            i++;
        }

        this.val.length = values.length;

        this.ver++;

        this.updated();
    }
}

observer("updatedCSSStyleProperty", styleprop.prototype);
