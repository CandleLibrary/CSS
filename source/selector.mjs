import whind from "@candlefw/whind";

import { property_definitions } from "./properties/property_and_type_definitions";

/**
 * Used to _bind_ a rule to a CSS selector.
 * @param      {string}  selector        The raw selector string value
 * @param      {array}  selector_array  An array of selector group identifiers.
 * @memberof module:wick~internals.css
 * @alias CSSSelector
 */
export class CSSSelector {

    constructor(value = "", value_array = []) {

        /**
         * The raw selector string value
         * @package
         */
        this.v = value;

        /**
         * Array of separated selector strings in reverse order.
         * @package
         */
        this.a = value_array;

        // CSS Rulesets the selector is member of .
        this.r = null;

        // CSS root the selector is a child of. 
        this.root = null;
    }

    get id() {
        return this.v.join("");
    }
    /**
     * Returns a string representation of the object.
     * @return     {string}  String representation of the object.
     */
    toString(off = 0) {
        let offset = ("    ").repeat(off);

        let str = `${offset}${this.v.join(", ")} {\n`;

        if (this.r)
            str += this.r.toString(off + 1);

        return str + `${offset}}\n`;
    }

    addProp(string) {
        let root = this.r.root;
        if (root) {
            let lex = whind(string);
            while (!lex.END)
                root.parseProperty(lex, this.r, property_definitions);
        }
    }

    removeRule(){
        if(this.r)
            this.r.decrementRef();

        this.r = null;
    }

    addRule(rule = null){
        
        this.removeRule();

        if(rule !== null)
            rule.incrementRef()

        this.r = rule;
    }

}
