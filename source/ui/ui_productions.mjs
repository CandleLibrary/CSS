import whind from "@candlefw/whind";
import * as prod from "../properties/productions.mjs";
import { LiteralTerm, ValueTerm, SymbolTerm } from "../properties/terms.mjs";


class literalHolder {

    constructor(values) {
        this.values = values;
    }

    parse(lex) {
        let v = lex.tx;

        for (let i = 0; i < this.values.length; i++) {
            if (this.values[i].includes(v)) {
                return v;
            }
        }

        return "";
    }

    setInput(input, value) {
        input.type = "";
        input.value = value;
    }
}

function input(e) {
    let v = e.target.value;
    let lex = whind(v);
    let dispatch = this.dispatch;

    if (v === "") {
        e.target.type = ""
        return;
    }

    e.target.style.color = "red";

    let val = null;

    for (let i = 0; i < dispatch.length; i++) {
        if ((val = dispatch[i].parse(lex.copy()))) {
            dispatch[i].setInput(e.target, val)
            e.target.style.color = "black";
        };
    }
}

function button(e){
    const repeat = e.target.repeat;
    e.target.style.display = "none";
    e.target.parentElement.appendChild(this.buildInput(repeat + 1))
}
/**
 * wick internals.
 * @class      NR (name)
 */
class NR extends prod.NR {
    seal() {
        //Create Element
        let literals = [];
        this.dispatch = [];
        const dispatch = this.dispatch;

        for (let i = 0; i < this.terms.length; i++) {

            let term = this.terms[i];
            if (term instanceof LiteralTerm)
                literals.push(term.value);
            else
                dispatch.push(term.value);

        }

        this.input = input.bind(this);
        this.button = button.bind(this);

        if (literals.length > 0)
            dispatch.push(new literalHolder(literals));
    }

    parseInput(lx, ele, out_val) {
        if (typeof(lx) == "string")
            lx = whind(lx);

        let r = out_val || { v: null },
            start = isNaN(this.r[0]) ? 1 : this.r[0],
            end = isNaN(this.r[1]) ? 1 : this.r[1];

        let result = this.pi(lx, ele, out_val, r, start, end);
    }

    pi(lx, ele, out_val, r, start, end) {
        let element_selector = document.createElement("input");
        element_selector.addEventListener("input", this.input);
        ele.appendChild(element_selector);

        let bool = true;

        for (let j = 0; j < end && !lx.END; j++) {

            for (let i = 0, l = this.terms.length; i < l; i++) {
                bool = this.terms[i].parse(lx.copy(), rule, r);
                this.terms[i].setInput(e.target, bool)
                if (bool) break;
            }

            if (!bool) {

                this.sp(r.v, rule);

                if (j < start)
                    return false;
                else
                    return true;
            }
        }

        this.sp(r.v, rule);

        return true;
    }

    buildInput(repeat = 1, lex) {

        let element = document.createElement("div");
        this.parseInput(lex, element);

        if(lex){
            var value = lex.tx;
            var g = {props:{}}
            var = this.parseInput(lex, g)
        }

        //Build Element
        let element_selector = document.createElement("input");
        element.appendChild(element_selector);
        element_selector.addEventListener("input", this.input);

        element_selector.value = value;
        
        //this.input({target:element_selector})

        if (this.r[1] > 1 && this.r[1] > repeat) {
            let button = document.createElement("button");
            button.repeat = repeat;
            button.innerHTML = "+";
            element.appendChild(button);
            button.addEventListener("click", this.button)
        }

        return element;
    }
}

class AND extends NR {
    pi(lx, rule, r, start, end) {

        outer:
            for (let j = 0; j < end && !lx.END; j++) {
                for (let i = 0, l = this.terms.length; i < l; i++)
                    if (!this.terms[i].parse(lx, rule, r)) return false;
            }

        this.sp(r.v, rule);

        return true;
    }
}
Object.assign(AND.prototype, prod.AND.prototype);

class OR extends NR {
    pi(lx, rule, r, start, end) {
        let bool = false;

        for (let j = 0; j < end && !lx.END; j++) {
            bool = false;

            for (let i = 0, l = this.terms.length; i < l; i++)
                if (this.terms[i].parse(lx, rule, r)) bool = true;

            if (!bool && j < start) {
                this.sp(r.v, rule);
                return false;
            }
        }

        this.sp(r.v, rule);

        return true;
    }
}
Object.assign(OR.prototype, prod.OR.prototype)

class ONE_OF extends NR {
    pi(lx, ele, r, start, end) {
        let bool = false;

        for (let j = 0; j < end && !lx.END; j++) {
            bool = false;

            for (let i = 0, l = this.terms.length; i < l; i++) {
                bool = this.terms[i].parse(lx, rule, r);
                if (bool) break;
            }

            if (!bool)
                if (j < start) {
                    this.sp(r.v, rule);
                    return false;
                }
        }

        this.sp(r.v, rule);

        //create alternatives
        let button = document.createElement("button");
        button.innerHTML = "ONE_OF";
        ele.push(button);

        return bool;
    }
}
Object.assign(ONE_OF.prototype, prod.ONE_OF.prototype)

export { NR, AND, OR, ONE_OF };
