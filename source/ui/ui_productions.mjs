import whind from "@candlefw/whind";
import * as prod from "../properties/productions.mjs";
import * as terms from "../properties/terms.mjs";

class ValueTerm extends terms.ValueTerm {
    list(ele) {
        let element = document.createElement("div")
        element.innerHTML = this.value.name;
        ele.appendChild(element)
    }

    parse(l, ele, r) {

    }
}

class LiteralTerm extends terms.LiteralTerm {
    list(ele) {
        let element = document.createElement("div")
        element.innerHTML = this.value;
        ele.appendChild(element)
    }

    parse(l, ele, r) {
        if (l.tx == this.value) {
            l.next();
            let element = document.createElement("div")
            element.innerHTML = this.value;
            ele.appendChild(element);
        }
    }
}

class SymbolTerm extends LiteralTerm {
    list(){}
    parse(l, ele, r) {
        if (typeof(l) == "string")
            l = whind(l);

        if (l.tx == this.value) {
            l.next();
            return true;
        }

        return false;
    }
}

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

function button(e) {
    const repeat = e.target.repeat;
    e.target.style.display = "none";
    e.target.parentElement.appendChild(this.buildInput(repeat + 1))
}
/**
 * wick internals.
 * @class      NR (name)
 */
class NR extends prod.NR {
    //Adds an entry in options list. 
    list(ele) {
        let list = document.createElement("div")

        this.buildList()
    }

    buildList(list_ele) {
        let list = document.createElement("div");
        //Build List
        for (let i = 0, l = this.terms.length; i < l; i++) {
            this.terms[i].list(list);
        }

        return list;
    }

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

        //List
        let list_ele = document.createElement("div")
        ele.appendChild(list_ele);

        let bool = true;

        this.buildList(list_ele);

        for (let j = 0; j < end && !lx.END; j++) {

            for (let i = 0, l = this.terms.length; i < l; i++) {
                bool = this.terms[i].parse(lx.copy(), ele, r);
                if (bool) break;
            }

            if (!bool) {

                // this.sp(r.v, ele);

                if (j < start)
                    return false;
                else
                    return true;
            }
        }

        //this.sp(r.v, ele);

        return true;
    }

    buildInput(repeat = 1, lex) {
        let ele = document.createElement("div");
        this.parseInput(lex, ele)
        return ele;
    }
}

class AND extends NR {
    pi(lx, ele, r, start, end) {


        outer: for (let j = 0; j < end && !lx.END; j++) {
            for (let i = 0, l = this.terms.length; i < l; i++)
                if (!this.terms[i].parse(lx, ele, r)) return false;
        }

        this.sp(r.v, ele);

        return true;
    }
}
Object.assign(AND.prototype, prod.AND.prototype);

class OR extends NR {

    list(ele) {
        debugger
        let element = document.createElement("div")
        element.innerHTML = this.value;
        ele.appendChild(element)
    }

    pi(lx, ele, r, start, end) {
        let bool = false;

        ele.appendChild(this.buildList());

        for (let j = 0; j < end && !lx.END; j++) {
            bool = false;

            for (let i = 0, l = this.terms.length; i < l; i++)
                if (this.terms[i].parse(lx, ele, r)) bool = true;

            if (!bool && j < start) {
                this.sp(r.v, ele);
                return false;
            }
        }

        this.sp(r.v, ele);

        return true;
    }
}
Object.assign(OR.prototype, prod.OR.prototype)

class ONE_OF extends NR {

    pi(lx, ele, r, start, end) {
        //List

        ele.appendChild(this.buildList());

        //Add new
        let bool = false;

        ;
        if (lx) {


            //Parse Input
            for (let j = 0; j < end && !lx.END; j++) {
                bool = false;

                for (let i = 0, l = this.terms.length; i < l; i++) {
                    bool = this.terms[i].parse(lx, ele);
                    if (bool) break;
                }

                if (!bool)
                    if (j < start) {
                        this.sp(r.v, ele);
                        return false;
                    }
            }

        }

        //append extender if end is less than start
        if (start < end) {
            //this.addExtensions(ele);
        }

        return bool;
    }
}
Object.assign(ONE_OF.prototype, prod.ONE_OF.prototype)

export { NR, AND, OR, ONE_OF, LiteralTerm, ValueTerm, SymbolTerm };
