import whind from "@candlefw/whind";
import * as prod from "../properties/productions.mjs";
import { Segment } from "./ui_segment.mjs"
import { ValueTerm, LiteralTerm, SymbolTerm } from "./ui_terms.mjs";

class literalHolder {

    constructor(values) {
        this.values = values;
    }

    parseInput(lex) {
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

/**
 * wick internals.
 * @class      NR (name)
 */
class NR extends prod.NR {
    //Adds an entry in options list. 
    list(ele, slot) {
        this.buildList(ele, slot)
    }

    buildList(list, slot) {

        if (!slot) {
            let element = document.createElement("div")
            element.classList.add("css_ui_slot")
            slot = element;
        }

        if (!list) {
            list = document.createElement("div");
            list.classList.add("css_ui_slot")
            slot.appendChild(list);
        }

        //Build List
        for (let i = 0, l = this.terms.length; i < l; i++) {
            this.terms[i].list(list, slot);
        }

        return slot;
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

        if (literals.length > 0)
            dispatch.push(new literalHolder(literals));
    }

    parseInput(lx, segment, list) {

        if (typeof(lx) == "string")
            lx = whind(lx);

        let start = isNaN(this.r[0]) ? 1 : this.r[0],
            end = isNaN(this.r[1]) ? 1 : this.r[1];

        return this.pi(lx, segment, list, start, end);
    }

    createSegment(lx) {
        let seg = new Segment;
        for (let i = 0; i < this.terms.length; i++) {
            seg.addSub(this.terms[i].createSegment());
        }
        //this.buildList(seg);
        return seg;
    }

    buildDefault() {

    }

    extend(segment) {
        this.default(segment);
    }

    default (segment) {
        for (let i = 0; i < this.terms.length; i++) {
            this.terms[i].default(segment, null);
        }
    }

    pi(lx, ele, lister = this, start = 1, end = 1) {
        //List
        let segment = null;
        if (ele) {
            segment = ele;
        } else {
            segment = new Segment()
            segment.start = start;
            segment.end = end;
            this.addExtensions();
        }

        let bool = true,
            j = 0,
            last_segment = null,
            first;

        for (; j < end && !lx.END; j++) {

            for (let i = 0, l = this.terms.length; i < l; i++) {
                bool = this.terms[i].parseInput(lx, segment, lister);

                if (!bool) {
                    bool = false;
                    //segment = segment.prev;
                    break;
                };
                //We know that this is in the original input, so we'll create an input for this object. 
            }

            if (!bool) {
                if (j < start)
                    bool = false;
                else
                    bool = true;
                break;
            }
        }

        this.addExtensions(segment, j, end);

        return (bool) ? segment : null;
    }

    buildInput(repeat = 1, lex) {
        let seg = this.parseInput(lex);
        return seg;
    }

    addExtensions(segment, start, end) {

        if (start < end)
            segment.repeat(this, start, end);
    }
}

class AND extends NR {
    pi(lx, ele, lister = this, start = 1, end = 1) {


        outer: for (let j = 0; j < end && !lx.END; j++) {
            for (let i = 0, l = this.terms.length; i < l; i++)
                if (!this.terms[i].parseInput(lx, ele, r)) return false;
        }

        this.sp(r.v, ele);

        return true;
    }
}
Object.assign(AND.prototype, prod.AND.prototype);

class OR extends NR {

    pi(lx, ele, lister = this, start = 1, end = 1) {
        let segment = null;
        if (ele) {
            segment = ele;
        } else {
            segment = new Segment()
            segment.start = start;
            segment.end = end;
            this.addExtensions();
        }

        let bool = false;

        let j = 0;

        for (let j = 0; j < end && !lx.END; j++) {
            bool = false;

            for (let i = 0, l = this.terms.length; i < l; i++) {
                if (this.terms[i].parseInput(lx, segment)) {
                    bool = true;
                }else{
                    //Make blank segment that can be filled. 
                }
            }

            if (!bool && j < start)
                bool = false;
        }

        this.addExtensions(segment, j, end);

        return (bool) ? segment : null;
    }
}
Object.assign(OR.prototype, prod.OR.prototype)

class ONE_OF extends NR {

    pi(lx, ele, lister = this, start = 1, end = 1) {
        //List
        let segment = null;

        if (ele) {
            segment = ele;
        } else {
            segment = new Segment()
            segment.start = start;
            segment.end = end;
            this.addExtensions();
        }

        //Add new
        let bool = false;

        let j = 0;
        //Parse Input
        for (; j < end && !lx.END; j++) {
            bool = false;

            for (let i = 0, l = this.terms.length; i < l; i++) {
                bool = this.terms[i].parseInput(lx, segment, lister);
                if (bool) break;
            }

            if (!bool) {
                if (j < start) {
                    bool = false
                    break;
                }
            }
        }

        return (bool) ? segment : null;
    }
}
Object.assign(ONE_OF.prototype, prod.ONE_OF.prototype)

export { NR, AND, OR, ONE_OF, LiteralTerm, ValueTerm, SymbolTerm };
