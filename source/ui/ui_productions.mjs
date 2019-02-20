import whind from "@candlefw/whind";
import * as prod from "../properties/productions.mjs";
import { Segment } from "./ui_segment.mjs"
import { ValueTerm, LiteralTerm, SymbolTerm } from "./ui_terms.mjs";

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

    seal() {}

    parseInput(lx, segment, list) {

        if (typeof(lx) == "string")
            lx = whind(lx);

        return this.pi(lx, segment, list);
    }

    extend(segment) {

        this.default(segment);
    }

    default (segment) {
        for (let i = 0; i < this.terms.length; i++) {
            this.terms[i].default(segment, null);
        }
    }

    pi(lx, ele, lister = this, start = this.start, end = this.end) {
        //List
        let segment = null;
        if (false &&ele) {
            segment = ele;
        } else {
            segment = new Segment()
            segment.start = start;
            segment.end = end;
            lister = this;
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

        if(bool){
            segment.repeat();
            if(ele)
                ele.addSub(segment);
            this.last_segment = segment;    
        }


        return (!bool && start === 0) ? true : bool;
    }

    buildInput(repeat = 1, lex) {
        let seg = new Segment;
        seg.start = this.start;
        seg.end = this.end;
        seg.prod = this;
        this.parseInput(lex, seg, this);
        return this.last_segment;
    }

    get start(){
        return isNaN(this.r[0]) ? 1 : this.r[0];
    }

    get end(){
        return isNaN(this.r[1]) ? 1 : this.r[1];
    }
}

class AND extends NR {

    default(segment, list = this) {
        for (let i = 0, l = this.terms.length; i < l; i++) {
            this.terms[i].default(segment)
        }
    }

    list(ele, slot) {

        let name = (this.name) ? this.name.replace("\_\g", " "): this.terms.reduce((r, t) => r += " | " + t.name, "")
        let element = document.createElement("div")
        element.classList.add("css_ui_selection");
        element.innerHTML = name;
        ele.appendChild(element)

        element.addEventListener("click", e => {
            slot.innerHTML = this.value;
            if (slot) {
                slot.reset();
                this.default(slot);
                slot.update();
            } else {
                let sub = new Segment();
                sub.setValueHandler(this.value)
                seg.addSub(sub);
            }
        })
    }

    pi(lx, ele, lister = this, start = 1, end = 1) {

        outer: for (let j = 0; j < end && !lx.END; j++) {
            for (let i = 0, l = this.terms.length; i < l; i++)
                if (!this.terms[i].parseInput(lx, ele, r)) return (start === 0) ? true : false
        }

        segment.repeat();

        return true;
    }
}
Object.assign(AND.prototype, prod.AND.prototype);

class OR extends NR {

    list(ele, slot) {

        let name = this.terms.reduce((r, t) => r += " | " + t.name, "")
        let element = document.createElement("div")
        element.classList.add("css_ui_selection");
        element.innerHTML = name;
        ele.appendChild(element)

        element.addEventListener("click", e => {
            slot.innerHTML = this.value;
            if (slot) {
                slot.reset();
                this.default(slot);
                slot.update();
            } else {
                let sub = new Segment();
                sub.setValueHandler(this.value)
                seg.addSub(sub);
            }
        })
    }

    default(segment, list = this) {
        for (let i = 0, l = this.terms.length; i < l; i++) {
            this.terms[i].default(segment, list)
        }
    }

    pi(lx, ele, lister = this, start = this.start, end = this.end){

        let segment = null;

        if (false &&ele) {
            segment = ele;
        } else {
            segment = new Segment();
            segment.start = start;
            segment.end = end;
            lister = this;
        }

        let bool = false;

        let j = 0;

        for (let j = 0; j < end && !lx.END; j++) {
            bool = false;

            for (let i = 0, l = this.terms.length; i < l; i++) {
                if (this.terms[i].parseInput(lx, segment)) {
                    bool = true;
                } else {
                    //Make blank segment that can be filled. 
                }
            }

            if (!bool && j < start){
                bool = false;
            }else if(start === 0)
                bool = true;
        }

        if(bool){
            segment.repeat();
            if(ele)
                ele.addSub(segment);
            this.last_segment = segment;    
        }


        return (!bool && start === 0) ? true : bool;
    }
}
Object.assign(OR.prototype, prod.OR.prototype)

class ONE_OF extends NR {

    list(ele, slot) {

        let name = (this.name) ? this.name.replace(/_/g, " "): this.terms.reduce((r, t) => r += " | " + t.name, "")
        let element = document.createElement("div")
        element.classList.add("css_ui_selection");
        element.innerHTML = name;
        ele.appendChild(element)

        element.addEventListener("click", e => {
            slot.innerHTML = this.value;
            if (slot) {
                slot.reset();
                this.default(slot);
                slot.update();
            } else {
                let sub = new Segment();
                sub.setValueHandler(this.value)
                seg.addSub(sub);
            }
        })
    }

    default (segment, list) {
        this.terms[0].default(segment, this);
    }

    pi(lx, ele, lister = this, start = this.start, end = this.end) {

        //List
        let segment = null;

        if (false &&ele) {
            segment = ele;
        } else {
            segment = new Segment()
            segment.start = start;
            segment.end = end;
            segment.prod = this;
            lister = this;
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

        
        if(bool){
            segment.repeat();
            if(ele)
                ele.addSub(segment);
            this.last_segment = segment;    
        }


        return (!bool && start === 0) ? true : bool;
    }
}

Object.assign(ONE_OF.prototype, prod.ONE_OF.prototype)

export { NR, AND, OR, ONE_OF, LiteralTerm, ValueTerm, SymbolTerm };
