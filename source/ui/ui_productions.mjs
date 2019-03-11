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


    createSegment() {
        let segment = new Segment()
        segment.start = this.start;
        segment.end = this.end;
        segment.prod = this;
        return segment
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
        let count = 0;
        //Build List
        for (let i = 0, l = this.terms.length; i < l; i++) {
            count += this.terms[i].list(list, slot);
        }

        return count > 1;
    }

    seal() {}

    parseInput(lx, segment, list) {

        if (typeof(lx) == "string")
            lx = whind(lx);

        return this.pi(lx, segment, list);
    }

    default (segment, EXTENDED = true) {
        let seg = this.createSegment();

        segment.addSub(seg);

        for (let i = 0, l = this.terms.length; i < l; i++) {
            this.terms[i].default(seg, l > 1);
        }
        seg.setList();

        if (!EXTENDED) seg.repeat();
    }

    pi(lx, ele, lister = this, start = this.start, end = this.end) {

        //List
        let segment = this.createSegment()

        let bool = true,
            j = 0,
            last_segment = null,
            first;

        for (; j < end && !lx.END; j++) {
            const REPEAT = j > 0

            let seg = (REPEAT) ? new Segment : segment;

            seg.prod = this;

            outer:
            
                for (let i = 0, l = this.terms.length; i < l; i++) {
                    bool = this.terms[i].parseInput(lx, seg, l > 1);

                    if (!bool) {
                        bool = false;
                        //segment = segment.prev;
                        
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

            if (REPEAT)
                segment.addRepeat(seg);
        }

        if (bool) {
            segment.repeat();
            if (ele)
                ele.addSub(segment);
            this.last_segment = segment;
        }


        return (!bool && start === 0) ? true : bool;
    }

    buildInput(repeat = 1, lex) {
        this.last_segment = null;
        let seg = new Segment;
        seg.start = this.start;
        seg.end = this.end;
        seg.prod = this;
        this.parseInput(lex, seg, this);
        return this.last_segment;
    }

    get start() {
        return isNaN(this.r[0]) ? 1 : this.r[0];
    }

    get end() {
        return isNaN(this.r[1]) ? 1 : this.r[1];
    }
}

class AND extends NR {

    default (segment, EXTENDED = false) {
        //let seg = this.createSegment();
        //segment.addSub(seg);
        for (let i = 0, l = this.terms.length; i < l; i++) {
            this.terms[i].default(segment, i > 1);
        }
        //seg.repeat();
    }

    list(ele, slot) {

        let name = (this.name) ? this.name.replace("\_\g", " ") : this.terms.reduce((r, t) => r += " | " + t.name, "")
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

        return 1;
    }

    pi(lx, ele, lister = this, start = 1, end = 1) {

        outer: for (let j = 0; j < end && !lx.END; j++) {
            for (let i = 0, l = this.terms.length; i < l; i++)
                if (!this.terms[i].parseInput(lx, ele)) return (start === 0) ? true : false
        }

        segment.repeat();

        return true;
    }
}
Object.assign(AND.prototype, prod.AND.prototype);

class OR extends NR {

    default (segment, EXTENDED = false) {
        //let seg = this.createSegment();
        //segment.addSub(seg);
        for (let i = 0, l = this.terms.length; i < l; i++) {
            this.terms[i].default(segment, l > 1);
        }
        //seg.repeat();
    }

    buildList(list, slot) {
        return false;
    }

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

        return 1;
    }

    pi(lx, ele, lister = this, start = this.start, end = this.end) {
        
        let segment = ele //this.createSegment()

        let bool = false;

        let j = 0;

        let OVERALL_BOOL = false;

        for (let j = 0; j < end && !lx.END; j++) {
            const REPEAT = j > 0

            let seg = (REPEAT) ? new Segment : segment;


            bool = false;

            this.count = (this.count) ? this.count:this.count = 0;
            
            outer:
            //User "factorial" expression to isolate used results in a continous match. 
            while(true){
                for (let i = 0, l = this.terms.length; i < l; i++) {
                    //if(this.terms[i].count == this.count) continue

                    if (this.terms[i].parseInput(lx, seg, true)) {
                        this.terms[i].count = this.count;
                        OVERALL_BOOL = true;
                        bool = true;
                        continue outer;
                    }
                }
                break;
            }

            {
                //Go through unmatched and make placeholders.
            }

            {
                //Sort everything based on parse 
            }

            if (!bool && j < start) {
                bool = false;
            } else if (start === 0)
                bool = true;
                if (REPEAT)
            segment.addRepeat(seg);
        }

        if (OVERALL_BOOL) {
            segment.repeat();
            //if (ele)
            //    ele.addSub(segment);
            this.last_segment = segment;
        }


        return (!bool && start === 0) ? true : bool;
    }
}

Object.assign(OR.prototype, prod.OR.prototype)

class ONE_OF extends NR {

    default (segment, EXTENDED = false) {
        let seg = this.createSegment();
        this.terms[0].default(seg);
        segment.addSub(seg);
        seg.setList();
        if (!EXTENDED) seg.repeat();
    }

    list(ele, slot) {

        let name = (this.name) ? this.name.replace(/_/g, " ") : this.terms.reduce((r, t) => r += " | " + t.name, "")
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

        return 1;
    }

    pi(lx, ele, lister = this, start = this.start, end = this.end) {
        //List
        let segment = this.createSegment()

        //Add new
        let bool = false;

        let j = 0;

        //Parse Input
        for (; j < end && !lx.END; j++) {
            const REPEAT = j > 0

            let seg = segment;
            
            if(REPEAT){
                seg = new Segment;
                seg.prod = this;
            }

            bool = false;

            for (let i = 0, l = this.terms.length; i < l; i++) {
                bool = this.terms[i].parseInput(lx, seg);
                if (bool) break;
            }

            if (!bool) {
                if (j < start) {
                    bool = false
                    break;
                }
            }
            if (REPEAT)
                segment.addRepeat(seg);

        }

        if (bool) {
            segment.repeat();
            if (ele)
                ele.addSub(segment);
            this.last_segment = segment;
        }

        return /*(!bool && start === 0) ? true :*/ bool;
    }
}

Object.assign(ONE_OF.prototype, prod.ONE_OF.prototype)

export { NR, AND, OR, ONE_OF, LiteralTerm, ValueTerm, SymbolTerm };
