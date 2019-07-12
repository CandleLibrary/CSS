import whind from "@candlefw/whind";
import * as prod from "../properties/productions.mjs";
import { Segment } from "./ui_segment.mjs"
import { ValueTerm, LiteralTerm, SymbolTerm,  BlankTerm } from "./ui_terms.mjs";

/**
 * wick internals.
 * @class      JUX (name)
 */
class JUX extends prod.JUX {
    //Adds an entry in options list. 


    createSegment(segment) {
        segment.reset();
        segment.production = this;
        segment.start = this.start;
        segment.end = this.end;
        segment.prod = this;
        return segment
    }

    insertBlank(seg){
        let blank = new BlankTerm;
        blank.parseInput(seg);
    }

    buildList(list, slot) {

        if (!slot) {
            let element = document.createElement("div")
            element.classList.add("prop_slot")
            slot = element;
        }

        if (!list) {
            list = document.createElement("div");
            list.classList.add("prop_slot")
            slot.appendChild(list);
        }
        let count = 0;
        //Build List
        for (let i = 0, l = this.terms.length; i < l; i++) 
            count += this.terms[i].list(list, slot);

        return count > 1;
    }

    seal() {}

    parseInput(lx, segment, list) {

        if (typeof(lx) == "string")
            lx = whind(lx);

        const out = this.pi(lx, segment, list);

        segment.finalize();

        return out;
    }

    default (segment, EXTENDED = true) {
        
        const sub_segment = this.createSegment(segment.getSub(this));

        segment.addSub(sub_segment);

        for (let i = 0, l = this.terms.length; i < l; i++) 
            this.terms[i].default(sub_segment, l > 1);

        sub_segment.setList();

        if (!EXTENDED) sub_segment.repeat();
    }

    pi(lx, master_segment, lister = this, start = this.start, end = this.end) {
        
        const segment = this.createSegment(master_segment.getSub(this));

        let bool = false,
            j = 0,
            last_segment = null,
            first;

        repeat:
            for (let j = 0; j < end && !lx.END; j++) {
                const REPEAT = j > 0;

                let copy = lx.copy();

                let seg = (REPEAT) ? segment.getSub(this) : segment;

                seg.prod = this;

                for (let i = 0, l = this.terms.length; i < l; i++) {

                    let term = this.terms[i];

                    if (!term.parseInput(copy, seg, l > 1)) {
                        if (!term.OPTIONAL) {
                            break repeat;
                        }
                    }
                }

                lx.sync(copy);

                bool = true;

                if (!this.checkForComma(lx))
                    break;

                if (REPEAT)
                    segment.addRepeat(seg);
            }

            this.capParse(segment, master_segment, bool)
            
            return bool;
    }

    capParse(sub_segment, master_segment, bool){
        if (bool) {
            sub_segment.repeat();
            if (master_segment)
                master_segment.addSub(sub_segment);
            this.last_sub_segment = sub_segment;
            sub_segment.finalize();
        }else {
            sub_segment.destroy();
            if(this.OPTIONAL){
                if(master_segment){
                    let sub_segment = this.createSegment(master_segment.getSub(this));
                    let blank = new BlankTerm();
                    blank.parseInput(sub_segment);
                    sub_segment.prod = this;
                    
                    sub_segment.repeat();
                    master_segment.addSub(sub_segment)
                }
            }
        }

    }

    buildInput(repeat = 1, lex, segment = new Segment(null, this)) {
        this.last_segment = segment;
        
        segment.reset();
        segment.start = this.start;
        segment.end = this.end;
        segment.prod = this;
        this.parseInput(lex, segment, this);
        segment.finalize();

        return this.last_segment;
    }

    list(){
        
    }
}

class AND extends JUX {

    default (segment, EXTENDED = false) {

        for (let i = 0, l = this.terms.length; i < l; i++) 
            this.terms[i].default(segment, i > 1);
    }

    list(ele, slot) {

        let name = (this.name) ? this.name.replace("\_\g", " ") : this.terms.reduce((r, t) => r += " | " + t.name, "")
        let element = document.createElement("div")
        element.classList.add("option");
        element.innerHTML = name;
        ele.appendChild(element)

        element.addEventListener("click", e => {
            
            slot.innerHTML = this.value;
            if (slot) {
                slot.clearSegments();
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

    pi(lx, segment, lister = this, start = 1, end = 1) {

        outer: for (let j = 0; j < end && !lx.END; j++) {
            for (let i = 0, l = this.terms.length; i < l; i++)
                if (!this.terms[i].parseInput(lx, segment)) return (start === 0) ? true : false
        }

        segment.repeat();

        return true;
    }
}
Object.assign(AND.prototype, prod.AND.prototype);

class OR extends JUX {

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
        element.classList.add("option");
        element.innerHTML = name;
        ele.appendChild(element)

        element.addEventListener("click", e => {
            
            slot.innerHTML = this.value;
            if (slot) {
                slot.clearSegments();
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

    pi(lx, master_segment, lister = this, start = this.start, end = this.end) {
        
        let bool = false;

        let j = 0;

        let OVERALL_BOOL = false;

        for (let j = 0; j < end && !lx.END; j++) {
            const 
                REPEAT = j > 0,
                sub_segment = (REPEAT) ? master_segment.getSub(this) : master_segment;

            bool = false;

            this.count = (this.count) ? this.count:this.count = 0;
            
            outer:
            //User "factorial" expression to isolate used results in a continous match. 
            while(true){
                for (let i = 0, l = this.terms.length; i < l; i++) {
                    //if(this.terms[i].count == this.count) continue

                    if (this.terms[i].parseInput(lx, sub_segment, true)) {
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
                master_segment.addRepeat(sub_segment);
        }

        if (OVERALL_BOOL) {
            master_segment.repeat();
            master_segment.finalize();
            this.last_segment = master_segment;
        }


        return (!bool && start === 0) ? true : bool;
    }
}

Object.assign(OR.prototype, prod.OR.prototype)

class ONE_OF extends JUX {

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
        element.classList.add("option");
        element.innerHTML = name;
        ele.appendChild(element)

        element.addEventListener("click", e => {
            //debugger
            slot.innerHTML = this.value;
            if (slot) {
                slot.clearSegments();
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

    pi(lx, master_segment, lister = this, start = this.start, end = this.end) {
        //List
        let segment = master_segment.getSub(this);

        //Add new
        let bool = false;

        let j = 0;

        //Parse Input
        for (; j < end && !lx.END; j++) {
            const REPEAT = j > 0

            let seg = segment;
            
            if(REPEAT){
                seg = segment.getSub(this);
                seg.production = this;
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

        this.capParse(segment, master_segment, bool)

        return  bool;
    }
}

Object.assign(ONE_OF.prototype, prod.ONE_OF.prototype)

export { JUX, AND, OR, ONE_OF, LiteralTerm, ValueTerm, SymbolTerm };
