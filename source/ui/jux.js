import * as prod from "../properties/productions.js";
import whind from "@candlefw/whind";
import { Segment } from "./ui_segment.js"
import { ValueTerm, LiteralTerm, SymbolTerm, BlankTerm } from "./ui_terms.js";

/**
 * wick internals.
 * @class      JUX (name)
 */
export default class JUX extends prod.JUX {
    //Adds an entry in options list. 

    createSegment(segment) {
        segment.reset();
        segment.production = this;
        segment.start = this.start;
        segment.end = this.end;
        segment.prod = this;
        return segment
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

    parseInputHeader(lx, segment, list) {

        if (typeof(lx) == "string")
            lx = whind(lx);

        segment.production = this;
        segment.end = this.end;
        segment.start = this.start;

        let bool = this.parseInput(lx, segment, list);

        //segment.finalize();

        return bool;
    }

    default (segment, EXTENDED = true) {

        const sub_segment = segment.getSub(this);

        segment.addSub(sub_segment);

        for (let i = 0, l = this.terms.length; i < l; i++)
            this.terms[i].default(sub_segment, l > 1);
    }

    parseInput(lx, master_segment, lister = this, start = this.start, end = this.end) {
        /* 
            Each Juxtaposed production value must be present, or replaced with a place 
            holder if its optional and its corresponding value is absent 
        */

        let bool = false;

        repeat:
            for(let j = 0; j < end && !lx.END; j++){
                
                const copy = lx.copy();

                //Each of these segments will serve as a repeater entry
                const segment = master_segment.getSub();
                segment.prod = "jux"

                for(let i = 0, l = this.terms.length; i < l; i++){

                    const term = this.terms[i];

                    const seg = segment.getSub();

                    seg.start = term.start;
                    seg.end = term.end;
                    seg.production = term;


                    if(!term.parseInput(copy, seg)){
                        if(term.OPTIONAL)
                            seg.setExtraAsPlaceHolder(term);
                        else{
                            segment.destroy();
                            break repeat;
                        }
                    }

                    segment.addSub(seg);
                }

                segment.sync();
                master_segment.addSub(segment);

                lx.sync(copy);

                bool = true;

                if (!this.checkForComma(lx))
                    break;

            }

        if(bool)
            master_segment
                .sync()
                .setExtraAsRepeatExtender(this)
                ;
        
        return bool;
    }

    buildInput(repeat = 1, lex, segment = new Segment(null, this)) {
        segment.reset();
        segment.start = this.start;
        segment.end = this.end;
        segment.production = this;
        this.parseInputHeader(lex, segment, this);
        segment.sync();
        return segment;
    }

    list() {

    }
}
