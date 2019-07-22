
import * as prod from "../properties/productions.js";
import JUX from "./jux.js"

export default class OR extends JUX {

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

    parseInput(lx, master_segment, lister = this, start = this.start, end = this.end) {
        const
            PROTO = new Array(this.terms.length),
            l = this.terms.length;

        let
            bool = false,
            NO_HIT = true;

        repeat:
            for (let j = 0; j < end && !lx.END; j++) {

                const HIT = PROTO.fill(0);
                
                let  
                    copy = lx.copy();

                //Each of these segments will serve as a repeater entry
                const segment = master_segment.getSub();
                segment.prod = "or"
                segment.preserve(); //Preserve existing 

                or:
                    while (true) {

                        let FAILED = false;

                        const seg = segment.getSub();
                        seg.start = term.start;
                        seg.end = term.end;
                        seg.production = term;

                        
                        for (let i = 0; i < l; i++) {

                            if (HIT[i] === 2) continue;

                            let term = this.terms[i];

                            if (term.parseInput(copy, seg)) {

                                segment.addSub(seg);
                                
                                NO_HIT = false;
                                
                                HIT[i] = 2;
                                
                                continue or;
                            }
                        }

                        if (NO_HIT) {
                            segment.destroy();
                            break repeat;
                        }

                        break;
                    }
                    
                master_segment.addSub(segment.sync());

                lx.sync(copy);

                bool = true;

                if (!this.checkForComma(lx))
                    break;
            }

        if(bool){
            master_segment
                .sync()
                .setExtraAsRepeatExtender(this);
        }

        return bool;
    }
}

Object.assign(OR.prototype, prod.OR.prototype)
