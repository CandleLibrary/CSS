import * as prod from "../properties/productions.js";
import JUX from "./jux.js"

export default class ONE_OF extends JUX {

    default (segment, EXTENDED = false) {
        const seg = segment.getSub();

        this.terms[0].default(seg);

        segment.addSub(seg);

        seg.setExtraAsValueList();
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

    parseInput(lx, master_segment, lister = this, start = this.start, end = this.end) {
    	let BOOL = false;

        for (let j = 0; j < end && !lx.END; j++) {

            const 
                copy = lx.copy(),
                temp_r = [];
            
            let bool = false;

            //Each of these segments will serve as a repeater entry
            const segment = master_segment.getSub();
            segment.setExtraAsValueList(this);
            segment.prod = "one_of"

            for (let i = 0, l = this.terms.length; i < l; i++) {

                if (this.terms[i].parseInput(copy, segment)) {
           			master_segment.addSub(segment);
                    bool = true;
                    break;
                }
            }

            if (!bool){
                segment.destroy();
                break;
            }

            lx.sync(copy)
            
            BOOL = true;

            if (!this.checkForComma(lx))
                break;
        }

        if(BOOL)
        	master_segment
                .sync()
                .setExtraAsRepeatExtender(this)
                ;

        return BOOL;
    }
}

Object.assign(ONE_OF.prototype, prod.ONE_OF.prototype)
