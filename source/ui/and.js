import * as prod from "../properties/productions.js";
import JUX from "./jux.js"

export default class AND extends JUX {

    default (segment, EXTENDED = false) {

        const sub_segment = segment.getSub(this);

        for (let i = 0, l = this.terms.length; i < l; i++)
            this.terms[i].default(sub_segment, i > 1);

        segment.addSub(sub_segment)
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

    parseInput(lx, segment, lister = this, start = 1, end = 1) {

        const
            PROTO = new Array(this.terms.length),
            l = this.terms.length;

        let bool = false;

        repeat:
            for (let j = 0; j < end && !lx.END; j++) {

                const
                    HIT = PROTO.fill(0),
                    copy = lx.copy();

                //Each of these segments will serve as a repeater entry
                const segment = master_segment.getSub();
                segment.prod = "and"

                and:
                    while (true) {

                        let FAILED = false;

                        for (let i = 0; i < l; i++) {

                            if (HIT[i] === 2) continue;

                            let term = this.terms[i];

                            const seg = segment.getSub();
                            seg.start = term.start;
		                    seg.end = term.end;
		                    seg.production = term;

                            if (!term.parseInput(copy, seg)) {
                                if (term.OPTIONAL) {
                                    HIT[i] = 1;
                                } else {
                                    segment.addSub(seg);
                                    HIT[i] = 2;
                                    continue and;
                                }
                            }
                        }

                        if (HIT.reduce((a, v) => a * v, 1) === 0) {
                            segment.destroy();
                            break repeat;
                        }

                        break
                    }

                master_segment.addSub(segment.sync());

                lx.sync(copy);

                bool = true;

                if (!this.checkForComma(lx))
                    break;
            }

        if (bool)
        	master_segment
                .sync()
                .setExtraAsRepeatExtender(this)
                ;

        return bool;
    }
}
Object.assign(AND.prototype, prod.AND.prototype);
