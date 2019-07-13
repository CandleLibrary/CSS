import * as terms from "../properties/terms.mjs";
import { Segment } from "./ui_segment.mjs"

export class ValueTerm extends terms.ValueTerm {

    default (seg, APPEND = false, value = null) {
        if (!APPEND) {
            if (seg.vh !== this.value) {

                const element = this.value.valueHandler(seg, value, (ele, seg, event) => {
                    seg.css_val = ele.css_value;
                    seg.update();
                })

                if (value)
                    seg.css_val = value.toString();

            } else if (value) {
                this.value.setValue(seg, value)
                seg.css_val = value.toString();
            }


        } else {
            if (seg.vh !== this.value) {
                let sub = seg.getSub(this);

                let element = this.value.valueHandler(value, sub);

                if (value)
                    sub.css_val = value.toString();

                sub.setValueHandler(element, (ele, seg, event) => {
                    seg.css_val = element.css_value;
                    seg.update();
                });
                //sub.prod = list;
                seg.addSub(sub);

                sub.finalize();
            } else {
                this.value.setValue(seg.subs[0].value_element, value);
                seg.css_val = value.toString();
            }
        }

        seg.vh = this.value;
    }

    buildInput(rep = 1, value, segment = new Segment(null, this)) {
        this.default(segment, false, value);
        return segment;
    }

    parseInput(l, seg, APPEND = false) {
        let val = this.value.parse(l)

        if (val) {
            this.default(seg, APPEND, val)
            return {segment:seg, bool:true};
        }

        return {segment:seg, bool:false};
    }

    list(ele, slot) {
        let element = document.createElement("div")
        element.classList.add("option");
        element.innerHTML = this.value.label_name || this.value.name;
        ele.appendChild(element)

        element.addEventListener("click", e => {

            slot.innerHTML = this.value;
            if (slot) {
                let element = this.value.valueHandler(this.value, slot);
                element.addEventListener("change", e => {

                    let value = element.value;
                    slot.css_val = value;
                    slot.update();
                })
                slot.setValueHandler(element);
            } else {
                let sub = seg.getSub();
                sub.setValueHandler(this.value, sub)
                seg.addSub(sub);
            }
        })

        return 1;
    }

    setSegment(segment) {
        segment.element.innerHTML = this.value.name;
    }
}

export class BlankTerm extends terms.LiteralTerm {

    default (seg, APPEND = false) {

        if (!APPEND) {
            seg.value = "  ";
        } else {
            let sub = seg.getSub();
            sub.value = "";
            seg.addSub(sub);
        }
    }

    list(ele, slot) {
        let element = document.createElement("div")
        element.innerHTML = this.value;
        element.classList.add("option");
        //        ele.appendChild(element) 

        return 1;
    }

    parseInput(seg, APPEND = false) {
        this.default(seg, APPEND)
        return {segment:seg, bool:false};
    }
}

export class LiteralTerm extends terms.LiteralTerm {

    default (seg, APPEND = false) {
        if (!APPEND) {
            seg.value = this.value;
        } else {
            let sub = seg.getSub();
            sub.value = this.value;
            seg.addSub(sub);
        }
    }

    list(ele, slot) {
        let element = document.createElement("div")
        element.innerHTML = this.value;
        element.classList.add("option");
        ele.appendChild(element)
        element.addEventListener("click", e => {
            slot.value = this.value + "";
            slot.update();
        })

        return 1;
    }

    parseInput(l, seg, APPEND = false) {
        if (typeof(l) == "string")
            l = whind(l);

        if (l.tx == this.value) {
            l.next();
            this.default(seg, APPEND)
            return {segment:seg, bool:true};
        }

        return {segment:seg, bool:false};
    }
}

export class SymbolTerm extends LiteralTerm {
    list() { return 0 }

    parseInput(l, seg, r) {
        if (typeof(l) == "string")
            l = whind(l);

        if (l.tx == this.value) {
            l.next();
            let sub = seg.getSub();
            sub.value = this.value + "";
            seg.addSub(sub);
            return {segment:seg, bool:true};
        }

        return {segment:seg, bool:false};
    }
}
