import * as terms from "../properties/terms.mjs";
import { Segment } from "./ui_segment.mjs"

export class ValueTerm extends terms.ValueTerm {

    default (seg, list) {
        let sub = new Segment();
        let element = this.value.valueHandler();
        element.addEventListener("change", e => {
            let value = element.value;
            sub.css_val = value;
            sub.update();
        })
        sub.setValueHandler(element);
        sub.prod = list;
        seg.addSub(sub);
    }

    list(ele, slot) {
        let element = document.createElement("div")
        element.classList.add("css_ui_selection");
        element.innerHTML = this.value.name;
        ele.appendChild(element)

        element.addEventListener("click", e => {
            slot.innerHTML = this.value;
            if (slot) {
                let element = this.value.valueHandler();
                element.addEventListener("change", e => {
                    let value = element.value;
                    slot.css_val = value;
                    slot.update();
                })
                slot.setValueHandler(element);
            } else {
                let sub = new Segment();
                sub.setValueHandler(this.value)
                seg.addSub(sub);
            }
        })
    }

    setSegment(segment) {
        segment.element.innerHTML = this.value.name;
    }

    parseInput(l, seg, list) {
        let val = this.value.parse(l)

        if (val) {
            let sub = new Segment();
            let element = this.value.valueHandler(val);
            element.addEventListener("change", e => {
                let value = element.value;
                sub.css_val = value;
                sub.update();
            })
            sub.setValueHandler(element);
            sub.css_val = val + "";
            sub.prod = list;
            seg.addSub(sub);
        }

        return val;
    }
}

export class LiteralTerm extends terms.LiteralTerm {

	default (seg, list) {
        let sub = new Segment();
        let element = document.createElement("div")
        element.innerHTML = this.value;
        element.addEventListener("change", e => {
            sub.value = this.value + "";
            sub.css_val = this.value + "";
            sub.update();
        })
        sub.setValueHandler(element);
        sub.value = this.value;
        sub.prod = list;
        seg.addSub(sub);
    }

    list(ele, slot) {

        let element = document.createElement("div")
        element.innerHTML = this.value;
        element.classList.add("css_ui_selection");
        ele.appendChild(element)
        element.addEventListener("click", e => {
            slot.value = this.value + "";
            slot.css_val = this.value + "";
            slot.update();
        })
    }

    parseInput(l, seg, list) {
        if (typeof(l) == "string")
            l = whind(l);

        if (l.tx == this.value) {
            l.next();
            //let sub = new Segment();
            seg.value = this.value + "";
            seg.css_val = this.value + "";
            seg.prod = list;
            //seg.addSub(sub);
            return true;
        }

        return false;
    }
}

export class SymbolTerm extends LiteralTerm {
    list() {}

    parseInput(l, seg, r) {
        if (typeof(l) == "string")
            l = whind(l);

        if (l.tx == this.value) {
            l.next();
            let sub = new Segment();
            sub.value = this.value + "";
            seg.addSub(sub);
            return true;
        }

        return false;
    }
}
