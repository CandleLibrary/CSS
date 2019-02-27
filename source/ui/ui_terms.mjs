import * as terms from "../properties/terms.mjs";
import { Segment } from "./ui_segment.mjs"

export class ValueTerm extends terms.ValueTerm {

    default (seg, APPEND = false, value = null) {

        let element = this.value.valueHandler(value);

        if(!APPEND){  
            if(value)
                seg.css_val = value + "";
                seg.setValueHandler(element, (ele, seg, event)=>{
                seg.css_val = element.value;
                seg.update();
            });
        }else{
            let sub = new Segment();
            
            if(value)
                sub.css_val = value + "";
            
            sub.setValueHandler(element, (ele, seg, event)=>{
                seg.css_val = element.value;
                seg.update();
            });
            //sub.prod = list;
            seg.addSub(sub);
        }
    }

    buildInput(rep = 1, value){
        let seg = new Segment();
        this.default(seg, false, value);
        return seg;
    }

    parseInput(l, seg, APPEND = false) {
        let val = this.value.parse(l)

        if (val) {
            this.default(seg, APPEND, val)
            return true;
        }

        return val;
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

        return 1;
    }

    setSegment(segment) {
        segment.element.innerHTML = this.value.name;
    }
}

export class LiteralTerm extends terms.LiteralTerm {

	default (seg, APPEND = false) {
        if(!APPEND){
            seg.value = this.value;
        }else{
            let sub = new Segment();
            sub.value = this.value;
            seg.addSub(sub);
        }
    }

    list(ele, slot) {
        let element = document.createElement("div")
        element.innerHTML = this.value;
        element.classList.add("css_ui_selection");
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
            return true;
        }

        return false;
    }
}

export class SymbolTerm extends LiteralTerm {
    list() {return 0}

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
