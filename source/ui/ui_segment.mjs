import whind from "@candlefw/whind";
import cached_factory from "@candlefw/cached_factory"

//A Class for a CSS UI element that represents, using HTML Elements, a value, or value potential, of a CSS property. 
class SegmentDefault {

    constructor(parent, production) {   
        this.css_val = "";
        
        this.production = null;
        this.parent = null;

        this.value_list = null;
        this.subs = null;
        this.old_subs = null;
        this.sib = null;
        this.HAS_VALUE = false;
        this.DEMOTED = false;
        this.sub_count = 0;

        this.val = document.createElement("span");
        this.val.classList.add("prop_value");

        this.list = document.createElement("div");
        this.list.classList.add("prop_list");
        this.list.style.display = "none"

        this.ext = document.createElement("button");
        this.ext.classList.add("prop_extender");
        this.ext.style.display = "none";
        this.ext.setAttribute("action","ext")

        this.menu_icon = document.createElement("span");
        this.menu_icon.classList.add("prop_list_icon");
        //this.menu_icon.innerHTML = "+"
        this.menu_icon.style.display = "none";
        this.menu_icon.setAttribute("superset", false)
        this.menu_icon.appendChild(this.list);

        this.element = document.createElement("span");
        this.element.classList.add("prop_segment");

        this.element.appendChild(this.menu_icon);
        this.element.appendChild(this.val);
        this.element.appendChild(this.ext);
    }

    initializer(parent, production){
        this.production = parent;
        this.parent = production;
        this.value_list = [];
        this.subs = [];
        this.old_subs = [];
        this.sib = null;
        this.HAS_VALUE = false;
        this.DEMOTED = false;

        this.sub_count = 0;
    }

    destructor(){
        this.parent = null;

        if(this.element.parentElement)
            this.element.parentElement.removeChild(this.element);

        this.val.innerHTML = "";

        this.menu_icon = null;
        this.subs.forEach(e => e.destroy())
        this.subs = null;
    }

    destroy() {
        cached_factory.collect(this);
    }

    reset() {
        this.sub_count = 0;
    }

    finalize(){
        //Remove sub-segments that are no longer used.
        for(let i = this.sub_count; i < this.subs.length; i++)
            this.subs[i].destroy();

        this.subs.length = this.sub_count;
    }

    clearSegments(){
        if(this.subs.length > 0){
            this.val.innerHTML = "";
            for(let i = 0; i < this.subs.length; i++){
                let sub = this.subs[i];
                sub.destroy();
            }   
            this.subs.length = 0;
        }
    }

    replaceSub(old_sub, new_sub) {
        for (let i = 0; i < this.subs.length; i++) {
            if (this.subs[i] == old_sub) {
                this.sub[i] = new_sub;
                this.val.replaceChild(old_sub.element, new_sub.element);
                return;
            }
        }
    }

    mount(element) {
        element.appendChild(this.element);
    }
    setList() {
        //if(this.DEMOTED) debugger
        if (this.prod && this.list.innerHTML == "") {
            if (this.DEMOTED || !this.prod.buildList(this.list, this))
                this.menu_icon.style.display = "none";
            else
                this.menu_icon.style.display = "inline-block";
        }
    }
    change(e) {
        if (this.changeEvent)
            this.changeEvent(this.setElement, this, e);
    }
    promote() {

    }

    getSub(production = this.production, REPEATING = -1){

        var sub, index = this.sub_count;

        if(REPEATING >= 0){
            // the sub will be added after the current sub due as demoting will be called
            // before the sub is added to the current segment
            if(REPEATING == 0 && this.subs.length == 0)
                return this;

            sub = (REPEATING < this.subs.length) 
                ? this.subs[REPEATING] 
                : new Segment(null, production); 

            this.sub_count = REPEATING;
        }else{
           sub = (index < this.subs.length) 
                ? this.subs[index] 
                : new Segment(null, production); 
        }
                
        sub.production = production;
        sub.reset();
        return sub;
    }

    addSub(seg) {
        this.menu_icon.setAttribute("superset", true)
        seg.parent = this;
        seg.id = this.sub_count;

        if(this.subs[this.sub_count++] !== seg){
            this.subs[this.sub_count-1] = seg;
            this.val.appendChild(seg.element)
        }
    }

    removeSub(seg) {
        if (seg.parent == this) {
            for (let i = 0; i < this.subs.length; i++) {
                if (this.subs[i] == seg) {
                    this.val.removeChild(seg.element);
                    seg.parent = null;
                    break;
                }
            }
        }
        return seg;
    }


    demote() {
        const seg = new Segment(this.parent, true);

        seg.reset()
        seg.production = this.production;
        seg.prod = this.prod;
        seg.DEMOTED = true;
        seg.addSub(this);

        seg.setList();

        return seg;
    }

    getRepeat(){
        if(this.parent){
            if(this.parent.subs[this.id+1])
                return this.parent.subs[this.id+1]
        }

        return new Segment();
    }

    addRepeat(seg) {

        let out = this;
        
        if (!this.DEMOTED)
            //Turn self into own sub seg
            out = this.demote();
        
        out.addSub(seg);
        
        seg.setList();

        return out;
    }

    repeat(prod = this.prod) {
        
        if (this.value_count <= this.end && this.prod.end > 1) {
            this.ext.style.display = "inline-block";

            let root_x = 0;
            let width = 0;
            let diff_width = 0;

            const move = (e) => {

                let diff = e.clientX - root_x;
                let min_diff = diff + diff_width;   

                let EXTENDABLE = this.value_count < this.end;
                let RETRACTABLE = this.value_count > 1;

                if(EXTENDABLE && RETRACTABLE)
                    this.ext.setAttribute("action","both")
                else if(EXTENDABLE)
                    this.ext.setAttribute("action","ext")
                else
                    this.ext.setAttribute("action","ret")

                if (diff > 15 && EXTENDABLE) {
                    let bb = this.element

                    if (!this.DEMOTED) {
                        //Turn self into own sub seg
                        this.demote()
                    }

                    if (this.old_subs.length > 1) {
                        this.addSub(this.old_subs.pop());
                    } else {
                        prod.default(this, true);
                    }

                    let w = this.element.clientWidth;
                    diff_width = w - width
                    width = w;
                    root_x += diff_width;

                    return;
                }

                let last_sub = this.subs[this.subs.length - 1];

                if (diff < -5 - last_sub.width && RETRACTABLE) {
                    const sub = this.subs[this.subs.length - 1];
                    this.old_subs.push(sub);
                    this.removeSub(sub);
                    this.subs.length = this.subs.length - 1;

                    let w = this.element.clientWidth;
                    diff_width = w - width
                    width = w;

                    root_x += diff_width;
                }
            }

            const up = (e) => {
                window.removeEventListener("pointermove", move);
                window.removeEventListener("pointerup", up)
            }

            this.ext.onpointerdown = e => {
                width = this.element.clientWidth;
                root_x = e.clientX;
                window.addEventListener("pointermove", move);
                window.addEventListener("pointerup", up)
            }

        } else {
            this.ext.style.display = "none";
        }

        this.setList();
    }

    get width() {
        return this.element.clientWidth;
    }

    update() {
        if (this.parent)
            this.parent.update(this);
        else {
            let val = this.getValue();
        }
    }

    setValueHandler(element, change_event_function) {

        this.val.innerHTML = "";
        this.val.appendChild(element);
        this.value_element = element;

        if (change_event_function) {
            this.setElement = element;
            this.changeEvent = change_event_function;
            this.setElement.onchange = this.change.bind(this);
        }

        this.HAS_VALUE = true;
        //this.menu_icon.style.display = "none";
        this.setList();
    }

    set value(v) {
        this.val.innerHTML = v;
        this.css_val = v;
        this.HAS_VALUE = true;
        this.setList();
    }

    get value_count() {
        if (this.subs.length > 0)
            return this.subs.length
        return (this.HAS_VALUE) ? 1 : 0;
    }


    setValue(value){
        //debugger
        this.production.buildInput(0, whind(value.toString()), this);
    }

    getValue() {
        let val = ""

        if (this.subs && this.subs.length > 0)
            for (let i = 0; i < this.subs.length; i++)
                val += " " + this.subs[i].getValue();
        else
            val = this.css_val;
        return val;
    }

    toString() {
        return this.getValue();
    }
}


export const Segment = cached_factory(SegmentDefault);
