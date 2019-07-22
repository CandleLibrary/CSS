import whind from "@candlefw/whind";
import cached_factory from "@candlefw/cached_factory"
let id = 0;
//A Class for a CSS UI element that represents, using HTML Elements, a value, or value potential, of a CSS property. 
class SegmentDefault {

    constructor(parent, production) {   
        this.css_val = "";
        
        this.production = null;
        this.parent = null;
        
        this.subs = [];
        this.old_subs = null;
        this.HAS_VALUE = false;
        this.PROMOTED = false;
        this.out_sub_count = 0;
        this.in_sub_count = 0;
        this.vh = null;

        // Element is one of three things
        // Place Holder with list
        // Value with list
        // Extender

        this.val = document.createElement("span");
        this.element = this.val;
        this.element.setAttribute("id", id++)

        //The extra value is one of three different things...
        // A place holder value
        // A handle to extend the value;
        // A icon to reveal the values menu.
        this.extra = document.createElement("span");
        

        //this.element.appendChild(this.ext);
    }

    initializer(parent, production){
        this.parent = parent;
        this.production = production;
        this.subs = [];
        this.old_subs = [];
        this.HAS_VALUE = false;
        this.PROMOTED = false;
        this.out_sub_count = 0;
        this.in_sub_count = 0;
        this.DESTROYED = false;
    }

    destructor(){
        this.DESTROYED = true;
        this.parent = null;

        if(this.element.parentElement)
            this.element.parentElement.removeChild(this.element);

        this.element.innerHTML = "";
        this.css_val = "";

        this.subs.forEach(e => e.destroy())
        this.subs.length = 0;
        this.sub_count = 0;
        this.vh = null;
    }

    destroy() {
        if(!this.DESTROYED) // Make sure this is idempotent
            cached_factory.collect(this);
    }

    reset() {
        this.in_sub_count = 0;
        this.out_sub_count = 0;
    }

    sync(){
        for(let i = this.in_sub_count; i < this.subs.length; i++)
            this.subs[i].destroy();

        this.out_sub_count = this.in_sub_count;

        this.subs.length = this.in_sub_count;

        return this;
    }

    getSub(production = this.production, REPEATING = -1, USE_UNIQUE = false){

        var sub, index = this.out_sub_count++;

       sub = (index < this.subs.length) 
            ? this.subs[index] 
            : new Segment(null, production); 
        
        if(sub === this) debugger
 
        sub.production = production;
        sub.reset();
        return sub;
    }

    addSub(seg) {
        //this.menu_icon.setAttribute("superset", true)
        seg.parent = this;
       // seg.id = this.in_sub_count;

        if(this.subs[this.in_sub_count++] !== seg){
            if(this.subs[0] == seg)debugger;

            if(this.subs[this.in_sub_count-1])
                this.subs[this.in_sub_count-1].destroy();

            this.subs[this.in_sub_count-1] = seg;
            
            this.element.appendChild(seg.element);
        }
    }
    
    mount(element) {
        element.appendChild(this.element);
    }

    change(e) {
        if (this.changeEvent)
            this.changeEvent(this.setElement, this, e);
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

    set prod(prod){
        this.element.setAttribute("production", prod)
    }

    set value(v) {
        //this.element.innerHTML = v;
        this.css_val = v;
        this.HAS_VALUE = !!v;
        //this.setList();
    }

    get value_count() {
        if (this.subs.length > 0)
            return this.subs.length
        return (this.HAS_VALUE) ? 1 : 0;
    }

    setValue(value){
        return this.production.buildInput(0, whind(value.toString()), this);
    }

    getValue() {
        let val = this.css_val;

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

    setValueHandler(element, change_event_function) {

        this.element.innerHTML = "";
        this.element.appendChild(element);
        this.value_element = element;

        if (change_event_function) {
            this.setElement = element;
            this.changeEvent = change_event_function;
            this.setElement.onchange = this.change.bind(this);
        }

        this.HAS_VALUE = true;
    }

    setExtraAsValueList(production = this.production) {
        this.production = production;


        
        if (this.production) {

            this.element.appendChild(this.extra)   
            this.extra.setAttribute("class", "list")
            /*
            if (this.PROMOTED || !this.production.buildList(this.list, this))
                this.menu_icon.style.display = "none";
            else
                this.menu_icon.style.display = "inline-block";
            */
        }

    }

    setExtraAsPlaceHolder(){
        this.element.appendChild(this.extra)    
        this.extra.setAttribute("class", "placeholder")
        this.sync();
        return this;
    }

    setExtraAsRepeatExtender(prod = this.production) {
        
        if (this.value_count <= this.end && this.production.end > 1) {

            this.element.appendChild(this.extra)    
            this.extra.setAttribute("class", "repeating")

            let root_x = 0;
            let width = 0;
            let diff_width = 0;

            const 
                    EXTENDABLE = this.value_count < this.end,
                    RETRACTABLE = this.value_count > 1;

                if(EXTENDABLE && RETRACTABLE)
                    this.extra.setAttribute("action","both")
                else if(EXTENDABLE)
                    this.extra.setAttribute("action","ext")
                else
                    this.extra.setAttribute("action","ret")

            const move = (e) => {

                let 
                    diff = e.clientX - root_x,
                    min_diff = diff + diff_width;   

                const 
                    EXTENDABLE = this.value_count < this.end,
                    RETRACTABLE = this.value_count > 1;

                if(EXTENDABLE && RETRACTABLE)
                    this.extra.setAttribute("action","both")
                else if(EXTENDABLE)
                    this.extra.setAttribute("action","ext")
                else
                    this.extra.setAttribute("action","ret")

                if (diff > 15 && EXTENDABLE) {
                    let bb = this.element

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

            this.extra.onpointerdown = e => {
                width = this.element.clientWidth;
                root_x = e.clientX;
                window.addEventListener("pointermove", move);
                window.addEventListener("pointerup", up)
            }
        }

        return this;
    }
}


export const Segment = cached_factory(SegmentDefault);
