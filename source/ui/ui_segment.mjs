export class Segment {
    constructor(parent) {
        this.parent = null;

        this.css_val = "";

        this.val = document.createElement("span");
        this.val.classList.add("prop_value");

        this.list = document.createElement("div");
        this.list.classList.add("prop_list");
        //this.list.style.display = "none"

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

        this.value_list = [];
        this.subs = [];
        this.old_subs = [];
        this.sib = null;
        this.value_set;
        this.HAS_VALUE = false;
        this.DEMOTED = false;

        this.element.addEventListener("mouseover", e => {
            //this.setList();
        })
    }

    destroy() {
        this.parent = null;
        this.element = null;
        this.val = null;
        this.list = null;
        this.ext = null;
        this.menu_icon = null;
        this.subs.forEach(e => e.destroy())
        this.subs = null;
    }

    reset() {
        this.list.innerHTML = "";
        this.val.innerHTML = "";
        //this.subs.forEach(e => e.destroy);
        this.subs = [];
        this.setElement = null;
        this.changeEvent = null;
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


    addSub(seg) {
        this.menu_icon.setAttribute("superset", true)
        seg.parent = this;
        this.subs.push(seg);
        this.val.appendChild(seg.element)
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

    setList() {
        if(this.DEMOTED) debugger
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

    setValueHandler(element, change_event_function) {
        this.val.innerHTML = "";
        this.val.appendChild(element);

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

    promote() {

    }

    demote() {
        let seg = new Segment;
        seg.prod = this.prod;
        seg.css_val = this.css_val;

        if (this.change_event_function) {
            seg.changeEvent = this.changeEvent;
            seg.setElement = this.setElement;
            seg.setElement.onchange = seg.change.bind(seg);
        }

        let subs = this.subs;

        if (subs.length > 0) {

            for (let i = 0; i < this.subs.length; i++) 
                seg.addSub(this.subs[i]);
            
        } else {


            let children = this.val.childNodes;

            if (children.length > 0) {
                for (let i = 0, l = children.length; i < l; i++) {
                    seg.val.appendChild(children[0]);
                }
            } else {
                seg.val.innerHTML = this.val.innerHTML
            }
        }


        this.menu_icon.innerHTML = ""
        this.menu_icon.style.display = "none";
        this.menu_icon.setAttribute("superset", false)
        this.list.innerHTML = "";

        this.reset();

        this.addSub(seg);
        seg.setList();
        
        this.DEMOTED = true;
    }

    addRepeat(seg) {
        if (!this.DEMOTED)
            //Turn self into own sub seg
            this.demote();
        this.addSub(seg);
        seg.setList();
    }

    repeat(prod = this.prod) {
        if (this.value_count <= this.end && this.start + this.end !== 2) {
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


            /*
            this.ext.onclick = e => {
                if (this.subs.length == 0)
                    //Turn self into own sub seg
                    this.demote()

                prod.default(this, true);

                if (this.value_count >= this.end)
                    this.ext.style.display = "none";
            }
            */
        } else {
            this.ext.style.display = "none";
        }
        this.setList();
        this.update();
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

    getValue() {
        let val = ""

        if (this.subs.length > 0)
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
