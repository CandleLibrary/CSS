export class Segment {
    constructor(parent) {
        this.parent = null;

        this.css_val = "";

        this.val = document.createElement("span");
        this.val.classList.add("css_ui_val");

        this.list = document.createElement("div");
        this.list.classList.add("css_ui_list");
        //this.list.style.display = "none"

        this.ext = document.createElement("button");
        this.ext.classList.add("css_ui_ext");
        this.ext.innerHTML = "+"
        this.ext.style.display = "none";

        this.menu = document.createElement("span");
        this.menu.classList.add("css_ui_menu");
        this.menu.innerHTML = "+"
        this.menu.appendChild(this.list);

        this.element = document.createElement("span");
        this.element.classList.add("css_ui_seg");

        this.element.appendChild(this.menu);
        this.element.appendChild(this.val);
        this.element.appendChild(this.ext)

        this.value_list = [];
        this.subs = [];
        this.sib = null;
        //*
        this.element.addEventListener("mouseover", e => {
            if (this.prod && this.list.innerHTML == "") {
                this.prod.buildList(this.list, this);
            }
        })
        //*//
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

    addSub(seg) {
        seg.parent = this;
        this.subs.push(seg);
        this.val.appendChild(seg.element)
    }

    displayList() {

    }

    destroy() {

    }

    set value(v) {
        this.val.innerHTML = v;
        this.css_val = v;
    }

    repeat(prod = this.prod) {

        if(this.end > this.subs.length){
            this.ext.style.display = "inline-block";

            this.ext.onclick = e => {
                if(this.subs.length == 1){
                    //Turn self into own seg
                    let seg = new Segment;                        
                    seg.prod = this.prod;
                    seg.css_val = this.css_val;
                    this.val.innerHTML = "";
                    this.list.innerHTML = "";
                    this.menu.style.display = "none";
                    this.prod = null;
                    let s = this.subs[0]
                    this.subs = [];
                    this.addSub(seg);
                    seg.addSub(s);
                }

                let seg = new Segment;
                seg.prod = prod;
                this.addSub(seg);
                prod.extend(seg)
                
                if (this.subs.length >= this.end)
                    this.ext.style.display = "none";
            }
        }else{
            this.ext.style.display = "none";
        }
    }

    mount(element) {
        element.appendChild(this.element);
    }

    setValueHandler(element) {
        this.val.innerHTML = "";
        this.val.appendChild(element);
    }

    update() {
        if (this.parent)
            this.parent.update();
        else {
            let val = this.getValue();
            console.log(val)
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

    reset(){
        this.val.innerHTML = "";
        this.subs.forEach(e=>e.destroy);
        this.subs = [];
    }


    updateFunction(v) {}
}
