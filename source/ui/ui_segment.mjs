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
        this.menu.style.display = "none";
        this.menu.appendChild(this.list);

        this.element = document.createElement("span");
        this.element.classList.add("css_ui_seg");

        this.element.appendChild(this.menu);
        this.element.appendChild(this.val);
        this.element.appendChild(this.ext)

        this.value_list = [];
        this.subs = [];
        this.sib = null;
        this.value_set;
        this.HAS_VALUE = false;

        this.element.addEventListener("mouseover", e => {
            this.setList();
        })
    }

    destroy() {
        this.element = null;
        this.val = null;
        this.list = null;
        this.ext = null;
        this.menu = null;
        this.subs.forEach(e => e.destroy())
        this.subs = null;
    }

    reset() {
        this.list.innerHTML = "";
        this.val.innerHTML = "";
        this.subs.forEach(e => e.destroy);
        this.subs = [];
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
        seg.parent = this;
        this.subs.push(seg);
        this.val.appendChild(seg.element)
    }

    setList() {
        if (this.prod && this.list.innerHTML == "") {
            if (!this.prod.buildList(this.list, this))
                this.menu.style.display = "none";
            else
                this.menu.style.display = "inline-block";
        }
    }

    setValueHandler(element) {
        this.val.innerHTML = "";
        this.val.appendChild(element);
        this.menu.style.display = "none";
        this.HAS_VALUE = true;
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

    promote(){

    }

    demote() {
        let seg = new Segment;
        seg.prod = this.prod;
        seg.css_val = this.css_val;

        let children = this.val.childNodes;
        if (children.length > 0) {
            for (let i = 0, l = children.length; i < l; i++) {
                seg.val.appendChild(children[0]);
            }
        } else {
            seg.val.innerHTML = this.val.innerHTML
        }
        
        this.reset();
        //this.prod = null;
        this.addSub(seg);
        seg.setList();
    }

    addRepeat(seg) {
        if (this.subs.length == 0)
            //Turn self into own sub seg
            this.demote();
        this.addSub(seg);
    }

    repeat(prod = this.prod) {
        if (this.end > this.value_count) {
            this.ext.style.display = "inline-block";

            this.ext.onclick = e => {
                if (this.subs.length == 0)
                    //Turn self into own sub seg
                    this.demote()

                prod.default(this, true);

                if (this.value_count >= this.end)
                    this.ext.style.display = "none";
            }
        } else {
            this.ext.style.display = "none";
        }
        this.setList();
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
