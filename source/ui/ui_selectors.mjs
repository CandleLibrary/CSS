import whind from "@candlefw/whind";
import cached_factory from "@candlefw/cached_factory";

function dragstart(e){
    event.dataTransfer.setData('text/plain',null)
    UISelectorPart.dragee = this;
}

function dragover(e){
    e.preventDefault();
}

class UISelectorPart{

    constructor(selector_part, index){
        this.txt = selector_part + "";
        this.index = index;
        this.element = document.createElement("span");
        this.element.classList.add("selector");
        this.element.innerHTML = this.txt;
        this.element.setAttribute("draggable", true)
        this.parent = null;
        this.element.addEventListener("dragstart",dragstart.bind(this))
    }

    mount(element, parent){
        this.parent = parent;
        if (element instanceof HTMLElement)
            element.appendChild(this.element);
    }

    unmount(){
        this.parent = null;
        if (this.element.parentElement)
            this.element.parentElement.removeChild(this.element);
    }

    compare(other_part){
        return other_part.txt === this.txt
    }

    toString(){
        return this.txt;
    }

};


function drop(e){
    if(UISelectorPart.dragee){
        const part = UISelectorPart.dragee;
        const parent = part.parent;

        loop:
        while(parent != this){

            //Ignore if part is already present in the selector area
            for(let i = 0; i < this.parts.length; i++)
                if(this.parts[i].compare(part)) break loop;

            part.unmount();
            let d = parent.remove(part);
            this.add(part, d);
            part.mount(this.element, this);
            break;
        }
    }
    UISelectorPart.dragee = null;
    e.preventDefault();
    e.stopPropagation();
    e.stopImmediatePropagation();
    return false;
}

class UISelector {
    constructor(selector) {
        this.selector = null;
        this.parts = null;        
        this.text = "";
    }

    initializer(selector){
        this.selector = selector;
        this.parts = [];
        this.text = selector + "";

        selector.vals.forEach((e, i) => {
            this.parts.push(new UISelectorPart(e, i))
        })
    }

    destructor(){
        this.selector = null;
        this.text = "";
    }

    destroy(){
        cached_factory.collect(this);
    }

    update() {
        this.parent.update();
    }

    mount(parent) {
        this.element = parent.selector_space;
        this.element.ondrop = drop.bind(this);
        this.element.ondragover = dragover;
        
        this.parent = parent;

        this.parts.forEach(e=>e.mount(this.element, this));
    }

    unmount() {
        if (this.element.parentElement)
            this.element.parentElement.removeChild(this.element);
    }

    remove(part){
        let index = part.index;
        this.parts.splice(index,1);
        this.parts.forEach((e,i)=>e.index = i);
        //const a = this.selector.a.splice(index,1)[0];
        const v = this.selector.vals.splice(index,1)[0];
        this.update();
        return v
    }

    add(part, v){
        this.parts.push(part)
        //this.selector.a.push(a);
        this.selector.vals.push(v);
        this.parts.forEach((e,i)=>e.index = i);
        this.update();
    }

    rebuild(selector){
        this.parts.forEach(e=>e.unmount(false))
        this.parts.length = 0;
        selector.vals.forEach((e,i) => {
            this.parts.push(new UISelectorPart(e, i))
        })
        this.mount(this.parent);
    }
}


export default cached_factory(UISelector);
