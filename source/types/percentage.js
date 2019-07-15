export default class CSS_Percentage extends Number {
    
    static setInput(input, value){
        input.type = "number";
        input.value = parseFloat(value);
    }

    static setValue(ele, value){
        ele.value = parseFloat(value) || 0;
        ele.css_value = ele.value + "%";
    }

    static buildInput(value){
        let ele = document.createElement("input")
        ele.type = "number";

        CSS_Percentage.setValue(ele, value);

        ele.addEventListener("change", (e)=>{
            ele.css_value = ele.value + "%"
        })
        
        return ele;
    }
    
    static parse(l, rule, r) {
        let tx = l.tx,
            pky = l.pk.ty;

        if (l.ty == l.types.num || tx == "-" && pky == l.types.num) {
            let mult = 1;

            if (l.ch == "-") {
                mult = -1;
                tx = l.p.tx;
                l.p.next();
            }

            if (l.p.ch == "%") {
                l.sync().next();
                return new CSS_Percentage(parseFloat(tx) * mult);
            }
        }
        return null;
    }

    static _verify_(l) {
        if(typeof(l) == "string" &&  !isNaN(parseInt(l)) && l.includes("%"))
            return true;
        return false;
    }

    static valueHandler(){
        let ele = document.createElement("input");
        ele.type = "number"
        ele.value = 100;
        return ele;
    }

    constructor(v) {

        if (typeof(v) == "string") {
            let lex = whind(v);
            let val = CSS_Percentage.parse(lex);
            if (val) 
                return val;
        }
        
        super(v);
    }

    toJSON() {
        return super.toString() + "%";
    }

    toString(radix) {
        return super.toString(radix) + "%";
    }

    get str() {
        return this.toString();
    }

    lerp(to, t) {
        return new CSS_Percentage(this + (to - this) * t);
    }

    copy(other){
        return new CSS_Percentage(other);
    }

    get type(){
        return "%";
    }
}

CSS_Percentage.label_name = "Percentage";

