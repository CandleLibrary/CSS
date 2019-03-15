export default  class CSS_String extends String {
    
    static list(){}

    static valueHandler(existing_value){
        let ele = document.createElement("input");
        ele.type = "text"
        ele.value = existing_value || "";
        return ele;
    }

    static setInput(input, value){
        input.type = "text";
        input.value = value;
    }

    static buildInput(){
        let ele = document.createElement("input")
        ele.type = "text";
        return ele;
    }

    static parse(l, rule, r) {
        if (l.ty == l.types.str) {
            let tx = l.tx;
            l.next();
            return new CSS_String(tx);
        }
        return null;
    }
}

var t = (s, l = s.length, n = parseFloat, i = isNaN)=> !i(n(s.slice(2))) &  (l==5 || (l==6 & ["",..."-_*"].includes(s[2]))) & !i(n(s.slice(-3)))