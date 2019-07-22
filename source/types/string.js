import whind from "@candlefw/whind";

export default class CSS_String extends String {

    static list() {}

    static setValue(ui_segment, value) {
        ui_segment.setElement.innerHTML = (value) ? value + "" : "";
    }

    static valueHandler(ui_segment, value, update_function) {
        const ele = document.createElement("div");

        ele.type = "text";
        
        ele.createTextNode(whind(value+""), 0, 50)

        //ele.addEventListener("change", (e) => { ele.css_value = ele.value; })

        ui_segment.setValueHandler(ele, update_function);

        CSS_String.setValue(ui_segment, value);
    }

    static parse(l) {
        if (l.ty == l.types.str) {
            let tx = l.tx;
            l.next();
            return new CSS_String(tx);
        }
        return null;
    }

    constructor(string) {
        //if(string[0] == "\"" || string[0] == "\'" || string[0] == "\'")
        //    string = string.slice(1,-1);
        super(string)
    }
}
