import parseDeclaration from "./properties/parse_declaration.mjs";

function setParent(array, parent) {
    for (const prop of array)
        prop.parent = parent;
}
/*
 * Holds a set of css style properties.
 */

export default class stylerule {

    constructor(selectors = [], props = []) {
        this.selectors = selectors;
        this.properties = new Map;

        this.addProp(props);
        //Reference Counting
        this.refs = 0;

        //Versioning
        this.ver = 0;

        this.parent = null;

        setParent(this.selectors, this);
        setParent(this.properties.values(), this);

        this.props = new Proxy(this, this);
        this.addProperty = this.addProp;
        this.addProps = this.addProp;
    }

    get type(){
        return "stylerule"
    }

    get(obj, name) {
        let prop = obj.properties.get(name);
        if (prop)
            prop.parent = this;
        return prop;
    }
    /*  
        Adds properties to the stylerule
        arg1 string - accepts a string of semicolon seperated css style rules.   
    */
    addProp(props) {
        if (typeof props == "string") {
            return this.addProps(
                props.split(";")
                .filter(e=> e !== "")
                .map((e, a) => (a = e.split(":"), a.splice(1, 0, null), a))
                .map(parseDeclaration)
            )
        }

        if (props.type == "stylerule")
            props = props.properties.values();
        else
        if (!Array.isArray(props))
            props = [props];

        for (const prop of props)
            if (prop)
                this.properties.set(prop.name, prop);

        this.ver++;

        return props;
    }

    match(element, window) {
        for (const selector of this.selectors)
            if (selector.match(element, window))
                return true;
        return false;
    }

    * getApplicableSelectors(element, window) {
        for (const selector of this.selectors)
            if (selector.match(element, window))
                yield selector;
    }

    * getApplicableRules(element, window) {
        if (this.match(element, window))
            yield this;
    }

    * iterateProps() {
        for (const prop of this.properties.values())
            yield prop;
    }

    incrementRef() {
        this.refs++;
    }

    decrementRef() {
        this.refs--;
        if (this.refs <= 0) {
            //TODO: remove from rules entries.
            debugger
        }
    }

    toString(off = 0, rule = "") {

        let str = [],
            offset = ("    ").repeat(off);

        for (const prop of this.properties.values())
            str.push(prop.toString(off));

        return `${this.selectors.join("")}{${str.join(";")}}`;
    }

    merge(rule) {
        if (rule.type ==  "stylerule")
            this.addProp(rule);
    }

    get _wick_type_() { return 0; }

    set _wick_type_(v) {}
}
