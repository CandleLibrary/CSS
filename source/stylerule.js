import parseDeclaration from "./properties/parse_declaration.js";
import observer from "@candlefw/observer";

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

        //Versioning
        this.ver = 0;

        this.parent = null;

        setParent(this.selectors, this);
        setParent(this.properties.values(), this);

        this.props = new Proxy(this, this);
        this.addProperty = this.addProp;
        this.addProps = this.addProp;
        this.UPDATE_LOOP_GAURD = false;
    }
    
    get css_type(){
        return "stylerule"
    }

    destroy(){
        
        for(const prop of this.properties.values())
            prop.destroy();

        for(const selector of this.selectors)
            selector.destroy();

        this.parent = null;
        this.selectors = null;
        this.properties = null;

        observer.destroy(this);
    }

    /* sends an update signal up the hiearchy to allow style sheets to alert observers of new changes. */
    update() {
        this.ver++;

        //if(this.UPDATE_LOOP_GAURD) return;

        if (this.parent)
            this.parent.update();

        this.updateObservers();
    }

    get type() {
        return "stylerule"
    }

    get(obj, name) {
        let prop = obj.properties.get(name);
        //if (prop)
        //    prop.parent = this;
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
                .filter(e => e !== "")
                .map((e, a) => (a = e.split(":"), a.splice(1, 0, null), a))
                .map(parseDeclaration)
            )
        }

        if (props.type == "stylerule")
            props = props.properties.values();
        else
        if (!Array.isArray(props))
            props = [props];


       // this.UPDATE_LOOP_GAURD = true;
        for (const prop of props)
            if (prop) {
                if(this.properties.has(prop.name))
                    this.properties.get(prop.name).setValue(...prop.val);
                else
                    this.properties.set(prop.name, prop);
                
                prop.parent = this;
            }
        //this.UPDATE_LOOP_GAURD = false;

        this.ver++;

        this.update();

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

    toString(off = 0, rule = "") {

        let str = [],
            offset = ("    ").repeat(off);

        for (const prop of this.properties.values())
            str.push(prop.toString(off));

        return `${this.selectors.join("")}{${str.join(";")}}`;
    }

    merge(rule) {
        if(!rule) return;
        if (rule.type == "stylerule"){
            for (const prop of rule.properties.values()){
                if (prop) {
                    this.properties.set(prop.name, prop);
                }
            }
        }
                
    }

    get _wick_type_() { return 0; }

    set _wick_type_(v) {}
}

observer("updatedCSSStyleRule", stylerule.prototype);
