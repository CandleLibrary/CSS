/**
 * Holds a set of rendered CSS properties.
 * @memberof module:wick~internals.css
 * @alias CSSRule
 */
export class CSSRule {
    constructor(root) {
        /**
         * Collection of properties held by this rule.
         * @public
         */
        this.props = [];
        this.LOADED = false;
        this.root = root;

        //Reference Counting
        this.refs = 0;

        //Versioning
        this.ver = 0;
    }

    incrementRef(){
        this.refs++;
    }

    decrementRef(){
        this.refs--;
        if(this.refs <= 0){
            //TODO: remove from rules entries.
            debugger
        }
    }

    addProperty(prop, rule) {
        if (prop)
            this.props[prop.name] = prop.value;
    }



    toString(off = 0, rule = "") {
        let str = [],
            offset = ("    ").repeat(off);

        if (rule) {
            if (this.props[rule]) {
                if (Array.isArray(this.props[rule]))
                    str.push(this.props[rule].join(" "));
                else
                    str.push(this.props[rule].toString());
            }else
                return "";
        } else {
            for (const a of this.props) {
                if (a !== null) {
                    if (Array.isArray(this.props[a]))
                        str.push(offset, a.replace(/\_/g, "-"), ":", this.props[a].join(" "), ";\n");
                    else
                        str.push(offset, a.replace(/\_/g, "-"), ":", this.props[a].toString(), ";\n");
                }
            }
        }

        return str.join(""); //JSON.stringify(this.props).replace(/\"/g, "").replace(/\_/g, "-");
    }

    merge(rule) {
        if (rule.props) {
            for (let n in rule.props)
                this.props[n] = rule.props[n];
            this.LOADED = true;
            this.ver++;
        }
    }

    get _wick_type_() { return 0; }

    set _wick_type_(v) {}
}
