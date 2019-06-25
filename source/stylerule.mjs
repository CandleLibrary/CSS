/*
 * Holds a set of css style properties.
 */

export default class stylerule {

	constructor(selectors, props){
		
		this.selectors = selectors;
		this.props = props;

		//Reference Counting
        this.refs = 0;

        //Versioning
        this.ver = 0;

        console.log(this + "")
	}

	getApplicableRules(element, rule = new CSSRule(), win = window) {
        for (let node = this.fch; node; node = this.getNextChild(node))
            node.getApplicableRules(element, rule, win);
        return rule;
    }

    getRule(string) {
        let r = null;
        for (let node = this.fch; node; node = this.getNextChild(node))
            r = node.getRule(string, r);
        return r;
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

    toString(off = 0, rule = "") {
        let str = [],
            offset = ("    ").repeat(off);

        for(const prop of this.props){
        	str.push(prop.toString(off));
        }

        return `${this.selectors.join("")}{${str.join(";")}}`; 
    }

    addProperty(prop, rule) {
        if (prop)
            this.props[prop.name] = prop.value;
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