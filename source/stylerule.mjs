/*
 * Holds a set of css style properties.
 */

export default class stylerule {

	constructor(selectors = [], props = []){
		
		this.selectors = selectors;
		this.props = props;

		//Reference Counting
        this.refs = 0;

        //Versioning
        this.ver = 0;

        this.par = null;

        for(const prop of props){
            this[prop.name] = prop;
        }

        /*
        return new Proxy(this, {
            get:(obj, prop_name)=>{
                
                if(!this[prop_name]){
                    console.log(prop_name, props, props.filter(p=>p.name == prop_name))
                    return props.filter(p=>p.name == prop_name)[0];
                }

                return obj[prop_name];
            }
        })*/
	}

    addProperty(props) {
        if(props instanceof stylerule){
            props = props.props;
        }

        props = Array.isArray(props) ? props : [props];

        for(const prop of props){
            this.props.push(prop);
        }
    }

    match(element, window){
        for(const selector_array of this.selectors)
            if(selector_array[0].matchBottomUp(element, selector_array) !== null)
                return true;
        return false;
    }

    * getApplicableSelectors(element, window){
        for(const selector_array of this.selectors)
            if(selector_array[0].matchBU(element, selector_array) !== null)
                yield selector_array;
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
