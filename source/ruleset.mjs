import stylerule from "./stylerule.mjs";

export default class ruleset {
	constructor(ats, rules){
		this.rules = rules;

        rules.forEach(r=>r.parent = this);

        this.parent = null;
	}

    * getApplicableSelectors(element, win = window) {
        for(const rule of this.rules)
            yield * rule.getApplicableSelectors(element, win)
    }

	* getApplicableRules(element, win = window){
        for(const rule of this.rules)
            yield * rule.getApplicableRules(element, window)
    }
    /*
	getApplicableRules(element, new_rule = new stylerule, win = window, us) {

        for(const rule of this.rules){
            if(rule.match(element, win))
                new_rule.addProperty(rule);
        }
        
        return new_rule;
    }*/

    /* sends an update signal up the hiearchy to allow style sheets to alert observers of new changes. */
    update(){
        if(this.parent)
            this.parent.updated();
    }

    getRule(string) {
        let r = null;
        for (let node = this.fch; node; node = this.getNextChild(node))
            r = node.getRule(string, r);
        return r;
    }

    toString(){
        return this.rules.join("\n");
    }
}
