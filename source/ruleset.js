import stylerule from "./stylerule.js";

export default class ruleset {
	constructor(ats, rules){
		this.rules = rules;

        rules.forEach(r=>r.parent = this);

        this.parent = null;
	}

    destroy(){
        for(const rule of this.rules)
            rule.destroy();
        this.rules = null;
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
