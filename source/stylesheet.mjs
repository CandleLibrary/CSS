export default class stylesheet {
	constructor(sym){
		this.rulesets = sym[0];
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
}