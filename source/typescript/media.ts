
function queryNodeToString(node) {
    switch (node.type) {

        case "query":
            return node.val.map(queryNodeToString).join(" ");

        case "type":
            return node.val;

        case "keyvalue":
            return `${node.key} : ${node.val}`;

        case "feature":
        case "parenthesis":
            return `( ${queryNodeToString(node.val)} )`;

        case "and":
            return `and ${queryNodeToString(node.val)}`;

        default:
            return node + "";

    }
}

export default class media {

    get type() { return "media"; }
    constructor(sym, rules = []) {

        this.queries = sym[2];

        this.rules = sym[4];

        this.rules.forEach(r => r.parent = this);

        this.parent = null;
    }

    destroy() {
        for (const rule of this.rules)
            rule.destroy();
        this.rules = null;
        this.parent = null;
    }

    * getApplicableSelectors(element, win = window) {
        for (const rule of this.rules)
            yield* rule.getApplicableSelectors(element, win);
    }

    * getApplicableRules(element, win = window) {
        for (const rule of this.rules)
            yield* rule.getApplicableRules(element, window);
    }

    /* sends an update signal up the hiearchy to allow style sheets to alert observers of new changes. */
    update() {
        if (this.parent)
            this.parent.updated();
    }

    getRule(string) {
        let r = null;
        for (let node = this.fch; node; node = this.getNextChild(node))
            r = node.getRule(string, r);
        return r;
    }

    toString() {
        return `@media ${this.queries.map(queryNodeToString).join(",")} {\n\t${this.rules.join("\n")}\n}`;
    }
}
