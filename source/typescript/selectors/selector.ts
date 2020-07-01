export default class selector {

    vals: string[];

    parent: selector;

    constructor(sym, env) {
        if (sym.length > 1)
            this.vals = [sym[0], ...sym[1]];
        else
            this.vals = sym;

        this.parent = null;
    }

    match(element, win = window) {

        for (const selector of this.vals.reverse()) {
            if (!(element = selector.matchReturnElement(element, win)))
                return false;
        }
        return true;
    }

    toString() {
        return this.vals.join(" ");
    }
}
