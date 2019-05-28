export default class compoundSelector {
    constructor(sym, env) {
        this.subclass = null;
        this.tag = null;
        this.pseudo = null;

        if (sym[0].type == "type")
            this.tag = sym.shift();

        if (sym[0] && sym[0][0] && sym[0][0].type !== "pseudoElement")
            this.subclass = sym.shift();

        this.pseudo = sym[0];
    }

    get type() {
        return "basic"
    }

    match(element, result) {

        if (this.tag) {
            this.tag.match(element, result);
            if (!result.match)
                return element;
        }

        if (this.subclass) {
            for (const sel of this.subclass) {
                sel.match(element, result);
                if (!result.match)
                    return element;
            }
        }

        if (this.pseudo) {
            this.subclass.match(element, result);
            if (!result.match)
                return element;
        }

        return element;
    }

    toString() {

    }
}
