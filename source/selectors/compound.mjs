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

    match(element) {
        if (this.tag) {
            if (!this.tag.match(element))
                return null;
        }

        if (this.subclass) {
            for (const sel of this.subclass) {
                if (!sel.match(element))
                    return null;
            }
        }

        if (this.pseudo) {
            if (!this.subclass.match(element))
                return null;
        }

        return element;
    }

    matchBU(element, selector_array, selector = null, index = 0) {
        if (index + 1 < selector_array.length) {
            return selector_array[index + 1].matchBU(element, selector_array, this, index + 1);
        } else {
            return this.match(element);
        }
    }

    toString() {
        const 
            tag = this.tag ? this.tag + "" : "",
            subclass = this.subclass ? this.subclass.join("") + "" : "",
            pseudo = this.pseudo ? this.pseudo + "" : "";

        return `${tag + subclass + pseudo}`;
    }
}