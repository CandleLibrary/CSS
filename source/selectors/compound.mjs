export default class compoundSelector {
    constructor(sym, env) {

        if(sym.length = 1)
            if(Array.isArray(sym[0]) && sym[0].length == 1)
                return sym[0][0]
            else
                return sym[0]

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
        return "compound"
    }

    matchReturnElement(element, win) {
        if (this.tag) {
            if (!this.tag.matchReturnElement(element, win))
                return null;
        }

        if (this.subclass) {
            for (const sel of this.subclass) {
                if (!sel.matchReturnElement(element, win))
                    return null;
            }
        }

        if (this.pseudo) {
            if (!this.subclass.matchReturnElement(element, win))
                return null;
        }

        return element;
    }

    toString() {
        const
            tag = this.tag ? this.tag + "" : "",
            subclass = this.subclass ? this.subclass.join("") + "" : "",
            pseudo = this.pseudo ? this.pseudo + "" : "";

        return `${tag + subclass + pseudo}`;
    }
}
