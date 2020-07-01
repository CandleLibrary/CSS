import observer from "@candlefw/observer";

import stylerule from "./stylerule.js";

export default class stylesheet {

    rules: any[];
    parent: any;
    READY: boolean;

    imports: any[];

    constructor(sym) {

        const [imports, blocks = []] = sym[0];

        this.imports = imports;

        this.rules = blocks;

        this.parent = null;

        this.READY = true;
    }

    destroy() {

        this.rules.forEach(r => r.destroy());
        this.parent = null;
        this.READY = false;

        observer.destroy(this);
    }

    get css_type() {
        return "stylesheet";
    }

    /**
     * Creates a new instance of the object with same properties as the original.
     * @return     {CSSRootNode}  Copy of this object.
     * @public
     */
    clone() {
        let rn = new this.constructor();
        rn._selectors_ = this._selectors_;
        rn._sel_a_ = this._sel_a_;
        rn._media_ = this._media_;
        return rn;
    }

    merge(in_stylesheet) {
        if (in_stylesheet instanceof stylesheet) {

            let ruleset = in_stylesheet.rules;
            outer:
            for (let i = 0; i < children.length; i++) {
                //determine if this child matches any existing selectors
                let child = children[i];

                for (let i = 0; i < this.children.length; i++) {
                    let own_child = this.children[i];

                    if (own_child.isSame(child)) {
                        own_child.merge(child);
                        continue outer;
                    }
                }

                this.children.push(child);
            }
        }
    }

    _resolveReady_(res, rej) {
        if (this.pending_build > 0) this.resolves.push(res);
        res(this);
    }

    _setREADY_() {
        if (this.pending_build < 1) {
            for (let i = 0, l = this.resolves; i < l; i++) this.resolves[i](this);
            this.resolves.length = 0;
            this.res = null;
        }
    }

    updated() {
        this.updateObservers();
    }

    * getApplicableSelectors(element, win = window) {
        yield* this.rules.flatMap(s => [...s.getApplicableSelectors(element, window)]);
    }

    getApplicableRules(element, win = window, RETURN_ITERATOR = false, new_rule = new stylerule) {
        if (!(element instanceof HTMLElement))
            return new_rule;

        const iter = this.rules.getApplicableRules(element, win);
        if (RETURN_ITERATOR) {
            return iter;
        } else
            for (const rule of iter) {
                new_rule.merge(rule);
            }
        return new_rule;
    }

    * getApplicableProperties(element, win = window) {
        for (const rule of this.getApplicableRules(element, win, true))
            yield* rule.iterateProps();
    }

    getRule(string) {
        let r = null;
        for (let node = this.fch; node; node = this.getNextChild(node))
            r = node.getRule(string, r);
        return r;
    }

    toString() {
        return this.rules.map(r => r + "").join("\n");
    }
}

observer("updatedCSS", stylesheet.prototype);
