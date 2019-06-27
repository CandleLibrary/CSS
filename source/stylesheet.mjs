import stylerule from "./stylerule.mjs";
import whind from "@candlefw/whind";
import css_parser from "./Parser/css.mjs";

export default class stylesheet {
    constructor(sym) {
        this.ruleset = null;

        if (sym) {
            this.ruleset = sym[0];
            this.ruleset.parent = this;
        }

        this.parent = null;

        this.READY = true;

        this.observers = [];
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

            let ruleset = in_stylesheet.ruleset;
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
        if (this.observers.length > 0)
            for (let i = 0; i < this.observers.length; i++) this.observers[i].updatedCSS(this);
    }

    addObserver(observer) {
        this.observers.push(observer);
    }

    removeObserver(observer) {
        for (let i = 0; i < this.observers.length; i++)
            if (this.observers[i] == observer) return this.observers.splice(i, 1);
    }

    * getApplicableSelectors(element, win = window) {
        yield * this.ruleset.getApplicableSelectors(element, window);
    }

    getApplicableRules(element, win = window, RETURN_ITERATOR = false, new_rule = new stylerule) {
        if(!(element instanceof HTMLElement))
            return new_rule;

        const iter = this.ruleset.getApplicableRules(element, win);
        if (RETURN_ITERATOR) {
            return iter
        } else
            for (const rule of iter) {
                new_rule.addProperty(rule);
            }
        return new_rule;
    }

    * getApplicableProperties(element, win = window){
        for(const rule of this.getApplicableRules(element, win))
            yield * rule.iterateProps();
    }

    getRule(string) {
        let r = null;
        for (let node = this.fch; node; node = this.getNextChild(node))
            r = node.getRule(string, r);
        return r;
    }

    toString() {
        return this.ruleset + "";
    }
}
