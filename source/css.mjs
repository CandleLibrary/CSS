import  ll  from "@candlefw/ll";
import  whind from "@candlefw/whind";

import { CSSRule as R, CSSSelector as S } from "./nodes";
import { types } from "./properties/property_and_type_definitions";
import { CSSRuleBody } from "./body";

export { R as CSSRule, S as CSSSelector };



/**
 * Container for all rules found in a CSS string or strings.
 *
 * @memberof module:wick~internals.css
 * @alias CSSRootNode
 */
class CSSRootNode {
    constructor() {
        this.promise = null;
        /**
         * Media query selector
         */
        this.pending_build = 0;
        this.resolves = [];
        this.res = null;
        this.observers = [];
        
        this.addChild(new CSSRuleBody());
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

    READY() {
        if (!this.res) this.res = this._resolveReady_.bind(this);
        return new Promise(this.res);
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

    * getApplicableSelectors(element, win = window) {

        for (let node = this.fch; node; node = this.getNextChild(node)) {

            if(node.matchMedia(win)){
                let gen = node.getApplicableSelectors(element, win);
                let v = null;
                while ((v = gen.next().value))
                    yield v;
            }
        }
    }

    /**
     * Retrieves the set of rules from all matching selectors for an element.
     * @param      {HTMLElement}  element - An element to retrieve CSS rules.
     * @public
     */
    getApplicableRules(element, rule = new R(), win = window) {
        for (let node = this.fch; node; node = this.getNextChild(node))
            node.getApplicableRules(element, rule, win);
        return rule;
    }

    /**
     * Gets the last rule matching the selector
     * @param      {string}  string  The string
     * @return     {CSSRule}  The combined set of rules that match the selector.
     */
    getRule(string) {
        let r = null;
        for (let node = this.fch; node; node = this.getNextChild(node))
            r = node.getRule(string, r);
        return r;
    }

    toString(off = 0) {
        let str = "";
        for (let node = this.fch; node; node = this.getNextChild(node))
            str += node.toString(off);
        return str;
    }

    addObserver(observer) {
        this.observers.push(observer);
    }

    removeObserver(observer) {
        for (let i = 0; i < this.observers.length; i++)
            if (this.observers[i] == observer) return this.observers.splice(i, 1);
    }

    updated() {
        if (this.observers.length > 0)
            for (let i = 0; i < this.observers.length; i++) this.observers[i].updatedCSS(this);
    }

    parse(lex, root) {
        if (typeof(lex) == "string")
            lex = whind(lex);

        if (lex.sl > 0) {

            if (!root && root !== null) {
                root = this;
                this.pending_build++;
            }

            return this.fch.parse(lex, this).then(e => {
                this._setREADY_();
                this.updated();
                return this;
            });
        }
    }

    merge(inCSSRootNode){
        if(inCSSRootNode instanceof CSSRootNode){
            
            let children = inCSSRootNode.children;
            outer:
            for(let i = 0; i < children.length; i++){
                //determine if this child matches any existing selectors
                let child = children[i];
                
                for(let i = 0; i < this.children.length; i++){
                    let own_child = this.children[i];

                    if(own_child.isSame(child)){
                        own_child.merge(child);
                        continue outer;
                    }
                }

                this.children.push(child);
            }
        }
    }
}

/**
 * CSSRootNode implements all of ll
 * @extends ll
 * @memberof  module:wick~internals.html.CSSRootNode
 * @private
 */
ll.mixinTree(CSSRootNode);

export { CSSRootNode };
/*
 * Expecting ID error check.
 */
const _err_ = "Expecting Identifier";

/**
 * Builds a CSS object graph that stores `selectors` and `rules` pulled from a CSS string. 
 * @function
 * @param {string} css_string - A string containing CSS data.
 * @param {string} css_string - An existing CSSRootNode to merge with new `selectors` and `rules`.
 * @return {Promise} A `Promise` that will return a new or existing CSSRootNode.
 * @memberof module:wick.core
 * @alias css
 */
export const CSSParser = (css_string, root = null) => (root = (!root || !(root instanceof CSSRootNode)) ? new CSSRootNode() : root, root.parse(whind(css_string)));

CSSParser.types = types;

export default CSSParser;

