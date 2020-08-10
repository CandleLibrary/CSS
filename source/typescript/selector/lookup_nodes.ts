import { CSSTreeNode, CSSTreeNodeType, CSSRuleNode } from "../nodes/css_tree_node_type.js";
import { selector } from "../css.js";

export interface SelectionHelpers<Element> {
    hasAttribute: (ele: Element, namespace: string, name: string, value: string, sym: string, modifier: string) => boolean;
    hasType: (ele: Element, namespace: string, type: string) => boolean;
    hasClass: (ele: Element, class_: string) => boolean;
    hasID: (ele: Element, id: string) => boolean;
    hasPseudoClass: (ele: Element, id: string, val: string) => boolean;
    hasPseudoElement: (ele: Element, id: string, val: string) => boolean;
    WQmatch: (ele: Element, wq_selector: CSSTreeNode) => string;
    getParent: (ele: Element) => Element;
    getChildren: (ele: Element) => Element[];
    getIndexFigures: (ele, tag_name) => { tag_index: number, ele_index: number; };
}

export const DOMHelpers: SelectionHelpers<HTMLElement> = {
    hasAttribute: (ele, namespace, name, value, sym, modifier) => {
        const attrib = ele.getAttribute(name);
        if (attrib)
            if (value) return value == attrib;
            else return true;
        return false;
    },
    hasType: (ele, namespace, name) => {
        if (!namespace)
            return ele.tagName == name.toUpperCase();
    },
    hasClass: (ele, class_) => {
        return ele.classList.contains(class_);
    },
    hasID: (ele, id) => {
        return ele.id == id;
    },
    hasPseudoClass: (ele, id, val) => {
        return ele.id == id;
    },
    hasPseudoElement: (ele, id, val) => {
        return ele.id == id;
    },

    WQmatch: (ele, wq_selector) => wq_selector.val,

    getParent(ele) {
        return ele.parentElement;
    },

    getIndexFigures(ele, tag_name): { tag_index: number, ele_index: number; } {
        const par = this.get(parent);

        let tag_index = 0;
        let ele_index = 0;

        for (const child of par.children) {
            if (child == ele) break;
            if (child.tagName == tag_name.toUpperCase) tag_index++;
            ele_index++;
        }

        return {
            tag_index,
            ele_index
        };
    }

    getChildren(ele) {
        return <HTMLElement[]>Array.from(ele.children);
    }
};

export function matchElement<Element>(ele, selector: CSSTreeNode, helpers: SelectionHelpers<Element>, meta?: any): boolean {



    switch (selector.type) {

        case CSSTreeNodeType.ComplexSelector: //Complex
            {
                const selectors = selector.nodes.slice().reverse();
                for (let i = 0; i < selectors.length;) {
                    const sel = selectors[i];

                    if (!matchElement(ele, sel, helpers)) {
                        if (!helpers.getParent(ele) || i == 0)
                            return false;
                    } else
                        i++;

                    ele = helpers.getParent(ele);

                    if (!ele) return false;

                }
            }
            break;

        case CSSTreeNodeType.CompoundSelector:
            for (const sel of selector.nodes)
                if (!matchElement(ele, sel, helpers)) return false;
            break;

        case CSSTreeNodeType.TypeSelector: {
            const { ns, val } = selector.nodes[0];
            //const { tag_index, ele_index } = helpers.getIndexFigures(ele, val);
            //meta.tag_index = tag_index;
            //meta.ele_index = val;
            return helpers.hasType(ele, ns, val);
        }

        case CSSTreeNodeType.MetaSelector:
            return true;

        case CSSTreeNodeType.AttributeSelector: {
            const { ns, val } = selector.nodes[0];
            return helpers.hasAttribute(ele, ns, val, selector.match_type, selector.match_val, selector.mod);
        }

        case CSSTreeNodeType.ClassSelector:
            return helpers.hasClass(ele, selector.val);

        case CSSTreeNodeType.IdSelector:
            return helpers.hasID(ele, selector.val);

        case CSSTreeNodeType.PseudoClassSelector:
            if (!helpers.hasPseudoClass(ele, selector.id, selector.val)) return false;
            else if (selector.nodes[0]) return matchElement(ele, selector.nodes[0], helpers);
            break;

        case CSSTreeNodeType.PseudoElementSelector:
            if (!helpers.hasPseudoElement(ele, selector.id, selector.val)) return false;
            else if (selector.nodes[0]) return matchElement(ele, selector.nodes[0], helpers);
            break;
    }

    return true;
}

export function isSelectorEqual(a: CSSRuleNode, b: CSSRuleNode) {
    if (b.type == a.type) {
        switch (b.type) {

            case CSSTreeNodeType.ComplexSelector:
            case CSSTreeNodeType.CompoundSelector:
                {

                    if (a.nodes.length == b.nodes.length) {
                        const selectorsA = a.nodes;
                        const selectorsB = b.nodes;

                        for (let i = 0; i < selectorsA.length; i++) {
                            const a_sub = selectorsA[i],
                                b_sub = selectorsB[i];
                            if (!isSelectorEqual(a_sub, b_sub)) return false;
                        }

                        return true;
                    }
                }
                break;

            case CSSTreeNodeType.TypeSelector:
                return a.ns == b.ns && b.val == a.val;

            case CSSTreeNodeType.AttributeSelector:
                return a.ns == b.ns && b.val == a.val && a.match_type == b.match_type && b.match_val == a.match_val && b.mod == a.mod;

            case CSSTreeNodeType.ClassSelector:
            case CSSTreeNodeType.IdSelector:
                return b.val == a.val;

            case CSSTreeNodeType.PseudoClassSelector:
            case CSSTreeNodeType.PseudoElementSelector:
                return a.id == b.id && b.val == a.val;
        }
    }


    return false;
}

function matchAnySelector<Element>(ele: Element, helpers: SelectionHelpers<Element>, ...selectors: CSSTreeNode[]): boolean {
    for (const selector of selectors)
        if (matchElement<Element>(ele, selector, helpers))
            return true;
    return false;
}

export function* getMatchedElements<Element = HTMLElement>(
    ele: Element,
    node: CSSTreeNode,
    helpers: SelectionHelpers<Element> = DOMHelpers
): Generator<Element, Element> {

    let selectors = null;

    if (node.type == CSSTreeNodeType.Rule) {
        selectors = node.selectors;
    } else if (node.type == CSSTreeNodeType.Stylesheet) {
        selectors = node.nodes
            .filter(n => n.type == CSSTreeNodeType.Rule)
            .flatMap(r => r.selectors);
    } else selectors = [node];

    if (matchAnySelector<Element>(ele, helpers, ...selectors)) yield ele;

    for (const c_ele of helpers.getChildren(ele))
        yield* getMatchedElements<Element>(c_ele, node, helpers);

    return;
};

export function getMatchedSelectors<Element>(rule: CSSRuleNode, ele: Element, helpers: SelectionHelpers<Element> = DOMHelpers): CSSTreeNode[] {

    const matches = [];

    if (!rule.type || rule.type !== CSSTreeNodeType.Rule)
        throw new Error("rule argument is not a CSSTreeNodeType.Rule");

    for (const match_selector of rule.selectors) {
        if (matchElement<Element>(ele, match_selector, helpers)) matches.push(match_selector);
    }

    return matches;
}

export function getFirstMatchedSelector<Element>(rule: CSSRuleNode, ele: Element, helpers: SelectionHelpers<Element>) {
    return getMatchedSelectors(rule, ele, helpers)[0];
}

export function doesRuleHaveMatchingSelector(rule: CSSRuleNode, selector: CSSTreeNode): boolean {

    if (!rule.type || rule.type !== CSSTreeNodeType.Rule)
        throw new Error("rule argument is not a CSSTreeNodeType.Rule");

    for (const match_selector of rule.selectors) {
        if (isSelectorEqual(selector, match_selector)) return true;
    }
    return false;
}

export function getLastRuleWithMatchingSelector(stylesheet: CSSTreeNode, selector: CSSTreeNode, helpers: SelectionHelpers<any> = DOMHelpers): CSSRuleNode {
    if (stylesheet.type != CSSTreeNodeType.Stylesheet) return null;

    for (const node of stylesheet.nodes.reverse()) {
        if (node.type == CSSTreeNodeType.Rule) {
            if (doesRuleHaveMatchingSelector(node, selector)) return node;
        } else if (node.type == CSSTreeNodeType.Media) {

        }
    }

    return null;
}

export const getMatchedHTMLElements = (ele: HTMLElement, selector) => getMatchedElements<HTMLElement>(ele, selector, DOMHelpers);

