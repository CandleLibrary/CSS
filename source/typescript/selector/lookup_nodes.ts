import { CSSTreeNode, CSSTreeNodeType, CSSRuleNode } from "../nodes/css_tree_node_type.js";
import { selector } from "../css.js";

export interface SelectionHelpers<Element> {
    hasAttribute: (ele: Element, name: string, value: string, sym: string, modifier: string) => boolean;
    hasType: (ele: Element, type: string) => boolean;
    hasClass: (ele: Element, class_: string) => boolean;
    hasID: (ele: Element, id: string) => boolean;
    hasPseudoClass: (ele: Element, id: string) => boolean;
    hasPseudoElement: (ele: Element, id: string) => boolean;
    WQmatch: (ele: Element, wq_selector: CSSTreeNode) => string;
    getParent: (ele: Element) => Element;
    getChildren: (ele: Element) => Element[];
}

export const DOMHelpers: SelectionHelpers<HTMLElement> = {
    hasAttribute: (ele, name, value, sym, modifier) => {
        const attrib = ele.getAttribute(name);
        if (attrib)
            if (value) return value == attrib;
            else return true;
        return false;
    },
    hasType: (ele, type) => {
        return ele.tagName == type.toUpperCase();
    },
    hasClass: (ele, class_) => {
        return ele.classList.contains(class_);
    },
    hasID: (ele, id) => {
        return ele.id == id;
    },
    hasPseudoClass: (ele, id) => {
        return ele.id == id;
    },
    hasPseudoElement: (ele, id) => {
        return ele.id == id;
    },
    WQmatch: (ele, wq_selector) => wq_selector.val,

    getParent(ele) {
        return ele.parentElement;
    },

    getChildren(ele) {
        return <HTMLElement[]>Array.from(ele.children);
    }
};

export function matchElement<Element>(ele, selector: CSSTreeNode, helpers: SelectionHelpers<Element>): boolean {

    switch (selector.type) {

        case CSSTreeNodeType.ComplexSelector: //Complex
            {
                const selectors = selector.nodes.slice().reverse();
                for (const selector of selectors)
                    if (!matchElement(ele, selector, helpers)) return false;
            }
            break;

        case CSSTreeNodeType.CompoundSelector:
            {
                const selectors = selector.nodes.slice().reverse();
                for (const selector of selectors)
                    if (!matchElement(ele, selector, helpers)) return false;
            }
            break;

        case CSSTreeNodeType.TypeSelector:
            return helpers.hasType(ele, helpers.WQmatch(ele, selector.nodes[0]));

        case CSSTreeNodeType.MetaSelector:
            return true;

        case CSSTreeNodeType.AttributeSelector:
            return helpers.hasAttribute(ele, helpers.WQmatch(ele, selector.nodes[0]), selector.val, selector.match_type, selector.mod);

        case CSSTreeNodeType.ClassSelector:
            return helpers.hasClass(ele, selector.val);

        case CSSTreeNodeType.IdSelector:
            return helpers.hasID(ele, selector.val);

        case CSSTreeNodeType.PseudoClassSelector:
            if (!helpers.hasPseudoClass(ele, selector.val)) return false;
            else if (selector.nodes[0]) return matchElement(ele, selector.nodes[0], helpers);
            break;

        case CSSTreeNodeType.PseudoElementSelector:
            if (!helpers.hasPseudoElement(ele, selector.val)) return false;
            else if (selector.nodes[0]) return matchElement(ele, selector.nodes[0], helpers);
            break;
    }

    return true;
}

export function* getMatchedElements<Element = HTMLElement>(
    ele: Element,
    selector: CSSTreeNode,
    helpers: SelectionHelpers<Element>
): Generator<Element, Element> {

    if (matchElement<Element>(ele, selector, helpers)) yield ele;

    for (const c_ele of helpers.getChildren(ele))
        yield* getMatchedElements<Element>(c_ele, selector, helpers);

    return;
};

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

export function doesRuleHaveMatchingSelector(rule: CSSRuleNode, selector: CSSTreeNode): boolean {

    if (!rule.type || rule.type !== CSSTreeNodeType.Rule)
        throw new Error("rule argument is not a CSSTreeNodeType.Rule");

    for (const match_selector of rule.selectors) {
        if (isSelectorEqual(selector, match_selector)) return true;
    }
    return false;
}

export const getMatchedHTMLElements = (ele: HTMLElement, selector) => getMatchedElements<HTMLElement>(ele, selector, DOMHelpers);

