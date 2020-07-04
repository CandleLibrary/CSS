import { CSSTreeNode, CSSTreeNodeType } from "../nodes/css_tree_node_type.js";

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

const DOMHelpers: SelectionHelpers<HTMLElement> = {
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

export function matchElements<Element>(ele, selector: CSSTreeNode, helpers: SelectionHelpers<Element>): boolean {

    switch (selector.type) {

        case CSSTreeNodeType.ComplexSelector: //Complex
            {
                const selectors = selector.nodes.slice().reverse();
                for (const selector of selectors)
                    if (!matchElements(ele, selector, helpers)) return false;
            }
            break;

        case CSSTreeNodeType.CompoundSelector:
            {
                const selectors = selector.nodes.slice().reverse();
                for (const selector of selectors)
                    if (!matchElements(ele, selector, helpers)) return false;
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
            else if (selector.nodes[0]) return matchElements(ele, selector.nodes[0], helpers);
            break;

        case CSSTreeNodeType.PseudoElementSelector:
            if (!helpers.hasPseudoElement(ele, selector.val)) return false;
            else if (selector.nodes[0]) return matchElements(ele, selector.nodes[0], helpers);
            break;
    }

    return true;
}

export function* getMatchedElements<Element = HTMLElement>(
    ele: Element,
    selector: CSSTreeNode,
    helpers: SelectionHelpers<Element>
): Generator<Element, Element> {

    if (matchElements<Element>(ele, selector, helpers)) yield ele;

    for (const c_ele of helpers.getChildren(ele))
        yield* getMatchedElements<Element>(c_ele, selector, helpers);

    return;
};

export const getMatchedHTMLElements = (ele: HTMLElement, selector) => getMatchedElements<HTMLElement>(ele, selector, DOMHelpers);

