import { addModuleToCFW } from "@candlefw/candle";
import { property_definitions, media_feature_definitions } from "./properties/property_and_type_definitions.js";
import { getPropertyParser } from "./properties/parser.js";
import parseDeclaration from "./properties/parse_declaration.js";

import * as productions from "./properties/productions.js";
import * as terms from "./properties/terms.js";

import CSS_Length from "./types/length.js";
import CSS_URL from "./types/url.js";
import CSS_Color from "./types/color.js";
import CSS_Percentage from "./types/percentage.js";
import CSS_String from "./types/string.js";
import CSS_Id from "./types/id.js";
import CSS_Shape from "./types/shape.js";
import CSS_Number from "./types/number.js";
import CSS_Bezier from "./types/cubic_bezier.js";
import CSS_Gradient from "./types/gradient.js";
import CSS_Transform2D from "./types/transform.js";
import CSS_Path from "./types/path.js";
import CSS_FontName from "./types/font_name.js";

import { CSSProperty } from "./properties/property.js";
import { CSSNodeType } from "./types/node_type.js";
import { CSSNode, CSSRuleNode } from "./types/node";

import {
    getMatchedElements,
    SelectionHelpers,
    matchElement,
    DOMHelpers,
    isSelectorEqual,
    doesRuleHaveMatchingSelector,
    getFirstMatchedSelector,
    getMatchedSelectors,
    getLastRuleWithMatchingSelector,
    getMatchedRules,
    matchAnySelector
} from "./selector/lookup_nodes.js";

import { selector, properties, parse, property, rule } from "./parser/parse.js";
import { CSSNodeTypeLU } from "./types/node_type_lu.js";
export * from "./parser/parse.js";

const types = {
    color: CSS_Color,
    length: CSS_Length,
    time: CSS_Length,
    flex: CSS_Length,
    angle: CSS_Length,
    frequency: CSS_Length,
    resolution: CSS_Length,
    percentage: CSS_Percentage,
    url: CSS_URL,
    uri: CSS_URL,
    number: CSS_Number,
    id: CSS_Id,
    string: CSS_String,
    shape: CSS_Shape,
    cubic_bezier: CSS_Bezier,
    integer: CSS_Number,
    gradient: CSS_Gradient,
    transform2D: CSS_Transform2D,
    path: CSS_Path,
    fontname: CSS_FontName
};

const newRule = function (): CSSRuleNode {
    return <CSSRuleNode>{
        selectors: [],
        props: new Map,
        type: CSSNodeType.Rule,
        pos: null,
    };
};

function removeRule(stylesheet: CSSNode, rule: CSSRuleNode) {

    for (let i = 0; i < stylesheet.nodes.length; i++) {

        const node = stylesheet.nodes[i];

        if (node.type == CSSNodeType.Rule) {
            if (rule == node) {
                stylesheet.nodes.splice(i, 1);
                return true;
            }
        }

        if (node.type == CSSNodeType.Media && removeRule(node, rule))
            return true;

        if (node.type == CSSNodeType.Keyframes && removeRule(node, rule))
            return true;
    }

    return false;
}

export function matchAll<Element>(selector_string, ele, helpers: SelectionHelpers<Element>): Element[] {
    const selector_node = selector(selector_string);
    return [...getMatchedElements<Element>(ele, selector_node, helpers)];
};

/**
 * Merges properties and selectors from an array of rules into  a single,propert
 * monolithic rule. Property collisions are resolved in a first-come::only-set
 * basis, unless **!important** has been set on a following property.
 * 
 * Assumes rule precedence greatest to least, or lowest to highest (if
 * described as within a CSS).
 * 
 * @param rules 
 */
export function mergeRulesIntoOne(...rules: CSSRuleNode[]): CSSRuleNode {

    const new_rule = <CSSRuleNode>{
        type: CSSNodeType.Rule,
        props: new Map(),
        selectors: []
    };

    for (const rule of rules) {
        for (const prop of rule.props.values())
            if (!new_rule.props.has(prop.name) || prop.IMPORTANT)
                new_rule.props.set(prop.name, prop);

        new_rule.selectors.push(...(rule.selectors || []));
    }

    return new_rule;
}

export function addPropsToRule(rule: CSSRuleNode, prop_string: string): CSSRuleNode {

    const props = properties(prop_string);

    for (const prop of props.values())
        rule.props.set(prop.name, prop);

    return rule;
}

function renderProps(rule: CSSRuleNode) {
    return Array.from(rule.props.values()).join(";");
}

export * from "./render/render.js";
export * from "./render/rules.js";
export * from "./selector/lookup_nodes.js";

export {
    //object types
    CSSProperty,
    CSS_Length as length,
    CSS_Length,
    CSS_Percentage as percentage,
    CSS_URL,
    CSS_URL as url,
    types,
    terms,
    productions,
    property_definitions,
    media_feature_definitions,
    CSSNodeTypeLU,

    //pure types
    CSSNodeType,
    CSSNode,
    CSSRuleNode,

    //functions
    removeRule,
    renderProps,
    parseDeclaration,
    getPropertyParser,
    isSelectorEqual,
    doesRuleHaveMatchingSelector,
    getLastRuleWithMatchingSelector
};

addModuleToCFW(Object.assign({
    //types
    CSSNodeType: CSSNodeTypeLU,
    CSS_URL,

    //parsers
    parse,
    selector,
    rule,
    property,

    //Properties
    getPropertyParser,
    media_feature_definitions,
    terms,
    parseDeclaration,
    productions,
    property_definitions,
    properties,

    //Rule Helpers
    addPropsToRule,
    mergeRulesIntoOne,
    newRule,
    removeRule,
    renderProps,

    //Selector helpers
    matchAll,
    getMatchedSelectors,
    getFirstMatchedSelector,
    getLastRuleWithMatchingSelector,
    isSelectorEqual,
    doesRuleHaveMatchingSelector,
    getMatchedRules,
    DOMHelpers,
    getMatchedElements,
    matchElements: matchElement,
    matchAnySelector: matchAnySelector,
}, types), "css");