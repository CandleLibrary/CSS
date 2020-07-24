import { addModuleToCFW } from "@candlefw/candle";
import { Lexer } from "@candlefw/wind";

import { lrParse, ParserData, ParserEnvironment } from "@candlefw/hydrocarbon/build/library/runtime.js";

import parser_data from "./parser/css.js";

import { property_definitions, media_feature_definitions } from "./properties/property_and_type_definitions.js";
import { getPropertyParser } from "./properties/parser.js";
import * as productions from "./properties/productions.js";
import * as terms from "./properties/terms.js";
import parseDeclaration from "./properties/parse_declaration.js";

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

import { CSSTreeNodeType, render, CSSTreeNode, CSSRuleNode } from "./nodes/css_tree_node_type.js";
import {
    getMatchedElements, SelectionHelpers, matchElement, DOMHelpers,
    isSelectorEqual,
    doesRuleHaveMatchingSelector
} from "./selector/lookup_nodes.js";
import { property } from "./properties/property.js";

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

type CSSParserEnvironment = ParserEnvironment & {};

const env = <CSSParserEnvironment>{
    typ: CSSTreeNodeType,
    functions: {
        parseDeclaration,
        url: CSS_URL,
        percentage: CSS_Percentage,
        length: CSS_Length
    }
};

const parse = function (string_data): CSSTreeNode {

    let lex: Lexer = null;

    if (typeof string_data == "string")
        lex = new Lexer(string_data);
    else
        lex = string_data;

    const parse_result = lrParse<CSSTreeNode>(lex, <ParserData>parser_data, env);

    if (parse_result.error)
        throw new SyntaxError(parse_result.error);

    const node = parse_result.value;

    node.toString = () => render(node);

    return parse_result.value;
};

const properties = function (props): Map<string, property> {
    const css = parse(`*{${props}}`);
    return (<CSSRuleNode>css.nodes[0]).props;
};

const selector = function (selector): CSSTreeNode {
    const css = parse(`${selector}{top:0}`);
    return (<CSSRuleNode>css.nodes[0]).selectors[0];
};

const rule = function (rule: string = "*{display:block}"): CSSTreeNode {
    const css = parse(rule);
    return css.nodes[0];
};

const newRule = function (): CSSRuleNode {
    return <CSSRuleNode>{
        selectors: [],
        props: new Map,
        type: CSSTreeNodeType.Rule,
        pos: null,
    };
};

export function matchAll<Element>(selector_string, ele, helpers: SelectionHelpers<Element>): Element[] {
    const selector_node = selector(selector_string);
    return [...getMatchedElements<Element>(ele, selector_node, helpers)];
};

export function getApplicableRules(ele, css, helpers = DOMHelpers): CSSRuleNode[] {
    const rules = [];

    if (css.type == CSSTreeNodeType.Stylesheet) {
        for (const rule of css.nodes.filter(r => r.type == CSSTreeNodeType.Rule)) {
            for (const selector of rule.selectors) {
                if (matchElement(ele, selector, helpers)) {
                    rules.push(rule);
                    break;
                }
            }
        }
    }
    return rules;
}

/**
 * Merges properties and selectors from an array of rules into  a single,
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
        type: CSSTreeNodeType.Rule,
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

export {
    renderProps,
    parse,
    selector,
    CSS_Length as length,
    CSS_Length,
    CSS_Percentage as percentage,
    CSS_URL,
    CSS_URL as url,
    CSSTreeNodeType,
    CSSTreeNode,
    SelectionHelpers,
    parseDeclaration,
    types,
    property_definitions,
    media_feature_definitions,
    getPropertyParser,
    productions,
    terms,
    render,
    rule,
    isSelectorEqual,
    doesRuleHaveMatchingSelector
};

addModuleToCFW({
    isSelectorEqual,
    doesRuleHaveMatchingSelector,
    renderProps,
    getApplicableRules,
    DOMHelpers,
    getMatchedElements,
    matchElements: matchElement,
    parse,
    selector,
    CSS_URL,
    CSSTreeNodeType,
    parseDeclaration,
    types,
    property_definitions,
    media_feature_definitions,
    getPropertyParser,
    productions,
    terms,
    matchAll,
    render,
    rule,
    properties,
    addPropsToRule,
    mergeRulesIntoOne,
    newRule
}, "css");