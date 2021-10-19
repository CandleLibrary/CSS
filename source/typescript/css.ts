
import { property_definitions, media_feature_definitions } from "./properties/property_and_type_definitions.js";
import { getPropertyParser } from "./properties/construct_property_parser.js";
import parsePropertyDefinitionFromHydrocarbon, { parseProperty } from "./properties/parse_property_value.js";

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
import { CSS_Gradient } from "./types/gradient.js";
import { CSS_Transform2D, CSS_Transform3D } from "./types/transform.js";
import CSS_Path from "./types/path.js";
import CSS_FontName from "./types/font_name.js";
import CSS_Rectangle from "./types/rectangle.js";

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
    getArrayOfMatchedRules,
    matchAnySelector
} from "./selector/utilities.js";

import { selector, properties, parse, property, rule } from "./parser/parse.js";
import { CSSNodeTypeLU } from "./types/node_type_lu.js";
import { CSSNodeDefinitions } from "./render/rules.js";
import { PrecedenceFlags } from "./types/precedence_flags.js";
import { renderCompressed } from './render/render.js';
export { css_mappings } from './render/mappings.js';
export * from "./parser/parse.js";



const newRule = function (): CSSRuleNode {
    return <CSSRuleNode>{
        selectors: [],
        props: new Map,
        type: CSSNodeType.Rule,
        pos: null,
    };
};


const parent_types = new Set(
    [
        CSSNodeType.Stylesheet,
        CSSNodeType.Keyframes,
        CSSNodeType.Media,
    ]
);
export function attachParents(node: CSSNode): CSSNode {
    if (
        parent_types.has(node.type)
    ) {
        for (const child_node of node.nodes) {
            child_node.parent = node;
            attachParents(child_node);
        }
    }

    return node;
}

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

function getNodeHash(node: CSSNode): string {
    switch (node.type) {
        case CSSNodeType.Keyframes:
            return "@k-" + hashString(renderCompressed(node.name)).toString(16);
        case CSSNodeType.Media:
            return "@m-" + hashString(renderCompressed(node.nodes[0])).toString(16);
        case CSSNodeType.Supports:
            return "@s-" + hashString(renderCompressed(node.nodes[0])).toString(16);
        case CSSNodeType.Rule:

            let hash = 0n;

            for (const sel of node.selectors)
                hash = getSelectorHash(sel, hash);

            return "#r-" + hash.toString(16);
    }
}

export function selectMatchingRule(rule_path: string, input_node: CSSNode): CSSNode {
    const segments = rule_path.split("/").map(s => {
        const segment_header = s.slice(0, 3);
        const type = segment_header[1];
        return [{
            "k": CSSNodeType.Keyframes,
            "m": CSSNodeType.Media,
            "s": CSSNodeType.Supports,
            "r": CSSNodeType.Rule,
        }[type], s];
    });

    let out_node = null;

    outer: for (const [type, hash] of segments) {

        for (const node of input_node.nodes) {
            if (node.type == type && getNodeHash(node) == hash) {
                out_node = node;
                continue outer;
            }
        }

        out_node = null;
        break;
    }

    return out_node;
}

/**
 * The path to a rule
 * 
 * //@rule/.../rule_selector_hashes
 */
export function createRulePath(rule: CSSRuleNode) {

    const string = [];

    let node = rule;

    while (node) {
        if (node.type == CSSNodeType.Stylesheet) {
            break;
        } else {
            string.push(getNodeHash(node));
        }
        node = node.parent;
    }

    return string.reverse().join("/");
}

export function matchAll<Element>(selector_string, ele, helpers: SelectionHelpers<Element>): Element[] {
    const selector_node = selector(selector_string);
    return [...getMatchedElements<Element>(ele, selector_node, helpers)];
}

function hashString(string: string, hash: bigint = 0n): bigint {

    let i = string.length - 1;

    if (i < 0)
        return hash;

    do {
        const code = BigInt(string.charCodeAt(i));

        hash = (hash << 5n) - hash + code;

        hash &= 0xFFFFFFFFFFFFFFn;

    } while (i--);

    return hash;
}

export function getAtHash(selector: CSSRuleNode, hash: bigint = 0n): bigint {
    return hashString(renderCompressed(selector), hash);
}

export function getSelectorHash(selector: CSSRuleNode, hash: bigint = 0n): bigint {
    return hashString(renderCompressed(selector), hash);
}

/**
 * Merges properties and selectors from an array of rules into a single,
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

    const selectors_set = new Set();

    const prop_set = new Set();

    const props = rules.flatMap(r => [...r.props.values()])
        .reverse()
        .filter(prop => {
            if (prop_set.has(prop.name) && !prop.IMPORTANT) {
                return false;
            }
            prop_set.add(prop.name);
            return true;
        });
    const selectors = rules.flatMap(r => r.selectors);

    for (const prop of props.reverse())
        new_rule.props.set(prop.name, prop);

    for (const selector of selectors) {
        const hash = getSelectorHash(selector);
        if (!selectors_set.has(hash)) {
            selectors_set.add(hash);
            new_rule.selectors.push(selector);
        }
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
export * from "./render/mappings.js";
export * from "./render/render.js";
export * from "./render/rules.js";
export * from "./selector/utilities.js";
//import * as utilities from "./selector/utilities.js";
export {
    //object types
    CSSProperty,
    CSS_Length as length,
    CSS_Percentage as percentage,
    CSS_URL,
    CSS_URL as url,
    CSS_Color,
    CSS_Length,
    CSS_Percentage,
    CSS_Id,
    CSS_String,
    CSS_Shape,
    CSS_Bezier,
    CSS_Gradient,
    CSS_Path,
    CSS_FontName,
    CSS_Rectangle,
    CSS_Number,
    CSS_Transform2D,
    CSS_Transform3D,
    terms,
    productions,
    property_definitions,
    media_feature_definitions,
    CSSNodeTypeLU,
    CSSNodeDefinitions,

    //pure types
    CSSNodeType,
    CSSNode,
    CSSRuleNode,
    PrecedenceFlags,


    //functions
    removeRule,
    renderProps,
    parsePropertyDefinitionFromHydrocarbon as parseDeclaration,
    parseProperty,
    getPropertyParser
};

