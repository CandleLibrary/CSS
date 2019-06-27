import ll from "@candlefw/ll";
import whind from "@candlefw/whind";
import css_parser from "./Parser/css.mjs";
import {
    property_definitions,
    media_feature_definitions
} from "./properties/property_and_type_definitions";
import { types } from "./properties/property_and_type_definitions";
import UIMaster from "./ui/builder.mjs";
import UIRuleSet from "./ui/ui_ruleset.mjs"

import { getPropertyParser } from "./properties/parser";

import stylesheet from "./stylesheet.mjs"
import ruleset from "./ruleset.mjs"
import stylerule from "./stylerule.mjs"
import styleprop from "./styleprop.mjs"
import compoundSelector from "./selectors/compound.mjs"
import comboSelector from "./selectors/combo.mjs"
import selector from "./selectors/selector.mjs"
import typeselector from "./selectors/typeselector.mjs"
import idSelector from "./selectors/id.mjs"
import classSelector from "./selectors/class.mjs"
import attribSelector from "./selectors/attribute.mjs"
import pseudoClassSelector from "./selectors/pseudo_class.mjs"
import pseudoElementSelector from "./selectors/pseudo_element.mjs"
import parseDeclaration from "./properties/parse_declaration.mjs"
import CSS_Length from "./types/length.mjs";
import CSS_URL from "./types/url.mjs";

const env = {
    functions: {
        compoundSelector,
        comboSelector,
        typeselector,
        selector,
        idSelector,
        classSelector,
        attribSelector,
        pseudoClassSelector,
        pseudoElementSelector,
        parseDeclaration,
        stylerule,
        ruleset,
        stylesheet,
    },
    body: null
}

const parse = function (string_data) { return css_parser(whind(string_data), env) }
const ui = function(css) { if (css instanceof stylesheet) { return new UIMaster(css); } }

parse.types = types;

export {
    css_parser,
    parse,
    ui,
    CSS_Length,
    CSS_URL,
    UIMaster,
    UIRuleSet,
    stylerule,
    ruleset,
    compoundSelector,
    comboSelector,
    typeselector,
    selector,
    idSelector,
    classSelector,
    attribSelector,
    pseudoClassSelector,
    pseudoElementSelector,
    parseDeclaration,
    stylesheet,
    types
}