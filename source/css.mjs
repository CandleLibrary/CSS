import whind from "@candlefw/whind";
import css_parser from "./Parser/css.mjs";
import { types } from "./properties/property_and_type_definitions";
import ui_stylesheet from "./ui/builder.mjs";
import ui_stylerule from "./ui/ui_stylerule.mjs"
import ui_styleprop from "./ui/ui_properties.mjs"
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
const ui = function(css) { 
    if (css instanceof stylesheet) return new ui_stylesheet(css);  
    if (css instanceof styleprop) return new ui_styleprop(css);  
}

export {
    css_parser,
    parse,
    ui,
    CSS_Length as length,
    CSS_Length,
    CSS_URL,
    CSS_URL as url,
    ui_stylesheet,
    ui_stylerule,
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
