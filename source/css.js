import whind from "@candlefw/whind";
import css_parser from "./Parser/css.js";
import { types } from "./properties/property_and_type_definitions";
import ui_stylesheet from "./ui/ui_stylesheet.js";
import ui_stylerule from "./ui/ui_stylerule.js"
import ui_styleprop from "./ui/ui_property.js"
import stylesheet from "./stylesheet.js"
import ruleset from "./ruleset.js"
import stylerule from "./stylerule.js"
import styleprop from "./styleprop.js"
import compoundSelector from "./selectors/compound.js"
import comboSelector from "./selectors/combo.js"
import selector from "./selectors/selector.js"
import typeselector from "./selectors/typeselector.js"
import idSelector from "./selectors/id.js"
import classSelector from "./selectors/class.js"
import attribSelector from "./selectors/attribute.js"
import pseudoClassSelector from "./selectors/pseudo_class.js"
import pseudoElementSelector from "./selectors/pseudo_element.js"
import parseDeclaration from "./properties/parse_declaration.js"
import CSS_Length from "./types/length.js";
import CSS_URL from "./types/url.js";

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
const ui = function(css_obj) { 
    if (css_obj.css_type == "stylesheet") return new ui_stylesheet(css_obj);  
    if (css_obj.css_type == "styleprop") return new ui_styleprop(css_obj);  
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
