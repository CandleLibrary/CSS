import whind from "@candlefw/whind";
import { CSSRule } from "./rule.mjs";
import { CSSSelector } from "./selector.mjs";
import {
    property_definitions,
    media_feature_definitions
} from "./property_and_type_definitions";
import { types } from "./property_and_type_definitions";
import { CSSRuleBody } from "./body";
import UIMaster from "./ui/builder.mjs";
import UIRuleSet from "./ui/ui_ruleset.mjs"
import { getPropertyParser } from "./parser";

function parseDeclaration(sym) {
    let rule_name = sym[0];
    let body_data = sym[2];
    let important = sym[3] ? true : false;
    let prop = null;

    const IS_VIRTUAL = { is: false }
    const parser = getPropertyParser(rule_name.replace(/\-/g, "_"), IS_VIRTUAL, property_definitions);

    if (parser && !IS_VIRTUAL.is) {

        prop = parser.parse(whind(body_data).useExtendedId());

    } else
        //Need to know what properties have not been defined
        console.warn(`Unable to get parser for css property ${rule_name}`);

    return new styleprop(rule_name, body_data, prop);
}
