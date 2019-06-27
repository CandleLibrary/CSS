import whind from "@candlefw/whind";
import {
    property_definitions,
    media_feature_definitions
} from "./property_and_type_definitions";
import { types } from "./property_and_type_definitions";
import { getPropertyParser } from "./parser";
import styleprop from "../styleprop.mjs";

export default function parseDeclaration(sym) {
    if(sym.length == 0)
        return null;

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
