import observer from "@candlefw/observer";
import wind from "@candlefw/wind";
import {
    property_definitions
} from "./property_and_type_definitions.js";
import { getPropertyParser } from "./parser.js";
import { property } from "./property.js";

/* 
    Parses a string value of a css property. Returns result of parse or null.

    Arg - Array - An array with values:
        0 :  string name of css rule that should be used to parse the value string.
        1 :  string value of the css rule.
        2 :  BOOL value for the presence of the "important" value in the original string. 

    Returns object containing:
        rule_name : the string name of the rule used to parse the value.
        body_string : the original string value
        prop : An array of CSS type instances that are the parsed values.
        important : boolean value indicating the presence of "important" value.
*/

observer("updatedCSSStyleProperty", property.prototype);

export default function parseDeclaration(sym, a, b, pos) {


    if (sym.length == 0)
        return null;

    let prop = null;

    const
        rule_name = sym[0],
        body_string = sym[2],
        important = sym[3] ? true : false,
        IS_VIRTUAL = { is: false },
        parser = getPropertyParser(rule_name.replace(/\-/g, "_"), IS_VIRTUAL, property_definitions);

    if (parser && !IS_VIRTUAL.is)

        prop = parser.parse(wind(body_string));

    else
        //Need to know what properties have not been defined
        console.warn(`Unable to get parser for CSS property ${rule_name}`);


    if (prop)
        return new property(rule_name, body_string, prop, important, pos);

    return null;
}
