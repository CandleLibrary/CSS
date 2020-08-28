import wind from "@candlefw/wind";
import {
    property_definitions
} from "./property_and_type_definitions.js";
import { getPropertyParser } from "./parser.js";
import { CSSProperty } from "./property.js";
import { CSS_String } from "../css.js";

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

export default function parsePropertyDefinitionFromHydrocarbon(sym: { 0: string, 2: string, 3: boolean; length: number; }, a, b, pos): CSSProperty {


    if (sym.length == 0)
        return null;

    let prop = null;


    const
        rule_name = sym[0],
        body_string = sym[2].trim(),
        important = sym[3] ? true : false,
        IS_VIRTUAL = { is: false };


    const parser = getPropertyParser(rule_name.replace(/\-/g, "_"), IS_VIRTUAL, property_definitions);

    if (parser && !IS_VIRTUAL.is) {
        if (body_string == "unset" || body_string == "inherit" || body_string == "initial")
            return new CSSProperty(rule_name, body_string, [new CSS_String(body_string)], important, pos);


        prop = parser.parse(wind(body_string));

    } else
        //Need to know what properties have not been defined
        console.warn(`Unable to get parser for CSS property ${rule_name}`);


    if (prop)
        return new CSSProperty(rule_name, body_string, prop, important, pos);

    return null;
}

export function parseProperty(name: string, value: string, important: boolean): CSSProperty {
    return parsePropertyDefinitionFromHydrocarbon([name, , value, important], undefined, undefined, undefined);
}
