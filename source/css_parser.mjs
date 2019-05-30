import ll from "@candlefw/ll";
import whind from "@candlefw/whind";
import css_parser from "./Parser/css.mjs";

import { CSSRule } from "./rule.mjs";
import { CSSSelector } from "./selector.mjs";
import {
    property_definitions,
    media_feature_definitions
} from "./properties/property_and_type_definitions";
import { types } from "./properties/property_and_type_definitions";

import { getPropertyParser } from "./properties/parser";

function parseProperty(lexer, rule, definitions) {
    const name = lexer.tx.replace(/\-/g, "_");

    //Catch any comments
    if (lexer.ch == "/") {
        lexer.comment(true);
        let bool = parseProperty(lexer, rule, definitions);
        return
    }
    lexer.next().a(":");
    //allow for short circuit < | > | =
    const p = lexer.pk;
    while ((p.ch !== "}" && p.ch !== ";") && !p.END) {
        //look for end of property;
        p.next();
    }
    const out_lex = lexer.copy();
    lexer.sync();
    out_lex.fence(p);
    if (!false /*this._getPropertyHook_(out_lex, name, rule)*/ ) {
        try {
            const IS_VIRTUAL = {
                is: false
            };
            const parser = getPropertyParser(name, IS_VIRTUAL, definitions);
            if (parser && !IS_VIRTUAL.is) {
                if (!rule.props) rule.props = {};
                parser.parse(out_lex, rule.props);
            } else
                //Need to know what properties have not been defined
                console.warn(`Unable to get parser for css property ${name}`);
        } catch (e) {
            console.log(e);
        }
    }
    if (lexer.ch == ";") lexer.next();
}
import compoundSelector from "./selectors/compound.mjs"
import comboSelector from "./selectors/combo.mjs"
import selector from "./selectors/selector.mjs"
import idSelector from "./selectors/id.mjs"
import classSelector from "./selectors/class.mjs"
import attribSelector from "./selectors/attribute.mjs"
import pseudoClassSelector from "./selectors/pseudo_class.mjs"
import pseudoElementSelector from "./selectors/pseudo_element.mjs"


const env = {
    functions: {
        compoundSelector,
        comboSelector,
        selector,
        idSelector,
        classSelector,
        attribSelector,
        pseudoClassSelector,
        pseudoElementSelector,
        parseDeclaration: function(sym, env, lex) {
            let rule_name = sym[0];
            let body_data = sym[2];
            let important = sym[3] ? true : false

            const IS_VIRTUAL = { is: false }
            const parser = getPropertyParser(rule_name.replace(/\-/g, "_"), IS_VIRTUAL, property_definitions);

            if (parser && !IS_VIRTUAL.is) {

                const prop = parser.parse(whind(body_data));

                if (prop.length > 0)
                    return { name: rule_name, val: prop, original: body_data };

            } else
                //Need to know what properties have not been defined
                console.warn(`Unable to get parser for css property ${rule_name}`);

            return { name: rule_name, val: null, original: body_data };
        },
    },
    body: null
}

export default function parse(string_data) {
    try {
        const nodes = css_parser(whind(string_data), env);

        for (const node of nodes) {

            let selectors = node.selectors;


            selectors.forEach(sel_array => {

                let element = document.getElementById("test"),
                    match = { match: true };

                if (sel_array[0].matchBU(element, sel_array) !== null) {
                    element.style.backgroundColor = "red";
                } else {
                    element.style.backgroundColor = "blue";
                }
            })
        }
        console.log(nodes);
    } catch (e) {
        console.error(e);
    }
}
