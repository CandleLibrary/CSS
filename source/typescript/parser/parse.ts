import { Lexer } from "@candlefw/wind";
import { lrParse, ParserData } from "@candlefw/hydrocarbon/build/library/runtime.js";
import parser_data from "./css.js";
import { CSSNode, CSSRuleNode } from "../types/node";
import { CSSProperty } from "../properties/property.js";
import { renderWithFormatting } from "../render/render.js";
import env from "./env.js";
import parser_loader from "./parser.js";

const parser = await parser_loader();
export const parse = function (string_data): CSSNode {
    /*


    let lex: Lexer = null;

    if (typeof string_data == "string")
        lex = new Lexer(string_data);
    else
        lex = string_data;

    const parse_result = lrParse<CSSNode>(lex, <ParserData>parser_data, env);

    if (parse_result.error)
        throw new SyntaxError(parse_result.error);

    const node = parse_result.value;

    node.toString = () => renderWithFormatting(node);

    return parse_result.value;
    /*/
    const parse_result = parser(string_data, env);

    if (parse_result.FAILED)
        throw new SyntaxError(parse_result.FAILED);

    const node = parse_result.result[0];

    node.toString = () => renderWithFormatting(node);

    return parse_result.result[0];
    //*/
};

export const properties = function (props): Map<string, CSSProperty> {
    const css = parse(`*{${props}}`);
    return (<CSSRuleNode>css.nodes[0]).props;
};

export const property = function (prop): CSSProperty {
    const css = parse(`*{${prop}}`);
    return [...(<CSSRuleNode>css.nodes[0]).props.values()][0];
};

export const selector = function (selector): CSSNode {
    const css = parse(`${selector} {top:0}`);
    return (<CSSRuleNode>css.nodes[0]).selectors[0];
};

export const rule = function (rule: string = "*{display:block}"): CSSNode {
    const css = parse(rule);
    return css.nodes[0];
};
