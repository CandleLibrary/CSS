import { CSSNode, CSSRuleNode } from "../types/node";
import { CSSProperty } from "../properties/property.js";
import { renderWithFormatting } from "../render/render.js";
import env from "./env.js";
import parser from "./parser.js";

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
    const { FAILED, result, error_message } = parser(string_data, env);

    if (FAILED)
        throw new SyntaxError(error_message);

    const node = result[0];

    node.toString = () => renderWithFormatting(node);

    return node;
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
