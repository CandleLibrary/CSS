import { Lexer } from "@candlefw/wind";
import { CSSProperty } from "../properties/property.js";
import { CSSNodeType } from "./node_type.js";

export interface CSSNode {
    type: CSSNodeType;
    nodes?: CSSNode[];

    selectors?: CSSNode[];

    pos?: Lexer;
}
;

export interface CSSRuleNode extends CSSNode {
    selectors?: CSSNode[];
    props?: Map<string, CSSProperty>;
}
