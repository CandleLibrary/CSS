import { Lexer } from "@candlefw/wind";
import { CSSProperty } from "../properties/property.js";
import { CSSNodeType } from "./node_type.js";
import { PrecedenceFlags } from "../css.js";

export interface CSSNode {
    type: CSSNodeType;
    nodes?: CSSNode[];
    selectors?: CSSNode[];
    pos?: Lexer;

    /**
     * The calculated precedence of the node.
     */
    precedence?: PrecedenceFlags;
};

export interface CSSRuleNode extends CSSNode {
    selectors?: CSSNode[];
    props?: Map<string, CSSProperty>;
}

export interface CSSSelectorNode extends CSSNode {
    val?: string,
    id?: string,

    ns?: string;

    match_type?: string;

    match_val?: string;

    mod?: string;
}
