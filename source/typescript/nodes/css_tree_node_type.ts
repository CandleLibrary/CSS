import {
    buildRenderers as CFLbuildRenderers,
    renderCompressed as CFLrenderCompressed,
    renderWithFormatting as CFLrenderWithFormatting,
    renderWithSourceMap as CFLrenderWithSourceMap,
    renderWithFormattingAndSourceMap as CFLrenderWithFormattingAndSourceMap,
    FormatRule
} from "@candlefw/conflagrate";
import { Lexer } from "@candlefw/wind";
import { CSSProperty } from "../properties/property";

export interface CSSTreeNode {
    type: CSSTreeNodeType,
    nodes?: CSSTreeNode[];

    selectors?: CSSTreeNode[];

    pos?: Lexer;
    //Property Values
};

export interface CSSRuleNode extends CSSTreeNode {
    selectors?: CSSTreeNode[];
    props?: Map<string, CSSProperty>;
}

export enum CSSTreeNodeType {
    Stylesheet = (256 << 23),
    Rule = (257 << 23),
    Import = (258 << 23),
    Keyframes = (259 << 23),
    KeyframeBlock = (260 << 23),
    KeyframeSelector = (261 << 23),
    SupportConditions = (262 << 23),
    Supports = (263 << 23),
    Not = (264 << 23),
    And = (265 << 23),
    Or = (266 << 23),
    Parenthesis = (267 << 23),
    Function = (268 << 23),
    MediaQueries = (269 << 23),
    Media = (270 << 23),
    Query = (271 << 23),
    MediaFeature = (273 << 23),
    MediaFunction = (274 << 23),
    MediaValue = (275 << 23),
    MediaType = (292 << 23),
    MediaEquality = (276 << 23),
    MediaRangeAscending = (277 << 23),
    MediaRangeDescending = (278 << 23),
    ComboSelector = (279 << 23),
    ComplexSelector = (280 << 23),
    PseudoSelector = (281 << 23),
    CompoundSelector = (282 << 23),
    TypeSelector = (283 << 23),
    MetaSelector = (284 << 23),
    NamespacePrefix = (285 << 23),
    QualifiedName = (286 << 23),
    IdSelector = (287 << 23),
    ClassSelector = (288 << 23),
    AttributeSelector = (289 << 23),
    PseudoClassSelector = (290 << 23),
    PseudoElementSelector = (291 << 23),
    KeyframeSelectors = (293 << 23),
}

export const CSSTreeNodeDefinitions = [

    {
        type: CSSTreeNodeType.Stylesheet,
        template_pattern: "^1@...\n^0",
    },
    {
        type: CSSTreeNodeType.Rule,
        template_pattern: "@_selectors...,{}",

    },
    {
        type: CSSTreeNodeType.Import,
        template_pattern: "\@import @1 @2? @...",

    },
    {
        type: CSSTreeNodeType.Keyframes,
        template_pattern: "\@keyframes @name {^1%@2%^0}",

    },
    {
        type: CSSTreeNodeType.KeyframeBlock,
        template_pattern: "@1 ^1{^1@2;^0}^0",

    },
    {
        type: CSSTreeNodeType.KeyframeSelectors,
        template_pattern: "@...,",

    },
    {
        type: CSSTreeNodeType.KeyframeSelector,
        template_pattern: "@val",

    },
    {
        type: CSSTreeNodeType.SupportConditions,
        template_pattern: "",

    },
    {
        type: CSSTreeNodeType.Supports,
        template_pattern: "",

    },
    {
        type: CSSTreeNodeType.Not,
        template_pattern: "not @1",

    },
    {
        type: CSSTreeNodeType.And,
        template_pattern: "and @1",

    },
    {
        type: CSSTreeNodeType.Or,
        template_pattern: "or @1",

    },
    {
        type: CSSTreeNodeType.Parenthesis,
        template_pattern: "(@1)",

    },
    {
        type: CSSTreeNodeType.Function,
        template_pattern: "",

    },
    {
        type: CSSTreeNodeType.MediaQueries,
        template_pattern: "@... ",

    },
    {
        type: CSSTreeNodeType.Media,
        template_pattern: "@media @1%{^1@...\n^0}",

    },
    {
        type: CSSTreeNodeType.Query,
        template_pattern: "@... ",

    },
    {
        type: CSSTreeNodeType.MediaFeature,
        template_pattern: "(@1)",

    },
    {
        type: CSSTreeNodeType.MediaFunction,
        template_pattern: "",

    },
    {
        type: CSSTreeNodeType.MediaValue,
        template_pattern: "@key:@val",

    },
    {
        type: CSSTreeNodeType.MediaType,
        template_pattern: "@val",

    },
    {
        type: CSSTreeNodeType.MediaEquality,
        template_pattern: "",

    },
    {
        type: CSSTreeNodeType.MediaRangeAscending,
        template_pattern: "",

    },
    {
        type: CSSTreeNodeType.MediaRangeDescending,
        template_pattern: "@max @s",

    },
    {
        type: CSSTreeNodeType.ComplexSelector,
        template_pattern: "@... ",

    },
    {
        type: CSSTreeNodeType.CompoundSelector, template_pattern: {
            default: "@...%",
            combinator: "@combinator @...%"
        }

    },
    {
        type: CSSTreeNodeType.PseudoSelector,
        template_pattern: "@1",

    },
    {
        type: CSSTreeNodeType.MetaSelector,
        template_pattern: "@1?\*",

    },
    {
        type: CSSTreeNodeType.NamespacePrefix,
        template_pattern: "@vals|",

    },
    {
        type: CSSTreeNodeType.QualifiedName,
        template_pattern: { ns: "@ns|@val", default: "@val" },

    },
    {
        type: CSSTreeNodeType.IdSelector,
        template_pattern: "#@val",

    },
    {
        type: CSSTreeNodeType.ClassSelector,
        template_pattern: ".@val",

    },
    {
        type: CSSTreeNodeType.AttributeSelector,
        template_pattern: {
            default: "[%@1%]",
            mod: "[@1 @match_type @match_val @mod]",
            match_type: "[@1 @match_type @match_val]"

        }

    },
    {
        type: CSSTreeNodeType.PseudoClassSelector,
        template_pattern: {
            val: ":@id(@val)",
            default: ":@id"
        },

    },
    {
        type: CSSTreeNodeType.PseudoElementSelector,
        template_pattern: {
            val: "::@id(@val)",
            default: "::@id"
        }

    },
    {
        type: CSSTreeNodeType.TypeSelector,
        template_pattern: "@1",

    },
];


const definitions = CFLbuildRenderers(CSSTreeNodeDefinitions, CSSTreeNodeType);


export function render(
    node: CSSTreeNode,
    //format rules
    format_rules: FormatRule[] = []
): string {
    return CFLrenderWithFormatting<CSSTreeNode>(node, definitions, format_rules, (str, name, node): string => {
        if (node.type == CSSTreeNodeType.Rule && name !== "@full_render")
            return `{${Array.from((<CSSRuleNode>node).props.values()).map(n => n + "").join(";\n")}}`;
        return str;
    });
};