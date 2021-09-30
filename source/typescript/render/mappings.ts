import { CSSNodeType } from "../types/node_type.js";
import { CSSNode } from "../types/node";
import { buildRenderers, buildFormatRules, FormatRule as $, NodeMappings } from "@candlelib/conflagrate";
import { CSSNodeTypeLU } from "../types/node_type_lu.js";

export const css_mappings: NodeMappings<CSSNode, "type"> = <NodeMappings<CSSNode, "type">>{
    typename: "type",
    type_lookup: () => 0,
    mappings: [
        {
            type: CSSNodeType.Stylesheet,
            template: " i:s @nodes...[ m:n] i:e",
        },
        {
            type: CSSNodeType.Rule,
            template: "@selectors...[ \\, o:n  ] \\{ i:s i:e \\}",
        },
        {
            type: CSSNodeType.Import,
            template: "\\@import @nodes[0] @nodes[1]? @nodes...",
        },
        {
            type: CSSNodeType.Keyframes,
            template: "\\@keyframes @name \\{ i:s   @nodes[1]   i:e \\}",
        },
        {
            type: CSSNodeType.KeyframeBlock,
            template: "@nodes[0]  \\{ i:s @nodes[1]; i:e \\}",
        },
        {
            type: CSSNodeType.KeyframeSelectors,
            template: "@nodes...[\\, ]",
        },
        {
            type: CSSNodeType.KeyframeSelector,
            template: "@val",
        },
        {
            type: CSSNodeType.SupportConditions,
            template: "",
        },
        {
            type: CSSNodeType.Supports,
            template: "",
        },
        {
            type: CSSNodeType.Not,
            template: "\\not @nodes[0]",
        },
        {
            type: CSSNodeType.And,
            template: "\\and @nodes[0]",
        },
        {
            type: CSSNodeType.Or,
            template: "\\or @nodes[0]",
        },
        {
            type: CSSNodeType.Parenthesis,
            template: "\\( @nodes[0] \\)",
        },
        {
            type: CSSNodeType.Function,
            template: "",
        },
        {
            type: CSSNodeType.MediaQueries,
            template: "@nodes...[ \\, ]",
        },
        {
            type: CSSNodeType.Media,
            template: "@media @nodes[0] \\{ i:s @nodes...\n i:e \\}",
        },
        {
            type: CSSNodeType.Query,
            template: "@nodes...[ o:s ]",
        },
        {
            type: CSSNodeType.MediaFeature,
            template: "\\( @nodes[0] \\)",
        },
        {
            type: CSSNodeType.MediaFunction,
            template: "",
        },
        {
            type: CSSNodeType.MediaValue,
            template: "@key \\: @val",
        },
        {
            type: CSSNodeType.MediaType,
            template: "@val",
        },
        {
            type: CSSNodeType.MediaEquality,
            template: "@left o:s @sym o:s @right",
        },
        {
            type: CSSNodeType.MediaRangeAscending,
            template: "",
        },
        {
            type: CSSNodeType.MediaRangeDescending,
            template: "@max @s",
        },
        {
            type: CSSNodeType.ComplexSelector,
            template: "@nodes... ",
        },
        {
            type: CSSNodeType.CompoundSelector, template: {
                default: "@nodes... ",
                combinator: "\\@ @nodes... "
            }
        },
        {
            type: CSSNodeType.Combinator,
            template: "@val",
        },
        {
            type: CSSNodeType.PseudoSelector,
            template: "@nodes[0] @nodes... ",
        },
        {
            type: CSSNodeType.MetaSelector,
            template: "@nodes[0]? \\* ",
        },
        {
            type: CSSNodeType.NamespacePrefix,
            template: "\\ @vals \\|",
        },
        {
            type: CSSNodeType.QualifiedName,
            template: { ns: "@ns|@val", default: "@val" },
        },
        {
            type: CSSNodeType.IdSelector,
            template: "\\# @val",
        },
        {
            type: CSSNodeType.ClassSelector,
            template: "\\. @val",
        },
        {
            type: CSSNodeType.AttributeSelector,
            template: "\\[ @nodes[0] @match_type? @match_val? @mod?\\]",
        },
        {
            type: CSSNodeType.PseudoClassSelector,
            template: "\\: @id {val : \\( @val \\) }",
        },
        {
            type: CSSNodeType.PseudoElementSelector,
            template: "\\:: @id {val : \\( @val \\) }",
        },
        {
            type: CSSNodeType.TypeSelector,
            template: "@nodes[0]",
        },
    ]
};

const lu_table = new Map(css_mappings.mappings.map((i, j) => [i.type, j]));

css_mappings.type_lookup = (node, name) => lu_table.get(node.type) || -1;

export const renderers = buildRenderers<CSSNode>(CSSNodeDefinitions, CSSNodeTypeLU);
