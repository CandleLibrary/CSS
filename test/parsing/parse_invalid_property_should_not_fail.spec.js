/**[API]:testing
 *  
 * Parsing an invalid property should not fail.
 */


import { rule, CSSNodeTypeLU } from "@candlefw/css";

const parsed_rule = rule(`
    .invalid-prop-name {
        position:relative;
        position:block;
        invalid:name 22;
        --new-property: 22px;
    }
`);

assert(parsed_rule.selectors[0].type == CSSNodeTypeLU.IdSelector);
assert(parsed_rule.props.size == 0);