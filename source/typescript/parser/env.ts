import { ParserEnvironment } from "@candlelib/hydrocarbon/build/library/runtime.js";
import CSS_Length from "../types/length.js";
import CSS_URL from "../types/url.js";
import CSS_Percentage from "../types/percentage.js";
import parseDeclaration from "../properties/parse_property_value.js";
import { CSSNodeTypeLU } from "../types/node_type_lu.js";
type CSSParserEnvironment = ParserEnvironment & {};
const env = <CSSParserEnvironment>{
    typ: CSSNodeTypeLU,
    functions: {
        parseDeclaration,
        url: CSS_URL,
        percentage: CSS_Percentage,
        length: CSS_Length
    }
};

env.fn = env.functions;

export default env;
