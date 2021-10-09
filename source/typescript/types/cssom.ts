/**
 * Candle Library TS partial implementation of the CSS Object Model
 * https://drafts.csswg.org/cssom/#css-style-sheets
 */

import URI from '@candlelib/uri';
import { CSSNodeType } from './node_type';

export interface CSSOM_NODE {
    type: CSSNodeType;
}



export interface CSSOM_Media extends CSSOM_NODE {
    queries: CSSOM_MediaQueries[];
}

export type CSSOM_Selector = null;

export interface CSSOM_StyleDeclaration {
    properties: any;
}

export interface CSSOM_StyleRule extends CSSOM_NODE {

    type: CSSNodeType.Rule;
    selector: CSSOM_Selector;
    style: CSSOM_StyleDeclaration;

}

export interface CSSOM_ImportRule extends CSSOM_NODE {

    type: CSSNodeType.Import;

}

export interface CSSOM_MediaRule extends CSSOM_NODE {
    type: CSSNodeType.Media;

}

export interface CSSOM_FontFaceRule extends CSSOM_NODE {
    type: CSSNodeType.FontFace;

}

export interface CSSOM_KeyFramesRule extends CSSOM_NODE {
    type: CSSNodeType.Keyframes;

}

export interface CSSOM_KeyFramesRule extends CSSOM_NODE {
    type: CSSNodeType.Keyframes;

}

export interface CSSOM_MediaQueries extends CSSOM_NODE {

}





export type CSSOM = null;
export type CSSOM_Rule = CSSOM_StyleRule | CSSOM_ImportRule | CSSOM_MediaRule;

export interface CSSOM_StyleSheet extends CSSOM_NODE {
    type: CSSNodeType.Stylesheet;
    href?: URI;
    media: CSSOM_Media[];
    cssRules: CSSOM_Rule[];
    disabled: boolean;
}