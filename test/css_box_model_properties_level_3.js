/*** 

	Tests for parsing of CSS level 3 box model properties https://drafts.csswg.org/css-box-3/

***/

import {
    textSpread,
    checkText,
    checkArray,
    checkColor,
    checkLength,
    checkPercentage,
    checkNumber,
    checkURL,
    test,
    px,
    mm,
    cm,
    _in,
    pc,
    pt,
    ch,
    em,
    ex,
    rem,
    vh,
    vw,
    vmin,
    vmax,
    deg,
    message,
    inherit
} from "./test_tools.js"

const color = checkColor;
const text = checkText;
const url = checkURL;

describe("CSS Level 3 Box Model Properties https://drafts.csswg.org/css-box-3/", () => {
    textSpread("margin-top",...inherit)
    textSpread("margin-right",...inherit)
    textSpread("margin-bottom",...inherit)
    textSpread("margin-left",...inherit)

    textSpread("margin-trim","none", "in-flow", "all", ...inherit)

    textSpread("padding-top",...inherit)
    textSpread("padding-right",...inherit)
    textSpread("padding-bottom",...inherit)
    textSpread("padding-left",...inherit)

})
