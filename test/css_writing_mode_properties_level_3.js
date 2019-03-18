/*** 

	Tests for parsing of CSS level 1 flexbox properties https://www.w3.org/TR/css-flexbox-1/

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

describe("CSS Level 3 Writing Modes Properties https://drafts.csswg.org/css-writing-modes-3/", () => {
    textSpread("direction", "ltr", "rtl", ...inherit)
    textSpread("unicode-bidi", "normal", "embed", "isolate", "bidi-override", "isolate-override", "plaintext", ...inherit)
    textSpread("writing-mode", "horizontal-tb", "vertical-rl", "vertical-lr", ...inherit)
    textSpread("text-orientation", "mixed", "upright", "sideways", ...inherit)
    //test.only();
    textSpread("glyph-orientation-vertical", "auto", "0deg", "90deg", "0", "90", ...inherit)
    textSpread("text-combine-upright", "none", "all", ...inherit)
})
