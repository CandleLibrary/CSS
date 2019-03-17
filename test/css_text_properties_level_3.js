/*** 

	Tests for parsing of CSS level 3 text properties https://www.w3.org/TR/2018/WD-css-text-3-20181212/

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

describe("CSS Level 3 Text Properties https://www.w3.org/TR/2018/WD-css-text-3-20181212/", () => {

    message("text-transform - see also CSS Level 1 Properties tests & CSS Level 2 Properties tests")
    textSpread("text-transform", "full-width", "full-size-kana", ...inherit);

    test.value = "text-transform: uppercase full-width full-size-kana"
    test.check = ["uppercase", "full-width", "full-size-kana"]

    message("text-transform - see also CSS Level 1 Properties tests & CSS Level 2 Properties tests")
    textSpread("text-transform", "break-spaces", ...inherit);

    test.value = "tab-size: 23px"
    test.check = px(23);

    test.value = "tab-size: 23"
    test.check = 23;

    textSpread("word-break", "normal", "keep-all", "break-all", ...inherit);

    textSpread("line-break", "auto", "loose", "normal", "strict", "anywhere", ...inherit);

    textSpread("hyphens", "none", "manual", "auto", ...inherit);

    textSpread("overflow-wrap", "normal", "break-word", "anywhere", ...inherit);

    textSpread("word-wrap", "normal", "break-word", "anywhere", ...inherit);

    textSpread("text-align", "justify", "justify-all", "start", "end",  ...inherit);

    textSpread("text-align-all", "end",  "left",  "right",  "center",  "justify",  "match-parent", ...inherit)

    textSpread("text-align-all", "start",  "end",  "left",  "right",  "center",  "justify",  "match-parent", ...inherit)

    textSpread("text-justify", "auto",  "none",  "inter-word",  "inter-character", ...inherit)

    message("word-spacing - see also CSS Level 1 Properties tests & CSS Level 2 Properties tests")
    textSpread("word-spacing",...inherit)

    message("letter-spacing - see also CSS Level 1 Properties tests & CSS Level 2 Properties tests")
    textSpread("letter-spacing",...inherit)

    message("text-indent - see also CSS Level 1 Properties tests & CSS Level 2 Properties tests")
    textSpread("text-indent", ...inherit)

    test.value = "text-indent: 23% hanging"
    test.check = [checkPercentage(23), "hanging"];

    test.value = "text-indent: 23% hanging each-line"
    test.check = [checkPercentage(23), "hanging", "each-line"];

    textSpread("hanging-punctuation", "none", "first", "force-end", "allow-end", "last", ...inherit);
    test.value = "hanging-punctuation:first allow-end last"
    test.check = ["first", "allow-end", "last"]
})
