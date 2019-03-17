/*** 

	Tests for parsing of CSS level 2.1 properties https://www.w3.org/TR/CSS2/

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
    message
} from "./test_tools.js"

const color = checkColor;
const text = checkText;
const url = checkURL;

describe("CSS Level 2.1 ", () => {
    describe("box-model: https://www.w3.org/TR/CSS2/box.html", () => {

        test.value = "margin-top:inherit"
        test.check = "inherit"

        test.value = "margin-right:inherit"
        test.check = "inherit"

        test.value = "margin-bottom:inherit"
        test.check = "inherit"

        test.value = "margin-left:inherit"
        test.check = "inherit"

        test.value = "margin:inherit"
        test.check = "inherit"

        test.value = "padding-top:inherit"
        test.check = "inherit"

        test.value = "padding-right:inherit"
        test.check = "inherit"

        test.value = "padding-bottom:inherit"
        test.check = "inherit"

        test.value = "padding-left:inherit"
        test.check = "inherit"

        test.value = "padding:inherit"
        test.check = "inherit"

    })

    describe("Vsiaul Formatting: https://www.w3.org/TR/CSS2/visuren.html", () => {
        textSpread("display",
            "inline",
            "block",
            "list-item",
            "inline-block",
            "table",
            "inline-table",
            "table-row-group",
            "table-header-group",
            "table-footer-group",
            "table-row",
            "table-column-group",
            "table-column",
            "table-cell",
            "table-caption",
            "none",
            "inherit"
        )

        textSpread("position",
            "static",
            "relative",
            "absolute",
            "fixed",
            "inherit"
        )

        textSpread("top",
            "auto",
            "inherit"
        )

        test.value = "top:20px"
        test.check = px(20)

        test.value = "top:20%"
        test.check = checkPercentage(20)

        textSpread("right",
            "auto",
            "inherit"
        )

        test.value = "right:20px"
        test.check = px(20)

        test.value = "right:20%"
        test.check = checkPercentage(20)

        textSpread("bottom",
            "auto",
            "inherit"
        )

        test.value = "bottom:20px"
        test.check = px(20)

        test.value = "bottom:20%"
        test.check = checkPercentage(20)

        textSpread("left",
            "auto",
            "inherit"
        )

        test.value = "left:20px"
        test.check = px(20)

        test.value = "left:20%"
        test.check = checkPercentage(20)

        textSpread("float",
            "left",
            "right",
            "none",
            "inherit"
        )

        textSpread("clear",
            "left",
            "right",
            "none",
            "both",
            "inherit"
        )

        textSpread("z-index",
            "auto",
            "inherit")

        test.value = "z-index:20"
        test.check = checkNumber(20);

        textSpread("direction",
            "ltr",
            "rtl",
            "inherit")

        textSpread("unicode-bidi",
            "normal",
            "embed",
            "bidi-override",
            "inherit")

        test.value = "width:auto"
        test.check = "auto";

        test.value = "width:inherit"
        test.check = "inherit";

        test.value = "width:20%"
        test.check = checkPercentage(20);

        test.value = "width:180vh"
        test.check = vh(180);

        test.value = "min-width:inherit"
        test.check = "inherit";

        test.value = "min-width:20%"
        test.check = checkPercentage(20);

        test.value = "min-width:180vh"
        test.check = vh(180);

        test.value = "max-width:none"
        test.check = "none";

        test.value = "max-width:inherit"
        test.check = "inherit";

        test.value = "max-width:20%"
        test.check = checkPercentage(20);

        test.value = "max-width:180vh"
        test.check = vh(180);

        test.value = "height:auto"
        test.check = "auto";

        test.value = "height:inherit"
        test.check = "inherit";

        test.value = "height:180vh"
        test.check = vh(180);

        test.value = "height:20%"
        test.check = checkPercentage(20);

        test.value = "min-height:inherit"
        test.check = "inherit";

        test.value = "min-height:180vh"
        test.check = vh(180);

        test.value = "min-height:20%"
        test.check = checkPercentage(20);

        test.value = "max-height:none"
        test.check = "none";

        test.value = "max-height:inherit"
        test.check = "inherit";

        test.value = "max-height:180vh"
        test.check = vh(180);

        test.value = "max-height:20%"
        test.check = checkPercentage(20);

        message("line-height", "see also CSS Level 1 Properties test")

        test.value = "line-height:inherit"
        test.check = "inherit";

        message("vertical-align", "see alse CSS Level 1 Properties test")

        test.value = "vertical-align:inherit"
        test.check = "inherit";

        test.value = "vertical-align:2px"
        test.check = px(2);
    })

    describe("Visual Effect: https://www.w3.org/TR/CSS2/visufx.html", function() {

        textSpread("overflow",
            "visible",
            "hidden",
            "scroll",
            "auto",
            "inherit"
        )

        textSpread("clip",
            "auto",
            "inherit"
        )

        test.value = "clip:rect(20vh,31em,auto,58)";
        test.check = "";

        textSpread("visibility",
            "visible",
            "hidden",
            "collapse",
            "inherit"
        )
    })

    describe("Generate: https://www.w3.org/TR/CSS2/generate.html", function() {
        textSpread("content",
            "normal",
            "none",
            "open-quote",
            "close-quote",
            "no-open-quote",
            "no-close-quote",
            "inherit"
        )

        test.value = "content:close-quote close-quote"
        test.check = ["close-quote", "close-quote"]

        test.value = "content:close-quote close-quote"
        test.check = ["close-quote", "close-quote"]

        test.value = "content:counter(section) ' ' counters(sub-section, ' ') 'test'"
        test.check = ""

        test.value = "content:attr('open') "
        test.check = "";


        textSpread("quotes",
            "none",
            "inherit"
        )

        test.value = "quotes: \"|\" \"|\" "
        test.check = ["|", "|"];

        textSpread("conter-reset",
            "none",
            "inherit"
        )

        test.value = "counter-reset:chapter section 1"
        test.check = ["chapter", "section", checkNumber(1)];

        textSpread("conter-reset",
            "none",
            "inherit"
        )

        test.value = "counter-increment:chapter section 22"
        test.check = ["chapter", "section", checkNumber(22)];

        textSpread(
            "list-style-type",
            "disc",
            "circle",
            "square",
            "decimal",
            "decimal-leading-zero",
            "lower-roman",
            "upper-roman",
            "lower-greek",
            "lower-latin",
            "upper-latin",
            "armenian",
            "georgian",
            "lower-alpha",
            "upper-alpha",
            "none",
            "inherit"
        );

        message("list-style-image", "see alse CSS Level 1 Properties test")

        test.value = "list-style-image:inherit";
        test.check = "inherit";

        message("list-style-position", "see alse CSS Level 1 Properties test")

        test.value = "list-style-position:inherit";
        test.check = "inherit";

        message("list-style", "see alse CSS Level 1 Properties test")

        test.value = "list-style:inherit";
        test.check = "inherit";
    })

    describe("Page: https://www.w3.org/TR/CSS2/page.html", function() {
        textSpread(
            "page-break-before",
            "auto",
            "always",
            "avoid",
            "left",
            "right",
            "inherit"
        );

        textSpread(
            "page-break-after",
            "auto",
            "always",
            "avoid",
            "left",
            "right",
            "inherit"
        );

        textSpread(
            "page-break-inside",
            "auto",
            "avoid",
            "inherit"
        );

        test.value = "orphans:inherit";
        test.check = "inherit";

        test.value = "orphans:88";
        test.check = 88;

        test.value = "widows:inherit";
        test.check = "inherit";

        test.value = "widows:88";
        test.check = 88;
    })

    describe("Colors and Backgrounds: https://www.w3.org/TR/CSS2/colors.html", function() {
        message("color", "see alse CSS Level 1 Properties test")
        textSpread("color", "inherit");

        message("background-color", "see alse CSS Level 1 Properties test")
        textSpread("background-color", "inherit");

        message("background-image", "see alse CSS Level 1 Properties test")
        textSpread("background-image", "inherit");

        message("background-repeat", "see alse CSS Level 1 Properties test")
        textSpread("background-repeat", "inherit");

        message("background-attachment", "see alse CSS Level 1 Properties test")
        textSpread("background-attachment", "inherit");

        message("background-position", "see alse CSS Level 1 Properties test")
        textSpread("background-position", "inherit");

        message("background", "see alse CSS Level 1 Properties test")
        textSpread("background", "inherit");
    })

    describe("Fonts: https://www.w3.org/TR/CSS2/fonts.html", function() {
        message("font-family", "see alse CSS Level 1 Properties test")
        textSpread("font-family", "inherit");

        message("font-style", "see alse CSS Level 1 Properties test")
        textSpread("font-style", "inherit");

        message("font-variant", "see alse CSS Level 1 Properties test")
        textSpread("font-variant", "inherit");

        message("font-weight", "see alse CSS Level 1 Properties test")
        textSpread("font-weight", "inherit");

        message("font-size", "see CSS Level 1 Properties test")

        message("font", "see alse CSS Level 1 Properties test")

        textSpread(
            "font",
            "caption",
            "icon",
            "menu",
            "message-box",
            "small-caption",
            "status-bar",
            "inherit"
        );
    })

    describe("Text: https://www.w3.org/TR/CSS2/text.html", function() {
        message("text-indent", "see alse CSS Level 1 Properties test")
        textSpread("text-indent", "inherit");

        message("text-align", "see alse CSS Level 1 Properties test")
        textSpread("text-align", "inherit");

        message("text-decoration", "see alse CSS Level 1 Properties test")
        textSpread("text-decoration", "inherit");

        message("letter-spacing", "see alse CSS Level 1 Properties test")
        textSpread("letter-spacing", "inherit");

        message("word-spacing", "see alse CSS Level 1 Properties test")
        textSpread("word-spacing", "inherit");

        message("text-transform", "see alse CSS Level 1 Properties test")
        textSpread("text-transform", "inherit");

        message("white-space", "see alse CSS Level 1 Properties test")
        textSpread("text-transform", "pre-line", "inherit");
    })

    describe("Tables: https://www.w3.org/TR/CSS2/tables.html", function() {
        textSpread("caption-side", "top", "bottom", "inherit");
        textSpread("table-layout", "auto", "fixed", "inherit");
        textSpread("border-collapse", "collapse", "seperate", "inherit");

        textSpread("border-spacing", "inherit");

        test.value = "border-spacing: 12px 2em";
        test.check = [px(12), em(2)];
        textSpread("empty-cells", "show", "hide", "inherit");
    })

    describe("User Interface: https://www.w3.org/TR/CSS2/ui.html", function() {
        textSpread("cursor",
            "auto ",
            "crosshair",
            "default",
            "pointer",
            "move",
            "e-resize",
            "ne-resize",
            "nw-resize",
            "n-resize",
            "se-resize",
            "sw-resize",
            "s-resize",
            "w-resize",
            "text",
            "wait",
            "help",
            "progress",
            "inherit"
        )

        test.value = "border-spacing: url(my.com:80/giraffe.cur) auto";
        test.check = [url("my.com:80/giraffe.cur"), "auto"];

        textSpread("cursor",
            "auto ",
            "crosshair",
            "default",
            "pointer",
            "move",
            "e-resize",
            "ne-resize",
            "nw-resize",
            "n-resize",
            "se-resize",
            "sw-resize",
            "s-resize",
            "w-resize",
            "text",
            "wait",
            "help",
            "progress",
            "inherit"
        )

        textSpread(
            "outline-width",
            "inherit"
        );

        test.value = "outline-width: 2px 100px"
        test.check = [checkLength(2,"px"), checkLength(100, "px")];

        test.value = "outline-width: 2px 4px 8px 9px"
        test.check = [checkLength(2,"px"),checkLength(4,"px"), checkLength(8,"px"), checkLength(9,"px")];

        textSpread(
            "outline-style",
            "none",
            "dotted",
            "dashed",
            "solid",
            "double",
            "groove",
            "ridge",
            "inset",
            "outset",
            "inherit"
        );

        textSpread("outline-color","invert","inherit")
        test.value = "outline-color: #00FF00 green";
        test.check = [color(0,255), color(0,128)];

        textSpread(
            "outline",
            "inherit"
        );

        test.value = "outline: red green solid 3px 5px 2px";
        test.check = [color(255), color(0,128), "solid", px(3), px(5), px(2)];

    })

    describe("Aural: https://www.w3.org/TR/CSS2/aural.html", function() {

    })
})
