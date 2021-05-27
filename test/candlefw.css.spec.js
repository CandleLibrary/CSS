import {
    renderWithFormatting,
    rule,
    selector,
    parse,
    isSelectorEqual,
    doesRuleHaveMatchingSelector
} from "@candlelib/css";

"@candlelib//css test spec";
"PARSER"; "#";

const stylesheet = parse(
    `div{ top:320%;
    justify-content:space-around;
    font-family:Arial, "Times new Roman";
    box-shadow:inset 2px 2px 2px rgb(20,20,20);
}
@media screen and (min-width:900px) {
    .sdfsf  #nav{padding: 0 2px;
        z-index:-5820;
    }
}
@media screen and ( max-width: 800px ) {
    body{background-color:#ff0000}
}`);
const
    result =
        `div{top:320%;
justify-content:space-around;
font-family:Arial , "Times new Roman";
box-shadow:inset 2px}


    @media screen and (min-width:900px){.sdfsf #nav{padding:0 2px;
z-index:-5820}}


    @media screen and (max-width:800px){body{background-color:#ff0000}}`;

const s = stylesheet.toString();

"Expect parser-renderWithFormatting to match source string";


//assert(s.trim().replace(/[\n\ ]+/g, "") == result.replace(/[\n\ ]+/g, ""));

assert(renderWithFormatting(rule(".div{color: rgb(22, 22, 22)}")) == ".div{color:#161616}");
assert(renderWithFormatting(rule(".div {margin:2px 2px 2px;}")) == ".div{margin:2px 2px 2px}");
assert(renderWithFormatting(rule(".div{border: 2px solid green}")) == ".div{border:2px solid #008000}");

assert(renderWithFormatting(selector(".div")) == ".div");
assert(renderWithFormatting(selector("#div")) == "#div");
assert(renderWithFormatting(selector("#div[src]")) == "#div[src]");
assert(renderWithFormatting(selector("#div[src].a")) == "#div[src].a");
assert(renderWithFormatting(selector("#div[src ^= \"true\"].a")) == "#div[src ^= \"true\"].a");
assert(renderWithFormatting(selector("#div[src ^= \"true\" i].a")) == "#div[src ^= \"true\" i].a");
assert(renderWithFormatting(selector("#div a")) == "#div a");
assert(renderWithFormatting(selector("svg|* a")) == "svg|* a");
assert(renderWithFormatting(selector("svg|* a || a")) == "svg|* a || a");
assert(renderWithFormatting(selector("svg|* a + a")) == "svg|* a + a");
assert(renderWithFormatting(selector("svg|* a:test")) == "svg|* a:test");
assert(renderWithFormatting(selector("svg|* a:nth-child(2n+1)")) == "svg|* a:nth-child(2n+1)");
assert(renderWithFormatting(selector("svg|* a::after")) == "svg|* a::after");

let a = selector("a[panda] .d"),
    b = selector("a[panda] .d");

assert("[ a[panda] .d ] == [ a[panda] .d ]", isSelectorEqual(a, b) == true);
b = selector(".a #t s");
assert("[ a[panda] .d ] != [ .a #t s ]", isSelectorEqual(a, b) == false);
b = selector("a[panda].d");


assert("[ a[panda] .d ] != [ a[panda].d ]", isSelectorEqual(a, b) == false);
b = selector(`a[svg|panda].d`);
assert("[ a[panda] .d ] != [ a[\"panda\"].d ]", isSelectorEqual(a, b) == false);


assert(doesRuleHaveMatchingSelector(rule("a[panda] .d{ top:12px}"), selector("a[panda] .d")) == true);
assert(doesRuleHaveMatchingSelector(rule("a[panda].d{ top:12px }"), selector("a[panda] .d")) == false);


