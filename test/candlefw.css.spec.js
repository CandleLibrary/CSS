import {
    renderWithFormatting,
    rule,
    selector,
    parse,
    isSelectorEqual,
    doesRuleHaveMatchingSelector
} from "../build/library/css.js";

"@candlefw/css test spec";
"PARSER"; "#";
{
    const stylesheet = parse(`div{
        top:320%;
        justify-content:space-around;
        font-family:Arial  , "Times new Roman";
        box-shadow:inset 2px 2px 2px rgb(20,20,20);
    }
@media screen and (min-width : 900px) {
    .sdfsf  #nav{
        padding: 0 2px;
        z-index:-5820;
    }
}
@media screen and ( max-width: 800px ) {
    body{
        background-color:#ff0000
    }
}
    `),
        result =
            `div{top:320%;
justify-content:space-around;
font-family:Arial   , "Times new Roman";
box-shadow:inset 2px 2px 2px #141414}
@media screen and (min-width:900px){.sdfsf #nav{padding:0 2px;
z-index:-5820}}
@media screen and (max-width:800px){body{background-color:#ff0000}}`;

    const s = stylesheet.toString();
    "Expect parser-renderWithFormatting to match source string"; "#";
    ((s == result));
}

"selectors"; "#";
{
    ((renderWithFormatting(selector(".div")) == ".div"));
    ((renderWithFormatting(selector("#div")) == "#div"));
    ((renderWithFormatting(selector("#div[src]")) == "#div[src]"));
    ((renderWithFormatting(selector("#div[src].a")) == "#div[src].a"));
    ((renderWithFormatting(selector("#div[src ^= \"true\"].a")) == "#div[src ^= \"true\"].a"));
    ((renderWithFormatting(selector("#div[src ^= \"true\" i].a")) == "#div[src ^= \"true\" i].a"));
    ((renderWithFormatting(selector("#div a")) == "#div a"));
    ((renderWithFormatting(selector("svg|* a")) == "svg|* a"));
    ((renderWithFormatting(selector("svg|* a || a")) == "svg|* a || a"));
    ((renderWithFormatting(selector("svg|* a + a")) == "svg|* a + a"));
    ((renderWithFormatting(selector("svg|* a:test")) == "svg|* a:test"));
    ((renderWithFormatting(selector("svg|* a:nth-child(2n+1)")) == "svg|* a:nth-child(2n+1)"));
    ((renderWithFormatting(selector("svg|* a::after")) == "svg|* a::after"));
}

"isSelectorEqual"; "#";
{
    let a = selector("a[panda] .d"),
        b = selector("a[panda] .d");

    "[ a[panda] .d ] == [ a[panda] .d ]";
    ((isSelectorEqual(a, b) == true));

    b = selector(".a #t s");

    "[ a[panda] .d ] != [ .a #t s ]";
    ((isSelectorEqual(a, b) == false));

    b = selector("a[panda].d");
    "[ a[panda] .d ] != [ a[panda].d ]";
    ((isSelectorEqual(a, b) == false));

    b = selector(`a[svg|panda].d`);
    "[ a[panda] .d ] != [ a[\"panda\"].d ]";
    ((isSelectorEqual(a, b) == false));
}

"doesRuleHaveMatchingSelector"; "#"; {
    ((doesRuleHaveMatchingSelector(rule("a[panda] .d{ top:12px }"), selector("a[panda] .d")) == true));
    ((doesRuleHaveMatchingSelector(rule("a[panda] .d{ top:12px }"), selector("a[panda].d")) == false));
}

