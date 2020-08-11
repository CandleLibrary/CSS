import {
    render,
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
            ` div{
    top:320%;
    justify-content:space-around;
    font-family:Arial  , "Times new Roman"
}`;
    console.log(stylesheet);
    const s = stylesheet.toString();
    "Expect parser-render to match source string"; "#";
    ((s == result));
}

"selectors"; "#";
{
    ((render(selector(".div")) == ".div"));
    ((render(selector("#div")) == "#div"));
    ((render(selector("#div[src]")) == "#div[src]"));
    ((render(selector("#div[src].a")) == "#div[src].a"));
    ((render(selector("#div[src ^= \"true\"].a")) == "#div[src ^= \"true\"].a"));
    ((render(selector("#div[src ^= \"true\" i].a")) == "#div[src ^= \"true\" i].a"));
    ((render(selector("#div a")) == "#div a"));
    ((render(selector("svg|* a")) == "svg|* a"));
    ((render(selector("svg|* a || a")) == "svg|* a || a"));
    ((render(selector("svg|* a + a")) == "svg|* a + a"));
    ((render(selector("svg|* a:test")) == "svg|* a:test"));
    ((render(selector("svg|* a:nth-child(2n+1)")) == "svg|* a:nth-child(2n+1)"));
    ((render(selector("svg|* a::after")) == "svg|* a::after"));
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

