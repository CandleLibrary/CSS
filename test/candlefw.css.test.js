import { parse } from "../build/library/css.js";


"@candlefw/css test spec";
"PARSER"; "#";
{
    const stylesheet = parse(`div{
        font-family: Arial, "Times new Roman"
    }`);

    const result = `div{
        top:320%;
        justify-content:space-around;
        font-family:Arial, "Times new Roman"
    }`;

    ((stylesheet + "" == result));
}