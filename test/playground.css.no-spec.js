import {
    renderWithFormatting,
    parse
} from "@candlefw/css";

const val = `root{
    border-radius: calc(2px) 20px 0 50px;
    background-position: center;
}`;

assert(renderWithFormatting(parse(val)) == "");


