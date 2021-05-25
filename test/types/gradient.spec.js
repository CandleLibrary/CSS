
import { CSS_Gradient } from "../../build/library/types/gradient.js";
import { test } from "../test_tools.js";

const gradient = CSS_Gradient.parse("linear-gradient(red 20%, blue)");

assert(i, gradient + "" == "");


assert_group("test", sequence, () => {

    test.value("background: linear-gradient(red 20%, blue) top, top;");

    assert(i, test.prop() == "");

    assert(2 == 3);
});