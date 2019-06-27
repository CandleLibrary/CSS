export default class combination_selector_part {
    constructor(sym, env) {
        if (sym.length > 1) {
            this.op = sym[0];
            this.selector = sym[1];
        } else 
            return sym[0]
    }

    get type() {
        return "complex"
    }

    matchReturnElement(element, selector_array, selector = null, index = 0) {
        let ele;

        if ((ele = this.selector.matchReturnElement(element, selector_array))) {
            switch (this.op) {
                case ">":
                    return selector.match(ele.parentElement);
                case "+":
                    return selector.match(ele.previousElementSibling);
                case "~":
                    let children = ele.parentElement.children.slice(0, element.index);

                    for (const child of children) {
                        if (selector.match(child))
                            return child;
                    }
                    return null;
                default:
                    ele = ele.parentElement
                    while (ele) {
                        if (selector.match(ele))
                            return ele;
                        ele = ele.parentElement;
                    }
            }
        }

        return null;
    }

    toString() {
        return this.op + this.selector + "";
    }
}
