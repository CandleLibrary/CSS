export default class comboSelector {
    constructor(sym, env) {
        if (sym.length > 1) {
            this.op = sym[0];
            this.selector = sym[1];
        } else {
            this.op = " ";
            this.selector = sym[0]
        }

    }

    get type() {
        return "basic"
    }

    matchBU(element, selector_array, selector = null, index = 0) {
        let ele;
        if (index < selector_array.length) {
            if ((ele = this.selector.matchBU(element, selector_array, null, index))) {
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
        }

        return null;
    }

    toString() {
        return  this.op + this.selector + "";
    }
}
