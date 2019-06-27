import css, { CSSRuleBody, CSS_Length, CSS_URL } from "../source/css.mjs";
import chai from "chai";
chai.should();
import whind from "@candlefw/whind";

function checkF(f) {
    if (typeof(f) == "string")
        return checkText(f);
    else
    if (Array.isArray(f))
        return checkArray(...f)
    else
    if (typeof(f) == "object" && f.unit)
        return checkLength(f);
    else
    if (!isNaN(f))
        return checkNumber(f);
    return f
}

export function message(string) {
    it.skip(string);
}

export function checkURL(url) {
    const URL = new CSS_URL(url)
    return function(prop) {
        prop.should.have.property("host", URL.host);
        prop.should.have.property("port", URL.port);
    }
}

export function checkColor(r = 0, g = 0, b = 0, a = 1) {
    return function(prop) {
        prop.should.have.property("r", r)
        prop.should.have.property("g", g)
        prop.should.have.property("b", b)
        prop.should.have.property("a", a)
    }
}

export function checkNumber(val) {
    return function(prop) {
        prop.should.equal(val);
    }
}

export function checkLength(val, type) {
    return function(prop) {
        if (typeof(val) == "object" && val.unit) {
            type = val.unit;
        }
        prop.should.equal(parseFloat(val));
        typeof(prop.unit).should.not.equal("undefined")
        prop.unit.should.equal(type);
    }
}

export function checkPercentage(val) {
    return function(prop) {
        prop.should.equal(val);
        prop.toString().should.equal(val + "%");
    }
}

export function checkText(val) {
    return function(prop) {
        prop.should.equal(val);
    }
}

export function checkArray(...rest) {
    return function(prop) {
        if (!prop)
            throw new Error("Property note created")

        const array = prop;

        array.should.have.property("length", rest.length);

        for (let i = 0; i < rest.length; i++) {
            let func = rest[i];
            
            func = checkF(func)

            let prop = array[i];

            func(prop);
        }
    }
}

export function textSpread(name, ...rest) {
    const ONLY = test.ONLY;

    let prop_name = name.replace(/\-/g, "_");
    for (let i = 0; i < rest.length; i++) {
        let text = rest[i];
        test.value = `${name}:${text}`
        test.ONLY = ONLY;
        test.check = checkText(text);
    }

    test.ONLY = false;
}
let itOnly = null;
export const test = {
    v: "",
    prop_name: "",
    ONLY : false,
    only: function (){
        if(!itOnly)
            itOnly = it.only.bind(it);
        this.ONLY = true;
    },
    set value(v) {
        this.ONLY = false;
        this.v = v;
        this.prop_name = (new whind(v)).useExtendedId().tx.replace(/\-/g, "_");
    },
    set check(f) {
        if (!this.v)
            throw new Error("Please provide CSS property value before defining a check funcition.")
        const v = this.v;
        const prop_name = this.prop_name;

        f = checkF(f);


        (this.ONLY ? itOnly : it)(`Parses property {${v}}`, async () => {
            let sheet = css(`a{${v}}`);
            /*
            try{
            	await body.parse(whind(`{${v}}`));
            }catch(e){
            	throw "expected error"
            }
            */

            sheet.ruleset.rules[0].props[prop_name].should.not.be.undefined
            sheet.ruleset.rules[0].props[prop_name].should.not.be.null
            const prop = sheet.ruleset.rules[0].props[prop_name];

            f(prop.value);
        });
        this.prop_name = ""
        this.v = "";
    }
}

export function px(n) {
    return checkLength(new CSS_Length(n, "px"));
}

export function mm(n) {
    return checkLength(new CSS_Length(n, "mm"));
}
export function cm(n) {
    return checkLength(new CSS_Length(n, "cm"));
}
export function _in(n) {
    return checkLength(new CSS_Length(n, "_in"));
}
export function pc(n) {
    return checkLength(new CSS_Length(n, "pc"));
}
export function pt(n) {
    return checkLength(new CSS_Length(n, "pt"));
}
export function ch(n) {
    return checkLength(new CSS_Length(n, "ch"));
}
export function em(n) {
    return checkLength(new CSS_Length(n, "em"));
}
export function ex(n) {
    return checkLength(new CSS_Length(n, "ex"));
}
export function rem(n) {
    return checkLength(new CSS_Length(n, "rem"));
}
export function vh(n) {
    return checkLength(new CSS_Length(n, "vh"));
}
export function vw(n) {
    return checkLength(new CSS_Length(n, "vw"));
}
export function vmin(n) {
    return checkLength(new CSS_Length(n, "vmin"));
}
export function vmax(n) {
    return checkLength(new CSS_Length(n, "vmax"));
}
export function deg(n) {
    return checkLength(new CSS_Length(n, "deg"));
}

export const inherit = ["inherit", "unset", "revert"]
