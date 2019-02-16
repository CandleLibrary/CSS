import whind from "@candlefw/whind";

import { NR } from "./productions";

import { types } from "./property_and_type_definitions";

class ValueTerm {

    constructor(value, getPropertyParser, definitions) {

        this.value = null;

        const IS_VIRTUAL = { is: false };

        if (!(this.value = types[value]))
            this.value = getPropertyParser(value, IS_VIRTUAL, definitions);

        this.prop = "";

        if (!this.value)
            return new LiteralTerm(value);

        if (this.value instanceof NR && IS_VIRTUAL.is)
            this.virtual = true;
    }

    seal(){}

    parse(l, rule, r) {
        if (typeof(l) == "string")
            l = whind(l);

        let rn = { v: null };

        let v = this.value.parse(l, rule, rn);

        if (rn.v) {
            if (r)
                if (r.v) {
                    if (Array.isArray(r.v)) {
                        if (Array.isArray(rn.v) && !this.virtual)
                            r.v = r.v.concat(rn.v);
                        else
                            r.v.push(rn.v);
                    } else {
                        if (Array.isArray(rn.v) && !this.virtual)
                            r.v = ([r.v]).concat(rn.v);
                        else
                            r.v = [r.v, rn.v];
                    }
                } else
                    r.v = (this.virtual) ? [rn.v] : rn.v;

            if (this.prop)
                rule[this.prop] = rn.v;

            return true;

        } else if (v) {
            if (r)
                if (r.v) {
                    if (Array.isArray(r.v))
                        r.v.push(v);
                    else
                        r.v = [r.v, v];
                } else
                    r.v = v;

            if (this.prop)
                rule[this.prop] = v;

            return true;
        } else
            return false;
    }
}

class LiteralTerm {

    constructor(value) {
        this.value = value;
        this.prop = null;
    }

    seal(){}

    parse(l, rule, r) {

        if (typeof(l) == "string")
            l = whind(l);

        let v = l.tx;
        if (v == this.value) {
            l.next();

            if (r)
                if (r.v) {
                    if (Array.isArray(r.v))
                        r.v.push(v);
                    else {
                        let t = r.v;
                        r.v = [t, v];
                    }
                } else
                    r.v = v;

            if (this.prop)
                rule[this.prop] = v;

            return true;
        }
        return false;
    }
}

class SymbolTerm extends LiteralTerm {
    parse(l, rule, r) {
        if (typeof(l) == "string")
            l = whind(l);

        if (l.tx == this.value) {
            l.next();
            return true;
        }

        return false;
    }
};

export { LiteralTerm, ValueTerm, SymbolTerm }
