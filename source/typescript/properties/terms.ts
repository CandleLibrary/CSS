import wind, { Lexer } from "@candlefw/wind";

import { JUX, checkDefaults } from "./productions.js";

import { types } from "./property_and_type_definitions.js";

function getExtendedIdentifier(l: Lexer) {
    const IWS = l.IWS;

    l.IWS = false;

    let pk = l.pk;

    let id = "";

    while (
        !pk.END &&
        pk.ty != pk.types.ws &&
        (
            pk.ty & (wind.types.id | wind.types.num)
            || pk.tx == "-"
            || pk.tx == "_"
        )
    ) pk.next();

    id = pk.slice(l).trim();

    l.sync();

    l.tl = 0;

    l.IWS = IWS;

    return id;
}

class LiteralTerm {

    value: any;
    HAS_PROP: boolean;

    get type() { return "term"; }

    constructor(lex: Lexer) {

        let value = getExtendedIdentifier(lex);

        if (lex.type == lex.types.string)
            value = value.slice(1, -1);

        this.value = value;

        this.HAS_PROP = false;
    }

    seal() { }

    parse(data) {

        const prop_data = [];

        this.parseLVL1(data instanceof wind.constructor ? data : wind(data + ""), prop_data);

        return prop_data;
    }

    parseLVL1(l: Lexer, r, root = true) {

        if (typeof (l) == "string")
            l = wind(l);

        if (root) {
            switch (checkDefaults(l)) {
                case 1:
                    r.push(l.tx);
                    return true;
                case 0:
                    return false;
            }
        }

        const cp = l.copy();
        const v = getExtendedIdentifier(cp);

        if (v == this.value) {
            l.sync(cp);
            l.tl = 0;
            l.next();
            r.push(this.value);
            return true;
        }

        return false;
    }

    get OPTIONAL() { return false; }
    set OPTIONAL(a) { }
}

class ValueTerm extends LiteralTerm {

    constructor(value, getPropertyParser, definitions, productions) {

        if (value instanceof JUX)
            return value;

        super(wind(value));

        this.value = null;

        const IS_VIRTUAL = { is: false };

        if (typeof (value) == "string")
            var u_value = value.replace(/\-/g, "_");

        if (!(this.value = types[u_value]))
            this.value = getPropertyParser(u_value, IS_VIRTUAL, definitions, productions);

        if (!this.value)
            return new LiteralTerm(value);

        if (this.value instanceof JUX) {

            if (IS_VIRTUAL.is)
                this.value.virtual = true;

            return this.value;
        }
    }

    parseLVL1(l, r, ROOT = true) {
        if (typeof (l) == "string")
            l = wind(l);

        if (ROOT) {
            switch (checkDefaults(l)) {
                case 1:
                    r.push(l.tx);
                    return true;
                case 0:
                    return false;
            }
        }

        const v = this.value.parse(l);

        if (v) {

            if (Array.isArray(v))
                r.push(...v);
            else
                r.push(v);

            return true;
        } else
            return false;
    }
}



class SymbolTerm extends LiteralTerm {
    parseLVL1(l, rule, r) {
        if (typeof (l) == "string")
            l = wind(l);

        if (l.tx == this.value) {
            l.next();
            rule.push(this.value);
            return true;
        }

        return false;
    }
};

export { LiteralTerm, ValueTerm, SymbolTerm };
