import whind from "@candlefw/whind";

import { JUX, checkDefaults } from "./productions";

import { types } from "./property_and_type_definitions";




class ValueTerm {

    constructor(value, getPropertyParser, definitions, productions) {

        if(value instanceof JUX)
            return value;
        

        this.value = null;

        const IS_VIRTUAL = { is: false };
        
        if(typeof(value) == "string")
            var u_value = value.replace(/\-/g,"_")

        if (!(this.value = types[u_value]))
            this.value = getPropertyParser(u_value, IS_VIRTUAL, definitions, productions);

        this.prop = "";

        if (!this.value)
            return new LiteralTerm(value);

        if(this.value instanceof JUX){
            if (IS_VIRTUAL.is)
                this.value.virtual = true;
            return this.value;
        }

    }

    seal(){}

    parse(l, rule, r, ROOT = true) {
        if (typeof(l) == "string")
            l = whind(l);

        if (ROOT) {

            switch(checkDefaults(l)){
                case 1:
                rule[this.prop] = l.tx;
                return true;
                case 0:
                return false;
            }
        }

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

            if (this.prop && !this.virtual)
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

            if (this.prop && !this.virtual && ROOT)
                rule[this.prop] = v;

            return true;
        } else
            return false;
    }

    get OPTIONAL (){ return false }
    set OPTIONAL (a){}
}

class LiteralTerm {

    constructor(value, type) {
        
        if(type == whind.types.string)
            value = value.slice(1,-1);

        this.value = value;
        this.prop = null;
    }

    seal(){}

    parse(l, rule, r, root = true) {

        if (typeof(l) == "string")
            l = whind(l);

        if (root) {
            switch(checkDefaults(l)){
                case 1:
                rule[this.prop] = l.tx;
                return true;
                case 0:
                return false;
            }
        }

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

            if (this.prop  && !this.virtual && root)
                rule[this.prop] = v;

            return true;
        }
        return false;
    }

    get OPTIONAL (){ return false }
    set OPTIONAL (a){}
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
