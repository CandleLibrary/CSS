import whind from "@candlefw/whind";
import { NR, AND, OR, ONE_OF } from "./productions";
import { LiteralTerm, ValueTerm, SymbolTerm } from "./terms";
import { virtual_property_definitions } from "./property_and_type_definitions";

const standard_productions = {
    NR,
    AND,
    OR,
    ONE_OF,
    LiteralTerm,
    ValueTerm,
    SymbolTerm
}
export function getPropertyParser(property_name, IS_VIRTUAL = { is: false }, definitions = null, productions = standard_productions) {

    let prop = definitions[property_name];

    if (prop) {

        if (typeof(prop) == "string")
            prop = definitions[property_name] = CreatePropertyParser(prop, property_name, definitions, productions);

        return prop;
    }

    if (!definitions.__virtual)
        definitions.__virtual = Object.assign({}, virtual_property_definitions);

    prop = definitions.__virtual[property_name];

    if (prop) {

        IS_VIRTUAL.is = true;

        if (typeof(prop) == "string")
            prop = definitions.__virtual[property_name] = CreatePropertyParser(prop, "", definitions, productions);

        return prop;
    }

    return null;
}


function CreatePropertyParser(notation, name, definitions, productions) {

    const l = whind(notation);

    const important = { is: false };

    let n = d(l, definitions, productions);
    n.seal();

    //if (n instanceof productions.NR && n.terms.length == 1 && n.r[1] < 2)
    //    n = n.terms[0];

    n.prop = name;
    n.IMP = important.is;

    return n;
}

function d(l, definitions, productions, super_term = false, group = false, need_group = false, and_group = false, important = null) {
    let term, nt;
    const { NR, AND, OR, ONE_OF, LiteralTerm, ValueTerm, SymbolTerm } = productions;

    while (!l.END) {
        switch (l.ch) {
            case "]":
                if (term) return term;
                else
                    throw new Error("Expected to have term before \"]\"");
            case "[":
                if (term) return term;
                term = d(l.next(), definitions, productions);
                l.a("]");
                break;
            case "&":
                if (l.pk.ch == "&") {
                    if (and_group)
                        return term;

                    nt = new AND();

                    nt.terms.push(term);

                    l.sync().next();

                    while (!l.END) {
                        nt.terms.push(d(l, definitions, productions, super_term, group, need_group, true, important));
                        if (l.ch !== "&" || l.pk.ch !== "&") break;
                        l.a("&").a("&");
                    }

                    return nt;
                }
            case "|":
                {
                    if (l.pk.ch == "|") {

                        if (need_group)
                            return term;

                        nt = new OR();

                        nt.terms.push(term);

                        l.sync().next();

                        while (!l.END) {
                            nt.terms.push(d(l, definitions, productions, super_term, group, true, and_group, important));
                            if (l.ch !== "|" || l.pk.ch !== "|") break;
                            l.a("|").a("|");
                        }

                        return nt;

                    } else {
                        if (group) {
                            return term;
                        }

                        nt = new ONE_OF();

                        nt.terms.push(term);

                        l.next();

                        while (!l.END) {
                            nt.terms.push(d(l, definitions, productions, super_term, true, need_group, and_group, important));
                            if (l.ch !== "|") break;
                            l.a("|");
                        }

                        return nt;
                    }
                }
                break;
            case "{":
                term = _Jux_(productions, term);
                term.r[0] = parseInt(l.next().tx);
                if (l.next().ch == ",") {
                    l.next();
                    if (l.pk.ch == "}") {

                        term.r[1] = parseInt(l.tx);
                        l.next();
                    } else {
                        term.r[1] = Infinity;
                    }
                } else
                    term.r[1] = term.r[0];
                l.a("}");
                if (super_term) return term;
                break;
            case "*":
                term = _Jux_(productions, term);
                term.r[0] = 0;
                term.r[1] = Infinity;
                l.next();
                if (super_term) return term;
                break;
            case "+":
                term = _Jux_(productions, term);
                term.r[0] = 1;
                term.r[1] = Infinity;
                l.next();
                if (super_term) return term;
                break;
            case "?":
                term = _Jux_(productions, term);
                term.r[0] = 0;
                term.r[1] = 1;
                l.next();
                if (super_term) return term;
                break;
            case "#":
                term = _Jux_(productions, term);
                term.terms.push(new SymbolTerm(","));
                term.r[0] = 1;
                term.r[1] = Infinity;
                l.next();
                if (l.ch == "{") {
                    term.r[0] = parseInt(l.next().tx);
                    term.r[1] = parseInt(l.next().a(",").tx);
                    l.next().a("}");
                }
                if (super_term) return term;
                break;
            case "<":
                let v;

                if (term) {
                    if (term instanceof NR && term.isRepeating()) term = _Jux_(productions, new NR, term);
                    let v = d(l, definitions, productions, true);
                    term = _Jux_(productions, term, v);
                } else {
                    let v = new ValueTerm(l.next().tx, getPropertyParser, definitions, productions);
                    l.next().a(">");
                    term = v;
                }
                break;
            case "!":
                /* https://www.w3.org/TR/CSS21/cascade.html#important-rules */

                l.next().a("important");
                important.is = true;
                break;
            default:
                if (term) {
                    if (term instanceof NR && term.isRepeating()) term = _Jux_(productions, new NR, term);
                    let v = d(l, definitions, productions, true);
                    term = _Jux_(productions, term, v);
                } else {
                    let v = (l.ty == l.types.symbol) ? new SymbolTerm(l.tx) : new LiteralTerm(l.tx);
                    l.next();
                    term = v;
                }
        }
    }

    return term;
}

function _Jux_(productions, term, new_term = null) {
    if (term) {
        if (!(term instanceof productions.NR)) {
            let nr = new productions.NR();
            nr.terms.push(term);
            term = nr;
        }
        if (new_term) {
            term.seal();
            term.terms.push(new_term);
        }
        return term;
    }
    return new_term;
}
