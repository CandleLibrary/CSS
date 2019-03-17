import whind from "@candlefw/whind";
import { NR, AND, OR, ONE_OF } from "./productions";
import { LiteralTerm, ValueTerm, SymbolTerm } from "./terms";
import { virtual_property_definitions } from "./property_and_type_definitions";
import util from "util"
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

        if (typeof(prop) == "string") {
            const def = definitions.default;
            prop = definitions[property_name] = CreatePropertyParser(prop + def, property_name, definitions, productions);
        }
        prop.name = property_name;
        return prop;
    }

    if (!definitions.__virtual)
        definitions.__virtual = Object.assign({}, virtual_property_definitions);

    prop = definitions.__virtual[property_name];

    if (prop) {

        IS_VIRTUAL.is = true;

        if (typeof(prop) == "string") {
            prop = definitions.__virtual[property_name] = CreatePropertyParser(prop, "", definitions, productions);
            prop.virtual = true;
            prop.name = property_name;
        }

        return prop;
    }

    return null;
}


function CreatePropertyParser(notation, name, definitions, productions) {

    const l = whind(notation);

    const important = { is: false };

    let n = d(l, definitions, productions);
    console.log(util.inspect(n, { showHidden: false, depth: null }))
    n.seal();

    //if (n instanceof productions.NR && n.terms.length == 1 && n.r[1] < 2)
    //    n = n.terms[0];

    n.prop = name;
    n.IMP = important.is;

    return n;
}

function d(l, definitions, productions, super_term = false, group = false, need_group = false, and_group = false, important = null) {
    let term, nt, v;
    const { NR, AND, OR, ONE_OF, LiteralTerm, ValueTerm, SymbolTerm } = productions;

    let GROUP_BREAK = false;

    while (!l.END) {

        switch (l.ch) {
            case "!":
                /* https://www.w3.org/TR/CSS21/cascade.html#important-rules */
                l.next().a("important");
                important.is = true;
                break;
            case "]":

                return term;
                break;

            case "[":
                v = d(l.next(), definitions, productions, true);
                l.assert("]");
                v = checkExtensions(l, v, productions);

                if (term) {
                    if (term instanceof NR && term.isRepeating()) term = _Jux_(productions, new NR, term);
                    term = _Jux_(productions, term, v);
                } else
                    term = v;

                break;

            case "<":

                v = new ValueTerm(l.next().tx, getPropertyParser, definitions, productions);
                l.next().assert(">")
                v = checkExtensions(l, v, productions);

                if (term) {
                    if (term instanceof NR && term.isRepeating()) term = _Jux_(productions, new NR, term);
                    term = _Jux_(productions, term, v);
                } else {
                    term = v;
                }

                break;

            case "&":

                if (l.pk.ch == "&") {

                    if (and_group)
                        return term;

                    nt = new AND();

                    if (!term) throw new Error("missing term!");

                    nt.terms.push(term);

                    l.sync().next();

                    while (!l.END) {
                        nt.terms.push(d(l, definitions, productions, super_term, false, false, true, important));
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
                            nt.terms.push(d(l, definitions, productions, super_term, false, true, false, important));
                            if (l.ch !== "|" || l.pk.ch !== "|") break;
                            l.a("|").a("|");
                        }

                        return nt;

                    } else {

                        if (group)
                            return term;

                        nt = new ONE_OF();

                        nt.terms.push(term);

                        l.next();

                        while (!l.END) {
                            nt.terms.push(d(l, definitions, productions, super_term, true, false, false, important));
                            if (l.ch !== "|") break;
                            l.a("|");
                        }

                        return nt;
                    }
                }
                break;
            default:

                v = (l.ty == l.types.symbol) ? new SymbolTerm(l.tx) : new LiteralTerm(l.tx);
                l.next();
                v = checkExtensions(l, v, productions)

                if (term) {
                    if (term instanceof NR && term.isRepeating()) term = _Jux_(productions, new NR, term);
                    term = _Jux_(productions, term, v);
                } else {
                    term = v;
                }
        }
    }

    return term;
}

function checkExtensions(l, term, productions) {
    switch (l.ch) {
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
            break;
        case "*":
            term = _Jux_(productions, term);
            term.r[0] = 0;
            term.r[1] = Infinity;
            l.next();
            break;
        case "+":
            term = _Jux_(productions, term);
            term.r[0] = 1;
            term.r[1] = Infinity;
            l.next();
            break;
        case "?":
            term = _Jux_(productions, term);
            term.r[0] = 0;
            term.r[1] = 1;
            l.next();
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
            break;
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
