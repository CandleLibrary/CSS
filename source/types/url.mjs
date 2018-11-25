import URL from "@candlefw/url"

export default class CSS_URL extends URL {
    static parse(l, rule, r) {
        if (l.tx == "url" || l.tx == "uri") {
            l.n().a("(");
            let v = "";
            if (l.ty == l.types.str) {
                v = l.tx.slice(1,-1);
                l.n().a(")");
            } else {
                let p = l.p;
                while (!p.END && p.n().tx !== ")") { /* NO OP */ }
                v = p.slice(l);
                l.sync().a(")");
            }
            return new CSS_URL(v);
        } if (l.ty == l.types.str){
            let v = l.tx.slice(1,-1);
            l.n();
            return new CSS_URL(v);
        }

        return null;
    }
}