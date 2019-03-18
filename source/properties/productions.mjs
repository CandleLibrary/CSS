import whind from "@candlefw/whind";
var step = 0;

export function checkDefaults(lx) {
    const tx = lx.tx;
    /* https://drafts.csswg.org/css-cascade/#inherited-property */
    switch (lx.tx) {
        case "initial": //intentional
        case "inherit": //intentional
        case "unset": //intentional
        case "revert": //intentional
            if (!lx.pk.pk.END) // These values should be the only ones present. Failure otherwise.
                return 0; // Default value present among other values. Invalid
            return 1; // Default value present only. Valid
    };
    return 2; // Default value not present. Ignore
}

class JUX { /* Juxtaposition */

    constructor() {
        this.id = JUX.step++;
        this.r = [NaN, NaN];
        this.terms = [];
        this.prop = null;
        this.name = "";
        this.virtual = false;
        this.REQUIRE_COMMA = false;
    }
    mergeValues(existing_v, new_v) {
        if (existing_v)
            if (existing_v.v) {
                if (Array.isArray(existing_v.v))
                    existing_v.v.push(new_v.v);
                else {
                    existing_v.v = [existing_v.v, new_v.v];
                }
            } else
                existing_v.v = new_v.v;
    }

    seal() {

    }

    sp(value, rule) { /* Set Property */
        if (this.prop) {
            if (value)
                if (Array.isArray(value) && value.length === 1 && Array.isArray(value[0]))
                    rule[this.prop] = value[0];
                else
                    rule[this.prop] = value;
        }
    }

    isRepeating() {
        return !(isNaN(this.r[0]) && isNaN(this.r[1]));
    }

    parse(lx, rule, out_val, ROOT = true) {
            
        if (typeof(lx) == "string")
            lx = whind(lx);

        let r = out_val || { v: null },
            bool = false;

        if (ROOT) {
            switch (checkDefaults(lx)) {
                case 1:
                    this.sp(lx.tx, rule);
                    return true;
                case 0:
                    return false;
            }

            bool = this.innerParser(lx, rule, out_val, r, this.start, this.end);

            if (!lx.END)
                return false;
            else
                this.sp(r.v, rule);
        } else
            bool = this.innerParser(lx, rule, out_val, r, this.start, this.end);

        return bool;
    }

    checkForComma(lx) {
        if (this.REQUIRE_COMMA) {
            if (lx.ch == ",")
                lx.next();
            else return false;
        }
        return true;
    }

    innerParser(lx, rule, out_val, r, start, end) {

        let bool = false;

        repeat:
            for (let j = 0; j < end && !lx.END; j++) {
                let copy = lx.copy();
                let temp_r = { v: null }

                for (let i = 0, l = this.terms.length; i < l; i++) {

                    let term = this.terms[i];

                    if (!term.parse(copy, rule, temp_r, false)) {
                        if (!term.OPTIONAL) {
                            break repeat;
                        }
                    }
                }

                if (temp_r.v)
                    this.mergeValues(r, temp_r)

                lx.sync(copy);

                bool = true;

                if (!this.checkForComma(lx))
                    break;
            }

        if (bool)
            //console.log("JUX", s, bool)
            return bool;
    }

    get start() {
        return isNaN(this.r[0]) ? 1 : this.r[0];
    }
    set start(e) {}

    get end() {
        return isNaN(this.r[1]) ? 1 : this.r[1];
    }
    set end(e) {}

    get OPTIONAL() { return this.r[0] === 0 }
    set OPTIONAL(a) {}
}
JUX.step = 0;
class AND extends JUX {
    innerParser(lx, rule, out_val, r, start, end) {

        const
            PROTO = new Array(this.terms.length),
            l = this.terms.length;

        let bool = false;

        repeat:
            for (let j = 0; j < end && !lx.END; j++) {

                const
                    HIT = PROTO.fill(0),
                    copy = lx.copy(),
                    temp_r = { v: null }

                and:
                    while (true) {
                        let FAILED = false;



                        for (let i = 0; i < l; i++) {

                            if (HIT[i] === 2) continue;

                            let term = this.terms[i];

                            if (!term.parse(copy, rule, temp_r, false)) {
                                if (term.OPTIONAL)
                                    HIT[i] = 1;
                            } else {
                                HIT[i] = 2;
                                continue and;
                            }
                        }

                        if (HIT.reduce((a, v) => a * v, 1) === 0)
                            break repeat;

                        break
                    }



                lx.sync(copy);

                if (temp_r.v)
                    this.mergeValues(r, temp_r)

                bool = true;

                if (!this.checkForComma(lx))
                    break;
            }

        return bool;
    }
}

class OR extends JUX {
    innerParser(lx, rule, out_val, r, start, end) {

        const
            PROTO = new Array(this.terms.length),
            l = this.terms.length;

        let
            bool = false,
            NO_HIT = true;

        repeat:
            for (let j = 0; j < end && !lx.END; j++) {

                const HIT = PROTO.fill(0);
                let copy = lx.copy();
                let temp_r = { v: null }

                or:
                    while (true) {
                        let FAILED = false;
                        for (let i = 0; i < l; i++) {

                            if (HIT[i] === 2) continue;

                            let term = this.terms[i];

                            if (term.parse(copy, temp_r, r, false)) {
                                NO_HIT = false;
                                HIT[i] = 2;
                                continue or;
                            }
                        }

                        if (NO_HIT) break repeat;

                        break;
                    }

                lx.sync(copy)

                if (temp_r.v)
                    this.mergeValues(r, temp_r)

                bool = true;

                if (!this.checkForComma(lx))
                    break;
            }

        return bool;
    }
}

OR.step = 0;

class ONE_OF extends JUX {
    innerParser(lx, rule, out_val, r, start, end) {

        let BOOL = false;

        let j;
        for (j = 0; j < end && !lx.END; j++) {
            let bool = false;
            let copy = lx.copy();
            let temp_r = { v: null }

            for (let i = 0, l = this.terms.length; i < l; i++) {
                ////if (!this.terms[i]) console.log(this)
                if (this.terms[i].parse(copy, rule, r, false)) {
                    bool = true;
                    break;
                }
            }

            if (!bool)
                break;

            lx.sync(copy)

            if (temp_r.v)
                this.mergeValues(r, temp_r)

            BOOL = true;

            if (!this.checkForComma(lx))
                break;
        }

        return BOOL;
    }
}

ONE_OF.step = 0;

export { JUX, AND, OR, ONE_OF };
