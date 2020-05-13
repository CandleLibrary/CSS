import whind from "@candlefw/wind";
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

    get type() {
        return "jux";
    }

    constructor() {
        this.id = JUX.step++;
        this.r = [NaN, NaN];
        this.terms = [];
        this.HAS_PROP = false;
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

    sp(value, out_val) { /* Set Property */
        if (this.HAS_PROP) {
            if (value)
                if (Array.isArray(value) && value.length === 1 && Array.isArray(value[0]))
                    out_val[0] = value[0];
                else
                    out_val[0] = value;
        }
    }

    isRepeating() {
        return !(isNaN(this.r[0]) && isNaN(this.r[1]));
    }

    parse(data) {
        const prop_data = [];

        this.parseLVL1(data instanceof whind.constructor ? data : whind(data + ""), prop_data);

        return prop_data;
    }



    parseLVL1(lx, out_val = [], ROOT = true) {

        if (typeof (lx) == "string")
            lx = whind(lx);

        let bool = false;

        if (ROOT) {
            switch (checkDefaults(lx)) {
                case 1:
                    this.sp(lx.tx, out_val);
                    return true;
                case 0:
                    return false;
            }
            bool = this.parseLVL2(lx, out_val, this.start, this.end);
        } else
            bool = this.parseLVL2(lx, out_val, this.start, this.end);

        return bool;
    }

    checkForComma(lx, out_val, temp_val = [], j = 0) {
        if (this.REQUIRE_COMMA) {
            if (out_val) {
                if (j > 0)
                    out_val.push(",", ...temp_val);
                else
                    out_val.push(...temp_val);
            }

            if (lx.ch !== ",")
                return false;

            lx.next();
        } else if (out_val)
            out_val.push(...temp_val);

        return true;
    }

    parseLVL2(lx, out_val, start, end) {

        let bool = false,
            copy = lx.copy(),
            temp_val = [];

        repeat:
        for (let j = 0; j < end && !lx.END; j++) {

            //const copy = lx.copy();

            const temp = [];

            for (let i = 0, l = this.terms.length; i < l; i++) {

                const term = this.terms[i];

                if (!term.parseLVL1(copy, temp, false)) {
                    if (!term.OPTIONAL) {
                        break repeat;
                    }
                }
            }

            temp_val.push(...temp);

            lx.sync(copy);

            bool = true;

            if (!this.checkForComma(copy, out_val, temp_val, j))
                break;
        }

        return bool;
    }

    get start() {
        return isNaN(this.r[0]) ? 1 : this.r[0];
    }
    set start(e) { }

    get end() {
        return isNaN(this.r[1]) ? 1 : this.r[1];
    }
    set end(e) { }

    get OPTIONAL() { return this.r[0] === 0; }
    set OPTIONAL(a) { }
}
JUX.step = 0;
class AND extends JUX {

    get type() {
        return "and";
    }
    parseLVL2(lx, out_val, start, end) {

        const
            PROTO = new Array(this.terms.length),
            l = this.terms.length;

        let bool = false,
            temp_val = [],
            copy = lx.copy();

        repeat:
        for (let j = 0; j < end && !lx.END; j++) {

            const
                HIT = PROTO.fill(0);
            //temp_r = [];

            and:
            while (!copy.END) {
                let FAILED = false;



                for (let i = 0; i < l; i++) {

                    if (HIT[i] === 2) continue;

                    let term = this.terms[i];

                    const temp = [];

                    if (!term.parseLVL1(copy, temp, false)) {
                        if (term.OPTIONAL)
                            HIT[i] = 1;
                    } else {
                        temp_val.push(...temp);
                        HIT[i] = 2;
                        continue and;
                    }
                }

                if (HIT.reduce((a, v) => a * v, 1) === 0)
                    break repeat;

                break;
            }

            lx.sync(copy);

            bool = true;

            if (!this.checkForComma(copy, out_val, temp_val, j))
                break;
        }

        return bool;
    }
}

class OR extends JUX {
    get type() {
        return "or";
    }
    parseLVL2(lx, out_val, start, end) {

        const
            PROTO = new Array(this.terms.length),
            l = this.terms.length;

        let
            bool = false,
            NO_HIT = true,
            copy = lx.copy(),
            temp_val = [];

        repeat:
        for (let j = 0; j < end && !lx.END; j++) {

            const HIT = PROTO.fill(0);
            let temp_r = { v: null };

            or:
            while (!copy.END) {
                let FAILED = false;
                for (let i = 0; i < l; i++) {

                    if (HIT[i] === 2) continue;

                    let term = this.terms[i];

                    if (term.parseLVL1(copy, temp_val, false)) {
                        NO_HIT = false;
                        HIT[i] = 2;
                        continue or;
                    }
                }

                if (NO_HIT) break repeat;

                break;
            }

            lx.sync(copy);

            //if (temp_r.v)
            //    this.mergeValues(r, temp_r)

            bool = true;

            if (!this.checkForComma(copy, out_val, temp_val, j))
                break;
        }

        return bool;
    }
}

OR.step = 0;

class ONE_OF extends JUX {
    get type() {
        return "one_of";
    }
    parseLVL2(lx, out_val, start, end) {

        let BOOL = false;
        const
            copy = lx.copy(),
            temp_val = [];

        for (let j = 0; j < end && !lx.END; j++) {

            const
                temp_r = [];

            let bool = false;

            for (let i = 0, l = this.terms.length; i < l; i++) {
                if (this.terms[i].parseLVL1(copy, temp_val, false)) {
                    bool = true;
                    break;
                }
            }

            if (!bool)
                break;

            lx.sync(copy);

            BOOL = true;

            if (!this.checkForComma(copy, out_val, temp_val, j))
                break;
        }

        return BOOL;
    }
}

ONE_OF.step = 0;

export { JUX, AND, OR, ONE_OF };
