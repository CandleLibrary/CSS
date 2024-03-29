import wind from "@candlelib/wind";

function getValue(lex, attribute?) {
    let v = lex.tx,
        mult = 1;

    if (v == "-")
        v = lex.n.tx, mult = -1;

    if (lex.pk.tx == ".")
        lex.next(), (v += lex.tx);

    if (lex.pk.ty == lex.types.number)
        lex.next(), (v += lex.tx);

    if (lex.pk.tx == "e")
        lex.next(), (v += lex.tx);

    if (lex.pk.tx == "-")
        lex.next(), (v += lex.tx);

    if (lex.pk.ty == lex.types.number)
        lex.next(), (v += lex.tx);

    let n = parseFloat(v) * mult;

    lex.next();

    if (lex.ch !== ")" && lex.ch !== ",") {
        switch (lex.tx) {
            case "%":
                break;

            /* Rotational Values */
            case "grad":
                n *= Math.PI / 200;
                break;
            case "deg":
                n *= Math.PI / 180;
                break;
            case "turn":
                n *= Math.PI * 2;
                break;
            case "px":
                break;
            case "em":
                break;
        }
        lex.next();
    }
    return n;
}

function ParseString(string, transform) {
    let lex = null;
    lex = string;

    if (typeof (string) == "string")
        lex = wind(string);

    while (!lex.END) {
        let tx = lex.tx;
        lex.next();
        switch (tx) {
            case "matrix":

                let a = getValue(lex.a("(")),
                    b = getValue(lex.a(",")),
                    c = getValue(lex.a(",")),
                    d = getValue(lex.a(",")),
                    r = -Math.atan2(b, a),
                    sx1 = (a / Math.cos(r)) || 0,
                    sx2 = (b / -Math.sin(r)) || 0,
                    sy1 = (c / Math.sin(r)) || 0,
                    sy2 = (d / Math.cos(r)) || 0;

                if (sx2 !== 0)
                    transform.sx = (sx1 + sx2) * 0.5;
                else
                    transform.sx = sx1;

                if (sy1 !== 0)
                    transform.sy = (sy1 + sy2) * 0.5;
                else
                    transform.sy = sy2;

                transform.px = getValue(lex.a(","));
                transform.py = getValue(lex.a(","));
                transform.r = r;
                lex.a(")");
                break;
            case "matrix3d":
                break;
            case "translate":
                transform.px = getValue(lex.a("("), "left");
                lex.a(",");
                transform.py = getValue(lex, "left");
                lex.a(")");
                continue;
            case "translateX":
                transform.px = getValue(lex.a("("), "left");
                lex.a(")");
                continue;
            case "translateY":
                transform.py = getValue(lex.a("("), "left");
                lex.a(")");
                continue;
            case "translateZ":
                transform = CSS_Transform3D.FromCSS_Transform2D(transform);
                transform.pz = getValue(lex.a("("), "left");
                lex.a(")");
                continue;
            case "scale":
                transform.sx = getValue(lex.a("("), "left");
                if (lex.ch == ",") {
                    lex.a(",");
                    transform.sy = getValue(lex, "left");
                }
                else transform.sy = transform.sx;
                lex.a(")");
                continue;
            case "scaleX":
                transform.sx = getValue(lex.a("("), "left");
                lex.a(")");
                continue;
            case "scaleY":
                transform.sy = getValue(lex.a("("), "left");
                lex.a(")");
                continue;
            case "scaleZ":
                transform = CSS_Transform3D.FromCSS_Transform2D(transform);
                transform.sz = getValue(lex.a("("));
                lex.a(")");
                break;
            case "rotate":
                transform.r = getValue(lex.a("("));
                lex.a(")");
                continue;
            case "rotateX":
                transform = CSS_Transform3D.FromCSS_Transform2D(transform);
                transform.rx = getValue(lex.a("("));
                lex.a(")");
                break;
            case "rotateY":
                transform = CSS_Transform3D.FromCSS_Transform2D(transform);
                transform.ry = getValue(lex.a("("));
                lex.a(")");
                break;
            case "rotateZ":
                if (transform instanceof CSS_Transform2D) {
                    transform.r = getValue(lex.a("("));
                } else {
                    transform = CSS_Transform3D.FromCSS_Transform2D(transform);
                    transform.rz = getValue(lex.a("("));
                }
                lex.a(")");
                break;
            case "rotate3d":
                break;
            case "perspective":
                break;
        }
        lex.next();
    }

    return transform;
}
// A 2D transform composition of 2D position, 2D scale, and 1D rotation.
const cos = Math.cos, sin = Math.sin;

const smooth_float = i => Math.round(i * 10000) * 0.0001;

export class CSS_Transform2D extends Float64Array {

    static parse(lex) {
        return ParseString(lex, new CSS_Transform2D());
    }

    static ToString(pos: number[] | CSS_Transform2D = [0, 0], scl = [1, 1], rot = 0) {

        var px = 0,
            py = 0,
            sx = 1,
            sy = 1,
            r = 0, cos = 1, sin = 0;
        if (pos.length == 5) {
            px = pos[0];
            py = pos[1];
            sx = pos[2];
            sy = pos[3];
            r = pos[4];
        } else {
            px = pos[0];
            py = pos[1];
            sx = scl[0];
            sy = scl[1];
            r = rot;
        }

        if (r !== 0) {
            cos = Math.cos(r);
            sin = Math.sin(r);
        }

        return `matrix(${[cos * sx, -sin * sx, sy * sin, sy * cos, px, py].map(smooth_float).join(",")})`;
    }


    constructor(px?, py?, sx?, sy?, r?) {
        super(5);
        this.sx = 1;
        this.sy = 1;
        if (px !== undefined) {
            if (px instanceof CSS_Transform2D) {
                this[0] = px[0];
                this[1] = px[1];
                this[2] = px[2];
                this[3] = px[3];
                this[4] = px[4];
            } else if (typeof (px) == "string") return ParseString(px, this);
            else {
                this[0] = px;
                this[1] = py;
                this[2] = sx;
                this[3] = sy;
                this[4] = r;
            }
        }
    }
    get type(): string {
        return "transform-2D";
    }
    get px(): number {
        return this[0];
    }
    set px(v: number) {
        this[0] = v;
    }
    get py(): number {
        return this[1];
    }
    set py(v: number) {
        this[1] = v;
    }
    get sx(): number {
        return this[2];
    }
    set sx(v: number) {
        this[2] = v;
    }
    get sy(): number {
        return this[3];
    }
    set sy(v: number) {
        this[3] = v;
    }
    get r(): number {
        return this[4];
    }
    set r(v: number) {
        this[4] = v;
    }

    set scale(s: number) {
        this.sx = s;
        this.sy = s;
    }

    get scale(): number {
        return this.sx;
    }

    lerp(to, t) {
        let out = new CSS_Transform2D();
        for (let i = 0; i < 5; i++) out[i] = this[i] + (to[i] - this[i]) * t;
        return out;
    }

    toString() {
        return CSS_Transform2D.ToString(this);
    }

    copy(v) {
        let copy = new CSS_Transform2D(this);


        if (typeof (v) == "string")
            return ParseString(v, copy);
    }

    /**
     * Sets the transform value of a canvas 2D context;
     */
    setCTX(ctx) {
        let cos = 1, sin = 0;
        if (this[4] != 0) {
            cos = Math.cos(this[4]);
            sin = Math.sin(this[4]);
        }
        ctx.transform(cos * this[2], -sin * this[2], this[3] * sin, this[3] * cos, this[0], this[1]);
    }

    getLocalX(X) {
        return (X - this.px) / this.sx;
    }

    getLocalY(Y) {
        return (Y - this.py) / this.sy;
    }
}


export class CSS_Transform3D extends Float64Array {

    static FromCSS_Transform2D(transform: CSS_Transform2D | CSS_Transform3D) {

        if (transform instanceof CSS_Transform3D)
            return transform;

        return new CSS_Transform3D(transform);
    }

    static ToString(pos: number[] | CSS_Transform3D = [0, 0, 1, 1, 0, 0, 0, 0, 1]) {

        var cX = 0, cY = 0, cZ = 0,
            sX = 0, sY = 0, sZ = 0,
            sx = 0, sy = 0, sz = 0,
            px = 0, py = 0, pz = 0,
            sr1 = 0, sr2 = 0, sr3 = 0,
            sr4 = 0, sr5 = 0, sr6 = 0,
            sr7 = 0, sr8 = 0, sr9 = 0;

        px = pos[0];
        py = pos[1];
        pz = pos[7];
        sx = pos[2];
        sy = pos[3];
        sz = pos[8];
        cX = cos(pos[5]);
        sX = sin(pos[5]);
        cY = cos(pos[6]);
        sY = sin(pos[6]);
        cZ = cos(pos[4]);
        sZ = sin(pos[4]);

        sr1 = (cZ * cY) * sx;
        sr4 = ((cZ * sY * sX) - (sZ * cX)) * sx;
        sr7 = ((cZ * sY * sX) + (sZ * sX)) * sx;

        sr2 = (sZ * cY) * sy;
        sr5 = ((sZ * sY * sX) + (cZ * cX)) * sy;
        sr8 = ((sZ * sY * cX) - (cZ * sX)) * sy;

        sr3 = (-sY) * sz;
        sr6 = (cY * sX) * sz;
        sr9 = (cX * cY) * sz;

        return `matrix3d(${[sr1, sr2, sr3, 0, sr4, sr5, sr6, 0, sr7, sr8, sr9, 0, px, py, pz, 1].map(smooth_float).join(",")})`;
    }

    get px(): number {
        return this[0];
    }
    set px(v: number) {
        this[0] = v;
    }
    get py(): number {
        return this[1];
    }
    set py(v: number) {
        this[1] = v;
    }
    get pz(): number {
        return this[7];
    }
    set pz(v: number) {
        this[7] = v;
    }
    get sx(): number {
        return this[2];
    }
    set sx(v: number) {
        this[2] = v;
    }
    get sy(): number {
        return this[3];
    }
    set sy(v: number) {
        this[3] = v;
    }
    set r(v: number) {
        this.rx = 0;
        this.ry = 0;
        this.rz = v;
    }
    get r(): number {
        return this.rz;
    }
    get rx(): number {
        return this[5];
    }
    set rx(v: number) {
        this[5] = v;
    }
    get ry(): number {
        return this[6];
    }
    set ry(v: number) {
        this[6] = v;
    }
    get rz(): number {
        return this[4];
    }
    set rz(v: number) {
        this[4] = v;
    }

    set scale(s: number) {
        this.sx = s;
        this.sy = s;
    }

    get scale(): number {
        return this.sx;
    }

    constructor(px: CSS_Transform2D | string | number = 0, py = 0, pz = 0, sx = 1, sy = 1, sz = 1, rx = 0, ry = 0, rz = 0) {
        super(9);

        if (px instanceof CSS_Transform2D) {
            const transform = px;
            px = transform.px;
            py = transform.py;
        } else if (typeof (px) == "string") return ParseString(px, this);

        this[0] = px;
        this[1] = py;
        this[2] = sx;
        this[3] = sy;
        this[4] = rz;
        this[5] = rx;
        this[6] = ry;
        this[7] = pz;
        this[8] = sz;
    }

    lerp(to, t) {
        let out = new CSS_Transform3D();
        for (let i = 0; i < 9; i++) out[i] = this[i] + (to[i] - this[i]) * t;
        return out;
    }

    toString() {

        return CSS_Transform3D.ToString(this);
    }

    copy(v) {
        let copy = new CSS_Transform3D();


        if (typeof (v) == "string")
            ParseString(v, copy);

        return copy;
    }

}