import wind from "@candlefw/wind";

/*
    BODY {color: black; background: white }
    H1 { color: maroon }
    H2 { color: olive }
    EM { color: #f00 }              // #rgb //
    EM { color: #ff0000 }           // #rrggbb //
    EM { color: rgb(255,0,0) }      // integer range 0 - 255 //
    EM { color: rgb(100%, 0%, 0%) } // float range 0.0% - 100.0% //
*/
export default class CSS_Color extends Float64Array {

    static colors: CSS_Color[];

    static fromHCMX(
        h: number,
        c: number,
        m: number,
        x: number
    ) {
        let r = m, g = m, b = m;

        if (h < 1 && h >= 0) {
            r += c;
            g += x;
        } else if (h < 2) {
            r += x;
            g += c;
        } else if (h < 3) {
            g += c;
            b += x;
        } else if (h < 4) {
            g += x;
            b += c;
        } else if (h < 5) {
            r += x;
            b += c;
        } else if (h < 6) {
            r += c;
            b += x;
        };

        r *= 255;
        g *= 255;
        b *= 255;

        return new CSS_Color(r, g, b);
    };

    static fromHSV(
        hue: number,
        saturation: number,
        value: number
    ) {
        const
            h = (hue) / 60,
            c = value * saturation,
            m = value - c,
            x = c * (1 - Math.abs((h % 2) - 1));

        return CSS_Color.fromHCMX(h, c, m, x);
    }

    static fromHSL(
        hue: number,
        saturation: number,
        lightness: number
    ) {
        const
            h = (hue % 360) / 60,
            c = (1 - Math.abs(2 * lightness - 1)) * saturation,
            x = c * (1 - Math.abs((h % 2) - 1)),
            m = lightness - 0.5 * c;

        return CSS_Color.fromHCMX(h, c, m, x);
    }


    static parse(l) {

        let c = CSS_Color._fs_(l);

        if (c) {

            let color = new CSS_Color();

            color.set(c);

            return color;
        }

        return null;
    }

    static _verify_(l) {
        let c = CSS_Color._fs_(l, true);
        if (c)
            return true;
        return false;
    }

    /**
        Creates a new Color from a string or a Lexer.
    */
    static _fs_(l, v = false) {
        let c;

        if (typeof (l) == "string")
            l = wind(l);

        let out = { r: 0, g: 0, b: 0, a: 1 };

        switch (l.ch) {
            case "#":
                l.next();
                let pk = l.copy();

                let type = l.types;
                pk.IWS = false;


                while (!(pk.ty & (type.newline | type.ws)) && !pk.END && pk.ch !== ";") {
                    pk.next();
                }

                var value = pk.slice(l);
                l.sync(pk);
                l.tl = 0;
                l.next();

                let num = parseInt(value, 16);

                if (value.length == 3 || value.length == 4) {

                    if (value.length == 4) {
                        const a = (num >> 8) & 0xF;
                        out.a = a | a << 4;
                        num >>= 4;
                    }

                    const r = (num >> 8) & 0xF;
                    out.r = r | r << 4;

                    const g = (num >> 4) & 0xF;
                    out.g = g | g << 4;

                    const b = (num) & 0xF;
                    out.b = b | b << 4;

                } else {

                    if (value.length == 8) {
                        out.a = num & 0xFF;
                        num >>= 8;
                    }

                    out.r = (num >> 16) & 0xFF;
                    out.g = (num >> 8) & 0xFF;
                    out.b = (num) & 0xFF;
                }
                l.next();
                break;

            case "r":

                let tx = l.tx;

                const RGB_TYPE = tx === "rgba" ? 1 : tx === "rgb" ? 2 : 0;

                if (RGB_TYPE > 0) {

                    l.next(); // (

                    out.r = parseInt(l.next().tx);

                    l.next(); // , or  %

                    if (l.ch == "%") {
                        l.next(); out.r = out.r * 255 / 100;
                    }


                    out.g = parseInt(l.next().tx);

                    l.next(); // , or  %

                    if (l.ch == "%") {
                        l.next(); out.g = out.g * 255 / 100;
                    }


                    out.b = parseInt(l.next().tx);

                    l.next(); // , or ) or %

                    if (l.ch == "%")
                        l.next(), out.b = out.b * 255 / 100;

                    if (RGB_TYPE < 2) {
                        out.a = parseFloat(l.next().tx);

                        l.next();

                        if (l.ch == "%")
                            l.next(), out.a = out.a * 255 / 100;
                    }

                    l.a(")");
                    c = new CSS_Color();
                    c.set(out);
                    return c;
                }  // intentional

            case "h":

                tx = l.tx;

                const HSL_TYPE = tx === "hsla" ? 1 : tx === "hsl" ? 2 : 0;

                let h = 0, s = 0, l_ = 0;

                if (HSL_TYPE > 0) {

                    l.next(); // (

                    h = parseInt(l.next().tx);

                    l.next(); // , or  %

                    if (l.ch == "%") {
                        l.next(); out.r = out.r * 255 / 100;
                    }


                    s = parseInt(l.next().tx);

                    l.next(); // , or  %

                    if (l.ch == "%") {
                        l.next(); out.g = out.g * 255 / 100;
                    }


                    l_ = parseInt(l.next().tx);

                    l.next(); // , or ) or %

                    if (l.ch == "%")
                        l.next(), out.b = out.b * 255 / 100;

                    if (HSL_TYPE < 2) {
                        out.a = parseFloat(l.next().tx);

                        l.next();

                        if (l.ch == "%")
                            l.next(), out.a = out.a * 255 / 100;
                    }

                    l.a(")");

                    return CSS_Color.fromHSL(h, s, l_);
                }  // intentional
            default:

                let string = l.tx;

                if (l.ty == l.types.str) { string = string.slice(1, -1); }

                out = CSS_Color.colors[string.toLowerCase()];

                if (out) l.next();
        }

        return out;
    }


    get r(): number { return this[0]; }
    set r(r: number) { this[0] = Math.min(Math.max(0, r), 255) | 0; }

    get g(): number { return this[1]; }
    set g(g: number) { this[1] = Math.min(Math.max(0, g), 255) | 0; }

    get b(): number { return this[2]; }
    set b(b: number) { this[2] = Math.min(Math.max(0, b), 255) | 0; }

    get a(): number { return this[3]; }
    set a(a: number) { this[3] = a; }

    constructor(r?: number, g?: number, b?: number, a: number = 1) {
        super(4);

        this.r = 0;
        this.g = 0;
        this.b = 0;
        this.a = 1;

        if (typeof (r) === "number") {
            this.r = r; //Math.max(Math.min(Math.round(r),255),-255);
            this.g = g; //Math.max(Math.min(Math.round(g),255),-255);
            this.b = b; //Math.max(Math.min(Math.round(b),255),-255);
            this.a = a; //Math.max(Math.min(a,1),-1);
        } else if (typeof (r) == "string")
            this.set(CSS_Color._fs_(r) || { r: 255, g: 255, b: 255, a: 0 });
    }

    set(color: CSS_Color) {
        this.r = color.r;
        this.g = color.g;
        this.b = color.b;
        this.a = (color.a != undefined) ? color.a : this.a;
    }

    add(color: CSS_Color): CSS_Color {
        return new CSS_Color(
            color.r + this.r,
            color.g + this.g,
            color.b + this.b,
            color.a + this.a
        );
    }

    mult(val: number | CSS_Color) {
        if (typeof (val) == "number") {
            return new CSS_Color(
                this.r * val,
                this.g * val,
                this.b * val,
                this.a * val
            );
        } else {
            return new CSS_Color(
                this.r * val.r,
                this.g * val.g,
                this.b * val.b,
                this.a * val.a
            );
        }
    }

    sub(color: CSS_Color) {
        return new CSS_Color(
            this.r - color.r,
            this.g - color.g,
            this.b - color.b,
            this.a - color.a
        );
    }

    lerp(to: CSS_Color, t: number) {
        return this.add(to.sub(this).mult(t));
    }

    copy(other) { return new CSS_Color(other); }

    toString() {

        if (this.a !== 1)
            return this.toRGBAString();

        return `#${("0" + this.r.toString(16)).slice(-2)}${("0" + this.g.toString(16)).slice(-2)}${("0" + this.b.toString(16)).slice(-2)}`;
    }

    toRGBAString() {
        const rgb = this.toRGBString();
        if (this.a == 1) return rgb;
        return "rgba" + rgb.slice(3, -1) + `,${this.a})`;
    }
    toRGBString() { return `rgb(${this.r | 0},${this.g | 0},${this.b | 0})`; }

    toHSLString() {

        let { r, g, b } = this;

        r /= 255;
        g /= 255;
        b /= 255;

        let h = 0, h_ = 0, l = 0, s = 0,
            // hue
            M = Math.max(r, g, b),
            m = Math.min(r, g, b),
            c = M - m;

        if (M === r)
            h_ = ((g - b) / c);
        else if (M === g)
            h_ = ((b - r) / c) + 2;
        else
            h_ = ((r - g) / c) + 4;

        h_ *= 60;

        h = h_; //(((Math.PI / 180) * 60) * Math.abs(((h_+30) % 360)));

        if (h < 0) h += 360;

        //value
        l = (r * 0.3 + g * 0.59 + b * 0.11) / (r + g + b);

        //saturation
        s = (c == 0) ? 0 : c / M;

        return `hsl(${Math.round(h * 10) / 10},${Math.round(s * 10) / 10},${Math.round(l * 10) / 10})`;
    }

    toHSLAString() {
        const hsv = this.toHSLString();
        if (this.a == 1) return hsv;
        return "hsla" + hsv.slice(3, -1) + `,${this.a})`;
    }
} {

    let _$ = (r = 0, g = 0, b = 0, a = 1) => ({ r, g, b, a });
    let c = _$(0, 255, 25);
    CSS_Color.colors = {
        "alice blue": _$(240, 248, 255),
        "antique white": _$(250, 235, 215),
        "aqua marine": _$(127, 255, 212),
        "aqua": c,
        "azure": _$(240, 255, 255),
        "beige": _$(245, 245, 220),
        "bisque": _$(255, 228, 196),
        "black": _$(),
        "blanched almond": _$(255, 235, 205),
        "blue violet": _$(138, 43, 226),
        "blue": _$(0, 0, 255),
        "brown": _$(165, 42, 42),
        "burly wood": _$(222, 184, 135),
        "cadet blue": _$(95, 158, 160),
        "chart reuse": _$(127, 255),
        "chocolate": _$(210, 105, 30),
        "clear": _$(255, 255, 255),
        "coral": _$(255, 127, 80),
        "corn flower blue": _$(100, 149, 237),
        "corn silk": _$(255, 248, 220),
        "crimson": _$(220, 20, 60),
        "cyan": c,
        "dark blue": _$(0, 0, 139),
        "dark cyan": _$(0, 139, 139),
        "dark golden rod": _$(184, 134, 11),
        "dark gray": _$(169, 169, 169),
        "dark green": _$(0, 100),
        "dark khaki": _$(189, 183, 107),
        "dark magenta": _$(139, 0, 139),
        "dark olive green": _$(85, 107, 47),
        "dark orange": _$(255, 140),
        "dark orchid": _$(153, 50, 204),
        "dark red": _$(139),
        "dark salmon": _$(233, 150, 122),
        "dark sea green": _$(143, 188, 143),
        "dark slate blue": _$(72, 61, 139),
        "dark slate gray": _$(47, 79, 79),
        "dark turquoise": _$(0, 206, 209),
        "dark violet": _$(148, 0, 211),
        "deep pink": _$(255, 20, 147),
        "deep sky blue": _$(0, 191, 255),
        "dim gray": _$(105, 105, 105),
        "dodger blue": _$(30, 144, 255),
        "firebrick": _$(178, 34, 34),
        "floral white": _$(255, 250, 240),
        "forest green": _$(34, 139, 34),
        "fuchsia": _$(255, 0, 255),
        "gainsboro": _$(220, 220, 220),
        "ghost white": _$(248, 248, 255),
        "gold": _$(255, 215),
        "golden rod": _$(218, 165, 32),
        "gray": _$(128, 128, 128),
        "green yellow": _$(173, 255, 47),
        "green": _$(0, 128),
        "honeydew": _$(240, 255, 240),
        "hot pink": _$(255, 105, 180),
        "indian red": _$(205, 92, 92),
        "indigo": _$(75, 0, 130),
        "ivory": _$(255, 255, 240),
        "khaki": _$(240, 230, 140),
        "lavender blush": _$(255, 240, 245),
        "lavender": _$(230, 230, 250),
        "lawn green": _$(124, 252),
        "lemon chiffon": _$(255, 250, 205),
        "light blue": _$(173, 216, 230),
        "light coral": _$(240, 128, 128),
        "light cyan": _$(224, 255, 255),
        "light golden rod yellow": _$(250, 250, 210),
        "light gray": _$(211, 211, 211),
        "light green": _$(144, 238, 144),
        "light pink": _$(255, 182, 193),
        "light salmon": _$(255, 160, 122),
        "light sea green": _$(32, 178, 170),
        "light sky blue": _$(135, 206, 250),
        "light slate gray": _$(119, 136, 153),
        "light steel blue": _$(176, 196, 222),
        "light yellow": _$(255, 255, 224),
        "lime green": _$(50, 205, 50),
        "lime": _$(0, 255),
        "lime": _$(0, 255),
        "linen": _$(250, 240, 230),
        "magenta": _$(255, 0, 255),
        "maroon": _$(128),
        "medium aqua marine": _$(102, 205, 170),
        "medium blue": _$(0, 0, 205),
        "medium orchid": _$(186, 85, 211),
        "medium purple": _$(147, 112, 219),
        "medium sea green": _$(60, 179, 113),
        "medium slate blue": _$(123, 104, 238),
        "medium spring green": _$(0, 250, 154),
        "medium turquoise": _$(72, 209, 204),
        "medium violet red": _$(199, 21, 133),
        "midnight blue": _$(25, 25, 112),
        "mint cream": _$(245, 255, 250),
        "misty rose": _$(255, 228, 225),
        "moccasin": _$(255, 228, 181),
        "navajo white": _$(255, 222, 173),
        "navy": _$(0, 0, 128),
        "old lace": _$(253, 245, 230),
        "olive drab": _$(107, 142, 35),
        "olive": _$(128, 128),
        "orange red": _$(255, 69),
        "orange": _$(255, 165),
        "orchid": _$(218, 112, 214),
        "pale golden rod": _$(238, 232, 170),
        "pale green": _$(152, 251, 152),
        "pale turquoise": _$(175, 238, 238),
        "pale violet red": _$(219, 112, 147),
        "papaya whip": _$(255, 239, 213),
        "peach puff": _$(255, 218, 185),
        "peru": _$(205, 133, 63),
        "pink": _$(255, 192, 203),
        "plum": _$(221, 160, 221),
        "powder blue": _$(176, 224, 230),
        "purple": _$(128, 0, 128),
        "red": _$(255),
        "rosy brown": _$(188, 143, 143),
        "royal blue": _$(65, 105, 225),
        "saddle brown": _$(139, 69, 19),
        "salmon": _$(250, 128, 114),
        "sandy brown": _$(244, 164, 96),
        "sea green": _$(46, 139, 87),
        "sea shell": _$(255, 245, 238),
        "sienna": _$(160, 82, 45),
        "silver": _$(192, 192, 192),
        "sky blue": _$(135, 206, 235),
        "slate blue": _$(106, 90, 205),
        "slate gray": _$(112, 128, 144),
        "snow": _$(255, 250, 250),
        "spring green": _$(0, 255, 127),
        "steel blue": _$(70, 130, 180),
        "tan": _$(210, 180, 140),
        "teal": _$(0, 128, 128),
        "thistle": _$(216, 191, 216),
        "tomato": _$(255, 99, 71),
        "transparent": _$(0, 0, 0, 0),
        "turquoise": _$(64, 224, 208),
        "violet": _$(238, 130, 238),
        "wheat": _$(245, 222, 179),
        "white smoke": _$(245, 245, 245),
        "white": _$(255, 255, 255),
        "yellow green": _$(154, 205, 50),
        "yellow": _$(255, 255),
        "rebeccapurple": _$(102, 81, 153)
    };
}
