import
CSS_Color
    from "./color.js";

import
CSS_Percentage
    from "./percentage.js";

import
CSS_Length
    from "./length.js";
import { Lexer } from "@candlefw/wind";
import { consumeComma } from "./consume_comma.js";

class Stop {

    color: CSS_Color;

    percentage: CSS_Percentage;

    constructor(color, percentage) {
        this.color = color;
        this.percentage = percentage || null;
    }

    toString() {
        return `${this.color}${(this.percentage) ? " " + this.percentage.toString() : ""}`;
    }
}

export default class CSS_Gradient {

    stops: Stop[];

    type: number;

    direction: CSS_Length;

    static parse(l: Lexer) {
        let cp = l.copy();
        try {
            if (cp.ty == cp.types.id) {

                switch (cp.tx) {
                    case "linear":
                        cp.n.a("-").a("gradient").a("(");
                        let dir, num, rot = null;
                        //@ts-ignore
                        if (cp.tx == "to") {
                            //@ts-ignore
                        } else if (cp.ty == cp.types.num) {
                            rot = CSS_Length.parse(cp);
                            cp.a(',');
                        }

                        let stops = [];

                        while (!cp.END && cp.ch != ")") {

                            let v = CSS_Color.parse(cp), len = null;

                            if (cp.ch != ",") {
                                if (!(len = CSS_Length.parse(cp)))
                                    len = CSS_Percentage.parse(cp);
                            };

                            consumeComma(cp);

                            stops.push(new Stop(v, len));

                            if (cp.ch == ")")
                                break;
                        }
                        cp.a(")");
                        let grad = new CSS_Gradient(0, rot);
                        grad.stops = stops;
                        l.sync(cp);
                        return grad;
                }
            }
        } catch (e) { }

        return null;
    }


    constructor(type = 0, rot = new CSS_Length(0, "deg")) {
        this.type = type; //linear gradient
        this.direction = rot;
        this.stops = [];
    }

    toString() {

        let str = [];

        switch (this.type) {
            case 0:
                str.push("linear-gradient(");
                if (Number(this.direction) !== 0)
                    str.push(this.direction.toString() + ",");
                break;
        }

        for (let i = 0; i < this.stops.length; i++)
            str.push(this.stops[i].toString() + ((i < this.stops.length - 1) ? "," : ""));

        str.push(")");

        return str.join(" ");
    }
}
