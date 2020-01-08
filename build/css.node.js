'use strict';

Object.defineProperty(exports, '__esModule', { value: true });

function _interopNamespace(e) {
    if (e && e.__esModule) { return e; } else {
        var n = {};
        if (e) {
            Object.keys(e).forEach(function (k) {
                var d = Object.getOwnPropertyDescriptor(e, k);
                Object.defineProperty(n, k, d.get ? d : {
                    enumerable: true,
                    get: function () {
                        return e[k];
                    }
                });
            });
        }
        n['default'] = e;
        return n;
    }
}

const A = 65;
const a = 97;
const ACKNOWLEDGE = 6;
const AMPERSAND = 38;
const ASTERISK = 42;
const AT = 64;
const B = 66;
const b = 98;
const BACKSLASH = 92;
const BACKSPACE = 8;
const BELL = 7;
const C = 67;
const c = 99;
const CANCEL = 24;
const CARET = 94;
const CARRIAGE_RETURN = 13;
const CLOSE_CURLY = 125;
const CLOSE_PARENTH = 41;
const CLOSE_SQUARE = 93;
const COLON = 58;
const COMMA = 44;
const d = 100;
const D = 68;
const DATA_LINK_ESCAPE = 16;
const DELETE = 127;
const DEVICE_CTRL_1 = 17;
const DEVICE_CTRL_2 = 18;
const DEVICE_CTRL_3 = 19;
const DEVICE_CTRL_4 = 20;
const DOLLAR = 36;
const DOUBLE_QUOTE = 34;
const e = 101;
const E = 69;
const EIGHT = 56;
const END_OF_MEDIUM = 25;
const END_OF_TRANSMISSION = 4;
const END_OF_TRANSMISSION_BLOCK = 23;
const END_OF_TXT = 3;
const ENQUIRY = 5;
const EQUAL = 61;
const ESCAPE = 27;
const EXCLAMATION = 33;
const f = 102;
const F = 70;
const FILE_SEPERATOR = 28;
const FIVE = 53;
const FORM_FEED = 12;
const FORWARD_SLASH = 47;
const FOUR = 52;
const g = 103;
const G = 71;
const GRAVE = 96;
const GREATER_THAN = 62;
const GROUP_SEPERATOR = 29;
const h = 104;
const H = 72;
const HASH = 35;
const HORIZONTAL_TAB = 9;
const HYPHEN = 45;
const i = 105;
const I = 73;
const j = 106;
const J = 74;
const k = 107;
const K = 75;
const l = 108;
const L = 76;
const LESS_THAN = 60;
const LINE_FEED = 10;
const m = 109;
const M = 77;
const n = 110;
const N = 78;
const NEGATIVE_ACKNOWLEDGE = 21;
const NINE = 57;
const NULL = 0;
const o = 111;
const O = 79;
const ONE = 49;
const OPEN_CURLY = 123;
const OPEN_PARENTH = 40;
const OPEN_SQUARE = 91;
const p = 112;
const P = 80;
const PERCENT = 37;
const PERIOD = 46;
const PLUS = 43;
const q = 113;
const Q = 81;
const QMARK = 63;
const QUOTE = 39;
const r$1 = 114;
const R = 82;
const RECORD_SEPERATOR = 30;
const s = 115;
const S = 83;
const SEMICOLON = 59;
const SEVEN = 55;
const SHIFT_IN = 15;
const SHIFT_OUT = 14;
const SIX = 54;
const SPACE = 32;
const START_OF_HEADER = 1;
const START_OF_TEXT = 2;
const SUBSTITUTE = 26;
const SYNCH_IDLE = 22;
const t = 116;
const T = 84;
const THREE = 51;
const TILDE = 126;
const TWO = 50;
const u = 117;
const U = 85;
const UNDER_SCORE = 95;
const UNIT_SEPERATOR = 31;
const v = 118;
const V = 86;
const VERTICAL_BAR = 124;
const VERTICAL_TAB = 11;
const w = 119;
const W = 87;
const x = 120;
const X = 88;
const y = 121;
const Y = 89;
const z = 122;
const Z = 90;
const ZERO = 48;

/**
 * Lexer Jump table reference 
 * 0. NUMBER
 * 1. IDENTIFIER
 * 2. QUOTE STRING
 * 3. SPACE SET
 * 4. TAB SET
 * 5. CARIAGE RETURN
 * 6. LINEFEED
 * 7. SYMBOL
 * 8. OPERATOR
 * 9. OPEN BRACKET
 * 10. CLOSE BRACKET 
 * 11. DATA_LINK
 */ 
const jump_table = [
7, 	 	/* NULL */
7, 	 	/* START_OF_HEADER */
7, 	 	/* START_OF_TEXT */
7, 	 	/* END_OF_TXT */
7, 	 	/* END_OF_TRANSMISSION */
7, 	 	/* ENQUIRY */
7, 	 	/* ACKNOWLEDGE */
7, 	 	/* BELL */
7, 	 	/* BACKSPACE */
4, 	 	/* HORIZONTAL_TAB */
6, 	 	/* LINEFEED */
7, 	 	/* VERTICAL_TAB */
7, 	 	/* FORM_FEED */
5, 	 	/* CARRIAGE_RETURN */
7, 	 	/* SHIFT_OUT */
7, 		/* SHIFT_IN */
11,	 	/* DATA_LINK_ESCAPE */
7, 	 	/* DEVICE_CTRL_1 */
7, 	 	/* DEVICE_CTRL_2 */
7, 	 	/* DEVICE_CTRL_3 */
7, 	 	/* DEVICE_CTRL_4 */
7, 	 	/* NEGATIVE_ACKNOWLEDGE */
7, 	 	/* SYNCH_IDLE */
7, 	 	/* END_OF_TRANSMISSION_BLOCK */
7, 	 	/* CANCEL */
7, 	 	/* END_OF_MEDIUM */
7, 	 	/* SUBSTITUTE */
7, 	 	/* ESCAPE */
7, 	 	/* FILE_SEPERATOR */
7, 	 	/* GROUP_SEPERATOR */
7, 	 	/* RECORD_SEPERATOR */
7, 	 	/* UNIT_SEPERATOR */
3, 	 	/* SPACE */
8, 	 	/* EXCLAMATION */
2, 	 	/* DOUBLE_QUOTE */
7, 	 	/* HASH */
7, 	 	/* DOLLAR */
8, 	 	/* PERCENT */
8, 	 	/* AMPERSAND */
2, 	 	/* QUOTE */
9, 	 	/* OPEN_PARENTH */
10, 	 /* CLOSE_PARENTH */
8, 	 	/* ASTERISK */
8, 	 	/* PLUS */
7, 	 	/* COMMA */
7, 	 	/* HYPHEN */
7, 	 	/* PERIOD */
7, 	 	/* FORWARD_SLASH */
0, 	 	/* ZERO */
0, 	 	/* ONE */
0, 	 	/* TWO */
0, 	 	/* THREE */
0, 	 	/* FOUR */
0, 	 	/* FIVE */
0, 	 	/* SIX */
0, 	 	/* SEVEN */
0, 	 	/* EIGHT */
0, 	 	/* NINE */
8, 	 	/* COLON */
7, 	 	/* SEMICOLON */
8, 	 	/* LESS_THAN */
8, 	 	/* EQUAL */
8, 	 	/* GREATER_THAN */
7, 	 	/* QMARK */
7, 	 	/* AT */
1, 	 	/* A*/
1, 	 	/* B */
1, 	 	/* C */
1, 	 	/* D */
1, 	 	/* E */
1, 	 	/* F */
1, 	 	/* G */
1, 	 	/* H */
1, 	 	/* I */
1, 	 	/* J */
1, 	 	/* K */
1, 	 	/* L */
1, 	 	/* M */
1, 	 	/* N */
1, 	 	/* O */
1, 	 	/* P */
1, 	 	/* Q */
1, 	 	/* R */
1, 	 	/* S */
1, 	 	/* T */
1, 	 	/* U */
1, 	 	/* V */
1, 	 	/* W */
1, 	 	/* X */
1, 	 	/* Y */
1, 	 	/* Z */
9, 	 	/* OPEN_SQUARE */
7, 	 	/* TILDE */
10, 	/* CLOSE_SQUARE */
7, 	 	/* CARET */
7, 	 	/* UNDER_SCORE */
2, 	 	/* GRAVE */
1, 	 	/* a */
1, 	 	/* b */
1, 	 	/* c */
1, 	 	/* d */
1, 	 	/* e */
1, 	 	/* f */
1, 	 	/* g */
1, 	 	/* h */
1, 	 	/* i */
1, 	 	/* j */
1, 	 	/* k */
1, 	 	/* l */
1, 	 	/* m */
1, 	 	/* n */
1, 	 	/* o */
1, 	 	/* p */
1, 	 	/* q */
1, 	 	/* r */
1, 	 	/* s */
1, 	 	/* t */
1, 	 	/* u */
1, 	 	/* v */
1, 	 	/* w */
1, 	 	/* x */
1, 	 	/* y */
1, 	 	/* z */
9, 	 	/* OPEN_CURLY */
7, 	 	/* VERTICAL_BAR */
10,  	/* CLOSE_CURLY */
7,  	/* TILDE */
7 		/* DELETE */
];	

/**
 * LExer Number and Identifier jump table reference
 * Number are masked by 12(4|8) and Identifiers are masked by 10(2|8)
 * entries marked as `0` are not evaluated as either being in the number set or the identifier set.
 * entries marked as `2` are in the identifier set but not the number set
 * entries marked as `4` are in the number set but not the identifier set
 * entries marked as `8` are in both number and identifier sets
 */
const number_and_identifier_table = [
0, 		/* NULL */
0, 		/* START_OF_HEADER */
0, 		/* START_OF_TEXT */
0, 		/* END_OF_TXT */
0, 		/* END_OF_TRANSMISSION */
0, 		/* ENQUIRY */
0,		/* ACKNOWLEDGE */
0,		/* BELL */
0,		/* BACKSPACE */
0,		/* HORIZONTAL_TAB */
0,		/* LINEFEED */
0,		/* VERTICAL_TAB */
0,		/* FORM_FEED */
0,		/* CARRIAGE_RETURN */
0,		/* SHIFT_OUT */
0,		/* SHIFT_IN */
0,		/* DATA_LINK_ESCAPE */
0,		/* DEVICE_CTRL_1 */
0,		/* DEVICE_CTRL_2 */
0,		/* DEVICE_CTRL_3 */
0,		/* DEVICE_CTRL_4 */
0,		/* NEGATIVE_ACKNOWLEDGE */
0,		/* SYNCH_IDLE */
0,		/* END_OF_TRANSMISSION_BLOCK */
0,		/* CANCEL */
0,		/* END_OF_MEDIUM */
0,		/* SUBSTITUTE */
0,		/* ESCAPE */
0,		/* FILE_SEPERATOR */
0,		/* GROUP_SEPERATOR */
0,		/* RECORD_SEPERATOR */
0,		/* UNIT_SEPERATOR */
0,		/* SPACE */
0,		/* EXCLAMATION */
0,		/* DOUBLE_QUOTE */
0,		/* HASH */
0,		/* DOLLAR */
0,		/* PERCENT */
0,		/* AMPERSAND */
0,		/* QUOTE */
0,		/* OPEN_PARENTH */
0,		 /* CLOSE_PARENTH */
0,		/* ASTERISK */
0,		/* PLUS */
0,		/* COMMA */
0,		/* HYPHEN */
4,		/* PERIOD */
0,		/* FORWARD_SLASH */
8,		/* ZERO */
8,		/* ONE */
8,		/* TWO */
8,		/* THREE */
8,		/* FOUR */
8,		/* FIVE */
8,		/* SIX */
8,		/* SEVEN */
8,		/* EIGHT */
8,		/* NINE */
0,		/* COLON */
0,		/* SEMICOLON */
0,		/* LESS_THAN */
0,		/* EQUAL */
0,		/* GREATER_THAN */
0,		/* QMARK */
0,		/* AT */
2,		/* A*/
8,		/* B */
2,		/* C */
2,		/* D */
8,		/* E */
2,		/* F */
2,		/* G */
2,		/* H */
2,		/* I */
2,		/* J */
2,		/* K */
2,		/* L */
2,		/* M */
2,		/* N */
8,		/* O */
2,		/* P */
2,		/* Q */
2,		/* R */
2,		/* S */
2,		/* T */
2,		/* U */
2,		/* V */
2,		/* W */
8,		/* X */
2,		/* Y */
2,		/* Z */
0,		/* OPEN_SQUARE */
0,		/* TILDE */
0,		/* CLOSE_SQUARE */
0,		/* CARET */
0,		/* UNDER_SCORE */
0,		/* GRAVE */
2,		/* a */
8,		/* b */
2,		/* c */
2,		/* d */
2,		/* e */
2,		/* f */
2,		/* g */
2,		/* h */
2,		/* i */
2,		/* j */
2,		/* k */
2,		/* l */
2,		/* m */
2,		/* n */
8,		/* o */
2,		/* p */
2,		/* q */
2,		/* r */
2,		/* s */
2,		/* t */
2,		/* u */
2,		/* v */
2,		/* w */
8,		/* x */
2,		/* y */
2,		/* z */
0,		/* OPEN_CURLY */
0,		/* VERTICAL_BAR */
0,		/* CLOSE_CURLY */
0,		/* TILDE */
0		/* DELETE */
];

const extended_number_and_identifier_table = number_and_identifier_table.slice();
extended_number_and_identifier_table[45] = 2;
extended_number_and_identifier_table[95] = 2;

const
    number = 1,
    identifier = 2,
    string = 4,
    white_space = 8,
    open_bracket = 16,
    close_bracket = 32,
    operator = 64,
    symbol = 128,
    new_line = 256,
    data_link = 512,
    alpha_numeric = (identifier | number),
    white_space_new_line = (white_space | new_line),
    Types = {
        num: number,
        number,
        id: identifier,
        identifier,
        str: string,
        string,
        ws: white_space,
        white_space,
        ob: open_bracket,
        open_bracket,
        cb: close_bracket,
        close_bracket,
        op: operator,
        operator,
        sym: symbol,
        symbol,
        nl: new_line,
        new_line,
        dl: data_link,
        data_link,
        alpha_numeric,
        white_space_new_line,
    },

    /*** MASKS ***/

    TYPE_MASK = 0xF,
    PARSE_STRING_MASK = 0x10,
    IGNORE_WHITESPACE_MASK = 0x20,
    CHARACTERS_ONLY_MASK = 0x40,
    TOKEN_LENGTH_MASK = 0xFFFFFF80,

    //De Bruijn Sequence for finding index of right most bit set.
    //http://supertech.csail.mit.edu/papers/debruijn.pdf
    debruijnLUT = [
        0, 1, 28, 2, 29, 14, 24, 3, 30, 22, 20, 15, 25, 17, 4, 8,
        31, 27, 13, 23, 21, 19, 16, 7, 26, 12, 18, 6, 11, 5, 10, 9
    ];

const getNumbrOfTrailingZeroBitsFromPowerOf2 = (value) => debruijnLUT[(value * 0x077CB531) >>> 27];

class Lexer {

    constructor(string = "", INCLUDE_WHITE_SPACE_TOKENS = false, PEEKING = false) {

        if (typeof(string) !== "string") throw new Error(`String value must be passed to Lexer. A ${typeof(string)} was passed as the \`string\` argument.`);

        /**
         * The string that the Lexer tokenizes.
         */
        this.str = string;

        /**
         * Reference to the peeking Lexer.
         */
        this.p = null;

        /**
         * The type id of the current token.
         */
        this.type = 32768; //Default "non-value" for types is 1<<15;

        /**
         * The offset in the string of the start of the current token.
         */
        this.off = 0;

        this.masked_values = 0;

        /**
         * The character offset of the current token within a line.
         */
        this.char = 0;
        /**
         * The line position of the current token.
         */
        this.line = 0;
        /**
         * The length of the string being parsed
         */
        this.sl = string.length;
        /**
         * The length of the current token.
         */
        this.tl = 0;

        /**
         * Flag to ignore white spaced.
         */
        this.IWS = !INCLUDE_WHITE_SPACE_TOKENS;

        this.USE_EXTENDED_ID = false;

        /**
         * Flag to force the lexer to parse string contents
         */
        this.PARSE_STRING = false;

        this.id_lu = number_and_identifier_table;

        if (!PEEKING) this.next();
    }

    useExtendedId(){
        this.id_lu = extended_number_and_identifier_table;
        this.tl = 0;
        this.next();
        return this;
    }

    /**
     * Restricts max parse distance to the other Lexer's current position.
     * @param      {Lexer}  Lexer   The Lexer to limit parse distance by.
     */
    fence(marker = this) {
        if (marker.str !== this.str)
            return;
        this.sl = marker.off;
        return this;
    }

    /**
     * Copies the Lexer.
     * @return     {Lexer}  Returns a new Lexer instance with the same property values.
     */
    copy(destination = new Lexer(this.str, false, true)) {
        destination.off = this.off;
        destination.char = this.char;
        destination.line = this.line;
        destination.sl = this.sl;
        destination.masked_values = this.masked_values;
        destination.id_lu = this.id_lu;
        return destination;
    }

    /**
     * Given another Lexer with the same `str` property value, it will copy the state of that Lexer.
     * @param      {Lexer}  [marker=this.peek]  The Lexer to clone the state from. 
     * @throws     {Error} Throws an error if the Lexers reference different strings.
     * @public
     */
    sync(marker = this.p) {

        if (marker instanceof Lexer) {
            if (marker.str !== this.str) throw new Error("Cannot sync Lexers with different strings!");
            this.off = marker.off;
            this.char = marker.char;
            this.line = marker.line;
            this.masked_values = marker.masked_values;
        }

        return this;
    }

    /**
    Creates an error message with a diagram illustrating the location of the error. 
    */
    errorMessage(message = "") {
        const pk = this.copy();

        pk.IWS = false;

        while (!pk.END && pk.ty !== Types.nl) { pk.next(); }

        const end = (pk.END) ? this.str.length : pk.off,

            nls = (this.line > 0) ? 1 : 0,
            number_of_tabs = this.str
                .slice(this.off - this.char + nls + nls, this.off + nls)
                .split("")
                .reduce((r, v) => (r + ((v.charCodeAt(0) == HORIZONTAL_TAB) | 0)), 0),

            arrow = String.fromCharCode(0x2b89),

            line = String.fromCharCode(0x2500),

            thick_line = String.fromCharCode(0x2501),

            line_number = `    ${this.line+1}: `,

            line_fill = line_number.length + number_of_tabs,

            line_text = this.str.slice(this.off - this.char + nls + (nls), end).replace(/\t/g, "  "),

            error_border = thick_line.repeat(line_text.length + line_number.length + 2),

            is_iws = (!this.IWS) ? "\n The Lexer produced whitespace tokens" : "",

            msg =[ `${message} at ${this.line+1}:${this.char - nls}` ,
            `${error_border}` ,
            `${line_number+line_text}` ,
            `${line.repeat(this.char-nls+line_fill-(nls))+arrow}` ,
            `${error_border}` ,
            `${is_iws}`].join("\n");

        return msg;
    }

    /**
     * Will throw a new Error, appending the parsed string line and position information to the the error message passed into the function.
     * @instance
     * @public
     * @param {String} message - The error message.
     * @param {Bool} DEFER - if true, returns an Error object instead of throwing.
     */
    throw (message, DEFER = false) {
        const error = new Error(this.errorMessage(message));
        if (DEFER)
            return error;
        throw error;
    }

    /**
     * Proxy for Lexer.prototype.reset
     * @public
     */
    r() { return this.reset() }

    /**
     * Restore the Lexer back to it's initial state.
     * @public
     */
    reset() {
        this.p = null;
        this.type = 32768;
        this.off = 0;
        this.tl = 0;
        this.char = 0;
        this.line = 0;
        this.n;
        return this;
    }

    resetHead() {
        this.off = 0;
        this.tl = 0;
        this.char = 0;
        this.line = 0;
        this.p = null;
        this.type = 32768;
    }

    /**
     * Sets the internal state to point to the next token. Sets Lexer.prototype.END to `true` if the end of the string is hit.
     * @public
     * @param {Lexer} [marker=this] - If another Lexer is passed into this method, it will advance the token state of that Lexer.
     */
    next(marker = this, USE_CUSTOM_SYMBOLS = !!this.symbol_map) {

        if (marker.sl < 1) {
            marker.off = 0;
            marker.type = 32768;
            marker.tl = 0;
            marker.line = 0;
            marker.char = 0;
            return marker;
        }

        //Token builder
        const l = marker.sl,
            str = marker.str,
            number_and_identifier_table = this.id_lu,
            IWS = marker.IWS;

        let length = marker.tl,
            off = marker.off + length,
            type = symbol,
            line = marker.line,
            base = off,
            char = marker.char,
            root = marker.off;

        if (off >= l) {
            length = 0;
            base = l;
            //char -= base - off;
            marker.char = char + (base - marker.off);
            marker.type = type;
            marker.off = base;
            marker.tl = 0;
            marker.line = line;
            return marker;
        }

        let NORMAL_PARSE = true;

        if (USE_CUSTOM_SYMBOLS) {

            let code = str.charCodeAt(off);
            let off2 = off;
            let map = this.symbol_map,
                m;
            let i = 0;

            while (code == 32 && IWS)
                (code = str.charCodeAt(++off2), off++);

            while ((m = map.get(code))) {
                map = m;
                off2 += 1;
                code = str.charCodeAt(off2);
            }

            if (map.IS_SYM) {
                NORMAL_PARSE = false;
                base = off;
                length = off2 - off;
                //char += length;
            }
        }

        if (NORMAL_PARSE) {

            for (;;) {

                base = off;

                length = 1;

                const code = str.charCodeAt(off);

                if (code < 128) {

                    switch (jump_table[code]) {
                        case 0: //NUMBER
                            while (++off < l && (12 & number_and_identifier_table[str.charCodeAt(off)]));

                            if ((str[off] == "e" || str[off] == "E") && (12 & number_and_identifier_table[str.charCodeAt(off + 1)])) {
                                off++;
                                if (str[off] == "-") off++;
                                marker.off = off;
                                marker.tl = 0;
                                marker.next();
                                off = marker.off + marker.tl;
                                //Add e to the number string
                            }

                            type = number;
                            length = off - base;

                            break;
                        case 1: //IDENTIFIER
                            while (++off < l && ((10 & number_and_identifier_table[str.charCodeAt(off)])));
                            type = identifier;
                            length = off - base;
                            break;
                        case 2: //QUOTED STRING
                            if (this.PARSE_STRING) {
                                type = symbol;
                            } else {
                                while (++off < l && str.charCodeAt(off) !== code);
                                type = string;
                                length = off - base + 1;
                            }
                            break;
                        case 3: //SPACE SET
                            while (++off < l && str.charCodeAt(off) === SPACE);
                            type = white_space;
                            length = off - base;
                            break;
                        case 4: //TAB SET
                            while (++off < l && str[off] === HORIZONTAL_TAB);
                            type = white_space;
                            length = off - base;
                            break;
                        case 5: //CARIAGE RETURN
                            length = 2;
                            //intentional
                        case 6: //LINEFEED
                            type = new_line;
                            line++;
                            base = off;
                            root = off;
                            off += length;
                            char = 0;
                            break;
                        case 7: //SYMBOL
                            type = symbol;
                            break;
                        case 8: //OPERATOR
                            type = operator;
                            break;
                        case 9: //OPEN BRACKET
                            type = open_bracket;
                            break;
                        case 10: //CLOSE BRACKET
                            type = close_bracket;
                            break;
                        case 11: //Data Link Escape
                            type = data_link;
                            length = 4; //Stores two UTF16 values and a data link sentinel
                            break;
                    }
                } else {
                    break;
                }

                if (IWS && (type & white_space_new_line)) {
                    if (off < l) {
                        type = symbol;
                        //off += length;
                        continue;
                    } else {
                        //Trim white space from end of string
                        //base = l - off;
                        //marker.sl -= off;
                        //length = 0;
                    }
                }
                break;
            }
        }

        marker.type = type;
        marker.off = base;
        marker.tl = (this.masked_values & CHARACTERS_ONLY_MASK) ? Math.min(1, length) : length;
        marker.char = char + base - root;
        marker.line = line;

        return marker;
    }


    /**
     * Proxy for Lexer.prototype.assert
     * @public
     */
    a(text) {
        return this.assert(text);
    }

    /**
     * Compares the string value of the current token to the value passed in. Advances to next token if the two are equal.
     * @public
     * @throws {Error} - `Expecting "${text}" got "${this.text}"`
     * @param {String} text - The string to compare.
     */
    assert(text) {

        if (this.off < 0) this.throw(`Expecting ${text} got null`);

        if (this.text == text)
            this.next();
        else
            this.throw(`Expecting "${text}" got "${this.text}"`);

        return this;
    }

    /**
     * Proxy for Lexer.prototype.assertCharacter
     * @public
     */
    aC(char) { return this.assertCharacter(char) }
    /**
     * Compares the character value of the current token to the value passed in. Advances to next token if the two are equal.
     * @public
     * @throws {Error} - `Expecting "${text}" got "${this.text}"`
     * @param {String} text - The string to compare.
     */
    assertCharacter(char) {

        if (this.off < 0) this.throw(`Expecting ${char[0]} got null`);

        if (this.ch == char[0])
            this.next();
        else
            this.throw(`Expecting "${char[0]}" got "${this.tx[this.off]}"`);

        return this;
    }

    /**
     * Returns the Lexer bound to Lexer.prototype.p, or creates and binds a new Lexer to Lexer.prototype.p. Advences the other Lexer to the token ahead of the calling Lexer.
     * @public
     * @type {Lexer}
     * @param {Lexer} [marker=this] - The marker to originate the peek from. 
     * @param {Lexer} [peek_marker=this.p] - The Lexer to set to the next token state.
     * @return {Lexer} - The Lexer that contains the peeked at token.
     */
    peek(marker = this, peek_marker = this.p) {

        if (!peek_marker) {
            if (!marker) return null;
            if (!this.p) {
                this.p = new Lexer(this.str, false, true);
                peek_marker = this.p;
            }
        }
        peek_marker.masked_values = marker.masked_values;
        peek_marker.type = marker.type;
        peek_marker.off = marker.off;
        peek_marker.tl = marker.tl;
        peek_marker.char = marker.char;
        peek_marker.line = marker.line;
        this.next(peek_marker);
        return peek_marker;
    }


    /**
     * Proxy for Lexer.prototype.slice
     * @public
     */
    s(start) { return this.slice(start) }

    /**
     * Returns a slice of the parsed string beginning at `start` and ending at the current token.
     * @param {Number | LexerBeta} start - The offset in this.str to begin the slice. If this value is a LexerBeta, sets the start point to the value of start.off.
     * @return {String} A substring of the parsed string.
     * @public
     */
    slice(start = this.off) {

        if (start instanceof Lexer) start = start.off;

        return this.str.slice(start, (this.off <= start) ? this.sl : this.off);
    }

    /**
     * Skips to the end of a comment section.
     * @param {boolean} ASSERT - If set to true, will through an error if there is not a comment line or block to skip.
     * @param {Lexer} [marker=this] - If another Lexer is passed into this method, it will advance the token state of that Lexer.
     */
    comment(ASSERT = false, marker = this) {

        if (!(marker instanceof Lexer)) return marker;

        if (marker.ch == "/") {
            if (marker.pk.ch == "*") {
                marker.sync();
                while (!marker.END && (marker.next().ch != "*" || marker.pk.ch != "/")) { /* NO OP */ }
                marker.sync().assert("/");
            } else if (marker.pk.ch == "/") {
                const IWS = marker.IWS;
                while (marker.next().ty != Types.new_line && !marker.END) { /* NO OP */ }
                marker.IWS = IWS;
                marker.next();
            } else
            if (ASSERT) marker.throw("Expecting the start of a comment");
        }

        return marker;
    }

    setString(string, RESET = true) {
        this.str = string;
        this.sl = string.length;
        if (RESET) this.resetHead();
    }

    toString() {
        return this.slice();
    }

    /**
     * Returns new Whind Lexer that has leading and trailing whitespace characters removed from input. 
     * leave_leading_amount - Maximum amount of leading space caracters to leave behind. Default is zero
     * leave_trailing_amount - Maximum amount of trailing space caracters to leave behind. Default is zero
     */
    trim(leave_leading_amount = 0, leave_trailing_amount = leave_leading_amount) {
        const lex = this.copy();

        let space_count = 0,
            off = lex.off;

        for (; lex.off < lex.sl; lex.off++) {
            const c = jump_table[lex.string.charCodeAt(lex.off)];

            if (c > 2 && c < 7) {

                if (space_count >= leave_leading_amount) {
                    off++;
                } else {
                    space_count++;
                }
                continue;
            }

            break;
        }

        lex.off = off;
        space_count = 0;
        off = lex.sl;

        for (; lex.sl > lex.off; lex.sl--) {
            const c = jump_table[lex.string.charCodeAt(lex.sl - 1)];

            if (c > 2 && c < 7) {
                if (space_count >= leave_trailing_amount) {
                    off--;
                } else {
                    space_count++;
                }
                continue;
            }

            break;
        }

        lex.sl = off;

        if (leave_leading_amount > 0)
            lex.IWS = false;

        lex.token_length = 0;

        lex.next();

        return lex;
    }

    /** Adds symbol to symbol_map. This allows custom symbols to be defined and tokenized by parser. **/
    addSymbol(sym) {
        if (!this.symbol_map)
            this.symbol_map = new Map;


        let map = this.symbol_map;

        for (let i = 0; i < sym.length; i++) {
            let code = sym.charCodeAt(i);
            let m = map.get(code);
            if (!m) {
                m = map.set(code, new Map).get(code);
            }
            map = m;
        }
        map.IS_SYM = true;
    }

    /*** Getters and Setters ***/
    get string() {
        return this.str;
    }

    get string_length() {
        return this.sl - this.off;
    }

    set string_length(s) {}

    /**
     * The current token in the form of a new Lexer with the current state.
     * Proxy property for Lexer.prototype.copy
     * @type {Lexer}
     * @public
     * @readonly
     */
    get token() {
        return this.copy();
    }


    get ch() {
        return this.str[this.off];
    }

    /**
     * Proxy for Lexer.prototype.text
     * @public
     * @type {String}
     * @readonly
     */
    get tx() { return this.text }

    /**
     * The string value of the current token.
     * @type {String}
     * @public
     * @readonly
     */
    get text() {
        return (this.off < 0) ? "" : this.str.slice(this.off, this.off + this.tl);
    }

    /**
     * The type id of the current token.
     * @type {Number}
     * @public
     * @readonly
     */
    get ty() { return this.type }

    /**
     * The current token's offset position from the start of the string.
     * @type {Number}
     * @public
     * @readonly
     */
    get pos() {
        return this.off;
    }

    /**
     * Proxy for Lexer.prototype.peek
     * @public
     * @readonly
     * @type {Lexer}
     */
    get pk() { return this.peek() }

    /**
     * Proxy for Lexer.prototype.next
     * @public
     */
    get n() { return this.next() }

    get END() { return this.off >= this.sl }
    set END(v) {}

    get type() {
        return 1 << (this.masked_values & TYPE_MASK);
    }

    set type(value) {
        //assuming power of 2 value.
        this.masked_values = (this.masked_values & ~TYPE_MASK) | ((getNumbrOfTrailingZeroBitsFromPowerOf2(value)) & TYPE_MASK);
    }

    get tl() {
        return this.token_length;
    }

    set tl(value) {
        this.token_length = value;
    }

    get token_length() {
        return ((this.masked_values & TOKEN_LENGTH_MASK) >> 7);
    }

    set token_length(value) {
        this.masked_values = (this.masked_values & ~TOKEN_LENGTH_MASK) | (((value << 7) | 0) & TOKEN_LENGTH_MASK);
    }

    get IGNORE_WHITE_SPACE() {
        return this.IWS;
    }

    set IGNORE_WHITE_SPACE(bool) {
        this.iws = !!bool;
    }

    get CHARACTERS_ONLY() {
        return !!(this.masked_values & CHARACTERS_ONLY_MASK);
    }

    set CHARACTERS_ONLY(boolean) {
        this.masked_values = (this.masked_values & ~CHARACTERS_ONLY_MASK) | ((boolean | 0) << 6);
    }

    get IWS() {
        return !!(this.masked_values & IGNORE_WHITESPACE_MASK);
    }

    set IWS(boolean) {
        this.masked_values = (this.masked_values & ~IGNORE_WHITESPACE_MASK) | ((boolean | 0) << 5);
    }

    get PARSE_STRING() {
        return !!(this.masked_values & PARSE_STRING_MASK);
    }

    set PARSE_STRING(boolean) {
        this.masked_values = (this.masked_values & ~PARSE_STRING_MASK) | ((boolean | 0) << 4);
    }

    /**
     * Reference to token id types.
     */
    get types() {
        return Types;
    }
}

Lexer.prototype.addCharacter = Lexer.prototype.addSymbol;

function whind$1(string, INCLUDE_WHITE_SPACE_TOKENS = false) { return new Lexer(string, INCLUDE_WHITE_SPACE_TOKENS) }

whind$1.constructor = Lexer;

Lexer.types = Types;
whind$1.types = Types;

let fn = {}; const 
/************** Maps **************/

    /* Symbols To Inject into the Lexer */
    symbols = ["||","^=","$=","*=","<=",">="],

    /* Goto lookup maps */
    gt0 = [0,-1,4,2,7,3,1,10,8,-2,9,-5,5,-1,37,-4,38,-9,36,-38,12,15,-1,34,16,13,-1,14,21,18,17,25,26,27,-1,28,-3,29,35,-9,23,-3,20],
gt1 = [0,-1,40,-1,7,39,-1,10,8,-2,9,-5,5,-1,37,-4,38,-9,36,-38,12,15,-1,34,16,13,-1,14,21,18,17,25,26,27,-1,28,-3,29,35,-9,23,-3,20],
gt2 = [0,-3,41,-2,10,8,-2,9,-5,42,-1,37,-4,38,-9,36,-38,12,15,-1,34,16,13,-1,14,21,18,17,25,26,27,-1,28,-3,29,35,-9,23,-3,20],
gt3 = [0,-10,52,-5,42,-1,37,-4,38,-9,36,-59,53,-1,51,50,-1,54,-3,23,-3,55],
gt4 = [0,-70,57,56,-1,15,-1,34,16,59,58,14,21,18,17,25,26,27,-1,28,-3,29,35,-9,23,-3,20],
gt5 = [0,-73,64,-1,34,65,-6,25,26,27,-1,28,-3,29,35],
gt6 = [0,-75,34,66,-6,67,26,27,-1,28,-3,29,35],
gt7 = [0,-75,68,-16,35],
gt8 = [0,-102,23,-3,71],
gt9 = [0,-103,75,73,74],
gt10 = [0,-102,23,-3,81],
gt11 = [0,-102,23,-3,82],
gt12 = [0,-80,21,84,83,-19,23,-3,20],
gt13 = [0,-91,87,-10,23,-3,86],
gt14 = [0,-74,89,-16,90],
gt15 = [0,-11,94,95,-53,98,-2,97],
gt16 = [0,-32,102,-1,105,-1,103,107,104,109,-2,110,-2,108,111,-1,114,-3,115,-8,106,-40,23,-3,116],
gt17 = [0,-19,118,-49,120],
gt18 = [0,-27,121,123,125,128,127,-20,126,-49,23,-3,130],
gt19 = [0,-10,52,-5,42,-1,37,-4,38,-9,36,-59,53,-1,51,131,-1,54,-3,23,-3,55],
gt20 = [0,-72,132,15,-1,34,16,13,-1,14,21,18,17,25,26,27,-1,28,-3,29,35,-9,23,-3,20],
gt21 = [0,-10,135,-5,42,-1,37,-4,38,-9,36,-59,53,-1,136,-2,54,-3,23,-3,55],
gt22 = [0,-70,139,-2,15,-1,34,16,59,58,14,21,18,17,25,26,27,-1,28,-3,29,35,-9,23,-3,20],
gt23 = [0,-73,15,-1,34,16,140,-1,14,21,18,17,25,26,27,-1,28,-3,29,35,-9,23,-3,20],
gt24 = [0,-75,34,141,-6,67,26,27,-1,28,-3,29,35],
gt25 = [0,-91,87],
gt26 = [0,-103,143,-1,142],
gt27 = [0,-88,145],
gt28 = [0,-90,151],
gt29 = [0,-102,23,-3,86],
gt30 = [0,-91,153],
gt31 = [0,-12,154,-53,98,-2,97],
gt32 = [0,-14,156,-17,157,-1,105,-1,103,107,104,109,-2,110,-2,108,111,-1,114,-3,115,-8,106,-40,23,-3,116],
gt33 = [0,-68,159],
gt34 = [0,-68,161],
gt35 = [0,-61,165,-40,23,-3,166],
gt36 = [0,-35,167],
gt37 = [0,-40,171,169,-1,173,170],
gt38 = [0,-46,175,-1,114,-3,115,-49,23,-3,130],
gt39 = [0,-37,107,176,109,-2,110,-2,108,111,177,114,-3,115,180,-3,182,184,181,183,-1,187,-2,186,-36,23,-3,178],
gt40 = [0,-28,191,125,128,127,-20,126,-49,23,-3,130],
gt41 = [0,-24,194,193,192],
gt42 = [0,-27,197,123,125,128,127,-20,126,-45,198,-3,23,-3,199],
gt43 = [0,-93,205,-4,54,-3,23,-3,55],
gt44 = [0,-99,209,207,206],
gt45 = [0,-86,213,-15,23,-3,214],
gt46 = [0,-72,217,15,-1,34,16,13,-1,14,21,18,17,25,26,27,-1,28,-3,29,35,-9,23,-3,20],
gt47 = [0,-14,218,-17,219,-1,105,-1,103,107,104,109,-2,110,-2,108,111,-1,114,-3,115,-8,106,-40,23,-3,116],
gt48 = [0,-32,220,-1,105,-1,103,107,104,109,-2,110,-2,108,111,-1,114,-3,115,-8,106,-40,23,-3,116],
gt49 = [0,-69,225],
gt50 = [0,-6,10,228,227,226,-62,12,15,-1,34,16,13,-1,14,21,18,17,25,26,27,-1,28,-3,29,35,-9,23,-3,20],
gt51 = [0,-34,105,-1,229,107,104,109,-2,110,-2,108,111,-1,114,-3,115,-8,106,-40,23,-3,116],
gt52 = [0,-35,230],
gt53 = [0,-37,231,-1,109,-2,110,-3,232,-1,114,-3,115,-49,23,-3,130],
gt54 = [0,-40,233],
gt55 = [0,-43,234],
gt56 = [0,-46,235,-1,114,-3,115,-49,23,-3,130],
gt57 = [0,-46,236,-1,114,-3,115,-49,23,-3,130],
gt58 = [0,-49,241,-1,239],
gt59 = [0,-54,245],
gt60 = [0,-54,251,252,253],
gt61 = [0,-64,258],
gt62 = [0,-49,241,-1,263],
gt63 = [0,-17,265,-2,267,266,268,-40,271],
gt64 = [0,-6,10,228,227,273,-62,12,15,-1,34,16,13,-1,14,21,18,17,25,26,27,-1,28,-3,29,35,-9,23,-3,20],
gt65 = [0,-24,194,274],
gt66 = [0,-28,275,125,128,127,-20,126,-49,23,-3,130],
gt67 = [0,-72,278,15,-1,34,16,13,-1,14,21,18,17,25,26,27,-1,28,-3,29,35,-9,23,-3,20],
gt68 = [0,-97,280,-1,209,207,281],
gt69 = [0,-99,283],
gt70 = [0,-99,209,207,284],
gt71 = [0,-89,285],
gt72 = [0,-32,290,-1,105,-1,103,107,104,109,-2,110,-2,108,111,-1,114,-3,115,-8,106,-40,23,-3,116],
gt73 = [0,-13,291,-13,292,123,125,128,127,-20,126,-45,293,-3,23,-3,294],
gt74 = [0,-6,10,297,-64,12,15,-1,34,16,13,-1,14,21,18,17,25,26,27,-1,28,-3,29,35,-9,23,-3,20],
gt75 = [0,-40,171,169],
gt76 = [0,-49,299],
gt77 = [0,-58,300,-1,301,-1,187,-2,186,-36,23,-3,302],
gt78 = [0,-58,303,-1,301,-1,187,-2,186,-36,23,-3,302],
gt79 = [0,-60,304,-41,23,-3,302],
gt80 = [0,-102,23,-3,305],
gt81 = [0,-102,23,-3,306],
gt82 = [0,-20,267,310,268,-40,271],
gt83 = [0,-99,209,207,281],
gt84 = [0,-55,320],
gt85 = [0,-56,323],
gt86 = [0,-10,52,-5,42,-1,37,-4,38,-9,36,-59,53,-1,51,326,-1,54,-3,23,-3,55],
gt87 = [0,-22,327,-40,271],
gt88 = [0,-58,328,-1,301,-1,187,-2,186,-36,23,-3,302],
gt89 = [0,-58,329,-1,301,-1,187,-2,186,-36,23,-3,302],

    // State action lookup maps
    sm0=[0,1,-1,2,-1,0,-4,0,-6,3,-6,4,-31,5,6,7,8,-1,9,-9,10],
sm1=[0,11,-3,0,-4,0],
sm2=[0,12,-1,2,-1,0,-4,0,-6,3,-6,4,-31,5,6,7,8,-1,9,-9,10],
sm3=[0,13,-1,2,-1,0,-4,0,-6,3,-6,4,-31,5,6,7,8,-1,9,-9,10],
sm4=[0,14,-1,14,-1,0,-4,0,-6,14,-6,14,-31,14,14,14,14,-1,14,-9,14],
sm5=[0,15,-1,15,-1,0,-4,0,-6,15,-6,15,-31,15,15,15,15,-1,15,-6,16,-2,15],
sm6=[0,-4,0,-4,0,-10,17,-3,18,19,-7,20],
sm7=[0,21,-1,21,-1,0,-4,0,-6,21,-6,21,-31,21,21,21,21,-1,21,-9,21],
sm8=[0,22,-1,22,-1,0,-4,0,-6,22,-6,22,-31,22,22,22,22,-1,22,-9,22],
sm9=[0,-4,0,-4,0,-5,23,24],
sm10=[0,-2,2,-1,0,-4,0,-13,4],
sm11=[0,-4,0,-4,0,-5,25,25],
sm12=[0,-2,2,-1,0,-4,0,-5,26,26,-5,26,-15,27,-12,28,29,30,-1,5,6,7,8,-1,9,-9,10],
sm13=[0,-2,31,-1,0,-4,0,-5,31,31,-5,31,-15,31,-12,31,31,31,-1,31,31,7,8,-1,9,-9,10],
sm14=[0,-2,31,-1,0,-4,0,-5,31,31,-5,31,-15,31,-12,31,31,31,-1,31,31,31,31,-1,31,-9,32],
sm15=[0,-2,33,-1,0,-4,0,-5,33,33,-5,33,-15,33,-12,33,33,33,-1,33,33,33,33,-1,33,-9,33],
sm16=[0,-2,2,-1,0,-4,0,-45,34],
sm17=[0,-2,35,-1,0,-4,0,-5,35,35,-5,35,-15,35,-12,35,35,35,-1,35,36,35,35,-1,35,-9,35],
sm18=[0,-2,37,-1,0,-4,0,-5,37,37,-5,37,-15,37,-2,37,-9,37,37,37,-1,37,36,37,37,-1,37,37,37,37,37,-5,37],
sm19=[0,-4,0,-4,0,-46,38],
sm20=[0,-2,39,-1,0,-4,0,-45,39],
sm21=[0,40,-1,41,-1,42,-4,43,-3,40,-1,40,40,-4,40,40,40,-5,40,-7,40,40,40,40,40,-9,40,40,40,-1,40,40,40,40,-1,40,40,40,40,40,40,40,40,-2,40,-1,44,45],
sm22=[0,46,-1,46,-1,46,-4,46,-3,46,-1,46,46,-4,46,46,46,-5,46,-7,46,46,46,46,46,-9,46,46,46,-1,46,46,46,46,-1,46,46,46,46,46,46,46,46,-2,46,-1,46,46],
sm23=[0,-2,47,-1,0,-4,0,-5,47,47,-5,47,-15,47,-12,47,47,47,-1,47,47,47,47,-1,47,-9,47],
sm24=[0,-2,48,-1,0,-4,0,-5,48,48,-5,48,-15,48,-12,48,48,48,-1,48,48,48,48,-1,48,-9,48],
sm25=[0,-2,2,-1,0,-4,0],
sm26=[0,-2,2,-1,0,-4,0,-45,49,6],
sm27=[0,-2,2,-1,0,-4,0,-60,50],
sm28=[0,-2,51,-1,0,-4,0,-5,51,51,-5,51,-15,51,-12,51,51,51,-1,51,51,51,51,-1,51,-9,51],
sm29=[0,-2,52,-1,0,-4,0,-5,52,52,-5,52,-15,52,-12,52,52,52,-1,52,52,52,52,-1,52,-9,50],
sm30=[0,-4,0,-4,0,-57,53],
sm31=[0,-4,0,-4,0,-57,54],
sm32=[0,-4,0,-4,0,-57,55],
sm33=[0,56,-1,2,-1,0,-4,0,-6,3,-6,4,-31,5,6,7,8,-1,9,-9,10],
sm34=[0,57,-1,57,-1,0,-4,0,-6,57,-6,57,-31,57,57,57,57,-1,57,-9,57],
sm35=[0,58,-1,58,-1,0,-4,0,-6,58,-6,58,-31,58,58,58,58,-1,58,-9,58],
sm36=[0,-4,0,-4,0,-57,16],
sm37=[0,59,-1,59,-1,0,-4,0,-6,59,-1,59,-4,59,-31,59,59,59,59,-1,59,-6,59,-2,59],
sm38=[0,-4,60,-4,0,-38,61,62,63],
sm39=[0,-2,2,-1,0,-4,0,-11,64,-9,65,-2,66],
sm40=[0,-4,0,-4,0,-16,67,-22,62,63],
sm41=[0,-2,2,-1,0,-4,68,-11,69,-9,70],
sm42=[0,-2,2,-1,0,-4,0,-45,5,6,7,8,-1,9,-9,10],
sm43=[0,-2,2,-1,0,-4,0,-8,71,-4,4,-43,72],
sm44=[0,-2,73,-1,0,-4,0,-8,73,-4,73,-43,74],
sm45=[0,-2,73,-1,0,-4,0,-8,73,-4,73,-43,73],
sm46=[0,-2,75,-1,0,-4,0,-8,75,-4,75,-43,75],
sm47=[0,-2,76,-1,0,-4,0,-8,76,-4,76,-43,76],
sm48=[0,-4,0,-4,0,-60,77],
sm49=[0,-2,2,-1,0,-4,0,-5,78,78,-5,78,-15,27,-12,28,29,30,-1,5,6,7,8,-1,9,-9,10],
sm50=[0,-2,79,-1,0,-4,0,-5,79,79,-5,79,-15,79,-12,79,79,79,-1,79,79,79,79,-1,79,-9,79],
sm51=[0,-2,80,-1,0,-4,0,-5,80,80,-5,80,-15,80,-12,80,80,80,-1,80,80,80,80,-1,80,-9,80],
sm52=[0,-2,81,-1,0,-4,0,-45,81,81,81,81,-1,81,-9,81],
sm53=[0,-2,82,-1,0,-4,0,-5,82,82,-5,82,-15,82,-12,82,82,82,-1,82,82,7,8,-1,9,-9,10],
sm54=[0,-2,82,-1,0,-4,0,-5,82,82,-5,82,-15,82,-12,82,82,82,-1,82,82,82,82,-1,82,-9,32],
sm55=[0,-2,83,-1,0,-4,0,-5,83,83,-5,83,-15,83,-12,83,83,83,-1,83,83,83,83,-1,83,-9,83],
sm56=[0,-2,84,-1,0,-4,0,-5,84,84,-5,84,-15,84,-12,84,84,84,-1,84,84,84,84,-1,84,-9,84],
sm57=[0,-4,0,-4,0,-60,50],
sm58=[0,-2,85,-1,0,-4,0,-5,85,85,-5,85,-15,85,-12,85,85,85,-1,85,85,85,85,-1,85,-9,85],
sm59=[0,-2,86,-1,0,-4,0,-5,86,86,-5,86,-15,86,-2,86,-9,86,86,86,-1,86,86,86,86,-1,86,86,86,86,86,-5,86],
sm60=[0,-2,87,-1,0,-4,0,-45,87],
sm61=[0,88,-1,41,-1,42,-4,43,-3,88,-1,88,88,-4,88,88,88,-5,88,-7,88,88,88,88,88,-9,88,88,88,-1,88,88,88,88,-1,88,88,88,88,88,88,88,88,-2,88,-1,44,45],
sm62=[0,89,-1,89,-1,89,-4,0,-3,89,-1,89,89,-4,89,89,89,-5,89,-7,89,89,89,89,89,-9,89,89,89,-1,89,89,89,89,-1,89,89,89,89,89,89,89,89,-2,89],
sm63=[0,90,-1,90,-1,90,-4,90,-3,90,-1,90,90,-4,90,90,90,-5,90,-7,90,90,90,90,90,-9,90,90,90,-1,90,90,90,90,-1,90,90,90,90,90,90,90,90,-2,90,-1,90,90],
sm64=[0,91,-1,91,-1,91,-4,91,-3,91,-1,91,91,-4,91,91,91,-5,91,-7,91,91,91,91,91,-9,91,91,91,-1,91,91,91,91,-1,91,91,91,91,91,91,91,91,-2,91,-1,91,91],
sm65=[0,92,-1,92,-1,92,-4,0,-3,92,-1,92,92,-4,92,92,92,-5,92,-7,92,92,92,92,92,-9,92,92,92,-1,92,92,92,92,-1,92,92,92,92,92,92,92,92,-2,92],
sm66=[0,-2,93,-1,0,-4,0,-5,93,93,-5,93,-15,93,-12,93,93,93,-1,93,93,93,93,-1,93,-9,93],
sm67=[0,-2,94,-1,0,-4,0,-5,94,94,-5,94,-15,94,-12,94,94,94,-1,94,94,94,94,-1,94,-9,94],
sm68=[0,-4,0,-4,0,-31,95,-10,96,-8,97,98,99,100],
sm69=[0,-4,0,-4,0,-46,36],
sm70=[0,-2,101,-1,0,-4,0,-5,101,101,-4,102,101,-15,101,-12,101,101,101,-1,101,101,101,101,-1,101,-9,101],
sm71=[0,-2,103,-1,0,-4,0,-5,103,103,-5,103,-15,103,-12,103,103,103,-1,103,103,103,103,-1,103,-9,103],
sm72=[0,-2,104,-1,0,-4,0,-5,104,104,-5,104,-15,104,-12,104,104,104,-1,104,104,104,104,-1,104,-9,50],
sm73=[0,-2,105,-1,0,-4,0,-5,105,105,-5,105,-15,105,-12,105,105,105,-1,105,105,105,105,-1,105,-9,105],
sm74=[0,-4,106,-4,0,-38,61,62,63],
sm75=[0,107,-1,2,-1,0,-4,0,-6,107,-3,108,64,-1,107,-7,65,-2,66,-20,107,107,107,107,-1,107,-6,107,-2,107],
sm76=[0,-4,109,-4,0,-38,109,109,109],
sm77=[0,110,-1,110,-1,0,-4,0,-6,110,-3,110,110,-1,110,-7,110,-2,110,-20,110,110,110,110,-1,110,-6,110,-2,110],
sm78=[0,-4,0,-4,0,-3,111],
sm79=[0,-4,0,-4,0,-11,112],
sm80=[0,-4,0,-4,0,-5,113,114],
sm81=[0,115,-1,115,-1,0,-4,0,-5,115,115,-6,115,-31,115,115,115,115,-1,115,-6,115,-2,115],
sm82=[0,116,-1,116,-1,0,-4,0,-5,116,116,-6,116,-31,116,116,116,116,-1,116,-6,116,-2,116],
sm83=[0,116,-1,116,-1,0,-4,0,-5,116,116,-6,116,-5,117,-25,116,116,116,116,-1,116,-6,116,-2,116],
sm84=[0,118,-1,118,-1,0,-4,0,-5,118,118,-5,118,118,-31,118,118,118,118,-1,118,-6,118,-2,118],
sm85=[0,119,-1,119,-1,0,-4,0,-5,119,119,-5,119,119,-31,119,119,119,119,-1,119,-6,119,-2,119],
sm86=[0,119,-1,119,-1,0,-4,0,-5,119,119,-5,119,119,-5,120,121,-24,119,119,119,119,-1,119,-6,119,-2,119],
sm87=[0,-2,2,-1,0,-4,0,-11,64],
sm88=[0,-1,122,2,-1,0,-4,0,-11,64,-9,123],
sm89=[0,124,-1,124,-1,0,-4,0,-5,124,124,-5,124,124,-5,124,124,-24,124,124,124,124,-1,124,-6,124,-2,124],
sm90=[0,125,-1,125,-1,0,-4,0,-5,125,125,-4,126,-1,125,-5,125,-25,125,125,125,125,-1,125,-6,125,-2,125],
sm91=[0,-2,127,-1,0,-4,0],
sm92=[0,-4,0,-4,0,-6,128],
sm93=[0,-4,0,-4,0,-6,129],
sm94=[0,-4,0,-4,0,-6,130],
sm95=[0,-2,2,-1,0,-4,68,-11,69],
sm96=[0,-4,0,-4,0,-6,131,-5,131,-6,132,133],
sm97=[0,-4,0,-4,0,-6,134,-5,134,-6,134,134],
sm98=[0,-4,0,-4,0,-6,135,-5,135,-6,135,135],
sm99=[0,-4,0,-4,0,-11,136],
sm100=[0,-4,0,-4,0,-11,126],
sm101=[0,-2,2,-1,0,-4,0,-8,137,-4,4,-43,138],
sm102=[0,-4,0,-4,0,-5,139,139],
sm103=[0,-4,0,-4,0,-8,140],
sm104=[0,141,-1,141,-1,0,-4,0,-6,141,-1,141,-4,141,-31,141,141,141,141,-1,141,-9,141],
sm105=[0,-2,142,-1,0,-4,0,-8,142,-4,142,-43,142],
sm106=[0,-2,143,-1,0,-4,0,-8,143,-4,143,-43,144],
sm107=[0,-2,2,-1,0,-4,0,-8,145,-4,145,-43,145],
sm108=[0,-2,146,-1,147,-4,0,-3,148,-7,149],
sm109=[0,-2,150,-1,0,-4,0,-5,150,150,-5,150,-15,150,-12,150,150,150,-1,150,150,150,150,-1,150,-9,150],
sm110=[0,-2,151,-1,0,-4,0,-5,151,151,-5,151,-15,151,-12,151,151,151,-1,151,151,151,151,-1,151,-9,151],
sm111=[0,-2,152,-1,0,-4,0,-5,152,152,-5,152,-15,152,-12,152,152,152,-1,152,152,152,152,-1,152,-9,32],
sm112=[0,153,-1,153,-1,153,-4,0,-3,153,-1,153,153,-4,153,153,153,-5,153,-7,153,153,153,153,153,-9,153,153,153,-1,153,153,153,153,-1,153,153,153,153,153,153,153,153,-2,153],
sm113=[0,154,-1,154,-1,154,-4,154,-3,154,-1,154,154,-4,154,154,154,-5,154,-7,154,154,154,154,154,-9,154,154,154,-1,154,154,154,154,-1,154,154,154,154,154,154,154,154,-2,154,-1,154,154],
sm114=[0,-2,155,-1,0,-4,0,-5,155,155,-5,155,-15,155,-12,155,155,155,-1,155,155,155,155,-1,155,-9,155],
sm115=[0,-2,2,156,0,-4,0],
sm116=[0,-4,0,-4,0,-31,157],
sm117=[0,-2,158,158,0,-4,0],
sm118=[0,-2,159,-1,0,-4,0,-5,159,159,-5,159,-15,159,-12,159,159,159,-1,159,159,159,159,-1,159,-9,159],
sm119=[0,-2,160,-1,0,-4,0,-5,160,160,-5,160,-15,160,-12,160,160,160,-1,160,160,160,160,-1,160,-9,160],
sm120=[0,161,-1,2,-1,0,-4,0,-6,161,-3,108,64,-1,161,-7,65,-2,66,-20,161,161,161,161,-1,161,-6,161,-2,161],
sm121=[0,-4,162,-4,0,-38,162,162,162],
sm122=[0,161,-1,2,-1,0,-4,0,-6,161,-4,64,-1,161,-7,65,-2,66,-20,161,161,161,161,-1,161,-6,161,-2,161],
sm123=[0,161,-1,161,-1,0,-4,0,-5,113,161,-6,161,-31,161,161,161,161,-1,161,-6,161,-2,161],
sm124=[0,-4,0,-4,0,-11,163],
sm125=[0,-4,0,-4,0,-3,164,-35,165],
sm126=[0,-4,0,-4,0,-3,166,-35,166,166],
sm127=[0,-4,0,-4,0,-3,164,-36,167],
sm128=[0,-4,0,-4,0,-39,62,63],
sm129=[0,-2,2,-1,0,-4,0,-6,3,-1,168,-36,5,6,7,8,-1,9,-9,10],
sm130=[0,169,-1,169,-1,0,-4,0,-5,169,169,-6,169,-5,117,-25,169,169,169,169,-1,169,-6,169,-2,169],
sm131=[0,125,-1,125,-1,0,-4,0,-5,125,125,-6,125,-5,125,-25,125,125,125,125,-1,125,-6,125,-2,125],
sm132=[0,169,-1,169,-1,0,-4,0,-5,169,169,-6,169,-31,169,169,169,169,-1,169,-6,169,-2,169],
sm133=[0,-2,2,-1,0,-4,0,-11,64,-9,123],
sm134=[0,170,-1,170,-1,0,-4,0,-5,170,170,-5,170,170,-5,120,-25,170,170,170,170,-1,170,-6,170,-2,170],
sm135=[0,171,-1,171,-1,0,-4,0,-5,171,171,-5,171,171,-6,121,-24,171,171,171,171,-1,171,-6,171,-2,171],
sm136=[0,172,-1,172,-1,0,-4,0,-5,172,172,-5,172,172,-5,172,-25,172,172,172,172,-1,172,-6,172,-2,172],
sm137=[0,173,-1,173,-1,0,-4,0,-5,173,173,-5,173,173,-6,173,-24,173,173,173,173,-1,173,-6,173,-2,173],
sm138=[0,174,-1,174,-1,0,-4,0,-5,174,174,-5,174,174,-31,174,174,174,174,-1,174,-6,174,-2,174],
sm139=[0,-4,0,-4,0,-12,175],
sm140=[0,-4,0,-4,0,-12,176],
sm141=[0,-4,177,-4,0,-3,178,-7,126,179,-14,180,180,180,180,180,-28,180],
sm142=[0,-4,0,-4,0,-12,181],
sm143=[0,-4,0,-4,0,-27,182,183,184,185,186,-28,187],
sm144=[0,-4,0,-4,0,-27,188,189,190,191,186],
sm145=[0,-4,0,-4,0,-12,192,-14,192,192,192,192,192,-1,193,-1,194,195,196],
sm146=[0,-4,0,-4,0,-12,192,-14,192,192,192,192,192],
sm147=[0,-4,177,-4,0,-3,178,-8,197],
sm148=[0,-1,198,-2,0,-4,0,-17,199,200],
sm149=[0,-4,0,-4,0,-6,201,-5,201],
sm150=[0,-4,0,-4,0,-6,201,-5,201,-6,132,133],
sm151=[0,-4,0,-4,0,-6,202,-5,202,-6,202,202],
sm152=[0,-2,203,-1,0,-4,203,-11,203],
sm153=[0,-4,0,-4,0,-12,204],
sm154=[0,-4,0,-4,0,-12,205],
sm155=[0,-4,177,-4,0,-3,178,-7,126,179,-47,77],
sm156=[0,-4,0,-4,0,-8,206],
sm157=[0,207,-1,207,-1,0,-4,0,-6,207,-1,207,-4,207,-31,207,207,207,207,-1,207,-9,207],
sm158=[0,208,-1,208,-1,0,-4,0,-6,208,-1,208,-4,208,-31,208,208,208,208,-1,208,-9,208],
sm159=[0,-2,2,-1,0,-4,0,-8,209,-4,209,-43,209],
sm160=[0,-2,210,-1,0,-4,0,-8,210,-4,210,-43,210],
sm161=[0,-2,146,-1,147,-4,0,-3,148,-4,211,-2,149,211,211,-43,211,212],
sm162=[0,-2,146,-1,147,-4,0,-3,148,-4,213,-2,213,213,213,-43,213,213],
sm163=[0,-2,214,-1,214,-4,0,-3,214,-4,214,-2,214,214,214,-43,214,214],
sm164=[0,-2,215,-1,215,-4,0,-3,215,-4,215,-2,215,215,215,-43,215,215],
sm165=[0,-4,0,-4,0,-51,216,-3,217,218],
sm166=[0,-4,0,-4,0,-51,219,-3,219,219],
sm167=[0,-2,220,220,0,-4,0],
sm168=[0,-4,0,-4,0,-12,221],
sm169=[0,222,-1,2,-1,0,-4,0,-6,222,-4,64,-1,222,-7,65,-2,66,-20,222,222,222,222,-1,222,-6,222,-2,222],
sm170=[0,222,-1,222,-1,0,-4,0,-5,113,222,-6,222,-31,222,222,222,222,-1,222,-6,222,-2,222],
sm171=[0,223,-1,223,-1,0,-4,0,-6,223,-3,223,223,223,223,-7,223,-2,223,-20,223,223,223,223,-1,223,-6,223,-2,223],
sm172=[0,-4,0,-4,0,-3,224,-35,224,224],
sm173=[0,-4,0,-4,0,-12,225],
sm174=[0,-4,0,-4,0,-8,226],
sm175=[0,-2,2,-1,0,-4,0,-6,3,-1,227,-36,5,6,7,8,-1,9,-9,10],
sm176=[0,-2,228,-1,0,-4,0,-6,228,-1,228,-36,228,228,228,228,-1,228,-9,228],
sm177=[0,229,-1,229,-1,0,-4,0,-5,229,229,-6,229,-31,229,229,229,229,-1,229,-6,229,-2,229],
sm178=[0,230,-1,230,-1,0,-4,0,-5,230,230,-6,230,-31,230,230,230,230,-1,230,-6,230,-2,230],
sm179=[0,231,-1,231,-1,0,-4,0,-5,231,231,-6,231,-31,231,231,231,231,-1,231,-6,231,-2,231],
sm180=[0,119,-1,119,-1,0,-4,0,-5,119,119,-6,119,-5,120,-25,119,119,119,119,-1,119,-6,119,-2,119],
sm181=[0,232,-1,232,-1,0,-4,0,-5,232,232,-5,232,232,-5,232,-25,232,232,232,232,-1,232,-6,232,-2,232],
sm182=[0,233,-1,233,-1,0,-4,0,-5,233,233,-5,233,233,-6,233,-24,233,233,233,233,-1,233,-6,233,-2,233],
sm183=[0,234,-1,234,-1,0,-4,0,-5,234,234,-5,234,234,-5,234,-25,234,234,234,234,-1,234,-6,234,-2,234],
sm184=[0,235,-1,235,-1,0,-4,0,-5,235,235,-5,235,235,-6,235,-24,235,235,235,235,-1,235,-6,235,-2,235],
sm185=[0,236,-1,236,-1,0,-4,0,-5,236,236,-5,236,236,-5,236,236,-24,236,236,236,236,-1,236,-6,236,-2,236],
sm186=[0,237,-1,237,-1,0,-4,0,-5,237,237,-5,237,237,-5,237,237,-24,237,237,237,237,-1,237,-6,237,-2,237],
sm187=[0,-4,177,-4,0,-3,178,-8,238],
sm188=[0,239,-1,239,-1,0,-4,0,-5,239,239,-5,239,239,-5,239,239,-24,239,239,239,239,-1,239,-6,239,-2,239],
sm189=[0,-4,240,-4,0,-3,240,-8,240],
sm190=[0,-4,241,-4,0,-3,241,-8,241],
sm191=[0,-1,122,2,-1,0,-4,0],
sm192=[0,-1,242,242,-1,0,-4,0],
sm193=[0,-2,242,-1,0,-4,0],
sm194=[0,-4,0,-4,0,-12,243,-14,243,243,243,243,243],
sm195=[0,-1,244,-2,0,-4,0],
sm196=[0,-4,0,-4,0,-12,245,-14,245,245,245,245,245],
sm197=[0,-4,177,-4,0,-3,178,-8,246],
sm198=[0,-1,198,-2,0,-4,0,-8,247,-8,199,200],
sm199=[0,-1,248,-2,0,-4,0,-8,248,-8,248,248],
sm200=[0,-4,0,-4,0,-5,249,250],
sm201=[0,-4,0,-4,0,-5,251,251],
sm202=[0,-4,0,-4,0,-5,252,252],
sm203=[0,-4,0,-4,0,-34,253],
sm204=[0,-4,0,-4,0,-8,254],
sm205=[0,-4,0,-4,0,-6,255,-5,255,-6,255,255],
sm206=[0,-4,0,-4,0,-6,256,-5,256,-6,256,256],
sm207=[0,-4,0,-4,0,-6,257,-5,257,-6,257,257],
sm208=[0,-4,0,-4,0,-6,258,-5,258,-6,258,258],
sm209=[0,-4,0,-4,0,-12,259],
sm210=[0,260,-1,260,-1,0,-4,0,-6,260,-1,260,-4,260,-31,260,260,260,260,-1,260,-9,260],
sm211=[0,-2,261,-1,0,-4,0,-8,261,-3,261,261,-43,261],
sm212=[0,-2,146,-1,147,-4,0,-3,148,-4,262,-2,149,262,262,-43,262,262],
sm213=[0,-4,0,-4,0,-59,263],
sm214=[0,-2,264,-1,264,-4,0,-3,264,-4,264,-2,264,264,264,-43,264,264],
sm215=[0,-2,146,-1,147,-4,0,-3,148,-7,149,265],
sm216=[0,-4,0,-4,0,-51,266],
sm217=[0,-2,267,-1,0,-4,0,-5,267,267,-5,267,-15,267,-12,267,267,267,-1,267,267,267,267,-1,267,-9,267],
sm218=[0,-4,0,-4,0,-51,268],
sm219=[0,-2,269,-1,0,-4,0,-5,269,269,-5,269,-15,269,-12,269,269,269,-1,269,269,269,269,-1,269,-9,269],
sm220=[0,270,-1,270,-1,0,-4,0,-5,113,270,-6,270,-31,270,270,270,270,-1,270,-6,270,-2,270],
sm221=[0,-4,0,-4,0,-12,271],
sm222=[0,-4,0,-4,0,-12,272],
sm223=[0,-4,0,-4,0,-11,126,-48,77],
sm224=[0,273,-1,273,-1,0,-4,0,-6,273,-3,273,273,-1,273,-7,273,-2,273,-20,273,273,273,273,-1,273,-6,273,-2,273],
sm225=[0,-4,0,-4,0,-57,274],
sm226=[0,-2,275,-1,0,-4,0,-6,275,-1,275,-36,275,275,275,275,-1,275,-9,275],
sm227=[0,276,-1,276,-1,0,-4,0,-5,276,276,-5,276,276,-5,276,276,-24,276,276,276,276,-1,276,-6,276,-2,276],
sm228=[0,-4,277,-4,0,-3,277,-8,277],
sm229=[0,-4,0,-4,0,-12,278],
sm230=[0,-4,0,-4,0,-12,192],
sm231=[0,-4,0,-4,0,-12,180],
sm232=[0,-4,0,-4,0,-12,279],
sm233=[0,-4,0,-4,0,-28,280,-1,281],
sm234=[0,-4,0,-4,0,-27,282,-1,283],
sm235=[0,-4,0,-4,0,-12,284,-14,284,284,284,284,284],
sm236=[0,-4,0,-4,0,-57,285],
sm237=[0,-1,286,-2,0,-4,0,-8,286,-8,286,286],
sm238=[0,-4,0,-4,0,-5,287,287],
sm239=[0,-4,0,-4,0,-57,288],
sm240=[0,-4,0,-4,0,-6,289,-5,289,-6,289,289],
sm241=[0,-2,290,-1,0,-4,0,-8,290,-3,290,290,-43,290],
sm242=[0,-2,291,-1,291,-4,0,-3,291,-4,291,-2,291,291,291,-43,291,291],
sm243=[0,-2,292,-1,0,-4,0,-5,292,292,-5,292,-15,292,-12,292,292,292,-1,292,292,292,292,-1,292,-9,292],
sm244=[0,293,-1,293,-1,0,-4,0,-6,293,-4,293,-1,293,-7,293,-2,293,-20,293,293,293,293,-1,293,-6,293,-2,293],
sm245=[0,-1,294,294,-1,0,-4,0],
sm246=[0,-1,295,295,-1,0,-4,0],
sm247=[0,-2,2,-1,0,-4,0,-8,296,-4,4,-43,297],
sm248=[0,-4,0,-4,0,-5,298,298],
sm249=[0,-4,0,-4,0,-12,299],
sm250=[0,-4,0,-4,0,-8,300],
sm251=[0,-1,301,-2,0,-4,0,-8,301,-8,301,301],
sm252=[0,-1,302,-2,0,-4,0,-8,302,-8,302,302],

    // Symbol Lookup map
    lu = new Map([[1,1],[2,2],[4,3],[8,4],[16,5],[32,6],[64,7],[128,8],[256,9],[512,10],[3,11],[264,11],[200,13],[201,14],[",",15],["{",16],[";",67],["}",18],[null,9],["supports",20],["(",21],[")",22],["@",23],["import",24],["keyframes",25],["id",26],["from",27],["to",28],["and",29],["or",30],["not",31],["media",33],["only",34],[":",70],["<",37],[">",38],["<=",39],[">=",40],["=",41],["/",43],["%",44],["px",45],["in",46],["rad",47],["url",48],["\"",49],["'",50],["+",51],["~",52],["||",53],["*",55],["|",56],["#",57],[".",58],["[",60],["]",61],["^=",62],["$=",63],["*=",64],["i",65],["s",66],["!",68],["important",69],["-",72],["_",73]]),

    //Reverse Symbol Lookup map
    rlu = new Map([[1,1],[2,2],[3,4],[4,8],[5,16],[6,32],[7,64],[8,128],[9,256],[10,512],[11,3],[11,264],[13,200],[14,201],[15,","],[16,"{"],[67,";"],[18,"}"],[9,null],[20,"supports"],[21,"("],[22,")"],[23,"@"],[24,"import"],[25,"keyframes"],[26,"id"],[27,"from"],[28,"to"],[29,"and"],[30,"or"],[31,"not"],[33,"media"],[34,"only"],[70,":"],[37,"<"],[38,">"],[39,"<="],[40,">="],[41,"="],[43,"/"],[44,"%"],[45,"px"],[46,"in"],[47,"rad"],[48,"url"],[49,"\""],[50,"'"],[51,"+"],[52,"~"],[53,"||"],[55,"*"],[56,"|"],[57,"#"],[58,"."],[60,"["],[61,"]"],[62,"^="],[63,"$="],[64,"*="],[65,"i"],[66,"s"],[68,"!"],[69,"important"],[72,"-"],[73,"_"]]),

    // States 
    state = [sm0,
sm1,
sm2,
sm3,
sm4,
sm5,
sm6,
sm7,
sm8,
sm8,
sm9,
sm10,
sm11,
sm12,
sm13,
sm13,
sm14,
sm15,
sm16,
sm17,
sm18,
sm19,
sm20,
sm21,
sm22,
sm23,
sm24,
sm24,
sm24,
sm24,
sm25,
sm25,
sm26,
sm27,
sm28,
sm29,
sm30,
sm31,
sm32,
sm33,
sm34,
sm35,
sm36,
sm37,
sm38,
sm39,
sm40,
sm41,
sm10,
sm42,
sm43,
sm44,
sm45,
sm46,
sm47,
sm48,
sm49,
sm50,
sm42,
sm51,
sm52,
sm52,
sm52,
sm52,
sm53,
sm54,
sm54,
sm55,
sm56,
sm57,
sm58,
sm59,
sm60,
sm61,
sm62,
sm63,
sm64,
sm64,
sm64,
sm65,
sm65,
sm66,
sm67,
sm68,
sm25,
sm69,
sm70,
sm71,
sm25,
sm72,
sm73,
sm37,
sm37,
sm37,
sm74,
sm75,
sm76,
sm77,
sm77,
sm78,
sm78,
sm79,
sm80,
sm81,
sm82,
sm25,
sm83,
sm84,
sm84,
sm85,
sm85,
sm86,
sm87,
sm88,
sm89,
sm89,
sm90,
sm91,
sm92,
sm93,
sm93,
sm94,
sm95,
sm96,
sm41,
sm97,
sm97,
sm98,
sm98,
sm99,
sm100,
sm101,
sm102,
sm103,
sm104,
sm105,
sm106,
sm107,
sm108,
sm109,
sm110,
sm111,
sm112,
sm113,
sm114,
sm115,
sm116,
sm117,
sm117,
sm117,
sm117,
sm118,
sm42,
sm119,
sm120,
sm121,
sm122,
sm123,
sm124,
sm125,
sm126,
sm127,
sm128,
sm129,
sm39,
sm130,
sm131,
sm132,
sm133,
sm134,
sm135,
sm136,
sm87,
sm137,
sm87,
sm138,
sm139,
sm140,
sm141,
sm87,
sm142,
sm142,
sm142,
sm143,
sm144,
sm145,
sm146,
sm146,
sm147,
sm148,
sm129,
sm149,
sm150,
sm151,
sm95,
sm152,
sm152,
sm153,
sm154,
sm155,
sm42,
sm156,
sm157,
sm158,
sm159,
sm160,
sm161,
sm162,
sm108,
sm163,
sm164,
sm164,
sm164,
sm165,
sm166,
sm166,
sm167,
sm168,
sm169,
sm170,
sm170,
sm41,
sm171,
sm172,
sm171,
sm173,
sm174,
sm175,
sm176,
sm177,
sm178,
sm179,
sm180,
sm181,
sm182,
sm183,
sm184,
sm185,
sm186,
sm187,
sm188,
sm189,
sm190,
sm190,
sm191,
sm191,
sm192,
sm192,
sm192,
sm192,
sm192,
sm25,
sm25,
sm25,
sm193,
sm193,
sm193,
sm193,
sm194,
sm195,
sm196,
sm196,
sm196,
sm197,
sm188,
sm198,
sm199,
sm200,
sm201,
sm202,
sm202,
sm202,
sm203,
sm204,
sm205,
sm206,
sm207,
sm208,
sm209,
sm210,
sm211,
sm212,
sm213,
sm214,
sm215,
sm216,
sm217,
sm218,
sm218,
sm219,
sm220,
sm221,
sm222,
sm222,
sm223,
sm224,
sm225,
sm226,
sm227,
sm228,
sm229,
sm230,
sm231,
sm232,
sm232,
sm233,
sm234,
sm235,
sm227,
sm236,
sm237,
sm10,
sm148,
sm238,
sm239,
sm240,
sm241,
sm242,
sm243,
sm244,
sm191,
sm245,
sm245,
sm191,
sm246,
sm246,
sm247,
sm248,
sm249,
sm249,
sm250,
sm251,
sm252],

/************ Functions *************/

    max = Math.max, min = Math.min,

    //Error Functions
    e$1 = (tk,r,o,l,p)=>{if(l.END)l.throw("Unexpected end of input");else if(l.ty & (264)) l.throw(`Unexpected space character within input "${p.slice(l)}" `) ; else l.throw(`Unexpected token ${l.tx}" `);}, 
    eh = [e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1,
e$1],

    //Empty Function
    nf = ()=>-1, 

    //Environment Functions
    
redv = (ret, fn, plen, ln, t, e, o, l, s) => {        ln = max(o.length - plen, 0);        const slice = o.slice(-plen);        o.length = ln + 1;        o[ln] = fn(slice, e, l, s, o, plen);        return ret;    },
rednv = (ret, Fn, plen, ln, t, e, o, l, s) => {        ln = max(o.length - plen, 0);        const slice = o.slice(-plen);        o.length = ln + 1;        o[ln] = new Fn(slice, e, l, s, o, plen);        return ret;    },
redn = (ret, plen, t, e, o) => {        if (plen > 0) {            let ln = max(o.length - plen, 0);            o[ln] = o[o.length - 1];            o.length = ln + 1;        }        return ret;    },
shftf = (ret, fn, t, e, o, l, s) => (fn(o, e, l, s), ret),
R20_STYLE_SHEET201_group_list=function (sym,env,lex,state,output,len) {return ((sym[1] !== null) ? sym[0].push(sym[1]) : null,sym[0])},
R21_STYLE_SHEET201_group_list=function (sym,env,lex,state,output,len) {return (sym[0] !== null) ? [sym[0]] : []},
R50_STYLE_SHEET=function (sym,env,lex,state,output,len) {return new fn.ruleset(sym[0],sym[1])},
R51_STYLE_SHEET=function (sym,env,lex,state,output,len) {return new fn.ruleset(null,sym[0])},
R52_STYLE_SHEET=function (sym,env,lex,state,output,len) {return new fn.ruleset(sym[0],null)},
R53_STYLE_SHEET=function (sym,env,lex,state,output,len) {return new fn.ruleset(null,null)},
R60_COMPLEX_SELECTOR_list=function (sym,env,lex,state,output,len) {return ((sym[1] !== null) ? sym[0].push(sym[2]) : null,sym[0])},
R70_STYLE_RULE=function (sym,env,lex,state,output,len) {return new fn.stylerule(sym[0],sym[2])},
R71_STYLE_RULE=function (sym,env,lex,state,output,len) {return new fn.stylerule(null,sym[1])},
C180_keyframes=function (sym,env,lex,state,output,len) {this.keyframes = sym[4];},
C210_keyframes_blocks=function (sym,env,lex,state,output,len) {this.selectors = sym[0];this.props = sym[2].props;},
R500_general_enclosed6202_group_list=function (sym,env,lex,state,output,len) {return sym[0] + sym[1]},
R501_general_enclosed6202_group_list=function (sym,env,lex,state,output,len) {return sym[0] + ""},
R790_TYPE_SELECTOR=function (sym,env,lex,state,output,len) {return new fn.type_selector([sym[0],sym[1]])},
R791_TYPE_SELECTOR=function (sym,env,lex,state,output,len) {return new fn.type_selector([sym[0]])},
R820_WQ_NAME=function (sym,env,lex,state,output,len) {return [sym[0],sym[1]]},
R821_WQ_NAME=function (sym,env,lex,state,output,len) {return [sym[0]]},
R960_declaration_list=function (sym,env,lex,state,output,len) {return sym[0]},
R961_declaration_list=function (sym,env,lex,state,output,len) {return (sym[0].push(sym[1]),sym[0])},
R962_declaration_list=function (sym,env,lex,state,output,len) {return (sym[0].push(...sym[1]),sym[0])},
R1010_declaration_values=function (sym,env,lex,state,output,len) {return sym.join("")},

    //Sparse Map Lookup
    lsm = (index, map) => {    if (map[0] == 0xFFFFFFFF) return map[index + 1];    for (let i = 1, ind = 0, l = map.length, n = 0; i < l && ind <= index; i++) {        if (ind !== index) {            if ((n = map[i]) > -1) ind++;            else ind += -n;        } else return map[i];    }    return -1;},

    //State Action Functions
    state_funct = [(...v)=>((redn(5123,0,...v))),
()=>(98),
()=>(46),
()=>(26),
()=>(78),
()=>(90),
()=>(122),
()=>(126),
()=>(130),
()=>(134),
(...v)=>(rednv(5,fn.stylesheet,1,0,...v)),
(...v)=>(redv(5127,R52_STYLE_SHEET,1,0,...v)),
(...v)=>(redv(5127,R51_STYLE_SHEET,1,0,...v)),
(...v)=>(redv(2055,R21_STYLE_SHEET201_group_list,1,0,...v)),
(...v)=>(redn(1031,1,...v)),
()=>(174),
()=>(190),
()=>(178),
()=>(186),
()=>(182),
(...v)=>(redv(4103,R21_STYLE_SHEET201_group_list,1,0,...v)),
(...v)=>(redn(3079,1,...v)),
()=>(198),
()=>(194),
(...v)=>(redv(6151,R21_STYLE_SHEET201_group_list,1,0,...v)),
(...v)=>(rednv(73735,fn.selector,1,0,...v)),
()=>(242),
()=>(246),
()=>(250),
()=>(254),
(...v)=>(rednv(78855,fn.compoundSelector,1,0,...v)),
()=>(278),
(...v)=>(rednv(80903,fn.typeselector,1,0,...v)),
()=>(282),
(...v)=>(redv(80903,R791_TYPE_SELECTOR,1,0,...v)),
(...v)=>(redn(81927,1,...v)),
(...v)=>(redv(83975,R821_WQ_NAME,1,0,...v)),
()=>(290),
(...v)=>(redn(82951,1,...v)),
(...v)=>(redv(108551,R960_declaration_list,1,0,...v)),
()=>(306),
()=>(318),
()=>(322),
()=>(310),
()=>(314),
(...v)=>(redn(104455,1,...v)),
(...v)=>(redv(74759,R21_STYLE_SHEET201_group_list,1,0,...v)),
(...v)=>(redn(84999,1,...v)),
()=>(342),
()=>(354),
(...v)=>(redv(77831,R21_STYLE_SHEET201_group_list,1,0,...v)),
(...v)=>(redn(76807,1,...v)),
()=>(366),
()=>(370),
()=>(374),
(...v)=>(redv(5131,R50_STYLE_SHEET,2,0,...v)),
(...v)=>(redv(2059,R20_STYLE_SHEET201_group_list,2,0,...v)),
(...v)=>(redv(4107,R20_STYLE_SHEET201_group_list,2,0,...v)),
(...v)=>(redn(10251,2,...v)),
()=>(386),
()=>(406),
()=>(398),
()=>(402),
()=>(454),
()=>(450),
()=>(470),
()=>(478),
()=>(518),
()=>(498),
()=>(490),
()=>(538),
()=>(534),
(...v)=>(redv(98311,R960_declaration_list,1,0,...v)),
()=>(550),
(...v)=>(redv(97287,R21_STYLE_SHEET201_group_list,1,0,...v)),
(...v)=>(redn(95239,1,...v)),
()=>(554),
(...v)=>(rednv(73739,fn.selector,2,0,...v)),
(...v)=>(redv(72711,R21_STYLE_SHEET201_group_list,1,0,...v)),
(...v)=>(rednv(71687,fn.comboSelector,1,0,...v)),
(...v)=>(redn(79879,1,...v)),
(...v)=>(rednv(78859,fn.compoundSelector,2,0,...v)),
(...v)=>(redv(74763,R20_STYLE_SHEET201_group_list,2,0,...v)),
(...v)=>(redv(77835,R20_STYLE_SHEET201_group_list,2,0,...v)),
(...v)=>(redv(80907,R790_TYPE_SELECTOR,2,0,...v)),
(...v)=>(redv(83979,R820_WQ_NAME,2,0,...v)),
(...v)=>(redn(82955,2,...v)),
(...v)=>(redv(108555,R500_general_enclosed6202_group_list,2,0,...v)),
(...v)=>(redv(108555,R960_declaration_list,2,0,...v)),
(...v)=>(redv(106503,R501_general_enclosed6202_group_list,1,0,...v)),
(...v)=>(redn(105479,1,...v)),
(...v)=>(redn(107527,1,...v)),
(...v)=>(rednv(86027,fn.idSelector,2,0,...v)),
(...v)=>(rednv(87051,fn.classSelector,2,0,...v)),
()=>(602),
()=>(586),
()=>(578),
()=>(590),
()=>(594),
()=>(598),
(...v)=>(rednv(93195,fn.pseudoClassSelector,2,0,...v)),
()=>(610),
(...v)=>(rednv(94219,fn.pseudoElementSelector,2,0,...v)),
(...v)=>(redn(76811,2,...v)),
(...v)=>(redv(75783,R21_STYLE_SHEET201_group_list,1,0,...v)),
()=>(622),
(...v)=>(redn(16399,3,...v)),
()=>(634),
(...v)=>(redv(11271,R21_STYLE_SHEET201_group_list,1,0,...v)),
(...v)=>(redn(12295,1,...v)),
()=>(642),
()=>(650),
()=>(658),
()=>(654),
(...v)=>(redv(32775,R21_STYLE_SHEET201_group_list,1,0,...v)),
(...v)=>(redn(36871,1,...v)),
()=>(674),
(...v)=>(redn(38919,1,...v)),
(...v)=>(redn(37895,1,...v)),
()=>(690),
()=>(698),
()=>(742),
()=>(718),
(...v)=>(redn(47111,1,...v)),
(...v)=>(redn(62471,1,...v)),
()=>(754),
(...v)=>(redn(34823,1,...v)),
()=>(758),
(...v)=>(redn(19463,1,...v)),
()=>(762),
(...v)=>(redn(27655,1,...v)),
()=>(782),
()=>(786),
(...v)=>(redn(28679,1,...v)),
(...v)=>(redn(29703,1,...v)),
()=>(802),
()=>(810),
()=>(806),
(...v)=>(redv(6159,R60_COMPLEX_SELECTOR_list,3,0,...v)),
()=>(814),
(...v)=>(redv(7183,R71_STYLE_RULE,3,0,...v)),
(...v)=>(redv(98315,R961_declaration_list,2,0,...v)),
(...v)=>(redv(98315,R962_declaration_list,2,0,...v)),
()=>(818),
(...v)=>(redv(98315,R960_declaration_list,2,0,...v)),
()=>(850),
()=>(842),
()=>(846),
()=>(834),
(...v)=>(redv(72715,R20_STYLE_SHEET201_group_list,2,0,...v)),
(...v)=>(rednv(71691,fn.comboSelector,2,0,...v)),
(...v)=>(rednv(78863,fn.compoundSelector,3,0,...v)),
(...v)=>(redv(108559,R500_general_enclosed6202_group_list,3,0,...v)),
(...v)=>(redv(106507,R500_general_enclosed6202_group_list,2,0,...v)),
(...v)=>(rednv(89103,fn.attribSelector,3,0,...v)),
()=>(862),
()=>(866),
(...v)=>(redn(90119,1,...v)),
(...v)=>(rednv(93199,fn.pseudoClassSelector,3,0,...v)),
(...v)=>(redv(75787,R20_STYLE_SHEET201_group_list,2,0,...v)),
(...v)=>(redn(16403,4,...v)),
(...v)=>(redv(11275,R20_STYLE_SHEET201_group_list,2,0,...v)),
()=>(886),
()=>(894),
()=>(890),
(...v)=>(redv(69639,R501_general_enclosed6202_group_list,1,0,...v)),
()=>(898),
(...v)=>((redn(9219,0,...v))),
(...v)=>(redn(36875,2,...v)),
(...v)=>(redn(43019,2,...v)),
(...v)=>(redn(46091,2,...v)),
(...v)=>(redv(41991,R21_STYLE_SHEET201_group_list,1,0,...v)),
(...v)=>(redv(45063,R21_STYLE_SHEET201_group_list,1,0,...v)),
(...v)=>(redn(39947,2,...v)),
()=>(950),
()=>(954),
()=>(974),
()=>(970),
()=>(962),
(...v)=>(redn(61447,1,...v)),
(...v)=>(redn(48135,1,...v)),
()=>(986),
()=>(990),
()=>(994),
()=>(998),
()=>(1002),
()=>(978),
()=>(1018),
()=>(1022),
()=>(1026),
()=>(1030),
(...v)=>(redn(59399,1,...v)),
()=>(1038),
()=>(1042),
()=>(1046),
()=>(1050),
()=>(1058),
()=>(1090),
()=>(1078),
()=>(1082),
(...v)=>(redn(27659,2,...v)),
(...v)=>(redv(26631,R21_STYLE_SHEET201_group_list,1,0,...v)),
(...v)=>(redn(24583,1,...v)),
()=>(1106),
()=>(1110),
()=>(1118),
(...v)=>(redv(7187,R70_STYLE_RULE,4,0,...v)),
(...v)=>(redv(7187,R71_STYLE_RULE,4,0,...v)),
(...v)=>(redv(98319,R962_declaration_list,3,0,...v)),
(...v)=>(redv(97295,R60_COMPLEX_SELECTOR_list,3,0,...v)),
(...v)=>(redv(100367,fn.parseDeclaration,3,0,...v)),
()=>(1130),
(...v)=>(redn(103431,1,...v)),
(...v)=>(redv(102407,R501_general_enclosed6202_group_list,1,0,...v)),
(...v)=>(redn(101383,1,...v)),
()=>(1146),
()=>(1150),
()=>(1154),
(...v)=>(redn(88071,1,...v)),
(...v)=>(redn(90123,2,...v)),
()=>(1158),
(...v)=>(redn(16407,5,...v)),
(...v)=>(redn(70671,3,...v)),
(...v)=>(redv(69643,R500_general_enclosed6202_group_list,2,0,...v)),
()=>(1182),
()=>(1186),
(...v)=>(redn(9223,1,...v)),
(...v)=>(redv(8199,R21_STYLE_SHEET201_group_list,1,0,...v)),
(...v)=>(redv(32783,R60_COMPLEX_SELECTOR_list,3,0,...v)),
(...v)=>(redn(36879,3,...v)),
(...v)=>(redn(35851,2,...v)),
(...v)=>(redv(41995,R20_STYLE_SHEET201_group_list,2,0,...v)),
(...v)=>(redv(45067,R20_STYLE_SHEET201_group_list,2,0,...v)),
(...v)=>(redn(40971,2,...v)),
(...v)=>(redn(44043,2,...v)),
(...v)=>(redn(47119,3,...v)),
(...v)=>(redn(49167,3,...v)),
()=>(1194),
(...v)=>(redn(53263,3,...v)),
(...v)=>(redv(52231,R501_general_enclosed6202_group_list,1,0,...v)),
(...v)=>(redn(50183,1,...v)),
(...v)=>(redn(55303,1,...v)),
(...v)=>(redn(66571,2,...v)),
()=>(1230),
(...v)=>(redn(65543,1,...v)),
()=>(1234),
()=>(1238),
(...v)=>(redv(17415,R21_STYLE_SHEET201_group_list,1,0,...v)),
()=>(1250),
()=>(1246),
(...v)=>(redv(20487,R21_STYLE_SHEET201_group_list,1,0,...v)),
(...v)=>(redn(22535,1,...v)),
()=>(1254),
()=>(1258),
(...v)=>(redv(26635,R20_STYLE_SHEET201_group_list,2,0,...v)),
(...v)=>(redn(25611,2,...v)),
(...v)=>(redn(28687,3,...v)),
(...v)=>(redn(30735,3,...v)),
()=>(1262),
(...v)=>(redv(7191,R70_STYLE_RULE,5,0,...v)),
(...v)=>(redv(100371,fn.parseDeclaration,4,0,...v)),
(...v)=>(redv(103435,R1010_declaration_values,2,0,...v)),
()=>(1266),
(...v)=>(redv(102411,R500_general_enclosed6202_group_list,2,0,...v)),
()=>(1270),
()=>(1274),
(...v)=>(rednv(89111,fn.attribSelector,5,0,...v)),
(...v)=>(redn(91143,1,...v)),
(...v)=>(redn(92175,3,...v)),
(...v)=>(redn(16411,6,...v)),
()=>(1278),
(...v)=>(redn(13319,1,...v)),
(...v)=>(redn(67603,4,...v)),
(...v)=>(redn(33819,6,...v)),
(...v)=>(redv(8203,R20_STYLE_SHEET201_group_list,2,0,...v)),
(...v)=>(redn(53267,4,...v)),
(...v)=>(redv(52235,R500_general_enclosed6202_group_list,2,0,...v)),
(...v)=>(redn(54287,3,...v)),
(...v)=>(redn(58383,3,...v)),
()=>(1286),
()=>(1290),
()=>(1298),
()=>(1302),
(...v)=>(redn(63503,3,...v)),
(...v)=>(rednv(18459,C180_keyframes,6,0,...v)),
(...v)=>(redv(17419,R20_STYLE_SHEET201_group_list,2,0,...v)),
(...v)=>(redn(64523,2,...v)),
(...v)=>(redn(23579,6,...v)),
(...v)=>(redn(31763,4,...v)),
(...v)=>(redn(99339,2,...v)),
(...v)=>(redv(103439,R1010_declaration_values,3,0,...v)),
(...v)=>(rednv(89115,fn.attribSelector,6,0,...v)),
(...v)=>(redn(14355,4,...v)),
(...v)=>(redn(56327,1,...v)),
(...v)=>(redn(57351,1,...v)),
()=>(1326),
()=>(1322),
(...v)=>(redv(20495,R60_COMPLEX_SELECTOR_list,3,0,...v)),
(...v)=>(redn(58391,5,...v)),
()=>(1330),
(...v)=>(rednv(21523,C210_keyframes_blocks,4,0,...v)),
(...v)=>(rednv(21527,C210_keyframes_blocks,5,0,...v))],

    //Goto Lookup Functions
    goto = [v=>lsm(v,gt0),
nf,
v=>lsm(v,gt1),
v=>lsm(v,gt2),
nf,
nf,
nf,
nf,
nf,
nf,
nf,
v=>lsm(v,gt3),
nf,
v=>lsm(v,gt4),
v=>lsm(v,gt5),
v=>lsm(v,gt6),
v=>lsm(v,gt7),
nf,
v=>lsm(v,gt8),
nf,
nf,
nf,
nf,
v=>lsm(v,gt9),
nf,
nf,
nf,
nf,
nf,
nf,
v=>lsm(v,gt10),
v=>lsm(v,gt11),
v=>lsm(v,gt12),
v=>lsm(v,gt13),
nf,
v=>lsm(v,gt14),
nf,
nf,
nf,
v=>lsm(v,gt2),
nf,
nf,
nf,
nf,
v=>lsm(v,gt15),
v=>lsm(v,gt16),
v=>lsm(v,gt17),
v=>lsm(v,gt18),
v=>lsm(v,gt19),
v=>lsm(v,gt20),
v=>lsm(v,gt21),
nf,
nf,
nf,
nf,
nf,
v=>lsm(v,gt22),
nf,
v=>lsm(v,gt23),
nf,
nf,
nf,
nf,
nf,
v=>lsm(v,gt24),
v=>lsm(v,gt7),
v=>lsm(v,gt7),
nf,
nf,
v=>lsm(v,gt25),
nf,
nf,
nf,
v=>lsm(v,gt26),
nf,
nf,
nf,
nf,
nf,
nf,
nf,
nf,
nf,
v=>lsm(v,gt27),
v=>lsm(v,gt8),
nf,
v=>lsm(v,gt28),
nf,
v=>lsm(v,gt29),
v=>lsm(v,gt30),
nf,
nf,
nf,
nf,
v=>lsm(v,gt31),
v=>lsm(v,gt32),
nf,
nf,
nf,
v=>lsm(v,gt33),
v=>lsm(v,gt34),
nf,
nf,
nf,
nf,
v=>lsm(v,gt35),
v=>lsm(v,gt36),
nf,
nf,
nf,
nf,
v=>lsm(v,gt37),
v=>lsm(v,gt38),
v=>lsm(v,gt39),
nf,
nf,
nf,
nf,
nf,
nf,
nf,
nf,
v=>lsm(v,gt40),
v=>lsm(v,gt41),
v=>lsm(v,gt42),
nf,
nf,
nf,
nf,
nf,
nf,
v=>lsm(v,gt21),
nf,
nf,
nf,
nf,
nf,
v=>lsm(v,gt43),
v=>lsm(v,gt44),
nf,
nf,
v=>lsm(v,gt7),
nf,
nf,
nf,
v=>lsm(v,gt45),
nf,
nf,
nf,
nf,
nf,
nf,
v=>lsm(v,gt46),
nf,
v=>lsm(v,gt47),
nf,
v=>lsm(v,gt48),
nf,
nf,
nf,
nf,
nf,
v=>lsm(v,gt49),
v=>lsm(v,gt50),
v=>lsm(v,gt51),
v=>lsm(v,gt52),
nf,
nf,
v=>lsm(v,gt53),
v=>lsm(v,gt54),
v=>lsm(v,gt55),
nf,
v=>lsm(v,gt56),
nf,
v=>lsm(v,gt57),
nf,
nf,
nf,
v=>lsm(v,gt58),
v=>lsm(v,gt38),
nf,
nf,
nf,
v=>lsm(v,gt59),
v=>lsm(v,gt60),
v=>lsm(v,gt61),
nf,
nf,
v=>lsm(v,gt62),
v=>lsm(v,gt63),
v=>lsm(v,gt64),
nf,
v=>lsm(v,gt65),
nf,
v=>lsm(v,gt66),
nf,
nf,
nf,
nf,
v=>lsm(v,gt58),
v=>lsm(v,gt67),
nf,
nf,
nf,
v=>lsm(v,gt43),
nf,
v=>lsm(v,gt68),
v=>lsm(v,gt69),
v=>lsm(v,gt70),
nf,
nf,
nf,
nf,
v=>lsm(v,gt71),
nf,
nf,
nf,
nf,
v=>lsm(v,gt72),
nf,
nf,
v=>lsm(v,gt73),
nf,
nf,
nf,
nf,
nf,
v=>lsm(v,gt74),
nf,
nf,
nf,
nf,
v=>lsm(v,gt75),
nf,
nf,
nf,
nf,
nf,
nf,
v=>lsm(v,gt76),
nf,
nf,
nf,
nf,
v=>lsm(v,gt77),
v=>lsm(v,gt78),
nf,
nf,
nf,
nf,
nf,
v=>lsm(v,gt79),
v=>lsm(v,gt80),
v=>lsm(v,gt81),
nf,
nf,
nf,
nf,
nf,
nf,
nf,
nf,
nf,
v=>lsm(v,gt76),
nf,
v=>lsm(v,gt82),
nf,
nf,
nf,
nf,
nf,
nf,
nf,
nf,
nf,
nf,
nf,
nf,
nf,
nf,
nf,
v=>lsm(v,gt83),
nf,
nf,
v=>lsm(v,gt83),
nf,
nf,
nf,
nf,
nf,
nf,
nf,
nf,
nf,
nf,
nf,
nf,
nf,
nf,
nf,
nf,
nf,
nf,
nf,
nf,
v=>lsm(v,gt84),
v=>lsm(v,gt85),
nf,
nf,
nf,
nf,
v=>lsm(v,gt86),
v=>lsm(v,gt87),
nf,
nf,
nf,
nf,
nf,
nf,
nf,
v=>lsm(v,gt88),
nf,
nf,
v=>lsm(v,gt89),
nf,
nf,
v=>lsm(v,gt21),
nf,
nf,
nf,
nf,
nf,
nf];

function getToken(l, SYM_LU) {
    if (l.END) return 0; /*$eof*/

    switch (l.ty) {
        case 2:
            //*
            if (SYM_LU.has(l.tx)) return  14;
            /*/
                console.log(l.tx, SYM_LU.has(l.tx), SYM_LU.get(l.tx))
                if (SYM_LU.has(l.tx)) return SYM_LU.get(l.tx);
            //*/
            return 2;
        case 1:
            return 1;
        case 4:
            return 3;
        case 256:
            return 9;
        case 8:
            return 4;
        case 512:
            return 10;
        default:
            return SYM_LU.get(l.tx) || SYM_LU.get(l.ty);
    }
}

/************ Parser *************/

function parser(l, e = {}) {

    fn = e.functions;

    l.IWS = false;
    l.PARSE_STRING = true;

    if (symbols.length > 0) {
        symbols.forEach(s => { l.addSymbol(s); });
        l.tl = 0;
        l.next();
    }

    const recovery_chain = [];

    const o = [],
        ss = [0, 0];

    let time = 1000000,
        RECOVERING = 100,
        RESTARTED = true,
        tk = getToken(l, lu),
        p = l.copy(),
        sp = 1,
        len = 0,
        reduceStack = (e.reduceStack = []),
        ROOT = 10000,
        off = 0;

    outer:

        while (time-- > 0) {

            const fn = lsm(tk, state[ss[sp]]) || 0;

            let r,
                gt = -1;

            if (fn == 0) {
                /*Ignore the token*/
                tk = getToken(l.next(), lu);
                continue;
            }

            if (fn > 0) {
                r = state_funct[fn - 1](tk, e, o, l, ss[sp - 1]);
            } else {

                if (tk == 14) {
                    tk = lu.get(l.tx);
                    continue;
                }

                if (l.ty == 8 && l.tl > 1) {
                    // Make sure that special tokens are not getting in the way
                    l.tl = 0;
                    // This will skip the generation of a custom symbol
                    l.next(l, false);

                    if (l.tl == 1)
                        continue;
                }

                if (RECOVERING > 1 && !l.END) {

                    if (tk !== lu.get(l.ty)) {
                        tk = lu.get(l.ty);
                        continue;
                    }

                    if (tk !== 13) {
                        tk = 13;
                        RECOVERING = 1;
                        continue;
                    }
                }

                tk = getToken(l, lu);

                const recovery_token = eh[ss[sp]](tk, e, o, l, p, ss[sp], (lex) => getToken(lex, lu));

                if (RECOVERING > 0 && recovery_token >= 0) {
                    RECOVERING = -1; /* To prevent infinite recursion */
                    tk = recovery_token;
                    l.tl = 0; /*reset current token */
                    continue;
                }
            }

            switch (r & 3) {
                case 0:
                    /* ERROR */

                    if (tk == "$eof")
                        l.throw("Unexpected end of input");

                    l.throw(`Unexpected token [${RECOVERING ? l.next().tx : l.tx}]`);
                    return [null];

                case 1:
                    /* ACCEPT */
                    break outer;

                case 2:

                    /*SHIFT */
                    o.push(l.tx);
                    ss.push(off, r >> 2);
                    sp += 2;
                    l.next();
                    off = l.off;
                    tk = getToken(l, lu);
                    RECOVERING++;
                    break;

                case 3:
                    /* REDUCE */
                    RESTARTED = true;

                    len = (r & 0x3FC) >> 1;

                    ss.length -= len;
                    sp -= len;
                    gt = goto[ss[sp]](r >> 10);

                    if (gt < 0)
                        l.throw("Invalid state reached!");

                    if (reduceStack.length > 0) {
                        let i = reduceStack.length - 1;
                        while (i > -1) {
                            let item = reduceStack[i--];

                            if (item.index == sp) {
                                item.action(output);
                            } else if (item.index > sp) {
                                reduceStack.length--;
                            } else {
                                break;
                            }
                        }
                    }

                    ss.push(off, gt);
                    sp += 2;
                    break;
            }
        }
    return o[0];
};

class Color extends Float64Array {

    constructor(r, g, b, a = 0) {
        super(4);

        this.r = 0;
        this.g = 0;
        this.b = 0;
        this.a = 1;

        if (typeof(r) === "number") {
            this.r = r; //Math.max(Math.min(Math.round(r),255),-255);
            this.g = g; //Math.max(Math.min(Math.round(g),255),-255);
            this.b = b; //Math.max(Math.min(Math.round(b),255),-255);
            this.a = a; //Math.max(Math.min(a,1),-1);
        }
    }

    get r() {
        return this[0];
    }

    set r(r) {
        this[0] = r;
    }

    get g() {
        return this[1];
    }

    set g(g) {
        this[1] = g;
    }

    get b() {
        return this[2];
    }

    set b(b) {
        this[2] = b;
    }

    get a() {
        return this[3];
    }

    set a(a) {
        this[3] = a;
    }

    set(color) {
        this.r = color.r;
        this.g = color.g;
        this.b = color.b;
        this.a = (color.a != undefined) ? color.a : this.a;
    }

    add(color) {
        return new Color(
            color.r + this.r,
            color.g + this.g,
            color.b + this.b,
            color.a + this.a
        );
    }

    mult(color) {
        if (typeof(color) == "number") {
            return new Color(
                this.r * color,
                this.g * color,
                this.b * color,
                this.a * color
            );
        } else {
            return new Color(
                this.r * color.r,
                this.g * color.g,
                this.b * color.b,
                this.a * color.a
            );
        }
    }

    sub(color) {
        return new Color(
            this.r - color.r,
            this.g - color.g,
            this.b - color.b,
            this.a - color.a
        );
    }

    lerp(to, t){
        return this.add(to.sub(this).mult(t));
    }

    toString() {
        return `rgba(${this.r|0},${this.g|0},${this.b|0},${this.a})`;
    }

    toJSON() {
        return `rgba(${this.r|0},${this.g|0},${this.b|0},${this.a})`;
    }

    copy(other){
        let out = new Color(other);
        return out;
    }
}

/*
    BODY {color: black; background: white }
    H1 { color: maroon }
    H2 { color: olive }
    EM { color: #f00 }              // #rgb //
    EM { color: #ff0000 }           // #rrggbb //
    EM { color: rgb(255,0,0) }      // integer range 0 - 255 //
    EM { color: rgb(100%, 0%, 0%) } // float range 0.0% - 100.0% //
*/
class CSS_Color extends Color {
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

        if (typeof(l) == "string")
            l = whind$1(l);

        let out = { r: 0, g: 0, b: 0, a: 1 };

        switch (l.ch) {
            case "#":
                l.next();
                let pk = l.copy();

                let type = l.types;
                pk.IWS = false;


                while(!(pk.ty & (type.newline | type.ws)) && !pk.END && pk.ch !== ";"){
                    pk.next();
                }

                var value = pk.slice(l);
                l.sync(pk);
                l.tl = 0;
                l.next();
                
                let num = parseInt(value,16);

                if(value.length == 3 || value.length == 4){
                    
                    if(value.length == 4){
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

                }else{

                    if(value.length == 8){
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

                const RGB_TYPE = tx === "rgba"  ? 1 : tx === "rgb" ? 2 : 0;
                
                if(RGB_TYPE > 0){

                    l.next(); // (
                    
                    out.r = parseInt(l.next().tx);
                    
                    l.next(); // , or  %

                    if(l.ch == "%"){
                        l.next(); out.r = out.r * 255 / 100;
                    }
                    
                    
                    out.g = parseInt(l.next().tx);
                    
                    l.next(); // , or  %
                   
                    if(l.ch == "%"){
                        l.next(); out.g = out.g * 255 / 100;
                    }
                    
                    
                    out.b = parseInt(l.next().tx);
                    
                    l.next(); // , or ) or %
                    
                    if(l.ch == "%")
                        l.next(), out.b = out.b * 255 / 100;

                    if(RGB_TYPE < 2){
                        out.a = parseFloat(l.next().tx);

                        l.next();
                        
                        if(l.ch == "%")
                            l.next(), out.a = out.a * 255 / 100;
                    }

                    l.a(")");
                    c = new CSS_Color();
                    c.set(out);
                    return c;
                }  // intentional
            default:

                let string = l.tx;

                if (l.ty == l.types.str){
                    string = string.slice(1, -1);
                }

                out = CSS_Color.colors[string.toLowerCase()];

                if(out)
                    l.next();
        }

        return out;
    }

    constructor(r, g, b, a) {
        super(r, g, b, a);

        if (typeof(r) == "string")
            this.set(CSS_Color._fs_(r) || {r:255,g:255,b:255,a:0});

    }

    toString(){
        if(this.a !== 1)
            return this.toRGBString();
        return `#${("0"+this.r.toString(16)).slice(-2)}${("0"+this.g.toString(16)).slice(-2)}${("0"+this.b.toString(16)).slice(-2)}`
    }
    toRGBString(){
        return `rgba(${this.r.toString()},${this.g.toString()},${this.b.toString()},${this.a.toString()})`   
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
        "yellow": _$(255, 255)
    };
}

class CSS_Percentage extends Number {    
    static parse(l, rule, r) {
        let tx = l.tx,
            pky = l.pk.ty;

        if (l.ty == l.types.num || tx == "-" && pky == l.types.num) {
            let mult = 1;

            if (l.ch == "-") {
                mult = -1;
                tx = l.p.tx;
                l.p.next();
            }

            if (l.p.ch == "%") {
                l.sync().next();
                return new CSS_Percentage(parseFloat(tx) * mult);
            }
        }
        return null;
    }

    static _verify_(l) {
        if(typeof(l) == "string" &&  !isNaN(parseInt(l)) && l.includes("%"))
            return true;
        return false;
    }
    
    constructor(v) {

        if (typeof(v) == "string") {
            let lex = whind(v);
            let val = CSS_Percentage.parse(lex);
            if (val) 
                return val;
        }
        
        super(v);
    }

    toJSON() {
        return super.toString() + "%";
    }

    toString(radix) {
        return super.toString(radix) + "%";
    }

    get str() {
        return this.toString();
    }

    lerp(to, t) {
        return new CSS_Percentage(this + (to - this) * t);
    }

    copy(other){
        return new CSS_Percentage(other);
    }

    get type(){
        return "%";
    }
}

CSS_Percentage.label_name = "Percentage";

class CSS_Length extends Number {

    static parse(l) {
        let tx = l.tx,
            pky = l.pk.ty;
        if (l.ty == l.types.num || tx == "-" && pky == l.types.num) {
            let sign = 1;
            if (l.ch == "-") {
                sign = -1;
                tx = l.p.tx;
                l.p.next();
            }
            if (l.p.ty == l.types.id) {
                let id = l.sync().tx;
                l.next();
                return new CSS_Length(parseFloat(tx) * sign, id);
            }
        }
        return null;
    }

    static _verify_(l) {
        if (typeof(l) == "string" && !isNaN(parseInt(l)) && !l.includes("%")) return true;
        return false;
    }

    constructor(v, u = "") {
        
        if (typeof(v) == "string") {
            let lex = whind$1(v);
            let val = CSS_Length.parse(lex);
            if (val) return val;
        }

        if(u){
            switch(u){
                //Absolute
                case "px": return new PXLength(v);
                case "mm": return new MMLength(v);
                case "cm": return new CMLength(v);
                case "in": return new INLength(v);
                case "pc": return new PCLength(v);
                case "pt": return new PTLength(v);
                
                //Relative
                case "ch": return new CHLength(v);
                case "em": return new EMLength(v);
                case "ex": return new EXLength(v);
                case "rem": return new REMLength(v);

                //View Port
                case "vh": return new VHLength(v);
                case "vw": return new VWLength(v);
                case "vmin": return new VMINLength(v);
                case "vmax": return new VMAXLength(v);

                //Deg
                case "deg": return new DEGLength(v);

                case "%" : return new CSS_Percentage(v);
            }
        }

        super(v);
    }

    get milliseconds() {
        switch (this.unit) {
            case ("s"):
                return parseFloat(this) * 1000;
        }
        return parseFloat(this);
    }

    toString(radix) {
        return super.toString(radix) + "" + this.unit;
    }

    toJSON() {
        return super.toString() + "" + this.unit;
    }

    get str() {
        return this.toString();
    }

    lerp(to, t) {
        return new CSS_Length(this + (to - this) * t, this.unit);
    }

    copy(other) {
        return new CSS_Length(other, this.unit);
    }

    set unit(t){}
    get unit(){return "";}
}

class PXLength extends CSS_Length {
    get unit(){return "px";}
}
class MMLength extends CSS_Length {
    get unit(){return "mm";}
}
class CMLength extends CSS_Length {
    get unit(){return "cm";}
}
class INLength extends CSS_Length {
    get unit(){return "in";}
}
class PTLength extends CSS_Length {
    get unit(){return "pt";}
}
class PCLength extends CSS_Length {
    get unit(){return "pc";}
}
class CHLength extends CSS_Length {
    get unit(){return "ch";}
}
class EMLength extends CSS_Length {
    get unit(){return "em";}
}
class EXLength extends CSS_Length {
    get unit(){return "ex";}
}
class REMLength extends CSS_Length {
    get unit(){return "rem";}
}
class VHLength extends CSS_Length {
    get unit(){return "vh";}
}
class VWLength extends CSS_Length {
    get unit(){return "vw";}
}
class VMINLength extends CSS_Length {
    get unit(){return "vmin";}
}
class VMAXLength extends CSS_Length {
    get unit(){return "vmax";}
}
class DEGLength extends CSS_Length {
    get unit(){return "deg";}
}

const A$1 = 65;
const a$1 = 97;
const ACKNOWLEDGE$1 = 6;
const AMPERSAND$1 = 38;
const ASTERISK$1 = 42;
const AT$1 = 64;
const B$1 = 66;
const b$1 = 98;
const BACKSLASH$1 = 92;
const BACKSPACE$1 = 8;
const BELL$1 = 7;
const C$1 = 67;
const c$1 = 99;
const CANCEL$1 = 24;
const CARET$1 = 94;
const CARRIAGE_RETURN$1 = 13;
const CLOSE_CURLY$1 = 125;
const CLOSE_PARENTH$1 = 41;
const CLOSE_SQUARE$1 = 93;
const COLON$1 = 58;
const COMMA$1 = 44;
const d$1 = 100;
const D$1 = 68;
const DATA_LINK_ESCAPE$1 = 16;
const DELETE$1 = 127;
const DEVICE_CTRL_1$1 = 17;
const DEVICE_CTRL_2$1 = 18;
const DEVICE_CTRL_3$1 = 19;
const DEVICE_CTRL_4$1 = 20;
const DOLLAR$1 = 36;
const DOUBLE_QUOTE$1 = 34;
const e$2 = 101;
const E$1 = 69;
const EIGHT$1 = 56;
const END_OF_MEDIUM$1 = 25;
const END_OF_TRANSMISSION$1 = 4;
const END_OF_TRANSMISSION_BLOCK$1 = 23;
const END_OF_TXT$1 = 3;
const ENQUIRY$1 = 5;
const EQUAL$1 = 61;
const ESCAPE$1 = 27;
const EXCLAMATION$1 = 33;
const f$1 = 102;
const F$1 = 70;
const FILE_SEPERATOR$1 = 28;
const FIVE$1 = 53;
const FORM_FEED$1 = 12;
const FORWARD_SLASH$1 = 47;
const FOUR$1 = 52;
const g$1 = 103;
const G$1 = 71;
const GRAVE$1 = 96;
const GREATER_THAN$1 = 62;
const GROUP_SEPERATOR$1 = 29;
const h$1 = 104;
const H$1 = 72;
const HASH$1 = 35;
const HORIZONTAL_TAB$1 = 9;
const HYPHEN$1 = 45;
const i$1 = 105;
const I$1 = 73;
const j$1 = 106;
const J$1 = 74;
const k$1 = 107;
const K$1 = 75;
const l$1 = 108;
const L$1 = 76;
const LESS_THAN$1 = 60;
const LINE_FEED$1 = 10;
const m$1 = 109;
const M$1 = 77;
const n$1 = 110;
const N$1 = 78;
const NEGATIVE_ACKNOWLEDGE$1 = 21;
const NINE$1 = 57;
const NULL$1 = 0;
const o$1 = 111;
const O$1 = 79;
const ONE$1 = 49;
const OPEN_CURLY$1 = 123;
const OPEN_PARENTH$1 = 40;
const OPEN_SQUARE$1 = 91;
const p$1 = 112;
const P$1 = 80;
const PERCENT$1 = 37;
const PERIOD$1 = 46;
const PLUS$1 = 43;
const q$1 = 113;
const Q$1 = 81;
const QMARK$1 = 63;
const QUOTE$1 = 39;
const r$2 = 114;
const R$1 = 82;
const RECORD_SEPERATOR$1 = 30;
const s$1 = 115;
const S$1 = 83;
const SEMICOLON$1 = 59;
const SEVEN$1 = 55;
const SHIFT_IN$1 = 15;
const SHIFT_OUT$1 = 14;
const SIX$1 = 54;
const SPACE$1 = 32;
const START_OF_HEADER$1 = 1;
const START_OF_TEXT$1 = 2;
const SUBSTITUTE$1 = 26;
const SYNCH_IDLE$1 = 22;
const t$1 = 116;
const T$1 = 84;
const THREE$1 = 51;
const TILDE$1 = 126;
const TWO$1 = 50;
const u$1 = 117;
const U$1 = 85;
const UNDER_SCORE$1 = 95;
const UNIT_SEPERATOR$1 = 31;
const v$1 = 118;
const V$1 = 86;
const VERTICAL_BAR$1 = 124;
const VERTICAL_TAB$1 = 11;
const w$1 = 119;
const W$1 = 87;
const x$1 = 120;
const X$1 = 88;
const y$1 = 121;
const Y$1 = 89;
const z$1 = 122;
const Z$1 = 90;
const ZERO$1 = 48;

/**
 * Lexer Jump table reference 
 * 0. NUMBER
 * 1. IDENTIFIER
 * 2. QUOTE STRING
 * 3. SPACE SET
 * 4. TAB SET
 * 5. CARIAGE RETURN
 * 6. LINEFEED
 * 7. SYMBOL
 * 8. OPERATOR
 * 9. OPEN BRACKET
 * 10. CLOSE BRACKET 
 * 11. DATA_LINK
 */ 
const jump_table$1 = [
7, 	 	/* NULL */
7, 	 	/* START_OF_HEADER */
7, 	 	/* START_OF_TEXT */
7, 	 	/* END_OF_TXT */
7, 	 	/* END_OF_TRANSMISSION */
7, 	 	/* ENQUIRY */
7, 	 	/* ACKNOWLEDGE */
7, 	 	/* BELL */
7, 	 	/* BACKSPACE */
4, 	 	/* HORIZONTAL_TAB */
6, 	 	/* LINEFEED */
7, 	 	/* VERTICAL_TAB */
7, 	 	/* FORM_FEED */
5, 	 	/* CARRIAGE_RETURN */
7, 	 	/* SHIFT_OUT */
7, 		/* SHIFT_IN */
11,	 	/* DATA_LINK_ESCAPE */
7, 	 	/* DEVICE_CTRL_1 */
7, 	 	/* DEVICE_CTRL_2 */
7, 	 	/* DEVICE_CTRL_3 */
7, 	 	/* DEVICE_CTRL_4 */
7, 	 	/* NEGATIVE_ACKNOWLEDGE */
7, 	 	/* SYNCH_IDLE */
7, 	 	/* END_OF_TRANSMISSION_BLOCK */
7, 	 	/* CANCEL */
7, 	 	/* END_OF_MEDIUM */
7, 	 	/* SUBSTITUTE */
7, 	 	/* ESCAPE */
7, 	 	/* FILE_SEPERATOR */
7, 	 	/* GROUP_SEPERATOR */
7, 	 	/* RECORD_SEPERATOR */
7, 	 	/* UNIT_SEPERATOR */
3, 	 	/* SPACE */
8, 	 	/* EXCLAMATION */
2, 	 	/* DOUBLE_QUOTE */
7, 	 	/* HASH */
7, 	 	/* DOLLAR */
8, 	 	/* PERCENT */
8, 	 	/* AMPERSAND */
2, 	 	/* QUOTE */
9, 	 	/* OPEN_PARENTH */
10, 	 /* CLOSE_PARENTH */
8, 	 	/* ASTERISK */
8, 	 	/* PLUS */
7, 	 	/* COMMA */
7, 	 	/* HYPHEN */
7, 	 	/* PERIOD */
7, 	 	/* FORWARD_SLASH */
0, 	 	/* ZERO */
0, 	 	/* ONE */
0, 	 	/* TWO */
0, 	 	/* THREE */
0, 	 	/* FOUR */
0, 	 	/* FIVE */
0, 	 	/* SIX */
0, 	 	/* SEVEN */
0, 	 	/* EIGHT */
0, 	 	/* NINE */
8, 	 	/* COLON */
7, 	 	/* SEMICOLON */
8, 	 	/* LESS_THAN */
8, 	 	/* EQUAL */
8, 	 	/* GREATER_THAN */
7, 	 	/* QMARK */
7, 	 	/* AT */
1, 	 	/* A*/
1, 	 	/* B */
1, 	 	/* C */
1, 	 	/* D */
1, 	 	/* E */
1, 	 	/* F */
1, 	 	/* G */
1, 	 	/* H */
1, 	 	/* I */
1, 	 	/* J */
1, 	 	/* K */
1, 	 	/* L */
1, 	 	/* M */
1, 	 	/* N */
1, 	 	/* O */
1, 	 	/* P */
1, 	 	/* Q */
1, 	 	/* R */
1, 	 	/* S */
1, 	 	/* T */
1, 	 	/* U */
1, 	 	/* V */
1, 	 	/* W */
1, 	 	/* X */
1, 	 	/* Y */
1, 	 	/* Z */
9, 	 	/* OPEN_SQUARE */
7, 	 	/* TILDE */
10, 	/* CLOSE_SQUARE */
7, 	 	/* CARET */
7, 	 	/* UNDER_SCORE */
2, 	 	/* GRAVE */
1, 	 	/* a */
1, 	 	/* b */
1, 	 	/* c */
1, 	 	/* d */
1, 	 	/* e */
1, 	 	/* f */
1, 	 	/* g */
1, 	 	/* h */
1, 	 	/* i */
1, 	 	/* j */
1, 	 	/* k */
1, 	 	/* l */
1, 	 	/* m */
1, 	 	/* n */
1, 	 	/* o */
1, 	 	/* p */
1, 	 	/* q */
1, 	 	/* r */
1, 	 	/* s */
1, 	 	/* t */
1, 	 	/* u */
1, 	 	/* v */
1, 	 	/* w */
1, 	 	/* x */
1, 	 	/* y */
1, 	 	/* z */
9, 	 	/* OPEN_CURLY */
7, 	 	/* VERTICAL_BAR */
10,  	/* CLOSE_CURLY */
7,  	/* TILDE */
7 		/* DELETE */
];	

/**
 * LExer Number and Identifier jump table reference
 * Number are masked by 12(4|8) and Identifiers are masked by 10(2|8)
 * entries marked as `0` are not evaluated as either being in the number set or the identifier set.
 * entries marked as `2` are in the identifier set but not the number set
 * entries marked as `4` are in the number set but not the identifier set
 * entries marked as `8` are in both number and identifier sets
 */
const number_and_identifier_table$1 = [
0, 		/* NULL */
0, 		/* START_OF_HEADER */
0, 		/* START_OF_TEXT */
0, 		/* END_OF_TXT */
0, 		/* END_OF_TRANSMISSION */
0, 		/* ENQUIRY */
0,		/* ACKNOWLEDGE */
0,		/* BELL */
0,		/* BACKSPACE */
0,		/* HORIZONTAL_TAB */
0,		/* LINEFEED */
0,		/* VERTICAL_TAB */
0,		/* FORM_FEED */
0,		/* CARRIAGE_RETURN */
0,		/* SHIFT_OUT */
0,		/* SHIFT_IN */
0,		/* DATA_LINK_ESCAPE */
0,		/* DEVICE_CTRL_1 */
0,		/* DEVICE_CTRL_2 */
0,		/* DEVICE_CTRL_3 */
0,		/* DEVICE_CTRL_4 */
0,		/* NEGATIVE_ACKNOWLEDGE */
0,		/* SYNCH_IDLE */
0,		/* END_OF_TRANSMISSION_BLOCK */
0,		/* CANCEL */
0,		/* END_OF_MEDIUM */
0,		/* SUBSTITUTE */
0,		/* ESCAPE */
0,		/* FILE_SEPERATOR */
0,		/* GROUP_SEPERATOR */
0,		/* RECORD_SEPERATOR */
0,		/* UNIT_SEPERATOR */
0,		/* SPACE */
0,		/* EXCLAMATION */
0,		/* DOUBLE_QUOTE */
0,		/* HASH */
0,		/* DOLLAR */
0,		/* PERCENT */
0,		/* AMPERSAND */
0,		/* QUOTE */
0,		/* OPEN_PARENTH */
0,		 /* CLOSE_PARENTH */
0,		/* ASTERISK */
0,		/* PLUS */
0,		/* COMMA */
0,		/* HYPHEN */
0,		/* PERIOD */
0,		/* FORWARD_SLASH */
8,		/* ZERO */
8,		/* ONE */
8,		/* TWO */
8,		/* THREE */
8,		/* FOUR */
8,		/* FIVE */
8,		/* SIX */
8,		/* SEVEN */
8,		/* EIGHT */
8,		/* NINE */
0,		/* COLON */
0,		/* SEMICOLON */
0,		/* LESS_THAN */
0,		/* EQUAL */
0,		/* GREATER_THAN */
0,		/* QMARK */
0,		/* AT */
2,		/* A*/
8,		/* B */
2,		/* C */
2,		/* D */
8,		/* E */
2,		/* F */
2,		/* G */
2,		/* H */
2,		/* I */
2,		/* J */
2,		/* K */
2,		/* L */
2,		/* M */
2,		/* N */
8,		/* O */
2,		/* P */
2,		/* Q */
2,		/* R */
2,		/* S */
2,		/* T */
2,		/* U */
2,		/* V */
2,		/* W */
8,		/* X */
2,		/* Y */
2,		/* Z */
0,		/* OPEN_SQUARE */
0,		/* TILDE */
0,		/* CLOSE_SQUARE */
0,		/* CARET */
0,		/* UNDER_SCORE */
0,		/* GRAVE */
2,		/* a */
8,		/* b */
2,		/* c */
2,		/* d */
2,		/* e */
2,		/* f */
2,		/* g */
2,		/* h */
2,		/* i */
2,		/* j */
2,		/* k */
2,		/* l */
2,		/* m */
2,		/* n */
8,		/* o */
2,		/* p */
2,		/* q */
2,		/* r */
2,		/* s */
2,		/* t */
2,		/* u */
2,		/* v */
2,		/* w */
8,		/* x */
2,		/* y */
2,		/* z */
0,		/* OPEN_CURLY */
0,		/* VERTICAL_BAR */
0,		/* CLOSE_CURLY */
0,		/* TILDE */
0		/* DELETE */
];

const extended_number_and_identifier_table$1 = number_and_identifier_table$1.slice();
extended_number_and_identifier_table$1[45] = 2;
extended_number_and_identifier_table$1[95] = 2;

const
    number$1 = 1,
    identifier$1 = 2,
    string$1 = 4,
    white_space$1 = 8,
    open_bracket$1 = 16,
    close_bracket$1 = 32,
    operator$1 = 64,
    symbol$1 = 128,
    new_line$1 = 256,
    data_link$1 = 512,
    alpha_numeric$1 = (identifier$1 | number$1),
    white_space_new_line$1 = (white_space$1 | new_line$1),
    Types$1 = {
        num: number$1,
        number: number$1,
        id: identifier$1,
        identifier: identifier$1,
        str: string$1,
        string: string$1,
        ws: white_space$1,
        white_space: white_space$1,
        ob: open_bracket$1,
        open_bracket: open_bracket$1,
        cb: close_bracket$1,
        close_bracket: close_bracket$1,
        op: operator$1,
        operator: operator$1,
        sym: symbol$1,
        symbol: symbol$1,
        nl: new_line$1,
        new_line: new_line$1,
        dl: data_link$1,
        data_link: data_link$1,
        alpha_numeric: alpha_numeric$1,
        white_space_new_line: white_space_new_line$1,
    },

    /*** MASKS ***/

    TYPE_MASK$1 = 0xF,
    PARSE_STRING_MASK$1 = 0x10,
    IGNORE_WHITESPACE_MASK$1 = 0x20,
    CHARACTERS_ONLY_MASK$1 = 0x40,
    TOKEN_LENGTH_MASK$1 = 0xFFFFFF80,

    //De Bruijn Sequence for finding index of right most bit set.
    //http://supertech.csail.mit.edu/papers/debruijn.pdf
    debruijnLUT$1 = [
        0, 1, 28, 2, 29, 14, 24, 3, 30, 22, 20, 15, 25, 17, 4, 8,
        31, 27, 13, 23, 21, 19, 16, 7, 26, 12, 18, 6, 11, 5, 10, 9
    ];

const getNumbrOfTrailingZeroBitsFromPowerOf2$1 = (value) => debruijnLUT$1[(value * 0x077CB531) >>> 27];

class Lexer$1 {

    constructor(string = "", INCLUDE_WHITE_SPACE_TOKENS = false, PEEKING = false) {

        if (typeof(string) !== "string") throw new Error(`String value must be passed to Lexer. A ${typeof(string)} was passed as the \`string\` argument.`);

        /**
         * The string that the Lexer tokenizes.
         */
        this.str = string;

        /**
         * Reference to the peeking Lexer.
         */
        this.p = null;

        /**
         * The type id of the current token.
         */
        this.type = 32768; //Default "non-value" for types is 1<<15;

        /**
         * The offset in the string of the start of the current token.
         */
        this.off = 0;

        this.masked_values = 0;

        /**
         * The character offset of the current token within a line.
         */
        this.char = 0;
        /**
         * The line position of the current token.
         */
        this.line = 0;
        /**
         * The length of the string being parsed
         */
        this.sl = string.length;
        /**
         * The length of the current token.
         */
        this.tl = 0;

        /**
         * Flag to ignore white spaced.
         */
        this.IWS = !INCLUDE_WHITE_SPACE_TOKENS;

        this.USE_EXTENDED_ID = false;

        /**
         * Flag to force the lexer to parse string contents
         */
        this.PARSE_STRING = false;

        this.id_lu = number_and_identifier_table$1;

        if (!PEEKING) this.next();
    }

    useExtendedId(){
        this.id_lu = extended_number_and_identifier_table$1;
        this.tl = 0;
        this.next();
        return this;
    }

    /**
     * Restricts max parse distance to the other Lexer's current position.
     * @param      {Lexer}  Lexer   The Lexer to limit parse distance by.
     */
    fence(marker = this) {
        if (marker.str !== this.str)
            return;
        this.sl = marker.off;
        return this;
    }

    /**
     * Copies the Lexer.
     * @return     {Lexer}  Returns a new Lexer instance with the same property values.
     */
    copy(destination = new Lexer$1(this.str, false, true)) {
        destination.off = this.off;
        destination.char = this.char;
        destination.line = this.line;
        destination.sl = this.sl;
        destination.masked_values = this.masked_values;
        destination.id_lu = this.id_lu;
        return destination;
    }

    /**
     * Given another Lexer with the same `str` property value, it will copy the state of that Lexer.
     * @param      {Lexer}  [marker=this.peek]  The Lexer to clone the state from. 
     * @throws     {Error} Throws an error if the Lexers reference different strings.
     * @public
     */
    sync(marker = this.p) {

        if (marker instanceof Lexer$1) {
            if (marker.str !== this.str) throw new Error("Cannot sync Lexers with different strings!");
            this.off = marker.off;
            this.char = marker.char;
            this.line = marker.line;
            this.masked_values = marker.masked_values;
        }

        return this;
    }

    /**
    Creates an error message with a diagram illustrating the location of the error. 
    */
    errorMessage(message = "") {
        const pk = this.copy();

        pk.IWS = false;

        while (!pk.END && pk.ty !== Types$1.nl) { pk.next(); }

        const end = (pk.END) ? this.str.length : pk.off,

            nls = (this.line > 0) ? 1 : 0,
            number_of_tabs = this.str
                .slice(this.off - this.char + nls + nls, this.off + nls)
                .split("")
                .reduce((r, v) => (r + ((v.charCodeAt(0) == HORIZONTAL_TAB$1) | 0)), 0),

            arrow = String.fromCharCode(0x2b89),

            line = String.fromCharCode(0x2500),

            thick_line = String.fromCharCode(0x2501),

            line_number = `    ${this.line+1}: `,

            line_fill = line_number.length + number_of_tabs,

            line_text = this.str.slice(this.off - this.char + nls + (nls), end).replace(/\t/g, "  "),

            error_border = thick_line.repeat(line_text.length + line_number.length + 2),

            is_iws = (!this.IWS) ? "\n The Lexer produced whitespace tokens" : "",

            msg =[ `${message} at ${this.line+1}:${this.char - nls}` ,
            `${error_border}` ,
            `${line_number+line_text}` ,
            `${line.repeat(this.char-nls+line_fill-(nls))+arrow}` ,
            `${error_border}` ,
            `${is_iws}`].join("\n");

        return msg;
    }

    /**
     * Will throw a new Error, appending the parsed string line and position information to the the error message passed into the function.
     * @instance
     * @public
     * @param {String} message - The error message.
     * @param {Bool} DEFER - if true, returns an Error object instead of throwing.
     */
    throw (message, DEFER = false) {
        const error = new Error(this.errorMessage(message));
        if (DEFER)
            return error;
        throw error;
    }

    /**
     * Proxy for Lexer.prototype.reset
     * @public
     */
    r() { return this.reset() }

    /**
     * Restore the Lexer back to it's initial state.
     * @public
     */
    reset() {
        this.p = null;
        this.type = 32768;
        this.off = 0;
        this.tl = 0;
        this.char = 0;
        this.line = 0;
        this.n;
        return this;
    }

    resetHead() {
        this.off = 0;
        this.tl = 0;
        this.char = 0;
        this.line = 0;
        this.p = null;
        this.type = 32768;
    }

    /**
     * Sets the internal state to point to the next token. Sets Lexer.prototype.END to `true` if the end of the string is hit.
     * @public
     * @param {Lexer} [marker=this] - If another Lexer is passed into this method, it will advance the token state of that Lexer.
     */
    next(marker = this, USE_CUSTOM_SYMBOLS = !!this.symbol_map) {

        if (marker.sl < 1) {
            marker.off = 0;
            marker.type = 32768;
            marker.tl = 0;
            marker.line = 0;
            marker.char = 0;
            return marker;
        }

        //Token builder
        const l = marker.sl,
            str = marker.str,
            number_and_identifier_table = this.id_lu,
            IWS = marker.IWS;

        let length = marker.tl,
            off = marker.off + length,
            type = symbol$1,
            line = marker.line,
            base = off,
            char = marker.char,
            root = marker.off;

        if (off >= l) {
            length = 0;
            base = l;
            //char -= base - off;
            marker.char = char + (base - marker.off);
            marker.type = type;
            marker.off = base;
            marker.tl = 0;
            marker.line = line;
            return marker;
        }

        let NORMAL_PARSE = true;

        if (USE_CUSTOM_SYMBOLS) {

            let code = str.charCodeAt(off);
            let off2 = off;
            let map = this.symbol_map,
                m;
            let i = 0;

            while (code == 32 && IWS)
                (code = str.charCodeAt(++off2), off++);

            while ((m = map.get(code))) {
                map = m;
                off2 += 1;
                code = str.charCodeAt(off2);
            }

            if (map.IS_SYM) {
                NORMAL_PARSE = false;
                base = off;
                length = off2 - off;
                //char += length;
            }
        }

        while (NORMAL_PARSE) {

                base = off;

                length = 1;

                const code = str.charCodeAt(off);

                if (code < 128) {

                    switch (jump_table$1[code]) {
                        case 0: //NUMBER
                            while (++off < l && (12 & number_and_identifier_table[str.charCodeAt(off)]));

                            if ((str[off] == "e" || str[off] == "E") && (12 & number_and_identifier_table[str.charCodeAt(off + 1)])) {
                                off++;
                                if (str[off] == "-") off++;
                                marker.off = off;
                                marker.tl = 0;
                                marker.next();
                                off = marker.off + marker.tl;
                                //Add e to the number string
                            }

                            type = number$1;
                            length = off - base;

                            break;
                        case 1: //IDENTIFIER
                            while (++off < l && ((10 & number_and_identifier_table[str.charCodeAt(off)])));
                            type = identifier$1;
                            length = off - base;
                            break;
                        case 2: //QUOTED STRING
                            if (this.PARSE_STRING) {
                                type = symbol$1;
                            } else {
                                while (++off < l && str.charCodeAt(off) !== code);
                                type = string$1;
                                length = off - base + 1;
                            }
                            break;
                        case 3: //SPACE SET
                            while (++off < l && str.charCodeAt(off) === SPACE$1);
                            type = white_space$1;
                            length = off - base;
                            break;
                        case 4: //TAB SET
                            while (++off < l && str[off] === HORIZONTAL_TAB$1);
                            type = white_space$1;
                            length = off - base;
                            break;
                        case 5: //CARIAGE RETURN
                            length = 2;
                            //intentional
                        case 6: //LINEFEED
                            type = new_line$1;
                            line++;
                            base = off;
                            root = off;
                            off += length;
                            char = 0;
                            break;
                        case 7: //SYMBOL
                            type = symbol$1;
                            break;
                        case 8: //OPERATOR
                            type = operator$1;
                            break;
                        case 9: //OPEN BRACKET
                            type = open_bracket$1;
                            break;
                        case 10: //CLOSE BRACKET
                            type = close_bracket$1;
                            break;
                        case 11: //Data Link Escape
                            type = data_link$1;
                            length = 4; //Stores two UTF16 values and a data link sentinel
                            break;
                    }
                } else {
                    break;
                }

                if (IWS && (type & white_space_new_line$1)) {
                    if (off < l) {
                        type = symbol$1;
                        //off += length;
                        continue;
                    } else {
                        //Trim white space from end of string
                        //base = l - off;
                        //marker.sl -= off;
                        //length = 0;
                    }
                }
                break;
        }

        marker.type = type;
        marker.off = base;
        marker.tl = (this.masked_values & CHARACTERS_ONLY_MASK$1) ? Math.min(1, length) : length;
        marker.char = char + base - root;
        marker.line = line;

        return marker;
    }


    /**
     * Proxy for Lexer.prototype.assert
     * @public
     */
    a(text) {
        return this.assert(text);
    }

    /**
     * Compares the string value of the current token to the value passed in. Advances to next token if the two are equal.
     * @public
     * @throws {Error} - `Expecting "${text}" got "${this.text}"`
     * @param {String} text - The string to compare.
     */
    assert(text) {

        if (this.off < 0) this.throw(`Expecting ${text} got null`);

        if (this.text == text)
            this.next();
        else
            this.throw(`Expecting "${text}" got "${this.text}"`);

        return this;
    }

    /**
     * Proxy for Lexer.prototype.assertCharacter
     * @public
     */
    aC(char) { return this.assertCharacter(char) }
    /**
     * Compares the character value of the current token to the value passed in. Advances to next token if the two are equal.
     * @public
     * @throws {Error} - `Expecting "${text}" got "${this.text}"`
     * @param {String} text - The string to compare.
     */
    assertCharacter(char) {

        if (this.off < 0) this.throw(`Expecting ${char[0]} got null`);

        if (this.ch == char[0])
            this.next();
        else
            this.throw(`Expecting "${char[0]}" got "${this.tx[this.off]}"`);

        return this;
    }

    /**
     * Returns the Lexer bound to Lexer.prototype.p, or creates and binds a new Lexer to Lexer.prototype.p. Advences the other Lexer to the token ahead of the calling Lexer.
     * @public
     * @type {Lexer}
     * @param {Lexer} [marker=this] - The marker to originate the peek from. 
     * @param {Lexer} [peek_marker=this.p] - The Lexer to set to the next token state.
     * @return {Lexer} - The Lexer that contains the peeked at token.
     */
    peek(marker = this, peek_marker = this.p) {

        if (!peek_marker) {
            if (!marker) return null;
            if (!this.p) {
                this.p = new Lexer$1(this.str, false, true);
                peek_marker = this.p;
            }
        }
        peek_marker.masked_values = marker.masked_values;
        peek_marker.type = marker.type;
        peek_marker.off = marker.off;
        peek_marker.tl = marker.tl;
        peek_marker.char = marker.char;
        peek_marker.line = marker.line;
        this.next(peek_marker);
        return peek_marker;
    }


    /**
     * Proxy for Lexer.prototype.slice
     * @public
     */
    s(start) { return this.slice(start) }

    /**
     * Returns a slice of the parsed string beginning at `start` and ending at the current token.
     * @param {Number | LexerBeta} start - The offset in this.str to begin the slice. If this value is a LexerBeta, sets the start point to the value of start.off.
     * @return {String} A substring of the parsed string.
     * @public
     */
    slice(start = this.off) {

        if (start instanceof Lexer$1) start = start.off;

        return this.str.slice(start, (this.off <= start) ? this.sl : this.off);
    }

    /**
     * Skips to the end of a comment section.
     * @param {boolean} ASSERT - If set to true, will through an error if there is not a comment line or block to skip.
     * @param {Lexer} [marker=this] - If another Lexer is passed into this method, it will advance the token state of that Lexer.
     */
    comment(ASSERT = false, marker = this) {

        if (!(marker instanceof Lexer$1)) return marker;

        if (marker.ch == "/") {
            if (marker.pk.ch == "*") {
                marker.sync();
                while (!marker.END && (marker.next().ch != "*" || marker.pk.ch != "/")) { /* NO OP */ }
                marker.sync().assert("/");
            } else if (marker.pk.ch == "/") {
                const IWS = marker.IWS;
                while (marker.next().ty != Types$1.new_line && !marker.END) { /* NO OP */ }
                marker.IWS = IWS;
                marker.next();
            } else
            if (ASSERT) marker.throw("Expecting the start of a comment");
        }

        return marker;
    }

    setString(string, RESET = true) {
        this.str = string;
        this.sl = string.length;
        if (RESET) this.resetHead();
    }

    toString() {
        return this.slice();
    }

    /**
     * Returns new Whind Lexer that has leading and trailing whitespace characters removed from input. 
     * leave_leading_amount - Maximum amount of leading space caracters to leave behind. Default is zero
     * leave_trailing_amount - Maximum amount of trailing space caracters to leave behind. Default is zero
     */
    trim(leave_leading_amount = 0, leave_trailing_amount = leave_leading_amount) {
        const lex = this.copy();

        let space_count = 0,
            off = lex.off;

        for (; lex.off < lex.sl; lex.off++) {
            const c = jump_table$1[lex.string.charCodeAt(lex.off)];

            if (c > 2 && c < 7) {

                if (space_count >= leave_leading_amount) {
                    off++;
                } else {
                    space_count++;
                }
                continue;
            }

            break;
        }

        lex.off = off;
        space_count = 0;
        off = lex.sl;

        for (; lex.sl > lex.off; lex.sl--) {
            const c = jump_table$1[lex.string.charCodeAt(lex.sl - 1)];

            if (c > 2 && c < 7) {
                if (space_count >= leave_trailing_amount) {
                    off--;
                } else {
                    space_count++;
                }
                continue;
            }

            break;
        }

        lex.sl = off;

        if (leave_leading_amount > 0)
            lex.IWS = false;

        lex.token_length = 0;

        lex.next();

        return lex;
    }

    /** Adds symbol to symbol_map. This allows custom symbols to be defined and tokenized by parser. **/
    addSymbol(sym) {
        if (!this.symbol_map)
            this.symbol_map = new Map;


        let map = this.symbol_map;

        for (let i = 0; i < sym.length; i++) {
            let code = sym.charCodeAt(i);
            let m = map.get(code);
            if (!m) {
                m = map.set(code, new Map).get(code);
            }
            map = m;
        }
        map.IS_SYM = true;
    }

    /*** Getters and Setters ***/
    get string() {
        return this.str;
    }

    get string_length() {
        return this.sl - this.off;
    }

    set string_length(s) {}

    /**
     * The current token in the form of a new Lexer with the current state.
     * Proxy property for Lexer.prototype.copy
     * @type {Lexer}
     * @public
     * @readonly
     */
    get token() {
        return this.copy();
    }


    get ch() {
        return this.str[this.off];
    }

    /**
     * Proxy for Lexer.prototype.text
     * @public
     * @type {String}
     * @readonly
     */
    get tx() { return this.text }

    /**
     * The string value of the current token.
     * @type {String}
     * @public
     * @readonly
     */
    get text() {
        return (this.off < 0) ? "" : this.str.slice(this.off, this.off + this.tl);
    }

    /**
     * The type id of the current token.
     * @type {Number}
     * @public
     * @readonly
     */
    get ty() { return this.type }

    /**
     * The current token's offset position from the start of the string.
     * @type {Number}
     * @public
     * @readonly
     */
    get pos() {
        return this.off;
    }

    /**
     * Proxy for Lexer.prototype.peek
     * @public
     * @readonly
     * @type {Lexer}
     */
    get pk() { return this.peek() }

    /**
     * Proxy for Lexer.prototype.next
     * @public
     */
    get n() { return this.next() }

    get END() { return this.off >= this.sl }
    set END(v) {}

    get type() {
        return 1 << (this.masked_values & TYPE_MASK$1);
    }

    set type(value) {
        //assuming power of 2 value.
        this.masked_values = (this.masked_values & ~TYPE_MASK$1) | ((getNumbrOfTrailingZeroBitsFromPowerOf2$1(value)) & TYPE_MASK$1);
    }

    get tl() {
        return this.token_length;
    }

    set tl(value) {
        this.token_length = value;
    }

    get token_length() {
        return ((this.masked_values & TOKEN_LENGTH_MASK$1) >> 7);
    }

    set token_length(value) {
        this.masked_values = (this.masked_values & ~TOKEN_LENGTH_MASK$1) | (((value << 7) | 0) & TOKEN_LENGTH_MASK$1);
    }

    get IGNORE_WHITE_SPACE() {
        return this.IWS;
    }

    set IGNORE_WHITE_SPACE(bool) {
        this.iws = !!bool;
    }

    get CHARACTERS_ONLY() {
        return !!(this.masked_values & CHARACTERS_ONLY_MASK$1);
    }

    set CHARACTERS_ONLY(boolean) {
        this.masked_values = (this.masked_values & ~CHARACTERS_ONLY_MASK$1) | ((boolean | 0) << 6);
    }

    get IWS() {
        return !!(this.masked_values & IGNORE_WHITESPACE_MASK$1);
    }

    set IWS(boolean) {
        this.masked_values = (this.masked_values & ~IGNORE_WHITESPACE_MASK$1) | ((boolean | 0) << 5);
    }

    get PARSE_STRING() {
        return !!(this.masked_values & PARSE_STRING_MASK$1);
    }

    set PARSE_STRING(boolean) {
        this.masked_values = (this.masked_values & ~PARSE_STRING_MASK$1) | ((boolean | 0) << 4);
    }

    /**
     * Reference to token id types.
     */
    get types() {
        return Types$1;
    }
}

Lexer$1.prototype.addCharacter = Lexer$1.prototype.addSymbol;

function whind$2(string, INCLUDE_WHITE_SPACE_TOKENS = false) { return new Lexer$1(string, INCLUDE_WHITE_SPACE_TOKENS) }

whind$2.constructor = Lexer$1;

Lexer$1.types = Types$1;
whind$2.types = Types$1;

const uri_reg_ex = /(?:([a-zA-Z][\dA-Za-z\+\.\-]*)(?:\:\/\/))?(?:([a-zA-Z][\dA-Za-z\+\.\-]*)(?:\:([^\<\>\:\?\[\]\@\/\#\b\s]*)?)?\@)?(?:(\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3})|((?:\[[0-9a-f]{1,4})+(?:\:[0-9a-f]{0,4}){2,7}\])|([^\<\>\:\?\[\]\@\/\#\b\s\.]{2,}(?:\.[^\<\>\:\?\[\]\@\/\#\b\s]*)*))?(?:\:(\d+))?((?:[^\?\[\]\#\s\b]*)+)?(?:\?([^\[\]\#\s\b]*))?(?:\#([^\#\s\b]*))?/i;

const STOCK_LOCATION = {
    protocol: "",
    host: "",
    port: "",
    path: "",
    hash: "",
    query: "",
    search: ""
};

function fetchLocalText(URL, m = "same-origin") {
    return new Promise((res, rej) => {
        fetch(URL, {
            mode: m, // CORs not allowed
            credentials: m,
            method: "GET"
        }).then(r => {

            if (r.status < 200 || r.status > 299)
                r.text().then(rej);
            else
                r.text().then(res);
        }).catch(e => rej(e));
    });
}

function fetchLocalJSON(URL, m = "same-origin") {
    return new Promise((res, rej) => {
        fetch(URL, {
            mode: m, // CORs not allowed
            credentials: m,
            method: "GET"
        }).then(r => {
            if (r.status < 200 || r.status > 299)
                r.json().then(rej);
            else
                r.json().then(res).catch(rej);
        }).catch(e => rej(e));
    });
}

function submitForm(URL, form_data, m = "same-origin") {
    return new Promise((res, rej) => {
        var form;

        if (form_data instanceof FormData)
            form = form_data;
        else {
            form = new FormData();
            for (let name in form_data)
                form.append(name, form_data[name] + "");
        }

        fetch(URL, {
            mode: m, // CORs not allowed
            credentials: m,
            method: "POST",
            body: form,
        }).then(r => {
            if (r.status < 200 || r.status > 299)
                r.text().then(rej);
            else
                r.json().then(res);
        }).catch(e => e.text().then(rej));
    });
}

function submitJSON(URL, json_data, m = "same-origin") {
    return new Promise((res, rej) => {
        fetch(URL, {
            headers: {
                'Content-Type': 'application/json',
                'Accept': 'application/json',
            },
            mode: m, // CORs not allowed
            credentials: m,
            method: "POST",
            body: JSON.stringify(json_data),
        }).then(r => {
            if (r.status < 200 || r.status > 299)
                r.json().then(rej);
            else
                r.json().then(res);
        }).catch(e => e.text().then(rej));
    });
}



/**
 * Used for processing URLs, handling `document.location`, and fetching data.
 * @param      {string}   url           The URL string to wrap.
 * @param      {boolean}  USE_LOCATION  If `true` missing URL parts are filled in with data from `document.location`. 
 * @return     {URL}   If a falsy value is passed to `url`, and `USE_LOCATION` is `true` a Global URL is returned. This is directly linked to the page and will _update_ the actual page URL when its values are change. Use with caution. 
 * @alias URL
 * @memberof module:wick.core.network
 */
class URL {

    static resolveRelative(URL_or_url_new, URL_or_url_original = document.location.toString(), ) {

        let URL_old = (URL_or_url_original instanceof URL) ? URL_or_url_original : new URL(URL_or_url_original);
        let URL_new = (URL_or_url_new instanceof URL) ? URL_or_url_new : new URL(URL_or_url_new);

        if (!(URL_old + "") || !(URL_new + "")) return null;

        let new_path = "";
        if (URL_new.path[0] != "/") {

            let a = URL_old.path.split("/");
            let b = URL_new.path.split("/");


            if (b[0] == "..") a.splice(a.length - 1, 1);
            for (let i = 0; i < b.length; i++) {
                switch (b[i]) {
                    case "..":
                    case ".":
                        a.splice(a.length - 1, 1);
                        break;
                    default:
                        a.push(b[i]);
                }
            }
            URL_new.path = a.join("/");
        }

        return URL_new;
    }

    constructor(url = "", USE_LOCATION = false) {

        let IS_STRING = true,
            IS_LOCATION = false;


        let location = (typeof(document) !== "undefined") ? document.location : STOCK_LOCATION;

        if (typeof(Location) !== "undefined" && url instanceof Location) {
            location = url;
            url = "";
            IS_LOCATION = true;
        }
        if (!url || typeof(url) != "string") {
            IS_STRING = false;
            IS_LOCATION = true;
            if (URL.GLOBAL && USE_LOCATION)
                return URL.GLOBAL;
        }

        /**
         * URL protocol
         */
        this.protocol = "";

        /**
         * Username string
         */
        this.user = "";

        /**
         * Password string
         */
        this.pwd = "";

        /**
         * URL hostname
         */
        this.host = "";

        /**
         * URL network port number.
         */
        this.port = 0;

        /**
         * URL resource path
         */
        this.path = "";

        /**
         * URL query string.
         */
        this.query = "";

        /**
         * Hashtag string
         */
        this.hash = "";

        /**
         * Map of the query data
         */
        this.map = null;

        if (IS_STRING) {
            if (url instanceof URL) {
                this.protocol = url.protocol;
                this.user = url.user;
                this.pwd = url.pwd;
                this.host = url.host;
                this.port = url.port;
                this.path = url.path;
                this.query = url.query;
                this.hash = url.hash;
            } else {
                let part = url.match(uri_reg_ex);

                //If the complete string is not matched than we are dealing with something other 
                //than a pure URL. Thus, no object is returned. 
                if (part[0] !== url) return null;

                this.protocol = part[1] || ((USE_LOCATION) ? location.protocol : "");
                this.user = part[2] || "";
                this.pwd = part[3] || "";
                this.host = part[4] || part[5] || part[6] || ((USE_LOCATION) ? location.hostname : "");
                this.port = parseInt(part[7] || ((USE_LOCATION) ? location.port : 0));
                this.path = part[8] || ((USE_LOCATION) ? location.pathname : "");
                this.query = part[9] || ((USE_LOCATION) ? location.search.slice(1) : "");
                this.hash = part[10] || ((USE_LOCATION) ? location.hash.slice(1) : "");

            }
        } else if (IS_LOCATION) {
            this.protocol = location.protocol.replace(/\:/g, "");
            this.host = location.hostname;
            this.port = location.port;
            this.path = location.pathname;
            this.hash = location.hash.slice(1);
            this.query = location.search.slice(1);
            this._getQuery_(this.query);

            if (USE_LOCATION) {
                URL.G = this;
                return URL.R;
            }
        }
        this._getQuery_(this.query);
    }


    /**
    URL Query Syntax

    root => [root_class] [& [class_list]]
         => [class_list]

    root_class = key_list

    class_list [class [& key_list] [& class_list]]

    class => name & key_list

    key_list => [key_val [& key_list]]

    key_val => name = val

    name => ALPHANUMERIC_ID

    val => NUMBER
        => ALPHANUMERIC_ID
    */

    /**
     * Pulls query string info into this.map
     * @private
     */
    _getQuery_() {
        let map = (this.map) ? this.map : (this.map = new Map());

        let lex = whind$2(this.query);


        const get_map = (k, m) => (m.has(k)) ? m.get(k) : m.set(k, new Map).get(k);

        let key = 0,
            key_val = "",
            class_map = get_map(key_val, map),
            lfv = 0;

        while (!lex.END) {
            switch (lex.tx) {
                case "&": //At new class or value
                    if (lfv > 0)
                        key = (class_map.set(key_val, lex.s(lfv)), lfv = 0, lex.n.pos);
                    else {
                        key_val = lex.s(key);
                        key = (class_map = get_map(key_val, map), lex.n.pos);
                    }
                    continue;
                case "=":
                    //looking for a value now
                    key_val = lex.s(key);
                    lfv = lex.n.pos;
                    continue;
            }
            lex.n;
        }

        if (lfv > 0) class_map.set(key_val, lex.s(lfv));
    }

    setPath(path) {

        this.path = path;

        return new URL(this);
    }

    setLocation() {
        history.replaceState({}, "replaced state", `${this}`);
        window.onpopstate();
    }

    toString() {
        let str = [];

        if (this.host) {

            if (this.protocol)
                str.push(`${this.protocol}://`);

            str.push(`${this.host}`);
        }

        if (this.port)
            str.push(`:${this.port}`);

        if (this.path)
            str.push(`${this.path[0] == "/" ? "" : "/"}${this.path}`);

        if (this.query)
            str.push(((this.query[0] == "?" ? "" : "?") + this.query));

        if (this.hash)
            str.push("#" + this.hash);


        return str.join("");
    }

    /**
     * Pulls data stored in query string into an object an returns that.
     * @param      {string}  class_name  The class name
     * @return     {object}  The data.
     */
    getData(class_name = "") {
        if (this.map) {
            let out = {};
            let _c = this.map.get(class_name);
            if (_c) {
                for (let [key, val] of _c.entries())
                    out[key] = val;
                return out;
            }
        }
        return null;
    }

    /**
     * Sets the data in the query string. Wick data is added after a second `?` character in the query field, and appended to the end of any existing data.
     * @param      {string}  class_name  Class name to use in query string. Defaults to root, no class 
     * @param      {object | Model | AnyModel}  data        The data
     */
    setData(data = null, class_name = "") {

        if (data) {

            let map = this.map = new Map();

            let store = (map.has(class_name)) ? map.get(class_name) : (map.set(class_name, new Map()).get(class_name));

            //If the data is a falsy value, delete the association.

            for (let n in data) {
                if (data[n] !== undefined && typeof data[n] !== "object")
                    store.set(n, data[n]);
                else
                    store.delete(n);
            }

            //set query
            let class_, null_class, str = "";

            if ((null_class = map.get(""))) {
                if (null_class.size > 0) {
                    for (let [key, val] of null_class.entries())
                        str += `&${key}=${val}`;

                }
            }

            for (let [key, class_] of map.entries()) {
                if (key === "")
                    continue;
                if (class_.size > 0) {
                    str += `&${key}`;
                    for (let [key, val] of class_.entries())
                        str += `&${key}=${val}`;
                }
            }
            
            str = str.slice(1);

            this.query = this.query.split("?")[0] + "?" + str;

            if (URL.G == this)
                this.goto();
        } else {
            this.query = "";
        }

        return this;

    }

    /**
     * Fetch a string value of the remote resource. 
     * Just uses path component of URL. Must be from the same origin.
     * @param      {boolean}  [ALLOW_CACHE=true]  If `true`, the return string will be cached. If it is already cached, that will be returned instead. If `false`, a network fetch will always occur , and the result will not be cached.
     * @return     {Promise}  A promise object that resolves to a string of the fetched value.
     */
    fetchText(ALLOW_CACHE = true) {

        if (ALLOW_CACHE) {

            let resource = URL.RC.get(this.path);

            if (resource)
                return new Promise((res) => {
                    res(resource);
                });
        }

        return fetchLocalText(this.path).then(res => (URL.RC.set(this.path, res), res));
    }

    /**
     * Fetch a JSON value of the remote resource. 
     * Just uses path component of URL. Must be from the same origin.
     * @param      {boolean}  [ALLOW_CACHE=true]  If `true`, the return string will be cached. If it is already cached, that will be returned instead. If `false`, a network fetch will always occur , and the result will not be cached.
     * @return     {Promise}  A promise object that resolves to a string of the fetched value.
     */
    fetchJSON(ALLOW_CACHE = true) {

        let string_url = this.toString();

        if (ALLOW_CACHE) {

            let resource = URL.RC.get(string_url);

            if (resource)
                return new Promise((res) => {
                    res(resource);
                });
        }

        return fetchLocalJSON(string_url).then(res => (URL.RC.set(this.path, res), res));
    }

    /**
     * Cache a local resource at the value 
     * @param    {object}  resource  The resource to store at this URL path value.
     * @returns {boolean} `true` if a resource was already cached for this URL, false otherwise.
     */
    cacheResource(resource) {

        let occupied = URL.RC.has(this.path);

        URL.RC.set(this.path, resource);

        return occupied;
    }

    submitForm(form_data) {
        return submitForm(this.toString(), form_data);
    }

    submitJSON(json_data, mode) {
        return submitJSON(this.toString(), json_data, mode);
    }
    /**
     * Goes to the current URL.
     */
    goto() {
        return;
        let url = this.toString();
        history.pushState({}, "ignored title", url);
        window.onpopstate();
        URL.G = this;
    }
    //Returns the last segment of the path
    get file() {
        return this.path.split("/").pop();
    }
    //returns the name of the file less the extension
    get filename() {
        return this.file.split(".").shift();
    }



    //Returns the all but the last segment of the path
    get dir() {
        return this.path.split("/").slice(0, -1).join("/") || "/";
    }

    get pathname() {
        return this.path;
    }

    get href() {
        return this.toString();
    }

    get ext() {
        const m = this.path.match(/\.([^\.]*)$/);
        return m ? m[1] : "";
    }

    get search() {
        return this.query;
    }
}

/**
 * The fetched resource cache.
 */
URL.RC = new Map();

/**
 * The Default Global URL object. 
 */
URL.G = null;

/**
 * The Global object Proxy.
 */
URL.R = {
    get protocol() {
        return URL.G.protocol;
    },
    set protocol(v) {
        return;
        URL.G.protocol = v;
    },
    get user() {
        return URL.G.user;
    },
    set user(v) {
        return;
        URL.G.user = v;
    },
    get pwd() {
        return URL.G.pwd;
    },
    set pwd(v) {
        return;
        URL.G.pwd = v;
    },
    get host() {
        return URL.G.host;
    },
    set host(v) {
        return;
        URL.G.host = v;
    },
    get port() {
        return URL.G.port;
    },
    set port(v) {
        return;
        URL.G.port = v;
    },
    get path() {
        return URL.G.path;
    },
    set path(v) {
        return;
        URL.G.path = v;
    },
    get query() {
        return URL.G.query;
    },
    set query(v) {
        return;
        URL.G.query = v;
    },
    get hash() {
        return URL.G.hash;
    },
    set hash(v) {
        return;
        URL.G.hash = v;
    },
    get map() {
        return URL.G.map;
    },
    set map(v) {
        return;
        URL.G.map = v;
    },
    setPath(path) {
        return URL.G.setPath(path);
    },
    setLocation() {
        return URL.G.setLocation();
    },
    toString() {
        return URL.G.toString();
    },
    getData(class_name = "") {
        return URL.G.getData(class_name = "");
    },
    setData(class_name = "", data = null) {
        return URL.G.setData(class_name, data);
    },
    fetchText(ALLOW_CACHE = true) {
        return URL.G.fetchText(ALLOW_CACHE);
    },
    cacheResource(resource) {
        return URL.G.cacheResource(resource);
    }
};





let SIMDATA = null;

/* Replaces the fetch actions with functions that simulate network fetches. Resources are added by the user to a Map object. */
URL.simulate = function() {
    SIMDATA = new Map;
    URL.prototype.fetchText = async d => ((d = this.toString()), SIMDATA.get(d)) ? SIMDATA.get(d) : "";
    URL.prototype.fetchJSON = async d => ((d = this.toString()), SIMDATA.get(d)) ? JSON.parse(SIMDATA.get(d).toString()) : {};
};

//Allows simulated resources to be added as a key value pair, were the key is a URI string and the value is string data.
URL.addResource = (n, v) => (n && v && (SIMDATA || (SIMDATA = new Map())) && SIMDATA.set(n.toString(), v.toString));

URL.polyfill = async function() {

    if (typeof(global) !== "undefined") {

        const 
            fs = (await new Promise(function (resolve) { resolve(_interopNamespace(require('fs'))); })).promises,
            path = (await new Promise(function (resolve) { resolve(_interopNamespace(require('path'))); }));


        global.Location = (class extends URL {});

        global.document = global.document || {};

        global.document.location = new URL(process.cwd() + "/");
        /**
         * Global `fetch` polyfill - basic support
         */
        global.fetch = async (url, data) => {
            let
                p = path.resolve(process.cwd(), "" + url),
                d = await fs.readFile(p, "utf8");

            try {
                return {
                    status: 200,
                    text: () => {
                        return {
                            then: (f) => f(d)
                        }
                    }
                };
            } catch (err) {
                throw err;
            }
        };
    }
};

Object.freeze(URL.R);
Object.freeze(URL.RC);
Object.seal(URL);

class CSS_URL extends URL {
    static parse(l) {
        if (l.tx == "url" || l.tx == "uri") {
            l.next().a("(");
            let v = "";
            if (l.ty == l.types.str) {
                v = l.tx.slice(1,-1);
                l.next().a(")");
            } else {
                const p = l.peek();
                while (!p.END && p.next().tx !== ")") { /* NO OP */ }
                v = p.slice(l);
                l.sync().a(")");
            }
            return new CSS_URL(v);
        } if (l.ty == l.types.str){
            let v = l.tx.slice(1,-1);
            l.next();
            return new CSS_URL(v);
        }

        return null;
    }
}

class CSS_String extends String {

    static parse(l) {
        if (l.ty == l.types.str) {
            let tx = l.tx;
            l.next();
            return new CSS_String(tx);
        }
        return null;
    }

    constructor(string) {
        //if(string[0] == "\"" || string[0] == "\'" || string[0] == "\'")
        //    string = string.slice(1,-1);
        super(string);
    }
}

class CSS_Id extends String {
    static parse(l) {
        if (l.ty == l.types.id) {
            let tx = l.tx;
            l.next();
            return new CSS_Id(tx);
        }
        return null;
    }
}

/* https://www.w3.org/TR/css-shapes-1/#typedef-basic-shape */
class CSS_Shape extends Array {
    static parse(l) {
        if (l.tx == "inset" || l.tx == "circle" || l.tx == "ellipse" || l.tx == "polygon" || l.tx == "rect") {
            l.next().a("(");
            let v = "";
            if (l.ty == l.types.str) {
                v = l.tx.slice(1,-1);
                l.next().a(")");
            } else {
                let p = l.pk;
                while (!p.END && p.next().tx !== ")") { /* NO OP */ }
                v = p.slice(l);
                l.sync().a(")");
            }
            return new CSS_Shape(v);
        }
        return null;
    }
}

class CSS_Number extends Number {

    static parse(l) {
        
        let sign = 1;

        if(l.ch == "-" && l.pk.ty == l.types.num){
        	l.sync();
        	sign = -1;
        }

        if(l.ty == l.types.num){
        	let tx = l.tx;
            l.next();
            return new CSS_Number(sign*(new Number(tx)));
        }
        return null;
    }
}

class Point2D extends Float64Array{
	
	constructor(x, y) {
		super(2);

		if (typeof(x) == "number") {
			this[0] = x;
			this[1] = y;
			return;
		}

		if (x instanceof Array) {
			this[0] = x[0];
			this[1] = x[1];
		}
	}

	draw(ctx, s = 1){
		ctx.beginPath();
		ctx.moveTo(this.x*s,this.y*s);
		ctx.arc(this.x*s, this.y*s, s*0.01, 0, 2*Math.PI);
		ctx.stroke();
	}

	get x (){ return this[0]}
	set x (v){if(typeof(v) !== "number") return; this[0] = v;}

	get y (){ return this[1]}
	set y (v){if(typeof(v) !== "number") return; this[1] = v;}
}

const sqrt = Math.sqrt;
const cos = Math.cos;
const acos = Math.acos;
const PI = Math.PI; 
const pow = Math.pow;

// A helper function to filter for values in the [0,1] interval:
function accept(t) {
  return 0<=t && t <=1;
}

// A real-cuberoots-only function:
function cuberoot(v) {
  if(v<0) return -pow(-v,1/3);
  return pow(v,1/3);
}

function point(t, p1, p2, p3, p4) {
	var ti = 1 - t;
	var ti2 = ti * ti;
	var t2 = t * t;
	return ti * ti2 * p1 + 3 * ti2 * t * p2 + t2 * 3 * ti * p3 + t2 * t * p4;
}


class CBezier extends Float64Array{
	constructor(x1, y1, x2, y2, x3, y3, x4, y4) {
		super(8);

		//Map P1 and P2 to {0,0,1,1} if only four arguments are provided; for use with animations
		if(arguments.length == 4){
			this[0] = 0;
			this[1] = 0;
			this[2] = x1;
			this[3] = y1;
			this[4] = x2;
			this[5] = y2;
			this[6] = 1;
			this[7] = 1;
			return;
		}
		
		if (typeof(x1) == "number") {
			this[0] = x1;
			this[1] = y1;
			this[2] = x2;
			this[3] = y2;
			this[4] = x3;
			this[5] = y3;
			this[6] = x4;
			this[7] = y4;
			return;
		}

		if (x1 instanceof Array) {
			this[0] = x1[0];
			this[1] = x1[1];
			this[2] = x1[2];
			this[3] = x1[3];
			this[4] = x1[4];
			this[5] = x1[5];
			this[6] = x1[6];
			this[7] = x1[4];
			return;
		}
	}

	get x1 (){ return this[0]}
	set x1 (v){this[0] = v;}
	get x2 (){ return this[2]}
	set x2 (v){this[2] = v;}
	get x3 (){ return this[4]}
	set x3 (v){this[4] = v;}
	get x4 (){ return this[6]}
	set x4 (v){this[6] = v;}
	get y1 (){ return this[1]}
	set y1 (v){this[1] = v;}
	get y2 (){ return this[3]}
	set y2 (v){this[3] = v;}
	get y3 (){ return this[5]}
	set y3 (v){this[5] = v;}
	get y4 (){ return this[7]}
	set y4 (v){this[7] = v;}

	add(x,y = 0){
		return new CCurve(
			this[0] + x,
			this[1] + y,
			this[2] + x,
			this[3] + y,
			this[4] + x,
			this[5] + y,
			this[6] + x,
			this[7] + y
		)
	}

	valY(t){
		return point(t, this[1], this[3], this[5], this[7]);
	}

	valX(t){
		return point(t, this[0], this[2], this[4], this[6]);
	}

	point(t) {
		return new Point2D(
			point(t, this[0], this[2], this[4], this[6]),
			point(t, this[1], this[3], this[5], this[7])
		)
	}
	
	/** 
		Acquired from : https://pomax.github.io/bezierinfo/
		Author:  Mike "Pomax" Kamermans
		GitHub: https://github.com/Pomax/
	*/

	roots(p1,p2,p3,p4) {
		var d = (-p1 + 3 * p2 - 3 * p3 + p4),
			a = (3 * p1 - 6 * p2 + 3 * p3) / d,
			b = (-3 * p1 + 3 * p2) / d,
			c = p1 / d;

		var p = (3 * b - a * a) / 3,
			p3 = p / 3,
			q = (2 * a * a * a - 9 * a * b + 27 * c) / 27,
			q2 = q / 2,
			discriminant = q2 * q2 + p3 * p3 * p3;

		// and some variables we're going to use later on:
		var u1, v1, root1, root2, root3;

		// three possible real roots:
		if (discriminant < 0) {
			var mp3 = -p / 3,
				mp33 = mp3 * mp3 * mp3,
				r = sqrt(mp33),
				t = -q / (2 * r),
				cosphi = t < -1 ? -1 : t > 1 ? 1 : t,
				phi = acos(cosphi),
				crtr = cuberoot(r),
				t1 = 2 * crtr;
			root1 = t1 * cos(phi / 3) - a / 3;
			root2 = t1 * cos((phi + 2 * PI) / 3) - a / 3;
			root3 = t1 * cos((phi + 4 * PI) / 3) - a / 3;
			return [root3, root1, root2]
		}

		// three real roots, but two of them are equal:
		if (discriminant === 0) {
			u1 = q2 < 0 ? cuberoot(-q2) : -cuberoot(q2);
			root1 = 2 * u1 - a / 3;
			root2 = -u1 - a / 3;
			return [root2, root1];
		}

		// one real root, two complex roots
		var sd = sqrt(discriminant);
		u1 = cuberoot(sd - q2);
		v1 = cuberoot(sd + q2);
		root1 = u1 - v1 - a / 3;
		return [root1];
	}

	rootsY() {
		return this.roots(this[1],this[3],this[5],this[7]);
	}

	rootsX() {
		return this.roots(this[0],this[2],this[4],this[6]);
	}
	
	getYatX(x){
		var x1 = this[0] - x, x2 = this[2] - x, x3 = this[4] - x, x4 = this[6] - x,
			x2_3 = x2 * 3, x1_3 = x1 *3, x3_3 = x3 * 3,
			d = (-x1 + x2_3 - x3_3 + x4), di = 1/d, i3 = 1/3,
			a = (x1_3 - 6 * x2 + x3_3) * di,
			b = (-x1_3 + x2_3) * di,
			c = x1 * di,
			p = (3 * b - a * a) * i3,
			p3 = p * i3,
			q = (2 * a * a * a - 9 * a * b + 27 * c) * (1/27),
			q2 = q * 0.5,
			discriminant = q2 * q2 + p3 * p3 * p3;

		// and some variables we're going to use later on:
		var u1, v1, root;

		//Three real roots can never happen if p1(0,0) and p4(1,1);

		// three real roots, but two of them are equal:
		if (discriminant < 0) {
			var mp3 = -p / 3,
				mp33 = mp3 * mp3 * mp3,
				r = sqrt(mp33),
				t = -q / (2 * r),
				cosphi = t < -1 ? -1 : t > 1 ? 1 : t,
				phi = acos(cosphi),
				crtr = cuberoot(r),
				t1 = 2 * crtr;
			root = t1 * cos((phi + 4 * PI) / 3) - a / 3;
		}else if (discriminant === 0) {
			u1 = q2 < 0 ? cuberoot(-q2) : -cuberoot(q2);
			root = -u1 - a * i3;
		}else{
			var sd = sqrt(discriminant);
			// one real root, two complex roots
			u1 = cuberoot(sd - q2);
			v1 = cuberoot(sd + q2);
			root = u1 - v1 - a * i3;	
		}

		return point(root, this[1], this[3], this[5], this[7]);
	}
	/**
		Given a Canvas 2D context object and scale value, strokes a cubic bezier curve.
	*/
	draw(ctx, s = 1){
		ctx.beginPath();
		ctx.moveTo(this[0]*s, this[1]*s);
		ctx.bezierCurveTo(
			this[2]*s, this[3]*s,
			this[4]*s, this[5]*s,
			this[6]*s, this[7]*s
			);
		ctx.stroke();
	}
}

function curvePoint(curve, t) {
    var point = {
        x: 0,
        y: 0
    };
    point.x = posOnCurve(t, curve[0], curve[2], curve[4]);
    point.y = posOnCurve(t, curve[1], curve[3], curve[5]);
    return point;
}

function posOnCurve(t, p1, p2, p3) {
    var ti = 1 - t;
    return ti * ti * p1 + 2 * ti * t * p2 + t * t * p3;
}

function splitCurve(bp, t) {
    var left = [];
    var right = [];

    function drawCurve(bp, t) {
        if (bp.length == 2) {
            left.push(bp[0], bp[1]);
            right.push(bp[0], bp[1]);
        } else {
            var new_bp = []; //bp.slice(0,-2);
            for (var i = 0; i < bp.length - 2; i += 2) {
                if (i == 0) {
                    left.push(bp[i], bp[i + 1]);
                }
                if (i == bp.length - 4) {
                    right.push(bp[i + 2], bp[i + 3]);
                }
                new_bp.push((1 - t) * bp[i] + t * bp[i + 2]);
                new_bp.push((1 - t) * bp[i + 1] + t * bp[i + 3]);
            }
            drawCurve(new_bp, t);
        }
    }

    drawCurve(bp, t);

    return {
        x: new QBezier(right),
        y: new QBezier(left)
    };
}

function curveIntersections(p1, p2, p3) {
    var intersections = {
        a: Infinity,
        b: Infinity
    };

    var a = p1 - 2 * p2 + p3;

    var b = 2 * (p2 - p1);

    var c = p1;

    if (b == 0) {} else if (Math.abs(a) < 0.00000000005) {
        intersections.a = (-c / b); //c / b;
    } else {

        intersections.a = ((-b - Math.sqrt((b * b) - 4 * a * c)) / (2 * a));
        intersections.b = ((-b + Math.sqrt((b * b) - 4 * a * c)) / (2 * a));
    }
    return intersections
}

class QBezier {
    constructor(x1, y1, x2, y2, x3, y3) {
        this.x1 = 0;
        this.x2 = 0;
        this.x3 = 0;
        this.y1 = 0;
        this.y2 = 0;
        this.y3 = 0;

        if (typeof(x1) == "number") {
            this.x1 = x1;
            this.x2 = x2;
            this.x3 = x3;
            this.y1 = y1;
            this.y2 = y2;
            this.y3 = y3;
            return;
        }

        if (x1 instanceof QBezier) {
            this.x1 = x1.x1;
            this.x2 = x1.x2;
            this.x3 = x1.x3;
            this.y1 = x1.y1;
            this.y2 = x1.y2;
            this.y3 = x1.y3;
            return;
        }

        if (x1 instanceof Array) {
            this.x1 = x1[0];
            this.y1 = x1[1];
            this.x2 = x1[2];
            this.y2 = x1[3];
            this.x3 = x1[4];
            this.y3 = x1[5];
            return;
        }
    }

    reverse() {
        return new QBezier(
            this.x3,
            this.y3,
            this.x2,
            this.y2,
            this.x1,
            this.y1
        )
    }

    point(t) {
        return new Point2D(
            posOnCurve(t, this.x1, this.x2, this.x3),
            posOnCurve(t, this.y1, this.y2, this.y3))

    }

    tangent(t) {
        var tan = {
            x: 0,
            y: 0
        };

        var px1 = this.x2 - this.x1;
        var py1 = this.y2 - this.y1;

        var px2 = this.x3 - this.x2;
        var py2 = this.y3 - this.y2;

        tan.x = (1 - t) * px1 + t * px2;
        tan.y = (1 - t) * py1 + t * py2;

        return tan;
    }

    toArray() {
        return [this.x1, this.y1, this.x2, this.y2, this.x3, this.y3];
    }

    split(t) {
        return splitCurve(this.toArray(), t);
    }

    rootsX() {
        return this.roots(
            this.x1,
            this.x2,
            this.x3
        )

    }

    roots(p1, p2, p3) {
        var curve = this.toArray();

        var c = p1 - (2 * p2) + p3;
        var b = 2 * (p2 - p1);
        var a = p1;
        var a2 = a * 2;
        var sqrt = Math.sqrt(b * b - (a * 4 * c));
        var t1 = (-b + sqrt) / a2;
        var t2 = (-b - sqrt) / a2;

        return [t1, t2];
    }

    rootsa() {
        var curve = this.toArray();

        var p1 = curve[1];
        var p2 = curve[3];
        var p3 = curve[5];
        var x1 = curve[0];
        var x2 = curve[2];
        var x3 = curve[4];

        var py1d = 2 * (p2 - p1);
        var py2d = 2 * (p3 - p2);
        var ad1 = -py1d + py2d;
        var bd1 = py1d;

        var px1d = 2 * (x2 - x1);
        var px2d = 2 * (x3 - x2);
        var ad2 = -px1d + px2d;
        var bd2 = px1d;

        var t1 = -bd1 / ad1;
        var t2 = -bd2 / ad2;

        return [t1, t2];
    }

    boundingBox() {
        var x1 = curve[0];
        var y1 = curve[1];
        var x2 = curve[2];
        var y2 = curve[3];
        var x3 = curve[4];
        var y3 = curve[5];
        var roots = getRootsClamped(curve);
        var min_x = Math.min(x1, x2, x3, roots.y[0] || Infinity, roots.x[0] || Infinity);
        var min_y = Math.min(y1, y2, y3, roots.y[1] || Infinity, roots.x[1] || Infinity);
        var max_x = Math.max(x1, x2, x3, roots.y[0] || -Infinity, roots.x[0] || -Infinity);
        var max_y = Math.max(y1, y2, y3, roots.y[1] || -Infinity, roots.x[1] || -Infinity);

        return {
            min: {
                x: min_x,
                y: min_y
            },
            max: {
                x: max_x,
                y: max_y
            }
        };
    }

    rotate(angle, offset) {
        angle = (angle / 180) * Math.PI;

        var new_curve = this.toArray();

        for (var i = 0; i < 6; i += 2) {
            var x = curve[i] - offset.x;
            var y = curve[i + 1] - offset.y;
            new_curve[i] = ((x * Math.cos(angle) - y * Math.sin(angle))) + offset.x;
            new_curve[i + 1] = ((x * Math.sin(angle) + y * Math.cos(angle))) + offset.y;
        }

        return new QBezier(new_curve);
    }

    intersects() {
        return {
            x: curveIntersections(this.x1, this.x2, this.x3),
            y: curveIntersections(this.y1, this.y2, this.y3)
        }
    }

    add(x, y) {
        if (typeof(x) == "number") {
            return new QBezier(
                this.x1 + x,
                this.y1 + y,
                this.x2 + x,
                this.y2 + y,
                this.x3 + x,
                this.y3 + y,
            )
        }
    }
}

class CSS_Bezier extends CBezier {
	static parse(l) {

		let out = null;

		switch(l.tx){
			case "cubic":
				l.next().a("(");
				let v1 = parseFloat(l.tx);
				let v2 = parseFloat(l.next().a(",").tx);
				let v3 = parseFloat(l.next().a(",").tx);
				let v4 = parseFloat(l.next().a(",").tx);
				l.a(")");
				out = new CSS_Bezier(v1, v2, v3, v4);
				break;
			case "ease":
				l.next();
				out = new CSS_Bezier(0.25, 0.1, 0.25, 1);
				break;
			case "ease-in":
				l.next();
				out = new CSS_Bezier(0.42, 0, 1, 1);
				break;
			case "ease-out":
				l.next();
				out = new CSS_Bezier(0, 0, 0.58, 1);
				break;
			case "ease-in-out":
				l.next();
				out = new CSS_Bezier(0.42, 0, 0.58, 1);
				break;
		}

		return out;
	}

	toString(){
		 return `cubic-bezier(${this[2]},${this[3]},${this[4]},${this[5]})`;
	}
}

class Stop{
    constructor(color, percentage){
        this.color = color;
        this.percentage = percentage || null;
    }

    toString(){
        return `${this.color}${(this.percentage)?" "+this.percentage:""}`;
    }
}

class CSS_Gradient{

    static parse(l) {
        let tx = l.tx,
            pky = l.pk.ty;
        if (l.ty == l.types.id) {
            switch(l.tx){
                case "linear-gradient":
                l.next().a("(");
                let dir,num,type ="deg";
                if(l.tx == "to"){

                }else if(l.ty == l.types.num){
                    num = parseFloat(l.ty);
                    type = l.next().tx;
                    l.next().a(',');
                }

                let stops = [];
                
                while(!l.END && l.ch != ")"){
                    let v = CSS_Color.parse(l, rule, r);
                    let len = null;

                    if(l.ch == ")") {
                        stops.push(new Stop(v, len));
                        break;
                    }
                    
                    if(l.ch != ","){
                        if(!(len = CSS_Length.parse(l)))
                            len = CSS_Percentage.parse(l);
                    }else
                        l.next();
                    

                    stops.push(new Stop(v, len));
                }
                l.a(")");
                let grad = new CSS_Gradient();
                grad.stops = stops;
                return grad;
            }
        }
        return null;
    }


    constructor(type = 0){
        this.type = type; //linear gradient
        this.direction = new CSS_Length(0, "deg");
        this.stops = [];
    }

    toString(){
        let str = [];
        switch(this.type){
            case 0:
            str.push("linear-gradient(");
            if(this.direction !== 0)
                str.push(this.direction.toString() + ",");
            break;
        }

        for(let i = 0; i < this.stops.length; i++)
            str.push(this.stops[i].toString()+((i<this.stops.length-1)?",":""));

        str.push(")");

        return str.join(" ");
    }
}

const $medh = (prefix) => ({
    parse: (l, r, a, n = 0) => (n = CSS_Length.parse(l, r, a), (prefix > 0) ? ((prefix > 1) ? (win) => win.innerHeight <= n : (win) => win.innerHeight >= n) : (win) => win.screen.height == n)
});


const $medw = (prefix) => ({
    parse: (l, r, a, n = 0) => 
        (n = CSS_Length.parse(l, r, a), (prefix > 0) ? ((prefix > 1) ? (win) => win.innerWidth >= n : (win) => win.innerWidth <= n) : (win) => win.screen.width == n)
});

function CSS_Media_handle(type, prefix) {
    switch (type) {
        case "h":
            return $medh(prefix);
        case "w":
            return $medw(prefix);
    }

    return {
        parse: function(a) {
            debugger;
        }
    };
}

function getValue(lex, attribute) {
    let v = lex.tx,
        mult = 1;

    if (v == "-")
        v = lex.n.tx, mult = -1;

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

    if(typeof(string) == "string")
        lex = whind$1(string);
    
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
                
                if(sx2 !== 0)
                    transform.sx = (sx1 + sx2) * 0.5;
                else
                    transform.sx = sx1;

                if(sy1 !== 0)
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
            case "scale":
                transform.sx = getValue(lex.a("("), "left");
                if(lex.ch ==","){
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
                break;
            case "rotate":
                transform.r = getValue(lex.a("("));
                lex.a(")");
                continue;
            case "rotateX":
                break;
            case "rotateY":
                break;
            case "rotateZ":
                break;
            case "rotate3d":
                break;
            case "perspective":
                break;
        }
        lex.next();
    }
}
// A 2D transform composition of 2D position, 2D scale, and 1D rotation.

class CSS_Transform2D extends Float64Array {
    static ToString(pos = [0, 0], scl = [1, 1], rot = 0) {
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
        
        if(r !== 0){
            cos = Math.cos(r);
            sin = Math.sin(r);
        }

        return `matrix(${cos * sx}, ${-sin * sx}, ${sy * sin}, ${sy * cos}, ${px}, ${py})`;
    }


    constructor(px, py, sx, sy, r) {
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
            } else if (typeof(px) == "string") ParseString(px, this);
            else {
                this[0] = px;
                this[1] = py;
                this[2] = sx;
                this[3] = sy;
                this[4] = r;
            }
        }
    }
    get px() {
        return this[0];
    }
    set px(v) {
        this[0] = v;
    }
    get py() {
        return this[1];
    }
    set py(v) {
        this[1] = v;
    }
    get sx() {
        return this[2];
    }
    set sx(v) {
        this[2] = v;
    }
    get sy() {
        return this[3];
    }
    set sy(v) {
        this[3] = v;
    }
    get r() {
        return this[4];
    }
    set r(v) {
        this[4] = v;
    }

    set scale(s){
        this.sx = s;
        this.sy = s;
    }

    get scale(){
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


        if (typeof(v) == "string")
            ParseString(v, copy);

        return copy;
    }

    /**
     * Sets the transform value of a canvas 2D context;
     */
    setCTX(ctx){       
        let cos = 1, sin = 0;
        if(this[4] != 0){
            cos = Math.cos(this[4]);
            sin = Math.sin(this[4]);
        }
        ctx.transform(cos * this[2], -sin * this[2], this[3] * sin, this[3] * cos, this[0], this[1]);
    }

    getLocalX(X){
        return (X - this.px) / this.sx;
    }

    getLocalY(Y){
        return (Y - this.py) / this.sy;
    }
}

/**
 * @brief Path Info
 * @details Path syntax information for reference
 * 
 * MoveTo: M, m
 * LineTo: L, l, H, h, V, v
 * Cubic Bézier Curve: C, c, S, s
 * Quadratic Bézier Curve: Q, q, T, t
 * Elliptical Arc Curve: A, a
 * ClosePath: Z, z
 * 
 * Capital symbols represent absolute positioning, lowercase is relative
 */
const PathSym = {
    M: 0,
    m: 1,
    L: 2,
    l: 3,
    h: 4,
    H: 5,
    V: 6,
    v: 7,
    C: 8,
    c: 9,
    S: 10,
    s: 11,
    Q: 12,
    q: 13,
    T: 14,
    t: 15,
    A: 16,
    a: 17,
    Z: 18,
    z: 19,
    pairs: 20
};

function getSignedNumber(lex) {
    let mult = 1,
        tx = lex.tx;
    if (tx == "-") {
        mult = -1;
        tx = lex.n.tx;
    }
    lex.next();
    return parseFloat(tx) * mult;
}

function getNumberPair(lex, array) {
    let x = getSignedNumber(lex);
    if (lex.ch == ',') lex.next();
    let y = getSignedNumber(lex);
    array.push(x, y);
}

function parseNumberPairs(lex, array) {
    while ((lex.ty == lex.types.num || lex.ch == "-") && !lex.END) {    	
    	array.push(PathSym.pairs);
        getNumberPair(lex, array);
    }
}
/**
 * @brief An array store of path data in numerical form
 */
class CSS_Path extends Array {
    static FromString(string, array) {
        let lex = whind(string);
        while (!lex.END) {
            let relative = false,
                x = 0,
                y = 0;
            switch (lex.ch) {
                //Move to
                case "m":
                    relative = true;
                case "M":
                    lex.next(); //
                    array.push((relative) ? PathSym.m : PathSym.M);
                    getNumberPair(lex, array);
                    parseNumberPairs(lex, array);
                    continue;
                    //Line to
                case "h":
                    relative = true;
                case "H":
                    lex.next();
                    x = getSignedNumber(lex);
                    array.push((relative) ? PathSym.h : PathSym.H, x);
                    continue;
                case "v":
                    relative = true;
                case "V":
                    lex.next();
                    y = getSignedNumber(lex);
                    array.push((relative) ? PathSym.v : PathSym.V, y);
                    continue;
                case "l":
                    relative = true;
                case "L":
                    lex.next();
                    array.push((relative) ? PathSym.l : PathSym.L);
                    getNumberPair(lex, array);
                    parseNumberPairs(lex, array);
                    continue;
                    //Cubic Curve
                case "c":
                    relative = true;
                case "C":
                    array.push((relative) ? PathSym.c : PathSym.C);
                    getNumberPair(lex, array);
                    getNumberPair(lex, array);
                    getNumberPair(lex, array);
                    parseNumberPairs(lex, array);
                    continue;
                case "s":
                    relative = true;
                case "S":
                    array.push((relative) ? PathSym.s : PathSym.S);
                    getNumberPair(lex, array);
                    getNumberPair(lex, array);
                    parseNumberPairs(lex, array);
                    continue;
                    //Quadratic Curve0
                case "q":
                    relative = true;
                case "Q":
                    array.push((relative) ? PathSym.q : PathSym.Q);
                    getNumberPair(lex, array);
                    getNumberPair(lex, array);
                    parseNumberPairs(lex, array);
                    continue;
                case "t":
                    relative = true;
                case "T":
                    array.push((relative) ? PathSym.t : PathSym.T);
                    getNumberPair(lex, array);
                    parseNumberPairs(lex, array);
                    continue;
                    //Elliptical Arc
                    //Close path:
                case "z":
                    relative = true;
                case "Z":
                    array.push((relative) ? PathSym.z : PathSym.Z);
            }
            lex.next();
        }
    }

    static ToString(array) {
    	let string = [], l = array.length, i = 0;
    	while(i < l){
    		switch(array[i++]){
    			case PathSym.M:
    				string.push("M", array[i++], array[i++]);
    				break;
			    case PathSym.m:
			    	string.push("m", array[i++], array[i++]);
			    	break;
			    case PathSym.L:
			    	string.push("L", array[i++], array[i++]);
			    	break;
			    case PathSym.l:
			    	string.push("l", array[i++], array[i++]);
			    	break;
			    case PathSym.h:
			    	string.push("h", array[i++]);
			    	break;
			    case PathSym.H:
			    	string.push("H", array[i++]);
			    	break;
			    case PathSym.V:
			    	string.push("V", array[i++]);
			    	break;
			    case PathSym.v:
			    	string.push("v", array[i++]);
			    	break;
			    case PathSym.C:
			    	string.push("C", array[i++], array[i++], array[i++], array[i++], array[i++], array[i++]);
			    	break;
			    case PathSym.c:
			    	string.push("c", array[i++], array[i++], array[i++], array[i++], array[i++], array[i++]);
			    	break;
			    case PathSym.S:
			    	string.push("S", array[i++], array[i++], array[i++], array[i++]);
			    	break;
			    case PathSym.s:
			    	string.push("s", array[i++], array[i++], array[i++], array[i++]);
			    	break;
			    case PathSym.Q:
			    	string.push("Q", array[i++], array[i++], array[i++], array[i++]);
			    	break;
			    case PathSym.q:
			    	string.push("q", array[i++], array[i++], array[i++], array[i++]);
			    	break;
			    case PathSym.T:
			    	string.push("T", array[i++], array[i++]);
			    	break;
			    case PathSym.t:
			    	string.push("t", array[i++], array[i++]);
			    	break;
			    case PathSym.Z:
			    	string.push("Z");
			    	break;
			    case PathSym.z:
			    	string.push("z");
			    	break;
			    case PathSym.pairs:
			    	string.push(array[i++], array[i++]);
			    	break;
			 	case PathSym.A:
			    case PathSym.a:
			    default:
			    	i++;
    		}
    	}

    	return string.join(" ");
    }

    
    constructor(data) {
        super();	

    	if(typeof(data) == "string"){
    		Path.FromString(data, this);
    	}else if(Array.isArray(data)){
    		for(let i = 0; i < data.length;i++){
    			this.push(parseFloat(data[i]));
    		}
    	}
    }

    toString(){
    	return Path.ToString(this);
    }

    lerp(to, t, array = new Path){
    	let l = Math.min(this.length, to.length);

    	for(let i = 0; i < l; i++)
    		array[i] = this[i] + (to[i] - this[i]) * t;

    	return array;
    }	
}

class CSS_FontName extends String {
	static parse(l) {

		if(l.ty == l.types.str){
			let tx = l.tx;
            l.next();
			return new CSS_String(tx);
		}		

		if(l.ty == l.types.id){

			let pk = l.peek();

			while(pk.type == l.types.id && !pk.END){
				pk.next();
			}

			let str = pk.slice(l);
			
			l.sync();
			return new CSS_String(str);
		}

        return null;
    }
}

/**
 * CSS Type constructors
 */
const types = {
	color: CSS_Color,
	length: CSS_Length,
	time: CSS_Length,
	flex: CSS_Length,
	angle: CSS_Length,
	frequency: CSS_Length,
	resolution: CSS_Length,
	percentage: CSS_Percentage,
	url: CSS_URL,
	uri: CSS_URL,
	number: CSS_Number,
	id: CSS_Id,
	string: CSS_String,
	shape: CSS_Shape,
	cubic_bezier: CSS_Bezier,
	integer: CSS_Number,
	gradient: CSS_Gradient,
	transform2D : CSS_Transform2D,
	path: CSS_Path,
	fontname: CSS_FontName,

	/* Media parsers */
	m_width: CSS_Media_handle("w", 0),
	m_min_width: CSS_Media_handle("w", 1),
	m_max_width: CSS_Media_handle("w", 2),
	m_height: CSS_Media_handle("h", 0),
	m_min_height: CSS_Media_handle("h", 1),
	m_max_height: CSS_Media_handle("h", 2),
	m_device_width: CSS_Media_handle("dw", 0),
	m_min_device_width: CSS_Media_handle("dw", 1),
	m_max_device_width: CSS_Media_handle("dw", 2),
	m_device_height: CSS_Media_handle("dh", 0),
	m_min_device_height: CSS_Media_handle("dh", 1),
	m_max_device_height: CSS_Media_handle("dh", 2)
};

/**
 * CSS Property Definitions https://www.w3.org/TR/css3-values/#value-defs
 */
const property_definitions = {

	/* https://drafts.csswg.org/css-writing-modes-3/ */
		direction:"ltr|rtl",
		unicode_bidi:"normal|embed|isolate|bidi-override|isolate-override|plaintext",
		writing_mode:"horizontal-tb|vertical-rl|vertical-lr",
		text_orientation:"mixed|upright|sideways",
		glyph_orientation_vertical:`auto|0deg|90deg|"0"|"90"`,
		text_combine_upright:"none|all",

	/* https://www.w3.org/TR/css-position-3 */ 
		position: "static|relative|absolute|sticky|fixed",
		top: `<length>|<number>|<percentage>|auto`,
		left: `<length>|<number>|<percentage>|auto`,
		bottom: `<length>|<number>|<percentage>|auto`,
		right: `<length>|<number>|<percentage>|auto`,
		offset_before: `<length>|<percentage>|auto`,
		offset_after: `<length>|<percentage>|auto`,
		offset_start: `<length>|<percentage>|auto`,
		offset_end: `<length>|<percentage>|auto`,
		z_index:"auto|<integer>",

	/* https://www.w3.org/TR/css-display-3/ */
		display: `[ <display_outside> || <display_inside> ] | <display_listitem> | <display_internal> | <display_box> | <display_legacy>`,

	/* https://www.w3.org/TR/css-box-3 */
		margin: `[<length>|<percentage>|0|auto]{1,4}`,
		margin_top: `<length>|<percentage>|0|auto`,
		margin_right: `<length>|<percentage>|0|auto`,
		margin_bottom: `<length>|<percentage>|0|auto`,
		margin_left: `<length>|<percentage>|0|auto`,

		margin_trim:"none|in-flow|all",

		padding: `[<length>|<percentage>|0|auto]{1,4}`,
		padding_top: `<length>|<percentage>|0|auto`,
		padding_right: `<length>|<percentage>|0|auto`,
		padding_bottom: `<length>|<percentage>|0|auto`,
		padding_left: `<length>|<percentage>|0|auto`,

	/* https://www.w3.org/TR/CSS2/visuren.html */
		float: `left|right|none`,
		clear: `left|right|both|none`,

	/* https://drafts.csswg.org/css-sizing-3 todo:implement fit-content(%) function */
		box_sizing: `content-box | border-box`,
		width: `<length>|<percentage>|min-content|max-content|fit-content|auto`,
		height: `<length>|<percentage>|min-content|max-content|fit-content|auto`,
		min_width: `<length>|<percentage>|min-content|max-content|fit-content|auto`,
		max_width: `<length>|<percentage>|min-content|max-content|fit-content|auto|none`,
		min_height: `<length>|<percentage>|min-content|max-content|fit-content|auto`,
		max_height: `<length>|<percentage>|min-content|max-content|fit-content|auto|none`,

	/* https://www.w3.org/TR/2018/REC-css-color-3-20180619 */
		color: `<color>`,
		opacity: `<alphavalue>`,

	/* https://www.w3.org/TR/css-backgrounds-3/ */
		background_color: `<color>|red`,
		background_image: `<bg_image>#`,
		background_repeat: `<repeat_style>#`,
		background_attachment: `scroll|fixed|local`,
		background_position: `[<percentage>|<length>]{1,2}|[top|center|bottom]||[left|center|right]`,
		background_clip: `<box>#`,
		background_origin: `<box>#`,
		background_size: `<bg_size>#`,
		background: `[<bg_layer>#,]?<final_bg_layer>`,
		border_color: `<color>{1,4}`,
		border_top_color: `<color>`,
		border_right_color: `<color>`,
		border_bottom_color: `<color>`,
		border_left_color: `<color>`,

		border_top_width: `<line_width>`,
		border_right_width: `<line_width>`,
		border_bottom_width: `<line_width>`,
		border_left_width: `<line_width>`,
		border_width: `<line_width>{1,4}`,

		border_style: `<line_style>{1,4}`,
		border_top_style: `<line_style>`,
		border_right_style: `<line_style>`,
		border_bottom_style: `<line_style>`,
		border_left_style: `<line_style>`,

		border_top: `<line_width>||<line_style>||<color>`,
		border_right: `<line_width>||<line_style>||<color>`,
		border_bottom: `<line_width>||<line_style>||<color>`,
		border_left: `<line_width>||<line_style>||<color>`,

		border_radius: `<length_percentage>{1,4}[ / <length_percentage>{1,4}]?`,
		border_top_left_radius: `<length_percentage>{1,2}`,
		border_top_right_radius: `<length_percentage>{1,2}`,
		border_bottom_right_radius: `<length_percentage>{1,2}`,
		border_bottom_left_radius: `<length_percentage>{1,2}`,

		border: `<line_width>||<line_style>||<color>`,

		border_image: `<border_image_source>||<border_image_slice>[/<border_image_width>|/<border_image_width>?/<border_image_outset>]?||<border_image_repeat>`,
		border_image_source: `none|<image>`,
		border_image_slice: `[<number>|<percentage>]{1,4}&&fill?`,
		border_image_width: `[<length_percentage>|<number>|auto]{1,4}`,
		border_image_outset: `[<length>|<number>]{1,4}`,
		border_image_repeat: `[stretch|repeat|round|space]{1,2}`,
		box_shadow: `none|<shadow>#`,
		line_height: `normal|<percentage>|<length>|<number>`,
		overflow: 'visible|hidden|scroll|auto',

	/* https://www.w3.org/TR/css-fonts-4 */
		font_display: "auto|block|swap|fallback|optional",
		font_family: `[<generic_family>|<family_name>]#`,
		font_language_override:"normal|<string>",
		font: `[[<font_style>||<font_variant>||<font_weight>]?<font_size>[/<line_height>]?<font_family>]|caption|icon|menu|message-box|small-caption|status-bar`,
		font_max_size: `<absolute_size>|<relative_size>|<length>|<percentage>|infinity`,
		font_min_size: `<absolute_size>|<relative_size>|<length>|<percentage>`,
		font_optical_sizing: `auto|none`,
		font_pallette: `normal|light|dark|<identifier>`,
		font_size: `<absolute_size>|<relative_size>|<length>|<percentage>`,
		font_stretch:"<percentage>|normal|ultra-condensed|extra-condensed|condensed|semi-condensed|semi-expanded|expanded|extra-expanded|ultra-expanded",
		font_style: `normal|italic|oblique<angle>?`,
		font_synthesis:"none|[weight||style]",
		font_synthesis_small_caps:"auto|none",
		font_synthesis_style:"auto|none",
		font_synthesis_weight:"auto|none",
		font_variant_alternates:"normal|[stylistic(<feature-value-name>)||historical-forms||styleset(<feature-value-name>#)||character-variant(<feature-value-name>#)||swash(<feature-value-name>)||ornaments(<feature-value-name>)||annotation(<feature-value-name>)]",
		font_variant_emoji:"auto|text|emoji|unicode",
		font_variation_settings:" normal|[<string><number>]#",
		font_size_adjust: `<number>|none`,
		
		font_weight: `normal|bold|bolder|lighter|100|200|300|400|500|600|700|800|900`,

	/* https://www.w3.org/TR/css-fonts-3/ */
		font_kerning: ` auto | normal | none`,
		font_variant: `normal|none|[<common-lig-values>||<discretionary-lig-values>||<historical-lig-values>||<contextual-alt-values>||[small-caps|all-small-caps|petite-caps|all-petite-caps|unicase|titling-caps]||<numeric-figure-values>||<numeric-spacing-values>||<numeric-fraction-values>||ordinal||slashed-zero||<east-asian-variant-values>||<east-asian-width-values>||ruby||[sub|super]]`,
		font_variant_ligatures:`normal|none|[<common-lig-values>||<discretionary-lig-values>||<historical-lig-values>||<contextual-alt-values> ]`,
		font_variant_position:`normal|sub|super`,
		font_variant_caps:`normal|small-caps|all-small-caps|petite-caps|all-petite-caps|unicase|titling-caps`,
		font_variant_numeric: "normal | [ <numeric-figure-values> || <numeric-spacing-values> || <numeric-fraction-values> || ordinal || slashed-zero ]",
		font_variant_east_asian:" normal | [ <east-asian-variant-values> || <east-asian-width-values> || ruby ]",

	/* https://drafts.csswg.org/css-text-3 */
		hanging_punctuation : "none|[first||[force-end|allow-end]||last]",
		hyphens : "none|manual|auto",
		letter_spacing: `normal|<length>`,
		line_break : "auto|loose|normal|strict|anywhere",
		overflow_wrap : "normal|break-word|anywhere",
		tab_size : "<length>|<number>",
		text_align : "start|end|left|right|center|justify|match-parent|justify-all",
		text_align_all : "start|end|left|right|center|justify|match-parent",
		text_align_last : "auto|start|end|left|right|center|justify|match-parent",
		text_indent : "[[<length>|<percentage>]&&hanging?&&each-line?]",
		text_justify : "auto|none|inter-word|inter-character",
		text_transform : "none|[capitalize|uppercase|lowercase]||full-width||full-size-kana",
		white_space : "normal|pre|nowrap|pre-wrap|break-spaces|pre-line",
		word_break : " normal|keep-all|break-all|break-word",
		word_spacing : "normal|<length>",
		word_wrap : "  normal | break-word | anywhere",

	/* https://drafts.csswg.org/css-text-decor-3 */
		text_decoration: "<text-decoration-line>||<text-decoration-style>||<color>",
		text_decoration_color:"<color>",
		text_decoration_line:"none|[underline||overline||line-through||blink]",
		text_decoration_style:"solid|double|dotted|dashed|wavy",
		text_emphasis:"<text-emphasis-style>||<text-emphasis-color>",
		text_emphasis_color:"<color>",
		text_emphasis_position:"[over|under]&&[right|left]?",
		text_emphasis_style:"none|[[filled|open]||[dot|circle|double-circle|triangle|sesame]]|<string>",
		text_shadow:"none|[<color>?&&<length>{2,3}]#",
		text_underline_position:"auto|[under||[left|right]]",

	/* Flex Box https://www.w3.org/TR/css-flexbox-1/ */
		align_content: `flex-start | flex-end | center | space-between | space-around | stretch`,
		align_items: `flex-start | flex-end | center | baseline | stretch`,
		align_self: `auto | flex-start | flex-end | center | baseline | stretch`,
		flex:`none|[<flex-grow> <flex-shrink>?||<flex-basis>]`,
		flex_basis:`content|<width>`,
		flex_direction:`row | row-reverse | column | column-reverse`,
		flex_flow:`<flex-direction>||<flex-wrap>`,
		flex_grow:`<number>`,
		flex_shrink:`<number>`,
		flex_wrap:`nowrap|wrap|wrap-reverse`,
		justify_content :"flex-start | flex-end | center | space-between | space-around",
		order:`<integer>`,

	/* https://drafts.csswg.org/css-transitions-1/ */
		transition: `<single_transition>#`,
		transition_delay: `<time>#`,
		transition_duration: `<time>#`,
		transition_property: `none|<single_transition_property>#`,
		transition_timing_function: `<timing_function>#`,

	/* CSS3 Animation https://drafts.csswg.org/css-animations-1/ */
		animation: `<single_animation>#`,
		animation_name: `[none|<keyframes_name>]#`,
		animation_duration: `<time>#`,
		animation_timing_function: `<timing_function>#`,
		animation_iteration_count: `<single_animation_iteration_count>#`,
		animation_direction: `<single_animation_direction>#`,
		animation_play_state: `<single_animation_play_state>#`,
		animation_delayed: `<time>#`,
		animation_fill_mode: `<single_animation_fill_mode>#`,

	/* https://svgwg.org/svg2-draft/interact.html#PointerEventsProperty */
		pointer_events : `visiblePainted|visibleFill|visibleStroke|visible|painted|fill|stroke|all|none|auto`,

	/* https://drafts.csswg.org/css-ui-3 */
		caret_color :"auto|<color>",
		cursor:"[[<url> [<number><number>]?,]*[auto|default|none|context-menu|help|pointer|progress|wait|cell|crosshair|text|vertical-text|alias|copy|move|no-drop|not-allowed|grab|grabbing|e-resize|n-resize|ne-resize|nw-resize|s-resize|se-resize|sw-resize|w-resize|ew-resize|ns-resize|nesw-resize|nwse-resize|col-resize|row-resize|all-scroll|zoom-in|zoom-out]]",
		outline:"[<outline-color>||<outline-style>||<outline-width>]",
		outline_color:"<color>|invert",
		outline_offset:"<length>",
		outline_style:"auto|<border-style>",
		outline_width:"<line-width>",
		resize:"none|both|horizontal|vertical",
		text_overflow:"clip|ellipsis",

	/* https://drafts.csswg.org/css-content-3/ */
		bookmark_label:"<content-list>",
		bookmark_level:"none|<integer>",
		bookmark_state:"open|closed",
		content:"normal|none|[<content-replacement>|<content-list>][/<string>]?",
		quotes:"none|[<string><string>]+",
		string_set:"none|[<custom-ident><string>+]#",
	
	/*https://www.w3.org/TR/CSS22/tables.html*/
		caption_side:"top|bottom",
		table_layout:"auto|fixed",
		border_collapse:"collapse|separate",
		border_spacing:"<length><length>?",
		empty_cells:"show|hide",

	/* https://www.w3.org/TR/CSS2/page.html */
		page_break_before:"auto|always|avoid|left|right",
		page_break_after:"auto|always|avoid|left|right",
		page_break_inside:"auto|avoid|left|right",
		orphans:"<integer>",
		widows:"<integer>",

	/* https://drafts.csswg.org/css-lists-3 */
		counter_increment:"[<custom-ident> <integer>?]+ | none",
		counter_reset:"[<custom-ident> <integer>?]+|none",
		counter_set:"[<custom-ident> <integer>?]+|none",
		list_style:"<list-style-type>||<list-style-position>||<list-style-image>",
		list_style_image:"<url>|none",
		list_style_position:"inside|outside",
		list_style_type:"<counter-style>|<string>|none",
		marker_side:"list-item|list-container",


	vertical_align: `baseline|sub|super|top|text-top|middle|bottom|text-bottom|<percentage>|<length>`,

	/* Visual Effects */
	clip: '<shape>|auto',
	visibility: `visible|hidden|collapse`,
	content: `normal|none|[<string>|<uri>|<counter>|attr(<identifier>)|open-quote|close-quote|no-open-quote|no-close-quote]+`,
	quotas: `[<string><string>]+|none`,
	counter_reset: `[<identifier><integer>?]+|none`,
	counter_increment: `[<identifier><integer>?]+|none`,
};

/* Properties that are not directly accessible by CSS prop creator */

const virtual_property_definitions = {
    /* https://drafts.csswg.org/css-counter-styles-3 */
        /*system:`cyclic|numeric|alphabetic|symbolic|additive|[fixed<integer>?]|[extends<counter-style-name>]`,
        negative:`<symbol><symbol>?`,
        prefix:`<symbol>`,
        suffix:`<symbol>`,
        range:`[[<integer>|infinite]{2}]#|auto`,
        pad:`<integer>&&<symbol>`,
        fallback:`<counter-style-name>`
        symbols:`<symbol>+`,*/

        counter_style:`<numeric_counter_style>|<alphabetic_counter_style>|<symbolic_counter_style>|<japanese_counter_style>|<korean_counter_style>|<chinese_counter_style>|ethiopic-numeric`,
        numeric_counter_style:`decimal|decimal-leading-zero|arabic-indic|armenian|upper-armenian|lower-armenian|bengali|cambodian|khmer|cjk-decimal|devanagari|georgian|gujarati|gurmukhi|hebrew|kannada|lao|malayalam|mongolian|myanmar|oriya|persian|lower-roman|upper-roman|tamil|telugu|thai|tibetan`,
        symbolic_counter_style:`disc|circle|square|disclosure-open|disclosure-closed`,
        alphabetic_counter_style:`lower-alpha|lower-latin|upper-alpha|upper-latin|cjk-earthly-branch|cjk-heavenly-stem|lower-greek|hiragana|hiragana-iroha|katakana|katakana-iroha`,
        japanese_counter_style:`japanese-informal|japanese-formal`,
        korean_counter_style:`korean-hangul-formal|korean-hanja-informal|and korean-hanja-formal`,
        chinese_counter_style:`simp-chinese-informal|simp-chinese-formal|trad-chinese-informal|and trad-chinese-formal`,

	/* https://drafts.csswg.org/css-conte-3/ */
		content_list:"[<string>|contents|<image>|<quote>|<target>|<leader()>]+",
		content_replacement:"<image>",

	/* https://drafts.csswg.org/css-values-4 */
		custom_ident:"<identifier>",
		position:"[[left|center|right]||[top|center|bottom]|[left|center|right|<length-percentage>][top|center|bottom|<length-percentage>]?|[[left|right]<length-percentage>]&&[[top|bottom]<length-percentage>]]",
	
	/* https://drafts.csswg.org/css-lists-3 */

	east_asian_variant_values:"[jis78|jis83|jis90|jis04|simplified|traditional]",

	alphavalue: '<number>',

	box: `border-box|padding-box|content-box`,

	/*Font-Size: www.w3.org/TR/CSS2/fonts.html#propdef-font-size */
	absolute_size: `xx-small|x-small|small|medium|large|x-large|xx-large`,
	relative_size: `larger|smaller`,

	/*https://www.w3.org/TR/css-backgrounds-3/#property-index*/

	bg_layer: `<bg_image>||<bg_position>[/<bg_size>]?||<repeat_style>||<attachment>||<box>||<box>`,
	final_bg_layer: `<background_color>||<bg_image>||<bg_position>[/<bg_size>]?||<repeat_style>||<attachment>||<box>||<box>`,
	bg_image: `<url>|<gradient>|none`,
	repeat_style: `repeat-x|repeat-y|[repeat|space|round|no-repeat]{1,2}`,
	background_attachment: `<attachment>#`,
	bg_size: `[<length_percentage>|auto]{1,2}|cover|contain`,
	bg_position: `[[left|center|right|top|bottom|<length_percentage>]|[left|center|right|<length_percentage>][top|center|bottom|<length_percentage>]|[center|[left|right]<length_percentage>?]&&[center|[top|bottom]<length_percentage>?]]`,
	attachment: `scroll|fixed|local`,
	line_style: `none|hidden|dotted|dashed|solid|double|groove|ridge|inset|outset`,
	line_width: `thin|medium|thick|<length>`,
	shadow: `inset?&&<length>{2,4}&&<color>?`,

	/* Font https://www.w3.org/TR/css-fonts-4/#family-name-value */
	
	family_name: `<fontname>`,
	generic_family: `serif|sans-serif|cursive|fantasy|monospace`,
	
	/* Identifier https://drafts.csswg.org/css-values-4/ */

	identifier: `<id>`,
	custom_ident: `<id>`,
	
	/* https://drafts.csswg.org/css-timing-1/#typedef-timing-function */

	timing_function: `linear|<cubic_bezier_timing_function>|<step_timing_function>|<frames_timing_function>`,
	cubic_bezier_timing_function: `<cubic_bezier>`,
	step_timing_function: `step-start|step-end|'steps()'`,
	frames_timing_function: `'frames()'`,

	/* https://drafts.csswg.org/css-transitions-1/ */

	single_animation_fill_mode: `none|forwards|backwards|both`,
	single_animation_play_state: `running|paused`,
	single_animation_direction: `normal|reverse|alternate|alternate-reverse`,
	single_animation_iteration_count: `infinite|<number>`,
	single_transition_property: `all|<custom_ident>`,
	single_transition: `[none|<single_transition_property>]||<time>||<timing_function>||<time>`,

	/* CSS3 Animation https://drafts.csswg.org/css-animations-1/ */

	single_animation: `<time>||<timing_function>||<time>||<single_animation_iteration_count>||<single_animation_direction>||<single_animation_fill_mode>||<single_animation_play_state>||[none|<keyframes_name>]`,
	keyframes_name: `<string>`,

	/* CSS3 Stuff */
	length_percentage: `<length>|<percentage>`,
	frequency_percentage: `<frequency>|<percentage>`,
	angle_percentage: `<angle>|<percentage>`,
	time_percentage: `<time>|<percentage>`,
	number_percentage: `<number>|<percentage>`,

	/*CSS Clipping https://www.w3.org/TR/css-masking-1/#clipping */
	clip_path: `<clip_source>|[<basic_shape>||<geometry_box>]|none`,
	clip_source: `<url>`,
	shape_box: `<box>|margin-box`,
	geometry_box: `<shape_box>|fill-box|stroke-box|view-box`,
	basic_shape: `<CSS_Shape>`,
	ratio: `<integer>/<integer>`,

	/* https://www.w3.org/TR/css-fonts-3/*/
	common_lig_values        : `[ common-ligatures | no-common-ligatures ]`,
	discretionary_lig_values : `[ discretionary-ligatures | no-discretionary-ligatures ]`,
	historical_lig_values    : `[ historical-ligatures | no-historical-ligatures ]`,
	contextual_alt_values    : `[ contextual | no-contextual ]`,

	//Display
	display_outside  : `block | inline | run-in`,
	display_inside   : `flow | flow-root | table | flex | grid | ruby`,
	display_listitem : `<display_outside>? && [ flow | flow-root ]? && list-item`,
	display_internal : `table-row-group | table-header-group | table-footer-group | table-row | table-cell | table-column-group | table-column | table-caption | ruby-base | ruby-text | ruby-base-container | ruby-text-container`,
	display_box      : `contents | none`,
	display_legacy   : `inline-block | inline-table | inline-flex | inline-grid`,
};

const media_feature_definitions = {
	width: "<m_width>",
	min_width: "<m_max_width>",
	max_width: "<m_min_width>",
	height: "<m_height>",
	min_height: "<m_min_height>",
	max_height: "<m_max_height>",
	orientation: "portrait  | landscape",
	aspect_ratio: "<ratio>",
	min_aspect_ratio: "<ratio>",
	max_aspect_ratio: "<ratio>",
	resolution: "<length>",
	min_resolution: "<length>",
	max_resolution: "<length>",
	scan: "progressive|interlace",
	grid: "",
	monochrome: "",
	min_monochrome: "<integer>",
	max_monochrome: "<integer>",
	color: "",
	min_color: "<integer>",
	max_color: "<integer>",
	color_index: "",
	min_color_index: "<integer>",
	max_color_index: "<integer>",

};

var step = 0;

function checkDefaults(lx) {
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

    get type(){
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

        this.parseLVL1(data instanceof whind$1.constructor ? data : whind$1(data + ""), prop_data);

        return prop_data;
    }



    parseLVL1(lx, out_val = [], ROOT = true) {

        if (typeof(lx) == "string")
            lx = whind$1(lx);

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
        } else if(out_val)
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

    get type(){
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

                        break
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
    get type(){
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
    get type(){
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

var productions = /*#__PURE__*/Object.freeze({
    checkDefaults: checkDefaults,
    JUX: JUX,
    AND: AND,
    OR: OR,
    ONE_OF: ONE_OF
});

class LiteralTerm{

    get type (){
        return "term";
    }

    constructor(value, type) {
        
        if(type == whind$1.types.string)
            value = value.slice(1,-1);

        this.value = value;
        this.HAS_PROP = false;
    }

    seal(){}

    parse(data){
        const prop_data = [];

        this.parseLVL1(data instanceof whind$1.constructor ? data : whind$1(data + ""), prop_data);

        return prop_data;
    }

    parseLVL1(l, r, root = true) {

        if (typeof(l) == "string")
            l = whind$1(l);

        if (root) {
            switch(checkDefaults(l)){
                case 1:
                rule.push(l.tx);
                return true;
                case 0:
                return false;
            }
        }

        let v = l.tx;
        
        if (v == this.value) {
            l.next();
            r.push(v);
            //if (this.HAS_PROP  && !this.virtual && root)
            //    rule[0] = v;

            return true;
        }
        return false;
    }

    get OPTIONAL (){ return false }
    set OPTIONAL (a){}
}

class ValueTerm extends LiteralTerm{

    constructor(value, getPropertyParser, definitions, productions) {
        
        super(value);

        if(value instanceof JUX)
            return value;

        this.value = null;

        const IS_VIRTUAL = { is: false };
        
        if(typeof(value) == "string")
            var u_value = value.replace(/\-/g,"_");

        if (!(this.value = types[u_value]))
            this.value = getPropertyParser(u_value, IS_VIRTUAL, definitions, productions);

        if (!this.value)
            return new LiteralTerm(value);

        if(this.value instanceof JUX){

            if (IS_VIRTUAL.is)
                this.value.virtual = true;

            return this.value;
        }
    }

    parseLVL1(l, r, ROOT = true) {
        if (typeof(l) == "string")
            l = whind$1(l);

        if (ROOT) {
            switch(checkDefaults(l)){
                case 1:
                r.push(l.tx);
                return true;
                case 0:
                return false;
            }
        }

        //const rn = [];

        const v = this.value.parse(l);

        /*if (rn.length > 0) {
            
           // r.push(...rn);

            // if (this.HAS_PROP && !this.virtual)
            //     rule[0] = rn.v;

            return true;

        } else */if (v) {

            r.push(v);

            //if (this.HAS_PROP && !this.virtual && ROOT)
            //    rule[0] = v;

            return true;
        } else
            return false;
    }
}



class SymbolTerm extends LiteralTerm {
    parseLVL1(l, rule, r) {
        if (typeof(l) == "string")
            l = whind$1(l);

        if (l.tx == this.value) {
            l.next();
            rule.push(this.value);
            return true;
        }

        return false;
    }
};

var terms = /*#__PURE__*/Object.freeze({
    LiteralTerm: LiteralTerm,
    ValueTerm: ValueTerm,
    SymbolTerm: SymbolTerm
});

//import util from "util"
const standard_productions = {
    JUX,
    AND,
    OR,
    ONE_OF,
    LiteralTerm,
    ValueTerm,
    SymbolTerm
};

function getExtendedIdentifier(l) {
    let pk = l.pk;

    let id = "";

    while (!pk.END && (pk.ty & (whind$1.types.id | whind$1.types.num)) || pk.tx == "-" || pk.tx == "_") { pk.next(); }

    id = pk.slice(l);

    l.sync();

    l.tl = 0;

    return id;
}

function getPropertyParser(property_name, IS_VIRTUAL = { is: false }, definitions = null, productions = standard_productions) {

    let parser_val = definitions[property_name];

    if (parser_val) {

        if (typeof(parser_val) == "string") {
            parser_val = definitions[property_name] = CreatePropertyParser(parser_val, property_name, definitions, productions);
        }
        parser_val.name = property_name;
        return parser_val;
    }

    if (!definitions.__virtual)
        definitions.__virtual = Object.assign({}, virtual_property_definitions);

    parser_val = definitions.__virtual[property_name];

    if (parser_val) {

        IS_VIRTUAL.is = true;

        if (typeof(parser_val) == "string") {
            parser_val = definitions.__virtual[property_name] = CreatePropertyParser(parser_val, "", definitions, productions);
            parser_val.virtual = true;
            parser_val.name = property_name;
        }

        return parser_val;
    }

    return null;
}


function CreatePropertyParser(notation, name, definitions, productions) {

    const l = whind$1(notation);
    l.useExtendedId();
    
    const important = { is: false };

    let n = d$2(l, definitions, productions);

    n.seal();

    //if (n instanceof productions.JUX && n.terms.length == 1 && n.r[1] < 2)
    //    n = n.terms[0];

    n.HAS_PROP = true;
    n.IMP = important.is;

    /*//******** DEV 
    console.log("")
    console.log("")
    console.log(util.inspect(n, { showHidden: false, depth: null })) 
    //********** END Dev*/

    return n;
}

function d$2(l, definitions, productions, super_term = false, oneof_group = false, or_group = false, and_group = false, important = null) {
    let term, nt, v;
    const { JUX, AND, OR, ONE_OF, LiteralTerm, ValueTerm, SymbolTerm } = productions;

    let GROUP_BREAK = false;

    while (!l.END) {

        switch (l.ch) {
            case "]":
                return term;
                break;
            case "[":

                v = d$2(l.next(), definitions, productions, true);
                l.assert("]");
                v = checkExtensions(l, v, productions);

                if (term) {
                    if (term instanceof JUX && term.isRepeating()) term = foldIntoProduction(productions, new JUX, term);
                    term = foldIntoProduction(productions, term, v);
                } else
                    term = v;
                break;

            case "<":
                let id = getExtendedIdentifier(l.next());

                v = new ValueTerm(id, getPropertyParser, definitions, productions);

                l.next().assert(">");

                v = checkExtensions(l, v, productions);

                if (term) {
                    if (term instanceof JUX /*&& term.isRepeating()*/ ) term = foldIntoProduction(productions, new JUX, term);
                    term = foldIntoProduction(productions, term, v);
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
                        nt.terms.push(d$2(l, definitions, productions, super_term, oneof_group, or_group, true, important));
                        if (l.ch !== "&" || l.pk.ch !== "&") break;
                        l.a("&").a("&");
                    }

                    return nt;
                }
                break;
            case "|":

                {
                    if (l.pk.ch == "|") {

                        if (or_group || and_group)
                            return term;

                        nt = new OR();

                        nt.terms.push(term);

                        l.sync().next();

                        while (!l.END) {
                            nt.terms.push(d$2(l, definitions, productions, super_term, oneof_group, true, and_group, important));
                            if (l.ch !== "|" || l.pk.ch !== "|") break;
                            l.a("|").a("|");
                        }

                        return nt;

                    } else {

                        if (oneof_group || or_group || and_group)
                            return term;

                        nt = new ONE_OF();

                        nt.terms.push(term);

                        l.next();

                        while (!l.END) {
                            nt.terms.push(d$2(l, definitions, productions, super_term, true, or_group, and_group, important));
                            if (l.ch !== "|") break;
                            l.a("|");
                        }

                        return nt;
                    }
                }
                break;
            default:

                v = (l.ty == l.types.symbol) ? new SymbolTerm(l.tx) : new LiteralTerm(l.tx, l.ty);
                l.next();
                v = checkExtensions(l, v, productions);

                if (term) {
                    if (term instanceof JUX /*&& (term.isRepeating() || term instanceof ONE_OF)*/ ) term = foldIntoProduction(productions, new JUX, term);
                    term = foldIntoProduction(productions, term, v);
                } else {
                    term = v;
                }
        }
    }

    return term;
}

function checkExtensions(l, term, productions) {
    const { JUX, AND, OR, ONE_OF, LiteralTerm, ValueTerm, SymbolTerm } = productions;

    outer: while (true) {

        switch (l.ch) {
            case "!":
                /* https://www.w3.org/TR/CSS21/cascade.html#important-rules */
                term.IMPORTANT = true;
                l.next();
                continue outer;
            case "{":
                term = foldIntoProduction(productions, term);
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
                term = foldIntoProduction(productions, term);
                term.r[0] = 0;
                term.r[1] = Infinity;
                l.next();
                break;
            case "+":
                term = foldIntoProduction(productions, term);
                term.r[0] = 1;
                term.r[1] = Infinity;
                l.next();
                break;
            case "?":
                term = foldIntoProduction(productions, term);
                term.r[0] = 0;
                term.r[1] = 1;
                l.next();
                break;
            case "#":

                let nr = new productions.JUX();
                //nr.terms.push(new SymbolTerm(","));
                nr.terms.push(term);
                term = nr;
                //term = foldIntoProduction(productions, term);
                term.r[0] = 1;
                term.r[1] = Infinity;
                term.REQUIRE_COMMA = true;
                l.next();
                if (l.ch == "{") {
                    term.r[0] = parseInt(l.next().tx);
                    term.r[1] = parseInt(l.next().a(",").tx);
                    l.next().a("}");
                }
                break;
        }
        break;
    }
    return term;
}

function foldIntoProduction(productions, term, new_term = null) {
    if (term) {
        if (!(term instanceof productions.JUX)) {
            let nr = new productions.JUX();
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

const observer_mixin_symbol = Symbol("observer_mixin_symbol");

const observer_mixin = function(calling_name, prototype) {

    const observer_identifier = Symbol("observer_array_reference");

    prototype[observer_mixin_symbol] = observer_identifier;

    //Adds an observer to the object instance. Applies a property to the observer that references the object instance.
    //Creates new observers array if one does not already exist.
    prototype.addObserver = function(...observer_list) {
        let observers = this[observer_identifier];

        if (!observers)
            observers = this[observer_identifier] = [];

        for (const observer of observer_list) {

            if (observer[observer_identifier] == this)
                return

            if (observer[observer_identifier])
                observer[observer_identifier].removeObserver(observer);

            observers.push(observer);

            observer[observer_identifier] = this;
        }
    };

    //Removes an observer from the object instance. 
    prototype.removeObserver = function(...observer_list) {

        const observers = this[observer_identifier];

        for (const observer of observer_list)
            for (let i = 0, l = observers.length; i < l; i++)
                if (observers[i] == observer) return (observer[observer_identifier] = null, observers.splice(i, 1));

    };


    prototype.updateObservers = function() {
        const observers = this[observer_identifier];

        if (observers)
            observers.forEach(obj => obj[calling_name](this));
    };
};

//Properly destructs this observers object on the object instance.
observer_mixin.destroy = function(observer_mixin_instance) {

    const symbol = observer_mixin_instance.constructor.prototype[observer_mixin_symbol];

    if (symbol) {
        if (observer_mixin_instance[symbol])
            observer_mixin_instance[symbol].forEach(observer=>observer[symbol] = null);

        observer_mixin_instance[symbol].length = 0;
        
        observer_mixin_instance[symbol] = null;
    }
};

observer_mixin.mixin_symbol = observer_mixin_symbol;

Object.freeze(observer_mixin);

/* 
    Parses a string value of a css property. Returns result of parse or null.

    Arg - Array - An array with values:
        0 :  string name of css rule that should be used to parse the value string.
        1 :  string value of the css rule.
        2 :  BOOL value for the presence of the "important" value in the original string. 

    Returns object containing:
        rule_name : the string name of the rule used to parse the value.
        body_string : the original string value
        prop : An array of CSS type instances that are the parsed values.
        important : boolean value indicating the presence of "important" value.
*/




function parseDeclaration(sym) {
    if(sym.length == 0)
        return null;
    
    let prop = null;

    const
        rule_name = sym[0],
        body_string = sym[2],
        important = sym[3] ? true : false,
        IS_VIRTUAL = { is: false },
        parser = getPropertyParser(rule_name.replace(/\-/g, "_"), IS_VIRTUAL, property_definitions);

    if (parser && !IS_VIRTUAL.is) 

        prop = parser.parse(whind$1(body_string).useExtendedId());

    else
        //Need to know what properties have not been defined
        console.warn(`Unable to get parser for CSS property ${rule_name}`);

    return {rule_name, body_string, prop, important};
}

function setParent(array, parent) {
    for (const prop of array)
        prop.parent = parent;
}

/*
 * Holds a set of css style properties.
 */
class stylerule {

    constructor(selectors = [], props = []) {
        this.selectors = selectors;
        this.properties = new Map;

        this.addProp(props);

        //Versioning
        this.ver = 0;

        this.parent = null;

        setParent(this.selectors, this);
        setParent(this.properties.values(), this);

        this.props = new Proxy(this, this);
        this.addProperty = this.addProp;
        this.addProps = this.addProp;
        this.UPDATE_LOOP_GAURD = false;
    }
    
    get css_type(){
        return "stylerule"
    }

    destroy(){
        
        for(const prop of this.properties.values())
            prop.destroy();

        for(const selector of this.selectors)
            selector.destroy();

        this.parent = null;
        this.selectors = null;
        this.properties = null;

        observer_mixin.destroy(this);
    }

    /* sends an update signal up the hiearchy to allow style sheets to alert observers of new changes. */
    update() {
        this.ver++;

        //if(this.UPDATE_LOOP_GAURD) return;

        if (this.parent)
            this.parent.update();

        this.updateObservers();
    }

    get type() {
        return "stylerule"
    }

    get(obj, name) {
        let prop = obj.properties.get(name);
        //if (prop)
        //    prop.parent = this;
        return prop;
    }
    /*  
        Adds properties to the stylerule
        arg1 string - accepts a string of semicolon seperated css style rules.   
    */
    addProp(props) {
        if (typeof props == "string") {
            return this.addProps(
                props.split(";")
                .filter(e => e !== "")
                .map((e, a) => (a = e.split(":"), a.splice(1, 0, null), a))
                .map(parseDeclaration)
            )
        }

        if (props.type == "stylerule")
            props = props.properties.values();
        else
        if (!Array.isArray(props))
            props = [props];


       // this.UPDATE_LOOP_GAURD = true;
        for (const prop of props)
            if (prop) {
                if(this.properties.has(prop.name))
                    this.properties.get(prop.name).setValue(...prop.val);
                else
                    this.properties.set(prop.name, prop);
                
                prop.parent = this;
            }
        //this.UPDATE_LOOP_GAURD = false;

        this.ver++;

        this.update();

        return props;
    }

    match(element, window) {
        for (const selector of this.selectors)
            if (selector.match(element, window))
                return true;
        return false;
    }

    * getApplicableSelectors(element, window) {
        for (const selector of this.selectors)
            if (selector.match(element, window))
                yield selector;
    }

    * getApplicableRules(element, window) {
        if (this.match(element, window))
            yield this;
    }

    * iterateProps() {
        for (const prop of this.properties.values())
            yield prop;
    }

    toString(off = 0, rule = "") {

        let str = [],
            offset = ("    ").repeat(off);

        for (const prop of this.properties.values())
            str.push(prop.toString(off));

        return `${this.selectors.join("")}{${str.join(";")}}`;
    }

    merge(rule) {
        if(!rule) return;
        if (rule.type == "stylerule"){
            for (const prop of rule.properties.values()){
                if (prop) {
                    this.properties.set(prop.name, prop);
                }
            }
        }
                
    }

    get _wick_type_() { return 0; }

    set _wick_type_(v) {}
}

observer_mixin("updatedCSSStyleRule", stylerule.prototype);

class ruleset {
	constructor(asts, rules = []){
		this.rules = rules;

        rules.forEach(r=>r.parent = this);

        this.parent = null;
	}

    destroy(){
        for(const rule of this.rules)
            rule.destroy();
        this.rules = null;
        this.parent = null;
    }

    * getApplicableSelectors(element, win = window) {
        for(const rule of this.rules)
            yield * rule.getApplicableSelectors(element, win);
    }

	* getApplicableRules(element, win = window){
        for(const rule of this.rules)
            yield * rule.getApplicableRules(element, window);
    }

    /* sends an update signal up the hiearchy to allow style sheets to alert observers of new changes. */
    update(){
        if(this.parent)
            this.parent.updated();
    }

    getRule(string) {
        let r = null;
        for (let node = this.fch; node; node = this.getNextChild(node))
            r = node.getRule(string, r);
        return r;
    }

    toString(){
        return this.rules.join("\n");
    }
}

class stylesheet {

    constructor(sym) {
        this.ruleset = null;

        if (sym) {
            this.ruleset = sym[0];
        }else {
            this.ruleset = new ruleset();
        }
        this.ruleset.parent = this;

        this.parent = null;

        this.READY = true;
    }

    destroy(){
        
        this.ruleset.destroy();
        this.parent = null;
        this.READY = false;

        observer_mixin.destroy(this);
    }

    get css_type(){
        return "stylesheet"
    }

    /**
     * Creates a new instance of the object with same properties as the original.
     * @return     {CSSRootNode}  Copy of this object.
     * @public
     */
    clone() {
        let rn = new this.constructor();
        rn._selectors_ = this._selectors_;
        rn._sel_a_ = this._sel_a_;
        rn._media_ = this._media_;
        return rn;
    }

    merge(in_stylesheet) {
        if (in_stylesheet instanceof stylesheet) {

            let ruleset = in_stylesheet.ruleset;
            outer:
                for (let i = 0; i < children.length; i++) {
                    //determine if this child matches any existing selectors
                    let child = children[i];

                    for (let i = 0; i < this.children.length; i++) {
                        let own_child = this.children[i];

                        if (own_child.isSame(child)) {
                            own_child.merge(child);
                            continue outer;
                        }
                    }

                    this.children.push(child);
                }
        }
    }

    _resolveReady_(res, rej) {
        if (this.pending_build > 0) this.resolves.push(res);
        res(this);
    }

    _setREADY_() {
        if (this.pending_build < 1) {
            for (let i = 0, l = this.resolves; i < l; i++) this.resolves[i](this);
            this.resolves.length = 0;
            this.res = null;
        }
    }

    updated() {
        this.updateObservers();
    }

    * getApplicableSelectors(element, win = window) {
        yield * this.ruleset.getApplicableSelectors(element, window);
    }

    getApplicableRules(element, win = window, RETURN_ITERATOR = false, new_rule = new stylerule) {
        if(!(element instanceof HTMLElement))
            return new_rule;

        const iter = this.ruleset.getApplicableRules(element, win);
        if (RETURN_ITERATOR) {
            return iter
        } else
            for (const rule of iter) {
                new_rule.merge(rule);
            }
        return new_rule;
    }

    * getApplicableProperties(element, win = window){
        for(const rule of this.getApplicableRules(element, win, true))
            yield * rule.iterateProps();
    }

    getRule(string) {
        let r = null;
        for (let node = this.fch; node; node = this.getNextChild(node))
            r = node.getRule(string, r);
        return r;
    }

    toString() {
        return this.ruleset + "";
    }
}

observer_mixin("updatedCSS", stylesheet.prototype);

class styleprop {

    constructor(name, original_value, val) {
        this.val = val;
        this.name = name.replace(/\-/g, "_");
        this.original_value = original_value;
        this.rule = null;
        this.ver = 0;
    }
    destroy() {
        this.val = null;
        this.name = "";
        this.original_value = "";
        this.rule = null;
        observer_mixin.destroy(this);
    }

    get css_type() {
        return "styleprop"
    }

    updated() {
        this.updateObservers();

        if (this.parent)
            this.parent.update();
    }

    get value() {
        return this.val.length > 1 ? this.val : this.val[0];
    }

    get value_string() {
        return this.val.join(" ");
    }

    toString(offset = 0) {
        const
            str = [],
            off = ("    ").repeat(offset);

        return `${off+this.name.replace(/\_/g, "-")}:${this.value_string}`;
    }

    setValueFromString(value) {
        const result = parseDeclaration([this.name, null, value]);

        if (result) 
            this.setValue(...result.prop);
    }

    setValue(...values) {

        let i = 0;

        for (const value of values) {
            const own_val = this.val[i];


            if (own_val && value instanceof own_val.constructor)
                this.val[i] = value;
            else
                this.val[i] = value;
            i++;
        }

        this.val.length = values.length;

        this.ver++;

        this.updated();
    }
}

observer_mixin("updatedCSSStyleProperty", styleprop.prototype);

class compoundSelector {
    constructor(sym, env) {

        if(sym.length = 1)
            if(Array.isArray(sym[0]) && sym[0].length == 1)
                return sym[0][0]
            else
                return sym[0]

        this.subclass = null;
        this.tag = null;
        this.pseudo = null;


        if (sym[0].type == "type")
            this.tag = sym.shift();

        if (sym[0] && sym[0][0] && sym[0][0].type !== "pseudoElement")
            this.subclass = sym.shift();

        this.pseudo = sym[0];
    }

    get type() {
        return "compound"
    }

    matchReturnElement(element, win) {
        if (this.tag) {
            if (!this.tag.matchReturnElement(element, win))
                return null;
        }

        if (this.subclass) {
            for (const sel of this.subclass) {
                if (!sel.matchReturnElement(element, win))
                    return null;
            }
        }

        if (this.pseudo) {
            if (!this.subclass.matchReturnElement(element, win))
                return null;
        }

        return element;
    }

    toString() {
        const
            tag = this.tag ? this.tag + "" : "",
            subclass = this.subclass ? this.subclass.join("") + "" : "",
            pseudo = this.pseudo ? this.pseudo + "" : "";

        return `${tag + subclass + pseudo}`;
    }
}

class combination_selector_part {
    constructor(sym, env) {
        if (sym.length > 1) {
            this.op = sym[0];
            this.selector = sym[1];
        } else 
            return sym[0]
    }

    get type() {
        return "complex"
    }

    matchReturnElement(element, selector_array, selector = null, index = 0) {
        let ele;

        if ((ele = this.selector.matchReturnElement(element, selector_array))) {
            switch (this.op) {
                case ">":
                    return selector.match(ele.parentElement);
                case "+":
                    return selector.match(ele.previousElementSibling);
                case "~":
                    let children = ele.parentElement.children.slice(0, element.index);

                    for (const child of children) {
                        if (selector.match(child))
                            return child;
                    }
                    return null;
                default:
                    ele = ele.parentElement;
                    while (ele) {
                        if (selector.match(ele))
                            return ele;
                        ele = ele.parentElement;
                    }
            }
        }

        return null;
    }

    toString() {
        return this.op + this.selector + "";
    }
}

class selector {
    constructor(sym, env) {
        if (sym.length > 1)
            this.vals = [sym, ...sym[1]];
        else
            this.vals = sym;

        this.parent = null;
    }

    match(element, win = window) {

        for (const selector of this.vals.reverse()) {
            if (!(element = selector.matchReturnElement(element, win)))
                return false;
        }
        return true;
    }

    toString() {
        return this.vals.join(" ");
    }
}

class type_selector_part{
	constructor(sym){
		const val = sym[0];
		this.namespace = "";

		if(val.length > 1)
			this.namespace = val[0];
		this.val = ((val.length > 1) ? val[1] : val[0]).toLowerCase();
	}

	get type(){
		return "type"
	}

	matchReturnElement(element, win){
		return element.tagName.toLowerCase() == this.val ? element : null;
	}

	toString(){
		return  this.namespace + " " + this.val;
	}
}

class idSelector{
	constructor(sym,env){
		this.val = sym[1];
	}

	get type(){
		return "id"
	}

	matchReturnElement(element){
		return element.id == this.val ? element : null;
	}

	toString(){
		return "#"+ this.val;
	}
}

class classSelector{
	constructor(sym,env){
		this.val = sym[1];
	}

	get type(){
		return "class"
	}

	matchReturnElement(element, window){
		return element.classList.contains(this.val) ? element : null;
	}

	toString(){
		return "."+this.val;
	}
}

class attribSelector{
	constructor(sym,env){
		this.key = sym[1];
		this.val = "";
		this.op = "";
		this.mod = "";

		if(sym.length > 3){
			this.val = sym[3];
			this.op = sym[2];
			this.mod = sym.length > 5 ? sym[4] : "";
		}

	}

	get type(){
		return "attrib"
	}

	matchReturnElement(element, result){
		
		let attr = element.getAttribute(this.key);

		if(!attr)
			return null
		if(this.val && attr !== this.val)
			return null;
		
		return element;
	}

	toString(){
		return `[${this.key+this.op+this.val+this.mod}]`;
	}
}

class pseudoClassSelector{
	constructor(sym,env){
		this.val = sym[1];
	}

	get type(){
		return "pseudoClass"
	}

	matchReturnElement(element){
		return element;
	}

	toString(){

	}
}

class pseudoElementSelector{
	constructor(sym,env){
		this.val = sym[1].val;
	}

	get type(){
		return "pseudo-element"
	}

	matchReturnElement(element){
		return element;
	}

	toString(){

	}
}

/* 	Wraps parseDeclaration with a function that returns a styleprop object or null. 
	Uses same args as parseDeclaration */

function parseDeclaration$1 (...v){

	const result = parseDeclaration(...v);

	if(result)
		return new styleprop(
			result.rule_name,
			result.body_string,
			result.prop
		)

	return null;
}

const env = {
    functions: {
        compoundSelector,
        comboSelector: combination_selector_part,
        typeselector: type_selector_part,
        selector,
        idSelector,
        classSelector,
        attribSelector,
        pseudoClassSelector,
        pseudoElementSelector,
        parseDeclaration: parseDeclaration$1,
        stylerule,
        ruleset,
        stylesheet
    },
    body: null
};

const parse = function (string_data) { return parser(whind$1(string_data), env) };

exports.CSS_Length = CSS_Length;
exports.CSS_URL = CSS_URL;
exports.attribSelector = attribSelector;
exports.classSelector = classSelector;
exports.comboSelector = combination_selector_part;
exports.compoundSelector = compoundSelector;
exports.css_parser = parser;
exports.getPropertyParser = getPropertyParser;
exports.idSelector = idSelector;
exports.length = CSS_Length;
exports.media_feature_definitions = media_feature_definitions;
exports.parse = parse;
exports.parseDeclaration = parseDeclaration$1;
exports.productions = productions;
exports.property_definitions = property_definitions;
exports.pseudoClassSelector = pseudoClassSelector;
exports.pseudoElementSelector = pseudoElementSelector;
exports.ruleset = ruleset;
exports.selector = selector;
exports.stylerule = stylerule;
exports.stylesheet = stylesheet;
exports.terms = terms;
exports.types = types;
exports.typeselector = type_selector_part;
exports.url = CSS_URL;
