'use strict';

Object.defineProperty(exports, '__esModule', { value: true });

/**
 * To be extended by objects needing linked list methods.
 */
const LinkedList = {

    props: {
        /**
         * Properties for horizontal graph traversal
         * @property {object}
         */
        defaults: {
            /**
             * Next sibling node
             * @property {object | null}
             */
            nxt: null,

            /**
             * Previous sibling node
             * @property {object | null}
             */
            prv: null
        },

        /**
         * Properties for vertical graph traversal
         * @property {object}
         */
        children: {
            /**
             * Number of children nodes.
             * @property {number}
             */
            noc: 0,
            /**
             * First child node
             * @property {object | null}
             */
            fch: null,
        },
        parent: {
            /**
             * Parent node
             * @property {object | null}
             */
            par: null
        }
    },

    methods: {
        /**
         * Default methods for Horizontal traversal
         */
        defaults: {

            insertBefore: function(node) {

                if (!this.nxt || !this.prv) {
                    this.nxt = this;
                    this.prv = this;
                }

                if (node.prv || node.nxt) {
                    node.prv.nxt = node.nxt;
                    node.nxt.prv = node.prv;
                }

                node.prv = this.prv;
                this.prv.nxt = node;
                node.nxt = this;
                this.prv = node;
            },

            insertAfter: function(node) {

                if (!this.nxt || !this.prv) {
                    this.nxt = this;
                    this.prv = this;
                }

                if (node.prv || node.nxt) {
                    node.prv.nxt = node.nxt;
                    node.nxt.prv = node.prv;
                }

                this.nxt.prv = node;
                node.nxt = this.nxt;
                this.nxt = node;
                node.prv = this;
            }
        },
        /**
         * Methods for both horizontal and vertical traversal.
         */
        parent_child: {
            /**
             *  Returns eve. 
             * @return     {<type>}  { description_of_the_return_value }
             */
            root() {
                return this.eve();
            },
            /**
             * Returns the root node. 
             * @return     {Object}  return the very first node in the linked list graph.
             */
            eve() {
                if (this.par)
                    return this.par.eve();
                return this;
            },

            push(node) {
                this.addChild(node);
            },

            unshift(node) {
                this.addChild(node, (this.fch) ? this.fch.pre : null);
            },

            replace(old_node, new_node) {
                if (old_node.par == this && old_node !== new_node) {
                    if (new_node.par) new_node.par.remove(new_node);

                    if (this.fch == old_node) this.fch = new_node;
                    new_node.par = this;


                    if (old_node.nxt == old_node) {
                        new_node.nxt = new_node;
                        new_node.prv = new_node;
                    } else {
                        new_node.prv = old_node.prv;
                        new_node.nxt = old_node.nxt;
                        old_node.nxt.prv = new_node;
                        old_node.prv.nxt = new_node;
                    }

                    old_node.par = null;
                    old_node.prv = null;
                    old_node.nxt = null;
                }
            },

            insertBefore: function(node) {
                if (this.par)
                    this.par.addChild(node, this.pre);
                else
                    LinkedList.methods.defaults.insertBefore.call(this, node);
            },

            insertAfter: function(node) {
                if (this.par)
                    this.par.addChild(node, this);
                else
                    LinkedList.methods.defaults.insertAfter.call(this, node);
            },

            addChild: function(child = null, prev = null) {

                if (!child) return;

                if (child.par)
                    child.par.removeChild(child);

                if (prev && prev.par && prev.par == this) {
                    if (child == prev) return;
                    child.prv = prev;
                    prev.nxt.prv = child;
                    child.nxt = prev.nxt;
                    prev.nxt = child;
                } else if (this.fch) {
                    child.prv = this.fch.prv;
                    this.fch.prv.nxt = child;
                    child.nxt = this.fch;
                    this.fch.prv = child;
                } else {
                    this.fch = child;
                    child.nxt = child;
                    child.prv = child;
                }

                child.par = this;
                this.noc++;
            },

            /**
             * Analogue to HTMLElement.removeChild()
             *
             * @param      {HTMLNode}  child   The child
             */
            removeChild: function(child) {
                if (child.par && child.par == this) {
                    child.prv.nxt = child.nxt;
                    child.nxt.prv = child.prv;

                    if (child.prv == child || child.nxt == child) {
                        if (this.fch == child)
                            this.fch = null;
                    } else if (this.fch == child)
                        this.fch = child.nxt;

                    child.prv = null;
                    child.nxt = null;
                    child.par = null;
                    this.noc--;
                }
            },

            /**
             * Gets the next node. 
             *
             * @param      {HTMLNode}  node    The node to get the sibling of.
             * @return {HTMLNode | TextNode | undefined}
             */
            getNextChild: function(node = this.fch) {
                if (node && node.nxt != this.fch && this.fch)
                    return node.nxt;
                return null;
            },

            /**
             * Gets the child at index.
             *
             * @param      {number}  index   The index
             */
            getChildAtIndex: function(index, node = this.fch) {
                if(node.par !== this)
                    node = this.fch;

                let first = node;
                let i = 0;
                while (node && node != first) {
                    if (i++ == index)
                        return node;
                    node = node.nxt;
                }

                return null;
            },
        }
    },

    gettersAndSetters : {
        peer : {
            next: {
                enumerable: true,
                configurable: true,
                get: function() {
                    return this.nxt;
                },
                set: function(n) {
                    this.insertAfter(n);
                }
            },
            previous: {
                enumerable: true,
                configurable: true,
                get: function() {
                    return this.prv;
                },
                set: function(n) {
                    this.insertBefore(n);
                }   
            }
        },
        tree : {
            children: {
                enumerable: true,
                configurable: true,
                /**
                 * @return {array} Returns an array of all children.
                 */
                get: function() {
                    for (var z = [], i = 0, node = this.fch; i++ < this.noc;)(
                        z.push(node), node = node.nxt
                    );
                    return z;
                },
                set: function(e) {
                    /* No OP */
                }
            },
            parent: {
                enumerable: true,
                configurable: true,
                /**
                 * @return parent node
                 */
                get: function() {
                    return this.par;
                },
                set: function(p) {
                    if(p && p.addChild)
                        p.addChild(this);
                    else if(p === null && this.par)
                        this.par.removeChild(this);
                }
            }
        }
    },


    mixin : (constructor)=>{
        const proto = (typeof(constructor) == "function") ? constructor.prototype : (typeof(constructor) == "object") ? constructor : null;
        if(proto){
            Object.assign(proto, 
                LinkedList.props.defaults, 
                LinkedList.methods.defaults
            );
        }
        Object.defineProperties(proto, LinkedList.gettersAndSetters.peer);
    },

    mixinTree : (constructor)=>{
        const proto = (typeof(constructor) == "function") ? constructor.prototype : (typeof(constructor) == "object") ? constructor : null;
        if(proto){
            Object.assign(proto, 
                LinkedList.props.defaults, 
                LinkedList.props.children, 
                LinkedList.props.parent, 
                LinkedList.methods.defaults, 
                LinkedList.methods.parent_child
                );
            Object.defineProperties(proto, LinkedList.gettersAndSetters.tree);
            Object.defineProperties(proto, LinkedList.gettersAndSetters.peer);
        }
    }
};

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
const r = 114;
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
7, 	 	/* A */
7, 	 	/* a */
7, 	 	/* ACKNOWLEDGE */
7, 	 	/* AMPERSAND */
7, 	 	/* ASTERISK */
7, 	 	/* AT */
7, 	 	/* B */
7, 	 	/* b */
7, 	 	/* BACKSLASH */
4, 	 	/* BACKSPACE */
6, 	 	/* BELL */
7, 	 	/* C */
7, 	 	/* c */
5, 	 	/* CANCEL */
7, 	 	/* CARET */
11, 	/* CARRIAGE_RETURN */
7, 	 	/* CLOSE_CURLY */
7, 	 	/* CLOSE_PARENTH */
7, 	 	/* CLOSE_SQUARE */
7, 	 	/* COLON */
7, 	 	/* COMMA */
7, 	 	/* d */
7, 	 	/* D */
7, 	 	/* DATA_LINK_ESCAPE */
7, 	 	/* DELETE */
7, 	 	/* DEVICE_CTRL_1 */
7, 	 	/* DEVICE_CTRL_2 */
7, 	 	/* DEVICE_CTRL_3 */
7, 	 	/* DEVICE_CTRL_4 */
7, 	 	/* DOLLAR */
7, 	 	/* DOUBLE_QUOTE */
7, 	 	/* e */
3, 	 	/* E */
8, 	 	/* EIGHT */
2, 	 	/* END_OF_MEDIUM */
7, 	 	/* END_OF_TRANSMISSION */
7, 	 	/* END_OF_TRANSMISSION_BLOCK */
8, 	 	/* END_OF_TXT */
8, 	 	/* ENQUIRY */
2, 	 	/* EQUAL */
9, 	 	/* ESCAPE */
10, 	 /* EXCLAMATION */
8, 	 	/* f */
8, 	 	/* F */
7, 	 	/* FILE_SEPERATOR */
7, 	 	/* FIVE */
7, 	 	/* FORM_FEED */
7, 	 	/* FORWARD_SLASH */
0, 	 	/* FOUR */
0, 	 	/* g */
0, 	 	/* G */
0, 	 	/* GRAVE */
0, 	 	/* GREATER_THAN */
0, 	 	/* GROUP_SEPERATOR */
0, 	 	/* h */
0, 	 	/* H */
0, 	 	/* HASH */
0, 	 	/* HORIZONTAL_TAB */
8, 	 	/* HYPHEN */
7, 	 	/* i */
8, 	 	/* I */
8, 	 	/* j */
8, 	 	/* J */
7, 	 	/* k */
7, 	 	/* K */
1, 	 	/* l */
1, 	 	/* L */
1, 	 	/* LESS_THAN */
1, 	 	/* LINE_FEED */
1, 	 	/* m */
1, 	 	/* M */
1, 	 	/* n */
1, 	 	/* N */
1, 	 	/* NEGATIVE_ACKNOWLEDGE */
1, 	 	/* NINE */
1, 	 	/* NULL */
1, 	 	/* o */
1, 	 	/* O */
1, 	 	/* ONE */
1, 	 	/* OPEN_CURLY */
1, 	 	/* OPEN_PARENTH */
1, 	 	/* OPEN_SQUARE */
1, 	 	/* p */
1, 	 	/* P */
1, 	 	/* PERCENT */
1, 	 	/* PERIOD */
1, 	 	/* PLUS */
1, 	 	/* q */
1, 	 	/* Q */
1, 	 	/* QMARK */
1, 	 	/* QUOTE */
9, 	 	/* r */
7, 	 	/* R */
10, 	/* RECORD_SEPERATOR */
7, 	 	/* s */
7, 	 	/* S */
2, 	 	/* SEMICOLON */
1, 	 	/* SEVEN */
1, 	 	/* SHIFT_IN */
1, 	 	/* SHIFT_OUT */
1, 	 	/* SIX */
1, 	 	/* SPACE */
1, 	 	/* START_OF_HEADER */
1, 	 	/* START_OF_TEXT */
1, 	 	/* SUBSTITUTE */
1, 	 	/* SYNCH_IDLE */
1, 	 	/* t */
1, 	 	/* T */
1, 	 	/* THREE */
1, 	 	/* TILDE */
1, 	 	/* TWO */
1, 	 	/* u */
1, 	 	/* U */
1, 	 	/* UNDER_SCORE */
1, 	 	/* UNIT_SEPERATOR */
1, 	 	/* v */
1, 	 	/* V */
1, 	 	/* VERTICAL_BAR */
1, 	 	/* VERTICAL_TAB */
1, 	 	/* w */
1, 	 	/* W */
1, 	 	/* x */
1, 	 	/* X */
9, 	 	/* y */
7, 	 	/* Y */
10,  	/* z */
7,  	/* Z */
7 		/* ZERO */
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
0, 		/* A */
0, 		/* a */
0, 		/* ACKNOWLEDGE */
0, 		/* AMPERSAND */
0, 		/* ASTERISK */
0, 		/* AT */
0,		/* B */
0,		/* b */
0,		/* BACKSLASH */
0,		/* BACKSPACE */
0,		/* BELL */
0,		/* C */
0,		/* c */
0,		/* CANCEL */
0,		/* CARET */
0,		/* CARRIAGE_RETURN */
0,		/* CLOSE_CURLY */
0,		/* CLOSE_PARENTH */
0,		/* CLOSE_SQUARE */
0,		/* COLON */
0,		/* COMMA */
0,		/* d */
0,		/* D */
0,		/* DATA_LINK_ESCAPE */
0,		/* DELETE */
0,		/* DEVICE_CTRL_1 */
0,		/* DEVICE_CTRL_2 */
0,		/* DEVICE_CTRL_3 */
0,		/* DEVICE_CTRL_4 */
0,		/* DOLLAR */
0,		/* DOUBLE_QUOTE */
0,		/* e */
0,		/* E */
0,		/* EIGHT */
0,		/* END_OF_MEDIUM */
0,		/* END_OF_TRANSMISSION */
8,		/* END_OF_TRANSMISSION_BLOCK */
0,		/* END_OF_TXT */
0,		/* ENQUIRY */
0,		/* EQUAL */
0,		/* ESCAPE */
0,		/* EXCLAMATION */
0,		/* f */
0,		/* F */
0,		/* FILE_SEPERATOR */
2,		/* FIVE */
4,		/* FORM_FEED */
0,		/* FORWARD_SLASH */
8,		/* FOUR */
8,		/* g */
8,		/* G */
8,		/* GRAVE */
8,		/* GREATER_THAN */
8,		/* GROUP_SEPERATOR */
8,		/* h */
8,		/* H */
8,		/* HASH */
8,		/* HORIZONTAL_TAB */
0,		/* HYPHEN */
0,		/* i */
0,		/* I */
0,		/* j */
0,		/* J */
0,		/* k */
0,		/* K */
2,		/* l */
8,		/* L */
2,		/* LESS_THAN */
2,		/* LINE_FEED */
8,		/* m */
2,		/* M */
2,		/* n */
2,		/* N */
2,		/* NEGATIVE_ACKNOWLEDGE */
2,		/* NINE */
2,		/* NULL */
2,		/* o */
2,		/* O */
2,		/* ONE */
8,		/* OPEN_CURLY */
2,		/* OPEN_PARENTH */
2,		/* OPEN_SQUARE */
2,		/* p */
2,		/* P */
2,		/* PERCENT */
2,		/* PERIOD */
2,		/* PLUS */
2,		/* q */
8,		/* Q */
2,		/* QMARK */
2,		/* QUOTE */
0,		/* r */
0,		/* R */
0,		/* RECORD_SEPERATOR */
0,		/* s */
2,		/* S */
0,		/* SEMICOLON */
2,		/* SEVEN */
8,		/* SHIFT_IN */
2,		/* SHIFT_OUT */
2,		/* SIX */
2,		/* SPACE */
2,		/* START_OF_HEADER */
2,		/* START_OF_TEXT */
2,		/* SUBSTITUTE */
2,		/* SYNCH_IDLE */
2,		/* t */
2,		/* T */
2,		/* THREE */
2,		/* TILDE */
2,		/* TWO */
8,		/* u */
2,		/* U */
2,		/* UNDER_SCORE */
2,		/* UNIT_SEPERATOR */
2,		/* v */
2,		/* V */
2,		/* VERTICAL_BAR */
2,		/* VERTICAL_TAB */
2,		/* w */
8,		/* W */
2,		/* x */
2,		/* X */
0,		/* y */
0,		/* Y */
0,		/* z */
0,		/* Z */
0		/* ZERO */
];

const number = 1,
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

function getNumbrOfTrailingZeroBitsFromPowerOf2(value){
    return debruijnLUT[(value * 0x077CB531) >>> 27];
}

class Lexer {

    constructor(string = "", INCLUDE_WHITE_SPACE_TOKENS = false, PEEKING = false) {

        if (typeof(string) !== "string") throw new Error("String value must be passed to Lexer");

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

        /**
         * Flag to force the lexer to parse string contents
         */
         this.PARSE_STRING = false;

        if (!PEEKING) this.next();
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
    copy( destination = new Lexer(this.str, false, true) ) {
        destination.off = this.off;
        destination.char = this.char;
        destination.line = this.line;
        destination.sl = this.sl;
        destination.masked_values = this.masked_values;
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
     * Will throw a new Error, appending the parsed string line and position information to the the error message passed into the function.
     * @instance
     * @public
     * @param {String} message - The error message.
     */
    throw (message) {
        let t$$1 = ("________________________________________________"),
            n$$1 = "\n",
            is_iws = (!this.IWS) ? "\n The Lexer produced whitespace tokens" : "";
        this.IWS = false;
        let pk = this.copy();
        while (!pk.END && pk.ty !== Types.nl) { pk.next(); }
        let end = pk.off;
        throw new Error(`${message} at ${this.line}:${this.char}\n${t$$1}\n${this.str.slice(this.off + this.tl + 1 - this.char, end)}\n${("").padStart(this.char - 2)}^\n${t$$1}\n${is_iws}`);
    }

    /**
     * Proxy for Lexer.prototype.reset
     * @public
     */
    r() { return this.reset(); }

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
    next(marker = this) {

        let str = marker.str;

        if (marker.sl < 1) {
            marker.off = 0;
            marker.type = 32768;
            marker.tl = 0;
            return marker;
        }

        //Token builder
        let length = marker.tl;
        let off = marker.off + length;
        let l$$1 = marker.sl;
        let IWS = marker.IWS;
        let type = symbol;
        let char = marker.char + length;
        let line = marker.line;
        let base = off;

        if (off >= l$$1) {
            length = 0;
            base = l$$1;
            char -= base - off;
            marker.type = type;
            marker.off = base;
            marker.tl = length;
            marker.char = char;
            marker.line = line;
            return marker;
        }

        while (true) {

            base = off;

            length = 1;

            let code = str.charCodeAt(off);

            if (code < 128) {

                switch (jump_table[code]) {
                    case 0: //NUMBER
                        while (++off < l$$1 && (12 & number_and_identifier_table[str.charCodeAt(off)])) {}

                        if (str[off] == "e" || str[off] == "E") {
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
                        while (++off < l$$1 && ((10 & number_and_identifier_table[str.charCodeAt(off)]))) {}
                        type = identifier;
                        length = off - base;
                        break;
                    case 2: //QUOTED STRING
                        if (this.PARSE_STRING) {
                            type = symbol;
                        } else {
                            while (++off < l$$1 && str.charCodeAt(off) !== code) {}
                            type = string;
                            length = off - base + 1;
                        }
                        break;
                    case 3: //SPACE SET
                        while (++off < l$$1 && str.charCodeAt(off) === SPACE) {}
                        type = white_space;
                        length = off - base;
                        break;
                    case 4: //TAB SET
                        while (++off < l$$1 && str[off] === HORIZONTAL_TAB) {}
                        type = white_space;
                        length = off - base;
                        break;
                    case 5: //CARIAGE RETURN
                        length = 2;
                    case 6: //LINEFEED
                        type = new_line;
                        char = 0;
                        line++;
                        off += length;
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
            }

            if (IWS && (type & white_space_new_line)) {
                if (off < l$$1) {
                    char += length;
                    type = symbol;
                    continue;
                } else {
                    //Trim white space from end of string
                    base = l$$1 - length;
                    marker.sl -= length;
                    length = 0;
                    char -= base - off;
                }
            }

            break;
        }

        marker.type = type;
        marker.off = base;
        marker.tl = (this.masked_values & CHARACTERS_ONLY_MASK) ? Math.min(1,length) : length;
        marker.char = char;
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
    aC(char) { return this.assertCharacter(char); }
    /**
     * Compares the character value of the current token to the value passed in. Advances to next token if the two are equal.
     * @public
     * @throws {Error} - `Expecting "${text}" got "${this.text}"`
     * @param {String} text - The string to compare.
     */
    assertCharacter(char) {

        if (this.off < 0) this.throw(`Expecting ${text} got null`);

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
    s(start) { return this.slice(start); }

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
                let IWS = marker.IWS;
                while (marker.next().ty != types.new_line && !marker.END) { /* NO OP */ }
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

    toString(){
        return this.slice();
    }

    /*** Getters and Setters ***/
    get string() {
        return this.str;
    }

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
    get tx() { return this.text; }
    
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
    get ty() { return this.type; }

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
    get pk() { return this.peek(); }

    /**
     * Proxy for Lexer.prototype.next
     * @public
     */
    get n() { return this.next(); }

    get END(){ return this.off >= this.sl; }
    set END(v$$1){}

    get type(){
        return 1 << (this.masked_values & TYPE_MASK);
    }

    set type(value){
        //assuming power of 2 value.

        this.masked_values = (this.masked_values & ~TYPE_MASK) | ((getNumbrOfTrailingZeroBitsFromPowerOf2(value)) & TYPE_MASK); 
    }

    get tl (){
        return this.token_length;
    }

    set tl(value){
        this.token_length = value;
    }

    get token_length(){
        return ((this.masked_values & TOKEN_LENGTH_MASK) >> 7);
    }

    set token_length(value){
        this.masked_values = (this.masked_values & ~TOKEN_LENGTH_MASK) | (((value << 7) | 0) & TOKEN_LENGTH_MASK); 
    }

    get IGNORE_WHITE_SPACE(){
        return this.IWS;
    }

    set IGNORE_WHITE_SPACE(bool){
        this.iws = !!bool;
    }

    get CHARACTERS_ONLY(){
        return !!(this.masked_values & CHARACTERS_ONLY_MASK);
    }

    set CHARACTERS_ONLY(boolean){
        this.masked_values = (this.masked_values & ~CHARACTERS_ONLY_MASK) | ((boolean | 0) << 6); 
    }

    get IWS(){
        return !!(this.masked_values & IGNORE_WHITESPACE_MASK);
    }

    set IWS(boolean){
        this.masked_values = (this.masked_values & ~IGNORE_WHITESPACE_MASK) | ((boolean | 0) << 5); 
    }

    get PARSE_STRING(){
        return !!(this.masked_values & PARSE_STRING_MASK);
    }

    set PARSE_STRING(boolean){
        this.masked_values = (this.masked_values & ~PARSE_STRING_MASK) | ((boolean | 0) << 4); 
    }

    /**
     * Reference to token id types.
     */
    get types() {
        return Types;
    }
}

function whind$1(string, INCLUDE_WHITE_SPACE_TOKENS = false) { return new Lexer(string, INCLUDE_WHITE_SPACE_TOKENS); }

whind$1.constructor = Lexer;

Lexer.types = Types;
whind$1.types = Types;

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

    constructor(r, g, b, a) {
        super(r, g, b, a);

        if (typeof(r) == "string")
            this.set(CSS_Color._fs_(r) || {r:255,g:255,b:255,a:0});

    }

    static parse(l, rule, r) {

        let c = CSS_Color._fs_(l);

        if (c) {
            l.next();

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
                var value = l.next().tx;
                let num = parseInt(value,16);
                
                out = { r: 0, g: 0, b: 0, a: 1 };
                if(value.length == 3){
                    out.r = (num >> 8) & 0xF;
                    out.g = (num >> 4) & 0xF;
                    out.b = (num) & 0xF;
                }else{
                    if(value.length == 6){
                        out.r = (num >> 16) & 0xFF;
                        out.g = (num >> 8) & 0xFF;
                        out.b = (num) & 0xFF;
                    }if(value.length == 8){
                        out.r = (num >> 24) & 0xFF;
                        out.g = (num >> 16) & 0xFF;
                        out.b = (num >> 8) & 0xFF;
                        out.a = ((num) & 0xFF);
                    }
                }
                l.next();
                break;
            case "r":
                let tx = l.tx;
                if (tx == "rgba") {
                    out = { r: 0, g: 0, b: 0, a: 1 };
                    l.next(); // (
                    out.r = parseInt(l.next().tx);
                    l.next(); // ,
                    out.g = parseInt(l.next().tx);
                    l.next(); // ,
                    out.b = parseInt(l.next().tx);
                    l.next(); // ,
                    out.a = parseFloat(l.next().tx);
                    l.next();
                    c = new CSS_Color();
                    c.set(out);
                    return c;
                } else if (tx == "rgb") {
                    out = { r: 0, g: 0, b: 0, a: 1 };
                    l.next(); // (
                    out.r = parseInt(l.next().tx);
                    l.next(); // ,
                    out.g = parseInt(l.next().tx);
                    l.next(); // ,
                    out.b = parseInt(l.next().tx);
                    l.next();
                    c = new CSS_Color();
                    c.set(out);
                    return c;
                } // intentional
            default:
                let string = l.tx;

                if (l.ty == l.types.str)
                    string = string.slice(1, -1);

                out = CSS_Color.colors[string.toLowerCase()];
        }

        return out;
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

    constructor(v) {

        if (typeof(v) == "string") {
            let lex = whind(v);
            let val = CSS_Percentage.parse(lex);
            if (val) 
                return val;
        }
        
        super(v);
    }

    static _verify_(l) {
        if(typeof(l) == "string" &&  !isNaN(parseInt(l)) && l.includes("%"))
            return true;
        return false;
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

class CSS_Length extends Number {
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
            if (l.p.ty == l.types.id) {
                let id = l.sync().tx;
                l.next();
                return new CSS_Length(parseFloat(tx) * mult, id);
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

const uri_reg_ex = /(?:([^\:\?\[\]\@\/\#\b\s][^\:\?\[\]\@\/\#\b\s]*)(?:\:\/\/))?(?:([^\:\?\[\]\@\/\#\b\s][^\:\?\[\]\@\/\#\b\s]*)(?:\:([^\:\?\[\]\@\/\#\b\s]*)?)?\@)?(?:(\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3})|((?:\[[0-9a-f]{1,4})+(?:\:[0-9a-f]{0,4}){2,7}\])|([^\:\?\[\]\@\/\#\b\s\.]{2,}(?:\.[^\:\?\[\]\@\/\#\b\s]*)*))?(?:\:(\d+))?((?:[^\?\[\]\#\s\b]*)+)?(?:\?([^\[\]\#\s\b]*))?(?:\#([^\#\s\b]*))?/i;

const STOCK_LOCATION = {
    protocol :"",
    host :"",
    port :"",
    path :"",
    hash :"",
    query :"",
    search:""
};

function fetchLocalText(URL, m = "same-origin") {
    return new Promise((res, rej) => {
        fetch(URL, {
            mode: m, // CORs not allowed
            credentials: m,
            method: "Get"
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
            method: "Get"
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

    static resolveRelative(URL_or_url_original, URL_or_url_new) {
        
        let URL_old = (URL_or_url_original instanceof URL) ? URL_or_url_original : new URL(URL_or_url_original);
        let URL_new = (URL_or_url_new instanceof URL) ? URL_or_url_new : new URL(URL_or_url_new);

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

        let IS_STRING = true;
        

        const location = (typeof(document) !== "undefined") ? document.location : STOCK_LOCATION;

        if (!url || typeof(url) != "string") {
            IS_STRING = false;
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
                this.protocol = part[1] || ((USE_LOCATION) ? location.protocol : "");
                this.user = part[2] || "";
                this.pwd = part[3] || "";
                this.host = part[4] || part[5] || part[6] || ((USE_LOCATION) ? location.hostname : "");
                this.port = parseInt(part[7] || ((USE_LOCATION) ? location.port : 0));
                this.path = part[8] || ((USE_LOCATION) ? location.pathname : "");
                this.query = part[9] || ((USE_LOCATION) ? location.search.slice(1) : "");
                this.hash = part[10] || ((USE_LOCATION) ? location.hash.slice(1) : "");
            }
        } else if (USE_LOCATION) {

            URL.G = this;
            this.protocol = location.protocol;
            this.host = location.hostname;
            this.port = location.port;
            this.path = location.pathname;
            this.hash = location.hash.slice(1);
            this.query = location.search.slice(1);
            this._getQuery_(this.query);

            return URL.R;
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

        let lex = whind$1(this.query);


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

        if (this.protocol && this.host)
            str.push(`${this.protocol}://`);

        if (this.host)
            str.push(`${this.host}`);

        if (this.port)
            str.push(`:${this.port}`);

        if (this.path)
            str.push(`${this.path[0] == "/" ? "" : "/"}${this.path}`);

        if (this.query)
            str.push(this.query);

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

    submitJSON(json_data) {
            return submitJSON(this.toString(), json_data);
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

    get pathname() {
        return this.path;
    }

    get href() {
        return this.toString();
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
Object.freeze(URL.R);
Object.freeze(URL.RC);
Object.seal(URL);

class CSS_URL extends URL {
    static parse(l, rule, r) {
        if (l.tx == "url" || l.tx == "uri") {
            l.next().a("(");
            let v = "";
            if (l.ty == l.types.str) {
                v = l.tx.slice(1,-1);
                l.next().a(")");
            } else {
                let p = l.p;
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
    static parse(l, rule, r) {
        if (l.ty == l.types.str) {
            let tx = l.tx;
            l.next();
            return new CSS_String(tx);
        }
        return null;
    }
}

class CSS_Id extends String {
    static parse(l, rule, r) {
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
    static parse(l, rule, r) {
        if (l.tx == "inset" || l.tx == "circle" || l.tx == "ellipse" || l.tx == "polygon") {
            l.next().a("(");
            let v = "";
            if (l.ty == l.types.str) {
                v = l.tx.slice(1,-1);
                l.next().a(")");
            } else {
                let p = l.p;
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
    static parse(l, rule, r) {
        let tx = l.tx;
        if(l.ty == l.types.num){
            l.next();
            return new CSS_Number(tx);
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
	static parse(l, rule, r) {

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

    static parse(l, rule, r) {
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
                        if(!(len = CSS_Length.parse(l, rule, r)))
                            len = CSS_Percentage.parse(l,rule,r);
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
        parse: function(a, b, c) {
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

    lex.n;

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
        lex.n;
    }
    return n;
}

function ParseString(string, transform) {
    var lex = whind$1(string);
    
    while (!lex.END) {
        let tx = lex.tx;
        lex.n;
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
        lex.n;
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
        if (px) {
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
    lex.n;
    return parseFloat(tx) * mult;
}

function getNumberPair(lex, array) {
    let x = getSignedNumber(lex);
    if (lex.ch == ',') lex.n;
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
                    lex.n; //
                    array.push((relative) ? PathSym.m : PathSym.M);
                    getNumberPair(lex, array);
                    parseNumberPairs(lex, array);
                    continue;
                    //Line to
                case "h":
                    relative = true;
                case "H":
                    lex.n;
                    x = getSignedNumber(lex);
                    array.push((relative) ? PathSym.h : PathSym.H, x);
                    continue;
                case "v":
                    relative = true;
                case "V":
                    lex.n;
                    y = getSignedNumber(lex);
                    array.push((relative) ? PathSym.v : PathSym.V, y);
                    continue;
                case "l":
                    relative = true;
                case "L":
                    lex.n;
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
            lex.n;
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

/**
 * CSS Type constructors
 * @alias module:wick~internals.css.types.
 * @enum {object}
 */
const types$1 = {
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
 * @alias module:wick~internals.css.property_definitions.
 * @enum {string}
 */
const property_definitions = {
    //https://www.w3.org/TR/2018/REC-css-color-3-20180619//
    
    color: `<color>`,

    opacity: `<alphavalue>|inherit`,


    /*https://www.w3.org/TR/css-backgrounds-3/*/
    /* Background */
    background_color: `<color>`,
    background_image: `<bg_image>#`,
    background_repeat: `<repeat_style>#`,
    background_attachment: `scroll|fixed|local`,
    background_position: `[<percentage>|<length>]{1,2}|[top|center|bottom]||[left|center|right]`,
    background_clip: `<box>#`,
    background_origin: `<box>#`,
    background_size: `<bg_size>#`,
    background: `<bg_layer>#,<final_bg_layer>`,

    /* Font https://www.w3.org/TR/css-fonts-4*/
    font_family: `[[<family_name>|<generic_family>],]*[<family_name>|<generic_family>]`,
    family_name: `<id>||<string>`,
    generic_name: `serif|sans_serif|cursive|fantasy|monospace`,
    font: `[<font_style>||<font_variant>||<font_weight>]?<font_size>[/<line_height>]?<font_family>`,
    font_variant: `normal|small_caps`,
    font_style: `normal | italic | oblique <angle>?`,
    font_kerning: ` auto | normal | none`,
    font_variant_ligatures:`normal|none|[<common-lig-values>||<discretionary-lig-values>||<historical-lig-values>||<contextual-alt-values> ]`,
    font_variant_position:`normal|sub|super`,
    font_variant_caps:`normal|small-caps|all-small-caps|petite-caps|all-petite-caps|unicase|titling-caps`,


    /*CSS Clipping https://www.w3.org/TR/css-masking-1/#clipping `normal|italic|oblique`, */
    font_size: `<absolute_size>|<relative_size>|<length>|<percentage>`,
    absolute_size: `xx_small|x_small|small|medium|large|x_large|xx_large`,
    relative_size: `larger|smaller`,
    font_wight: `normal|bold|bolder|lighter|100|200|300|400|500|600|700|800|900`,

    /* Text */
    word_spacing: `normal|<length>`,
    letter_spacing: `normal|<length>`,
    text_decoration: `none|[underline||overline||line-through||blink]`,
    text_transform: `capitalize|uppercase|lowercase|none`,
    text_align: `left|right|center|justify`,
    text_indent: `<length>|<percentage>`,


    /* Border  https://www.w3.org/TR/css-backgrounds-3 */
    border_color: `<color>{1,4}`,
    border_top_color: `<color>`,
    border_right_color: `<color>`,
    border_bottom_color: `<color>`,
    border_left_color: `<color>`,

    border_width: `<line_width>{1,4}`,
    border_top_width: `<line_width>`,
    border_right_width: `<line_width>`,
    border_bottom_width: `<line_width>`,
    border_left_width: `<line_width>`,

    border_style: `<line_style>{1,4}`,
    border_top_style: `<line_style>`,
    border_right_style: `<line_style>`,
    border_bottom_style: `<line_style>`,
    border_left_style: `<line_style>`,

    border_top: `<line_width>||<line_style>||<color>`,
    border_right: `<line_width>||<line_style>||<color>`,
    border_bottom: `<line_width>||<line_style>||<color>`,
    border_left: `<line_width>||<line_style>||<color>`,

    border_radius: `<length_percentage>{1,4}[/<length_percentage>{1,4}]?`,
    border_top_left_radius: `<length_percentage>{1,2}`,
    border_top_right_radius: `<length_percentage>{1,2}`,
    border_bottom_right_radius: `<length_percentage>{1,2}`,
    border_bottom_left_radius: `<length_percentage>{1,2}`,

    border_image: `<border_image_source>||<border_image_slice>[/<border_image_width>|/<border_image_width>?/<border_image_outset>]?||<border_image_repeat>`,
    border_image_source: `none|<image>`,
    border_image_slice: `[<number>|<percentage>]{1,4}&&fill?`,
    border_image_width: `[<length_percentage>|<number>|auto]{1,4}`,
    border_image_outset: `[<length>|<number>]{1,4}`,
    border_image_repeat: `[stretch|repeat|round|space]{1,2}`,

    box_shadow: `none|<shadow>#`,

    border: `<line_width>||<line_style>||<color>`,

    width: `<length>|<percentage>|auto|inherit`,
    height: `<length>|<percentage>|auto|inherit`,
    float: `left|right|none`,
    clear: `left|right|both`,

    /* Classification */

    display: `[ <display_outside> || <display_inside> ] | <display_listitem> | <display_internal> | <display_box> | <display_legacy>`,
    white_space: `normal|pre|nowrap`,
    list_style_type: `disc|circle|square|decimal|decimal-leading-zero|lower-roman|upper-roman|lower-greek|lower-latin|upper-latin|armenian|georgian|lower-alpha|upper-alpha|none|inherit`,
    list_style_image: `<url>|none`,
    list_style_position: `inside|outside`,
    list_style: `[disc|circle|square|decimal|lower-roman|upper-roman|lower-alpha|upper-alpha|none]||[inside|outside]||[<url>|none]`,
    vertical_align: `baseline|sub|super|top|text-top|middle|bottom|text-bottom|<percentage>|<length>|inherit`,

    /* Layout https://www.w3.org/TR/css-position-3 */ 
    position: "static|relative|absolute|sticky|fixed",
    top: `<length>|<percentage>|auto|inherit`,
    left: `<length>|<percentage>|auto|inherit`,
    bottom: `<length>|<percentage>|auto|inherit`,
    right: `<length>|<percentage>|auto|inherit`,

    
    /* Box Model https://www.w3.org/TR/css-box-3 */
    margin: `[<length>|<percentage>|0|auto]{1,4}`,
    margin_top: `<length>|<percentage>|0|auto`,
    margin_right: `<length>|<percentage>|0|auto`,
    margin_bottom: `<length>|<percentage>|0|auto`,
    margin_left: `<length>|<percentage>|0|auto`,

    padding: `[<length>|<percentage>|0|auto]{1,4}`,
    padding_top: `<length>|<percentage>|0|auto`,
    padding_right: `<length>|<percentage>|0|auto`,
    padding_bottom: `<length>|<percentage>|0|auto`,
    padding_left: `<length>|<percentage>|0|auto`,

    min_width: `<length>|<percentage>|inherit`,
    max_width: `<length>|<percentage>|none|inherit`,
    min_height: `<length>|<percentage>|inherit`,
    max_height: `<length>|<percentage>|none|inherit`,
    line_height: `normal|<number>|<length>|<percentage>|inherit`,
    overflow: 'visible|hidden|scroll|auto|inherit',


    box_sizing: `content-box | border-box`,

    /* Visual Effects */
    clip: '<shape>|auto|inherit',
    visibility: `visible|hidden|collapse|inherit`,
    content: `normal|none|[<string>|<uri>|<counter>|attr(<identifier>)|open-quote|close-quote|no-open-quote|no-close-quote]+|inherit`,
    quotas: `[<string><string>]+|none|inherit`,
    counter_reset: `[<identifier><integer>?]+|none|inherit`,
    counter_increment: `[<identifier><integer>?]+|none|inherit`,

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

    /* https://drafts.csswg.org/css-transitions-1/ */

    transition: `<single_transition>#`,
    transition_property: `none|<single_transition_property>#`,
    transition_duration: `<time>#`,
    transition_timing_function: `<timing_function>#`,
    transition_delay: `<time>#`,
};

/* Properties that are not directly accessible by CSS prop creator */

const virtual_property_definitions = {

    alphavalue: '<number>',

    box: `border-box|padding-box|content-box`,

    /*https://www.w3.org/TR/css-backgrounds-3/*/

    bg_layer: `<bg_image>||<bg_position>[/<bg_size>]?||<repeat_style>||<attachment>||<box>||<box>`,
    final_bg_layer: `<background_color>||<bg_image>||<bg_position>[/<bg_size>]?||<repeat_style>||<attachment>||<box>||<box>`,
    bg_image: `<url>|<gradient>|none`,
    repeat_style: `repeat-x|repeat-y|[repeat|space|round|no-repeat]{1,2}`,
    background_attachment: `<attachment>#`,
    bg_size: `<length_percentage>|auto]{1,2}|cover|contain`,
    bg_position: `[[left|center|right|top|bottom|<length_percentage>]|[left|center|right|<length_percentage>][top|center|bottom|<length_percentage>]|[center|[left|right]<length_percentage>?]&&[center|[top|bottom]<length_percentage>?]]`,
    attachment: `scroll|fixed|local`,
    line_style: `none|hidden|dotted|dashed|solid|double|groove|ridge|inset|outset`,
    line_width: `thin|medium|thick|<length>`,

    shadow: `inset?&&<length>{2,4}&&<color>?`,

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
    display_listitem : `<display-outside>? && [ flow | flow-root ]? && list-item`,
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

/**
 * Used to _bind_ a rule to a CSS selector.
 * @param      {string}  selector        The raw selector string value
 * @param      {array}  selector_array  An array of selector group identifiers.
 * @memberof module:wick~internals.css
 * @alias CSSSelector
 */
class CSSSelector {

    constructor(selectors /* string */ , selectors_arrays /* array */ ) {

        /**
         * The raw selector string value
         * @package
         */

        this.v = selectors;

        /**
         * Array of separated selector strings in reverse order.
         * @package
         */

        this.a = selectors_arrays;

        /**
         * The CSSRule.
         * @package
         */
        this.r = null;
    }

    get id() {
        return this.v.join("");
    }
    /**
     * Returns a string representation of the object.
     * @return     {string}  String representation of the object.
     */
    toString(off = 0) {
        let offset = ("    ").repeat(off);

        let str = `${offset}${this.v.join(", ")} {\n`;

        if (this.r)
            str += this.r.toString(off + 1);

        return str + `${offset}}\n`;
    }

    addProp(string) {
        let root = this.r.root;
        if (root) {
            let lex = whind$1(string);
            while (!lex.END)
                root.parseProperty(lex, this.r, property_definitions);
        }
    }

}

/**
 * Holds a set of rendered CSS properties.
 * @memberof module:wick~internals.css
 * @alias CSSRule
 */
class CSSRule {
    constructor(root) {
        /**
         * Collection of properties held by this rule.
         * @public
         */
        this.props = {};
        this.LOADED = false;
        this.root = root;
    }

    addProperty(prop, rule) {
        if (prop)
            this.props[prop.name] = prop.value;
    }

    toString(off = 0) {
        let str = [],
            offset = ("    ").repeat(off);

        for (let a in this.props) {
            if (this.props[a] !== null) {
                if (Array.isArray(this.props[a]))
                    str.push(offset, a.replace(/\_/g, "-"), ":", this.props[a].join(" "), ";\n");
                else
                    str.push(offset, a.replace(/\_/g, "-"), ":", this.props[a].toString(), ";\n");
            }
        }

        return str.join(""); //JSON.stringify(this.props).replace(/\"/g, "").replace(/\_/g, "-");
    }

    merge(rule) {
        if (rule.props) {
            for (let n in rule.props)
                this.props[n] = rule.props[n];
            this.LOADED = true;
        }
    }

    get _wick_type_() { return 0; }

    set _wick_type_(v) {}
}

/**
 * wick internals.
 * @class      NR (name)
 */
class NR { //Notation Rule

    constructor() {

        this.r = [NaN, NaN];
        this._terms_ = [];
        this._prop_ = null;
        this._virtual_ = false;
    }

    sp(value, rule) { //Set Property
        if (this._prop_){
            if (value)
                if (Array.isArray(value) && value.length === 1 && Array.isArray(value[0]))
                    rule[this._prop_] = value[0];
                else
                    rule[this._prop_] = value;
        }
    }

    isRepeating() {
        return !(isNaN(this.r[0]) && isNaN(this.r[1]));
    }

    parse(lx, rule, out_val) {
        if (typeof(lx) == "string")
            lx = whind$1(lx);

        let r = out_val || { v: null },
            start = isNaN(this.r[0]) ? 1 : this.r[0],
            end = isNaN(this.r[1]) ? 1 : this.r[1];

        return this.___(lx, rule, out_val, r, start, end);
    }

    ___(lx, rule, out_val, r, start, end) {
        let bool = true;
        for (let j = 0; j < end && !lx.END; j++) {

            for (let i = 0, l = this._terms_.length; i < l; i++) {
                bool = this._terms_[i].parse(lx, rule, r);
                if (!bool) break;
            }

            if (!bool) {

                this.sp(r.v, rule);

                if (j < start)
                    return false;
                else
                    return true;
            }
        }

        this.sp(r.v, rule);

        return true;
    }
}

class AND extends NR {
    ___(lx, rule, out_val, r, start, end) {

        outer:
            for (let j = 0; j < end && !lx.END; j++) {
                for (let i = 0, l = this._terms_.length; i < l; i++)
                    if (!this._terms_[i].parse(lx, rule, r)) return false;
            }

        this.sp(r.v, rule);

        return true;
    }
}

class OR extends NR {
    ___(lx, rule, out_val, r, start, end) {
        let bool = false;

        for (let j = 0; j < end && !lx.END; j++) {
            bool = false;

            for (let i = 0, l = this._terms_.length; i < l; i++)
                if (this._terms_[i].parse(lx, rule, r)) bool = true;

            if (!bool && j < start) {
                this.sp(r.v, rule);
                return false;
            }
        }

        this.sp(r.v, rule);

        return true;
    }
}

class ONE_OF extends NR {
    ___(lx, rule, out_val, r, start, end) {
        let bool = false;

        for (let j = 0; j < end && !lx.END; j++) {
            bool = false;

            for (let i = 0, l = this._terms_.length; i < l; i++) {
                bool = this._terms_[i].parse(lx, rule, r);
                if (bool) break;
            }

            if (!bool)
                if (j < start) {
                    this.sp(r.v, rule);
                    return false;
                }
        }

        this.sp(r.v, rule);

        return bool;
    }
}

class ValueTerm {

    constructor(value, getPropertyParser, definitions) {

        this._value_ = null;

        const IS_VIRTUAL = { is: false };

        if (!(this._value_ = types$1[value]))
            this._value_ = getPropertyParser(value, IS_VIRTUAL, definitions);

        this._prop_ = "";

        if (!this._value_)
            return new LiteralTerm(value);

        if (this._value_ instanceof NR && IS_VIRTUAL.is)
            this._virtual_ = true;
    }

    parse(l, rule, r) {
        if (typeof(l) == "string")
            l = whind$1(l);

        let rn = { v: null };

        let v = this._value_.parse(l, rule, rn);

        if (rn.v) {
            if (r)
                if (r.v) {
                    if (Array.isArray(r.v)) {
                        if (Array.isArray(rn.v) && !this._virtual_)
                            r.v = r.v.concat(rn.v);
                        else
                            r.v.push(rn.v);
                    } else {
                        if (Array.isArray(rn.v) && !this._virtual_)
                            r.v = ([r.v]).concat(rn.v);
                        else
                            r.v = [r.v, rn.v];
                    }
                } else
                    r.v = (this._virtual_) ? [rn.v] : rn.v;

            if (this._prop_)
                rule[this._prop_] = rn.v;

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

            if (this._prop_)
                rule[this._prop_] = v;

            return true;
        } else
            return false;
    }
}

class LiteralTerm {

    constructor(value) {
        this._value_ = value;
        this._prop_ = null;
    }

    parse(l, rule, r) {

        if (typeof(l) == "string")
            l = whind$1(l);

        let v = l.tx;
        if (v == this._value_) {
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

            if (this._prop_)
                rule[this._prop_] = v;

            return true;
        }
        return false;
    }
}

class SymbolTerm extends LiteralTerm {
    parse(l, rule, r) {
        if (typeof(l) == "string")
            l = whind$1(l);

        if (l.tx == this._value_) {
            l.next();
            return true;
        }

        return false;
    }
}

function getPropertyParser(property_name, IS_VIRTUAL = { is: false }, definitions = null) {

    let prop = definitions[property_name];

    if (prop) {

        if (typeof(prop) == "string")
            prop = definitions[property_name] = CreatePropertyParser(prop, property_name, definitions);

        return prop;
    }

    prop = virtual_property_definitions[property_name];

    if (prop) {

        IS_VIRTUAL.is = true;

        if (typeof(prop) == "string")
            prop = virtual_property_definitions[property_name] = CreatePropertyParser(prop, "", definitions);

        return prop;
    }

    return null;
}


function CreatePropertyParser(notation, name, definitions) {

    const l = whind$1(notation);

    const important = { is: false };

    let n = d$1(l, definitions);

    if (n instanceof NR && n._terms_.length == 1)
        n = n._terms_[0];

    n._prop_ = name;
    n.IMP = important.is;

    return n;
}

function d$1(l, definitions, super_term = false, group = false, need_group = false, and_group = false, important = null) {
    let term, nt;

    while (!l.END) {
        switch (l.ch) {
            case "]":
                if (term) return term;
                else 
                    throw new Error("Expected to have term before \"]\"");
            case "[":
                if (term) return term;
                term = d$1(l.next(), definitions);
                l.a("]");
                break;
            case "&":
                if (l.pk.ch == "&") {
                    if (and_group)
                        return term;

                    nt = new AND();

                    nt._terms_.push(term);

                    l.sync().next();

                    while (!l.END) {
                        nt._terms_.push(d$1(l, definitions, super_term, group, need_group, true, important));
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

                        nt._terms_.push(term);

                        l.sync().next();

                        while (!l.END) {
                            nt._terms_.push(d$1(l, definitions, super_term, group, true, and_group, important));
                            if (l.ch !== "|" || l.pk.ch !== "|") break;
                            l.a("|").a("|");
                        }

                        return nt;

                    } else {
                        if (group) {
                            return term;
                        }

                        nt = new ONE_OF();

                        nt._terms_.push(term);

                        l.next();

                        while (!l.END) {
                            nt._terms_.push(d$1(l, definitions, super_term, true, need_group, and_group, important));
                            if (l.ch !== "|") break;
                            l.a("|");
                        }

                        return nt;
                    }
                }
                break;
            case "{":
                term = _Jux_(term);
                term.r[0] = parseInt(l.next().tx);
                if (l.next().ch == ",") {
                    l.next();
                    if (l.next().ch == "}")
                        term.r[1] = Infinity;
                    else {
                        term.r[1] = parseInt(l.tx);
                        l.next();
                    }
                } else
                    term.r[1] = term.r[0];
                l.a("}");
                if (super_term) return term;
                break;
            case "*":
                term = _Jux_(term);
                term.r[0] = 0;
                term.r[1] = Infinity;
                l.next();
                if (super_term) return term;
                break;
            case "+":
                term = _Jux_(term);
                term.r[0] = 1;
                term.r[1] = Infinity;
                l.next();
                if (super_term) return term;
                break;
            case "?":
                term = _Jux_(term);
                term.r[0] = 0;
                term.r[1] = 1;
                l.next();
                if (super_term) return term;
                break;
            case "#":
                term = _Jux_(term);
                term._terms_.push(new SymbolTerm(","));
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
                    if (term instanceof NR && term.isRepeating()) term = _Jux_(new NR, term);
                    let v = d$1(l, definitions, true);
                    term = _Jux_(term, v);
                } else {
                    let v = new ValueTerm(l.next().tx, getPropertyParser, definitions);
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
                    if (term instanceof NR && term.isRepeating()) term = _Jux_(new NR, term);
                    let v = d$1(l, definitions, true);
                    term = _Jux_(term, v);
                } else {
                    let v = (l.ty == l.types.symbol) ? new SymbolTerm(l.tx) : new LiteralTerm(l.tx);
                    l.next();
                    term = v;
                }
        }
    }
    return term;
}

function _Jux_(term, new_term = null) {
    if (term) {
        if (!(term instanceof NR)) {
            let nr = new NR();
            nr._terms_.push(term);
            term = nr;
        }
        if (new_term) term._terms_.push(new_term);
        return term;
    }
    return new_term;
}

/**
 * Checks to make sure token is an Identifier.
 * @param      {Lexer} - A Lexical tokenizing object supporting methods found in {@link Lexer}.
 * @alias module:wick~internals.css.elementIsIdentifier
 */
function _eID_(lexer) {
    if (lexer.ty != lexer.types.id) lexer.throw("Expecting Identifier");
}

/**
 * The empty CSSRule instance
 * @alias module:wick~internals.css.empty_rule
 */
const er = Object.freeze(new CSSRule());

class _selectorPart_ {
    constructor() {
        this.e = "";
        this.ss = [];
        this.c = "";
    }
}
class _mediaSelectorPart_ {
    constructor() {
        this.id = "";
        this.props = {};
        this.c = "";
    }
}

class CSSRuleBody {
    constructor() {
        this.media_selector = null;
        /**
         * All selectors indexed by their value
         */
        this._selectors_ = {};
        /**
         * All selectors in order of appearance
         */
        this._sel_a_ = [];
    }

    _applyProperties_(lexer, rule) {
        while (!lexer.END && lexer.tx !== "}") this.parseProperty(lexer, rule, property_definitions);
        lexer.next();
    }

    /**
     * Gets the last rule matching the selector
     * @param      {string}  string  The string
     * @return     {CSSRule}  The combined set of rules that match the selector.
     */
    getRule(string, r) {
        let selector = this._selectors_[string];
        if (selector) return selector.r;
        return r;
    }


    /**
     * Hook method for hijacking the property parsing function. Return true if default property parsing should not take place
     * @param      {Lexer}   value_lexer    The value lexer
     * @param      {<type>}   property_name  The property name
     * @param      {<type>}   rule           The rule
     * @return     {boolean}  The property hook.
     */
    _getPropertyHook_(value_lexer, property_name, rule) {
        return false;
    }

    /**
     * Used to match selectors to elements
     * @param      {ele}   ele       The ele
     * @param      {string}   criteria  The criteria
     * @return     {boolean}  { description_of_the_return_value }
     * @private
     */
    matchCriteria(ele, criteria) {
        if (criteria.e && ele.tagName !== criteria.e.toUpperCase()) return false;
        outer: for (let i = 0, l = criteria.ss.length; i < l; i++) {
            let ss = criteria.ss[i];
            switch (ss.t) {
                case "attribute":
                    let lex = whind$1(ss.v);
                    if (lex.ch == "[" && lex.pk.ty == lex.types.id) {
                        let id = lex.sync().tx;
                        let attrib = ele.getAttribute(id);
                        if (!attrib) return;
                        if (lex.next().ch == "=") {
                            let value = lex.next().tx;
                            if (attrib !== value) return false;
                        }
                    }
                    break;
                case "pseudo":
                    debugger;
                    break;
                case "class":
                    let class_list = ele.classList;
                    for (let j = 0, jl = class_list.length; j < jl; j++) {
                        if (class_list[j] == ss.v) continue outer;
                    }
                    return false;
                case "id":
                    if (ele.id !== ss.v) return false;
            }
        }
        return true;
    }

    matchMedia(win = window) {

        if (this.media_selector) {
            for (let i = 0; i < this.media_selector.length; i++) {
                let m = this.media_selector[i];
                let props = m.props;
                for (let a in props) {
                    let prop = props[a];
                    if (!prop(win))
                        return false;
                }
            }
        }

        return true;
    }

    /**
     * Retrieves the set of rules from all matching selectors for an element.
     * @param      {HTMLElement}  element - An element to retrieve CSS rules.
     * @public
     */
    getApplicableRules(element, rule = new CSSRule(), win = window) {

        if (!this.matchMedia(win)) return;

        let gen = this.getApplicableSelectors(element),
            sel = null;

        while (sel = gen.next().value) rule.merge(sel.r);
    }

    * getApplicableSelectors(element) {
        for (let j = 0, jl = this._sel_a_.length; j < jl; j++) {
            let ancestor = element;
            let selector = this._sel_a_[j];
            let sn = selector.a;
            let criteria = null;
            outer: for (let x = 0; x < sn.length; x++) {

                let sa = sn[x];

                inner: for (let i = 0, l = sa.length; i < l; i++) {
                    criteria = sa[i];
                    switch (criteria.c) {
                        case "child":
                            if (!(ancestor = ancestor.parentElement) || !this.matchCriteria(ancestor, criteria)) continue outer;
                            break;
                        case "preceded":
                            while ((ancestor = ancestor.previousElementSibling))
                                if (this.matchCriteria(ancestor, criteria)) continue inner;
                            continue outer;
                        case "immediately preceded":
                            if (!(ancestor = ancestor.previousElementSibling) || !this.matchCriteria(ancestor, criteria)) continue outer;
                            break;
                        case "descendant":
                            while ((ancestor = ancestor.parentElement))
                                if (this.matchCriteria(ancestor, criteria)) continue inner;
                            continue outer;
                        default:
                            if (!this.matchCriteria(ancestor, criteria)) continue outer;
                    }
                }
                yield selector;
            }
        }
    }

    /**
     * Parses properties
     * @param      {Lexer}  lexer        The lexer
     * @param      {<type>}  rule         The rule
     * @param      {<type>}  definitions  The definitions
     */
    parseProperty(lexer, rule, definitions) {
        const name = lexer.tx.replace(/\-/g, "_");

        //Catch any comments
        if (lexer.ch == "/") {
            lexer.comment(true);
            return this.parseProperty(lexer, rule, definitions);
        }
        lexer.next().a(":");
        //allow for short circuit < | > | =
        const p = lexer.pk;
        while ((p.ch !== "}" && p.ch !== ";") && !p.END) {
            //look for end of property;
            p.next();
        }
        const out_lex = lexer.copy();
        lexer.sync();
        out_lex.fence(p);
        if (!this._getPropertyHook_(out_lex, name, rule)) {
            try {
                const IS_VIRTUAL = {
                    is: false
                };
                const parser = getPropertyParser(name, IS_VIRTUAL, definitions);
                if (parser && !IS_VIRTUAL.is) {
                    if (!rule.props) rule.props = {};
                    parser.parse(out_lex, rule.props);
                } else
                    //Need to know what properties have not been defined
                    console.warn(`Unable to get parser for css property ${name}`);
            } catch (e) {
                console.log(e);
            }
        }
        if (lexer.ch == ";") lexer.next();
    }

    /** 
    Parses a selector up to a token '{', creating or accessing necessary rules as it progresses. 

    Reference: https://www.w3.org/TR/selectors-3/ 
    https://www.w3.org/TR/css3-mediaqueries/
    https://www.w3.org/TR/selectors-3/

    @param {Lexer} - A Lexical tokenizing object supporting methods found in {@link Lexer}.

    @protected

    */
    parseSelector(lexer) {
        let rule = this,
            id = "",
            selector_array = [],
            selectors_array = [];
        let start = lexer.pos;
        let selectors = [];
        let sel = new _selectorPart_(),
            RETURN = false;
        while (!lexer.END) {
            if (!sel) sel = new _selectorPart_();
            switch (lexer.tx) {
                case "{":
                    RETURN = true;
                case ",":
                    selector_array.unshift(sel);
                    selectors_array.push(selector_array);
                    selector_array = [];
                    selectors.push(lexer.s(start).trim().slice(0));
                    sel = new _selectorPart_();
                    if (RETURN) return new CSSSelector(selectors, selectors_array, this);
                    lexer.next();
                    start = lexer.pos;
                    break;
                case "[":
                    let p = lexer.pk;
                    while (!p.END && p.next().tx !== "]") {};
                    p.a("]");
                    if (p.END) throw new _Error_("Unexpected end of input.");
                    sel.ss.push({
                        t: "attribute",
                        v: p.s(lexer)
                    });
                    lexer.sync();
                    break;
                case ":":
                    sel.ss.push({
                        t: "pseudo",
                        v: lexer.next().tx
                    });
                    _eID_(lexer);
                    lexer.next();
                    break;
                case ".":
                    sel.ss.push({
                        t: "class",
                        v: lexer.next().tx
                    });
                    _eID_(lexer);
                    lexer.next();
                    break;
                case "#":
                    sel.ss.push({
                        t: "id",
                        v: lexer.next().tx
                    });
                    _eID_(lexer);
                    lexer.next();
                    break;
                case "*":
                    lexer.next();
                    break;
                case ">":
                    sel.c = "child";
                    selector_array.unshift(sel);
                    sel = null;
                    lexer.next();
                    break;
                case "~":
                    sel.c = "preceded";
                    selector_array.unshift(sel);
                    sel = null;
                    lexer.next();
                    break;
                case "+":
                    sel.c = "immediately preceded";
                    selector_array.unshift(sel);
                    sel = null;
                    lexer.next();
                    break;
                default:
                    if (sel.e) {
                        sel.c = "descendant";
                        selector_array.unshift(sel);
                        sel = null;
                    } else {
                        sel.e = lexer.tx;

                        _eID_(lexer);
                        lexer.next();
                    }
                    break;
            }
        }
        selector_array.unshift(sel);
        selectors_array.push(selector_array);
        selectors.push(lexer.s(start).trim().slice(0));
        return new CSSSelector(selectors, selectors_array, this);
    }

    /**
     * Parses CSS string
     * @param      {Lexer} - A Lexical tokenizing object supporting methods found in {@link Lexer}
     * @param      {(Array|CSSRuleBody|Object|_mediaSelectorPart_)}  root    The root
     * @return     {Promise}  A promise which will resolve to a CSSRuleBody
     * @private
     */
    parse(lexer, root, res = null, rej = null) {

        if (root && !this.par) root.push(this);

        return new Promise((res, rej) => {
            let selectors = [],
                l = 0;
            while (!lexer.END) {
                switch (lexer.ch) {
                    case "@":
                        lexer.next();
                        switch (lexer.tx) {
                            case "media": //Ignored at this iteration /* https://drafts.csswg.org/mediaqueries/ */
                                //create media query selectors
                                let _med_ = [],
                                    sel = null;
                                while (!lexer.END && lexer.next().ch !== "{") {
                                    if (!sel) sel = new _mediaSelectorPart_();
                                    if (lexer.ch == ",") _med_.push(sel), sel = null;
                                    else if (lexer.ch == "(") {
                                        let start = lexer.next().off;
                                        while (!lexer.END && lexer.ch !== ")") lexer.next();
                                        let out_lex = lexer.copy();
                                        out_lex.off = start;
                                        out_lex.tl = 0;
                                        out_lex.next().fence(lexer);
                                        this.parseProperty(out_lex, sel, media_feature_definitions);
                                        if (lexer.pk.tx.toLowerCase() == "and") lexer.sync();
                                    } else {
                                        let id = lexer.tx.toLowerCase(),
                                            condition = "";
                                        if (id === "only" || id === "not")
                                            (condition = id, id = lexer.next().tx);
                                        sel.c = condition;
                                        sel.id = id;
                                        if (lexer.pk.tx.toLowerCase() == "and") lexer.sync();
                                    }
                                }
                                //debugger
                                lexer.a("{");
                                if (sel)
                                    _med_.push(sel);

                                if (_med_.length == 0)
                                    this.parse(lexer, null); // discard results
                                else {
                                    let media_root = new this.constructor();
                                    media_root.media_selector = _med_;
                                    res(media_root.parse(lexer, root).then(b => {
                                        let body = new this.constructor();
                                        return body.parse(lexer, root);
                                    }));
                                }
                                continue;
                            case "import":
                                /* https://drafts.csswg.org/css-cascade/#at-ruledef-import */
                                let type;
                                if (type = types$1.url.parse(lexer.next())) {
                                    lexer.a(";");
                                    /**
                                     * The {@link CSS_URL} incorporates a fetch mechanism that returns a Promise instance.
                                     * We use that promise to hook into the existing promise returned by CSSRoot#parse,
                                     * executing a new parse sequence on the fetched string data using the existing CSSRoot instance,
                                     * and then resume the current parse sequence.
                                     * @todo Conform to CSS spec and only parse if @import is at the top of the CSS string.
                                     */
                                    return type.fetchText().then((str) =>
                                        //Successfully fetched content, proceed to parse in the current root.
                                        //let import_lexer = ;
                                        res(this.parse(whind$1(str), this).then((r) => this.parse(lexer, r)))
                                        //parse returns Promise. 
                                        // return;
                                    ).catch((e) => res(this.parse(lexer)));
                                } else {
                                    //Failed to fetch resource, attempt to find the end to of the import clause.
                                    while (!lexer.END && lexer.next().tx !== ";") {}
                                    lexer.next();
                                }
                        }
                        break;
                    case "/":
                        lexer.comment(true);
                        break;
                    case "}":
                        lexer.next();
                        return res(this);
                    case "{":
                        let rule = new CSSRule(this);
                        this._applyProperties_(lexer.next(), rule);
                        for (let i = -1, sel = null; sel = selectors[++i];)
                            if (sel.r) sel.r.merge(rule);
                            else sel.r = rule;
                        selectors.length = l = 0;
                        continue;
                }

                let selector = this.parseSelector(lexer, this);

                if (selector) {
                    if (!this._selectors_[selector.id]) {
                        l = selectors.push(selector);
                        this._selectors_[selector.id] = selector;
                        this._sel_a_.push(selector);
                    } else l = selectors.push(this._selectors_[selector.id]);
                }
            }

            return res(this);
        });

    }

    isSame(inCSSRuleBody) {
        if (inCSSRuleBody instanceof CSSRuleBody) {
            if (this.media_selector) {
                if (inCSSRuleBody.media_selector) {
                    //TODO compare media selectors;
                }
            } else if (!inCSSRuleBody.media_selector)
                return true;
        }
        return false;
    }

    merge(inCSSRuleBody) {
        this.parse(whind$1(inCSSRuleBody + ""));
    }

    /**
     * Gets the media.
     * @return     {Object}  The media.
     * @public
     */
    getMedia(win = window) {
        let start = this;
        this._media_.forEach((m) => {
            if (m._med_) {
                let accept = true;
                for (let i = 0, l = m._med_.length; i < l; i++) {
                    let ms = m._med_[i];
                    if (ms.props) {
                        for (let n in ms.props) {
                            if (!ms.props[n](win)) accept = false;
                        }
                    }
                    //if(not)
                    //    accept = !accept;
                    if (accept)
                        (m._next_ = start, start = m);
                }
            }
        });
        return start;
    }

    updated() {
        this.par.updated();
    }

    toString(off = 0) {
        let str = "";
        for (let i = 0; i < this._sel_a_.length; i++) {
            str += this._sel_a_[i].toString(off);
        }
        return str;
    }

    createSelector(selector_value) {
        let selector = this.parseSelector(whind$1(selector_value));

        if (selector)
            if (!this._selectors_[selector.id]) {
                this._selectors_[selector.id] = selector;
                this._sel_a_.push(selector);
                selector.r = new CSSRule(this);
            } else
                selector = this._selectors_[selector.id];

        return selector;
    }
}

LinkedList.mixinTree(CSSRuleBody);

/**
 * Container for all rules found in a CSS string or strings.
 *
 * @memberof module:wick~internals.css
 * @alias CSSRootNode
 */
class CSSRootNode {
    constructor() {
        this.promise = null;
        /**
         * Media query selector
         */
        this.pending_build = 0;
        this.resolves = [];
        this.res = null;
        this.observers = [];
        
        this.addChild(new CSSRuleBody());
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

    READY() {
        if (!this.res) this.res = this._resolveReady_.bind(this);
        return new Promise(this.res);
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

    * getApplicableSelectors(element, win = window) {

        for (let node = this.fch; node; node = this.getNextChild(node)) {

            if(node.matchMedia(win)){
                let gen = node.getApplicableSelectors(element, win);
                let v = null;
                while ((v = gen.next().value))
                    yield v;
            }
        }
    }

    /**
     * Retrieves the set of rules from all matching selectors for an element.
     * @param      {HTMLElement}  element - An element to retrieve CSS rules.
     * @public
     */
    getApplicableRules(element, rule = new CSSRule(), win = window) {
        for (let node = this.fch; node; node = this.getNextChild(node))
            node.getApplicableRules(element, rule, win);
        return rule;
    }

    /**
     * Gets the last rule matching the selector
     * @param      {string}  string  The string
     * @return     {CSSRule}  The combined set of rules that match the selector.
     */
    getRule(string) {
        let r = null;
        for (let node = this.fch; node; node = this.getNextChild(node))
            r = node.getRule(string, r);
        return r;
    }

    toString(off = 0) {
        let str = "";
        for (let node = this.fch; node; node = this.getNextChild(node))
            str += node.toString(off);
        return str;
    }

    addObserver(observer) {
        this.observers.push(observer);
    }

    removeObserver(observer) {
        for (let i = 0; i < this.observers.length; i++)
            if (this.observers[i] == observer) return this.observers.splice(i, 1);
    }

    updated() {
        if (this.observers.length > 0)
            for (let i = 0; i < this.observers.length; i++) this.observers[i].updatedCSS(this);
    }

    parse(lex, root) {
        if (lex.sl > 0) {

            if (!root && root !== null) {
                root = this;
                this.pending_build++;
            }

            return this.fch.parse(lex, this).then(e => {
                this._setREADY_();
                return this;
            });
        }
    }

    merge(inCSSRootNode){
        if(inCSSRootNode instanceof CSSRootNode){
            
            let children = inCSSRootNode.children;
            outer:
            for(let i = 0; i < children.length; i++){
                //determine if this child matches any existing selectors
                let child = children[i];
                
                for(let i = 0; i < this.children.length; i++){
                    let own_child = this.children[i];

                    if(own_child.isSame(child)){
                        own_child.merge(child);
                        continue outer;
                    }
                }

                this.children.push(child);
            }
        }
    }
}

/**
 * CSSRootNode implements all of ll
 * @extends ll
 * @memberof  module:wick~internals.html.CSSRootNode
 * @private
 */
LinkedList.mixinTree(CSSRootNode);
/*
 * Expecting ID error check.
 */
const _err_ = "Expecting Identifier";

/**
 * Builds a CSS object graph that stores `selectors` and `rules` pulled from a CSS string. 
 * @function
 * @param {string} css_string - A string containing CSS data.
 * @param {string} css_string - An existing CSSRootNode to merge with new `selectors` and `rules`.
 * @return {Promise} A `Promise` that will return a new or existing CSSRootNode.
 * @memberof module:wick.core
 * @alias css
 */
const CSSParser = (css_string, root = null) => (root = (!root || !(root instanceof CSSRootNode)) ? new CSSRootNode() : root, root.parse(whind$1(css_string)));

CSSParser.types = types$1;

exports.CSSRule = CSSRule;
exports.CSSSelector = CSSSelector;
exports.CSSRootNode = CSSRootNode;
exports.CSSParser = CSSParser;
exports.default = CSSParser;