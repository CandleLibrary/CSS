
const
    action_array_offset = (191488 << 1),
    error_array_offset = action_array_offset + (1048576 << 2),
    TokenSpace: i32 = 1,
    TokenNumber: i32 = 2,
    TokenIdentifier: i32 = 3,
    TokenNewLine: i32 = 4,
    TokenSymbol: i32 = 5,
    TypeSymbol: i32 = 6,
    TokenKeyword: i32 = 7,
    id: u16 = 2,
    num: u16 = 4;

var
    mark_: u32 = 0,
    action_ptr: u32 = 0,
    error_ptr: u32 = 0,
    stack_ptr: u32 = 0,
    str: string = "",
    FAILED: boolean = false,
    prod: i32 = -1;


class Lexer {

    ty: i32;
    id: i32;
    tl: i32;
    off: i32;
    prev_off: i32;


    constructor() {
        this.ty = 0; //Default "non-value" for types is 1<<18;
        this.id = 0;
        this.tl = 0;
        this.off = 0;
        this.prev_off = 0;
    }

    copy(destination: Lexer = new Lexer()): Lexer {
        destination.off = this.off;
        destination.id = this.id;
        destination.ty = this.ty;
        destination.tl = this.tl;
        destination.prev_off = this.prev_off;
        return destination;
    }

    sync(marker: Lexer): void { marker.copy(this); }

    peek(): Lexer {

        var peeking_marker: Lexer = new Lexer();

        peeking_marker.copy(peeking_marker);

        peeking_marker.next();

        return peeking_marker;
    }

    getOffsetRegionDelta(): u32 {
        return this.off - this.prev_off;
    }

    advanceOffsetRegion(): void {
        this.prev_off = this.off + this.tl;
    }

    syncOffsetRegion(): void {
        this.prev_off = this.off;
    }

    next(): Lexer {

        var l: i32 = str.length,
            length: i32 = 1,
            off: i32 = this.off + this.tl,
            type: i32 = 0,
            base: i32 = off;

        this.ty = 0;
        this.id = 0;

        if (off >= l) {
            this.off = l;
            this.tl = 0;
            return this;
        }

        const code: i32 = str.codePointAt(off);

        switch (load<u16>(0 + (code << 1)) & 255) {
            default:
            case 0: //SYMBOL
                this.id = type = TypeSymbol;
                break;
            case 1: //IDENTIFIER
                while (1) {
                    while (++off < l && (((id | num) & (load<u16>(0 + (str.codePointAt(off) << 1)) >> 8))));
                    this.id = type = TokenIdentifier;
                    length = off - base;
                    break;
                } break;
            case 2: //SPACE SET
                this.id = type = TokenSpace;
                break;
            case 3: //CARRIAGE RETURN
                length = 2;
            //intentional
            case 4: //LINEFEED
                this.id = type = TokenNewLine;
                break;
            case 5: //NUMBER
                this.id = type = TokenNumber;
                //Check for binary, hexadecimal, and octal representation
                while (++off < l && (num & (load<u16>(0 + (str.codePointAt(off) << 1)) >> 8)));
                length = off - base;
                break;
        }
        if (type == TokenIdentifier) {
            const val: u32 = str.charCodeAt(base + 0);
            if (val == 105) {
                if (length <= 1) { type = TokenKeyword; this.id = 48; length = 1; }
                const val: u32 = str.charCodeAt(base + 1);
                if (val == 109) {
                    const val: u32 = str.charCodeAt(base + 2);
                    if (val == 112) {
                        const val: u32 = str.charCodeAt(base + 3);
                        if (val == 111) {
                            const val: u32 = str.charCodeAt(base + 4);
                            if (val == 114) {
                                const val: u32 = str.charCodeAt(base + 5);
                                if (val == 116) {
                                    if (length <= 6) { type = TokenKeyword; this.id = 15; length = 6; }
                                    const val: u32 = str.charCodeAt(base + 6);
                                    if (val == 97) {
                                        const val: u32 = str.charCodeAt(base + 7);
                                        if (val == 110) {
                                            const val: u32 = str.charCodeAt(base + 8);
                                            if (val == 116) {
                                                if (length <= 9) { type = TokenKeyword; this.id = 53; length = 9; }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            else if (val == 115) {
                if (length <= 1) { type = TokenKeyword; this.id = 49; length = 1; }
                const val: u32 = str.charCodeAt(base + 1);
                if (val == 117) {
                    const val: u32 = str.charCodeAt(base + 2);
                    if (val == 112) {
                        const val: u32 = str.charCodeAt(base + 3);
                        if (val == 112) {
                            const val: u32 = str.charCodeAt(base + 4);
                            if (val == 111) {
                                const val: u32 = str.charCodeAt(base + 5);
                                if (val == 114) {
                                    const val: u32 = str.charCodeAt(base + 6);
                                    if (val == 116) {
                                        const val: u32 = str.charCodeAt(base + 7);
                                        if (val == 115) {
                                            if (length <= 8) { type = TokenKeyword; this.id = 16; length = 8; }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            else if (val == 107) {
                const val: u32 = str.charCodeAt(base + 1);
                if (val == 101) {
                    const val: u32 = str.charCodeAt(base + 2);
                    if (val == 121) {
                        const val: u32 = str.charCodeAt(base + 3);
                        if (val == 102) {
                            const val: u32 = str.charCodeAt(base + 4);
                            if (val == 114) {
                                const val: u32 = str.charCodeAt(base + 5);
                                if (val == 97) {
                                    const val: u32 = str.charCodeAt(base + 6);
                                    if (val == 109) {
                                        const val: u32 = str.charCodeAt(base + 7);
                                        if (val == 101) {
                                            const val: u32 = str.charCodeAt(base + 8);
                                            if (val == 115) {
                                                if (length <= 9) { type = TokenKeyword; this.id = 19; length = 9; }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            else if (val == 102) {
                if (length <= 1) { type = TokenKeyword; this.id = 62; length = 1; }
                const val: u32 = str.charCodeAt(base + 1);
                if (val == 114) {
                    const val: u32 = str.charCodeAt(base + 2);
                    if (val == 111) {
                        const val: u32 = str.charCodeAt(base + 3);
                        if (val == 109) {
                            if (length <= 4) { type = TokenKeyword; this.id = 20; length = 4; }
                        }
                    }
                }
                else if (val == 97) {
                    const val: u32 = str.charCodeAt(base + 2);
                    if (val == 108) {
                        const val: u32 = str.charCodeAt(base + 3);
                        if (val == 115) {
                            const val: u32 = str.charCodeAt(base + 4);
                            if (val == 101) {
                                if (length <= 5) { type = TokenKeyword; this.id = 33; length = 5; }
                            }
                        }
                    }
                }
            }
            else if (val == 116) {
                const val: u32 = str.charCodeAt(base + 1);
                if (val == 111) {
                    if (length <= 2) { type = TokenKeyword; this.id = 21; length = 2; }
                }
                else if (val == 114) {
                    const val: u32 = str.charCodeAt(base + 2);
                    if (val == 117) {
                        const val: u32 = str.charCodeAt(base + 3);
                        if (val == 101) {
                            if (length <= 4) { type = TokenKeyword; this.id = 32; length = 4; }
                        }
                    }
                }
            }
            else if (val == 110) {
                const val: u32 = str.charCodeAt(base + 1);
                if (val == 111) {
                    const val: u32 = str.charCodeAt(base + 2);
                    if (val == 116) {
                        if (length <= 3) { type = TokenKeyword; this.id = 22; length = 3; }
                    }
                }
            }
            else if (val == 97) {
                if (length <= 1) { type = TokenKeyword; this.id = 57; length = 1; }
                const val: u32 = str.charCodeAt(base + 1);
                if (val == 110) {
                    const val: u32 = str.charCodeAt(base + 2);
                    if (val == 100) {
                        if (length <= 3) { type = TokenKeyword; this.id = 23; length = 3; }
                    }
                }
            }
            else if (val == 111) {
                const val: u32 = str.charCodeAt(base + 1);
                if (val == 114) {
                    if (length <= 2) { type = TokenKeyword; this.id = 24; length = 2; }
                }
                else if (val == 110) {
                    const val: u32 = str.charCodeAt(base + 2);
                    if (val == 108) {
                        const val: u32 = str.charCodeAt(base + 3);
                        if (val == 121) {
                            if (length <= 4) { type = TokenKeyword; this.id = 26; length = 4; }
                        }
                    }
                }
            }
            else if (val == 109) {
                const val: u32 = str.charCodeAt(base + 1);
                if (val == 101) {
                    const val: u32 = str.charCodeAt(base + 2);
                    if (val == 100) {
                        const val: u32 = str.charCodeAt(base + 3);
                        if (val == 105) {
                            const val: u32 = str.charCodeAt(base + 4);
                            if (val == 97) {
                                if (length <= 5) { type = TokenKeyword; this.id = 25; length = 5; }
                            }
                        }
                    }
                }
            }
            else if (val == 117) {
                const val: u32 = str.charCodeAt(base + 1);
                if (val == 114) {
                    const val: u32 = str.charCodeAt(base + 2);
                    if (val == 108) {
                        if (length <= 3) { type = TokenKeyword; this.id = 36; length = 3; }
                    }
                }
            }
            else if (val == 98) {
                if (length <= 1) { type = TokenKeyword; this.id = 58; length = 1; }
            }
            else if (val == 99) {
                if (length <= 1) { type = TokenKeyword; this.id = 59; length = 1; }
            }
            else if (val == 100) {
                if (length <= 1) { type = TokenKeyword; this.id = 60; length = 1; }
            }
            else if (val == 101) {
                if (length <= 1) { type = TokenKeyword; this.id = 61; length = 1; }
            }
            else if (val == 65) {
                if (length <= 1) { type = TokenKeyword; this.id = 63; length = 1; }
            }
            else if (val == 66) {
                if (length <= 1) { type = TokenKeyword; this.id = 64; length = 1; }
            }
            else if (val == 67) {
                if (length <= 1) { type = TokenKeyword; this.id = 65; length = 1; }
            }
            else if (val == 68) {
                if (length <= 1) { type = TokenKeyword; this.id = 66; length = 1; }
            }
            else if (val == 69) {
                if (length <= 1) { type = TokenKeyword; this.id = 67; length = 1; }
            }
            else if (val == 70) {
                if (length <= 1) { type = TokenKeyword; this.id = 68; length = 1; }
            }
        }
        if (type == TypeSymbol || type == TokenIdentifier) {
            const val: u32 = str.charCodeAt(base + 0);
            if (val == 44) {
                type = TokenSymbol; this.id = 10 /* , */; length = 1;
            }
            else if (val == 123) {
                type = TokenSymbol; this.id = 11 /* { */; length = 1;
            }
            else if (val == 59) {
                type = TokenSymbol; this.id = 12 /* ; */; length = 1;
            }
            else if (val == 125) {
                type = TokenSymbol; this.id = 13 /* } */; length = 1;
            }
            else if (val == 64) {
                type = TokenSymbol; this.id = 14 /* @ */; length = 1;
            }
            else if (val == 40) {
                type = TokenSymbol; this.id = 17 /* ( */; length = 1;
            }
            else if (val == 41) {
                type = TokenSymbol; this.id = 18 /* ) */; length = 1;
            }
            else if (val == 34) {
                type = TokenSymbol; this.id = 27 /* " */; length = 1;
            }
            else if (val == 58) {
                type = TokenSymbol; this.id = 28 /* : */; length = 1;
            }
            else if (val == 60) {
                type = TokenSymbol; this.id = 29 /* < */; length = 1;
            }
            else if (val == 61) {
                type = TokenSymbol; this.id = 30 /* = */; length = 1;
            }
            else if (val == 62) {
                type = TokenSymbol; this.id = 31 /* > */; length = 1;
            }
            else if (val == 47) {
                type = TokenSymbol; this.id = 34 /* / */; length = 1;
            }
            else if (val == 37) {
                type = TokenSymbol; this.id = 35 /* % */; length = 1;
            }
            else if (val == 39) {
                type = TokenSymbol; this.id = 37 /* ' */; length = 1;
            }
            else if (val == 43) {
                type = TokenSymbol; this.id = 38 /* + */; length = 1;
            }
            else if (val == 126) {
                type = TokenSymbol; this.id = 39 /* ~ */; length = 1;
            }
            else if (val == 124) {
                type = TokenSymbol; this.id = 51 /* | */; length = 1;
                const val: u32 = str.charCodeAt(base + 1);
                if (val == 124) {
                    type = TokenSymbol; this.id = 40 /* || */; length = 2;
                }
            }
            else if (val == 35) {
                type = TokenSymbol; this.id = 41 /* # */; length = 1;
            }
            else if (val == 46) {
                type = TokenSymbol; this.id = 42 /* . */; length = 1;
            }
            else if (val == 91) {
                type = TokenSymbol; this.id = 43 /* [ */; length = 1;
            }
            else if (val == 93) {
                type = TokenSymbol; this.id = 44 /* ] */; length = 1;
            }
            else if (val == 94) {
                const val: u32 = str.charCodeAt(base + 1);
                if (val == 61) {
                    type = TokenSymbol; this.id = 45 /* ^= */; length = 2;
                }
            }
            else if (val == 36) {
                type = TokenSymbol; this.id = 56 /* $ */; length = 1;
                const val: u32 = str.charCodeAt(base + 1);
                if (val == 61) {
                    type = TokenSymbol; this.id = 46 /* $= */; length = 2;
                }
            }
            else if (val == 42) {
                type = TokenSymbol; this.id = 50 /* astrix */; length = 1;
                const val: u32 = str.charCodeAt(base + 1);
                if (val == 61) {
                    type = TokenSymbol; this.id = 47 /* astrix= */; length = 2;
                }
            }
            else if (val == 33) {
                type = TokenSymbol; this.id = 52 /* ! */; length = 1;
            }
            else if (val == 95) {
                type = TokenSymbol; this.id = 54 /* _ */; length = 1;
            }
            else if (val == 45) {
                type = TokenSymbol; this.id = 55 /* - */; length = 1;
            }
            else if (val == 92) {
                type = TokenSymbol; this.id = 69 /* \ */; length = 1;
            }
        }

        this.ty = type;
        this.off = base;
        this.tl = length;

        return this;
    }
    get END(): boolean { return this.off >= str.length; }
}

function set_error(val: u32): void {
    store<u32>(((error_ptr++ & 0xFF) << 2) + error_array_offset, val);
}

function set_action(val: u32): void {
    store<u32>(((action_ptr++) << 2) + (action_array_offset), val);
}

function completeProduction(body: u32, len: u32, production: u32): void {
    add_reduce(len, body);
    prod = production;
}

function completeProductionPlain(len: u32, production: u32): void {
    prod = production;
}

@inline
function mark(): u32 {
    mark_ = action_ptr;
    return mark_;
}

@inline
function reset(mark: u32): void {
    action_ptr = mark;
}

function add_shift(l: Lexer, char_len: u32): void {
    const skip_delta = l.getOffsetRegionDelta();

    let has_skip: u32 = +(skip_delta > 0),
        has_len: u32 = +(char_len > 0),
        val: u32 = 1;

    val |= skip_delta << 3;

    if (has_skip && (skip_delta > 0x8FFF || char_len > 0x8FFF)) {
        add_shift(l, 0);
        has_skip = 0;
        val = 1;
    }

    val |= (has_skip << 2) | (has_len << 1) | char_len << (3 + 15 * has_skip);

    set_action(val);

    l.advanceOffsetRegion();
}

function add_reduce(sym_len: u32, body: u32, DO_NOT_PUSH_TO_STACK: boolean = false): void {
    const val: u32 = ((0 << 0) | ((DO_NOT_PUSH_TO_STACK ? 1 : 0) << 1) | ((sym_len & 0x3FFF) << 2) | (body << 16));
    set_action(val);
}

function fail(lex: Lexer): void {
    prod = -1;
    soft_fail(lex);
}

function soft_fail(lex: Lexer): void {
    FAILED = true;
    set_error(lex.off);
}

function setProduction(production: u32): void {
    prod = (-FAILED) + (-FAILED + 1) * production;
}
function _pk(l: Lexer, /* eh, */ skips: StaticArray<u32> = []): Lexer {
    l.next();
    _skip(l, skips);
    return l;
}

function _skip(l: Lexer, skips: StaticArray<u32>): void {
    while (1) {



        if ((!skips.includes(l.ty) && !skips.includes(l.id)))
            break;

        l.next();
    }
}

function _no_check_with_skip(lex: Lexer, skips: StaticArray<u32>): void {
    add_shift(lex, lex.tl);
    lex.next();
    _skip(lex, skips);
}

function _no_check(lex: Lexer): void {
    add_shift(lex, lex.tl);
    lex.next();
}

function _with_skip(lex: Lexer, skips: StaticArray<u32>, sym: u32 = 0): void {

    if (FAILED) return;

    if (sym == 0 || lex.id == sym || lex.ty == sym) {
        _no_check_with_skip(lex, skips);
    } else {
        //TODO error recovery
        soft_fail(lex);
    }
}

function _(lex: Lexer, sym: u32 = 0): void {

    if (FAILED) return;

    if (sym == 0 || lex.id == sym || lex.ty == sym) {
        _no_check(lex);
    } else {
        //TODO error recovery
        soft_fail(lex);
    }
}

var prob_index = 0;
//For Debugging
function probe(l: Lexer, id: u32 = 1): void {
    set_error(0xF000000F + (id << 16) + (prob_index << 4));
    set_error(l.ty);
    set_error(l.id);
    set_error(l.tl);
    set_error(l.off);
    set_error(prod);
    set_error(stack_ptr);
    set_error(FAILED);
    set_error(0xF000000F + (id << 16) + ((prob_index++) << 4));
}


const const__ = StaticArray.fromArray<u32>([4/* \nl */, 1/* \ws */]),
    const_0_ = StaticArray.fromArray<u32>([28/* \: */, 41/* \# */, 42/* \. */, 43/* \[ */, 50/* \* */, 51/* \| */, 55/* \- */, 56/* \$ */]),
    const_6_ = StaticArray.fromArray<u32>([18/* \) */, 28/* \: */, 41/* \# */, 42/* \. */, 43/* \[ */, 50/* \* */, 51/* \| */, 55/* \- */, 56/* \$ */]),
    const_1_ = StaticArray.fromArray<u32>([4/* \nl */]),
    const_13_ = StaticArray.fromArray<u32>([28/* \: */, 31/* \> */, 38/* \+ */, 39/* \~ */, 40/* \|| */, 41/* \# */, 42/* \. */, 43/* \[ */, 50/* \* */, 51/* \| */, 55/* \- */, 56/* \$ */]),
    _340id0 = (l: Lexer): void => {
        let $mark = mark(), sp = stack_ptr, cp = l.copy();
        $STYLE_SHEET_HC_listbody1_100(cp); stack_ptr++;
        if (FAILED) {
            reset($mark); FAILED = false; stack_ptr = sp;
            $STYLE_SHEET_HC_listbody1_102(l); stack_ptr++;;
        } else l.sync(cp);

    },
    _340id1 = (l: Lexer): void => {

        $STYLE_SHEET_HC_listbody1_102(l); stack_ptr++;

    },
    _342id0 = (l: Lexer): void => {

        $STYLE_SHEET_group_03_101(l); stack_ptr++;

    },
    _341id8 = (l: Lexer): void => {
        let $mark = mark(), sp = stack_ptr, cp = l.copy();
        $import(cp); stack_ptr++;
        if (FAILED) {
            reset($mark); FAILED = false; stack_ptr = sp;
            $STYLE_SHEET_HC_listbody1_102(l); stack_ptr++;;
        } else l.sync(cp);

    },
    _368id0 = (l: Lexer): void => {

        completeProduction(1, 2, 3); stack_ptr -= 2;

    },
    _481id0 = (l: Lexer): void => {
        let $mark = mark(), sp = stack_ptr, cp = l.copy();
        completeProduction(1, 2, 1); stack_ptr -= 2;
        if (FAILED) {
            reset($mark); FAILED = false; stack_ptr = sp;
            completeProductionPlain(1, 9); stack_ptr -= 1;;
        } else l.sync(cp);

    },
    _147id0 = (l: Lexer): void => {

        completeProductionPlain(1, 2); stack_ptr -= 1;

    },
    _145id0 = (l: Lexer): void => {

        completeProduction(2, 1, 3); stack_ptr -= 1;

    },
    _345id0 = (l: Lexer): void => {

        completeProductionPlain(2, 9); stack_ptr -= 2;

    },
    _2id0 = (l: Lexer): void => {

        completeProduction(2, 1, 1); stack_ptr -= 1;

    },
    _249id0 = (l: Lexer): void => {

        completeProduction(1, 2, 1); stack_ptr -= 2;

    },
    _166id0 = (l: Lexer): void => {

        $STYLE_RULE_HC_listbody2_103(l); stack_ptr++;

    },
    _133id0 = (l: Lexer): void => {

        _no_check(l);; stack_ptr++; State337(l);

    },
    _133id1 = (l: Lexer): void => {

        _no_check(l);; stack_ptr++; State315(l);

    },
    _337id0 = (l: Lexer): void => {
        let $mark = mark(), sp = stack_ptr, cp = l.copy();
        $declaration_list(cp); stack_ptr++;
        if (FAILED) {
            reset($mark); FAILED = false; stack_ptr = sp;
            _no_check(l);; stack_ptr++; State437(l);;
        } else l.sync(cp);

    },
    _337id1 = (l: Lexer): void => {

        $declaration_list(l); stack_ptr++;

    },
    _337id3 = (l: Lexer): void => {

        _no_check(l);; stack_ptr++; State438(l);

    },
    _315id0 = (l: Lexer): void => {

        $COMPLEX_SELECTOR(l); stack_ptr++;

    },
    _438id0 = (l: Lexer): void => {

        completeProduction(9, 3, 6); stack_ptr -= 3;

    },
    _436id0 = (l: Lexer): void => {

        _no_check(l);; stack_ptr++; State470(l);

    },
    _436id1 = (l: Lexer): void => {

        _no_check(l);; stack_ptr++; State471(l);

    },
    _472id0 = (l: Lexer): void => {

        completeProduction(9, 4, 6); stack_ptr -= 4;

    },
    _471id0 = (l: Lexer): void => {

        completeProduction(8, 4, 6); stack_ptr -= 4;

    },
    _491id0 = (l: Lexer): void => {

        completeProduction(8, 5, 6); stack_ptr -= 5;

    },
    const_2_ = StaticArray.fromArray<u32>([12/* \; */, 14/* \@ */, 28/* \: */, 41/* \# */, 42/* \. */, 43/* \[ */, 50/* \* */, 51/* \| */, 55/* \- */, 56/* \$ */]),
    _59id0 = (l: Lexer): void => {

        completeProduction(2, 1, 13); stack_ptr -= 1;

    },
    _410id0 = (l: Lexer): void => {

        completeProduction(7, 3, 13); stack_ptr -= 3;

    },
    _115id0 = (l: Lexer): void => {
        let $mark = mark(), sp = stack_ptr, cp = l.copy();
        $media_condition(cp); stack_ptr++;
        if (FAILED) {
            reset($mark); FAILED = false; stack_ptr = sp;
            $media_query_group_043_116(l); stack_ptr++;;
        } else l.sync(cp);

    },
    _115id1 = (l: Lexer): void => {

        $media_condition(l); stack_ptr++;

    },
    _115id2 = (l: Lexer): void => {
        let $mark = mark(), sp = stack_ptr, cp = l.copy();
        $media_condition(cp); stack_ptr++;
        if (FAILED) {
            reset($mark); FAILED = false; stack_ptr = sp;
            $media_type(l); stack_ptr++;;
        } else l.sync(cp);

    },
    _115id4 = (l: Lexer): void => {

        $media_query_group_043_116(l); stack_ptr++;

    },
    _44id0 = (l: Lexer): void => {

        completeProduction(28, 1, 37); stack_ptr -= 1;

    },
    _282id0 = (l: Lexer): void => {

        completeProduction(30, 2, 37); stack_ptr -= 2;

    },
    _292id0 = (l: Lexer): void => {

        completeProduction(31, 2, 37); stack_ptr -= 2;

    },
    _407id0 = (l: Lexer): void => {

        completeProduction(29, 3, 37); stack_ptr -= 3;

    },
    _20id0 = (l: Lexer): void => {

        $supports_condition(l); stack_ptr++;

    },
    _20id2 = (l: Lexer): void => {
        let $mark = mark(), sp = stack_ptr, cp = l.copy();
        $supports_condition(cp); stack_ptr++;
        if (FAILED) {
            reset($mark); FAILED = false; stack_ptr = sp;
            $import_declaration(l); stack_ptr++;;
        } else l.sync(cp);

    },
    _209id0 = (l: Lexer): void => {

        $COMPLEX_SELECTOR_group_1119_132(l); stack_ptr++;

    },
    _211id0 = (l: Lexer): void => {

        completeProduction(2, 1, 72); stack_ptr -= 1;

    },
    _390id0 = (l: Lexer): void => {

        completeProduction(1, 2, 72); stack_ptr -= 2;

    },
    _156id0 = (l: Lexer): void => {

        $STYLE_RULE(l); stack_ptr++;

    },
    _158id0 = (l: Lexer): void => {

        completeProduction(2, 1, 8); stack_ptr -= 1;

    },
    _369id0 = (l: Lexer): void => {

        completeProduction(1, 2, 8); stack_ptr -= 2;

    },
    _508id0 = (l: Lexer): void => {

        $media_condition_without_or(l); stack_ptr++;

    },
    _508id1 = (l: Lexer): void => {
        let $mark = mark(), sp = stack_ptr, cp = l.copy();
        $media_or(cp); stack_ptr++;
        if (FAILED) {
            reset($mark); FAILED = false; stack_ptr = sp;
            $media_condition_without_or(l); stack_ptr++;;
        } else l.sync(cp);

    },
    _46id0 = (l: Lexer): void => {

        completeProductionPlain(1, 38); stack_ptr -= 1;

    },
    _159id0 = (l: Lexer): void => {
        let $mark = mark(), sp = stack_ptr, cp = l.copy();
        $WQ_NAME(cp); stack_ptr++;
        if (FAILED) {
            reset($mark); FAILED = false; stack_ptr = sp;
            $NS_PREFIX(l); stack_ptr++;;
        } else l.sync(cp);

    },
    _159id2 = (l: Lexer): void => {
        let $mark = mark(), sp = stack_ptr, cp = l.copy();
        $WQ_NAME(cp); stack_ptr++;
        if (FAILED) {
            reset($mark); FAILED = false; stack_ptr = sp; cp = l.copy();
            $NS_PREFIX(cp); stack_ptr++;;
            if (FAILED) {
                reset($mark); FAILED = false; stack_ptr = sp;
                _no_check(l);; stack_ptr++; State92(l);;
            } else l.sync(cp);

        } else l.sync(cp);

    },
    _92id0 = (l: Lexer): void => {

        completeProduction(73, 1, 89); stack_ptr -= 1;

    },
    _92id4 = (l: Lexer): void => {
        let $mark = mark(), sp = stack_ptr, cp = l.copy();
        completeProductionPlain(1, 91); stack_ptr -= 1;
        if (FAILED) {
            reset($mark); FAILED = false; stack_ptr = sp;
            completeProduction(73, 1, 89); stack_ptr -= 1;;
        } else l.sync(cp);

    },
    _90id0 = (l: Lexer): void => {

        completeProduction(71, 1, 89); stack_ptr -= 1;

    },
    _91id0 = (l: Lexer): void => {

        $identifier(l); stack_ptr++;

    },
    _91id2 = (l: Lexer): void => {

        _no_check(l);; stack_ptr++; State327(l);

    },
    _327id0 = (l: Lexer): void => {

        completeProduction(72, 2, 89); stack_ptr -= 2;

    },
    _328id0 = (l: Lexer): void => {

        completeProduction(77, 2, 93); stack_ptr -= 2;

    },
    const_3_ = StaticArray.fromArray<u32>([10/* \, */, 11/* \{ */, 18/* \) */, 31/* \> */, 38/* \+ */, 39/* \~ */, 40/* \|| */, 50/* \* */, 51/* \| */, 55/* \- */, 56/* \$ */]),
    _96id0 = (l: Lexer): void => {

        completeProduction(2, 1, 74); stack_ptr -= 1;

    },
    _318id0 = (l: Lexer): void => {

        completeProduction(1, 2, 74); stack_ptr -= 2;

    },
    const_4_ = StaticArray.fromArray<u32>([10/* \, */, 11/* \{ */, 18/* \) */, 31/* \> */, 38/* \+ */, 39/* \~ */, 40/* \|| */, 41/* \# */, 42/* \. */, 43/* \[ */, 50/* \* */, 51/* \| */, 55/* \- */, 56/* \$ */]),
    _105id0 = (l: Lexer): void => {

        completeProduction(2, 1, 77); stack_ptr -= 1;

    },
    _320id0 = (l: Lexer): void => {

        completeProduction(1, 2, 77); stack_ptr -= 2;

    },
    _352id0 = (l: Lexer): void => {

        $media_not(l); stack_ptr++;

    },
    _352id1 = (l: Lexer): void => {
        let $mark = mark(), sp = stack_ptr, cp = l.copy();
        $media_in_parenths(cp); stack_ptr++;
        if (FAILED) {
            reset($mark); FAILED = false; stack_ptr = sp;
            $media_and(l); stack_ptr++;;
        } else l.sync(cp);

    },
    _353id0 = (l: Lexer): void => {

        completeProductionPlain(1, 39); stack_ptr -= 1;

    },
    _284id0 = (l: Lexer): void => {

        completeProduction(22, 2, 43); stack_ptr -= 2;

    },
    _301id0 = (l: Lexer): void => {

        completeProduction(1, 2, 42); stack_ptr -= 2;

    },
    _179id0 = (l: Lexer): void => {
        let $mark = mark(), sp = stack_ptr, cp = l.copy();
        $NS_PREFIX(cp); stack_ptr++;
        if (FAILED) {
            reset($mark); FAILED = false; stack_ptr = sp;
            $identifier(l); stack_ptr++;;
        } else l.sync(cp);

    },
    _179id2 = (l: Lexer): void => {

        $NS_PREFIX(l); stack_ptr++;

    },
    _93id0 = (l: Lexer): void => {

        completeProduction(78, 1, 93); stack_ptr -= 1;

    },
    _93id1 = (l: Lexer): void => {
        let $mark = mark(), sp = stack_ptr, cp = l.copy();
        completeProductionPlain(1, 91); stack_ptr -= 1;
        if (FAILED) {
            reset($mark); FAILED = false; stack_ptr = sp;
            completeProduction(78, 1, 93); stack_ptr -= 1;;
        } else l.sync(cp);

    },
    const_5_ = StaticArray.fromArray<u32>([10/* \, */, 11/* \{ */, 12/* \; */, 14/* \@ */, 18/* \) */, 28/* \: */, 41/* \# */, 42/* \. */, 43/* \[ */, 50/* \* */, 51/* \| */, 55/* \- */, 56/* \$ */]),
    _65id0 = (l: Lexer): void => {

        completeProduction(2, 1, 42); stack_ptr -= 1;

    },
    _69id0 = (l: Lexer): void => {

        completeProduction(2, 1, 45); stack_ptr -= 1;

    },
    _300id0 = (l: Lexer): void => {

        completeProduction(1, 2, 45); stack_ptr -= 2;

    },
    _204id0 = (l: Lexer): void => {

        _no_check(l);; stack_ptr++; State33(l);

    },
    _204id1 = (l: Lexer): void => {

        _no_check(l);; stack_ptr++; State36(l);

    },
    _204ty0 = (l: Lexer): void => {

        _no_check(l);; stack_ptr++; State34(l);

    },
    _204ty1 = (l: Lexer): void => {

        _no_check(l);; stack_ptr++; State35(l);

    },
    const_7_ = StaticArray.fromArray<u32>([10/* \, */, 11/* \{ */, 12/* \; */, 14/* \@ */, 17/* \( */, 18/* \) */, 23/* \and */, 28/* \: */, 29/* \< */, 30/* \= */, 31/* \> */, 38/* \+ */, 39/* \~ */, 40/* \|| */, 41/* \# */, 42/* \. */, 43/* \[ */, 44/* \] */, 45/* \^= */, 46/* \$= */, 47/* \*= */, 48/* \i */, 49/* \s */, 50/* \* */, 51/* \| */]),
    _33id0 = (l: Lexer): void => {

        completeProductionPlain(1, 105); stack_ptr -= 1;

    },
    _205id0 = (l: Lexer): void => {

        _no_check(l);; stack_ptr++; State271(l);

    },
    _205id1 = (l: Lexer): void => {

        _no_check(l);; stack_ptr++; State272(l);

    },
    _205id2 = (l: Lexer): void => {

        _no_check(l);; stack_ptr++; State273(l);

    },
    _205ty0 = (l: Lexer): void => {

        _no_check(l);; stack_ptr++; State269(l);

    },
    _205ty1 = (l: Lexer): void => {

        _no_check(l);; stack_ptr++; State270(l);

    },
    _205ty2 = (l: Lexer): void => {

        _no_check(l);; stack_ptr++; State274(l);

    },
    _269id0 = (l: Lexer): void => {

        completeProduction(34, 2, 105); stack_ptr -= 2;

    },
    _448id0 = (l: Lexer): void => {

        _no_check(l);; stack_ptr++; State482(l);

    },
    _448id1 = (l: Lexer): void => {

        _no_check(l);; stack_ptr++; State359(l);

    },
    _139id0 = (l: Lexer): void => {

        completeProduction(2, 1, 75); stack_ptr -= 1;

    },
    _339id0 = (l: Lexer): void => {

        completeProduction(1, 2, 75); stack_ptr -= 2;

    },
    _71id0 = (l: Lexer): void => {
        let $mark = mark(), sp = stack_ptr, cp = l.copy();
        $mf_range(cp); stack_ptr++;
        if (FAILED) {
            reset($mark); FAILED = false; stack_ptr = sp;
            $mf_plain(l); stack_ptr++;;
        } else l.sync(cp);

    },
    _71id2 = (l: Lexer): void => {

        $mf_boolean(l); stack_ptr++;

    },
    _71ty2 = (l: Lexer): void => {

        $mf_range(l); stack_ptr++;

    },
    _125ty0 = (l: Lexer): void => {
        let $mark = mark(), sp = stack_ptr, cp = l.copy();
        $mf_name(cp); stack_ptr++;
        if (FAILED) {
            reset($mark); FAILED = false; stack_ptr = sp;
            $mf_value(l); stack_ptr++;;
        } else l.sync(cp);

    },
    _125ty2 = (l: Lexer): void => {

        $mf_value(l); stack_ptr++;

    },
    _79id0 = (l: Lexer): void => {
        let $mark = mark(), sp = stack_ptr, cp = l.copy();
        $mf_range_group_074_125(cp); stack_ptr++;
        if (FAILED) {
            reset($mark); FAILED = false; stack_ptr = sp;
            $mf_range_group_188_127(l); stack_ptr++;;
        } else l.sync(cp);

    },
    _79id1 = (l: Lexer): void => {
        let $mark = mark(), sp = stack_ptr, cp = l.copy();
        $mf_range_group_074_125(cp); stack_ptr++;
        if (FAILED) {
            reset($mark); FAILED = false; stack_ptr = sp;
            $mf_range_group_183_126(l); stack_ptr++;;
        } else l.sync(cp);

    },
    _79id2 = (l: Lexer): void => {

        $mf_range_group_074_125(l); stack_ptr++;

    };
const idm340: Map<number, (L: Lexer) => void> = new Map();
idm340.set(14/* @ */, _340id0);
idm340.set(56/* $ */, _340id1);
idm340.set(55/* - */, _340id1);
idm340.set(50/* * */, _340id1);
idm340.set(51/* | */, _340id1);
idm340.set(41/* # */, _340id1);
idm340.set(42/* . */, _340id1);
idm340.set(43/* [ */, _340id1);
idm340.set(28/* : */, _340id1);
const idm342: Map<number, (L: Lexer) => void> = new Map();
idm342.set(56/* $ */, _342id0);
idm342.set(55/* - */, _342id0);
idm342.set(50/* * */, _342id0);
idm342.set(51/* | */, _342id0);
idm342.set(41/* # */, _342id0);
idm342.set(42/* . */, _342id0);
idm342.set(43/* [ */, _342id0);
idm342.set(28/* : */, _342id0);
idm342.set(14/* @ */, _342id0);
const idm341: Map<number, (L: Lexer) => void> = new Map();
idm341.set(56/* $ */, _340id1);
idm341.set(55/* - */, _340id1);
idm341.set(50/* * */, _340id1);
idm341.set(51/* | */, _340id1);
idm341.set(41/* # */, _340id1);
idm341.set(42/* . */, _340id1);
idm341.set(43/* [ */, _340id1);
idm341.set(28/* : */, _340id1);
idm341.set(14/* @ */, _341id8);
const idm368r: Map<number, (L: Lexer) => void> = new Map();
idm368r.set(56/* $ */, _368id0);
idm368r.set(55/* - */, _368id0);
idm368r.set(50/* * */, _368id0);
idm368r.set(51/* | */, _368id0);
idm368r.set(41/* # */, _368id0);
idm368r.set(42/* . */, _368id0);
idm368r.set(43/* [ */, _368id0);
idm368r.set(28/* : */, _368id0);
idm368r.set(14/* @ */, _368id0);
const idm481r: Map<number, (L: Lexer) => void> = new Map();
idm481r.set(56/* $ */, _481id0);
idm481r.set(55/* - */, _481id0);
idm481r.set(50/* * */, _481id0);
idm481r.set(51/* | */, _481id0);
idm481r.set(41/* # */, _481id0);
idm481r.set(42/* . */, _481id0);
idm481r.set(43/* [ */, _481id0);
idm481r.set(28/* : */, _481id0);
idm481r.set(14/* @ */, _481id0);
const idm147r: Map<number, (L: Lexer) => void> = new Map();
idm147r.set(56/* $ */, _147id0);
idm147r.set(55/* - */, _147id0);
idm147r.set(50/* * */, _147id0);
idm147r.set(51/* | */, _147id0);
idm147r.set(41/* # */, _147id0);
idm147r.set(42/* . */, _147id0);
idm147r.set(43/* [ */, _147id0);
idm147r.set(28/* : */, _147id0);
idm147r.set(14/* @ */, _147id0);
const idm145r: Map<number, (L: Lexer) => void> = new Map();
idm145r.set(56/* $ */, _145id0);
idm145r.set(55/* - */, _145id0);
idm145r.set(50/* * */, _145id0);
idm145r.set(51/* | */, _145id0);
idm145r.set(41/* # */, _145id0);
idm145r.set(42/* . */, _145id0);
idm145r.set(43/* [ */, _145id0);
idm145r.set(28/* : */, _145id0);
idm145r.set(14/* @ */, _145id0);
const idm345r: Map<number, (L: Lexer) => void> = new Map();
idm345r.set(56/* $ */, _345id0);
idm345r.set(55/* - */, _345id0);
idm345r.set(50/* * */, _345id0);
idm345r.set(51/* | */, _345id0);
idm345r.set(41/* # */, _345id0);
idm345r.set(42/* . */, _345id0);
idm345r.set(43/* [ */, _345id0);
idm345r.set(28/* : */, _345id0);
idm345r.set(14/* @ */, _345id0);
const idm2r: Map<number, (L: Lexer) => void> = new Map();
idm2r.set(14/* @ */, _2id0);
idm2r.set(56/* $ */, _2id0);
idm2r.set(55/* - */, _2id0);
idm2r.set(50/* * */, _2id0);
idm2r.set(51/* | */, _2id0);
idm2r.set(41/* # */, _2id0);
idm2r.set(42/* . */, _2id0);
idm2r.set(43/* [ */, _2id0);
idm2r.set(28/* : */, _2id0);
const idm249r: Map<number, (L: Lexer) => void> = new Map();
idm249r.set(14/* @ */, _249id0);
idm249r.set(56/* $ */, _249id0);
idm249r.set(55/* - */, _249id0);
idm249r.set(50/* * */, _249id0);
idm249r.set(51/* | */, _249id0);
idm249r.set(41/* # */, _249id0);
idm249r.set(42/* . */, _249id0);
idm249r.set(43/* [ */, _249id0);
idm249r.set(28/* : */, _249id0);
const idm166: Map<number, (L: Lexer) => void> = new Map();
idm166.set(56/* $ */, _166id0);
idm166.set(55/* - */, _166id0);
idm166.set(50/* * */, _166id0);
idm166.set(51/* | */, _166id0);
idm166.set(41/* # */, _166id0);
idm166.set(42/* . */, _166id0);
idm166.set(43/* [ */, _166id0);
idm166.set(28/* : */, _166id0);
const idm133: Map<number, (L: Lexer) => void> = new Map();
idm133.set(11/* { */, _133id0);
idm133.set(10/* , */, _133id1);
const idm337: Map<number, (L: Lexer) => void> = new Map();
idm337.set(12/* ; */, _337id0);
idm337.set(56/* $ */, _337id1);
idm337.set(55/* - */, _337id1);
idm337.set(13/* } */, _337id3);
const idm315: Map<number, (L: Lexer) => void> = new Map();
idm315.set(56/* $ */, _315id0);
idm315.set(55/* - */, _315id0);
idm315.set(50/* * */, _315id0);
idm315.set(51/* | */, _315id0);
idm315.set(41/* # */, _315id0);
idm315.set(42/* . */, _315id0);
idm315.set(43/* [ */, _315id0);
idm315.set(28/* : */, _315id0);
const idm438r: Map<number, (L: Lexer) => void> = new Map();
idm438r.set(56/* $ */, _438id0);
idm438r.set(55/* - */, _438id0);
idm438r.set(50/* * */, _438id0);
idm438r.set(51/* | */, _438id0);
idm438r.set(41/* # */, _438id0);
idm438r.set(42/* . */, _438id0);
idm438r.set(43/* [ */, _438id0);
idm438r.set(28/* : */, _438id0);
idm438r.set(14/* @ */, _438id0);
idm438r.set(13/* } */, _438id0);
const idm436: Map<number, (L: Lexer) => void> = new Map();
idm436.set(12/* ; */, _436id0);
idm436.set(13/* } */, _436id1);
const idm472r: Map<number, (L: Lexer) => void> = new Map();
idm472r.set(56/* $ */, _472id0);
idm472r.set(55/* - */, _472id0);
idm472r.set(50/* * */, _472id0);
idm472r.set(51/* | */, _472id0);
idm472r.set(41/* # */, _472id0);
idm472r.set(42/* . */, _472id0);
idm472r.set(43/* [ */, _472id0);
idm472r.set(28/* : */, _472id0);
idm472r.set(14/* @ */, _472id0);
idm472r.set(13/* } */, _472id0);
const idm471r: Map<number, (L: Lexer) => void> = new Map();
idm471r.set(56/* $ */, _471id0);
idm471r.set(55/* - */, _471id0);
idm471r.set(50/* * */, _471id0);
idm471r.set(51/* | */, _471id0);
idm471r.set(41/* # */, _471id0);
idm471r.set(42/* . */, _471id0);
idm471r.set(43/* [ */, _471id0);
idm471r.set(28/* : */, _471id0);
idm471r.set(14/* @ */, _471id0);
idm471r.set(13/* } */, _471id0);
const idm491r: Map<number, (L: Lexer) => void> = new Map();
idm491r.set(56/* $ */, _491id0);
idm491r.set(55/* - */, _491id0);
idm491r.set(50/* * */, _491id0);
idm491r.set(51/* | */, _491id0);
idm491r.set(41/* # */, _491id0);
idm491r.set(42/* . */, _491id0);
idm491r.set(43/* [ */, _491id0);
idm491r.set(28/* : */, _491id0);
idm491r.set(14/* @ */, _491id0);
idm491r.set(13/* } */, _491id0);
const idm59r: Map<number, (L: Lexer) => void> = new Map();
idm59r.set(10/* , */, _59id0);
idm59r.set(12/* ; */, _59id0);
idm59r.set(14/* @ */, _59id0);
idm59r.set(56/* $ */, _59id0);
idm59r.set(55/* - */, _59id0);
idm59r.set(50/* * */, _59id0);
idm59r.set(51/* | */, _59id0);
idm59r.set(41/* # */, _59id0);
idm59r.set(42/* . */, _59id0);
idm59r.set(43/* [ */, _59id0);
idm59r.set(28/* : */, _59id0);
const idm410r: Map<number, (L: Lexer) => void> = new Map();
idm410r.set(10/* , */, _410id0);
idm410r.set(12/* ; */, _410id0);
idm410r.set(14/* @ */, _410id0);
idm410r.set(56/* $ */, _410id0);
idm410r.set(55/* - */, _410id0);
idm410r.set(50/* * */, _410id0);
idm410r.set(51/* | */, _410id0);
idm410r.set(41/* # */, _410id0);
idm410r.set(42/* . */, _410id0);
idm410r.set(43/* [ */, _410id0);
idm410r.set(28/* : */, _410id0);
const idm115: Map<number, (L: Lexer) => void> = new Map();
idm115.set(22/* not */, _115id0);
idm115.set(17/* ( */, _115id1);
idm115.set(56/* $ */, _115id2);
idm115.set(55/* - */, _115id2);
idm115.set(26/* only */, _115id4);
const idm44r: Map<number, (L: Lexer) => void> = new Map();
idm44r.set(10/* , */, _44id0);
idm44r.set(12/* ; */, _44id0);
idm44r.set(14/* @ */, _44id0);
idm44r.set(56/* $ */, _44id0);
idm44r.set(55/* - */, _44id0);
idm44r.set(50/* * */, _44id0);
idm44r.set(51/* | */, _44id0);
idm44r.set(41/* # */, _44id0);
idm44r.set(42/* . */, _44id0);
idm44r.set(43/* [ */, _44id0);
idm44r.set(28/* : */, _44id0);
idm44r.set(11/* { */, _44id0);
const idm282r: Map<number, (L: Lexer) => void> = new Map();
idm282r.set(10/* , */, _282id0);
idm282r.set(12/* ; */, _282id0);
idm282r.set(14/* @ */, _282id0);
idm282r.set(56/* $ */, _282id0);
idm282r.set(55/* - */, _282id0);
idm282r.set(50/* * */, _282id0);
idm282r.set(51/* | */, _282id0);
idm282r.set(41/* # */, _282id0);
idm282r.set(42/* . */, _282id0);
idm282r.set(43/* [ */, _282id0);
idm282r.set(28/* : */, _282id0);
idm282r.set(11/* { */, _282id0);
const idm292r: Map<number, (L: Lexer) => void> = new Map();
idm292r.set(10/* , */, _292id0);
idm292r.set(11/* { */, _292id0);
idm292r.set(12/* ; */, _292id0);
idm292r.set(14/* @ */, _292id0);
idm292r.set(56/* $ */, _292id0);
idm292r.set(55/* - */, _292id0);
idm292r.set(50/* * */, _292id0);
idm292r.set(51/* | */, _292id0);
idm292r.set(41/* # */, _292id0);
idm292r.set(42/* . */, _292id0);
idm292r.set(43/* [ */, _292id0);
idm292r.set(28/* : */, _292id0);
const idm407r: Map<number, (L: Lexer) => void> = new Map();
idm407r.set(10/* , */, _407id0);
idm407r.set(11/* { */, _407id0);
idm407r.set(12/* ; */, _407id0);
idm407r.set(14/* @ */, _407id0);
idm407r.set(56/* $ */, _407id0);
idm407r.set(55/* - */, _407id0);
idm407r.set(50/* * */, _407id0);
idm407r.set(51/* | */, _407id0);
idm407r.set(41/* # */, _407id0);
idm407r.set(42/* . */, _407id0);
idm407r.set(43/* [ */, _407id0);
idm407r.set(28/* : */, _407id0);
const idm20: Map<number, (L: Lexer) => void> = new Map();
idm20.set(22/* not */, _20id0);
idm20.set(17/* ( */, _20id0);
idm20.set(56/* $ */, _20id2);
idm20.set(55/* - */, _20id2);
const tym20: Map<number, (L: Lexer) => void> = new Map();
tym20.set(6/* \selector */, _20id0);
tym20.set(3/* \id */, _20id2);
tym20.set(7/* \key */, _20id2);
const idm209: Map<number, (L: Lexer) => void> = new Map();
idm209.set(31/* > */, _209id0);
idm209.set(38/* + */, _209id0);
idm209.set(39/* ~ */, _209id0);
idm209.set(40/* || */, _209id0);
idm209.set(56/* $ */, _209id0);
idm209.set(55/* - */, _209id0);
idm209.set(50/* * */, _209id0);
idm209.set(51/* | */, _209id0);
idm209.set(41/* # */, _209id0);
idm209.set(42/* . */, _209id0);
idm209.set(43/* [ */, _209id0);
idm209.set(28/* : */, _209id0);
const idm211r: Map<number, (L: Lexer) => void> = new Map();
idm211r.set(31/* > */, _211id0);
idm211r.set(38/* + */, _211id0);
idm211r.set(39/* ~ */, _211id0);
idm211r.set(40/* || */, _211id0);
idm211r.set(56/* $ */, _211id0);
idm211r.set(55/* - */, _211id0);
idm211r.set(50/* * */, _211id0);
idm211r.set(51/* | */, _211id0);
idm211r.set(41/* # */, _211id0);
idm211r.set(42/* . */, _211id0);
idm211r.set(43/* [ */, _211id0);
idm211r.set(28/* : */, _211id0);
idm211r.set(18/* ) */, _211id0);
idm211r.set(10/* , */, _211id0);
idm211r.set(11/* { */, _211id0);
const idm390r: Map<number, (L: Lexer) => void> = new Map();
idm390r.set(31/* > */, _390id0);
idm390r.set(38/* + */, _390id0);
idm390r.set(39/* ~ */, _390id0);
idm390r.set(40/* || */, _390id0);
idm390r.set(56/* $ */, _390id0);
idm390r.set(55/* - */, _390id0);
idm390r.set(50/* * */, _390id0);
idm390r.set(51/* | */, _390id0);
idm390r.set(41/* # */, _390id0);
idm390r.set(42/* . */, _390id0);
idm390r.set(43/* [ */, _390id0);
idm390r.set(28/* : */, _390id0);
idm390r.set(18/* ) */, _390id0);
idm390r.set(10/* , */, _390id0);
idm390r.set(11/* { */, _390id0);
const idm156: Map<number, (L: Lexer) => void> = new Map();
idm156.set(56/* $ */, _156id0);
idm156.set(55/* - */, _156id0);
idm156.set(50/* * */, _156id0);
idm156.set(51/* | */, _156id0);
idm156.set(41/* # */, _156id0);
idm156.set(42/* . */, _156id0);
idm156.set(43/* [ */, _156id0);
idm156.set(28/* : */, _156id0);
const idm158r: Map<number, (L: Lexer) => void> = new Map();
idm158r.set(56/* $ */, _158id0);
idm158r.set(55/* - */, _158id0);
idm158r.set(50/* * */, _158id0);
idm158r.set(51/* | */, _158id0);
idm158r.set(41/* # */, _158id0);
idm158r.set(42/* . */, _158id0);
idm158r.set(43/* [ */, _158id0);
idm158r.set(28/* : */, _158id0);
idm158r.set(13/* } */, _158id0);
const idm369r: Map<number, (L: Lexer) => void> = new Map();
idm369r.set(56/* $ */, _369id0);
idm369r.set(55/* - */, _369id0);
idm369r.set(50/* * */, _369id0);
idm369r.set(51/* | */, _369id0);
idm369r.set(41/* # */, _369id0);
idm369r.set(42/* . */, _369id0);
idm369r.set(43/* [ */, _369id0);
idm369r.set(28/* : */, _369id0);
idm369r.set(13/* } */, _369id0);
const idm508: Map<number, (L: Lexer) => void> = new Map();
idm508.set(22/* not */, _508id0);
idm508.set(17/* ( */, _508id1);
idm508.set(56/* $ */, _508id1);
idm508.set(55/* - */, _508id1);
const idm46r: Map<number, (L: Lexer) => void> = new Map();
idm46r.set(10/* , */, _46id0);
idm46r.set(12/* ; */, _46id0);
idm46r.set(14/* @ */, _46id0);
idm46r.set(56/* $ */, _46id0);
idm46r.set(55/* - */, _46id0);
idm46r.set(50/* * */, _46id0);
idm46r.set(51/* | */, _46id0);
idm46r.set(41/* # */, _46id0);
idm46r.set(42/* . */, _46id0);
idm46r.set(43/* [ */, _46id0);
idm46r.set(28/* : */, _46id0);
idm46r.set(11/* { */, _46id0);
idm46r.set(18/* ) */, _46id0);
const idm159: Map<number, (L: Lexer) => void> = new Map();
idm159.set(56/* $ */, _159id0);
idm159.set(55/* - */, _159id0);
idm159.set(50/* * */, _159id2);
idm159.set(51/* | */, _159id0);
const idm92r: Map<number, (L: Lexer) => void> = new Map();
idm92r.set(41/* # */, _92id0);
idm92r.set(42/* . */, _92id0);
idm92r.set(43/* [ */, _92id0);
idm92r.set(28/* : */, _92id0);
idm92r.set(51/* | */, _92id4);
idm92r.set(31/* > */, _92id0);
idm92r.set(38/* + */, _92id0);
idm92r.set(39/* ~ */, _92id0);
idm92r.set(40/* || */, _92id0);
idm92r.set(56/* $ */, _92id0);
idm92r.set(55/* - */, _92id0);
idm92r.set(50/* * */, _92id0);
idm92r.set(10/* , */, _92id0);
idm92r.set(11/* { */, _92id0);
idm92r.set(18/* ) */, _92id0);
const idm90r: Map<number, (L: Lexer) => void> = new Map();
idm90r.set(41/* # */, _90id0);
idm90r.set(42/* . */, _90id0);
idm90r.set(43/* [ */, _90id0);
idm90r.set(28/* : */, _90id0);
idm90r.set(31/* > */, _90id0);
idm90r.set(38/* + */, _90id0);
idm90r.set(39/* ~ */, _90id0);
idm90r.set(40/* || */, _90id0);
idm90r.set(56/* $ */, _90id0);
idm90r.set(55/* - */, _90id0);
idm90r.set(50/* * */, _90id0);
idm90r.set(51/* | */, _90id0);
idm90r.set(10/* , */, _90id0);
idm90r.set(11/* { */, _90id0);
idm90r.set(18/* ) */, _90id0);
const idm91: Map<number, (L: Lexer) => void> = new Map();
idm91.set(56/* $ */, _91id0);
idm91.set(55/* - */, _91id0);
idm91.set(50/* * */, _91id2);
const idm327r: Map<number, (L: Lexer) => void> = new Map();
idm327r.set(41/* # */, _327id0);
idm327r.set(42/* . */, _327id0);
idm327r.set(43/* [ */, _327id0);
idm327r.set(28/* : */, _327id0);
idm327r.set(31/* > */, _327id0);
idm327r.set(38/* + */, _327id0);
idm327r.set(39/* ~ */, _327id0);
idm327r.set(40/* || */, _327id0);
idm327r.set(56/* $ */, _327id0);
idm327r.set(55/* - */, _327id0);
idm327r.set(50/* * */, _327id0);
idm327r.set(51/* | */, _327id0);
idm327r.set(10/* , */, _327id0);
idm327r.set(11/* { */, _327id0);
idm327r.set(18/* ) */, _327id0);
const idm328r: Map<number, (L: Lexer) => void> = new Map();
idm328r.set(41/* # */, _328id0);
idm328r.set(42/* . */, _328id0);
idm328r.set(43/* [ */, _328id0);
idm328r.set(28/* : */, _328id0);
idm328r.set(31/* > */, _328id0);
idm328r.set(38/* + */, _328id0);
idm328r.set(39/* ~ */, _328id0);
idm328r.set(40/* || */, _328id0);
idm328r.set(56/* $ */, _328id0);
idm328r.set(55/* - */, _328id0);
idm328r.set(50/* * */, _328id0);
idm328r.set(51/* | */, _328id0);
idm328r.set(10/* , */, _328id0);
idm328r.set(11/* { */, _328id0);
idm328r.set(18/* ) */, _328id0);
idm328r.set(44/* ] */, _328id0);
idm328r.set(45/* ^= */, _328id0);
idm328r.set(46/* $= */, _328id0);
idm328r.set(47/* *= */, _328id0);
idm328r.set(30/* = */, _328id0);
const idm96r: Map<number, (L: Lexer) => void> = new Map();
idm96r.set(28/* : */, _96id0);
idm96r.set(41/* # */, _96id0);
idm96r.set(42/* . */, _96id0);
idm96r.set(43/* [ */, _96id0);
idm96r.set(31/* > */, _96id0);
idm96r.set(38/* + */, _96id0);
idm96r.set(39/* ~ */, _96id0);
idm96r.set(40/* || */, _96id0);
idm96r.set(56/* $ */, _96id0);
idm96r.set(55/* - */, _96id0);
idm96r.set(50/* * */, _96id0);
idm96r.set(51/* | */, _96id0);
idm96r.set(10/* , */, _96id0);
idm96r.set(11/* { */, _96id0);
idm96r.set(18/* ) */, _96id0);
const idm318r: Map<number, (L: Lexer) => void> = new Map();
idm318r.set(28/* : */, _318id0);
idm318r.set(41/* # */, _318id0);
idm318r.set(42/* . */, _318id0);
idm318r.set(43/* [ */, _318id0);
idm318r.set(31/* > */, _318id0);
idm318r.set(38/* + */, _318id0);
idm318r.set(39/* ~ */, _318id0);
idm318r.set(40/* || */, _318id0);
idm318r.set(56/* $ */, _318id0);
idm318r.set(55/* - */, _318id0);
idm318r.set(50/* * */, _318id0);
idm318r.set(51/* | */, _318id0);
idm318r.set(10/* , */, _318id0);
idm318r.set(11/* { */, _318id0);
idm318r.set(18/* ) */, _318id0);
const idm105r: Map<number, (L: Lexer) => void> = new Map();
idm105r.set(28/* : */, _105id0);
idm105r.set(31/* > */, _105id0);
idm105r.set(38/* + */, _105id0);
idm105r.set(39/* ~ */, _105id0);
idm105r.set(40/* || */, _105id0);
idm105r.set(56/* $ */, _105id0);
idm105r.set(55/* - */, _105id0);
idm105r.set(50/* * */, _105id0);
idm105r.set(51/* | */, _105id0);
idm105r.set(41/* # */, _105id0);
idm105r.set(42/* . */, _105id0);
idm105r.set(43/* [ */, _105id0);
idm105r.set(10/* , */, _105id0);
idm105r.set(11/* { */, _105id0);
idm105r.set(18/* ) */, _105id0);
const idm320r: Map<number, (L: Lexer) => void> = new Map();
idm320r.set(28/* : */, _320id0);
idm320r.set(31/* > */, _320id0);
idm320r.set(38/* + */, _320id0);
idm320r.set(39/* ~ */, _320id0);
idm320r.set(40/* || */, _320id0);
idm320r.set(56/* $ */, _320id0);
idm320r.set(55/* - */, _320id0);
idm320r.set(50/* * */, _320id0);
idm320r.set(51/* | */, _320id0);
idm320r.set(41/* # */, _320id0);
idm320r.set(42/* . */, _320id0);
idm320r.set(43/* [ */, _320id0);
idm320r.set(10/* , */, _320id0);
idm320r.set(11/* { */, _320id0);
idm320r.set(18/* ) */, _320id0);
const idm352: Map<number, (L: Lexer) => void> = new Map();
idm352.set(22/* not */, _352id0);
idm352.set(17/* ( */, _352id1);
idm352.set(56/* $ */, _352id1);
idm352.set(55/* - */, _352id1);
const idm353r: Map<number, (L: Lexer) => void> = new Map();
idm353r.set(10/* , */, _353id0);
idm353r.set(12/* ; */, _353id0);
idm353r.set(14/* @ */, _353id0);
idm353r.set(56/* $ */, _353id0);
idm353r.set(55/* - */, _353id0);
idm353r.set(50/* * */, _353id0);
idm353r.set(51/* | */, _353id0);
idm353r.set(41/* # */, _353id0);
idm353r.set(42/* . */, _353id0);
idm353r.set(43/* [ */, _353id0);
idm353r.set(28/* : */, _353id0);
idm353r.set(11/* { */, _353id0);
idm353r.set(18/* ) */, _353id0);
const idm284r: Map<number, (L: Lexer) => void> = new Map();
idm284r.set(10/* , */, _284id0);
idm284r.set(12/* ; */, _284id0);
idm284r.set(14/* @ */, _284id0);
idm284r.set(56/* $ */, _284id0);
idm284r.set(55/* - */, _284id0);
idm284r.set(50/* * */, _284id0);
idm284r.set(51/* | */, _284id0);
idm284r.set(41/* # */, _284id0);
idm284r.set(42/* . */, _284id0);
idm284r.set(43/* [ */, _284id0);
idm284r.set(28/* : */, _284id0);
idm284r.set(11/* { */, _284id0);
idm284r.set(18/* ) */, _284id0);
const idm301r: Map<number, (L: Lexer) => void> = new Map();
idm301r.set(23/* and */, _301id0);
idm301r.set(10/* , */, _301id0);
idm301r.set(12/* ; */, _301id0);
idm301r.set(14/* @ */, _301id0);
idm301r.set(56/* $ */, _301id0);
idm301r.set(55/* - */, _301id0);
idm301r.set(50/* * */, _301id0);
idm301r.set(51/* | */, _301id0);
idm301r.set(41/* # */, _301id0);
idm301r.set(42/* . */, _301id0);
idm301r.set(43/* [ */, _301id0);
idm301r.set(28/* : */, _301id0);
idm301r.set(11/* { */, _301id0);
idm301r.set(18/* ) */, _301id0);
const idm179: Map<number, (L: Lexer) => void> = new Map();
idm179.set(56/* $ */, _179id0);
idm179.set(55/* - */, _179id0);
idm179.set(50/* * */, _179id2);
idm179.set(51/* | */, _179id2);
const idm93r: Map<number, (L: Lexer) => void> = new Map();
idm93r.set(41/* # */, _93id0);
idm93r.set(51/* | */, _93id1);
idm93r.set(42/* . */, _93id0);
idm93r.set(43/* [ */, _93id0);
idm93r.set(28/* : */, _93id0);
idm93r.set(31/* > */, _93id0);
idm93r.set(38/* + */, _93id0);
idm93r.set(39/* ~ */, _93id0);
idm93r.set(40/* || */, _93id0);
idm93r.set(56/* $ */, _93id0);
idm93r.set(55/* - */, _93id0);
idm93r.set(50/* * */, _93id0);
idm93r.set(10/* , */, _93id0);
idm93r.set(11/* { */, _93id0);
idm93r.set(18/* ) */, _93id0);
idm93r.set(44/* ] */, _93id0);
idm93r.set(45/* ^= */, _93id0);
idm93r.set(46/* $= */, _93id0);
idm93r.set(47/* *= */, _93id0);
idm93r.set(30/* = */, _93id0);
const idm65r: Map<number, (L: Lexer) => void> = new Map();
idm65r.set(23/* and */, _65id0);
idm65r.set(10/* , */, _65id0);
idm65r.set(12/* ; */, _65id0);
idm65r.set(14/* @ */, _65id0);
idm65r.set(56/* $ */, _65id0);
idm65r.set(55/* - */, _65id0);
idm65r.set(50/* * */, _65id0);
idm65r.set(51/* | */, _65id0);
idm65r.set(41/* # */, _65id0);
idm65r.set(42/* . */, _65id0);
idm65r.set(43/* [ */, _65id0);
idm65r.set(28/* : */, _65id0);
idm65r.set(11/* { */, _65id0);
idm65r.set(18/* ) */, _65id0);
const idm69r: Map<number, (L: Lexer) => void> = new Map();
idm69r.set(24/* or */, _69id0);
idm69r.set(18/* ) */, _69id0);
idm69r.set(10/* , */, _69id0);
idm69r.set(12/* ; */, _69id0);
idm69r.set(14/* @ */, _69id0);
idm69r.set(56/* $ */, _69id0);
idm69r.set(55/* - */, _69id0);
idm69r.set(50/* * */, _69id0);
idm69r.set(51/* | */, _69id0);
idm69r.set(41/* # */, _69id0);
idm69r.set(42/* . */, _69id0);
idm69r.set(43/* [ */, _69id0);
idm69r.set(28/* : */, _69id0);
idm69r.set(11/* { */, _69id0);
const idm300r: Map<number, (L: Lexer) => void> = new Map();
idm300r.set(24/* or */, _300id0);
idm300r.set(18/* ) */, _300id0);
idm300r.set(10/* , */, _300id0);
idm300r.set(12/* ; */, _300id0);
idm300r.set(14/* @ */, _300id0);
idm300r.set(56/* $ */, _300id0);
idm300r.set(55/* - */, _300id0);
idm300r.set(50/* * */, _300id0);
idm300r.set(51/* | */, _300id0);
idm300r.set(41/* # */, _300id0);
idm300r.set(42/* . */, _300id0);
idm300r.set(43/* [ */, _300id0);
idm300r.set(28/* : */, _300id0);
idm300r.set(11/* { */, _300id0);
const idm204: Map<number, (L: Lexer) => void> = new Map();
idm204.set(56/* $ */, _204id0);
idm204.set(55/* - */, _204id1);
const tym204: Map<number, (L: Lexer) => void> = new Map();
tym204.set(3/* \id */, _204ty0);
tym204.set(7/* \key */, _204ty1);
const idm33r: Map<number, (L: Lexer) => void> = new Map();
idm33r.set(54/* _ */, _33id0);
idm33r.set(55/* - */, _33id0);
idm33r.set(56/* $ */, _33id0);
idm33r.set(17/* ( */, _33id0);
idm33r.set(28/* : */, _33id0);
idm33r.set(23/* and */, _33id0);
idm33r.set(10/* , */, _33id0);
idm33r.set(12/* ; */, _33id0);
idm33r.set(14/* @ */, _33id0);
idm33r.set(50/* * */, _33id0);
idm33r.set(51/* | */, _33id0);
idm33r.set(41/* # */, _33id0);
idm33r.set(42/* . */, _33id0);
idm33r.set(43/* [ */, _33id0);
idm33r.set(11/* { */, _33id0);
idm33r.set(29/* < */, _33id0);
idm33r.set(31/* > */, _33id0);
idm33r.set(30/* = */, _33id0);
idm33r.set(38/* + */, _33id0);
idm33r.set(39/* ~ */, _33id0);
idm33r.set(40/* || */, _33id0);
idm33r.set(18/* ) */, _33id0);
idm33r.set(44/* ] */, _33id0);
idm33r.set(45/* ^= */, _33id0);
idm33r.set(46/* $= */, _33id0);
idm33r.set(47/* *= */, _33id0);
idm33r.set(48/* i */, _33id0);
idm33r.set(49/* s */, _33id0);
const idm205: Map<number, (L: Lexer) => void> = new Map();
idm205.set(54/* _ */, _205id0);
idm205.set(55/* - */, _205id1);
idm205.set(56/* $ */, _205id2);
const tym205: Map<number, (L: Lexer) => void> = new Map();
tym205.set(3/* \id */, _205ty0);
tym205.set(7/* \key */, _205ty1);
tym205.set(2/* \num */, _205ty2);
const idm269r: Map<number, (L: Lexer) => void> = new Map();
idm269r.set(54/* _ */, _269id0);
idm269r.set(55/* - */, _269id0);
idm269r.set(56/* $ */, _269id0);
idm269r.set(17/* ( */, _269id0);
idm269r.set(28/* : */, _269id0);
idm269r.set(23/* and */, _269id0);
idm269r.set(10/* , */, _269id0);
idm269r.set(12/* ; */, _269id0);
idm269r.set(14/* @ */, _269id0);
idm269r.set(50/* * */, _269id0);
idm269r.set(51/* | */, _269id0);
idm269r.set(41/* # */, _269id0);
idm269r.set(42/* . */, _269id0);
idm269r.set(43/* [ */, _269id0);
idm269r.set(11/* { */, _269id0);
idm269r.set(29/* < */, _269id0);
idm269r.set(31/* > */, _269id0);
idm269r.set(30/* = */, _269id0);
idm269r.set(38/* + */, _269id0);
idm269r.set(39/* ~ */, _269id0);
idm269r.set(40/* || */, _269id0);
idm269r.set(18/* ) */, _269id0);
idm269r.set(44/* ] */, _269id0);
idm269r.set(45/* ^= */, _269id0);
idm269r.set(46/* $= */, _269id0);
idm269r.set(47/* *= */, _269id0);
idm269r.set(48/* i */, _269id0);
idm269r.set(49/* s */, _269id0);
const idm448: Map<number, (L: Lexer) => void> = new Map();
idm448.set(18/* ) */, _448id0);
idm448.set(17/* ( */, _448id1);
const idm139r: Map<number, (L: Lexer) => void> = new Map();
idm139r.set(28/* : */, _139id0);
idm139r.set(31/* > */, _139id0);
idm139r.set(38/* + */, _139id0);
idm139r.set(39/* ~ */, _139id0);
idm139r.set(40/* || */, _139id0);
idm139r.set(56/* $ */, _139id0);
idm139r.set(55/* - */, _139id0);
idm139r.set(50/* * */, _139id0);
idm139r.set(51/* | */, _139id0);
idm139r.set(41/* # */, _139id0);
idm139r.set(42/* . */, _139id0);
idm139r.set(43/* [ */, _139id0);
idm139r.set(18/* ) */, _139id0);
idm139r.set(10/* , */, _139id0);
idm139r.set(11/* { */, _139id0);
const idm339r: Map<number, (L: Lexer) => void> = new Map();
idm339r.set(28/* : */, _339id0);
idm339r.set(31/* > */, _339id0);
idm339r.set(38/* + */, _339id0);
idm339r.set(39/* ~ */, _339id0);
idm339r.set(40/* || */, _339id0);
idm339r.set(56/* $ */, _339id0);
idm339r.set(55/* - */, _339id0);
idm339r.set(50/* * */, _339id0);
idm339r.set(51/* | */, _339id0);
idm339r.set(41/* # */, _339id0);
idm339r.set(42/* . */, _339id0);
idm339r.set(43/* [ */, _339id0);
idm339r.set(18/* ) */, _339id0);
idm339r.set(10/* , */, _339id0);
idm339r.set(11/* { */, _339id0);
const idm71: Map<number, (L: Lexer) => void> = new Map();
idm71.set(56/* $ */, _71id0);
idm71.set(55/* - */, _71id0);
idm71.set(32/* true */, _71id2);
idm71.set(33/* false */, _71id2);
const tym71: Map<number, (L: Lexer) => void> = new Map();
tym71.set(3/* \id */, _71id0);
tym71.set(7/* \key */, _71id0);
tym71.set(2/* \num */, _71ty2);
const tym125: Map<number, (L: Lexer) => void> = new Map();
tym125.set(3/* \id */, _125ty0);
tym125.set(7/* \key */, _125ty0);
tym125.set(2/* \num */, _125ty2);
const idm79: Map<number, (L: Lexer) => void> = new Map();
idm79.set(29/* < */, _79id0);
idm79.set(31/* > */, _79id1);
idm79.set(30/* = */, _79id2);
function $CSS(l: Lexer): void {//Production Start
    /*
    CSS=>• STYLE_SHEET
    STYLE_SHEET=>• STYLE_SHEET_HC_listbody1_100 STYLE_SHEET_HC_listbody1_102
    STYLE_SHEET=>• STYLE_SHEET_HC_listbody1_102
    STYLE_SHEET=>• STYLE_SHEET_HC_listbody1_100
    STYLE_SHEET=>•
    STYLE_SHEET_HC_listbody1_100=>• STYLE_SHEET_HC_listbody1_100 import
    STYLE_SHEET_HC_listbody1_100=>• import
    import=>• @ τimport import_group_012_105 import_group_315_107 import_HC_listbody4_108
    import=>• @ τimport import_group_012_105 import_HC_listbody4_108
    import=>• @ τimport import_group_012_105 import_group_315_107
    import=>• @ τimport import_group_012_105
    STYLE_SHEET_HC_listbody1_102=>• STYLE_SHEET_HC_listbody1_102 STYLE_SHEET_group_03_101
    STYLE_SHEET_HC_listbody1_102=>• STYLE_SHEET_group_03_101
    STYLE_SHEET_group_03_101=>• STYLE_RULE
    STYLE_SHEET_group_03_101=>• AT_RULE
    STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { declaration_list ; }
    STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { ; }
    STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { declaration_list }
    STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { }
    STYLE_RULE_HC_listbody2_103=>• STYLE_RULE_HC_listbody2_103 , COMPLEX_SELECTOR
    STYLE_RULE_HC_listbody2_103=>• COMPLEX_SELECTOR
    COMPLEX_SELECTOR=>• COMPOUND_SELECTOR COMPLEX_SELECTOR_HC_listbody2_133
    COMPLEX_SELECTOR=>• COMPOUND_SELECTOR
    COMPOUND_SELECTOR=>• TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody1_134 COMPOUND_SELECTOR_HC_listbody2_137 θws
    COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody1_134 COMPOUND_SELECTOR_HC_listbody2_137 θws
    COMPOUND_SELECTOR=>• TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody2_137 θws
    COMPOUND_SELECTOR=>• TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody1_134 θws
    COMPOUND_SELECTOR=>• TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody1_134 COMPOUND_SELECTOR_HC_listbody2_137
    COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody2_137 θws
    COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody1_134 θws
    COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody1_134 COMPOUND_SELECTOR_HC_listbody2_137
    COMPOUND_SELECTOR=>• TYPE_SELECTOR θws
    COMPOUND_SELECTOR=>• TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody2_137
    COMPOUND_SELECTOR=>• TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody1_134
    COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody2_137
    COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody1_134
    COMPOUND_SELECTOR=>• TYPE_SELECTOR
    TYPE_SELECTOR=>• WQ_NAME
    TYPE_SELECTOR=>• NS_PREFIX *
    TYPE_SELECTOR=>• *
    WQ_NAME=>• NS_PREFIX identifier
    WQ_NAME=>• identifier
    NS_PREFIX=>• NS_PREFIX_group_0150_140 |
    NS_PREFIX=>• |
    NS_PREFIX_group_0150_140=>• identifier
    NS_PREFIX_group_0150_140=>• *
    identifier=>• css_id_symbols θws
    identifier=>• css_id_symbols
    css_id_symbols=>• css_id_symbols θid
    css_id_symbols=>• css_id_symbols θkey
    css_id_symbols=>• css_id_symbols _
    css_id_symbols=>• css_id_symbols -
    css_id_symbols=>• css_id_symbols $
    css_id_symbols=>• css_id_symbols θnum
    css_id_symbols=>• $
    css_id_symbols=>• θid
    css_id_symbols=>• θkey
    css_id_symbols=>• -
    COMPOUND_SELECTOR_HC_listbody1_134=>• COMPOUND_SELECTOR_HC_listbody1_134 SUBCLASS_SELECTOR
    COMPOUND_SELECTOR_HC_listbody1_134=>• SUBCLASS_SELECTOR
    SUBCLASS_SELECTOR=>• ID_SELECTOR
    SUBCLASS_SELECTOR=>• CLASS_SELECTOR
    SUBCLASS_SELECTOR=>• ATTRIBUTE_SELECTOR
    SUBCLASS_SELECTOR=>• PSEUDO_CLASS_SELECTOR
    ID_SELECTOR=>• # identifier
    CLASS_SELECTOR=>• . identifier
    ATTRIBUTE_SELECTOR=>• [ WQ_NAME ]
    ATTRIBUTE_SELECTOR=>• [ WQ_NAME ATTR_MATCHER ATTRIBUTE_SELECTOR_group_2137_139 ATTR_MODIFIER ]
    ATTRIBUTE_SELECTOR=>• [ WQ_NAME ATTR_MATCHER ATTRIBUTE_SELECTOR_group_2137_139 ]
    PSEUDO_CLASS_SELECTOR=>• : identifier PSEUDO_CLASS_SELECTOR_group_2133_138
    PSEUDO_CLASS_SELECTOR=>• : identifier
    COMPOUND_SELECTOR_HC_listbody2_137=>• COMPOUND_SELECTOR_HC_listbody2_137 COMPOUND_SELECTOR_group_1121_136
    COMPOUND_SELECTOR_HC_listbody2_137=>• COMPOUND_SELECTOR_group_1121_136
    COMPOUND_SELECTOR_group_1121_136=>• PSEUDO_ELEMENT_SELECTOR COMPOUND_SELECTOR_HC_listbody1_135
    COMPOUND_SELECTOR_group_1121_136=>• PSEUDO_ELEMENT_SELECTOR
    PSEUDO_ELEMENT_SELECTOR=>• : PSEUDO_CLASS_SELECTOR
    AT_RULE=>• media ;
    AT_RULE=>• import ;
    AT_RULE=>• keyframes ;
    AT_RULE=>• supports ;
    AT_RULE=>• media
    AT_RULE=>• import
    AT_RULE=>• keyframes
    AT_RULE=>• supports
    media=>• @ τmedia media_queries { GROUP_RULE_BODY }
    media=>• @ τmedia media_queries { }
    keyframes=>• @ τkeyframes keyframes_name { keyframes_HC_listbody4_109 }
    supports=>• @ τsupports supports_group_025_111 { GROUP_RULE_BODY }
    supports=>• @ τsupports supports_group_025_111 { }
    */
    _skip(l, const__);
    //considered syms: END_OF_ITEM,@,*,|,$,id,key,-,#,.,[,:

    //Single Production Completion

    //peek 0

    //block 0

    //groups false
    /*
    CSS=>• STYLE_SHEET
    */
    $STYLE_SHEET(l);
    if (!FAILED) {
        setProduction(0);

        return;
    }
    fail(l);
}

function $STYLE_SHEET_group_03_101(l: Lexer): void {//Production Start
    /*
    STYLE_SHEET_group_03_101=>• STYLE_RULE
    STYLE_SHEET_group_03_101=>• AT_RULE
    STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { declaration_list ; }
    STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { ; }
    STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { declaration_list }
    STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { }
    STYLE_RULE_HC_listbody2_103=>• STYLE_RULE_HC_listbody2_103 , COMPLEX_SELECTOR
    STYLE_RULE_HC_listbody2_103=>• COMPLEX_SELECTOR
    COMPLEX_SELECTOR=>• COMPOUND_SELECTOR COMPLEX_SELECTOR_HC_listbody2_133
    COMPLEX_SELECTOR=>• COMPOUND_SELECTOR
    COMPOUND_SELECTOR=>• TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody1_134 COMPOUND_SELECTOR_HC_listbody2_137 θws
    COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody1_134 COMPOUND_SELECTOR_HC_listbody2_137 θws
    COMPOUND_SELECTOR=>• TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody2_137 θws
    COMPOUND_SELECTOR=>• TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody1_134 θws
    COMPOUND_SELECTOR=>• TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody1_134 COMPOUND_SELECTOR_HC_listbody2_137
    COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody2_137 θws
    COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody1_134 θws
    COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody1_134 COMPOUND_SELECTOR_HC_listbody2_137
    COMPOUND_SELECTOR=>• TYPE_SELECTOR θws
    COMPOUND_SELECTOR=>• TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody2_137
    COMPOUND_SELECTOR=>• TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody1_134
    COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody2_137
    COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody1_134
    COMPOUND_SELECTOR=>• TYPE_SELECTOR
    TYPE_SELECTOR=>• WQ_NAME
    TYPE_SELECTOR=>• NS_PREFIX *
    TYPE_SELECTOR=>• *
    WQ_NAME=>• NS_PREFIX identifier
    WQ_NAME=>• identifier
    NS_PREFIX=>• NS_PREFIX_group_0150_140 |
    NS_PREFIX=>• |
    NS_PREFIX_group_0150_140=>• identifier
    NS_PREFIX_group_0150_140=>• *
    identifier=>• css_id_symbols θws
    identifier=>• css_id_symbols
    css_id_symbols=>• css_id_symbols θid
    css_id_symbols=>• css_id_symbols θkey
    css_id_symbols=>• css_id_symbols _
    css_id_symbols=>• css_id_symbols -
    css_id_symbols=>• css_id_symbols $
    css_id_symbols=>• css_id_symbols θnum
    css_id_symbols=>• $
    css_id_symbols=>• θid
    css_id_symbols=>• θkey
    css_id_symbols=>• -
    COMPOUND_SELECTOR_HC_listbody1_134=>• COMPOUND_SELECTOR_HC_listbody1_134 SUBCLASS_SELECTOR
    COMPOUND_SELECTOR_HC_listbody1_134=>• SUBCLASS_SELECTOR
    SUBCLASS_SELECTOR=>• ID_SELECTOR
    SUBCLASS_SELECTOR=>• CLASS_SELECTOR
    SUBCLASS_SELECTOR=>• ATTRIBUTE_SELECTOR
    SUBCLASS_SELECTOR=>• PSEUDO_CLASS_SELECTOR
    ID_SELECTOR=>• # identifier
    CLASS_SELECTOR=>• . identifier
    ATTRIBUTE_SELECTOR=>• [ WQ_NAME ]
    ATTRIBUTE_SELECTOR=>• [ WQ_NAME ATTR_MATCHER ATTRIBUTE_SELECTOR_group_2137_139 ATTR_MODIFIER ]
    ATTRIBUTE_SELECTOR=>• [ WQ_NAME ATTR_MATCHER ATTRIBUTE_SELECTOR_group_2137_139 ]
    PSEUDO_CLASS_SELECTOR=>• : identifier PSEUDO_CLASS_SELECTOR_group_2133_138
    PSEUDO_CLASS_SELECTOR=>• : identifier
    COMPOUND_SELECTOR_HC_listbody2_137=>• COMPOUND_SELECTOR_HC_listbody2_137 COMPOUND_SELECTOR_group_1121_136
    COMPOUND_SELECTOR_HC_listbody2_137=>• COMPOUND_SELECTOR_group_1121_136
    COMPOUND_SELECTOR_group_1121_136=>• PSEUDO_ELEMENT_SELECTOR COMPOUND_SELECTOR_HC_listbody1_135
    COMPOUND_SELECTOR_group_1121_136=>• PSEUDO_ELEMENT_SELECTOR
    PSEUDO_ELEMENT_SELECTOR=>• : PSEUDO_CLASS_SELECTOR
    AT_RULE=>• media ;
    AT_RULE=>• import ;
    AT_RULE=>• keyframes ;
    AT_RULE=>• supports ;
    AT_RULE=>• media
    AT_RULE=>• import
    AT_RULE=>• keyframes
    AT_RULE=>• supports
    media=>• @ τmedia media_queries { GROUP_RULE_BODY }
    media=>• @ τmedia media_queries { }
    import=>• @ τimport import_group_012_105 import_group_315_107 import_HC_listbody4_108
    import=>• @ τimport import_group_012_105 import_HC_listbody4_108
    import=>• @ τimport import_group_012_105 import_group_315_107
    import=>• @ τimport import_group_012_105
    keyframes=>• @ τkeyframes keyframes_name { keyframes_HC_listbody4_109 }
    supports=>• @ τsupports supports_group_025_111 { GROUP_RULE_BODY }
    supports=>• @ τsupports supports_group_025_111 { }
    */
    _skip(l, const__);
    if (const_0_.includes(l.id) || l.ty == 3/* \id */ || l.ty == 7/* \key */) {
        //considered syms: *,|,$,id,key,-,#,.,[,:

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        STYLE_SHEET_group_03_101=>• STYLE_RULE
        */
        $STYLE_RULE(l);
        if (!FAILED) {
            setProduction(2);

            return;
        }
    } else if (l.id == 14/* \@ */) {
        //considered syms: @

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        STYLE_SHEET_group_03_101=>• AT_RULE
        */
        $AT_RULE(l);
        if (!FAILED) {
            setProduction(2);

            return;
        }
    }
    fail(l);
}





function $AT_RULE(l: Lexer): void {//Production Start
    /*
    AT_RULE=>• media ;
    AT_RULE=>• import ;
    AT_RULE=>• keyframes ;
    AT_RULE=>• supports ;
    AT_RULE=>• media
    AT_RULE=>• import
    AT_RULE=>• keyframes
    AT_RULE=>• supports
    media=>• @ τmedia media_queries { GROUP_RULE_BODY }
    media=>• @ τmedia media_queries { }
    import=>• @ τimport import_group_012_105 import_group_315_107 import_HC_listbody4_108
    import=>• @ τimport import_group_012_105 import_HC_listbody4_108
    import=>• @ τimport import_group_012_105 import_group_315_107
    import=>• @ τimport import_group_012_105
    keyframes=>• @ τkeyframes keyframes_name { keyframes_HC_listbody4_109 }
    supports=>• @ τsupports supports_group_025_111 { GROUP_RULE_BODY }
    supports=>• @ τsupports supports_group_025_111 { }
    */
    _skip(l, const__);
    //considered syms: @

    //Look Ahead Level 1
    /*
    AT_RULE=>• media ;
    AT_RULE=>• import ;
    AT_RULE=>• keyframes ;
    AT_RULE=>• supports ;
    AT_RULE=>• media
    AT_RULE=>• import
    AT_RULE=>• keyframes
    AT_RULE=>• supports
    */
    const pk1: Lexer = _pk(l.copy(), /* e.eh, */const__);
    /*AT_RULE=>• media ; peek 1 state: 
    media=>@ • τmedia media_queries { GROUP_RULE_BODY }
    media=>@ • τmedia media_queries { }*/

    /*AT_RULE=>• import ; peek 1 state: 
    import=>@ • τimport import_group_012_105 import_group_315_107 import_HC_listbody4_108
    import=>@ • τimport import_group_012_105 import_HC_listbody4_108
    import=>@ • τimport import_group_012_105 import_group_315_107
    import=>@ • τimport import_group_012_105*/

    /*AT_RULE=>• keyframes ; peek 1 state: 
    keyframes=>@ • τkeyframes keyframes_name { keyframes_HC_listbody4_109 }*/

    /*AT_RULE=>• supports ; peek 1 state: 
    supports=>@ • τsupports supports_group_025_111 { GROUP_RULE_BODY }
    supports=>@ • τsupports supports_group_025_111 { }*/

    /*AT_RULE=>• media peek 1 state: 
    media=>@ • τmedia media_queries { GROUP_RULE_BODY }
    media=>@ • τmedia media_queries { }*/

    /*AT_RULE=>• import peek 1 state: 
    import=>@ • τimport import_group_012_105 import_group_315_107 import_HC_listbody4_108
    import=>@ • τimport import_group_012_105 import_HC_listbody4_108
    import=>@ • τimport import_group_012_105 import_group_315_107
    import=>@ • τimport import_group_012_105*/

    /*AT_RULE=>• keyframes peek 1 state: 
    keyframes=>@ • τkeyframes keyframes_name { keyframes_HC_listbody4_109 }*/

    /*AT_RULE=>• supports peek 1 state: 
    supports=>@ • τsupports supports_group_025_111 { GROUP_RULE_BODY }
    supports=>@ • τsupports supports_group_025_111 { }*/

    if (pk1.id == 25/* \media */) {
        //considered syms: media

        //Parallel Transition
        /*
        AT_RULE=>• media ;
        AT_RULE=>• media
        */
        $media(l);

        //Look Ahead Level 0
        /*
        
        */
        _skip(l, const__);
        if (l.id == 12/* \; */) {
            //considered syms: ;

            //Single Production Completion

            //peek 0

            //block 4

            //groups true
            /*
            AT_RULE=>media • ;
            */
            _no_check(l);
            if (!FAILED) {
                setProduction(9);
                add_reduce(2, 0);
                return;
            }
        } else {
            //considered syms: END_OF_ITEM

            //Single Production Completion

            //peek 0

            //block 4

            //groups true
            /*
            AT_RULE=>media •
            */
            if (!FAILED) {
                setProduction(9);

                return;
            }
        }
    } else if (pk1.id == 15/* \import */) {
        //considered syms: import

        //Parallel Transition
        /*
        AT_RULE=>• import ;
        AT_RULE=>• import
        */
        $import(l);

        //Look Ahead Level 0
        /*
        
        */
        _skip(l, const__);
        if (l.id == 12/* \; */) {
            //considered syms: ;

            //Single Production Completion

            //peek 0

            //block 4

            //groups true
            /*
            AT_RULE=>import • ;
            */
            _no_check(l);
            if (!FAILED) {
                setProduction(9);
                add_reduce(2, 0);
                return;
            }
        } else {
            //considered syms: END_OF_ITEM

            //Single Production Completion

            //peek 0

            //block 4

            //groups true
            /*
            AT_RULE=>import •
            */
            if (!FAILED) {
                setProduction(9);

                return;
            }
        }
    } else if (pk1.id == 19/* \keyframes */) {
        //considered syms: keyframes

        //Parallel Transition
        /*
        AT_RULE=>• keyframes ;
        AT_RULE=>• keyframes
        */
        $keyframes(l);

        //Look Ahead Level 0
        /*
        
        */
        _skip(l, const__);
        if (l.id == 12/* \; */) {
            //considered syms: ;

            //Single Production Completion

            //peek 0

            //block 4

            //groups true
            /*
            AT_RULE=>keyframes • ;
            */
            _no_check(l);
            if (!FAILED) {
                setProduction(9);
                add_reduce(2, 0);
                return;
            }
        } else {
            //considered syms: END_OF_ITEM

            //Single Production Completion

            //peek 0

            //block 4

            //groups true
            /*
            AT_RULE=>keyframes •
            */
            if (!FAILED) {
                setProduction(9);

                return;
            }
        }
    } else if (pk1.id == 16/* \supports */) {
        //considered syms: supports

        //Parallel Transition
        /*
        AT_RULE=>• supports ;
        AT_RULE=>• supports
        */
        $supports(l);

        //Look Ahead Level 0
        /*
        
        */
        _skip(l, const__);
        if (l.id == 12/* \; */) {
            //considered syms: ;

            //Single Production Completion

            //peek 0

            //block 4

            //groups true
            /*
            AT_RULE=>supports • ;
            */
            _no_check(l);
            if (!FAILED) {
                setProduction(9);
                add_reduce(2, 0);
                return;
            }
        } else {
            //considered syms: END_OF_ITEM

            //Single Production Completion

            //peek 0

            //block 4

            //groups true
            /*
            AT_RULE=>supports •
            */
            if (!FAILED) {
                setProduction(9);

                return;
            }
        }
    }
    fail(l);
}
function $import_group_012_105(l: Lexer): void {//Production Start
    /*
    import_group_012_105=>• string
    import_group_012_105=>• url
    string=>• " string_HC_listbody1_129 "
    string=>• ' string_HC_listbody1_130 '
    url=>• τurl ( string )
    */
    _skip(l, const__);
    if (l.id == 27/* \" */ || l.id == 37/* \' */) {
        //considered syms: ",'

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        import_group_012_105=>• string
        */
        $string(l);
        if (!FAILED) {
            setProduction(10);

            return;
        }
    } else if (l.id == 36/* \url */) {
        //considered syms: url

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        import_group_012_105=>• url
        */
        $url(l);
        if (!FAILED) {
            setProduction(10);

            return;
        }
    }
    fail(l);
}

function $import_group_315_107(l: Lexer): void {//Production Start
    /*
    import_group_315_107=>• τsupports ( import_group_014_106 )
    */
    _skip(l, const__);
    //considered syms: supports

    //Single Production Completion

    //peek 0

    //block 0

    //groups false
    /*
    import_group_315_107=>• τsupports ( import_group_014_106 )
    */
    _with_skip(l, const__, 16/* \supports */);
    if (!FAILED) {
        _with_skip(l, const__, 17/* \( */);
        if (!FAILED) {
            $import_group_014_106(l);
            if (!FAILED) {
                _with_skip(l, const__, 18/* \) */);
                if (!FAILED) {
                    setProduction(12);
                    add_reduce(4, 0);
                    return;
                }
            }
        }
    }
    fail(l);
}

function $import(l: Lexer): void {//Production Start
    /*
    import=>• @ τimport import_group_012_105 import_group_315_107 import_HC_listbody4_108
    import=>• @ τimport import_group_012_105 import_HC_listbody4_108
    import=>• @ τimport import_group_012_105 import_group_315_107
    import=>• @ τimport import_group_012_105
    */
    _skip(l, const__);
    //considered syms: @

    //Parallel Transition
    /*
    import=>• @ τimport import_group_012_105 import_group_315_107 import_HC_listbody4_108
    import=>• @ τimport import_group_012_105 import_HC_listbody4_108
    import=>• @ τimport import_group_012_105 import_group_315_107
    import=>• @ τimport import_group_012_105
    */
    _no_check(l);

    //Parallel Transition
    /*
    import=>@ • τimport import_group_012_105 import_group_315_107 import_HC_listbody4_108
    import=>@ • τimport import_group_012_105 import_HC_listbody4_108
    import=>@ • τimport import_group_012_105 import_group_315_107
    import=>@ • τimport import_group_012_105
    */
    _with_skip(l, const__, 15/* \import */);

    //Parallel Transition
    /*
    import=>@ τimport • import_group_012_105 import_group_315_107 import_HC_listbody4_108
    import=>@ τimport • import_group_012_105 import_HC_listbody4_108
    import=>@ τimport • import_group_012_105 import_group_315_107
    import=>@ τimport • import_group_012_105
    */
    $import_group_012_105(l);

    //Look Ahead Level 0
    /*
    import=>@ τimport import_group_012_105 • import_group_315_107 import_HC_listbody4_108
    import=>@ τimport import_group_012_105 • import_HC_listbody4_108
    import=>@ τimport import_group_012_105 • import_group_315_107
    */
    _skip(l, const__);
    if (l.id == 17/* \( */ || l.id == 22/* \not */ || l.id == 26/* \only */ || l.id == 55/* \- */ || l.id == 56/* \$ */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {
        //considered syms: not,(,$,id,key,-,only

        //Single Production Completion

        //peek 0

        //block 2

        //groups true
        /*
        import=>@ τimport import_group_012_105 • import_HC_listbody4_108
        */
        $import_HC_listbody4_108(l);
        if (!FAILED) {
            setProduction(14);
            add_reduce(4, 11);
            return;
        }
    } else if (l.id == 16/* \supports */) {
        //considered syms: supports

        //Parallel Transition
        /*
        import=>@ τimport import_group_012_105 • import_group_315_107 import_HC_listbody4_108
        import=>@ τimport import_group_012_105 • import_group_315_107
        */
        $import_group_315_107(l);

        //Look Ahead Level 0
        /*
        import=>@ τimport import_group_012_105 import_group_315_107 • import_HC_listbody4_108
        */
        _skip(l, const__);
        if (l.id == 17/* \( */ || l.id == 22/* \not */ || l.id == 26/* \only */ || l.id == 55/* \- */ || l.id == 56/* \$ */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {
            //considered syms: not,(,$,id,key,-,only

            //Single Production Completion

            //peek 0

            //block 4

            //groups true
            /*
            import=>@ τimport import_group_012_105 import_group_315_107 • import_HC_listbody4_108
            */
            $import_HC_listbody4_108(l);
            if (!FAILED) {
                setProduction(14);
                add_reduce(5, 10);
                return;
            }
        } else {
            //considered syms: END_OF_ITEM

            //Single Production Completion

            //peek 0

            //block 4

            //groups true
            /*
            import=>@ τimport import_group_012_105 import_group_315_107 •
            */
            if (!FAILED) {
                setProduction(14);
                add_reduce(4, 12);
                return;
            }
        }
    } else {
        //considered syms: END_OF_ITEM

        //Single Production Completion

        //peek 0

        //block 2

        //groups true
        /*
        import=>@ τimport import_group_012_105 •
        */
        if (!FAILED) {
            setProduction(14);
            add_reduce(3, 13);
            return;
        }
    }
    fail(l);
}
function $import_declaration(l: Lexer): void {//Production Start
    /*
    import_declaration=>• declaration
    declaration=>• declaration_id : declaration_values declaration_group_1155_145
    declaration=>• declaration_id : declaration_values
    declaration_id=>• css_id_symbols
    css_id_symbols=>• css_id_symbols θid
    css_id_symbols=>• css_id_symbols θkey
    css_id_symbols=>• css_id_symbols _
    css_id_symbols=>• css_id_symbols -
    css_id_symbols=>• css_id_symbols $
    css_id_symbols=>• css_id_symbols θnum
    css_id_symbols=>• $
    css_id_symbols=>• θid
    css_id_symbols=>• θkey
    css_id_symbols=>• -
    */
    _skip(l, const__);
    //considered syms: $,id,key,-

    //Single Production Completion

    //peek 0

    //block 0

    //groups false
    /*
    import_declaration=>• declaration
    */
    $declaration(l);
    if (!FAILED) {
        setProduction(15);

        return;
    }
    fail(l);
}

function $keyframes(l: Lexer): void {//Production Start
    /*
    keyframes=>• @ τkeyframes keyframes_name { keyframes_HC_listbody4_109 }
    */
    _skip(l, const__);
    //considered syms: @

    //Single Production Completion

    //peek 0

    //block 0

    //groups false
    /*
    keyframes=>• @ τkeyframes keyframes_name { keyframes_HC_listbody4_109 }
    */
    _with_skip(l, const__, 14/* \@ */);
    if (!FAILED) {
        _with_skip(l, const__, 19/* \keyframes */);
        if (!FAILED) {
            $keyframes_name(l);
            if (!FAILED) {
                _with_skip(l, const__, 11/* \{ */);
                if (!FAILED) {
                    $keyframes_HC_listbody4_109(l);
                    if (!FAILED) {
                        _with_skip(l, const__, 13/* \} */);
                        if (!FAILED) {
                            setProduction(17);
                            add_reduce(6, 14);
                            return;
                        }
                    }
                }
            }
        }
    }
    fail(l);
}
function $keyframes_name(l: Lexer): void {//Production Start
    /*
    keyframes_name=>• θid
    keyframes_name=>• string
    string=>• " string_HC_listbody1_129 "
    string=>• ' string_HC_listbody1_130 '
    */
    _skip(l, const__);
    if (l.id == 27/* \" */ || l.id == 37/* \' */) {
        //considered syms: ",'

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        keyframes_name=>• string
        */
        $string(l);
        if (!FAILED) {
            setProduction(18);

            return;
        }
    } else if (l.ty == 3/* \id */) {
        //considered syms: id

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        keyframes_name=>• θid
        */
        _no_check(l);
        if (!FAILED) {
            setProduction(18);

            return;
        }
    }
    fail(l);
}

function $keyframes_blocks(l: Lexer): void {//Production Start
    /*
    keyframes_blocks=>• keyframes_blocks_HC_listbody1_110 { declaration_list ; }
    keyframes_blocks=>• keyframes_blocks_HC_listbody1_110 { declaration_list }
    keyframes_blocks_HC_listbody1_110=>• keyframes_blocks_HC_listbody1_110 , keyframe_selector
    keyframes_blocks_HC_listbody1_110=>• keyframe_selector
    keyframe_selector=>• τfrom
    keyframe_selector=>• τto
    keyframe_selector=>• percentage
    percentage=>• θnum %
    */
    _skip(l, const__);
    //considered syms: from,to,num

    //Parallel Transition
    /*
    keyframes_blocks=>• keyframes_blocks_HC_listbody1_110 { declaration_list ; }
    keyframes_blocks=>• keyframes_blocks_HC_listbody1_110 { declaration_list }
    */
    $keyframes_blocks_HC_listbody1_110(l);

    //Parallel Transition
    /*
    keyframes_blocks=>keyframes_blocks_HC_listbody1_110 • { declaration_list ; }
    keyframes_blocks=>keyframes_blocks_HC_listbody1_110 • { declaration_list }
    */
    _with_skip(l, const__, 11/* \{ */);

    //Parallel Transition
    /*
    keyframes_blocks=>keyframes_blocks_HC_listbody1_110 { • declaration_list ; }
    keyframes_blocks=>keyframes_blocks_HC_listbody1_110 { • declaration_list }
    */
    $declaration_list(l);

    //Look Ahead Level 0
    /*
    
    */
    _skip(l, const__);
    if (l.id == 12/* \; */) {
        //considered syms: ;

        //Single Production Completion

        //peek 0

        //block 2

        //groups true
        /*
        keyframes_blocks=>keyframes_blocks_HC_listbody1_110 { declaration_list • ; }
        */
        _no_check(l);
        if (!FAILED) {
            _with_skip(l, const__, 13/* \} */);
            if (!FAILED) {
                setProduction(20);
                add_reduce(5, 15);
                return;
            }
        }
    } else if (l.id == 13/* \} */) {
        //considered syms: }

        //Single Production Completion

        //peek 0

        //block 2

        //groups true
        /*
        keyframes_blocks=>keyframes_blocks_HC_listbody1_110 { declaration_list • }
        */
        _no_check(l);
        if (!FAILED) {
            setProduction(20);
            add_reduce(4, 15);
            return;
        }
    }
    fail(l);
}
function $keyframe_selector(l: Lexer): void {//Production Start
    /*
    keyframe_selector=>• τfrom
    keyframe_selector=>• τto
    keyframe_selector=>• percentage
    percentage=>• θnum %
    */
    _skip(l, const__);
    if (l.id == 20/* \from */) {
        //considered syms: from

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        keyframe_selector=>• τfrom
        */
        _no_check(l);
        if (!FAILED) {
            setProduction(21);
            add_reduce(1, 16);
            return;
        }
    } else if (l.id == 21/* \to */) {
        //considered syms: to

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        keyframe_selector=>• τto
        */
        _no_check(l);
        if (!FAILED) {
            setProduction(21);
            add_reduce(1, 16);
            return;
        }
    } else if (l.ty == 2/* \num */) {
        //considered syms: num

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        keyframe_selector=>• percentage
        */
        $percentage(l);
        if (!FAILED) {
            setProduction(21);
            add_reduce(1, 16);
            return;
        }
    }
    fail(l);
}
function $supports_group_025_111(l: Lexer): void {//Production Start
    /*
    supports_group_025_111=>• supports_condition
    supports_condition=>• τnot supports_in_parens
    supports_condition=>• supports_in_parens supports_condition_HC_listbody2_113
    supports_condition=>• supports_in_parens
    supports_in_parens=>• ( supports_condition )
    supports_in_parens=>• supports_feature
    supports_in_parens=>• general_enclosed
    supports_feature=>• supports_feature_fn
    supports_feature=>• supports_decl
    supports_feature_fn=>• θselector ( COMPLEX_SELECTOR )
    supports_decl=>• ( declaration )
    general_enclosed=>• identifier ( general_enclosed_HC_listbody1_124 )
    general_enclosed=>• identifier ( )
    identifier=>• css_id_symbols θws
    identifier=>• css_id_symbols
    css_id_symbols=>• css_id_symbols θid
    css_id_symbols=>• css_id_symbols θkey
    css_id_symbols=>• css_id_symbols _
    css_id_symbols=>• css_id_symbols -
    css_id_symbols=>• css_id_symbols $
    css_id_symbols=>• css_id_symbols θnum
    css_id_symbols=>• $
    css_id_symbols=>• θid
    css_id_symbols=>• θkey
    css_id_symbols=>• -
    */
    _skip(l, const__);
    //considered syms: not,(,selector,$,id,key,-

    //Single Production Completion

    //peek 0

    //block 0

    //groups false
    /*
    supports_group_025_111=>• supports_condition
    */
    $supports_condition(l);
    if (!FAILED) {
        setProduction(22);
        add_reduce(1, 17);
        return;
    }
    fail(l);
}
function $supports(l: Lexer): void {//Production Start
    /*
    supports=>• @ τsupports supports_group_025_111 { GROUP_RULE_BODY }
    supports=>• @ τsupports supports_group_025_111 { }
    */
    _skip(l, const__);
    //considered syms: @

    //Parallel Transition
    /*
    supports=>• @ τsupports supports_group_025_111 { GROUP_RULE_BODY }
    supports=>• @ τsupports supports_group_025_111 { }
    */
    _no_check(l);

    //Parallel Transition
    /*
    supports=>@ • τsupports supports_group_025_111 { GROUP_RULE_BODY }
    supports=>@ • τsupports supports_group_025_111 { }
    */
    _with_skip(l, const__, 16/* \supports */);

    //Parallel Transition
    /*
    supports=>@ τsupports • supports_group_025_111 { GROUP_RULE_BODY }
    supports=>@ τsupports • supports_group_025_111 { }
    */
    $supports_group_025_111(l);

    //Parallel Transition
    /*
    supports=>@ τsupports supports_group_025_111 • { GROUP_RULE_BODY }
    supports=>@ τsupports supports_group_025_111 • { }
    */
    _with_skip(l, const__, 11/* \{ */);

    //Look Ahead Level 0
    /*
    supports=>@ τsupports supports_group_025_111 { • GROUP_RULE_BODY }
    */
    _skip(l, const__);
    if (const_0_.includes(l.id) || l.ty == 3/* \id */ || l.ty == 7/* \key */) {
        //considered syms: *,|,$,id,key,-,#,.,[,:

        //Single Production Completion

        //peek 0

        //block 2

        //groups true
        /*
        supports=>@ τsupports supports_group_025_111 { • GROUP_RULE_BODY }
        */
        $GROUP_RULE_BODY(l);
        if (!FAILED) {
            _with_skip(l, const__, 13/* \} */);
            if (!FAILED) {
                setProduction(23);
                add_reduce(6, 18);
                return;
            }
        }
    } else if (l.id == 13/* \} */) {
        //considered syms: }

        //Single Production Completion

        //peek 0

        //block 2

        //groups true
        /*
        supports=>@ τsupports supports_group_025_111 { • }
        */
        _no_check(l);
        if (!FAILED) {
            setProduction(23);
            add_reduce(5, 18);
            return;
        }
    }
    fail(l);
}
function $supports_condition_group_129_112(l: Lexer): void {//Production Start
    /*
    supports_condition_group_129_112=>• τand supports_in_parens
    supports_condition_group_129_112=>• τor supports_in_parens
    */
    _skip(l, const__);
    if (l.id == 23/* \and */) {
        //considered syms: and

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        supports_condition_group_129_112=>• τand supports_in_parens
        */
        _no_check(l);
        if (!FAILED) {
            $supports_in_parens(l);
            if (!FAILED) {
                setProduction(24);
                add_reduce(2, 19);
                return;
            }
        }
    } else if (l.id == 24/* \or */) {
        //considered syms: or

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        supports_condition_group_129_112=>• τor supports_in_parens
        */
        _no_check(l);
        if (!FAILED) {
            $supports_in_parens(l);
            if (!FAILED) {
                setProduction(24);
                add_reduce(2, 20);
                return;
            }
        }
    }
    fail(l);
}

function $supports_condition(l: Lexer): void {//Production Start
    /*
    supports_condition=>• τnot supports_in_parens
    supports_condition=>• supports_in_parens supports_condition_HC_listbody2_113
    supports_condition=>• supports_in_parens
    supports_in_parens=>• ( supports_condition )
    supports_in_parens=>• supports_feature
    supports_in_parens=>• general_enclosed
    supports_feature=>• supports_feature_fn
    supports_feature=>• supports_decl
    supports_feature_fn=>• θselector ( COMPLEX_SELECTOR )
    supports_decl=>• ( declaration )
    general_enclosed=>• identifier ( general_enclosed_HC_listbody1_124 )
    general_enclosed=>• identifier ( )
    identifier=>• css_id_symbols θws
    identifier=>• css_id_symbols
    css_id_symbols=>• css_id_symbols θid
    css_id_symbols=>• css_id_symbols θkey
    css_id_symbols=>• css_id_symbols _
    css_id_symbols=>• css_id_symbols -
    css_id_symbols=>• css_id_symbols $
    css_id_symbols=>• css_id_symbols θnum
    css_id_symbols=>• $
    css_id_symbols=>• θid
    css_id_symbols=>• θkey
    css_id_symbols=>• -
    */
    _skip(l, const__);
    if (l.id == 17/* \( */ || l.id == 55/* \- */ || l.id == 56/* \$ */ || l.ty == 3/* \id */ || l.ty == 6/* \selector */ || l.ty == 7/* \key */) {
        //considered syms: (,selector,$,id,key,-

        //Parallel Transition
        /*
        supports_condition=>• supports_in_parens supports_condition_HC_listbody2_113
        supports_condition=>• supports_in_parens
        */
        $supports_in_parens(l);

        //Look Ahead Level 0
        /*
        supports_condition=>supports_in_parens • supports_condition_HC_listbody2_113
        */
        _skip(l, const__);
        if (l.id == 23/* \and */ || l.id == 24/* \or */) {
            //considered syms: and,or

            //Single Production Completion

            //peek 0

            //block 3

            //groups true
            /*
            supports_condition=>supports_in_parens • supports_condition_HC_listbody2_113
            */
            $supports_condition_HC_listbody2_113(l);
            if (!FAILED) {
                setProduction(26);
                add_reduce(2, 22);
                return;
            }
        } else {
            //considered syms: END_OF_ITEM

            //Single Production Completion

            //peek 0

            //block 3

            //groups true
            /*
            supports_condition=>supports_in_parens •
            */
            if (!FAILED) {
                setProduction(26);
                add_reduce(1, 2);
                return;
            }
        }
    } else if (l.id == 22/* \not */) {
        //considered syms: not

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        supports_condition=>• τnot supports_in_parens
        */
        _no_check(l);
        if (!FAILED) {
            $supports_in_parens(l);
            if (!FAILED) {
                setProduction(26);
                add_reduce(2, 21);
                return;
            }
        }
    }
    fail(l);
}
function $supports_in_parens(l: Lexer): void {//Production Start
    /*
    supports_in_parens=>• ( supports_condition )
    supports_in_parens=>• supports_feature
    supports_in_parens=>• general_enclosed
    supports_feature=>• supports_feature_fn
    supports_feature=>• supports_decl
    supports_feature_fn=>• θselector ( COMPLEX_SELECTOR )
    supports_decl=>• ( declaration )
    general_enclosed=>• identifier ( general_enclosed_HC_listbody1_124 )
    general_enclosed=>• identifier ( )
    identifier=>• css_id_symbols θws
    identifier=>• css_id_symbols
    css_id_symbols=>• css_id_symbols θid
    css_id_symbols=>• css_id_symbols θkey
    css_id_symbols=>• css_id_symbols _
    css_id_symbols=>• css_id_symbols -
    css_id_symbols=>• css_id_symbols $
    css_id_symbols=>• css_id_symbols θnum
    css_id_symbols=>• $
    css_id_symbols=>• θid
    css_id_symbols=>• θkey
    css_id_symbols=>• -
    */
    _skip(l, const__);
    if (l.id == 55/* \- */ || l.id == 56/* \$ */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {
        //considered syms: $,id,key,-

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        supports_in_parens=>• general_enclosed
        */
        $general_enclosed(l);
        if (!FAILED) {
            setProduction(27);

            return;
        }
    } else if (l.id == 17/* \( */) {
        //considered syms: (

        //Look Ahead Level 1
        /*
        supports_in_parens=>• supports_feature
        */
        const pk1: Lexer = _pk(l.copy(), /* e.eh, */const__);
        /*supports_in_parens=>• supports_feature peek 1 state: 
        supports_feature_fn=>θselector • ( COMPLEX_SELECTOR )
        css_id_symbols=>• $
        css_id_symbols=>• θid
        css_id_symbols=>• θkey
        css_id_symbols=>• -*/

        /*supports_in_parens=>• ( supports_condition ) peek 1 state: 
        supports_in_parens=>• ( supports_condition )*/

        if (!FAILED && pk1.id == 55/* \- */ || pk1.id == 56/* \$ */ || pk1.ty == 3/* \id */ || pk1.ty == 7/* \key */) {
            //considered syms: $,id,key,-

            //Single Production Completion

            //peek 1

            //block 3

            //groups true
            /*
            supports_in_parens=>• supports_feature
            */
            $supports_feature(l);
            if (!FAILED) {
                setProduction(27);

                return;
            }
        } else if (!FAILED && pk1.id == 17/* \( */) {
            //considered syms: (

            //Look Ahead Level 2
            /*
            supports_in_parens=>• supports_feature
            */
            const pk2: Lexer = _pk(pk1.copy(), /* e.eh, */const__);
            /*supports_in_parens=>• supports_feature peek 2 state: 
            TYPE_SELECTOR=>• *
            NS_PREFIX=>• |
            NS_PREFIX_group_0150_140=>• *
            css_id_symbols=>• $
            css_id_symbols=>• θid
            css_id_symbols=>• θkey
            css_id_symbols=>• -
            ID_SELECTOR=>• # identifier
            CLASS_SELECTOR=>• . identifier
            ATTRIBUTE_SELECTOR=>• [ WQ_NAME ]
            ATTRIBUTE_SELECTOR=>• [ WQ_NAME ATTR_MATCHER ATTRIBUTE_SELECTOR_group_2137_139 ATTR_MODIFIER ]
            ATTRIBUTE_SELECTOR=>• [ WQ_NAME ATTR_MATCHER ATTRIBUTE_SELECTOR_group_2137_139 ]
            PSEUDO_CLASS_SELECTOR=>• : identifier PSEUDO_CLASS_SELECTOR_group_2133_138
            PSEUDO_CLASS_SELECTOR=>• : identifier
            PSEUDO_ELEMENT_SELECTOR=>• : PSEUDO_CLASS_SELECTOR
            declaration=>declaration_id • : declaration_values declaration_group_1155_145
            supports_decl=>( declaration • )*/

            /*supports_in_parens=>• ( supports_condition ) peek 2 state: 
            supports_in_parens=>• ( supports_condition )*/

            if (!FAILED && const_6_.includes(pk2.id) || pk2.ty == 3/* \id */ || pk2.ty == 7/* \key */) {
                //considered syms: *,|,$,id,key,-,#,.,[,:,)

                //Single Production Completion

                //peek 2

                //block 5

                //groups true
                /*
                supports_in_parens=>• supports_feature
                */
                $supports_feature(l);
                if (!FAILED) {
                    setProduction(27);

                    return;
                }
            } else if (!FAILED && pk2.id == 17/* \( */) {
                //considered syms: (

                //Single Production Completion

                //peek 2

                //block 5

                //groups true
                /*
                supports_in_parens=>• ( supports_condition )
                */
                _no_check(l);
                if (!FAILED) {
                    $supports_condition(l);
                    if (!FAILED) {
                        _with_skip(l, const__, 18/* \) */);
                        if (!FAILED) {
                            setProduction(27);
                            add_reduce(3, 23);
                            return;
                        }
                    }
                }
            }
        }
    } else if (l.ty == 6/* \selector */) {
        //considered syms: selector

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        supports_in_parens=>• supports_feature
        */
        $supports_feature(l);
        if (!FAILED) {
            setProduction(27);

            return;
        }
    }
    fail(l);
}
function $supports_feature(l: Lexer): void {//Production Start
    /*
    supports_feature=>• supports_feature_fn
    supports_feature=>• supports_decl
    supports_feature_fn=>• θselector ( COMPLEX_SELECTOR )
    supports_decl=>• ( declaration )
    */
    _skip(l, const__);
    if (l.id == 17/* \( */) {
        //considered syms: (

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        supports_feature=>• supports_decl
        */
        $supports_decl(l);
        if (!FAILED) {
            setProduction(28);

            return;
        }
    } else if (l.ty == 6/* \selector */) {
        //considered syms: selector

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        supports_feature=>• supports_feature_fn
        */
        $supports_feature_fn(l);
        if (!FAILED) {
            setProduction(28);

            return;
        }
    }
    fail(l);
}
function $supports_decl(l: Lexer): void {//Production Start
    /*
    supports_decl=>• ( declaration )
    */
    _skip(l, const__);
    //considered syms: (

    //Single Production Completion

    //peek 0

    //block 0

    //groups false
    /*
    supports_decl=>• ( declaration )
    */
    _with_skip(l, const__, 17/* \( */);
    if (!FAILED) {
        $declaration(l);
        if (!FAILED) {
            _with_skip(l, const__, 18/* \) */);
            if (!FAILED) {
                setProduction(29);
                add_reduce(3, 0);
                return;
            }
        }
    }
    fail(l);
}
function $supports_feature_fn(l: Lexer): void {//Production Start
    /*
    supports_feature_fn=>• θselector ( COMPLEX_SELECTOR )
    */
    _skip(l, const__);
    //considered syms: selector

    //Single Production Completion

    //peek 0

    //block 0

    //groups false
    /*
    supports_feature_fn=>• θselector ( COMPLEX_SELECTOR )
    */
    _with_skip(l, const__, 6/* \selector */);
    if (!FAILED) {
        _with_skip(l, const__, 17/* \( */);
        if (!FAILED) {
            $COMPLEX_SELECTOR(l);
            if (!FAILED) {
                _with_skip(l, const__, 18/* \) */);
                if (!FAILED) {
                    setProduction(30);
                    add_reduce(4, 24);
                    return;
                }
            }
        }
    }
    fail(l);
}
function $media(l: Lexer): void {//Production Start
    /*
    media=>• @ τmedia media_queries { GROUP_RULE_BODY }
    media=>• @ τmedia media_queries { }
    */
    _skip(l, const__);
    //considered syms: @

    //Parallel Transition
    /*
    media=>• @ τmedia media_queries { GROUP_RULE_BODY }
    media=>• @ τmedia media_queries { }
    */
    _no_check(l);

    //Parallel Transition
    /*
    media=>@ • τmedia media_queries { GROUP_RULE_BODY }
    media=>@ • τmedia media_queries { }
    */
    _with_skip(l, const__, 25/* \media */);

    //Parallel Transition
    /*
    media=>@ τmedia • media_queries { GROUP_RULE_BODY }
    media=>@ τmedia • media_queries { }
    */
    $media_queries(l);

    //Parallel Transition
    /*
    media=>@ τmedia media_queries • { GROUP_RULE_BODY }
    media=>@ τmedia media_queries • { }
    */
    _with_skip(l, const__, 11/* \{ */);

    //Look Ahead Level 0
    /*
    media=>@ τmedia media_queries { • GROUP_RULE_BODY }
    */
    _skip(l, const__);
    if (const_0_.includes(l.id) || l.ty == 3/* \id */ || l.ty == 7/* \key */) {
        //considered syms: *,|,$,id,key,-,#,.,[,:

        //Single Production Completion

        //peek 0

        //block 2

        //groups true
        /*
        media=>@ τmedia media_queries { • GROUP_RULE_BODY }
        */
        $GROUP_RULE_BODY(l);
        if (!FAILED) {
            _with_skip(l, const__, 13/* \} */);
            if (!FAILED) {
                setProduction(31);
                add_reduce(6, 25);
                return;
            }
        }
    } else if (l.id == 13/* \} */) {
        //considered syms: }

        //Single Production Completion

        //peek 0

        //block 2

        //groups true
        /*
        media=>@ τmedia media_queries { • }
        */
        _no_check(l);
        if (!FAILED) {
            setProduction(31);
            add_reduce(5, 26);
            return;
        }
    }
    fail(l);
}

function $media_queries(l: Lexer): void {//Production Start
    /*
    media_queries=>• media_queries_group_039_115
    media_queries_group_039_115=>• media_queries_group_039_115 , media_query
    media_queries_group_039_115=>• media_query
    media_query=>• media_condition
    media_query=>• media_query_group_043_116 media_type media_query_group_144_117
    media_query=>• media_type media_query_group_144_117
    media_query=>• media_query_group_043_116 media_type
    media_query=>• media_type
    media_condition=>• media_condition_without_or
    media_condition=>• media_or
    media_condition_without_or=>• media_not
    media_condition_without_or=>• media_and
    media_condition_without_or=>• media_in_parenths
    media_not=>• τnot media_in_parenths
    media_and=>• media_in_parenths media_and_HC_listbody2_119
    media_in_parenths=>• ( media_condition )
    media_in_parenths=>• media_feature
    media_in_parenths=>• general_enclosed
    media_feature=>• ( media_feature_group_061_122 )
    general_enclosed=>• identifier ( general_enclosed_HC_listbody1_124 )
    general_enclosed=>• identifier ( )
    identifier=>• css_id_symbols θws
    identifier=>• css_id_symbols
    css_id_symbols=>• css_id_symbols θid
    css_id_symbols=>• css_id_symbols θkey
    css_id_symbols=>• css_id_symbols _
    css_id_symbols=>• css_id_symbols -
    css_id_symbols=>• css_id_symbols $
    css_id_symbols=>• css_id_symbols θnum
    css_id_symbols=>• $
    css_id_symbols=>• θid
    css_id_symbols=>• θkey
    css_id_symbols=>• -
    media_or=>• media_in_parenths media_or_HC_listbody2_121
    media_query_group_043_116=>• τnot
    media_query_group_043_116=>• τonly
    media_type=>• identifier
    */
    _skip(l, const__);
    //considered syms: not,(,$,id,key,-,only

    //Single Production Completion

    //peek 0

    //block 0

    //groups false
    /*
    media_queries=>• media_queries_group_039_115
    */
    $media_queries_group_039_115(l);
    if (!FAILED) {
        setProduction(34);
        add_reduce(1, 27);
        return;
    }
    fail(l);
}
function $media_query_group_043_116(l: Lexer): void {//Production Start
    /*
    media_query_group_043_116=>• τnot
    media_query_group_043_116=>• τonly
    */
    _skip(l, const__);
    if (l.id == 22/* \not */) {
        //considered syms: not

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        media_query_group_043_116=>• τnot
        */
        _no_check(l);
        if (!FAILED) {
            setProduction(35);

            return;
        }
    } else if (l.id == 26/* \only */) {
        //considered syms: only

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        media_query_group_043_116=>• τonly
        */
        _no_check(l);
        if (!FAILED) {
            setProduction(35);

            return;
        }
    }
    fail(l);
}
function $media_query_group_144_117(l: Lexer): void {//Production Start
    /*
    media_query_group_144_117=>• τand media_condition_without_or
    */
    _skip(l, const__);
    //considered syms: and

    //Single Production Completion

    //peek 0

    //block 0

    //groups false
    /*
    media_query_group_144_117=>• τand media_condition_without_or
    */
    _with_skip(l, const__, 23/* \and */);
    if (!FAILED) {
        $media_condition_without_or(l);
        if (!FAILED) {
            setProduction(36);
            add_reduce(2, 19);
            return;
        }
    }
    fail(l);
}



function $media_not(l: Lexer): void {//Production Start
    /*
    media_not=>• τnot media_in_parenths
    */
    _skip(l, const__);
    //considered syms: not

    //Single Production Completion

    //peek 0

    //block 0

    //groups false
    /*
    media_not=>• τnot media_in_parenths
    */
    _with_skip(l, const__, 22/* \not */);
    if (!FAILED) {
        $media_in_parenths(l);
        if (!FAILED) {
            setProduction(40);
            add_reduce(2, 32);
            return;
        }
    }
    fail(l);
}
function $media_and_group_152_118(l: Lexer): void {//Production Start
    /*
    media_and_group_152_118=>• τand media_in_parenths
    */
    _skip(l, const__);
    //considered syms: and

    //Single Production Completion

    //peek 0

    //block 0

    //groups false
    /*
    media_and_group_152_118=>• τand media_in_parenths
    */
    _with_skip(l, const__, 23/* \and */);
    if (!FAILED) {
        $media_in_parenths(l);
        if (!FAILED) {
            setProduction(41);
            add_reduce(2, 19);
            return;
        }
    }
    fail(l);
}

function $media_and(l: Lexer): void {//Production Start
    /*
    media_and=>• media_in_parenths media_and_HC_listbody2_119
    media_in_parenths=>• ( media_condition )
    media_in_parenths=>• media_feature
    media_in_parenths=>• general_enclosed
    media_feature=>• ( media_feature_group_061_122 )
    general_enclosed=>• identifier ( general_enclosed_HC_listbody1_124 )
    general_enclosed=>• identifier ( )
    identifier=>• css_id_symbols θws
    identifier=>• css_id_symbols
    css_id_symbols=>• css_id_symbols θid
    css_id_symbols=>• css_id_symbols θkey
    css_id_symbols=>• css_id_symbols _
    css_id_symbols=>• css_id_symbols -
    css_id_symbols=>• css_id_symbols $
    css_id_symbols=>• css_id_symbols θnum
    css_id_symbols=>• $
    css_id_symbols=>• θid
    css_id_symbols=>• θkey
    css_id_symbols=>• -
    */
    _skip(l, const__);
    //considered syms: (,$,id,key,-

    //Single Production Completion

    //peek 0

    //block 0

    //groups false
    /*
    media_and=>• media_in_parenths media_and_HC_listbody2_119
    */
    $media_in_parenths(l);
    if (!FAILED) {
        $media_and_HC_listbody2_119(l);
        if (!FAILED) {
            setProduction(43);
            add_reduce(2, 22);
            return;
        }
    }
    fail(l);
}
function $media_or_group_154_120(l: Lexer): void {//Production Start
    /*
    media_or_group_154_120=>• τor media_in_parenths
    */
    _skip(l, const__);
    //considered syms: or

    //Single Production Completion

    //peek 0

    //block 0

    //groups false
    /*
    media_or_group_154_120=>• τor media_in_parenths
    */
    _with_skip(l, const__, 24/* \or */);
    if (!FAILED) {
        $media_in_parenths(l);
        if (!FAILED) {
            setProduction(44);
            add_reduce(2, 19);
            return;
        }
    }
    fail(l);
}

function $media_or(l: Lexer): void {//Production Start
    /*
    media_or=>• media_in_parenths media_or_HC_listbody2_121
    media_in_parenths=>• ( media_condition )
    media_in_parenths=>• media_feature
    media_in_parenths=>• general_enclosed
    media_feature=>• ( media_feature_group_061_122 )
    general_enclosed=>• identifier ( general_enclosed_HC_listbody1_124 )
    general_enclosed=>• identifier ( )
    identifier=>• css_id_symbols θws
    identifier=>• css_id_symbols
    css_id_symbols=>• css_id_symbols θid
    css_id_symbols=>• css_id_symbols θkey
    css_id_symbols=>• css_id_symbols _
    css_id_symbols=>• css_id_symbols -
    css_id_symbols=>• css_id_symbols $
    css_id_symbols=>• css_id_symbols θnum
    css_id_symbols=>• $
    css_id_symbols=>• θid
    css_id_symbols=>• θkey
    css_id_symbols=>• -
    */
    _skip(l, const__);
    //considered syms: (,$,id,key,-

    //Single Production Completion

    //peek 0

    //block 0

    //groups false
    /*
    media_or=>• media_in_parenths media_or_HC_listbody2_121
    */
    $media_in_parenths(l);
    if (!FAILED) {
        $media_or_HC_listbody2_121(l);
        if (!FAILED) {
            setProduction(46);
            add_reduce(2, 22);
            return;
        }
    }
    fail(l);
}
function $media_in_parenths(l: Lexer): void {//Production Start
    /*
    media_in_parenths=>• ( media_condition )
    media_in_parenths=>• media_feature
    media_in_parenths=>• general_enclosed
    media_feature=>• ( media_feature_group_061_122 )
    general_enclosed=>• identifier ( general_enclosed_HC_listbody1_124 )
    general_enclosed=>• identifier ( )
    identifier=>• css_id_symbols θws
    identifier=>• css_id_symbols
    css_id_symbols=>• css_id_symbols θid
    css_id_symbols=>• css_id_symbols θkey
    css_id_symbols=>• css_id_symbols _
    css_id_symbols=>• css_id_symbols -
    css_id_symbols=>• css_id_symbols $
    css_id_symbols=>• css_id_symbols θnum
    css_id_symbols=>• $
    css_id_symbols=>• θid
    css_id_symbols=>• θkey
    css_id_symbols=>• -
    */
    _skip(l, const__);
    if (l.id == 55/* \- */ || l.id == 56/* \$ */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {
        //considered syms: $,id,key,-

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        media_in_parenths=>• general_enclosed
        */
        $general_enclosed(l);
        if (!FAILED) {
            setProduction(47);

            return;
        }
    } else if (l.id == 17/* \( */) {
        //considered syms: (

        //Look Ahead Level 1
        /*
        media_in_parenths=>• media_feature
        */
        const pk1: Lexer = _pk(l.copy(), /* e.eh, */const__);
        /*media_in_parenths=>• media_feature peek 1 state: 
        css_id_symbols=>• $
        css_id_symbols=>• θid
        css_id_symbols=>• θkey
        css_id_symbols=>• -
        mf_boolean=>• τtrue
        mf_boolean=>• τfalse
        mf_value=>• θnum
        dimension=>• θnum θid
        ratio=>• θnum / θnum*/

        /*media_in_parenths=>• ( media_condition ) peek 1 state: 
        media_in_parenths=>• ( media_condition )*/

        if (!FAILED && pk1.id == 32/* \true */ || pk1.id == 33/* \false */ || pk1.id == 55/* \- */ || pk1.id == 56/* \$ */ || pk1.ty == 2/* \num */ || pk1.ty == 3/* \id */ || pk1.ty == 7/* \key */) {
            //considered syms: $,id,key,-,true,false,num

            //Single Production Completion

            //peek 1

            //block 3

            //groups true
            /*
            media_in_parenths=>• media_feature
            */
            $media_feature(l);
            if (!FAILED) {
                setProduction(47);

                return;
            }
        } else if (!FAILED && pk1.id == 17/* \( */) {
            //considered syms: (

            //Single Production Completion

            //peek 1

            //block 3

            //groups true
            /*
            media_in_parenths=>• ( media_condition )
            */
            _no_check(l);
            if (!FAILED) {
                $media_condition(l);
                if (!FAILED) {
                    _with_skip(l, const__, 18/* \) */);
                    if (!FAILED) {
                        setProduction(47);
                        add_reduce(3, 23);
                        return;
                    }
                }
            }
        }
    }
    fail(l);
}

function $media_feature(l: Lexer): void {//Production Start
    /*
    media_feature=>• ( media_feature_group_061_122 )
    */
    _skip(l, const__);
    //considered syms: (

    //Single Production Completion

    //peek 0

    //block 0

    //groups false
    /*
    media_feature=>• ( media_feature_group_061_122 )
    */
    _with_skip(l, const__, 17/* \( */);
    if (!FAILED) {
        $media_feature_group_061_122(l);
        if (!FAILED) {
            _with_skip(l, const__, 18/* \) */);
            if (!FAILED) {
                setProduction(49);
                add_reduce(3, 33);
                return;
            }
        }
    }
    fail(l);
}
function $general_enclosed_group_067_123(l: Lexer): void {//Production Start
    /*
    general_enclosed_group_067_123=>• θtok
    general_enclosed_group_067_123=>• θsym
    general_enclosed_group_067_123=>• θid
    general_enclosed_group_067_123=>• θkey
    general_enclosed_group_067_123=>• θws
    */
    _skip(l, const_1_);
    if (l.ty == 5/* \tok */) {
        //considered syms: tok

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        general_enclosed_group_067_123=>• θtok
        */
        _no_check(l);
        if (!FAILED) {
            setProduction(50);

            return;
        }
    } else if (l.ty == 6/* \sym */) {
        //considered syms: sym

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        general_enclosed_group_067_123=>• θsym
        */
        _no_check(l);
        if (!FAILED) {
            setProduction(50);

            return;
        }
    } else if (l.ty == 3/* \id */) {
        //considered syms: id

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        general_enclosed_group_067_123=>• θid
        */
        _no_check(l);
        if (!FAILED) {
            setProduction(50);

            return;
        }
    } else if (l.ty == 7/* \key */) {
        //considered syms: key

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        general_enclosed_group_067_123=>• θkey
        */
        _no_check(l);
        if (!FAILED) {
            setProduction(50);

            return;
        }
    } else if (l.ty == 1/* \ws */) {
        //considered syms: ws

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        general_enclosed_group_067_123=>• θws
        */
        _no_check(l);
        if (!FAILED) {
            setProduction(50);

            return;
        }
    }
    fail(l);
}

function $general_enclosed(l: Lexer): void {//Production Start
    /*
    general_enclosed=>• identifier ( general_enclosed_HC_listbody1_124 )
    general_enclosed=>• identifier ( )
    identifier=>• css_id_symbols θws
    identifier=>• css_id_symbols
    css_id_symbols=>• css_id_symbols θid
    css_id_symbols=>• css_id_symbols θkey
    css_id_symbols=>• css_id_symbols _
    css_id_symbols=>• css_id_symbols -
    css_id_symbols=>• css_id_symbols $
    css_id_symbols=>• css_id_symbols θnum
    css_id_symbols=>• $
    css_id_symbols=>• θid
    css_id_symbols=>• θkey
    css_id_symbols=>• -
    */
    _skip(l, const__);
    //considered syms: $,id,key,-

    //Parallel Transition
    /*
    general_enclosed=>• identifier ( general_enclosed_HC_listbody1_124 )
    general_enclosed=>• identifier ( )
    */
    $identifier(l);

    //Parallel Transition
    /*
    general_enclosed=>identifier • ( general_enclosed_HC_listbody1_124 )
    general_enclosed=>identifier • ( )
    */
    _with_skip(l, const_1_, 17/* \( */);

    //Look Ahead Level 0
    /*
    general_enclosed=>identifier ( • general_enclosed_HC_listbody1_124 )
    */
    _skip(l, const_1_);
    if (l.id == 18/* \) */) {
        //considered syms: )

        //Single Production Completion

        //peek 0

        //block 2

        //groups true
        /*
        general_enclosed=>identifier ( • )
        */
        _no_check(l);
        if (!FAILED) {
            setProduction(52);
            add_reduce(3, 37);
            return;
        }
    } else if (l.ty == 1/* \ws */ || l.ty == 3/* \id */ || l.ty == 5/* \tok */ || l.ty == 6/* \sym */ || l.ty == 7/* \key */) {
        //considered syms: tok,sym,id,key,ws

        //Single Production Completion

        //peek 0

        //block 2

        //groups true
        /*
        general_enclosed=>identifier ( • general_enclosed_HC_listbody1_124 )
        */
        $general_enclosed_HC_listbody1_124(l);
        if (!FAILED) {
            _with_skip(l, const__, 18/* \) */);
            if (!FAILED) {
                setProduction(52);
                add_reduce(4, 36);
                return;
            }
        }
    }
    fail(l);
}
function $mf_plain(l: Lexer): void {//Production Start
    /*
    mf_plain=>• mf_name : mf_value
    mf_name=>• identifier
    identifier=>• css_id_symbols θws
    identifier=>• css_id_symbols
    css_id_symbols=>• css_id_symbols θid
    css_id_symbols=>• css_id_symbols θkey
    css_id_symbols=>• css_id_symbols _
    css_id_symbols=>• css_id_symbols -
    css_id_symbols=>• css_id_symbols $
    css_id_symbols=>• css_id_symbols θnum
    css_id_symbols=>• $
    css_id_symbols=>• θid
    css_id_symbols=>• θkey
    css_id_symbols=>• -
    */
    _skip(l, const__);
    //considered syms: $,id,key,-

    //Single Production Completion

    //peek 0

    //block 0

    //groups false
    /*
    mf_plain=>• mf_name : mf_value
    */
    $mf_name(l);
    if (!FAILED) {
        _with_skip(l, const__, 28/* \: */);
        if (!FAILED) {
            $mf_value(l);
            if (!FAILED) {
                setProduction(53);
                add_reduce(3, 38);
                return;
            }
        }
    }
    fail(l);
}
function $mf_range_group_074_125(l: Lexer): void {//Production Start
    /*
    mf_range_group_074_125=>• <
    mf_range_group_074_125=>• < =
    mf_range_group_074_125=>• >
    mf_range_group_074_125=>• > =
    mf_range_group_074_125=>• =
    */
    _skip(l, const__);
    if (l.id == 29/* \< */) {
        //considered syms: <

        //Parallel Transition
        /*
        mf_range_group_074_125=>• <
        mf_range_group_074_125=>• < =
        */
        _no_check(l);

        //Look Ahead Level 0
        /*
        
        */
        _skip(l, const__);
        if (l.id == 30/* \= */) {
            //considered syms: =

            //Single Production Completion

            //peek 0

            //block 3

            //groups true
            /*
            mf_range_group_074_125=>< • =
            */
            _no_check(l);
            if (!FAILED) {
                setProduction(54);
                add_reduce(2, 0);
                return;
            }
        } else {
            //considered syms: END_OF_ITEM

            //Single Production Completion

            //peek 0

            //block 3

            //groups true
            /*
            mf_range_group_074_125=>< •
            */
            if (!FAILED) {
                setProduction(54);

                return;
            }
        }
    } else if (l.id == 31/* \> */) {
        //considered syms: >

        //Parallel Transition
        /*
        mf_range_group_074_125=>• >
        mf_range_group_074_125=>• > =
        */
        _no_check(l);

        //Look Ahead Level 0
        /*
        
        */
        _skip(l, const__);
        if (l.id == 30/* \= */) {
            //considered syms: =

            //Single Production Completion

            //peek 0

            //block 3

            //groups true
            /*
            mf_range_group_074_125=>> • =
            */
            _no_check(l);
            if (!FAILED) {
                setProduction(54);
                add_reduce(2, 0);
                return;
            }
        } else {
            //considered syms: END_OF_ITEM

            //Single Production Completion

            //peek 0

            //block 3

            //groups true
            /*
            mf_range_group_074_125=>> •
            */
            if (!FAILED) {
                setProduction(54);

                return;
            }
        }
    } else if (l.id == 30/* \= */) {
        //considered syms: =

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        mf_range_group_074_125=>• =
        */
        _no_check(l);
        if (!FAILED) {
            setProduction(54);

            return;
        }
    }
    fail(l);
}
function $mf_range_group_183_126(l: Lexer): void {//Production Start
    /*
    mf_range_group_183_126=>• >
    mf_range_group_183_126=>• > =
    */
    _skip(l, const__);
    //considered syms: >

    //Parallel Transition
    /*
    mf_range_group_183_126=>• >
    mf_range_group_183_126=>• > =
    */
    _no_check(l);

    //Look Ahead Level 0
    /*
    
    */
    _skip(l, const__);
    if (l.id == 30/* \= */) {
        //considered syms: =

        //Single Production Completion

        //peek 0

        //block 2

        //groups true
        /*
        mf_range_group_183_126=>> • =
        */
        _no_check(l);
        if (!FAILED) {
            setProduction(55);
            add_reduce(2, 0);
            return;
        }
    } else {
        //considered syms: END_OF_ITEM

        //Single Production Completion

        //peek 0

        //block 2

        //groups true
        /*
        mf_range_group_183_126=>> •
        */
        if (!FAILED) {
            setProduction(55);

            return;
        }
    }
    fail(l);
}
function $mf_range_group_188_127(l: Lexer): void {//Production Start
    /*
    mf_range_group_188_127=>• <
    mf_range_group_188_127=>• < =
    */
    _skip(l, const__);
    //considered syms: <

    //Parallel Transition
    /*
    mf_range_group_188_127=>• <
    mf_range_group_188_127=>• < =
    */
    _no_check(l);

    //Look Ahead Level 0
    /*
    
    */
    _skip(l, const__);
    if (l.id == 30/* \= */) {
        //considered syms: =

        //Single Production Completion

        //peek 0

        //block 2

        //groups true
        /*
        mf_range_group_188_127=>< • =
        */
        _no_check(l);
        if (!FAILED) {
            setProduction(56);
            add_reduce(2, 0);
            return;
        }
    } else {
        //considered syms: END_OF_ITEM

        //Single Production Completion

        //peek 0

        //block 2

        //groups true
        /*
        mf_range_group_188_127=>< •
        */
        if (!FAILED) {
            setProduction(56);

            return;
        }
    }
    fail(l);
}

function $mf_value(l: Lexer): void {//Production Start
    /*
    mf_value=>• θnum
    mf_value=>• dimension
    mf_value=>• mf_name
    mf_value=>• ratio
    dimension=>• θnum θid
    mf_name=>• identifier
    identifier=>• css_id_symbols θws
    identifier=>• css_id_symbols
    css_id_symbols=>• css_id_symbols θid
    css_id_symbols=>• css_id_symbols θkey
    css_id_symbols=>• css_id_symbols _
    css_id_symbols=>• css_id_symbols -
    css_id_symbols=>• css_id_symbols $
    css_id_symbols=>• css_id_symbols θnum
    css_id_symbols=>• $
    css_id_symbols=>• θid
    css_id_symbols=>• θkey
    css_id_symbols=>• -
    ratio=>• θnum / θnum
    */
    _skip(l, const__);
    if (l.id == 55/* \- */ || l.id == 56/* \$ */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {
        //considered syms: $,id,key,-

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        mf_value=>• mf_name
        */
        $mf_name(l);
        if (!FAILED) {
            setProduction(58);

            return;
        }
    } else if (l.ty == 2/* \num */) {
        //considered syms: num

        //Look Ahead Level 1
        /*
        mf_value=>• dimension
        mf_value=>• ratio
        */
        const pk1: Lexer = _pk(l.copy(), /* e.eh, */const__);
        /*mf_value=>• dimension peek 1 state: 
        dimension=>θnum • θid*/

        /*mf_value=>• ratio peek 1 state: 
        ratio=>θnum • / θnum*/

        /*mf_value=>• θnum peek 1 state: 
        mf_value=>• θnum*/

        if (!FAILED && pk1.id == 34/* \/ */) {
            //considered syms: /

            //Single Production Completion

            //peek 1

            //block 3

            //groups true
            /*
            mf_value=>• ratio
            */
            $ratio(l);
            if (!FAILED) {
                setProduction(58);

                return;
            }
        } else if (!FAILED && pk1.ty == 3/* \id */) {
            //considered syms: id

            //Single Production Completion

            //peek 1

            //block 3

            //groups true
            /*
            mf_value=>• dimension
            */
            $dimension(l);
            if (!FAILED) {
                setProduction(58);

                return;
            }
        } else if (!FAILED && pk1.ty == 2/* \num */) {
            //considered syms: num

            //Single Production Completion

            //peek 1

            //block 3

            //groups true
            /*
            mf_value=>• θnum
            */
            _no_check(l);
            if (!FAILED) {
                setProduction(58);

                return;
            }
        }
    }
    fail(l);
}
function $mf_boolean(l: Lexer): void {//Production Start
    /*
    mf_boolean=>• τtrue
    mf_boolean=>• τfalse
    */
    _skip(l, const__);
    if (l.id == 32/* \true */) {
        //considered syms: true

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        mf_boolean=>• τtrue
        */
        _no_check(l);
        if (!FAILED) {
            setProduction(59);
            add_reduce(1, 42);
            return;
        }
    } else if (l.id == 33/* \false */) {
        //considered syms: false

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        mf_boolean=>• τfalse
        */
        _no_check(l);
        if (!FAILED) {
            setProduction(59);
            add_reduce(1, 43);
            return;
        }
    }
    fail(l);
}
function $mf_name(l: Lexer): void {//Production Start
    /*
    mf_name=>• identifier
    identifier=>• css_id_symbols θws
    identifier=>• css_id_symbols
    css_id_symbols=>• css_id_symbols θid
    css_id_symbols=>• css_id_symbols θkey
    css_id_symbols=>• css_id_symbols _
    css_id_symbols=>• css_id_symbols -
    css_id_symbols=>• css_id_symbols $
    css_id_symbols=>• css_id_symbols θnum
    css_id_symbols=>• $
    css_id_symbols=>• θid
    css_id_symbols=>• θkey
    css_id_symbols=>• -
    */
    _skip(l, const__);
    //considered syms: $,id,key,-

    //Single Production Completion

    //peek 0

    //block 0

    //groups false
    /*
    mf_name=>• identifier
    */
    $identifier(l);
    if (!FAILED) {
        setProduction(60);

        return;
    }
    fail(l);
}
function $media_type(l: Lexer): void {//Production Start
    /*
    media_type=>• identifier
    identifier=>• css_id_symbols θws
    identifier=>• css_id_symbols
    css_id_symbols=>• css_id_symbols θid
    css_id_symbols=>• css_id_symbols θkey
    css_id_symbols=>• css_id_symbols _
    css_id_symbols=>• css_id_symbols -
    css_id_symbols=>• css_id_symbols $
    css_id_symbols=>• css_id_symbols θnum
    css_id_symbols=>• $
    css_id_symbols=>• θid
    css_id_symbols=>• θkey
    css_id_symbols=>• -
    */
    _skip(l, const__);
    //considered syms: $,id,key,-

    //Single Production Completion

    //peek 0

    //block 0

    //groups false
    /*
    media_type=>• identifier
    */
    $identifier(l);
    if (!FAILED) {
        setProduction(61);
        add_reduce(1, 44);
        return;
    }
    fail(l);
}
function $ratio(l: Lexer): void {//Production Start
    /*
    ratio=>• θnum / θnum
    */
    _skip(l, const__);
    //considered syms: num

    //Single Production Completion

    //peek 0

    //block 0

    //groups false
    /*
    ratio=>• θnum / θnum
    */
    _with_skip(l, const__, 2/* \num */);
    if (!FAILED) {
        _with_skip(l, const__, 34/* \/ */);
        if (!FAILED) {
            _with_skip(l, const__, 2/* \num */);
            if (!FAILED) {
                setProduction(62);
                add_reduce(3, 45);
                return;
            }
        }
    }
    fail(l);
}
function $percentage(l: Lexer): void {//Production Start
    /*
    percentage=>• θnum %
    */
    _skip(l, const__);
    //considered syms: num

    //Single Production Completion

    //peek 0

    //block 0

    //groups false
    /*
    percentage=>• θnum %
    */
    _with_skip(l, const__, 2/* \num */);
    if (!FAILED) {
        _with_skip(l, const__, 35/* \% */);
        if (!FAILED) {
            setProduction(63);
            add_reduce(2, 46);
            return;
        }
    }
    fail(l);
}
function $dimension(l: Lexer): void {//Production Start
    /*
    dimension=>• θnum θid
    */
    _skip(l, const__);
    //considered syms: num

    //Single Production Completion

    //peek 0

    //block 0

    //groups false
    /*
    dimension=>• θnum θid
    */
    _with_skip(l, const__, 2/* \num */);
    if (!FAILED) {
        _with_skip(l, const__, 3/* \id */);
        if (!FAILED) {
            setProduction(64);
            add_reduce(2, 47);
            return;
        }
    }
    fail(l);
}
function $url(l: Lexer): void {//Production Start
    /*
    url=>• τurl ( string )
    */
    _skip(l, const__);
    //considered syms: url

    //Single Production Completion

    //peek 0

    //block 0

    //groups false
    /*
    url=>• τurl ( string )
    */
    _with_skip(l, const__, 36/* \url */);
    if (!FAILED) {
        _with_skip(l, const__, 17/* \( */);
        if (!FAILED) {
            $string(l);
            if (!FAILED) {
                _with_skip(l, const__, 18/* \) */);
                if (!FAILED) {
                    setProduction(65);
                    add_reduce(4, 48);
                    return;
                }
            }
        }
    }
    fail(l);
}
function $string_group_0109_128(l: Lexer): void {//Production Start
    /*
    string_group_0109_128=>• θtok
    string_group_0109_128=>• θkey
    string_group_0109_128=>• θid
    string_group_0109_128=>• θsym
    string_group_0109_128=>• θnum
    string_group_0109_128=>• θws
    */
    _skip(l, const_1_);
    if (l.ty == 5/* \tok */) {
        //considered syms: tok

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        string_group_0109_128=>• θtok
        */
        _no_check(l);
        if (!FAILED) {
            setProduction(66);

            return;
        }
    } else if (l.ty == 7/* \key */) {
        //considered syms: key

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        string_group_0109_128=>• θkey
        */
        _no_check(l);
        if (!FAILED) {
            setProduction(66);

            return;
        }
    } else if (l.ty == 3/* \id */) {
        //considered syms: id

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        string_group_0109_128=>• θid
        */
        _no_check(l);
        if (!FAILED) {
            setProduction(66);

            return;
        }
    } else if (l.ty == 6/* \sym */) {
        //considered syms: sym

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        string_group_0109_128=>• θsym
        */
        _no_check(l);
        if (!FAILED) {
            setProduction(66);

            return;
        }
    } else if (l.ty == 2/* \num */) {
        //considered syms: num

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        string_group_0109_128=>• θnum
        */
        _no_check(l);
        if (!FAILED) {
            setProduction(66);

            return;
        }
    } else if (l.ty == 1/* \ws */) {
        //considered syms: ws

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        string_group_0109_128=>• θws
        */
        _no_check(l);
        if (!FAILED) {
            setProduction(66);

            return;
        }
    }
    fail(l);
}


function $string(l: Lexer): void {//Production Start
    /*
    string=>• " string_HC_listbody1_129 "
    string=>• ' string_HC_listbody1_130 '
    */
    _skip(l, const__);
    if (l.id == 27/* \" */) {
        //considered syms: "

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        string=>• " string_HC_listbody1_129 "
        */
        _no_check(l);
        if (!FAILED) {
            $string_HC_listbody1_129(l);
            if (!FAILED) {
                _with_skip(l, const__, 27/* \" */);
                if (!FAILED) {
                    setProduction(69);
                    add_reduce(3, 0);
                    return;
                }
            }
        }
    } else if (l.id == 37/* \' */) {
        //considered syms: '

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        string=>• ' string_HC_listbody1_130 '
        */
        _no_check(l);
        if (!FAILED) {
            $string_HC_listbody1_130(l);
            if (!FAILED) {
                _with_skip(l, const__, 37/* \' */);
                if (!FAILED) {
                    setProduction(69);
                    add_reduce(3, 49);
                    return;
                }
            }
        }
    }
    fail(l);
}
function $COMPLEX_SELECTOR_group_0118_131(l: Lexer): void {//Production Start
    /*
    COMPLEX_SELECTOR_group_0118_131=>• COMBINATOR
    COMBINATOR=>• >
    COMBINATOR=>• +
    COMBINATOR=>• ~
    COMBINATOR=>• ||
    */
    _skip(l, const__);
    //considered syms: >,+,~,||

    //Single Production Completion

    //peek 0

    //block 0

    //groups false
    /*
    COMPLEX_SELECTOR_group_0118_131=>• COMBINATOR
    */
    $COMBINATOR(l);
    if (!FAILED) {
        setProduction(70);
        add_reduce(1, 50);
        return;
    }
    fail(l);
}
function $COMPLEX_SELECTOR_group_1119_132(l: Lexer): void {//Production Start
    /*
    COMPLEX_SELECTOR_group_1119_132=>• COMPLEX_SELECTOR_group_0118_131 COMPOUND_SELECTOR
    COMPLEX_SELECTOR_group_1119_132=>• COMPOUND_SELECTOR
    COMPLEX_SELECTOR_group_0118_131=>• COMBINATOR
    COMBINATOR=>• >
    COMBINATOR=>• +
    COMBINATOR=>• ~
    COMBINATOR=>• ||
    COMPOUND_SELECTOR=>• TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody1_134 COMPOUND_SELECTOR_HC_listbody2_137 θws
    COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody1_134 COMPOUND_SELECTOR_HC_listbody2_137 θws
    COMPOUND_SELECTOR=>• TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody2_137 θws
    COMPOUND_SELECTOR=>• TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody1_134 θws
    COMPOUND_SELECTOR=>• TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody1_134 COMPOUND_SELECTOR_HC_listbody2_137
    COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody2_137 θws
    COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody1_134 θws
    COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody1_134 COMPOUND_SELECTOR_HC_listbody2_137
    COMPOUND_SELECTOR=>• TYPE_SELECTOR θws
    COMPOUND_SELECTOR=>• TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody2_137
    COMPOUND_SELECTOR=>• TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody1_134
    COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody2_137
    COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody1_134
    COMPOUND_SELECTOR=>• TYPE_SELECTOR
    TYPE_SELECTOR=>• WQ_NAME
    TYPE_SELECTOR=>• NS_PREFIX *
    TYPE_SELECTOR=>• *
    WQ_NAME=>• NS_PREFIX identifier
    WQ_NAME=>• identifier
    NS_PREFIX=>• NS_PREFIX_group_0150_140 |
    NS_PREFIX=>• |
    NS_PREFIX_group_0150_140=>• identifier
    NS_PREFIX_group_0150_140=>• *
    identifier=>• css_id_symbols θws
    identifier=>• css_id_symbols
    css_id_symbols=>• css_id_symbols θid
    css_id_symbols=>• css_id_symbols θkey
    css_id_symbols=>• css_id_symbols _
    css_id_symbols=>• css_id_symbols -
    css_id_symbols=>• css_id_symbols $
    css_id_symbols=>• css_id_symbols θnum
    css_id_symbols=>• $
    css_id_symbols=>• θid
    css_id_symbols=>• θkey
    css_id_symbols=>• -
    COMPOUND_SELECTOR_HC_listbody1_134=>• COMPOUND_SELECTOR_HC_listbody1_134 SUBCLASS_SELECTOR
    COMPOUND_SELECTOR_HC_listbody1_134=>• SUBCLASS_SELECTOR
    SUBCLASS_SELECTOR=>• ID_SELECTOR
    SUBCLASS_SELECTOR=>• CLASS_SELECTOR
    SUBCLASS_SELECTOR=>• ATTRIBUTE_SELECTOR
    SUBCLASS_SELECTOR=>• PSEUDO_CLASS_SELECTOR
    ID_SELECTOR=>• # identifier
    CLASS_SELECTOR=>• . identifier
    ATTRIBUTE_SELECTOR=>• [ WQ_NAME ]
    ATTRIBUTE_SELECTOR=>• [ WQ_NAME ATTR_MATCHER ATTRIBUTE_SELECTOR_group_2137_139 ATTR_MODIFIER ]
    ATTRIBUTE_SELECTOR=>• [ WQ_NAME ATTR_MATCHER ATTRIBUTE_SELECTOR_group_2137_139 ]
    PSEUDO_CLASS_SELECTOR=>• : identifier PSEUDO_CLASS_SELECTOR_group_2133_138
    PSEUDO_CLASS_SELECTOR=>• : identifier
    COMPOUND_SELECTOR_HC_listbody2_137=>• COMPOUND_SELECTOR_HC_listbody2_137 COMPOUND_SELECTOR_group_1121_136
    COMPOUND_SELECTOR_HC_listbody2_137=>• COMPOUND_SELECTOR_group_1121_136
    COMPOUND_SELECTOR_group_1121_136=>• PSEUDO_ELEMENT_SELECTOR COMPOUND_SELECTOR_HC_listbody1_135
    COMPOUND_SELECTOR_group_1121_136=>• PSEUDO_ELEMENT_SELECTOR
    PSEUDO_ELEMENT_SELECTOR=>• : PSEUDO_CLASS_SELECTOR
    */
    _skip(l, const__);
    if (const_0_.includes(l.id) || l.ty == 3/* \id */ || l.ty == 7/* \key */) {
        //considered syms: *,|,$,id,key,-,#,.,[,:

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        COMPLEX_SELECTOR_group_1119_132=>• COMPOUND_SELECTOR
        */
        $COMPOUND_SELECTOR(l);
        if (!FAILED) {
            setProduction(71);
            add_reduce(1, 2);
            return;
        }
    } else if (l.id == 31/* \> */ || l.id == 38/* \+ */ || l.id == 39/* \~ */ || l.id == 40/* \|| */) {
        //considered syms: >,+,~,||

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        COMPLEX_SELECTOR_group_1119_132=>• COMPLEX_SELECTOR_group_0118_131 COMPOUND_SELECTOR
        */
        $COMPLEX_SELECTOR_group_0118_131(l);
        if (!FAILED) {
            $COMPOUND_SELECTOR(l);
            if (!FAILED) {
                setProduction(71);
                add_reduce(2, 51);
                return;
            }
        }
    }
    fail(l);
}

function $COMPLEX_SELECTOR(l: Lexer): void {//Production Start
    /*
    COMPLEX_SELECTOR=>• COMPOUND_SELECTOR COMPLEX_SELECTOR_HC_listbody2_133
    COMPLEX_SELECTOR=>• COMPOUND_SELECTOR
    COMPOUND_SELECTOR=>• TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody1_134 COMPOUND_SELECTOR_HC_listbody2_137 θws
    COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody1_134 COMPOUND_SELECTOR_HC_listbody2_137 θws
    COMPOUND_SELECTOR=>• TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody2_137 θws
    COMPOUND_SELECTOR=>• TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody1_134 θws
    COMPOUND_SELECTOR=>• TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody1_134 COMPOUND_SELECTOR_HC_listbody2_137
    COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody2_137 θws
    COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody1_134 θws
    COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody1_134 COMPOUND_SELECTOR_HC_listbody2_137
    COMPOUND_SELECTOR=>• TYPE_SELECTOR θws
    COMPOUND_SELECTOR=>• TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody2_137
    COMPOUND_SELECTOR=>• TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody1_134
    COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody2_137
    COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody1_134
    COMPOUND_SELECTOR=>• TYPE_SELECTOR
    TYPE_SELECTOR=>• WQ_NAME
    TYPE_SELECTOR=>• NS_PREFIX *
    TYPE_SELECTOR=>• *
    WQ_NAME=>• NS_PREFIX identifier
    WQ_NAME=>• identifier
    NS_PREFIX=>• NS_PREFIX_group_0150_140 |
    NS_PREFIX=>• |
    NS_PREFIX_group_0150_140=>• identifier
    NS_PREFIX_group_0150_140=>• *
    identifier=>• css_id_symbols θws
    identifier=>• css_id_symbols
    css_id_symbols=>• css_id_symbols θid
    css_id_symbols=>• css_id_symbols θkey
    css_id_symbols=>• css_id_symbols _
    css_id_symbols=>• css_id_symbols -
    css_id_symbols=>• css_id_symbols $
    css_id_symbols=>• css_id_symbols θnum
    css_id_symbols=>• $
    css_id_symbols=>• θid
    css_id_symbols=>• θkey
    css_id_symbols=>• -
    COMPOUND_SELECTOR_HC_listbody1_134=>• COMPOUND_SELECTOR_HC_listbody1_134 SUBCLASS_SELECTOR
    COMPOUND_SELECTOR_HC_listbody1_134=>• SUBCLASS_SELECTOR
    SUBCLASS_SELECTOR=>• ID_SELECTOR
    SUBCLASS_SELECTOR=>• CLASS_SELECTOR
    SUBCLASS_SELECTOR=>• ATTRIBUTE_SELECTOR
    SUBCLASS_SELECTOR=>• PSEUDO_CLASS_SELECTOR
    ID_SELECTOR=>• # identifier
    CLASS_SELECTOR=>• . identifier
    ATTRIBUTE_SELECTOR=>• [ WQ_NAME ]
    ATTRIBUTE_SELECTOR=>• [ WQ_NAME ATTR_MATCHER ATTRIBUTE_SELECTOR_group_2137_139 ATTR_MODIFIER ]
    ATTRIBUTE_SELECTOR=>• [ WQ_NAME ATTR_MATCHER ATTRIBUTE_SELECTOR_group_2137_139 ]
    PSEUDO_CLASS_SELECTOR=>• : identifier PSEUDO_CLASS_SELECTOR_group_2133_138
    PSEUDO_CLASS_SELECTOR=>• : identifier
    COMPOUND_SELECTOR_HC_listbody2_137=>• COMPOUND_SELECTOR_HC_listbody2_137 COMPOUND_SELECTOR_group_1121_136
    COMPOUND_SELECTOR_HC_listbody2_137=>• COMPOUND_SELECTOR_group_1121_136
    COMPOUND_SELECTOR_group_1121_136=>• PSEUDO_ELEMENT_SELECTOR COMPOUND_SELECTOR_HC_listbody1_135
    COMPOUND_SELECTOR_group_1121_136=>• PSEUDO_ELEMENT_SELECTOR
    PSEUDO_ELEMENT_SELECTOR=>• : PSEUDO_CLASS_SELECTOR
    */
    _skip(l, const__);
    //considered syms: *,|,$,id,key,-,#,.,[,:

    //Parallel Transition
    /*
    COMPLEX_SELECTOR=>• COMPOUND_SELECTOR COMPLEX_SELECTOR_HC_listbody2_133
    COMPLEX_SELECTOR=>• COMPOUND_SELECTOR
    */
    $COMPOUND_SELECTOR(l);

    //Look Ahead Level 0
    /*
    COMPLEX_SELECTOR=>COMPOUND_SELECTOR • COMPLEX_SELECTOR_HC_listbody2_133
    */
    _skip(l, const__);
    if (const_13_.includes(l.id) || l.ty == 3/* \id */ || l.ty == 7/* \key */) {
        //considered syms: >,+,~,||,*,|,$,id,key,-,#,.,[,:

        //Single Production Completion

        //peek 0

        //block 2

        //groups true
        /*
        COMPLEX_SELECTOR=>COMPOUND_SELECTOR • COMPLEX_SELECTOR_HC_listbody2_133
        */
        $COMPLEX_SELECTOR_HC_listbody2_133(l);
        if (!FAILED) {
            setProduction(73);
            add_reduce(2, 52);
            return;
        }
    } else {
        //considered syms: END_OF_ITEM

        //Single Production Completion

        //peek 0

        //block 2

        //groups true
        /*
        COMPLEX_SELECTOR=>COMPOUND_SELECTOR •
        */
        if (!FAILED) {
            setProduction(73);
            add_reduce(1, 53);
            return;
        }
    }
    fail(l);
}


function $COMPOUND_SELECTOR_group_1121_136(l: Lexer): void {//Production Start
    /*
    COMPOUND_SELECTOR_group_1121_136=>• PSEUDO_ELEMENT_SELECTOR COMPOUND_SELECTOR_HC_listbody1_135
    COMPOUND_SELECTOR_group_1121_136=>• PSEUDO_ELEMENT_SELECTOR
    PSEUDO_ELEMENT_SELECTOR=>• : PSEUDO_CLASS_SELECTOR
    */
    _skip(l, const__);
    //considered syms: :

    //Parallel Transition
    /*
    COMPOUND_SELECTOR_group_1121_136=>• PSEUDO_ELEMENT_SELECTOR COMPOUND_SELECTOR_HC_listbody1_135
    COMPOUND_SELECTOR_group_1121_136=>• PSEUDO_ELEMENT_SELECTOR
    */
    $PSEUDO_ELEMENT_SELECTOR(l);

    //Look Ahead Level 0
    /*
    COMPOUND_SELECTOR_group_1121_136=>PSEUDO_ELEMENT_SELECTOR • COMPOUND_SELECTOR_HC_listbody1_135
    */
    _skip(l, const__);
    if (l.id == 28/* \: */) {
        //considered syms: :

        //Single Production Completion

        //peek 0

        //block 2

        //groups true
        /*
        COMPOUND_SELECTOR_group_1121_136=>PSEUDO_ELEMENT_SELECTOR • COMPOUND_SELECTOR_HC_listbody1_135
        */
        $COMPOUND_SELECTOR_HC_listbody1_135(l);
        if (!FAILED) {
            setProduction(76);
            add_reduce(2, 54);
            return;
        }
    } else {
        //considered syms: END_OF_ITEM

        //Single Production Completion

        //peek 0

        //block 2

        //groups true
        /*
        COMPOUND_SELECTOR_group_1121_136=>PSEUDO_ELEMENT_SELECTOR •
        */
        if (!FAILED) {
            setProduction(76);
            add_reduce(1, 55);
            return;
        }
    }
    fail(l);
}

function $COMPOUND_SELECTOR(l: Lexer): void {//Production Start
    /*
    COMPOUND_SELECTOR=>• TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody1_134 COMPOUND_SELECTOR_HC_listbody2_137 θws
    COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody1_134 COMPOUND_SELECTOR_HC_listbody2_137 θws
    COMPOUND_SELECTOR=>• TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody2_137 θws
    COMPOUND_SELECTOR=>• TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody1_134 θws
    COMPOUND_SELECTOR=>• TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody1_134 COMPOUND_SELECTOR_HC_listbody2_137
    COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody2_137 θws
    COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody1_134 θws
    COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody1_134 COMPOUND_SELECTOR_HC_listbody2_137
    COMPOUND_SELECTOR=>• TYPE_SELECTOR θws
    COMPOUND_SELECTOR=>• TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody2_137
    COMPOUND_SELECTOR=>• TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody1_134
    COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody2_137
    COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody1_134
    COMPOUND_SELECTOR=>• TYPE_SELECTOR
    TYPE_SELECTOR=>• WQ_NAME
    TYPE_SELECTOR=>• NS_PREFIX *
    TYPE_SELECTOR=>• *
    WQ_NAME=>• NS_PREFIX identifier
    WQ_NAME=>• identifier
    NS_PREFIX=>• NS_PREFIX_group_0150_140 |
    NS_PREFIX=>• |
    NS_PREFIX_group_0150_140=>• identifier
    NS_PREFIX_group_0150_140=>• *
    identifier=>• css_id_symbols θws
    identifier=>• css_id_symbols
    css_id_symbols=>• css_id_symbols θid
    css_id_symbols=>• css_id_symbols θkey
    css_id_symbols=>• css_id_symbols _
    css_id_symbols=>• css_id_symbols -
    css_id_symbols=>• css_id_symbols $
    css_id_symbols=>• css_id_symbols θnum
    css_id_symbols=>• $
    css_id_symbols=>• θid
    css_id_symbols=>• θkey
    css_id_symbols=>• -
    COMPOUND_SELECTOR_HC_listbody1_134=>• COMPOUND_SELECTOR_HC_listbody1_134 SUBCLASS_SELECTOR
    COMPOUND_SELECTOR_HC_listbody1_134=>• SUBCLASS_SELECTOR
    SUBCLASS_SELECTOR=>• ID_SELECTOR
    SUBCLASS_SELECTOR=>• CLASS_SELECTOR
    SUBCLASS_SELECTOR=>• ATTRIBUTE_SELECTOR
    SUBCLASS_SELECTOR=>• PSEUDO_CLASS_SELECTOR
    ID_SELECTOR=>• # identifier
    CLASS_SELECTOR=>• . identifier
    ATTRIBUTE_SELECTOR=>• [ WQ_NAME ]
    ATTRIBUTE_SELECTOR=>• [ WQ_NAME ATTR_MATCHER ATTRIBUTE_SELECTOR_group_2137_139 ATTR_MODIFIER ]
    ATTRIBUTE_SELECTOR=>• [ WQ_NAME ATTR_MATCHER ATTRIBUTE_SELECTOR_group_2137_139 ]
    PSEUDO_CLASS_SELECTOR=>• : identifier PSEUDO_CLASS_SELECTOR_group_2133_138
    PSEUDO_CLASS_SELECTOR=>• : identifier
    COMPOUND_SELECTOR_HC_listbody2_137=>• COMPOUND_SELECTOR_HC_listbody2_137 COMPOUND_SELECTOR_group_1121_136
    COMPOUND_SELECTOR_HC_listbody2_137=>• COMPOUND_SELECTOR_group_1121_136
    COMPOUND_SELECTOR_group_1121_136=>• PSEUDO_ELEMENT_SELECTOR COMPOUND_SELECTOR_HC_listbody1_135
    COMPOUND_SELECTOR_group_1121_136=>• PSEUDO_ELEMENT_SELECTOR
    PSEUDO_ELEMENT_SELECTOR=>• : PSEUDO_CLASS_SELECTOR
    */
    _skip(l, const__);
    if (l.id == 50/* \* */ || l.id == 51/* \| */ || l.id == 55/* \- */ || l.id == 56/* \$ */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {
        //considered syms: *,|,$,id,key,-

        //Parallel Transition
        /*
        COMPOUND_SELECTOR=>• TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody1_134 COMPOUND_SELECTOR_HC_listbody2_137 θws
        COMPOUND_SELECTOR=>• TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody2_137 θws
        COMPOUND_SELECTOR=>• TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody1_134 θws
        COMPOUND_SELECTOR=>• TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody1_134 COMPOUND_SELECTOR_HC_listbody2_137
        COMPOUND_SELECTOR=>• TYPE_SELECTOR θws
        COMPOUND_SELECTOR=>• TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody2_137
        COMPOUND_SELECTOR=>• TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody1_134
        COMPOUND_SELECTOR=>• TYPE_SELECTOR
        */
        $TYPE_SELECTOR(l);

        //Look Ahead Level 0
        /*
        COMPOUND_SELECTOR=>TYPE_SELECTOR • COMPOUND_SELECTOR_HC_listbody1_134 COMPOUND_SELECTOR_HC_listbody2_137 θws
        COMPOUND_SELECTOR=>TYPE_SELECTOR • COMPOUND_SELECTOR_HC_listbody2_137 θws
        COMPOUND_SELECTOR=>TYPE_SELECTOR • COMPOUND_SELECTOR_HC_listbody1_134 θws
        COMPOUND_SELECTOR=>TYPE_SELECTOR • COMPOUND_SELECTOR_HC_listbody1_134 COMPOUND_SELECTOR_HC_listbody2_137
        COMPOUND_SELECTOR=>TYPE_SELECTOR • COMPOUND_SELECTOR_HC_listbody2_137
        COMPOUND_SELECTOR=>TYPE_SELECTOR • COMPOUND_SELECTOR_HC_listbody1_134
        */
        _skip(l, const_1_);
        if (l.id == 41/* \# */ || l.id == 42/* \. */ || l.id == 43/* \[ */) {
            //considered syms: #,.,[

            //Parallel Transition
            /*
            COMPOUND_SELECTOR=>TYPE_SELECTOR • COMPOUND_SELECTOR_HC_listbody1_134 COMPOUND_SELECTOR_HC_listbody2_137 θws
            COMPOUND_SELECTOR=>TYPE_SELECTOR • COMPOUND_SELECTOR_HC_listbody1_134 θws
            COMPOUND_SELECTOR=>TYPE_SELECTOR • COMPOUND_SELECTOR_HC_listbody1_134 COMPOUND_SELECTOR_HC_listbody2_137
            COMPOUND_SELECTOR=>TYPE_SELECTOR • COMPOUND_SELECTOR_HC_listbody1_134
            */
            $COMPOUND_SELECTOR_HC_listbody1_134(l);

            //Look Ahead Level 0
            /*
            COMPOUND_SELECTOR=>TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody1_134 • COMPOUND_SELECTOR_HC_listbody2_137 θws
            COMPOUND_SELECTOR=>TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody1_134 • COMPOUND_SELECTOR_HC_listbody2_137
            */
            _skip(l, const_1_);
            if (l.id == 28/* \: */) {
                //considered syms: :

                //Parallel Transition
                /*
                COMPOUND_SELECTOR=>TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody1_134 • COMPOUND_SELECTOR_HC_listbody2_137 θws
                COMPOUND_SELECTOR=>TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody1_134 • COMPOUND_SELECTOR_HC_listbody2_137
                */
                $COMPOUND_SELECTOR_HC_listbody2_137(l);

                //Look Ahead Level 0
                /*
                
                */
                _skip(l, const_1_);
                if (l.ty == 1/* \ws */) {
                    //considered syms: ws

                    //Single Production Completion

                    //peek 0

                    //block 7

                    //groups true
                    /*
                    COMPOUND_SELECTOR=>TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody1_134 COMPOUND_SELECTOR_HC_listbody2_137 • θws
                    */
                    _no_check(l);
                    if (!FAILED) {
                        setProduction(78);
                        add_reduce(4, 56);
                        return;
                    }
                } else {
                    //considered syms: END_OF_ITEM

                    //Single Production Completion

                    //peek 0

                    //block 7

                    //groups true
                    /*
                    COMPOUND_SELECTOR=>TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody1_134 COMPOUND_SELECTOR_HC_listbody2_137 •
                    */
                    if (!FAILED) {
                        setProduction(78);
                        add_reduce(3, 56);
                        return;
                    }
                }
            } else if (l.ty == 1/* \ws */) {
                //considered syms: ws

                //Single Production Completion

                //peek 0

                //block 5

                //groups true
                /*
                COMPOUND_SELECTOR=>TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody1_134 • θws
                */
                _no_check(l);
                if (!FAILED) {
                    setProduction(78);
                    add_reduce(3, 59);
                    return;
                }
            } else {
                //considered syms: END_OF_ITEM

                //Single Production Completion

                //peek 0

                //block 5

                //groups true
                /*
                COMPOUND_SELECTOR=>TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody1_134 •
                */
                if (!FAILED) {
                    setProduction(78);
                    add_reduce(2, 59);
                    return;
                }
            }
        } else if (l.id == 28/* \: */) {
            //considered syms: :

            //Look Ahead Level 1
            /*
            COMPOUND_SELECTOR=>TYPE_SELECTOR • COMPOUND_SELECTOR_HC_listbody1_134 COMPOUND_SELECTOR_HC_listbody2_137 θws
            COMPOUND_SELECTOR=>TYPE_SELECTOR • COMPOUND_SELECTOR_HC_listbody2_137 θws
            COMPOUND_SELECTOR=>TYPE_SELECTOR • COMPOUND_SELECTOR_HC_listbody1_134 θws
            COMPOUND_SELECTOR=>TYPE_SELECTOR • COMPOUND_SELECTOR_HC_listbody1_134 COMPOUND_SELECTOR_HC_listbody2_137
            COMPOUND_SELECTOR=>TYPE_SELECTOR • COMPOUND_SELECTOR_HC_listbody2_137
            COMPOUND_SELECTOR=>TYPE_SELECTOR • COMPOUND_SELECTOR_HC_listbody1_134
            */
            const pk1: Lexer = _pk(l.copy(), /* e.eh, */const__);
            /*COMPOUND_SELECTOR=>• TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody1_134 COMPOUND_SELECTOR_HC_listbody2_137 θws peek 1 state: 
            css_id_symbols=>• $
            css_id_symbols=>• θid
            css_id_symbols=>• θkey
            css_id_symbols=>• -
            NS_PREFIX=>• |
            NS_PREFIX_group_0150_140=>• **/

            /*COMPOUND_SELECTOR=>• TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody2_137 θws peek 1 state: 
            PSEUDO_CLASS_SELECTOR=>• : identifier PSEUDO_CLASS_SELECTOR_group_2133_138
            PSEUDO_CLASS_SELECTOR=>• : identifier*/

            /*COMPOUND_SELECTOR=>• TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody1_134 θws peek 1 state: 
            css_id_symbols=>• $
            css_id_symbols=>• θid
            css_id_symbols=>• θkey
            css_id_symbols=>• -
            NS_PREFIX=>• |
            NS_PREFIX_group_0150_140=>• **/

            /*COMPOUND_SELECTOR=>• TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody1_134 COMPOUND_SELECTOR_HC_listbody2_137 peek 1 state: 
            css_id_symbols=>• $
            css_id_symbols=>• θid
            css_id_symbols=>• θkey
            css_id_symbols=>• -
            NS_PREFIX=>• |
            NS_PREFIX_group_0150_140=>• **/

            /*COMPOUND_SELECTOR=>• TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody2_137 peek 1 state: 
            PSEUDO_CLASS_SELECTOR=>• : identifier PSEUDO_CLASS_SELECTOR_group_2133_138
            PSEUDO_CLASS_SELECTOR=>• : identifier*/

            /*COMPOUND_SELECTOR=>• TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody1_134 peek 1 state: 
            css_id_symbols=>• $
            css_id_symbols=>• θid
            css_id_symbols=>• θkey
            css_id_symbols=>• -
            NS_PREFIX=>• |
            NS_PREFIX_group_0150_140=>• **/

            if (pk1.id == 50/* \* */ || pk1.id == 51/* \| */ || pk1.id == 55/* \- */ || pk1.id == 56/* \$ */ || pk1.ty == 3/* \id */ || pk1.ty == 7/* \key */) {
                //considered syms: $,id,key,-,|,*

                //Parallel Transition
                /*
                COMPOUND_SELECTOR=>TYPE_SELECTOR • COMPOUND_SELECTOR_HC_listbody1_134 COMPOUND_SELECTOR_HC_listbody2_137 θws
                COMPOUND_SELECTOR=>TYPE_SELECTOR • COMPOUND_SELECTOR_HC_listbody1_134 θws
                COMPOUND_SELECTOR=>TYPE_SELECTOR • COMPOUND_SELECTOR_HC_listbody1_134 COMPOUND_SELECTOR_HC_listbody2_137
                COMPOUND_SELECTOR=>TYPE_SELECTOR • COMPOUND_SELECTOR_HC_listbody1_134
                */
                $COMPOUND_SELECTOR_HC_listbody1_134(l);

                //Look Ahead Level 0
                /*
                COMPOUND_SELECTOR=>TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody1_134 • COMPOUND_SELECTOR_HC_listbody2_137 θws
                COMPOUND_SELECTOR=>TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody1_134 • COMPOUND_SELECTOR_HC_listbody2_137
                */
                _skip(l, const_1_);
                if (l.id == 28/* \: */) {
                    //considered syms: :

                    //Parallel Transition
                    /*
                    COMPOUND_SELECTOR=>TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody1_134 • COMPOUND_SELECTOR_HC_listbody2_137 θws
                    COMPOUND_SELECTOR=>TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody1_134 • COMPOUND_SELECTOR_HC_listbody2_137
                    */
                    $COMPOUND_SELECTOR_HC_listbody2_137(l);

                    //Look Ahead Level 0
                    /*
                    
                    */
                    _skip(l, const_1_);
                    if (l.ty == 1/* \ws */) {
                        //considered syms: ws

                        //Single Production Completion

                        //peek 0

                        //block 9

                        //groups true
                        /*
                        COMPOUND_SELECTOR=>TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody1_134 COMPOUND_SELECTOR_HC_listbody2_137 • θws
                        */
                        _no_check(l);
                        if (!FAILED) {
                            setProduction(78);
                            add_reduce(4, 56);
                            return;
                        }
                    } else {
                        //considered syms: END_OF_ITEM

                        //Single Production Completion

                        //peek 0

                        //block 9

                        //groups true
                        /*
                        COMPOUND_SELECTOR=>TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody1_134 COMPOUND_SELECTOR_HC_listbody2_137 •
                        */
                        if (!FAILED) {
                            setProduction(78);
                            add_reduce(3, 56);
                            return;
                        }
                    }
                } else if (l.ty == 1/* \ws */) {
                    //considered syms: ws

                    //Single Production Completion

                    //peek 0

                    //block 7

                    //groups true
                    /*
                    COMPOUND_SELECTOR=>TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody1_134 • θws
                    */
                    _no_check(l);
                    if (!FAILED) {
                        setProduction(78);
                        add_reduce(3, 59);
                        return;
                    }
                } else {
                    //considered syms: END_OF_ITEM

                    //Single Production Completion

                    //peek 0

                    //block 7

                    //groups true
                    /*
                    COMPOUND_SELECTOR=>TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody1_134 •
                    */
                    if (!FAILED) {
                        setProduction(78);
                        add_reduce(2, 59);
                        return;
                    }
                }
            } else if (pk1.id == 28/* \: */) {
                //considered syms: :

                //Parallel Transition
                /*
                COMPOUND_SELECTOR=>TYPE_SELECTOR • COMPOUND_SELECTOR_HC_listbody2_137 θws
                COMPOUND_SELECTOR=>TYPE_SELECTOR • COMPOUND_SELECTOR_HC_listbody2_137
                */
                $COMPOUND_SELECTOR_HC_listbody2_137(l);

                //Look Ahead Level 0
                /*
                
                */
                _skip(l, const_1_);
                if (l.ty == 1/* \ws */) {
                    //considered syms: ws

                    //Single Production Completion

                    //peek 0

                    //block 7

                    //groups true
                    /*
                    COMPOUND_SELECTOR=>TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody2_137 • θws
                    */
                    _no_check(l);
                    if (!FAILED) {
                        setProduction(78);
                        add_reduce(3, 58);
                        return;
                    }
                } else {
                    //considered syms: END_OF_ITEM

                    //Single Production Completion

                    //peek 0

                    //block 7

                    //groups true
                    /*
                    COMPOUND_SELECTOR=>TYPE_SELECTOR COMPOUND_SELECTOR_HC_listbody2_137 •
                    */
                    if (!FAILED) {
                        setProduction(78);
                        add_reduce(2, 58);
                        return;
                    }
                }
            }
        } else if (l.ty == 1/* \ws */) {
            //considered syms: ws

            //Single Production Completion

            //peek 0

            //block 3

            //groups true
            /*
            COMPOUND_SELECTOR=>TYPE_SELECTOR • θws
            */
            _no_check(l);
            if (!FAILED) {
                setProduction(78);
                add_reduce(2, 62);
                return;
            }
        } else {
            //considered syms: END_OF_ITEM

            //Single Production Completion

            //peek 0

            //block 3

            //groups true
            /*
            COMPOUND_SELECTOR=>TYPE_SELECTOR •
            */
            if (!FAILED) {
                setProduction(78);
                add_reduce(1, 62);
                return;
            }
        }
    } else if (l.id == 41/* \# */ || l.id == 42/* \. */ || l.id == 43/* \[ */) {
        //considered syms: #,.,[

        //Parallel Transition
        /*
        COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody1_134 COMPOUND_SELECTOR_HC_listbody2_137 θws
        COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody1_134 θws
        COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody1_134 COMPOUND_SELECTOR_HC_listbody2_137
        COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody1_134
        */
        $COMPOUND_SELECTOR_HC_listbody1_134(l);

        //Look Ahead Level 0
        /*
        COMPOUND_SELECTOR=>COMPOUND_SELECTOR_HC_listbody1_134 • COMPOUND_SELECTOR_HC_listbody2_137 θws
        COMPOUND_SELECTOR=>COMPOUND_SELECTOR_HC_listbody1_134 • COMPOUND_SELECTOR_HC_listbody2_137
        */
        _skip(l, const_1_);
        if (l.id == 28/* \: */) {
            //considered syms: :

            //Parallel Transition
            /*
            COMPOUND_SELECTOR=>COMPOUND_SELECTOR_HC_listbody1_134 • COMPOUND_SELECTOR_HC_listbody2_137 θws
            COMPOUND_SELECTOR=>COMPOUND_SELECTOR_HC_listbody1_134 • COMPOUND_SELECTOR_HC_listbody2_137
            */
            $COMPOUND_SELECTOR_HC_listbody2_137(l);

            //Look Ahead Level 0
            /*
            
            */
            _skip(l, const_1_);
            if (l.ty == 1/* \ws */) {
                //considered syms: ws

                //Single Production Completion

                //peek 0

                //block 5

                //groups true
                /*
                COMPOUND_SELECTOR=>COMPOUND_SELECTOR_HC_listbody1_134 COMPOUND_SELECTOR_HC_listbody2_137 • θws
                */
                _no_check(l);
                if (!FAILED) {
                    setProduction(78);
                    add_reduce(3, 57);
                    return;
                }
            } else {
                //considered syms: END_OF_ITEM

                //Single Production Completion

                //peek 0

                //block 5

                //groups true
                /*
                COMPOUND_SELECTOR=>COMPOUND_SELECTOR_HC_listbody1_134 COMPOUND_SELECTOR_HC_listbody2_137 •
                */
                if (!FAILED) {
                    setProduction(78);
                    add_reduce(2, 57);
                    return;
                }
            }
        } else if (l.ty == 1/* \ws */) {
            //considered syms: ws

            //Single Production Completion

            //peek 0

            //block 3

            //groups true
            /*
            COMPOUND_SELECTOR=>COMPOUND_SELECTOR_HC_listbody1_134 • θws
            */
            _no_check(l);
            if (!FAILED) {
                setProduction(78);
                add_reduce(2, 61);
                return;
            }
        } else {
            //considered syms: END_OF_ITEM

            //Single Production Completion

            //peek 0

            //block 3

            //groups true
            /*
            COMPOUND_SELECTOR=>COMPOUND_SELECTOR_HC_listbody1_134 •
            */
            if (!FAILED) {
                setProduction(78);
                add_reduce(1, 61);
                return;
            }
        }
    } else if (l.id == 28/* \: */) {
        //considered syms: :

        //Look Ahead Level 1
        /*
        COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody1_134 COMPOUND_SELECTOR_HC_listbody2_137 θws
        COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody2_137 θws
        COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody1_134 θws
        COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody1_134 COMPOUND_SELECTOR_HC_listbody2_137
        COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody2_137
        COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody1_134
        */
        const pk1: Lexer = _pk(l.copy(), /* e.eh, */const__);
        /*COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody1_134 COMPOUND_SELECTOR_HC_listbody2_137 θws peek 1 state: 
        css_id_symbols=>• $
        css_id_symbols=>• θid
        css_id_symbols=>• θkey
        css_id_symbols=>• -
        NS_PREFIX=>• |
        NS_PREFIX_group_0150_140=>• **/

        /*COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody2_137 θws peek 1 state: 
        PSEUDO_CLASS_SELECTOR=>• : identifier PSEUDO_CLASS_SELECTOR_group_2133_138
        PSEUDO_CLASS_SELECTOR=>• : identifier*/

        /*COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody1_134 θws peek 1 state: 
        css_id_symbols=>• $
        css_id_symbols=>• θid
        css_id_symbols=>• θkey
        css_id_symbols=>• -
        NS_PREFIX=>• |
        NS_PREFIX_group_0150_140=>• **/

        /*COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody1_134 COMPOUND_SELECTOR_HC_listbody2_137 peek 1 state: 
        css_id_symbols=>• $
        css_id_symbols=>• θid
        css_id_symbols=>• θkey
        css_id_symbols=>• -
        NS_PREFIX=>• |
        NS_PREFIX_group_0150_140=>• **/

        /*COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody2_137 peek 1 state: 
        PSEUDO_CLASS_SELECTOR=>• : identifier PSEUDO_CLASS_SELECTOR_group_2133_138
        PSEUDO_CLASS_SELECTOR=>• : identifier*/

        /*COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody1_134 peek 1 state: 
        css_id_symbols=>• $
        css_id_symbols=>• θid
        css_id_symbols=>• θkey
        css_id_symbols=>• -
        NS_PREFIX=>• |
        NS_PREFIX_group_0150_140=>• **/

        if (pk1.id == 50/* \* */ || pk1.id == 51/* \| */ || pk1.id == 55/* \- */ || pk1.id == 56/* \$ */ || pk1.ty == 3/* \id */ || pk1.ty == 7/* \key */) {
            //considered syms: $,id,key,-,|,*

            //Parallel Transition
            /*
            COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody1_134 COMPOUND_SELECTOR_HC_listbody2_137 θws
            COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody1_134 θws
            COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody1_134 COMPOUND_SELECTOR_HC_listbody2_137
            COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody1_134
            */
            $COMPOUND_SELECTOR_HC_listbody1_134(l);

            //Look Ahead Level 0
            /*
            COMPOUND_SELECTOR=>COMPOUND_SELECTOR_HC_listbody1_134 • COMPOUND_SELECTOR_HC_listbody2_137 θws
            COMPOUND_SELECTOR=>COMPOUND_SELECTOR_HC_listbody1_134 • COMPOUND_SELECTOR_HC_listbody2_137
            */
            _skip(l, const_1_);
            if (l.id == 28/* \: */) {
                //considered syms: :

                //Parallel Transition
                /*
                COMPOUND_SELECTOR=>COMPOUND_SELECTOR_HC_listbody1_134 • COMPOUND_SELECTOR_HC_listbody2_137 θws
                COMPOUND_SELECTOR=>COMPOUND_SELECTOR_HC_listbody1_134 • COMPOUND_SELECTOR_HC_listbody2_137
                */
                $COMPOUND_SELECTOR_HC_listbody2_137(l);

                //Look Ahead Level 0
                /*
                
                */
                _skip(l, const_1_);
                if (l.ty == 1/* \ws */) {
                    //considered syms: ws

                    //Single Production Completion

                    //peek 0

                    //block 7

                    //groups true
                    /*
                    COMPOUND_SELECTOR=>COMPOUND_SELECTOR_HC_listbody1_134 COMPOUND_SELECTOR_HC_listbody2_137 • θws
                    */
                    _no_check(l);
                    if (!FAILED) {
                        setProduction(78);
                        add_reduce(3, 57);
                        return;
                    }
                } else {
                    //considered syms: END_OF_ITEM

                    //Single Production Completion

                    //peek 0

                    //block 7

                    //groups true
                    /*
                    COMPOUND_SELECTOR=>COMPOUND_SELECTOR_HC_listbody1_134 COMPOUND_SELECTOR_HC_listbody2_137 •
                    */
                    if (!FAILED) {
                        setProduction(78);
                        add_reduce(2, 57);
                        return;
                    }
                }
            } else if (l.ty == 1/* \ws */) {
                //considered syms: ws

                //Single Production Completion

                //peek 0

                //block 5

                //groups true
                /*
                COMPOUND_SELECTOR=>COMPOUND_SELECTOR_HC_listbody1_134 • θws
                */
                _no_check(l);
                if (!FAILED) {
                    setProduction(78);
                    add_reduce(2, 61);
                    return;
                }
            } else {
                //considered syms: END_OF_ITEM

                //Single Production Completion

                //peek 0

                //block 5

                //groups true
                /*
                COMPOUND_SELECTOR=>COMPOUND_SELECTOR_HC_listbody1_134 •
                */
                if (!FAILED) {
                    setProduction(78);
                    add_reduce(1, 61);
                    return;
                }
            }
        } else if (pk1.id == 28/* \: */) {
            //considered syms: :

            //Parallel Transition
            /*
            COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody2_137 θws
            COMPOUND_SELECTOR=>• COMPOUND_SELECTOR_HC_listbody2_137
            */
            $COMPOUND_SELECTOR_HC_listbody2_137(l);

            //Look Ahead Level 0
            /*
            
            */
            _skip(l, const_1_);
            if (l.ty == 1/* \ws */) {
                //considered syms: ws

                //Single Production Completion

                //peek 0

                //block 5

                //groups true
                /*
                COMPOUND_SELECTOR=>COMPOUND_SELECTOR_HC_listbody2_137 • θws
                */
                _no_check(l);
                if (!FAILED) {
                    setProduction(78);
                    add_reduce(2, 60);
                    return;
                }
            } else {
                //considered syms: END_OF_ITEM

                //Single Production Completion

                //peek 0

                //block 5

                //groups true
                /*
                COMPOUND_SELECTOR=>COMPOUND_SELECTOR_HC_listbody2_137 •
                */
                if (!FAILED) {
                    setProduction(78);
                    add_reduce(1, 60);
                    return;
                }
            }
        }
    }
    fail(l);
}
function $COMBINATOR(l: Lexer): void {//Production Start
    /*
    COMBINATOR=>• >
    COMBINATOR=>• +
    COMBINATOR=>• ~
    COMBINATOR=>• ||
    */
    _skip(l, const__);
    if (l.id == 31/* \> */) {
        //considered syms: >

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        COMBINATOR=>• >
        */
        _no_check(l);
        if (!FAILED) {
            setProduction(79);

            return;
        }
    } else if (l.id == 38/* \+ */) {
        //considered syms: +

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        COMBINATOR=>• +
        */
        _no_check(l);
        if (!FAILED) {
            setProduction(79);

            return;
        }
    } else if (l.id == 39/* \~ */) {
        //considered syms: ~

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        COMBINATOR=>• ~
        */
        _no_check(l);
        if (!FAILED) {
            setProduction(79);

            return;
        }
    } else if (l.id == 40/* \|| */) {
        //considered syms: ||

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        COMBINATOR=>• ||
        */
        _no_check(l);
        if (!FAILED) {
            setProduction(79);

            return;
        }
    }
    fail(l);
}
function $SUBCLASS_SELECTOR(l: Lexer): void {//Production Start
    /*
    SUBCLASS_SELECTOR=>• ID_SELECTOR
    SUBCLASS_SELECTOR=>• CLASS_SELECTOR
    SUBCLASS_SELECTOR=>• ATTRIBUTE_SELECTOR
    SUBCLASS_SELECTOR=>• PSEUDO_CLASS_SELECTOR
    ID_SELECTOR=>• # identifier
    CLASS_SELECTOR=>• . identifier
    ATTRIBUTE_SELECTOR=>• [ WQ_NAME ]
    ATTRIBUTE_SELECTOR=>• [ WQ_NAME ATTR_MATCHER ATTRIBUTE_SELECTOR_group_2137_139 ATTR_MODIFIER ]
    ATTRIBUTE_SELECTOR=>• [ WQ_NAME ATTR_MATCHER ATTRIBUTE_SELECTOR_group_2137_139 ]
    PSEUDO_CLASS_SELECTOR=>• : identifier PSEUDO_CLASS_SELECTOR_group_2133_138
    PSEUDO_CLASS_SELECTOR=>• : identifier
    */
    _skip(l, const__);
    if (l.id == 41/* \# */) {
        //considered syms: #

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        SUBCLASS_SELECTOR=>• ID_SELECTOR
        */
        $ID_SELECTOR(l);
        if (!FAILED) {
            setProduction(80);

            return;
        }
    } else if (l.id == 42/* \. */) {
        //considered syms: .

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        SUBCLASS_SELECTOR=>• CLASS_SELECTOR
        */
        $CLASS_SELECTOR(l);
        if (!FAILED) {
            setProduction(80);

            return;
        }
    } else if (l.id == 43/* \[ */) {
        //considered syms: [

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        SUBCLASS_SELECTOR=>• ATTRIBUTE_SELECTOR
        */
        $ATTRIBUTE_SELECTOR(l);
        if (!FAILED) {
            setProduction(80);

            return;
        }
    } else if (l.id == 28/* \: */) {
        //considered syms: :

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        SUBCLASS_SELECTOR=>• PSEUDO_CLASS_SELECTOR
        */
        $PSEUDO_CLASS_SELECTOR(l);
        if (!FAILED) {
            setProduction(80);

            return;
        }
    }
    fail(l);
}
function $ID_SELECTOR(l: Lexer): void {//Production Start
    /*
    ID_SELECTOR=>• # identifier
    */
    _skip(l, const__);
    //considered syms: #

    //Single Production Completion

    //peek 0

    //block 0

    //groups false
    /*
    ID_SELECTOR=>• # identifier
    */
    _with_skip(l, const__, 41/* \# */);
    if (!FAILED) {
        $identifier(l);
        if (!FAILED) {
            setProduction(81);
            add_reduce(2, 63);
            return;
        }
    }
    fail(l);
}
function $CLASS_SELECTOR(l: Lexer): void {//Production Start
    /*
    CLASS_SELECTOR=>• . identifier
    */
    _skip(l, const__);
    //considered syms: .

    //Single Production Completion

    //peek 0

    //block 0

    //groups false
    /*
    CLASS_SELECTOR=>• . identifier
    */
    _with_skip(l, const__, 42/* \. */);
    if (!FAILED) {
        $identifier(l);
        if (!FAILED) {
            setProduction(82);
            add_reduce(2, 64);
            return;
        }
    }
    fail(l);
}
function $PSEUDO_CLASS_SELECTOR_group_2133_138(l: Lexer): void {//Production Start
    /*
    PSEUDO_CLASS_SELECTOR_group_2133_138=>• ( string_value )
    */
    _skip(l, const__);
    //considered syms: (

    //Single Production Completion

    //peek 0

    //block 0

    //groups false
    /*
    PSEUDO_CLASS_SELECTOR_group_2133_138=>• ( string_value )
    */
    _with_skip(l, const_1_, 17/* \( */);
    if (!FAILED) {
        $string_value(l);
        if (!FAILED) {
            _with_skip(l, const__, 18/* \) */);
            if (!FAILED) {
                setProduction(83);
                add_reduce(3, 49);
                return;
            }
        }
    }
    fail(l);
}
function $PSEUDO_CLASS_SELECTOR(l: Lexer): void {//Production Start
    /*
    PSEUDO_CLASS_SELECTOR=>• : identifier PSEUDO_CLASS_SELECTOR_group_2133_138
    PSEUDO_CLASS_SELECTOR=>• : identifier
    */
    _skip(l, const__);
    //considered syms: :

    //Parallel Transition
    /*
    PSEUDO_CLASS_SELECTOR=>• : identifier PSEUDO_CLASS_SELECTOR_group_2133_138
    PSEUDO_CLASS_SELECTOR=>• : identifier
    */
    _no_check(l);

    //Parallel Transition
    /*
    PSEUDO_CLASS_SELECTOR=>: • identifier PSEUDO_CLASS_SELECTOR_group_2133_138
    PSEUDO_CLASS_SELECTOR=>: • identifier
    */
    $identifier(l);

    //Look Ahead Level 0
    /*
    PSEUDO_CLASS_SELECTOR=>: identifier • PSEUDO_CLASS_SELECTOR_group_2133_138
    */
    _skip(l, const__);
    if (l.id == 17/* \( */) {
        //considered syms: (

        //Single Production Completion

        //peek 0

        //block 2

        //groups true
        /*
        PSEUDO_CLASS_SELECTOR=>: identifier • PSEUDO_CLASS_SELECTOR_group_2133_138
        */
        $PSEUDO_CLASS_SELECTOR_group_2133_138(l);
        if (!FAILED) {
            setProduction(84);
            add_reduce(3, 65);
            return;
        }
    } else {
        //considered syms: END_OF_ITEM

        //Single Production Completion

        //peek 0

        //block 2

        //groups true
        /*
        PSEUDO_CLASS_SELECTOR=>: identifier •
        */
        if (!FAILED) {
            setProduction(84);
            add_reduce(2, 66);
            return;
        }
    }
    fail(l);
}
function $ATTRIBUTE_SELECTOR_group_2137_139(l: Lexer): void {//Production Start
    /*
    ATTRIBUTE_SELECTOR_group_2137_139=>• identifier
    ATTRIBUTE_SELECTOR_group_2137_139=>• " string_value "
    identifier=>• css_id_symbols θws
    identifier=>• css_id_symbols
    css_id_symbols=>• css_id_symbols θid
    css_id_symbols=>• css_id_symbols θkey
    css_id_symbols=>• css_id_symbols _
    css_id_symbols=>• css_id_symbols -
    css_id_symbols=>• css_id_symbols $
    css_id_symbols=>• css_id_symbols θnum
    css_id_symbols=>• $
    css_id_symbols=>• θid
    css_id_symbols=>• θkey
    css_id_symbols=>• -
    */
    _skip(l, const__);
    if (l.id == 55/* \- */ || l.id == 56/* \$ */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {
        //considered syms: $,id,key,-

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        ATTRIBUTE_SELECTOR_group_2137_139=>• identifier
        */
        $identifier(l);
        if (!FAILED) {
            setProduction(85);

            return;
        }
    } else if (l.id == 27/* \" */) {
        //considered syms: "

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        ATTRIBUTE_SELECTOR_group_2137_139=>• " string_value "
        */
        _no_check(l);
        if (!FAILED) {
            $string_value(l);
            if (!FAILED) {
                _with_skip(l, const__, 27/* \" */);
                if (!FAILED) {
                    setProduction(85);
                    add_reduce(3, 67);
                    return;
                }
            }
        }
    }
    fail(l);
}
function $ATTRIBUTE_SELECTOR(l: Lexer): void {//Production Start
    /*
    ATTRIBUTE_SELECTOR=>• [ WQ_NAME ]
    ATTRIBUTE_SELECTOR=>• [ WQ_NAME ATTR_MATCHER ATTRIBUTE_SELECTOR_group_2137_139 ATTR_MODIFIER ]
    ATTRIBUTE_SELECTOR=>• [ WQ_NAME ATTR_MATCHER ATTRIBUTE_SELECTOR_group_2137_139 ]
    */
    _skip(l, const__);
    //considered syms: [

    //Parallel Transition
    /*
    ATTRIBUTE_SELECTOR=>• [ WQ_NAME ]
    ATTRIBUTE_SELECTOR=>• [ WQ_NAME ATTR_MATCHER ATTRIBUTE_SELECTOR_group_2137_139 ATTR_MODIFIER ]
    ATTRIBUTE_SELECTOR=>• [ WQ_NAME ATTR_MATCHER ATTRIBUTE_SELECTOR_group_2137_139 ]
    */
    _no_check(l);

    //Parallel Transition
    /*
    ATTRIBUTE_SELECTOR=>[ • WQ_NAME ]
    ATTRIBUTE_SELECTOR=>[ • WQ_NAME ATTR_MATCHER ATTRIBUTE_SELECTOR_group_2137_139 ATTR_MODIFIER ]
    ATTRIBUTE_SELECTOR=>[ • WQ_NAME ATTR_MATCHER ATTRIBUTE_SELECTOR_group_2137_139 ]
    */
    $WQ_NAME(l);

    //Look Ahead Level 0
    /*
    ATTRIBUTE_SELECTOR=>[ WQ_NAME • ATTR_MATCHER ATTRIBUTE_SELECTOR_group_2137_139 ATTR_MODIFIER ]
    ATTRIBUTE_SELECTOR=>[ WQ_NAME • ATTR_MATCHER ATTRIBUTE_SELECTOR_group_2137_139 ]
    */
    _skip(l, const__);
    if (l.id == 30/* \= */ || l.id == 39/* \~ */ || l.id == 45/* \^= */ || l.id == 46/* \$= */ || l.id == 47/* \*= */) {
        //considered syms: ~,^=,$=,*=,=

        //Parallel Transition
        /*
        ATTRIBUTE_SELECTOR=>[ WQ_NAME • ATTR_MATCHER ATTRIBUTE_SELECTOR_group_2137_139 ATTR_MODIFIER ]
        ATTRIBUTE_SELECTOR=>[ WQ_NAME • ATTR_MATCHER ATTRIBUTE_SELECTOR_group_2137_139 ]
        */
        $ATTR_MATCHER(l);

        //Parallel Transition
        /*
        ATTRIBUTE_SELECTOR=>[ WQ_NAME ATTR_MATCHER • ATTRIBUTE_SELECTOR_group_2137_139 ATTR_MODIFIER ]
        ATTRIBUTE_SELECTOR=>[ WQ_NAME ATTR_MATCHER • ATTRIBUTE_SELECTOR_group_2137_139 ]
        */
        $ATTRIBUTE_SELECTOR_group_2137_139(l);

        //Look Ahead Level 0
        /*
        ATTRIBUTE_SELECTOR=>[ WQ_NAME ATTR_MATCHER ATTRIBUTE_SELECTOR_group_2137_139 • ATTR_MODIFIER ]
        */
        _skip(l, const__);
        if (l.id == 48/* \i */ || l.id == 49/* \s */) {
            //considered syms: i,s

            //Single Production Completion

            //peek 0

            //block 4

            //groups true
            /*
            ATTRIBUTE_SELECTOR=>[ WQ_NAME ATTR_MATCHER ATTRIBUTE_SELECTOR_group_2137_139 • ATTR_MODIFIER ]
            */
            $ATTR_MODIFIER(l);
            if (!FAILED) {
                _with_skip(l, const__, 44/* \] */);
                if (!FAILED) {
                    setProduction(86);
                    add_reduce(6, 69);
                    return;
                }
            }
        } else if (l.id == 44/* \] */) {
            //considered syms: ]

            //Single Production Completion

            //peek 0

            //block 4

            //groups true
            /*
            ATTRIBUTE_SELECTOR=>[ WQ_NAME ATTR_MATCHER ATTRIBUTE_SELECTOR_group_2137_139 • ]
            */
            _no_check(l);
            if (!FAILED) {
                setProduction(86);
                add_reduce(5, 70);
                return;
            }
        }
    } else if (l.id == 44/* \] */) {
        //considered syms: ]

        //Single Production Completion

        //peek 0

        //block 2

        //groups true
        /*
        ATTRIBUTE_SELECTOR=>[ WQ_NAME • ]
        */
        _no_check(l);
        if (!FAILED) {
            setProduction(86);
            add_reduce(3, 68);
            return;
        }
    }
    fail(l);
}
function $ATTR_MATCHER(l: Lexer): void {//Production Start
    /*
    ATTR_MATCHER=>• ~ =
    ATTR_MATCHER=>• ^=
    ATTR_MATCHER=>• $=
    ATTR_MATCHER=>• *=
    ATTR_MATCHER=>• =
    */
    _skip(l, const__);
    if (l.id == 39/* \~ */) {
        //considered syms: ~

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        ATTR_MATCHER=>• ~ =
        */
        _no_check(l);
        if (!FAILED) {
            _with_skip(l, const__, 30/* \= */);
            if (!FAILED) {
                setProduction(87);
                add_reduce(2, 0);
                return;
            }
        }
    } else if (l.id == 45/* \^= */) {
        //considered syms: ^=

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        ATTR_MATCHER=>• ^=
        */
        _no_check(l);
        if (!FAILED) {
            setProduction(87);

            return;
        }
    } else if (l.id == 46/* \$= */) {
        //considered syms: $=

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        ATTR_MATCHER=>• $=
        */
        _no_check(l);
        if (!FAILED) {
            setProduction(87);

            return;
        }
    } else if (l.id == 47/* \*= */) {
        //considered syms: *=

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        ATTR_MATCHER=>• *=
        */
        _no_check(l);
        if (!FAILED) {
            setProduction(87);

            return;
        }
    } else if (l.id == 30/* \= */) {
        //considered syms: =

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        ATTR_MATCHER=>• =
        */
        _no_check(l);
        if (!FAILED) {
            setProduction(87);

            return;
        }
    }
    fail(l);
}
function $ATTR_MODIFIER(l: Lexer): void {//Production Start
    /*
    ATTR_MODIFIER=>• τi
    ATTR_MODIFIER=>• τs
    */
    _skip(l, const__);
    if (l.id == 48/* \i */) {
        //considered syms: i

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        ATTR_MODIFIER=>• τi
        */
        _no_check(l);
        if (!FAILED) {
            setProduction(88);

            return;
        }
    } else if (l.id == 49/* \s */) {
        //considered syms: s

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        ATTR_MODIFIER=>• τs
        */
        _no_check(l);
        if (!FAILED) {
            setProduction(88);

            return;
        }
    }
    fail(l);
}

function $PSEUDO_ELEMENT_SELECTOR(l: Lexer): void {//Production Start
    /*
    PSEUDO_ELEMENT_SELECTOR=>• : PSEUDO_CLASS_SELECTOR
    */
    _skip(l, const__);
    //considered syms: :

    //Single Production Completion

    //peek 0

    //block 0

    //groups false
    /*
    PSEUDO_ELEMENT_SELECTOR=>• : PSEUDO_CLASS_SELECTOR
    */
    _with_skip(l, const__, 28/* \: */);
    if (!FAILED) {
        $PSEUDO_CLASS_SELECTOR(l);
        if (!FAILED) {
            setProduction(90);
            add_reduce(2, 74);
            return;
        }
    }
    fail(l);
}
function $NS_PREFIX_group_0150_140(l: Lexer): void {//Production Start
    /*
    NS_PREFIX_group_0150_140=>• identifier
    NS_PREFIX_group_0150_140=>• *
    identifier=>• css_id_symbols θws
    identifier=>• css_id_symbols
    css_id_symbols=>• css_id_symbols θid
    css_id_symbols=>• css_id_symbols θkey
    css_id_symbols=>• css_id_symbols _
    css_id_symbols=>• css_id_symbols -
    css_id_symbols=>• css_id_symbols $
    css_id_symbols=>• css_id_symbols θnum
    css_id_symbols=>• $
    css_id_symbols=>• θid
    css_id_symbols=>• θkey
    css_id_symbols=>• -
    */
    _skip(l, const__);
    if (l.id == 55/* \- */ || l.id == 56/* \$ */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {
        //considered syms: $,id,key,-

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        NS_PREFIX_group_0150_140=>• identifier
        */
        $identifier(l);
        if (!FAILED) {
            setProduction(91);

            return;
        }
    } else if (l.id == 50/* \* */) {
        //considered syms: *

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        NS_PREFIX_group_0150_140=>• *
        */
        _no_check(l);
        if (!FAILED) {
            setProduction(91);

            return;
        }
    }
    fail(l);
}
function $NS_PREFIX(l: Lexer): void {//Production Start
    /*
    NS_PREFIX=>• NS_PREFIX_group_0150_140 |
    NS_PREFIX=>• |
    NS_PREFIX_group_0150_140=>• identifier
    NS_PREFIX_group_0150_140=>• *
    identifier=>• css_id_symbols θws
    identifier=>• css_id_symbols
    css_id_symbols=>• css_id_symbols θid
    css_id_symbols=>• css_id_symbols θkey
    css_id_symbols=>• css_id_symbols _
    css_id_symbols=>• css_id_symbols -
    css_id_symbols=>• css_id_symbols $
    css_id_symbols=>• css_id_symbols θnum
    css_id_symbols=>• $
    css_id_symbols=>• θid
    css_id_symbols=>• θkey
    css_id_symbols=>• -
    */
    _skip(l, const__);
    if (l.id == 50/* \* */ || l.id == 55/* \- */ || l.id == 56/* \$ */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {
        //considered syms: *,$,id,key,-

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        NS_PREFIX=>• NS_PREFIX_group_0150_140 |
        */
        $NS_PREFIX_group_0150_140(l);
        if (!FAILED) {
            _with_skip(l, const__, 51/* \| */);
            if (!FAILED) {
                setProduction(92);
                add_reduce(2, 75);
                return;
            }
        }
    } else if (l.id == 51/* \| */) {
        //considered syms: |

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        NS_PREFIX=>• |
        */
        _no_check(l);
        if (!FAILED) {
            setProduction(92);
            add_reduce(1, 76);
            return;
        }
    }
    fail(l);
}


function $declaration_list_group_1153_142(l: Lexer): void {//Production Start
    /*
    declaration_list_group_1153_142=>• declaration_list_HC_listbody2_141 declaration
    declaration_list_group_1153_142=>• declaration
    declaration_list_HC_listbody2_141=>• declaration_list_HC_listbody2_141 ;
    declaration_list_HC_listbody2_141=>• ;
    declaration=>• declaration_id : declaration_values declaration_group_1155_145
    declaration=>• declaration_id : declaration_values
    declaration_id=>• css_id_symbols
    css_id_symbols=>• css_id_symbols θid
    css_id_symbols=>• css_id_symbols θkey
    css_id_symbols=>• css_id_symbols _
    css_id_symbols=>• css_id_symbols -
    css_id_symbols=>• css_id_symbols $
    css_id_symbols=>• css_id_symbols θnum
    css_id_symbols=>• $
    css_id_symbols=>• θid
    css_id_symbols=>• θkey
    css_id_symbols=>• -
    */
    _skip(l, const__);
    if (l.id == 55/* \- */ || l.id == 56/* \$ */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {
        //considered syms: $,id,key,-

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        declaration_list_group_1153_142=>• declaration
        */
        $declaration(l);
        if (!FAILED) {
            setProduction(95);

            return;
        }
    } else if (l.id == 12/* \; */) {
        //considered syms: ;

        //Single Production Completion

        //peek 0

        //block 1

        //groups true
        /*
        declaration_list_group_1153_142=>• declaration_list_HC_listbody2_141 declaration
        */
        $declaration_list_HC_listbody2_141(l);
        if (!FAILED) {
            $declaration(l);
            if (!FAILED) {
                setProduction(95);
                add_reduce(2, 0);
                return;
            }
        }
    }
    fail(l);
}


function $declaration_list(l: Lexer): void {//Production Start
    /*
    declaration_list=>• declaration_list_HC_listbody2_143 declaration_list_HC_listbody1_144
    declaration_list=>• declaration_list_HC_listbody2_143
    declaration_list_HC_listbody2_143=>• declaration_list_HC_listbody2_143 ; declaration_list_group_1153_142
    declaration_list_HC_listbody2_143=>• declaration_list_group_1153_142
    declaration_list_group_1153_142=>• declaration_list_HC_listbody2_141 declaration
    declaration_list_group_1153_142=>• declaration
    declaration_list_HC_listbody2_141=>• declaration_list_HC_listbody2_141 ;
    declaration_list_HC_listbody2_141=>• ;
    declaration=>• declaration_id : declaration_values declaration_group_1155_145
    declaration=>• declaration_id : declaration_values
    declaration_id=>• css_id_symbols
    css_id_symbols=>• css_id_symbols θid
    css_id_symbols=>• css_id_symbols θkey
    css_id_symbols=>• css_id_symbols _
    css_id_symbols=>• css_id_symbols -
    css_id_symbols=>• css_id_symbols $
    css_id_symbols=>• css_id_symbols θnum
    css_id_symbols=>• $
    css_id_symbols=>• θid
    css_id_symbols=>• θkey
    css_id_symbols=>• -
    */
    _skip(l, const__);
    //considered syms: ;,$,id,key,-

    //Parallel Transition
    /*
    declaration_list=>• declaration_list_HC_listbody2_143 declaration_list_HC_listbody1_144
    declaration_list=>• declaration_list_HC_listbody2_143
    */
    $declaration_list_HC_listbody2_143(l);

    //Look Ahead Level 0
    /*
    declaration_list=>declaration_list_HC_listbody2_143 • declaration_list_HC_listbody1_144
    */
    _skip(l, const__);
    if (l.id == 12/* \; */) {
        //considered syms: ;

        //Single Production Completion

        //peek 0

        //block 2

        //groups true
        /*
        declaration_list=>declaration_list_HC_listbody2_143 • declaration_list_HC_listbody1_144
        */
        $declaration_list_HC_listbody1_144(l);
        if (!FAILED) {
            setProduction(98);
            add_reduce(2, 75);
            return;
        }
    } else {
        //considered syms: END_OF_ITEM

        //Single Production Completion

        //peek 0

        //block 2

        //groups true
        /*
        declaration_list=>declaration_list_HC_listbody2_143 •
        */
        if (!FAILED) {
            setProduction(98);
            add_reduce(1, 75);
            return;
        }
    }
    fail(l);
}
function $declaration_group_1155_145(l: Lexer): void {//Production Start
    /*
    declaration_group_1155_145=>• ! τimportant
    */
    _skip(l, const__);
    //considered syms: !

    //Single Production Completion

    //peek 0

    //block 0

    //groups false
    /*
    declaration_group_1155_145=>• ! τimportant
    */
    _with_skip(l, const__, 52/* \! */);
    if (!FAILED) {
        _with_skip(l, const__, 53/* \important */);
        if (!FAILED) {
            setProduction(99);
            add_reduce(2, 0);
            return;
        }
    }
    fail(l);
}
function $declaration(l: Lexer): void {//Production Start
    /*
    declaration=>• declaration_id : declaration_values declaration_group_1155_145
    declaration=>• declaration_id : declaration_values
    declaration_id=>• css_id_symbols
    css_id_symbols=>• css_id_symbols θid
    css_id_symbols=>• css_id_symbols θkey
    css_id_symbols=>• css_id_symbols _
    css_id_symbols=>• css_id_symbols -
    css_id_symbols=>• css_id_symbols $
    css_id_symbols=>• css_id_symbols θnum
    css_id_symbols=>• $
    css_id_symbols=>• θid
    css_id_symbols=>• θkey
    css_id_symbols=>• -
    */
    _skip(l, const__);
    //considered syms: $,id,key,-

    //Parallel Transition
    /*
    declaration=>• declaration_id : declaration_values declaration_group_1155_145
    declaration=>• declaration_id : declaration_values
    */
    $declaration_id(l);

    //Parallel Transition
    /*
    declaration=>declaration_id • : declaration_values declaration_group_1155_145
    declaration=>declaration_id • : declaration_values
    */
    _with_skip(l, const_1_, 28/* \: */);

    //Parallel Transition
    /*
    declaration=>declaration_id : • declaration_values declaration_group_1155_145
    declaration=>declaration_id : • declaration_values
    */
    $declaration_values(l);

    //Look Ahead Level 0
    /*
    declaration=>declaration_id : declaration_values • declaration_group_1155_145
    */
    _skip(l, const__);
    if (l.id == 52/* \! */) {
        //considered syms: !

        //Single Production Completion

        //peek 0

        //block 2

        //groups true
        /*
        declaration=>declaration_id : declaration_values • declaration_group_1155_145
        */
        $declaration_group_1155_145(l);
        if (!FAILED) {
            setProduction(100);
            add_reduce(4, 79);
            return;
        }
    } else {
        //considered syms: END_OF_ITEM

        //Single Production Completion

        //peek 0

        //block 2

        //groups true
        /*
        declaration=>declaration_id : declaration_values •
        */
        if (!FAILED) {
            setProduction(100);
            add_reduce(3, 79);
            return;
        }
    }
    fail(l);
}


function $identifier(l: Lexer): void {//Production Start
    /*
    identifier=>• css_id_symbols θws
    identifier=>• css_id_symbols
    css_id_symbols=>• css_id_symbols θid
    css_id_symbols=>• css_id_symbols θkey
    css_id_symbols=>• css_id_symbols _
    css_id_symbols=>• css_id_symbols -
    css_id_symbols=>• css_id_symbols $
    css_id_symbols=>• css_id_symbols θnum
    css_id_symbols=>• $
    css_id_symbols=>• θid
    css_id_symbols=>• θkey
    css_id_symbols=>• -
    */
    _skip(l, const__);
    //considered syms: $,id,key,-

    //Parallel Transition
    /*
    identifier=>• css_id_symbols θws
    identifier=>• css_id_symbols
    */
    $css_id_symbols(l);

    //Look Ahead Level 0
    /*
    
    */
    _skip(l, const_1_);
    if (l.ty == 1/* \ws */) {
        //considered syms: ws

        //Single Production Completion

        //peek 0

        //block 2

        //groups true
        /*
        identifier=>css_id_symbols • θws
        */
        _no_check(l);
        if (!FAILED) {
            setProduction(104);
            add_reduce(2, 75);
            return;
        }
    } else {
        //considered syms: END_OF_ITEM

        //Single Production Completion

        //peek 0

        //block 2

        //groups true
        /*
        identifier=>css_id_symbols •
        */
        if (!FAILED) {
            setProduction(104);
            add_reduce(1, 75);
            return;
        }
    }
    fail(l);
}

function $declaration_id(l: Lexer): void {//Production Start
    /*
    declaration_id=>• css_id_symbols
    css_id_symbols=>• css_id_symbols θid
    css_id_symbols=>• css_id_symbols θkey
    css_id_symbols=>• css_id_symbols _
    css_id_symbols=>• css_id_symbols -
    css_id_symbols=>• css_id_symbols $
    css_id_symbols=>• css_id_symbols θnum
    css_id_symbols=>• $
    css_id_symbols=>• θid
    css_id_symbols=>• θkey
    css_id_symbols=>• -
    */
    _skip(l, const__);
    //considered syms: $,id,key,-

    //Single Production Completion

    //peek 0

    //block 0

    //groups false
    /*
    declaration_id=>• css_id_symbols
    */
    $css_id_symbols(l);
    if (!FAILED) {
        setProduction(106);

        return;
    }
    fail(l);
}

function $STYLE_SHEET_HC_listbody1_100(l: Lexer): void {
    /*
STYLE_SHEET_HC_listbody1_100=>• STYLE_SHEET_HC_listbody1_100 import|@: 1;
STYLE_SHEET_HC_listbody1_100=>• import|@: 1;
STYLE_SHEET_HC_listbody1_100=>• STYLE_SHEET_HC_listbody1_100 import|$: 1;
STYLE_SHEET_HC_listbody1_100=>• import|$: 1;
STYLE_SHEET_HC_listbody1_100=>• STYLE_SHEET_HC_listbody1_100 import|id: 1;
STYLE_SHEET_HC_listbody1_100=>• import|id: 1;
STYLE_SHEET_HC_listbody1_100=>• STYLE_SHEET_HC_listbody1_100 import|key: 1;
STYLE_SHEET_HC_listbody1_100=>• import|key: 1;
STYLE_SHEET_HC_listbody1_100=>• STYLE_SHEET_HC_listbody1_100 import|-: 1;
STYLE_SHEET_HC_listbody1_100=>• import|-: 1;
STYLE_SHEET_HC_listbody1_100=>• STYLE_SHEET_HC_listbody1_100 import|*: 1;
STYLE_SHEET_HC_listbody1_100=>• import|*: 1;
STYLE_SHEET_HC_listbody1_100=>• STYLE_SHEET_HC_listbody1_100 import||: 1;
STYLE_SHEET_HC_listbody1_100=>• import||: 1;
STYLE_SHEET_HC_listbody1_100=>• STYLE_SHEET_HC_listbody1_100 import|#: 1;
STYLE_SHEET_HC_listbody1_100=>• import|#: 1;
STYLE_SHEET_HC_listbody1_100=>• STYLE_SHEET_HC_listbody1_100 import|.: 1;
STYLE_SHEET_HC_listbody1_100=>• import|.: 1;
STYLE_SHEET_HC_listbody1_100=>• STYLE_SHEET_HC_listbody1_100 import|[: 1;
STYLE_SHEET_HC_listbody1_100=>• import|[: 1;
STYLE_SHEET_HC_listbody1_100=>• STYLE_SHEET_HC_listbody1_100 import|:: 1;
STYLE_SHEET_HC_listbody1_100=>• import|:: 1;
STYLE_SHEET_HC_listbody1_100=>• STYLE_SHEET_HC_listbody1_100 import|$eof: 1;
STYLE_SHEET_HC_listbody1_100=>• import|$eof: 1
*/
    _skip(l, const__);
    if (l.id == 14/* \@ */) {

        $import(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 14: //import
                State2(l);
                break;
            case 1: //STYLE_SHEET_HC_listbody1_100
                if (const_0_.includes(l.id) || l.ty == 0/* EOF */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) { return; }
                {
                    const cp = l.copy(), m = mark(), p = prod, s = stack_ptr;
                    State1(cp);
                    if (FAILED) {
                        prod = p;
                        FAILED = false;
                        stack_ptr = s;
                        reset(m);
                        return;
                    } else l.sync(cp);
                }
                break;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State1(l: Lexer): void {
    /*
STYLE_SHEET_HC_listbody1_100=>STYLE_SHEET_HC_listbody1_100 • import|@: 1;
STYLE_SHEET_HC_listbody1_100=>STYLE_SHEET_HC_listbody1_100 • import|$: 1;
STYLE_SHEET_HC_listbody1_100=>STYLE_SHEET_HC_listbody1_100 • import|id: 1;
STYLE_SHEET_HC_listbody1_100=>STYLE_SHEET_HC_listbody1_100 • import|key: 1;
STYLE_SHEET_HC_listbody1_100=>STYLE_SHEET_HC_listbody1_100 • import|-: 1;
STYLE_SHEET_HC_listbody1_100=>STYLE_SHEET_HC_listbody1_100 • import|*: 1;
STYLE_SHEET_HC_listbody1_100=>STYLE_SHEET_HC_listbody1_100 • import||: 1;
STYLE_SHEET_HC_listbody1_100=>STYLE_SHEET_HC_listbody1_100 • import|#: 1;
STYLE_SHEET_HC_listbody1_100=>STYLE_SHEET_HC_listbody1_100 • import|.: 1;
STYLE_SHEET_HC_listbody1_100=>STYLE_SHEET_HC_listbody1_100 • import|[: 1;
STYLE_SHEET_HC_listbody1_100=>STYLE_SHEET_HC_listbody1_100 • import|:: 1;
STYLE_SHEET_HC_listbody1_100=>STYLE_SHEET_HC_listbody1_100 • import|$eof: 1
*/
    _skip(l, const__);
    if (l.id == 14/* \@ */) {

        $import(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 14: //import
                State249(l);
                break;
            case 1/*STYLE_SHEET_HC_listbody1_100*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State2(l: Lexer): void {
    /*
STYLE_SHEET_HC_listbody1_100=>import •|@;
STYLE_SHEET_HC_listbody1_100=>import •|$;
STYLE_SHEET_HC_listbody1_100=>import •|id;
STYLE_SHEET_HC_listbody1_100=>import •|key;
STYLE_SHEET_HC_listbody1_100=>import •|-;
STYLE_SHEET_HC_listbody1_100=>import •|*;
STYLE_SHEET_HC_listbody1_100=>import •||;
STYLE_SHEET_HC_listbody1_100=>import •|#;
STYLE_SHEET_HC_listbody1_100=>import •|.;
STYLE_SHEET_HC_listbody1_100=>import •|[;
STYLE_SHEET_HC_listbody1_100=>import •|:;
STYLE_SHEET_HC_listbody1_100=>import •|$eof
*/
    _skip(l, const__);
    if (idm2r.has(l.id)) { idm2r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProduction(2, 1, 1); stack_ptr -= 1;

        ; return;
    }
    else fail(l);
}
function $keyframes_HC_listbody4_109(l: Lexer): void {
    /*
keyframes_HC_listbody4_109=>• keyframes_HC_listbody4_109 keyframes_blocks|from: 16;
keyframes_HC_listbody4_109=>• keyframes_blocks|from: 16;
keyframes_HC_listbody4_109=>• keyframes_HC_listbody4_109 keyframes_blocks|to: 16;
keyframes_HC_listbody4_109=>• keyframes_blocks|to: 16;
keyframes_HC_listbody4_109=>• keyframes_HC_listbody4_109 keyframes_blocks|num: 16;
keyframes_HC_listbody4_109=>• keyframes_blocks|num: 16;
keyframes_HC_listbody4_109=>• keyframes_HC_listbody4_109 keyframes_blocks|}: 16;
keyframes_HC_listbody4_109=>• keyframes_blocks|}: 16;
keyframes_HC_listbody4_109=>• keyframes_HC_listbody4_109 keyframes_blocks|$eof: 16;
keyframes_HC_listbody4_109=>• keyframes_blocks|$eof: 16
*/
    _skip(l, const__);
    if (l.id == 20/* \from */ || l.id == 21/* \to */) {

        $keyframes_blocks(l); stack_ptr++;

    } else if (l.ty == 2/* \num */) {

        $keyframes_blocks(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 20: //keyframes_blocks
                State6(l);
                break;
            case 16: //keyframes_HC_listbody4_109
                if (l.id == 13/* \} */ || l.ty == 0/* EOF */) { return; }
                {
                    const cp = l.copy(), m = mark(), p = prod, s = stack_ptr;
                    State5(cp);
                    if (FAILED) {
                        prod = p;
                        FAILED = false;
                        stack_ptr = s;
                        reset(m);
                        return;
                    } else l.sync(cp);
                }
                break;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State5(l: Lexer): void {
    /*
keyframes_HC_listbody4_109=>keyframes_HC_listbody4_109 • keyframes_blocks|from: 16;
keyframes_HC_listbody4_109=>keyframes_HC_listbody4_109 • keyframes_blocks|to: 16;
keyframes_HC_listbody4_109=>keyframes_HC_listbody4_109 • keyframes_blocks|num: 16;
keyframes_HC_listbody4_109=>keyframes_HC_listbody4_109 • keyframes_blocks|}: 16;
keyframes_HC_listbody4_109=>keyframes_HC_listbody4_109 • keyframes_blocks|$eof: 16
*/
    _skip(l, const__);
    if (l.id == 20/* \from */ || l.id == 21/* \to */) {

        $keyframes_blocks(l); stack_ptr++;

    } else if (l.ty == 2/* \num */) {

        $keyframes_blocks(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 20: //keyframes_blocks
                State251(l);
                break;
            case 16/*keyframes_HC_listbody4_109*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State6(l: Lexer): void {
    /*
keyframes_HC_listbody4_109=>keyframes_blocks •|from;
keyframes_HC_listbody4_109=>keyframes_blocks •|to;
keyframes_HC_listbody4_109=>keyframes_blocks •|num;
keyframes_HC_listbody4_109=>keyframes_blocks •|};
keyframes_HC_listbody4_109=>keyframes_blocks •|$eof
*/
    _skip(l, const__);
    if (l.id == 13/* \} */ || l.id == 20/* \from */ || l.id == 21/* \to */) {

        completeProduction(2, 1, 16); stack_ptr -= 1;

        ; return;
    } else if (l.ty == 0/* EOF */ || l.ty == 2/* \num */) {

        completeProduction(2, 1, 16); stack_ptr -= 1;

        ; return;
    }
    else fail(l);
}
function State8(l: Lexer): void {
    /*
keyframes_blocks_HC_listbody1_110=>keyframe_selector •|{;
keyframes_blocks_HC_listbody1_110=>keyframe_selector •|,;
keyframes_blocks_HC_listbody1_110=>keyframe_selector •|$eof
*/
    _skip(l, const__);
    if (l.id == 10/* \, */ || l.id == 11/* \{ */) {

        completeProduction(2, 1, 19); stack_ptr -= 1;

        ; return;
    } else if (l.ty == 0/* EOF */) {

        completeProduction(2, 1, 19); stack_ptr -= 1;

        ; return;
    }
    else fail(l);
}
function $keyframes_blocks_HC_listbody1_110(l: Lexer): void {
    /*
keyframes_blocks_HC_listbody1_110=>• keyframes_blocks_HC_listbody1_110 , keyframe_selector|,: 19;
keyframes_blocks_HC_listbody1_110=>• keyframe_selector|,: 19;
keyframes_blocks_HC_listbody1_110=>• keyframes_blocks_HC_listbody1_110 , keyframe_selector|{: 19;
keyframes_blocks_HC_listbody1_110=>• keyframe_selector|{: 19;
keyframes_blocks_HC_listbody1_110=>• keyframes_blocks_HC_listbody1_110 , keyframe_selector|$eof: 19;
keyframes_blocks_HC_listbody1_110=>• keyframe_selector|$eof: 19
*/
    _skip(l, const__);
    if (l.id == 20/* \from */ || l.id == 21/* \to */) {

        $keyframe_selector(l); stack_ptr++;

    } else if (l.ty == 2/* \num */) {

        $keyframe_selector(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 21: //keyframe_selector
                State8(l);
                break;
            case 19: //keyframes_blocks_HC_listbody1_110
                if (l.id == 11/* \{ */ || l.ty == 0/* EOF */) { return; }
                {
                    const cp = l.copy(), m = mark(), p = prod, s = stack_ptr;
                    State14(cp);
                    if (FAILED) {
                        prod = p;
                        FAILED = false;
                        stack_ptr = s;
                        reset(m);
                        return;
                    } else l.sync(cp);
                }
                break;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State14(l: Lexer): void {
    /*
keyframes_blocks_HC_listbody1_110=>keyframes_blocks_HC_listbody1_110 • , keyframe_selector|,;
keyframes_blocks_HC_listbody1_110=>keyframes_blocks_HC_listbody1_110 • , keyframe_selector|{;
keyframes_blocks_HC_listbody1_110=>keyframes_blocks_HC_listbody1_110 • , keyframe_selector|$eof
*/
    _skip(l, const__);
    if (l.id == 10/* \, */) {

        _no_check(l);; stack_ptr++; State253(l);

    }
    else fail(l);
}
function $supports_condition_HC_listbody2_113(l: Lexer): void {
    /*
supports_condition_HC_listbody2_113=>• supports_condition_HC_listbody2_113 supports_condition_group_129_112|and: 25;
supports_condition_HC_listbody2_113=>• supports_condition_group_129_112|and: 25;
supports_condition_HC_listbody2_113=>• supports_condition_HC_listbody2_113 supports_condition_group_129_112|or: 25;
supports_condition_HC_listbody2_113=>• supports_condition_group_129_112|or: 25;
supports_condition_HC_listbody2_113=>• supports_condition_HC_listbody2_113 supports_condition_group_129_112|): 25;
supports_condition_HC_listbody2_113=>• supports_condition_group_129_112|): 25;
supports_condition_HC_listbody2_113=>• supports_condition_HC_listbody2_113 supports_condition_group_129_112|{: 25;
supports_condition_HC_listbody2_113=>• supports_condition_group_129_112|{: 25;
supports_condition_HC_listbody2_113=>• supports_condition_HC_listbody2_113 supports_condition_group_129_112|$eof: 25;
supports_condition_HC_listbody2_113=>• supports_condition_group_129_112|$eof: 25
*/
    _skip(l, const__);
    if (l.id == 23/* \and */ || l.id == 24/* \or */) {

        $supports_condition_group_129_112(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 25: //supports_condition_HC_listbody2_113
                if (l.id == 11/* \{ */ || l.id == 18/* \) */ || l.ty == 0/* EOF */) { return; }
                {
                    const cp = l.copy(), m = mark(), p = prod, s = stack_ptr;
                    State16(cp);
                    if (FAILED) {
                        prod = p;
                        FAILED = false;
                        stack_ptr = s;
                        reset(m);
                        return;
                    } else l.sync(cp);
                }
                break;
            case 24: //supports_condition_group_129_112
                State17(l);
                break;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State16(l: Lexer): void {
    /*
supports_condition_HC_listbody2_113=>supports_condition_HC_listbody2_113 • supports_condition_group_129_112|and: 25;
supports_condition_HC_listbody2_113=>supports_condition_HC_listbody2_113 • supports_condition_group_129_112|or: 25;
supports_condition_HC_listbody2_113=>supports_condition_HC_listbody2_113 • supports_condition_group_129_112|): 25;
supports_condition_HC_listbody2_113=>supports_condition_HC_listbody2_113 • supports_condition_group_129_112|{: 25;
supports_condition_HC_listbody2_113=>supports_condition_HC_listbody2_113 • supports_condition_group_129_112|$eof: 25
*/
    _skip(l, const__);
    if (l.id == 23/* \and */ || l.id == 24/* \or */) {

        $supports_condition_group_129_112(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 24: //supports_condition_group_129_112
                State259(l);
                break;
            case 25/*supports_condition_HC_listbody2_113*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State17(l: Lexer): void {
    /*
supports_condition_HC_listbody2_113=>supports_condition_group_129_112 •|and;
supports_condition_HC_listbody2_113=>supports_condition_group_129_112 •|or;
supports_condition_HC_listbody2_113=>supports_condition_group_129_112 •|);
supports_condition_HC_listbody2_113=>supports_condition_group_129_112 •|{;
supports_condition_HC_listbody2_113=>supports_condition_group_129_112 •|$eof
*/
    _skip(l, const__);
    if (l.id == 11/* \{ */ || l.id == 18/* \) */ || l.id == 23/* \and */ || l.id == 24/* \or */) {

        completeProduction(2, 1, 25); stack_ptr -= 1;

        ; return;
    } else if (l.ty == 0/* EOF */) {

        completeProduction(2, 1, 25); stack_ptr -= 1;

        ; return;
    }
    else fail(l);
}
function $import_group_014_106(l: Lexer): void {
    /*
import_group_014_106=>• supports_condition|): 11;
import_group_014_106=>• import_declaration|): 11;
import_group_014_106=>• supports_condition|$eof: 11;
import_group_014_106=>• import_declaration|$eof: 11
*/
    _skip(l, const__);
    if (idm20.has(l.id)) { idm20.get(l.id)(l); } else if (tym20.has(l.ty)) { tym20.get(l.ty)(l); }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 26: //supports_condition
                State21(l);
                break;
            case 15: //import_declaration
                State22(l);
                break;
            case 11/*import_group_014_106*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State21(l: Lexer): void {
    /*
import_group_014_106=>supports_condition •|);
import_group_014_106=>supports_condition •|$eof
*/
    _skip(l, const__);
    if (l.id == 18/* \) */) {

        completeProductionPlain(1, 11); stack_ptr -= 1;

        ; return;
    } else if (l.ty == 0/* EOF */) {

        completeProductionPlain(1, 11); stack_ptr -= 1;

        ; return;
    }
    else fail(l);
}
function State22(l: Lexer): void {
    /*
import_group_014_106=>import_declaration •|);
import_group_014_106=>import_declaration •|$eof
*/
    _skip(l, const__);
    if (l.id == 18/* \) */) {

        completeProductionPlain(1, 11); stack_ptr -= 1;

        ; return;
    } else if (l.ty == 0/* EOF */) {

        completeProductionPlain(1, 11); stack_ptr -= 1;

        ; return;
    }
    else fail(l);
}
function State33(l: Lexer): void {
    /*
css_id_symbols=>$ •|ws;
css_id_symbols=>$ •|id;
css_id_symbols=>$ •|key;
css_id_symbols=>$ •|_;
css_id_symbols=>$ •|-;
css_id_symbols=>$ •|$;
css_id_symbols=>$ •|num;
css_id_symbols=>$ •|(;
css_id_symbols=>$ •|:;
css_id_symbols=>$ •|and;
css_id_symbols=>$ •|,;
css_id_symbols=>$ •|$eof;
css_id_symbols=>$ •|;;
css_id_symbols=>$ •|@;
css_id_symbols=>$ •|*;
css_id_symbols=>$ •||;
css_id_symbols=>$ •|#;
css_id_symbols=>$ •|.;
css_id_symbols=>$ •|[;
css_id_symbols=>$ •|{;
css_id_symbols=>$ •|<;
css_id_symbols=>$ •|>;
css_id_symbols=>$ •|=;
css_id_symbols=>$ •|+;
css_id_symbols=>$ •|~;
css_id_symbols=>$ •|||;
css_id_symbols=>$ •|);
css_id_symbols=>$ •|];
css_id_symbols=>$ •|^=;
css_id_symbols=>$ •|$=;
css_id_symbols=>$ •|*=;
css_id_symbols=>$ •|i;
css_id_symbols=>$ •|s
*/
    _skip(l, const_1_);
    if (idm33r.has(l.id)) { idm33r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 1/* \ws */ || l.ty == 2/* \num */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProductionPlain(1, 105); stack_ptr -= 1;

        ; return;
    }
    else fail(l);
}
function State34(l: Lexer): void {
    /*
css_id_symbols=>θid •|ws;
css_id_symbols=>θid •|id;
css_id_symbols=>θid •|key;
css_id_symbols=>θid •|_;
css_id_symbols=>θid •|-;
css_id_symbols=>θid •|$;
css_id_symbols=>θid •|num;
css_id_symbols=>θid •|(;
css_id_symbols=>θid •|:;
css_id_symbols=>θid •|and;
css_id_symbols=>θid •|,;
css_id_symbols=>θid •|$eof;
css_id_symbols=>θid •|;;
css_id_symbols=>θid •|@;
css_id_symbols=>θid •|*;
css_id_symbols=>θid •||;
css_id_symbols=>θid •|#;
css_id_symbols=>θid •|.;
css_id_symbols=>θid •|[;
css_id_symbols=>θid •|{;
css_id_symbols=>θid •|<;
css_id_symbols=>θid •|>;
css_id_symbols=>θid •|=;
css_id_symbols=>θid •|+;
css_id_symbols=>θid •|~;
css_id_symbols=>θid •|||;
css_id_symbols=>θid •|);
css_id_symbols=>θid •|];
css_id_symbols=>θid •|^=;
css_id_symbols=>θid •|$=;
css_id_symbols=>θid •|*=;
css_id_symbols=>θid •|i;
css_id_symbols=>θid •|s
*/
    _skip(l, const_1_);
    if (idm33r.has(l.id)) { idm33r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 1/* \ws */ || l.ty == 2/* \num */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProductionPlain(1, 105); stack_ptr -= 1;

        ; return;
    }
    else fail(l);
}
function State35(l: Lexer): void {
    /*
css_id_symbols=>θkey •|ws;
css_id_symbols=>θkey •|id;
css_id_symbols=>θkey •|key;
css_id_symbols=>θkey •|_;
css_id_symbols=>θkey •|-;
css_id_symbols=>θkey •|$;
css_id_symbols=>θkey •|num;
css_id_symbols=>θkey •|(;
css_id_symbols=>θkey •|:;
css_id_symbols=>θkey •|and;
css_id_symbols=>θkey •|,;
css_id_symbols=>θkey •|$eof;
css_id_symbols=>θkey •|;;
css_id_symbols=>θkey •|@;
css_id_symbols=>θkey •|*;
css_id_symbols=>θkey •||;
css_id_symbols=>θkey •|#;
css_id_symbols=>θkey •|.;
css_id_symbols=>θkey •|[;
css_id_symbols=>θkey •|{;
css_id_symbols=>θkey •|<;
css_id_symbols=>θkey •|>;
css_id_symbols=>θkey •|=;
css_id_symbols=>θkey •|+;
css_id_symbols=>θkey •|~;
css_id_symbols=>θkey •|||;
css_id_symbols=>θkey •|);
css_id_symbols=>θkey •|];
css_id_symbols=>θkey •|^=;
css_id_symbols=>θkey •|$=;
css_id_symbols=>θkey •|*=;
css_id_symbols=>θkey •|i;
css_id_symbols=>θkey •|s
*/
    _skip(l, const_1_);
    if (idm33r.has(l.id)) { idm33r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 1/* \ws */ || l.ty == 2/* \num */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProductionPlain(1, 105); stack_ptr -= 1;

        ; return;
    }
    else fail(l);
}
function State36(l: Lexer): void {
    /*
css_id_symbols=>- •|ws;
css_id_symbols=>- •|id;
css_id_symbols=>- •|key;
css_id_symbols=>- •|_;
css_id_symbols=>- •|-;
css_id_symbols=>- •|$;
css_id_symbols=>- •|num;
css_id_symbols=>- •|(;
css_id_symbols=>- •|:;
css_id_symbols=>- •|and;
css_id_symbols=>- •|,;
css_id_symbols=>- •|$eof;
css_id_symbols=>- •|;;
css_id_symbols=>- •|@;
css_id_symbols=>- •|*;
css_id_symbols=>- •||;
css_id_symbols=>- •|#;
css_id_symbols=>- •|.;
css_id_symbols=>- •|[;
css_id_symbols=>- •|{;
css_id_symbols=>- •|<;
css_id_symbols=>- •|>;
css_id_symbols=>- •|=;
css_id_symbols=>- •|+;
css_id_symbols=>- •|~;
css_id_symbols=>- •|||;
css_id_symbols=>- •|);
css_id_symbols=>- •|];
css_id_symbols=>- •|^=;
css_id_symbols=>- •|$=;
css_id_symbols=>- •|*=;
css_id_symbols=>- •|i;
css_id_symbols=>- •|s
*/
    _skip(l, const_1_);
    if (idm33r.has(l.id)) { idm33r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 1/* \ws */ || l.ty == 2/* \num */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProductionPlain(1, 105); stack_ptr -= 1;

        ; return;
    }
    else fail(l);
}
function State42(l: Lexer): void {
    /*
media_query=>media_condition •|,;
media_query=>media_condition •|$eof;
media_query=>media_condition •|;;
media_query=>media_condition •|@;
media_query=>media_condition •|$;
media_query=>media_condition •|id;
media_query=>media_condition •|key;
media_query=>media_condition •|-;
media_query=>media_condition •|*;
media_query=>media_condition •||;
media_query=>media_condition •|#;
media_query=>media_condition •|.;
media_query=>media_condition •|[;
media_query=>media_condition •|:;
media_query=>media_condition •|{
*/
    _skip(l, const__);
    if (idm44r.has(l.id)) { idm44r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProduction(28, 1, 37); stack_ptr -= 1;

        ; return;
    }
    else fail(l);
}
function State43(l: Lexer): void {
    /*
media_query=>media_query_group_043_116 • media_type media_query_group_144_117|,: 37;
media_query=>media_query_group_043_116 • media_type|,: 37;
media_query=>media_query_group_043_116 • media_type media_query_group_144_117|$eof: 37;
media_query=>media_query_group_043_116 • media_type|$eof: 37;
media_query=>media_query_group_043_116 • media_type media_query_group_144_117|;: 37;
media_query=>media_query_group_043_116 • media_type|;: 37;
media_query=>media_query_group_043_116 • media_type media_query_group_144_117|@: 37;
media_query=>media_query_group_043_116 • media_type|@: 37;
media_query=>media_query_group_043_116 • media_type media_query_group_144_117|$: 37;
media_query=>media_query_group_043_116 • media_type|$: 37;
media_query=>media_query_group_043_116 • media_type media_query_group_144_117|id: 37;
media_query=>media_query_group_043_116 • media_type|id: 37;
media_query=>media_query_group_043_116 • media_type media_query_group_144_117|key: 37;
media_query=>media_query_group_043_116 • media_type|key: 37;
media_query=>media_query_group_043_116 • media_type media_query_group_144_117|-: 37;
media_query=>media_query_group_043_116 • media_type|-: 37;
media_query=>media_query_group_043_116 • media_type media_query_group_144_117|*: 37;
media_query=>media_query_group_043_116 • media_type|*: 37;
media_query=>media_query_group_043_116 • media_type media_query_group_144_117||: 37;
media_query=>media_query_group_043_116 • media_type||: 37;
media_query=>media_query_group_043_116 • media_type media_query_group_144_117|#: 37;
media_query=>media_query_group_043_116 • media_type|#: 37;
media_query=>media_query_group_043_116 • media_type media_query_group_144_117|.: 37;
media_query=>media_query_group_043_116 • media_type|.: 37;
media_query=>media_query_group_043_116 • media_type media_query_group_144_117|[: 37;
media_query=>media_query_group_043_116 • media_type|[: 37;
media_query=>media_query_group_043_116 • media_type media_query_group_144_117|:: 37;
media_query=>media_query_group_043_116 • media_type|:: 37;
media_query=>media_query_group_043_116 • media_type media_query_group_144_117|{: 37;
media_query=>media_query_group_043_116 • media_type|{: 37
*/
    _skip(l, const__);
    if (l.id == 55/* \- */ || l.id == 56/* \$ */) {

        $media_type(l); stack_ptr++;

    } else if (l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        $media_type(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 61: //media_type
                State292(l);
                break;
            case 37/*media_query*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State44(l: Lexer): void {
    /*
media_query=>media_type • media_query_group_144_117|,: 37;
media_query=>media_type •|,;
media_query=>media_type • media_query_group_144_117|$eof: 37;
media_query=>media_type •|$eof;
media_query=>media_type • media_query_group_144_117|;: 37;
media_query=>media_type •|;;
media_query=>media_type • media_query_group_144_117|@: 37;
media_query=>media_type •|@;
media_query=>media_type • media_query_group_144_117|$: 37;
media_query=>media_type •|$;
media_query=>media_type • media_query_group_144_117|id: 37;
media_query=>media_type •|id;
media_query=>media_type • media_query_group_144_117|key: 37;
media_query=>media_type •|key;
media_query=>media_type • media_query_group_144_117|-: 37;
media_query=>media_type •|-;
media_query=>media_type • media_query_group_144_117|*: 37;
media_query=>media_type •|*;
media_query=>media_type • media_query_group_144_117||: 37;
media_query=>media_type •||;
media_query=>media_type • media_query_group_144_117|#: 37;
media_query=>media_type •|#;
media_query=>media_type • media_query_group_144_117|.: 37;
media_query=>media_type •|.;
media_query=>media_type • media_query_group_144_117|[: 37;
media_query=>media_type •|[;
media_query=>media_type • media_query_group_144_117|:: 37;
media_query=>media_type •|:;
media_query=>media_type • media_query_group_144_117|{: 37;
media_query=>media_type •|{
*/
    _skip(l, const__);
    if (l.id == 23/* \and */) {

        $media_query_group_144_117(l); stack_ptr++;

    } else if (idm44r.has(l.id)) { idm44r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProduction(28, 1, 37); stack_ptr -= 1;

        ; return;
    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 36: //media_query_group_144_117
                State282(l);
                break;
            case 37/*media_query*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State45(l: Lexer): void {
    /*
media_condition=>media_condition_without_or •|,;
media_condition=>media_condition_without_or •|$eof;
media_condition=>media_condition_without_or •|;;
media_condition=>media_condition_without_or •|@;
media_condition=>media_condition_without_or •|$;
media_condition=>media_condition_without_or •|id;
media_condition=>media_condition_without_or •|key;
media_condition=>media_condition_without_or •|-;
media_condition=>media_condition_without_or •|*;
media_condition=>media_condition_without_or •||;
media_condition=>media_condition_without_or •|#;
media_condition=>media_condition_without_or •|.;
media_condition=>media_condition_without_or •|[;
media_condition=>media_condition_without_or •|:;
media_condition=>media_condition_without_or •|{;
media_condition=>media_condition_without_or •|)
*/
    _skip(l, const__);
    if (idm46r.has(l.id)) { idm46r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProductionPlain(1, 38); stack_ptr -= 1;

        ; return;
    }
    else fail(l);
}
function State46(l: Lexer): void {
    /*
media_condition=>media_or •|,;
media_condition=>media_or •|$eof;
media_condition=>media_or •|;;
media_condition=>media_or •|@;
media_condition=>media_or •|$;
media_condition=>media_or •|id;
media_condition=>media_or •|key;
media_condition=>media_or •|-;
media_condition=>media_or •|*;
media_condition=>media_or •||;
media_condition=>media_or •|#;
media_condition=>media_or •|.;
media_condition=>media_or •|[;
media_condition=>media_or •|:;
media_condition=>media_or •|{;
media_condition=>media_or •|)
*/
    _skip(l, const__);
    if (idm46r.has(l.id)) { idm46r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProductionPlain(1, 38); stack_ptr -= 1;

        ; return;
    }
    else fail(l);
}
function State47(l: Lexer): void {
    /*
media_condition_without_or=>media_not •|,;
media_condition_without_or=>media_not •|$eof;
media_condition_without_or=>media_not •|;;
media_condition_without_or=>media_not •|@;
media_condition_without_or=>media_not •|$;
media_condition_without_or=>media_not •|id;
media_condition_without_or=>media_not •|key;
media_condition_without_or=>media_not •|-;
media_condition_without_or=>media_not •|*;
media_condition_without_or=>media_not •||;
media_condition_without_or=>media_not •|#;
media_condition_without_or=>media_not •|.;
media_condition_without_or=>media_not •|[;
media_condition_without_or=>media_not •|:;
media_condition_without_or=>media_not •|{;
media_condition_without_or=>media_not •|)
*/
    _skip(l, const__);
    if (idm353r.has(l.id)) { idm353r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProductionPlain(1, 39); stack_ptr -= 1;

        ; return;
    }
    else fail(l);
}
function State48(l: Lexer): void {
    /*
media_condition_without_or=>media_and •|,;
media_condition_without_or=>media_and •|$eof;
media_condition_without_or=>media_and •|;;
media_condition_without_or=>media_and •|@;
media_condition_without_or=>media_and •|$;
media_condition_without_or=>media_and •|id;
media_condition_without_or=>media_and •|key;
media_condition_without_or=>media_and •|-;
media_condition_without_or=>media_and •|*;
media_condition_without_or=>media_and •||;
media_condition_without_or=>media_and •|#;
media_condition_without_or=>media_and •|.;
media_condition_without_or=>media_and •|[;
media_condition_without_or=>media_and •|:;
media_condition_without_or=>media_and •|{;
media_condition_without_or=>media_and •|)
*/
    _skip(l, const__);
    if (idm353r.has(l.id)) { idm353r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProductionPlain(1, 39); stack_ptr -= 1;

        ; return;
    }
    else fail(l);
}
function $import_HC_listbody4_108(l: Lexer): void {
    /*
import_HC_listbody4_108=>• import_HC_listbody4_108 , media_query|,: 13;
import_HC_listbody4_108=>• media_query|,: 13;
import_HC_listbody4_108=>• import_HC_listbody4_108 , media_query|;: 13;
import_HC_listbody4_108=>• media_query|;: 13;
import_HC_listbody4_108=>• import_HC_listbody4_108 , media_query|@: 13;
import_HC_listbody4_108=>• media_query|@: 13;
import_HC_listbody4_108=>• import_HC_listbody4_108 , media_query|$: 13;
import_HC_listbody4_108=>• media_query|$: 13;
import_HC_listbody4_108=>• import_HC_listbody4_108 , media_query|id: 13;
import_HC_listbody4_108=>• media_query|id: 13;
import_HC_listbody4_108=>• import_HC_listbody4_108 , media_query|key: 13;
import_HC_listbody4_108=>• media_query|key: 13;
import_HC_listbody4_108=>• import_HC_listbody4_108 , media_query|-: 13;
import_HC_listbody4_108=>• media_query|-: 13;
import_HC_listbody4_108=>• import_HC_listbody4_108 , media_query|*: 13;
import_HC_listbody4_108=>• media_query|*: 13;
import_HC_listbody4_108=>• import_HC_listbody4_108 , media_query||: 13;
import_HC_listbody4_108=>• media_query||: 13;
import_HC_listbody4_108=>• import_HC_listbody4_108 , media_query|#: 13;
import_HC_listbody4_108=>• media_query|#: 13;
import_HC_listbody4_108=>• import_HC_listbody4_108 , media_query|.: 13;
import_HC_listbody4_108=>• media_query|.: 13;
import_HC_listbody4_108=>• import_HC_listbody4_108 , media_query|[: 13;
import_HC_listbody4_108=>• media_query|[: 13;
import_HC_listbody4_108=>• import_HC_listbody4_108 , media_query|:: 13;
import_HC_listbody4_108=>• media_query|:: 13;
import_HC_listbody4_108=>• import_HC_listbody4_108 , media_query|$eof: 13;
import_HC_listbody4_108=>• media_query|$eof: 13
*/
    _skip(l, const__);
    if (l.id == 17/* \( */ || l.id == 22/* \not */ || l.id == 26/* \only */ || l.id == 55/* \- */ || l.id == 56/* \$ */) {

        $media_query(l); stack_ptr++;

    } else if (l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        $media_query(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 37: //media_query
                State59(l);
                break;
            case 13: //import_HC_listbody4_108
                if (const_2_.includes(l.id) || l.ty == 0/* EOF */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) { return; }
                {
                    const cp = l.copy(), m = mark(), p = prod, s = stack_ptr;
                    State58(cp);
                    if (FAILED) {
                        prod = p;
                        FAILED = false;
                        stack_ptr = s;
                        reset(m);
                        return;
                    } else l.sync(cp);
                }
                break;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State58(l: Lexer): void {
    /*
import_HC_listbody4_108=>import_HC_listbody4_108 • , media_query|,;
import_HC_listbody4_108=>import_HC_listbody4_108 • , media_query|;;
import_HC_listbody4_108=>import_HC_listbody4_108 • , media_query|@;
import_HC_listbody4_108=>import_HC_listbody4_108 • , media_query|$;
import_HC_listbody4_108=>import_HC_listbody4_108 • , media_query|id;
import_HC_listbody4_108=>import_HC_listbody4_108 • , media_query|key;
import_HC_listbody4_108=>import_HC_listbody4_108 • , media_query|-;
import_HC_listbody4_108=>import_HC_listbody4_108 • , media_query|*;
import_HC_listbody4_108=>import_HC_listbody4_108 • , media_query||;
import_HC_listbody4_108=>import_HC_listbody4_108 • , media_query|#;
import_HC_listbody4_108=>import_HC_listbody4_108 • , media_query|.;
import_HC_listbody4_108=>import_HC_listbody4_108 • , media_query|[;
import_HC_listbody4_108=>import_HC_listbody4_108 • , media_query|:;
import_HC_listbody4_108=>import_HC_listbody4_108 • , media_query|$eof
*/
    _skip(l, const__);
    if (l.id == 10/* \, */) {

        _no_check(l);; stack_ptr++; State294(l);

    }
    else fail(l);
}
function State59(l: Lexer): void {
    /*
import_HC_listbody4_108=>media_query •|,;
import_HC_listbody4_108=>media_query •|;;
import_HC_listbody4_108=>media_query •|@;
import_HC_listbody4_108=>media_query •|$;
import_HC_listbody4_108=>media_query •|id;
import_HC_listbody4_108=>media_query •|key;
import_HC_listbody4_108=>media_query •|-;
import_HC_listbody4_108=>media_query •|*;
import_HC_listbody4_108=>media_query •||;
import_HC_listbody4_108=>media_query •|#;
import_HC_listbody4_108=>media_query •|.;
import_HC_listbody4_108=>media_query •|[;
import_HC_listbody4_108=>media_query •|:;
import_HC_listbody4_108=>media_query •|$eof
*/
    _skip(l, const__);
    if (idm59r.has(l.id)) { idm59r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProduction(2, 1, 13); stack_ptr -= 1;

        ; return;
    }
    else fail(l);
}
function $media_queries_group_039_115(l: Lexer): void {
    /*
media_queries_group_039_115=>• media_queries_group_039_115 , media_query|,: 33;
media_queries_group_039_115=>• media_query|,: 33;
media_queries_group_039_115=>• media_queries_group_039_115 , media_query|{: 33;
media_queries_group_039_115=>• media_query|{: 33;
media_queries_group_039_115=>• media_queries_group_039_115 , media_query|$eof: 33;
media_queries_group_039_115=>• media_query|$eof: 33
*/
    _skip(l, const__);
    if (l.id == 17/* \( */ || l.id == 22/* \not */ || l.id == 26/* \only */ || l.id == 55/* \- */ || l.id == 56/* \$ */) {

        $media_query(l); stack_ptr++;

    } else if (l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        $media_query(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 37: //media_query
                State62(l);
                break;
            case 33: //media_queries_group_039_115
                if (l.id == 11/* \{ */ || l.ty == 0/* EOF */) { return; }
                {
                    const cp = l.copy(), m = mark(), p = prod, s = stack_ptr;
                    State61(cp);
                    if (FAILED) {
                        prod = p;
                        FAILED = false;
                        stack_ptr = s;
                        reset(m);
                        return;
                    } else l.sync(cp);
                }
                break;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State61(l: Lexer): void {
    /*
media_queries_group_039_115=>media_queries_group_039_115 • , media_query|,;
media_queries_group_039_115=>media_queries_group_039_115 • , media_query|{;
media_queries_group_039_115=>media_queries_group_039_115 • , media_query|$eof
*/
    _skip(l, const__);
    if (l.id == 10/* \, */) {

        _no_check(l);; stack_ptr++; State299(l);

    }
    else fail(l);
}
function State62(l: Lexer): void {
    /*
media_queries_group_039_115=>media_query •|,;
media_queries_group_039_115=>media_query •|{;
media_queries_group_039_115=>media_query •|$eof
*/
    _skip(l, const__);
    if (l.id == 10/* \, */ || l.id == 11/* \{ */) {

        completeProduction(2, 1, 33); stack_ptr -= 1;

        ; return;
    } else if (l.ty == 0/* EOF */) {

        completeProduction(2, 1, 33); stack_ptr -= 1;

        ; return;
    }
    else fail(l);
}
function $media_and_HC_listbody2_119(l: Lexer): void {
    /*
media_and_HC_listbody2_119=>• media_and_HC_listbody2_119 media_and_group_152_118|and: 42;
media_and_HC_listbody2_119=>• media_and_group_152_118|and: 42;
media_and_HC_listbody2_119=>• media_and_HC_listbody2_119 media_and_group_152_118|,: 42;
media_and_HC_listbody2_119=>• media_and_group_152_118|,: 42;
media_and_HC_listbody2_119=>• media_and_HC_listbody2_119 media_and_group_152_118|;: 42;
media_and_HC_listbody2_119=>• media_and_group_152_118|;: 42;
media_and_HC_listbody2_119=>• media_and_HC_listbody2_119 media_and_group_152_118|@: 42;
media_and_HC_listbody2_119=>• media_and_group_152_118|@: 42;
media_and_HC_listbody2_119=>• media_and_HC_listbody2_119 media_and_group_152_118|$: 42;
media_and_HC_listbody2_119=>• media_and_group_152_118|$: 42;
media_and_HC_listbody2_119=>• media_and_HC_listbody2_119 media_and_group_152_118|id: 42;
media_and_HC_listbody2_119=>• media_and_group_152_118|id: 42;
media_and_HC_listbody2_119=>• media_and_HC_listbody2_119 media_and_group_152_118|key: 42;
media_and_HC_listbody2_119=>• media_and_group_152_118|key: 42;
media_and_HC_listbody2_119=>• media_and_HC_listbody2_119 media_and_group_152_118|-: 42;
media_and_HC_listbody2_119=>• media_and_group_152_118|-: 42;
media_and_HC_listbody2_119=>• media_and_HC_listbody2_119 media_and_group_152_118|*: 42;
media_and_HC_listbody2_119=>• media_and_group_152_118|*: 42;
media_and_HC_listbody2_119=>• media_and_HC_listbody2_119 media_and_group_152_118||: 42;
media_and_HC_listbody2_119=>• media_and_group_152_118||: 42;
media_and_HC_listbody2_119=>• media_and_HC_listbody2_119 media_and_group_152_118|#: 42;
media_and_HC_listbody2_119=>• media_and_group_152_118|#: 42;
media_and_HC_listbody2_119=>• media_and_HC_listbody2_119 media_and_group_152_118|.: 42;
media_and_HC_listbody2_119=>• media_and_group_152_118|.: 42;
media_and_HC_listbody2_119=>• media_and_HC_listbody2_119 media_and_group_152_118|[: 42;
media_and_HC_listbody2_119=>• media_and_group_152_118|[: 42;
media_and_HC_listbody2_119=>• media_and_HC_listbody2_119 media_and_group_152_118|:: 42;
media_and_HC_listbody2_119=>• media_and_group_152_118|:: 42;
media_and_HC_listbody2_119=>• media_and_HC_listbody2_119 media_and_group_152_118|$eof: 42;
media_and_HC_listbody2_119=>• media_and_group_152_118|$eof: 42;
media_and_HC_listbody2_119=>• media_and_HC_listbody2_119 media_and_group_152_118|{: 42;
media_and_HC_listbody2_119=>• media_and_group_152_118|{: 42;
media_and_HC_listbody2_119=>• media_and_HC_listbody2_119 media_and_group_152_118|): 42;
media_and_HC_listbody2_119=>• media_and_group_152_118|): 42
*/
    _skip(l, const__);
    if (l.id == 23/* \and */) {

        $media_and_group_152_118(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 42: //media_and_HC_listbody2_119
                if (const_5_.includes(l.id) || l.ty == 0/* EOF */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) { return; }
                {
                    const cp = l.copy(), m = mark(), p = prod, s = stack_ptr;
                    State64(cp);
                    if (FAILED) {
                        prod = p;
                        FAILED = false;
                        stack_ptr = s;
                        reset(m);
                        return;
                    } else l.sync(cp);
                }
                break;
            case 41: //media_and_group_152_118
                State65(l);
                break;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State64(l: Lexer): void {
    /*
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 • media_and_group_152_118|and: 42;
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 • media_and_group_152_118|,: 42;
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 • media_and_group_152_118|;: 42;
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 • media_and_group_152_118|@: 42;
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 • media_and_group_152_118|$: 42;
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 • media_and_group_152_118|id: 42;
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 • media_and_group_152_118|key: 42;
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 • media_and_group_152_118|-: 42;
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 • media_and_group_152_118|*: 42;
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 • media_and_group_152_118||: 42;
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 • media_and_group_152_118|#: 42;
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 • media_and_group_152_118|.: 42;
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 • media_and_group_152_118|[: 42;
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 • media_and_group_152_118|:: 42;
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 • media_and_group_152_118|$eof: 42;
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 • media_and_group_152_118|{: 42;
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 • media_and_group_152_118|): 42
*/
    _skip(l, const__);
    if (l.id == 23/* \and */) {

        $media_and_group_152_118(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 41: //media_and_group_152_118
                State301(l);
                break;
            case 42/*media_and_HC_listbody2_119*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State65(l: Lexer): void {
    /*
media_and_HC_listbody2_119=>media_and_group_152_118 •|and;
media_and_HC_listbody2_119=>media_and_group_152_118 •|,;
media_and_HC_listbody2_119=>media_and_group_152_118 •|;;
media_and_HC_listbody2_119=>media_and_group_152_118 •|@;
media_and_HC_listbody2_119=>media_and_group_152_118 •|$;
media_and_HC_listbody2_119=>media_and_group_152_118 •|id;
media_and_HC_listbody2_119=>media_and_group_152_118 •|key;
media_and_HC_listbody2_119=>media_and_group_152_118 •|-;
media_and_HC_listbody2_119=>media_and_group_152_118 •|*;
media_and_HC_listbody2_119=>media_and_group_152_118 •||;
media_and_HC_listbody2_119=>media_and_group_152_118 •|#;
media_and_HC_listbody2_119=>media_and_group_152_118 •|.;
media_and_HC_listbody2_119=>media_and_group_152_118 •|[;
media_and_HC_listbody2_119=>media_and_group_152_118 •|:;
media_and_HC_listbody2_119=>media_and_group_152_118 •|$eof;
media_and_HC_listbody2_119=>media_and_group_152_118 •|{;
media_and_HC_listbody2_119=>media_and_group_152_118 •|)
*/
    _skip(l, const__);
    if (idm65r.has(l.id)) { idm65r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProduction(2, 1, 42); stack_ptr -= 1;

        ; return;
    }
    else fail(l);
}
function $media_or_HC_listbody2_121(l: Lexer): void {
    /*
media_or_HC_listbody2_121=>• media_or_HC_listbody2_121 media_or_group_154_120|or: 45;
media_or_HC_listbody2_121=>• media_or_group_154_120|or: 45;
media_or_HC_listbody2_121=>• media_or_HC_listbody2_121 media_or_group_154_120|): 45;
media_or_HC_listbody2_121=>• media_or_group_154_120|): 45;
media_or_HC_listbody2_121=>• media_or_HC_listbody2_121 media_or_group_154_120|,: 45;
media_or_HC_listbody2_121=>• media_or_group_154_120|,: 45;
media_or_HC_listbody2_121=>• media_or_HC_listbody2_121 media_or_group_154_120|;: 45;
media_or_HC_listbody2_121=>• media_or_group_154_120|;: 45;
media_or_HC_listbody2_121=>• media_or_HC_listbody2_121 media_or_group_154_120|@: 45;
media_or_HC_listbody2_121=>• media_or_group_154_120|@: 45;
media_or_HC_listbody2_121=>• media_or_HC_listbody2_121 media_or_group_154_120|$: 45;
media_or_HC_listbody2_121=>• media_or_group_154_120|$: 45;
media_or_HC_listbody2_121=>• media_or_HC_listbody2_121 media_or_group_154_120|id: 45;
media_or_HC_listbody2_121=>• media_or_group_154_120|id: 45;
media_or_HC_listbody2_121=>• media_or_HC_listbody2_121 media_or_group_154_120|key: 45;
media_or_HC_listbody2_121=>• media_or_group_154_120|key: 45;
media_or_HC_listbody2_121=>• media_or_HC_listbody2_121 media_or_group_154_120|-: 45;
media_or_HC_listbody2_121=>• media_or_group_154_120|-: 45;
media_or_HC_listbody2_121=>• media_or_HC_listbody2_121 media_or_group_154_120|*: 45;
media_or_HC_listbody2_121=>• media_or_group_154_120|*: 45;
media_or_HC_listbody2_121=>• media_or_HC_listbody2_121 media_or_group_154_120||: 45;
media_or_HC_listbody2_121=>• media_or_group_154_120||: 45;
media_or_HC_listbody2_121=>• media_or_HC_listbody2_121 media_or_group_154_120|#: 45;
media_or_HC_listbody2_121=>• media_or_group_154_120|#: 45;
media_or_HC_listbody2_121=>• media_or_HC_listbody2_121 media_or_group_154_120|.: 45;
media_or_HC_listbody2_121=>• media_or_group_154_120|.: 45;
media_or_HC_listbody2_121=>• media_or_HC_listbody2_121 media_or_group_154_120|[: 45;
media_or_HC_listbody2_121=>• media_or_group_154_120|[: 45;
media_or_HC_listbody2_121=>• media_or_HC_listbody2_121 media_or_group_154_120|:: 45;
media_or_HC_listbody2_121=>• media_or_group_154_120|:: 45;
media_or_HC_listbody2_121=>• media_or_HC_listbody2_121 media_or_group_154_120|$eof: 45;
media_or_HC_listbody2_121=>• media_or_group_154_120|$eof: 45;
media_or_HC_listbody2_121=>• media_or_HC_listbody2_121 media_or_group_154_120|{: 45;
media_or_HC_listbody2_121=>• media_or_group_154_120|{: 45
*/
    _skip(l, const__);
    if (l.id == 24/* \or */) {

        $media_or_group_154_120(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 45: //media_or_HC_listbody2_121
                if (const_5_.includes(l.id) || l.ty == 0/* EOF */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) { return; }
                {
                    const cp = l.copy(), m = mark(), p = prod, s = stack_ptr;
                    State68(cp);
                    if (FAILED) {
                        prod = p;
                        FAILED = false;
                        stack_ptr = s;
                        reset(m);
                        return;
                    } else l.sync(cp);
                }
                break;
            case 44: //media_or_group_154_120
                State69(l);
                break;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State68(l: Lexer): void {
    /*
media_or_HC_listbody2_121=>media_or_HC_listbody2_121 • media_or_group_154_120|or: 45;
media_or_HC_listbody2_121=>media_or_HC_listbody2_121 • media_or_group_154_120|): 45;
media_or_HC_listbody2_121=>media_or_HC_listbody2_121 • media_or_group_154_120|,: 45;
media_or_HC_listbody2_121=>media_or_HC_listbody2_121 • media_or_group_154_120|;: 45;
media_or_HC_listbody2_121=>media_or_HC_listbody2_121 • media_or_group_154_120|@: 45;
media_or_HC_listbody2_121=>media_or_HC_listbody2_121 • media_or_group_154_120|$: 45;
media_or_HC_listbody2_121=>media_or_HC_listbody2_121 • media_or_group_154_120|id: 45;
media_or_HC_listbody2_121=>media_or_HC_listbody2_121 • media_or_group_154_120|key: 45;
media_or_HC_listbody2_121=>media_or_HC_listbody2_121 • media_or_group_154_120|-: 45;
media_or_HC_listbody2_121=>media_or_HC_listbody2_121 • media_or_group_154_120|*: 45;
media_or_HC_listbody2_121=>media_or_HC_listbody2_121 • media_or_group_154_120||: 45;
media_or_HC_listbody2_121=>media_or_HC_listbody2_121 • media_or_group_154_120|#: 45;
media_or_HC_listbody2_121=>media_or_HC_listbody2_121 • media_or_group_154_120|.: 45;
media_or_HC_listbody2_121=>media_or_HC_listbody2_121 • media_or_group_154_120|[: 45;
media_or_HC_listbody2_121=>media_or_HC_listbody2_121 • media_or_group_154_120|:: 45;
media_or_HC_listbody2_121=>media_or_HC_listbody2_121 • media_or_group_154_120|$eof: 45;
media_or_HC_listbody2_121=>media_or_HC_listbody2_121 • media_or_group_154_120|{: 45
*/
    _skip(l, const__);
    if (l.id == 24/* \or */) {

        $media_or_group_154_120(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 44: //media_or_group_154_120
                State300(l);
                break;
            case 45/*media_or_HC_listbody2_121*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State69(l: Lexer): void {
    /*
media_or_HC_listbody2_121=>media_or_group_154_120 •|or;
media_or_HC_listbody2_121=>media_or_group_154_120 •|);
media_or_HC_listbody2_121=>media_or_group_154_120 •|,;
media_or_HC_listbody2_121=>media_or_group_154_120 •|;;
media_or_HC_listbody2_121=>media_or_group_154_120 •|@;
media_or_HC_listbody2_121=>media_or_group_154_120 •|$;
media_or_HC_listbody2_121=>media_or_group_154_120 •|id;
media_or_HC_listbody2_121=>media_or_group_154_120 •|key;
media_or_HC_listbody2_121=>media_or_group_154_120 •|-;
media_or_HC_listbody2_121=>media_or_group_154_120 •|*;
media_or_HC_listbody2_121=>media_or_group_154_120 •||;
media_or_HC_listbody2_121=>media_or_group_154_120 •|#;
media_or_HC_listbody2_121=>media_or_group_154_120 •|.;
media_or_HC_listbody2_121=>media_or_group_154_120 •|[;
media_or_HC_listbody2_121=>media_or_group_154_120 •|:;
media_or_HC_listbody2_121=>media_or_group_154_120 •|$eof;
media_or_HC_listbody2_121=>media_or_group_154_120 •|{
*/
    _skip(l, const__);
    if (idm69r.has(l.id)) { idm69r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProduction(2, 1, 45); stack_ptr -= 1;

        ; return;
    }
    else fail(l);
}
function $media_feature_group_061_122(l: Lexer): void {
    /*
media_feature_group_061_122=>• mf_plain|): 48;
media_feature_group_061_122=>• mf_boolean|): 48;
media_feature_group_061_122=>• mf_range|): 48;
media_feature_group_061_122=>• mf_plain|$eof: 48;
media_feature_group_061_122=>• mf_boolean|$eof: 48;
media_feature_group_061_122=>• mf_range|$eof: 48
*/
    _skip(l, const__);
    if (idm71.has(l.id)) { idm71.get(l.id)(l); } else if (tym71.has(l.ty)) { tym71.get(l.ty)(l); }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 59: //mf_boolean
                State73(l);
                break;
            case 57: //mf_range
                State74(l);
                break;
            case 53: //mf_plain
                State72(l);
                break;
            case 48/*media_feature_group_061_122*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State72(l: Lexer): void {
    /*
media_feature_group_061_122=>mf_plain •|);
media_feature_group_061_122=>mf_plain •|$eof
*/
    _skip(l, const__);
    if (l.id == 18/* \) */) {

        completeProductionPlain(1, 48); stack_ptr -= 1;

        ; return;
    } else if (l.ty == 0/* EOF */) {

        completeProductionPlain(1, 48); stack_ptr -= 1;

        ; return;
    }
    else fail(l);
}
function State73(l: Lexer): void {
    /*
media_feature_group_061_122=>mf_boolean •|);
media_feature_group_061_122=>mf_boolean •|$eof
*/
    _skip(l, const__);
    if (l.id == 18/* \) */) {

        completeProductionPlain(1, 48); stack_ptr -= 1;

        ; return;
    } else if (l.ty == 0/* EOF */) {

        completeProductionPlain(1, 48); stack_ptr -= 1;

        ; return;
    }
    else fail(l);
}
function State74(l: Lexer): void {
    /*
media_feature_group_061_122=>mf_range •|);
media_feature_group_061_122=>mf_range •|$eof
*/
    _skip(l, const__);
    if (l.id == 18/* \) */) {

        completeProductionPlain(1, 48); stack_ptr -= 1;

        ; return;
    } else if (l.ty == 0/* EOF */) {

        completeProductionPlain(1, 48); stack_ptr -= 1;

        ; return;
    }
    else fail(l);
}
function State79(l: Lexer): void {
    /*
mf_range=>mf_value • mf_range_group_074_125 mf_name|): 57;
mf_range=>mf_value • mf_range_group_183_126 identifier mf_range_group_183_126 mf_value|): 57;
mf_range=>mf_value • mf_range_group_188_127 identifier mf_range_group_188_127 mf_value|): 57;
mf_range=>mf_value • mf_range_group_074_125 mf_name|$eof: 57;
mf_range=>mf_value • mf_range_group_183_126 identifier mf_range_group_183_126 mf_value|$eof: 57;
mf_range=>mf_value • mf_range_group_188_127 identifier mf_range_group_188_127 mf_value|$eof: 57
*/
    _skip(l, const__);
    if (idm79.has(l.id)) { idm79.get(l.id)(l); }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 56: //mf_range_group_188_127
                State310(l);
                break;
            case 55: //mf_range_group_183_126
                State309(l);
                break;
            case 54: //mf_range_group_074_125
                State308(l);
                break;
            case 57/*mf_range*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function $STYLE_RULE_HC_listbody2_103(l: Lexer): void {
    /*
STYLE_RULE_HC_listbody2_103=>• STYLE_RULE_HC_listbody2_103 , COMPLEX_SELECTOR|,: 5;
STYLE_RULE_HC_listbody2_103=>• COMPLEX_SELECTOR|,: 5;
STYLE_RULE_HC_listbody2_103=>• STYLE_RULE_HC_listbody2_103 , COMPLEX_SELECTOR|{: 5;
STYLE_RULE_HC_listbody2_103=>• COMPLEX_SELECTOR|{: 5;
STYLE_RULE_HC_listbody2_103=>• STYLE_RULE_HC_listbody2_103 , COMPLEX_SELECTOR|$eof: 5;
STYLE_RULE_HC_listbody2_103=>• COMPLEX_SELECTOR|$eof: 5
*/
    _skip(l, const__);
    if (idm315.has(l.id)) { idm315.get(l.id)(l); } else if (l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        $COMPLEX_SELECTOR(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 73: //COMPLEX_SELECTOR
                State85(l);
                break;
            case 5: //STYLE_RULE_HC_listbody2_103
                if (l.id == 11/* \{ */ || l.ty == 0/* EOF */) { return; }
                {
                    const cp = l.copy(), m = mark(), p = prod, s = stack_ptr;
                    State84(cp);
                    if (FAILED) {
                        prod = p;
                        FAILED = false;
                        stack_ptr = s;
                        reset(m);
                        return;
                    } else l.sync(cp);
                }
                break;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State84(l: Lexer): void {
    /*
STYLE_RULE_HC_listbody2_103=>STYLE_RULE_HC_listbody2_103 • , COMPLEX_SELECTOR|,;
STYLE_RULE_HC_listbody2_103=>STYLE_RULE_HC_listbody2_103 • , COMPLEX_SELECTOR|{;
STYLE_RULE_HC_listbody2_103=>STYLE_RULE_HC_listbody2_103 • , COMPLEX_SELECTOR|$eof
*/
    _skip(l, const__);
    if (l.id == 10/* \, */) {

        _no_check(l);; stack_ptr++; State315(l);

    }
    else fail(l);
}
function State85(l: Lexer): void {
    /*
STYLE_RULE_HC_listbody2_103=>COMPLEX_SELECTOR •|,;
STYLE_RULE_HC_listbody2_103=>COMPLEX_SELECTOR •|{;
STYLE_RULE_HC_listbody2_103=>COMPLEX_SELECTOR •|$eof
*/
    _skip(l, const__);
    if (l.id == 10/* \, */ || l.id == 11/* \{ */) {

        completeProduction(2, 1, 5); stack_ptr -= 1;

        ; return;
    } else if (l.ty == 0/* EOF */) {

        completeProduction(2, 1, 5); stack_ptr -= 1;

        ; return;
    }
    else fail(l);
}
function State90(l: Lexer): void {
    /*
TYPE_SELECTOR=>WQ_NAME •|#;
TYPE_SELECTOR=>WQ_NAME •|.;
TYPE_SELECTOR=>WQ_NAME •|[;
TYPE_SELECTOR=>WQ_NAME •|:;
TYPE_SELECTOR=>WQ_NAME •|ws;
TYPE_SELECTOR=>WQ_NAME •|>;
TYPE_SELECTOR=>WQ_NAME •|+;
TYPE_SELECTOR=>WQ_NAME •|~;
TYPE_SELECTOR=>WQ_NAME •|||;
TYPE_SELECTOR=>WQ_NAME •|$;
TYPE_SELECTOR=>WQ_NAME •|id;
TYPE_SELECTOR=>WQ_NAME •|key;
TYPE_SELECTOR=>WQ_NAME •|-;
TYPE_SELECTOR=>WQ_NAME •|*;
TYPE_SELECTOR=>WQ_NAME •||;
TYPE_SELECTOR=>WQ_NAME •|,;
TYPE_SELECTOR=>WQ_NAME •|{;
TYPE_SELECTOR=>WQ_NAME •|$eof;
TYPE_SELECTOR=>WQ_NAME •|)
*/
    _skip(l, const_1_);
    if (idm90r.has(l.id)) { idm90r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 1/* \ws */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProduction(71, 1, 89); stack_ptr -= 1;

        ; return;
    }
    else fail(l);
}
function State91(l: Lexer): void {
    /*
TYPE_SELECTOR=>NS_PREFIX • *|#;
TYPE_SELECTOR=>NS_PREFIX • *|.;
TYPE_SELECTOR=>NS_PREFIX • *|[;
TYPE_SELECTOR=>NS_PREFIX • *|:;
WQ_NAME=>NS_PREFIX • identifier|#: 93;
WQ_NAME=>NS_PREFIX • identifier|.: 93;
WQ_NAME=>NS_PREFIX • identifier|[: 93;
WQ_NAME=>NS_PREFIX • identifier|:: 93;
TYPE_SELECTOR=>NS_PREFIX • *|ws;
WQ_NAME=>NS_PREFIX • identifier|ws: 93;
TYPE_SELECTOR=>NS_PREFIX • *|>;
WQ_NAME=>NS_PREFIX • identifier|>: 93;
TYPE_SELECTOR=>NS_PREFIX • *|+;
WQ_NAME=>NS_PREFIX • identifier|+: 93;
TYPE_SELECTOR=>NS_PREFIX • *|~;
WQ_NAME=>NS_PREFIX • identifier|~: 93;
TYPE_SELECTOR=>NS_PREFIX • *|||;
WQ_NAME=>NS_PREFIX • identifier|||: 93;
TYPE_SELECTOR=>NS_PREFIX • *|$;
WQ_NAME=>NS_PREFIX • identifier|$: 93;
TYPE_SELECTOR=>NS_PREFIX • *|id;
WQ_NAME=>NS_PREFIX • identifier|id: 93;
TYPE_SELECTOR=>NS_PREFIX • *|key;
WQ_NAME=>NS_PREFIX • identifier|key: 93;
TYPE_SELECTOR=>NS_PREFIX • *|-;
WQ_NAME=>NS_PREFIX • identifier|-: 93;
TYPE_SELECTOR=>NS_PREFIX • *|*;
WQ_NAME=>NS_PREFIX • identifier|*: 93;
TYPE_SELECTOR=>NS_PREFIX • *||;
WQ_NAME=>NS_PREFIX • identifier||: 93;
TYPE_SELECTOR=>NS_PREFIX • *|,;
WQ_NAME=>NS_PREFIX • identifier|,: 93;
TYPE_SELECTOR=>NS_PREFIX • *|{;
WQ_NAME=>NS_PREFIX • identifier|{: 93;
TYPE_SELECTOR=>NS_PREFIX • *|$eof;
WQ_NAME=>NS_PREFIX • identifier|$eof: 93;
TYPE_SELECTOR=>NS_PREFIX • *|);
WQ_NAME=>NS_PREFIX • identifier|): 93
*/
    _skip(l, const_1_);
    if (idm91.has(l.id)) { idm91.get(l.id)(l); } else if (l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        $identifier(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 104: //identifier
                State328(l);
                break;
            case 89/*TYPE_SELECTOR*/:
            case 93/*WQ_NAME*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State92(l: Lexer): void {
    /*
TYPE_SELECTOR=>* •|#;
TYPE_SELECTOR=>* •|.;
TYPE_SELECTOR=>* •|[;
TYPE_SELECTOR=>* •|:;
NS_PREFIX_group_0150_140=>* •||;
TYPE_SELECTOR=>* •|ws;
TYPE_SELECTOR=>* •|>;
TYPE_SELECTOR=>* •|+;
TYPE_SELECTOR=>* •|~;
TYPE_SELECTOR=>* •|||;
TYPE_SELECTOR=>* •|$;
TYPE_SELECTOR=>* •|id;
TYPE_SELECTOR=>* •|key;
TYPE_SELECTOR=>* •|-;
TYPE_SELECTOR=>* •|*;
TYPE_SELECTOR=>* •||;
TYPE_SELECTOR=>* •|,;
TYPE_SELECTOR=>* •|{;
TYPE_SELECTOR=>* •|$eof;
TYPE_SELECTOR=>* •|)
*/
    _skip(l, const_1_);
    if (idm92r.has(l.id)) { idm92r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 1/* \ws */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProduction(73, 1, 89); stack_ptr -= 1;

        ; return;
    }
    else fail(l);
}
function State93(l: Lexer): void {
    /*
WQ_NAME=>identifier •|#;
NS_PREFIX_group_0150_140=>identifier •||;
WQ_NAME=>identifier •|.;
WQ_NAME=>identifier •|[;
WQ_NAME=>identifier •|:;
WQ_NAME=>identifier •|ws;
WQ_NAME=>identifier •|>;
WQ_NAME=>identifier •|+;
WQ_NAME=>identifier •|~;
WQ_NAME=>identifier •|||;
WQ_NAME=>identifier •|$;
WQ_NAME=>identifier •|id;
WQ_NAME=>identifier •|key;
WQ_NAME=>identifier •|-;
WQ_NAME=>identifier •|*;
WQ_NAME=>identifier •||;
WQ_NAME=>identifier •|,;
WQ_NAME=>identifier •|{;
WQ_NAME=>identifier •|$eof;
WQ_NAME=>identifier •|);
WQ_NAME=>identifier •|];
WQ_NAME=>identifier •|^=;
WQ_NAME=>identifier •|$=;
WQ_NAME=>identifier •|*=;
WQ_NAME=>identifier •|=
*/
    _skip(l, const_1_);
    if (idm93r.has(l.id)) { idm93r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 1/* \ws */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProduction(78, 1, 93); stack_ptr -= 1;

        ; return;
    }
    else fail(l);
}
function State94(l: Lexer): void {
    /*
NS_PREFIX=>NS_PREFIX_group_0150_140 • ||$;
NS_PREFIX=>NS_PREFIX_group_0150_140 • ||id;
NS_PREFIX=>NS_PREFIX_group_0150_140 • ||key;
NS_PREFIX=>NS_PREFIX_group_0150_140 • ||-;
NS_PREFIX=>NS_PREFIX_group_0150_140 • ||*
*/
    _skip(l, const__);
    if (l.id == 51/* \| */) {

        _no_check(l);; stack_ptr++; State321(l);

    }
    else fail(l);
}
function State96(l: Lexer): void {
    /*
COMPOUND_SELECTOR_HC_listbody1_134=>SUBCLASS_SELECTOR •|:;
COMPOUND_SELECTOR_HC_listbody1_134=>SUBCLASS_SELECTOR •|#;
COMPOUND_SELECTOR_HC_listbody1_134=>SUBCLASS_SELECTOR •|.;
COMPOUND_SELECTOR_HC_listbody1_134=>SUBCLASS_SELECTOR •|[;
COMPOUND_SELECTOR_HC_listbody1_134=>SUBCLASS_SELECTOR •|ws;
COMPOUND_SELECTOR_HC_listbody1_134=>SUBCLASS_SELECTOR •|>;
COMPOUND_SELECTOR_HC_listbody1_134=>SUBCLASS_SELECTOR •|+;
COMPOUND_SELECTOR_HC_listbody1_134=>SUBCLASS_SELECTOR •|~;
COMPOUND_SELECTOR_HC_listbody1_134=>SUBCLASS_SELECTOR •|||;
COMPOUND_SELECTOR_HC_listbody1_134=>SUBCLASS_SELECTOR •|$;
COMPOUND_SELECTOR_HC_listbody1_134=>SUBCLASS_SELECTOR •|id;
COMPOUND_SELECTOR_HC_listbody1_134=>SUBCLASS_SELECTOR •|key;
COMPOUND_SELECTOR_HC_listbody1_134=>SUBCLASS_SELECTOR •|-;
COMPOUND_SELECTOR_HC_listbody1_134=>SUBCLASS_SELECTOR •|*;
COMPOUND_SELECTOR_HC_listbody1_134=>SUBCLASS_SELECTOR •||;
COMPOUND_SELECTOR_HC_listbody1_134=>SUBCLASS_SELECTOR •|,;
COMPOUND_SELECTOR_HC_listbody1_134=>SUBCLASS_SELECTOR •|{;
COMPOUND_SELECTOR_HC_listbody1_134=>SUBCLASS_SELECTOR •|$eof;
COMPOUND_SELECTOR_HC_listbody1_134=>SUBCLASS_SELECTOR •|)
*/
    _skip(l, const_1_);
    if (idm96r.has(l.id)) { idm96r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 1/* \ws */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProduction(2, 1, 74); stack_ptr -= 1;

        ; return;
    }
    else fail(l);
}
function State105(l: Lexer): void {
    /*
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_group_1121_136 •|ws;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_group_1121_136 •|:;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_group_1121_136 •|>;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_group_1121_136 •|+;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_group_1121_136 •|~;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_group_1121_136 •|||;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_group_1121_136 •|$;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_group_1121_136 •|id;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_group_1121_136 •|key;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_group_1121_136 •|-;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_group_1121_136 •|*;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_group_1121_136 •||;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_group_1121_136 •|#;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_group_1121_136 •|.;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_group_1121_136 •|[;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_group_1121_136 •|,;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_group_1121_136 •|{;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_group_1121_136 •|$eof;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_group_1121_136 •|)
*/
    _skip(l, const_1_);
    if (idm105r.has(l.id)) { idm105r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 1/* \ws */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProduction(2, 1, 77); stack_ptr -= 1;

        ; return;
    }
    else fail(l);
}
function $general_enclosed_HC_listbody1_124(l: Lexer): void {
    /*
general_enclosed_HC_listbody1_124=>• general_enclosed_HC_listbody1_124 general_enclosed_group_067_123|tok: 51;
general_enclosed_HC_listbody1_124=>• general_enclosed_group_067_123|tok: 51;
general_enclosed_HC_listbody1_124=>• general_enclosed_HC_listbody1_124 general_enclosed_group_067_123|sym: 51;
general_enclosed_HC_listbody1_124=>• general_enclosed_group_067_123|sym: 51;
general_enclosed_HC_listbody1_124=>• general_enclosed_HC_listbody1_124 general_enclosed_group_067_123|id: 51;
general_enclosed_HC_listbody1_124=>• general_enclosed_group_067_123|id: 51;
general_enclosed_HC_listbody1_124=>• general_enclosed_HC_listbody1_124 general_enclosed_group_067_123|key: 51;
general_enclosed_HC_listbody1_124=>• general_enclosed_group_067_123|key: 51;
general_enclosed_HC_listbody1_124=>• general_enclosed_HC_listbody1_124 general_enclosed_group_067_123|ws: 51;
general_enclosed_HC_listbody1_124=>• general_enclosed_group_067_123|ws: 51;
general_enclosed_HC_listbody1_124=>• general_enclosed_HC_listbody1_124 general_enclosed_group_067_123|): 51;
general_enclosed_HC_listbody1_124=>• general_enclosed_group_067_123|): 51;
general_enclosed_HC_listbody1_124=>• general_enclosed_HC_listbody1_124 general_enclosed_group_067_123|$eof: 51;
general_enclosed_HC_listbody1_124=>• general_enclosed_group_067_123|$eof: 51
*/
    _skip(l, const_1_);
    if (l.ty == 1/* \ws */ || l.ty == 3/* \id */ || l.ty == 5/* \tok */ || l.ty == 6/* \sym */ || l.ty == 7/* \key */) {

        $general_enclosed_group_067_123(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 51: //general_enclosed_HC_listbody1_124
                if (l.id == 18/* \) */ || l.ty == 0/* EOF */) { return; }
                {
                    const cp = l.copy(), m = mark(), p = prod, s = stack_ptr;
                    State108(cp);
                    if (FAILED) {
                        prod = p;
                        FAILED = false;
                        stack_ptr = s;
                        reset(m);
                        return;
                    } else l.sync(cp);
                }
                break;
            case 50: //general_enclosed_group_067_123
                State109(l);
                break;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State108(l: Lexer): void {
    /*
general_enclosed_HC_listbody1_124=>general_enclosed_HC_listbody1_124 • general_enclosed_group_067_123|tok: 51;
general_enclosed_HC_listbody1_124=>general_enclosed_HC_listbody1_124 • general_enclosed_group_067_123|sym: 51;
general_enclosed_HC_listbody1_124=>general_enclosed_HC_listbody1_124 • general_enclosed_group_067_123|id: 51;
general_enclosed_HC_listbody1_124=>general_enclosed_HC_listbody1_124 • general_enclosed_group_067_123|key: 51;
general_enclosed_HC_listbody1_124=>general_enclosed_HC_listbody1_124 • general_enclosed_group_067_123|ws: 51;
general_enclosed_HC_listbody1_124=>general_enclosed_HC_listbody1_124 • general_enclosed_group_067_123|): 51;
general_enclosed_HC_listbody1_124=>general_enclosed_HC_listbody1_124 • general_enclosed_group_067_123|$eof: 51
*/
    _skip(l, const_1_);
    if (l.ty == 1/* \ws */ || l.ty == 3/* \id */ || l.ty == 5/* \tok */ || l.ty == 6/* \sym */ || l.ty == 7/* \key */) {

        $general_enclosed_group_067_123(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 50: //general_enclosed_group_067_123
                State330(l);
                break;
            case 51/*general_enclosed_HC_listbody1_124*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State109(l: Lexer): void {
    /*
general_enclosed_HC_listbody1_124=>general_enclosed_group_067_123 •|tok;
general_enclosed_HC_listbody1_124=>general_enclosed_group_067_123 •|sym;
general_enclosed_HC_listbody1_124=>general_enclosed_group_067_123 •|id;
general_enclosed_HC_listbody1_124=>general_enclosed_group_067_123 •|key;
general_enclosed_HC_listbody1_124=>general_enclosed_group_067_123 •|ws;
general_enclosed_HC_listbody1_124=>general_enclosed_group_067_123 •|);
general_enclosed_HC_listbody1_124=>general_enclosed_group_067_123 •|$eof
*/
    _skip(l, const_1_);
    if (l.id == 18/* \) */) {

        completeProduction(35, 1, 51); stack_ptr -= 1;

        ; return;
    } else if (l.ty == 0/* EOF */ || l.ty == 1/* \ws */ || l.ty == 3/* \id */ || l.ty == 5/* \tok */ || l.ty == 6/* \sym */ || l.ty == 7/* \key */) {

        completeProduction(35, 1, 51); stack_ptr -= 1;

        ; return;
    }
    else fail(l);
}
function $media_query(l: Lexer): void {
    /*
media_query=>• media_condition|,: 37;
media_query=>• media_query_group_043_116 media_type media_query_group_144_117|,: 37;
media_query=>• media_type media_query_group_144_117|,: 37;
media_query=>• media_query_group_043_116 media_type|,: 37;
media_query=>• media_type|,: 37;
media_query=>• media_condition|;: 37;
media_query=>• media_query_group_043_116 media_type media_query_group_144_117|;: 37;
media_query=>• media_type media_query_group_144_117|;: 37;
media_query=>• media_query_group_043_116 media_type|;: 37;
media_query=>• media_type|;: 37;
media_query=>• media_condition|@: 37;
media_query=>• media_query_group_043_116 media_type media_query_group_144_117|@: 37;
media_query=>• media_type media_query_group_144_117|@: 37;
media_query=>• media_query_group_043_116 media_type|@: 37;
media_query=>• media_type|@: 37;
media_query=>• media_condition|$: 37;
media_query=>• media_query_group_043_116 media_type media_query_group_144_117|$: 37;
media_query=>• media_type media_query_group_144_117|$: 37;
media_query=>• media_query_group_043_116 media_type|$: 37;
media_query=>• media_type|$: 37;
media_query=>• media_condition|id: 37;
media_query=>• media_query_group_043_116 media_type media_query_group_144_117|id: 37;
media_query=>• media_type media_query_group_144_117|id: 37;
media_query=>• media_query_group_043_116 media_type|id: 37;
media_query=>• media_type|id: 37;
media_query=>• media_condition|key: 37;
media_query=>• media_query_group_043_116 media_type media_query_group_144_117|key: 37;
media_query=>• media_type media_query_group_144_117|key: 37;
media_query=>• media_query_group_043_116 media_type|key: 37;
media_query=>• media_type|key: 37;
media_query=>• media_condition|-: 37;
media_query=>• media_query_group_043_116 media_type media_query_group_144_117|-: 37;
media_query=>• media_type media_query_group_144_117|-: 37;
media_query=>• media_query_group_043_116 media_type|-: 37;
media_query=>• media_type|-: 37;
media_query=>• media_condition|*: 37;
media_query=>• media_query_group_043_116 media_type media_query_group_144_117|*: 37;
media_query=>• media_type media_query_group_144_117|*: 37;
media_query=>• media_query_group_043_116 media_type|*: 37;
media_query=>• media_type|*: 37;
media_query=>• media_condition||: 37;
media_query=>• media_query_group_043_116 media_type media_query_group_144_117||: 37;
media_query=>• media_type media_query_group_144_117||: 37;
media_query=>• media_query_group_043_116 media_type||: 37;
media_query=>• media_type||: 37;
media_query=>• media_condition|#: 37;
media_query=>• media_query_group_043_116 media_type media_query_group_144_117|#: 37;
media_query=>• media_type media_query_group_144_117|#: 37;
media_query=>• media_query_group_043_116 media_type|#: 37;
media_query=>• media_type|#: 37;
media_query=>• media_condition|.: 37;
media_query=>• media_query_group_043_116 media_type media_query_group_144_117|.: 37;
media_query=>• media_type media_query_group_144_117|.: 37;
media_query=>• media_query_group_043_116 media_type|.: 37;
media_query=>• media_type|.: 37;
media_query=>• media_condition|[: 37;
media_query=>• media_query_group_043_116 media_type media_query_group_144_117|[: 37;
media_query=>• media_type media_query_group_144_117|[: 37;
media_query=>• media_query_group_043_116 media_type|[: 37;
media_query=>• media_type|[: 37;
media_query=>• media_condition|:: 37;
media_query=>• media_query_group_043_116 media_type media_query_group_144_117|:: 37;
media_query=>• media_type media_query_group_144_117|:: 37;
media_query=>• media_query_group_043_116 media_type|:: 37;
media_query=>• media_type|:: 37;
media_query=>• media_condition|$eof: 37;
media_query=>• media_query_group_043_116 media_type media_query_group_144_117|$eof: 37;
media_query=>• media_type media_query_group_144_117|$eof: 37;
media_query=>• media_query_group_043_116 media_type|$eof: 37;
media_query=>• media_type|$eof: 37;
media_query=>• media_condition|{: 37;
media_query=>• media_query_group_043_116 media_type media_query_group_144_117|{: 37;
media_query=>• media_type media_query_group_144_117|{: 37;
media_query=>• media_query_group_043_116 media_type|{: 37;
media_query=>• media_type|{: 37
*/
    _skip(l, const__);
    if (idm115.has(l.id)) { idm115.get(l.id)(l); } else if (l.ty == 3/* \id */ || l.ty == 7/* \key */) {
        let $mark = mark(), sp = stack_ptr, cp = l.copy();
        $media_condition(cp); stack_ptr++;
        if (FAILED) {
            reset($mark); FAILED = false; stack_ptr = sp;
            $media_type(l); stack_ptr++;;
        } else l.sync(cp);

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 61: //media_type
                State44(l);
                break;
            case 38: //media_condition
                State42(l);
                break;
            case 35: //media_query_group_043_116
                State43(l);
                break;
            case 37/*media_query*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function $string_HC_listbody1_129(l: Lexer): void {
    /*
string_HC_listbody1_129=>• string_HC_listbody1_129 string_group_0109_128|tok: 67;
string_HC_listbody1_129=>• string_group_0109_128|tok: 67;
string_HC_listbody1_129=>• string_HC_listbody1_129 string_group_0109_128|key: 67;
string_HC_listbody1_129=>• string_group_0109_128|key: 67;
string_HC_listbody1_129=>• string_HC_listbody1_129 string_group_0109_128|id: 67;
string_HC_listbody1_129=>• string_group_0109_128|id: 67;
string_HC_listbody1_129=>• string_HC_listbody1_129 string_group_0109_128|sym: 67;
string_HC_listbody1_129=>• string_group_0109_128|sym: 67;
string_HC_listbody1_129=>• string_HC_listbody1_129 string_group_0109_128|num: 67;
string_HC_listbody1_129=>• string_group_0109_128|num: 67;
string_HC_listbody1_129=>• string_HC_listbody1_129 string_group_0109_128|ws: 67;
string_HC_listbody1_129=>• string_group_0109_128|ws: 67;
string_HC_listbody1_129=>• string_HC_listbody1_129 string_group_0109_128|": 67;
string_HC_listbody1_129=>• string_group_0109_128|": 67;
string_HC_listbody1_129=>• string_HC_listbody1_129 string_group_0109_128|$eof: 67;
string_HC_listbody1_129=>• string_group_0109_128|$eof: 67
*/
    _skip(l, const_1_);
    if (l.ty == 1/* \ws */ || l.ty == 2/* \num */ || l.ty == 3/* \id */ || l.ty == 5/* \tok */ || l.ty == 6/* \sym */ || l.ty == 7/* \key */) {

        $string_group_0109_128(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 67: //string_HC_listbody1_129
                if (l.id == 27/* \" */ || l.ty == 0/* EOF */) { return; }
                {
                    const cp = l.copy(), m = mark(), p = prod, s = stack_ptr;
                    State117(cp);
                    if (FAILED) {
                        prod = p;
                        FAILED = false;
                        stack_ptr = s;
                        reset(m);
                        return;
                    } else l.sync(cp);
                }
                break;
            case 66: //string_group_0109_128
                State118(l);
                break;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State117(l: Lexer): void {
    /*
string_HC_listbody1_129=>string_HC_listbody1_129 • string_group_0109_128|tok: 67;
string_HC_listbody1_129=>string_HC_listbody1_129 • string_group_0109_128|key: 67;
string_HC_listbody1_129=>string_HC_listbody1_129 • string_group_0109_128|id: 67;
string_HC_listbody1_129=>string_HC_listbody1_129 • string_group_0109_128|sym: 67;
string_HC_listbody1_129=>string_HC_listbody1_129 • string_group_0109_128|num: 67;
string_HC_listbody1_129=>string_HC_listbody1_129 • string_group_0109_128|ws: 67;
string_HC_listbody1_129=>string_HC_listbody1_129 • string_group_0109_128|": 67;
string_HC_listbody1_129=>string_HC_listbody1_129 • string_group_0109_128|$eof: 67
*/
    _skip(l, const_1_);
    if (l.ty == 1/* \ws */ || l.ty == 2/* \num */ || l.ty == 3/* \id */ || l.ty == 5/* \tok */ || l.ty == 6/* \sym */ || l.ty == 7/* \key */) {

        $string_group_0109_128(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 66: //string_group_0109_128
                State332(l);
                break;
            case 67/*string_HC_listbody1_129*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State118(l: Lexer): void {
    /*
string_HC_listbody1_129=>string_group_0109_128 •|tok;
string_HC_listbody1_129=>string_group_0109_128 •|key;
string_HC_listbody1_129=>string_group_0109_128 •|id;
string_HC_listbody1_129=>string_group_0109_128 •|sym;
string_HC_listbody1_129=>string_group_0109_128 •|num;
string_HC_listbody1_129=>string_group_0109_128 •|ws;
string_HC_listbody1_129=>string_group_0109_128 •|";
string_HC_listbody1_129=>string_group_0109_128 •|$eof
*/
    _skip(l, const_1_);
    if (l.id == 27/* \" */) {

        completeProduction(35, 1, 67); stack_ptr -= 1;

        ; return;
    } else if (l.ty == 0/* EOF */ || l.ty == 1/* \ws */ || l.ty == 2/* \num */ || l.ty == 3/* \id */ || l.ty == 5/* \tok */ || l.ty == 6/* \sym */ || l.ty == 7/* \key */) {

        completeProduction(35, 1, 67); stack_ptr -= 1;

        ; return;
    }
    else fail(l);
}
function $mf_range(l: Lexer): void {
    /*
mf_range=>• mf_name mf_range_group_074_125 mf_value|): 57;
mf_range=>• mf_value mf_range_group_074_125 mf_name|): 57;
mf_range=>• mf_value mf_range_group_183_126 identifier mf_range_group_183_126 mf_value|): 57;
mf_range=>• mf_value mf_range_group_188_127 identifier mf_range_group_188_127 mf_value|): 57;
mf_range=>• mf_name mf_range_group_074_125 mf_value|$eof: 57;
mf_range=>• mf_value mf_range_group_074_125 mf_name|$eof: 57;
mf_range=>• mf_value mf_range_group_183_126 identifier mf_range_group_183_126 mf_value|$eof: 57;
mf_range=>• mf_value mf_range_group_188_127 identifier mf_range_group_188_127 mf_value|$eof: 57
*/
    _skip(l, const__);
    if (l.id == 55/* \- */ || l.id == 56/* \$ */) {
        let $mark = mark(), sp = stack_ptr, cp = l.copy();
        $mf_name(cp); stack_ptr++;
        if (FAILED) {
            reset($mark); FAILED = false; stack_ptr = sp;
            $mf_value(l); stack_ptr++;;
        } else l.sync(cp);

    } else if (tym125.has(l.ty)) { tym125.get(l.ty)(l); }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 60: //mf_name
                State126(l);
                break;
            case 58: //mf_value
                State79(l);
                break;
            case 57/*mf_range*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State126(l: Lexer): void {
    /*
mf_range=>mf_name • mf_range_group_074_125 mf_value|): 57;
mf_range=>mf_name • mf_range_group_074_125 mf_value|$eof: 57;
mf_value=>mf_name •|<;
mf_value=>mf_name •|>;
mf_value=>mf_name •|=
*/
    _skip(l, const__);
    if (l.id == 29/* \< */ || l.id == 30/* \= */ || l.id == 31/* \> */) {

        $mf_range_group_074_125(l); stack_ptr++;

    } else if (l.id == 29/* \< */ || l.id == 30/* \= */ || l.id == 31/* \> */) {

        completeProductionPlain(1, 58); stack_ptr -= 1;

        ; return;
    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 54: //mf_range_group_074_125
                State303(l);
                break;
            case 57/*mf_range*/:
            case 58/*mf_value*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function $string_HC_listbody1_130(l: Lexer): void {
    /*
string_HC_listbody1_130=>• string_HC_listbody1_130 string_group_0109_128|tok: 68;
string_HC_listbody1_130=>• string_group_0109_128|tok: 68;
string_HC_listbody1_130=>• string_HC_listbody1_130 string_group_0109_128|key: 68;
string_HC_listbody1_130=>• string_group_0109_128|key: 68;
string_HC_listbody1_130=>• string_HC_listbody1_130 string_group_0109_128|id: 68;
string_HC_listbody1_130=>• string_group_0109_128|id: 68;
string_HC_listbody1_130=>• string_HC_listbody1_130 string_group_0109_128|sym: 68;
string_HC_listbody1_130=>• string_group_0109_128|sym: 68;
string_HC_listbody1_130=>• string_HC_listbody1_130 string_group_0109_128|num: 68;
string_HC_listbody1_130=>• string_group_0109_128|num: 68;
string_HC_listbody1_130=>• string_HC_listbody1_130 string_group_0109_128|ws: 68;
string_HC_listbody1_130=>• string_group_0109_128|ws: 68;
string_HC_listbody1_130=>• string_HC_listbody1_130 string_group_0109_128|': 68;
string_HC_listbody1_130=>• string_group_0109_128|': 68;
string_HC_listbody1_130=>• string_HC_listbody1_130 string_group_0109_128|$eof: 68;
string_HC_listbody1_130=>• string_group_0109_128|$eof: 68
*/
    _skip(l, const_1_);
    if (l.ty == 1/* \ws */ || l.ty == 2/* \num */ || l.ty == 3/* \id */ || l.ty == 5/* \tok */ || l.ty == 6/* \sym */ || l.ty == 7/* \key */) {

        $string_group_0109_128(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 68: //string_HC_listbody1_130
                if (l.id == 37/* \' */ || l.ty == 0/* EOF */) { return; }
                {
                    const cp = l.copy(), m = mark(), p = prod, s = stack_ptr;
                    State128(cp);
                    if (FAILED) {
                        prod = p;
                        FAILED = false;
                        stack_ptr = s;
                        reset(m);
                        return;
                    } else l.sync(cp);
                }
                break;
            case 66: //string_group_0109_128
                State129(l);
                break;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State128(l: Lexer): void {
    /*
string_HC_listbody1_130=>string_HC_listbody1_130 • string_group_0109_128|tok: 68;
string_HC_listbody1_130=>string_HC_listbody1_130 • string_group_0109_128|key: 68;
string_HC_listbody1_130=>string_HC_listbody1_130 • string_group_0109_128|id: 68;
string_HC_listbody1_130=>string_HC_listbody1_130 • string_group_0109_128|sym: 68;
string_HC_listbody1_130=>string_HC_listbody1_130 • string_group_0109_128|num: 68;
string_HC_listbody1_130=>string_HC_listbody1_130 • string_group_0109_128|ws: 68;
string_HC_listbody1_130=>string_HC_listbody1_130 • string_group_0109_128|': 68;
string_HC_listbody1_130=>string_HC_listbody1_130 • string_group_0109_128|$eof: 68
*/
    _skip(l, const_1_);
    if (l.ty == 1/* \ws */ || l.ty == 2/* \num */ || l.ty == 3/* \id */ || l.ty == 5/* \tok */ || l.ty == 6/* \sym */ || l.ty == 7/* \key */) {

        $string_group_0109_128(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 66: //string_group_0109_128
                State334(l);
                break;
            case 68/*string_HC_listbody1_130*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State129(l: Lexer): void {
    /*
string_HC_listbody1_130=>string_group_0109_128 •|tok;
string_HC_listbody1_130=>string_group_0109_128 •|key;
string_HC_listbody1_130=>string_group_0109_128 •|id;
string_HC_listbody1_130=>string_group_0109_128 •|sym;
string_HC_listbody1_130=>string_group_0109_128 •|num;
string_HC_listbody1_130=>string_group_0109_128 •|ws;
string_HC_listbody1_130=>string_group_0109_128 •|';
string_HC_listbody1_130=>string_group_0109_128 •|$eof
*/
    _skip(l, const_1_);
    if (l.id == 37/* \' */) {

        completeProduction(35, 1, 68); stack_ptr -= 1;

        ; return;
    } else if (l.ty == 0/* EOF */ || l.ty == 1/* \ws */ || l.ty == 2/* \num */ || l.ty == 3/* \id */ || l.ty == 5/* \tok */ || l.ty == 6/* \sym */ || l.ty == 7/* \key */) {

        completeProduction(35, 1, 68); stack_ptr -= 1;

        ; return;
    }
    else fail(l);
}
function State133(l: Lexer): void {
    /*
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { declaration_list ; }|$;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { ; }|$;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { declaration_list }|$;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { }|$;
STYLE_RULE_HC_listbody2_103=>STYLE_RULE_HC_listbody2_103 • , COMPLEX_SELECTOR|{;
STYLE_RULE_HC_listbody2_103=>STYLE_RULE_HC_listbody2_103 • , COMPLEX_SELECTOR|,;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { declaration_list ; }|id;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { ; }|id;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { declaration_list }|id;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { }|id;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { declaration_list ; }|key;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { ; }|key;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { declaration_list }|key;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { }|key;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { declaration_list ; }|-;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { ; }|-;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { declaration_list }|-;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { }|-;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { declaration_list ; }|*;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { ; }|*;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { declaration_list }|*;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { }|*;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { declaration_list ; }||;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { ; }||;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { declaration_list }||;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { }||;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { declaration_list ; }|#;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { ; }|#;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { declaration_list }|#;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { }|#;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { declaration_list ; }|.;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { ; }|.;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { declaration_list }|.;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { }|.;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { declaration_list ; }|[;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { ; }|[;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { declaration_list }|[;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { }|[;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { declaration_list ; }|:;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { ; }|:;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { declaration_list }|:;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { }|:;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { declaration_list ; }|$eof;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { ; }|$eof;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { declaration_list }|$eof;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { }|$eof;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { declaration_list ; }|@;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { ; }|@;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { declaration_list }|@;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { }|@;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { declaration_list ; }|};
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { ; }|};
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { declaration_list }|};
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 • { }|}
*/
    _skip(l, const__);
    if (idm133.has(l.id)) { idm133.get(l.id)(l); }
    else fail(l);
}
function $COMPOUND_SELECTOR_HC_listbody1_134(l: Lexer): void {
    /*
COMPOUND_SELECTOR_HC_listbody1_134=>• COMPOUND_SELECTOR_HC_listbody1_134 SUBCLASS_SELECTOR|#: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>• SUBCLASS_SELECTOR|#: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>• COMPOUND_SELECTOR_HC_listbody1_134 SUBCLASS_SELECTOR|.: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>• SUBCLASS_SELECTOR|.: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>• COMPOUND_SELECTOR_HC_listbody1_134 SUBCLASS_SELECTOR|[: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>• SUBCLASS_SELECTOR|[: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>• COMPOUND_SELECTOR_HC_listbody1_134 SUBCLASS_SELECTOR|:: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>• SUBCLASS_SELECTOR|:: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>• COMPOUND_SELECTOR_HC_listbody1_134 SUBCLASS_SELECTOR|ws: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>• SUBCLASS_SELECTOR|ws: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>• COMPOUND_SELECTOR_HC_listbody1_134 SUBCLASS_SELECTOR|>: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>• SUBCLASS_SELECTOR|>: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>• COMPOUND_SELECTOR_HC_listbody1_134 SUBCLASS_SELECTOR|+: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>• SUBCLASS_SELECTOR|+: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>• COMPOUND_SELECTOR_HC_listbody1_134 SUBCLASS_SELECTOR|~: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>• SUBCLASS_SELECTOR|~: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>• COMPOUND_SELECTOR_HC_listbody1_134 SUBCLASS_SELECTOR|||: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>• SUBCLASS_SELECTOR|||: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>• COMPOUND_SELECTOR_HC_listbody1_134 SUBCLASS_SELECTOR|$: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>• SUBCLASS_SELECTOR|$: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>• COMPOUND_SELECTOR_HC_listbody1_134 SUBCLASS_SELECTOR|id: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>• SUBCLASS_SELECTOR|id: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>• COMPOUND_SELECTOR_HC_listbody1_134 SUBCLASS_SELECTOR|key: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>• SUBCLASS_SELECTOR|key: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>• COMPOUND_SELECTOR_HC_listbody1_134 SUBCLASS_SELECTOR|-: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>• SUBCLASS_SELECTOR|-: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>• COMPOUND_SELECTOR_HC_listbody1_134 SUBCLASS_SELECTOR|*: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>• SUBCLASS_SELECTOR|*: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>• COMPOUND_SELECTOR_HC_listbody1_134 SUBCLASS_SELECTOR||: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>• SUBCLASS_SELECTOR||: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>• COMPOUND_SELECTOR_HC_listbody1_134 SUBCLASS_SELECTOR|): 74;
COMPOUND_SELECTOR_HC_listbody1_134=>• SUBCLASS_SELECTOR|): 74;
COMPOUND_SELECTOR_HC_listbody1_134=>• COMPOUND_SELECTOR_HC_listbody1_134 SUBCLASS_SELECTOR|,: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>• SUBCLASS_SELECTOR|,: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>• COMPOUND_SELECTOR_HC_listbody1_134 SUBCLASS_SELECTOR|{: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>• SUBCLASS_SELECTOR|{: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>• COMPOUND_SELECTOR_HC_listbody1_134 SUBCLASS_SELECTOR|$eof: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>• SUBCLASS_SELECTOR|$eof: 74
*/
    _skip(l, const_1_);
    if (l.id == 28/* \: */ || l.id == 41/* \# */ || l.id == 42/* \. */ || l.id == 43/* \[ */) {

        $SUBCLASS_SELECTOR(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 80: //SUBCLASS_SELECTOR
                State96(l);
                break;
            case 74: //COMPOUND_SELECTOR_HC_listbody1_134
                if (const_3_.includes(l.id) || l.ty == 0/* EOF */ || l.ty == 1/* \ws */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) { return; }
                {
                    const cp = l.copy(), m = mark(), p = prod, s = stack_ptr;
                    State135(cp);
                    if (FAILED) {
                        prod = p;
                        FAILED = false;
                        stack_ptr = s;
                        reset(m);
                        return;
                    } else l.sync(cp);
                }
                break;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State135(l: Lexer): void {
    /*
COMPOUND_SELECTOR_HC_listbody1_134=>COMPOUND_SELECTOR_HC_listbody1_134 • SUBCLASS_SELECTOR|#: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>COMPOUND_SELECTOR_HC_listbody1_134 • SUBCLASS_SELECTOR|.: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>COMPOUND_SELECTOR_HC_listbody1_134 • SUBCLASS_SELECTOR|[: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>COMPOUND_SELECTOR_HC_listbody1_134 • SUBCLASS_SELECTOR|:: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>COMPOUND_SELECTOR_HC_listbody1_134 • SUBCLASS_SELECTOR|ws: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>COMPOUND_SELECTOR_HC_listbody1_134 • SUBCLASS_SELECTOR|>: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>COMPOUND_SELECTOR_HC_listbody1_134 • SUBCLASS_SELECTOR|+: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>COMPOUND_SELECTOR_HC_listbody1_134 • SUBCLASS_SELECTOR|~: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>COMPOUND_SELECTOR_HC_listbody1_134 • SUBCLASS_SELECTOR|||: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>COMPOUND_SELECTOR_HC_listbody1_134 • SUBCLASS_SELECTOR|$: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>COMPOUND_SELECTOR_HC_listbody1_134 • SUBCLASS_SELECTOR|id: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>COMPOUND_SELECTOR_HC_listbody1_134 • SUBCLASS_SELECTOR|key: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>COMPOUND_SELECTOR_HC_listbody1_134 • SUBCLASS_SELECTOR|-: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>COMPOUND_SELECTOR_HC_listbody1_134 • SUBCLASS_SELECTOR|*: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>COMPOUND_SELECTOR_HC_listbody1_134 • SUBCLASS_SELECTOR||: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>COMPOUND_SELECTOR_HC_listbody1_134 • SUBCLASS_SELECTOR|): 74;
COMPOUND_SELECTOR_HC_listbody1_134=>COMPOUND_SELECTOR_HC_listbody1_134 • SUBCLASS_SELECTOR|,: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>COMPOUND_SELECTOR_HC_listbody1_134 • SUBCLASS_SELECTOR|{: 74;
COMPOUND_SELECTOR_HC_listbody1_134=>COMPOUND_SELECTOR_HC_listbody1_134 • SUBCLASS_SELECTOR|$eof: 74
*/
    _skip(l, const_1_);
    if (l.id == 28/* \: */ || l.id == 41/* \# */ || l.id == 42/* \. */ || l.id == 43/* \[ */) {

        $SUBCLASS_SELECTOR(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 80: //SUBCLASS_SELECTOR
                State318(l);
                break;
            case 74/*COMPOUND_SELECTOR_HC_listbody1_134*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function $COMPOUND_SELECTOR_HC_listbody1_135(l: Lexer): void {
    /*
COMPOUND_SELECTOR_HC_listbody1_135=>• COMPOUND_SELECTOR_HC_listbody1_135 PSEUDO_CLASS_SELECTOR|:: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>• PSEUDO_CLASS_SELECTOR|:: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>• COMPOUND_SELECTOR_HC_listbody1_135 PSEUDO_CLASS_SELECTOR|ws: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>• PSEUDO_CLASS_SELECTOR|ws: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>• COMPOUND_SELECTOR_HC_listbody1_135 PSEUDO_CLASS_SELECTOR|>: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>• PSEUDO_CLASS_SELECTOR|>: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>• COMPOUND_SELECTOR_HC_listbody1_135 PSEUDO_CLASS_SELECTOR|+: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>• PSEUDO_CLASS_SELECTOR|+: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>• COMPOUND_SELECTOR_HC_listbody1_135 PSEUDO_CLASS_SELECTOR|~: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>• PSEUDO_CLASS_SELECTOR|~: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>• COMPOUND_SELECTOR_HC_listbody1_135 PSEUDO_CLASS_SELECTOR|||: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>• PSEUDO_CLASS_SELECTOR|||: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>• COMPOUND_SELECTOR_HC_listbody1_135 PSEUDO_CLASS_SELECTOR|$: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>• PSEUDO_CLASS_SELECTOR|$: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>• COMPOUND_SELECTOR_HC_listbody1_135 PSEUDO_CLASS_SELECTOR|id: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>• PSEUDO_CLASS_SELECTOR|id: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>• COMPOUND_SELECTOR_HC_listbody1_135 PSEUDO_CLASS_SELECTOR|key: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>• PSEUDO_CLASS_SELECTOR|key: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>• COMPOUND_SELECTOR_HC_listbody1_135 PSEUDO_CLASS_SELECTOR|-: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>• PSEUDO_CLASS_SELECTOR|-: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>• COMPOUND_SELECTOR_HC_listbody1_135 PSEUDO_CLASS_SELECTOR|*: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>• PSEUDO_CLASS_SELECTOR|*: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>• COMPOUND_SELECTOR_HC_listbody1_135 PSEUDO_CLASS_SELECTOR||: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>• PSEUDO_CLASS_SELECTOR||: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>• COMPOUND_SELECTOR_HC_listbody1_135 PSEUDO_CLASS_SELECTOR|#: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>• PSEUDO_CLASS_SELECTOR|#: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>• COMPOUND_SELECTOR_HC_listbody1_135 PSEUDO_CLASS_SELECTOR|.: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>• PSEUDO_CLASS_SELECTOR|.: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>• COMPOUND_SELECTOR_HC_listbody1_135 PSEUDO_CLASS_SELECTOR|[: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>• PSEUDO_CLASS_SELECTOR|[: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>• COMPOUND_SELECTOR_HC_listbody1_135 PSEUDO_CLASS_SELECTOR|): 75;
COMPOUND_SELECTOR_HC_listbody1_135=>• PSEUDO_CLASS_SELECTOR|): 75;
COMPOUND_SELECTOR_HC_listbody1_135=>• COMPOUND_SELECTOR_HC_listbody1_135 PSEUDO_CLASS_SELECTOR|,: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>• PSEUDO_CLASS_SELECTOR|,: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>• COMPOUND_SELECTOR_HC_listbody1_135 PSEUDO_CLASS_SELECTOR|{: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>• PSEUDO_CLASS_SELECTOR|{: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>• COMPOUND_SELECTOR_HC_listbody1_135 PSEUDO_CLASS_SELECTOR|$eof: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>• PSEUDO_CLASS_SELECTOR|$eof: 75
*/
    _skip(l, const_1_);
    if (l.id == 28/* \: */) {

        $PSEUDO_CLASS_SELECTOR(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 84: //PSEUDO_CLASS_SELECTOR
                State139(l);
                break;
            case 75: //COMPOUND_SELECTOR_HC_listbody1_135
                if (const_4_.includes(l.id) || l.ty == 0/* EOF */ || l.ty == 1/* \ws */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) { return; }
                {
                    const cp = l.copy(), m = mark(), p = prod, s = stack_ptr;
                    State138(cp);
                    if (FAILED) {
                        prod = p;
                        FAILED = false;
                        stack_ptr = s;
                        reset(m);
                        return;
                    } else l.sync(cp);
                }
                break;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State138(l: Lexer): void {
    /*
COMPOUND_SELECTOR_HC_listbody1_135=>COMPOUND_SELECTOR_HC_listbody1_135 • PSEUDO_CLASS_SELECTOR|:: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>COMPOUND_SELECTOR_HC_listbody1_135 • PSEUDO_CLASS_SELECTOR|ws: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>COMPOUND_SELECTOR_HC_listbody1_135 • PSEUDO_CLASS_SELECTOR|>: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>COMPOUND_SELECTOR_HC_listbody1_135 • PSEUDO_CLASS_SELECTOR|+: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>COMPOUND_SELECTOR_HC_listbody1_135 • PSEUDO_CLASS_SELECTOR|~: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>COMPOUND_SELECTOR_HC_listbody1_135 • PSEUDO_CLASS_SELECTOR|||: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>COMPOUND_SELECTOR_HC_listbody1_135 • PSEUDO_CLASS_SELECTOR|$: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>COMPOUND_SELECTOR_HC_listbody1_135 • PSEUDO_CLASS_SELECTOR|id: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>COMPOUND_SELECTOR_HC_listbody1_135 • PSEUDO_CLASS_SELECTOR|key: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>COMPOUND_SELECTOR_HC_listbody1_135 • PSEUDO_CLASS_SELECTOR|-: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>COMPOUND_SELECTOR_HC_listbody1_135 • PSEUDO_CLASS_SELECTOR|*: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>COMPOUND_SELECTOR_HC_listbody1_135 • PSEUDO_CLASS_SELECTOR||: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>COMPOUND_SELECTOR_HC_listbody1_135 • PSEUDO_CLASS_SELECTOR|#: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>COMPOUND_SELECTOR_HC_listbody1_135 • PSEUDO_CLASS_SELECTOR|.: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>COMPOUND_SELECTOR_HC_listbody1_135 • PSEUDO_CLASS_SELECTOR|[: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>COMPOUND_SELECTOR_HC_listbody1_135 • PSEUDO_CLASS_SELECTOR|): 75;
COMPOUND_SELECTOR_HC_listbody1_135=>COMPOUND_SELECTOR_HC_listbody1_135 • PSEUDO_CLASS_SELECTOR|,: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>COMPOUND_SELECTOR_HC_listbody1_135 • PSEUDO_CLASS_SELECTOR|{: 75;
COMPOUND_SELECTOR_HC_listbody1_135=>COMPOUND_SELECTOR_HC_listbody1_135 • PSEUDO_CLASS_SELECTOR|$eof: 75
*/
    _skip(l, const_1_);
    if (l.id == 28/* \: */) {

        $PSEUDO_CLASS_SELECTOR(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 84: //PSEUDO_CLASS_SELECTOR
                State339(l);
                break;
            case 75/*COMPOUND_SELECTOR_HC_listbody1_135*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State139(l: Lexer): void {
    /*
COMPOUND_SELECTOR_HC_listbody1_135=>PSEUDO_CLASS_SELECTOR •|:;
COMPOUND_SELECTOR_HC_listbody1_135=>PSEUDO_CLASS_SELECTOR •|ws;
COMPOUND_SELECTOR_HC_listbody1_135=>PSEUDO_CLASS_SELECTOR •|>;
COMPOUND_SELECTOR_HC_listbody1_135=>PSEUDO_CLASS_SELECTOR •|+;
COMPOUND_SELECTOR_HC_listbody1_135=>PSEUDO_CLASS_SELECTOR •|~;
COMPOUND_SELECTOR_HC_listbody1_135=>PSEUDO_CLASS_SELECTOR •|||;
COMPOUND_SELECTOR_HC_listbody1_135=>PSEUDO_CLASS_SELECTOR •|$;
COMPOUND_SELECTOR_HC_listbody1_135=>PSEUDO_CLASS_SELECTOR •|id;
COMPOUND_SELECTOR_HC_listbody1_135=>PSEUDO_CLASS_SELECTOR •|key;
COMPOUND_SELECTOR_HC_listbody1_135=>PSEUDO_CLASS_SELECTOR •|-;
COMPOUND_SELECTOR_HC_listbody1_135=>PSEUDO_CLASS_SELECTOR •|*;
COMPOUND_SELECTOR_HC_listbody1_135=>PSEUDO_CLASS_SELECTOR •||;
COMPOUND_SELECTOR_HC_listbody1_135=>PSEUDO_CLASS_SELECTOR •|#;
COMPOUND_SELECTOR_HC_listbody1_135=>PSEUDO_CLASS_SELECTOR •|.;
COMPOUND_SELECTOR_HC_listbody1_135=>PSEUDO_CLASS_SELECTOR •|[;
COMPOUND_SELECTOR_HC_listbody1_135=>PSEUDO_CLASS_SELECTOR •|);
COMPOUND_SELECTOR_HC_listbody1_135=>PSEUDO_CLASS_SELECTOR •|,;
COMPOUND_SELECTOR_HC_listbody1_135=>PSEUDO_CLASS_SELECTOR •|{;
COMPOUND_SELECTOR_HC_listbody1_135=>PSEUDO_CLASS_SELECTOR •|$eof
*/
    _skip(l, const_1_);
    if (idm139r.has(l.id)) { idm139r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 1/* \ws */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProduction(2, 1, 75); stack_ptr -= 1;

        ; return;
    }
    else fail(l);
}
function $COMPOUND_SELECTOR_HC_listbody2_137(l: Lexer): void {
    /*
COMPOUND_SELECTOR_HC_listbody2_137=>• COMPOUND_SELECTOR_HC_listbody2_137 COMPOUND_SELECTOR_group_1121_136|:: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>• COMPOUND_SELECTOR_group_1121_136|:: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>• COMPOUND_SELECTOR_HC_listbody2_137 COMPOUND_SELECTOR_group_1121_136|ws: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>• COMPOUND_SELECTOR_group_1121_136|ws: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>• COMPOUND_SELECTOR_HC_listbody2_137 COMPOUND_SELECTOR_group_1121_136|>: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>• COMPOUND_SELECTOR_group_1121_136|>: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>• COMPOUND_SELECTOR_HC_listbody2_137 COMPOUND_SELECTOR_group_1121_136|+: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>• COMPOUND_SELECTOR_group_1121_136|+: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>• COMPOUND_SELECTOR_HC_listbody2_137 COMPOUND_SELECTOR_group_1121_136|~: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>• COMPOUND_SELECTOR_group_1121_136|~: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>• COMPOUND_SELECTOR_HC_listbody2_137 COMPOUND_SELECTOR_group_1121_136|||: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>• COMPOUND_SELECTOR_group_1121_136|||: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>• COMPOUND_SELECTOR_HC_listbody2_137 COMPOUND_SELECTOR_group_1121_136|$: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>• COMPOUND_SELECTOR_group_1121_136|$: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>• COMPOUND_SELECTOR_HC_listbody2_137 COMPOUND_SELECTOR_group_1121_136|id: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>• COMPOUND_SELECTOR_group_1121_136|id: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>• COMPOUND_SELECTOR_HC_listbody2_137 COMPOUND_SELECTOR_group_1121_136|key: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>• COMPOUND_SELECTOR_group_1121_136|key: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>• COMPOUND_SELECTOR_HC_listbody2_137 COMPOUND_SELECTOR_group_1121_136|-: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>• COMPOUND_SELECTOR_group_1121_136|-: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>• COMPOUND_SELECTOR_HC_listbody2_137 COMPOUND_SELECTOR_group_1121_136|*: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>• COMPOUND_SELECTOR_group_1121_136|*: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>• COMPOUND_SELECTOR_HC_listbody2_137 COMPOUND_SELECTOR_group_1121_136||: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>• COMPOUND_SELECTOR_group_1121_136||: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>• COMPOUND_SELECTOR_HC_listbody2_137 COMPOUND_SELECTOR_group_1121_136|#: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>• COMPOUND_SELECTOR_group_1121_136|#: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>• COMPOUND_SELECTOR_HC_listbody2_137 COMPOUND_SELECTOR_group_1121_136|.: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>• COMPOUND_SELECTOR_group_1121_136|.: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>• COMPOUND_SELECTOR_HC_listbody2_137 COMPOUND_SELECTOR_group_1121_136|[: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>• COMPOUND_SELECTOR_group_1121_136|[: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>• COMPOUND_SELECTOR_HC_listbody2_137 COMPOUND_SELECTOR_group_1121_136|): 77;
COMPOUND_SELECTOR_HC_listbody2_137=>• COMPOUND_SELECTOR_group_1121_136|): 77;
COMPOUND_SELECTOR_HC_listbody2_137=>• COMPOUND_SELECTOR_HC_listbody2_137 COMPOUND_SELECTOR_group_1121_136|,: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>• COMPOUND_SELECTOR_group_1121_136|,: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>• COMPOUND_SELECTOR_HC_listbody2_137 COMPOUND_SELECTOR_group_1121_136|{: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>• COMPOUND_SELECTOR_group_1121_136|{: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>• COMPOUND_SELECTOR_HC_listbody2_137 COMPOUND_SELECTOR_group_1121_136|$eof: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>• COMPOUND_SELECTOR_group_1121_136|$eof: 77
*/
    _skip(l, const_1_);
    if (l.id == 28/* \: */) {

        $COMPOUND_SELECTOR_group_1121_136(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 77: //COMPOUND_SELECTOR_HC_listbody2_137
                if (const_4_.includes(l.id) || l.ty == 0/* EOF */ || l.ty == 1/* \ws */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) { return; }
                {
                    const cp = l.copy(), m = mark(), p = prod, s = stack_ptr;
                    State141(cp);
                    if (FAILED) {
                        prod = p;
                        FAILED = false;
                        stack_ptr = s;
                        reset(m);
                        return;
                    } else l.sync(cp);
                }
                break;
            case 76: //COMPOUND_SELECTOR_group_1121_136
                State105(l);
                break;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State141(l: Lexer): void {
    /*
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_HC_listbody2_137 • COMPOUND_SELECTOR_group_1121_136|:: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_HC_listbody2_137 • COMPOUND_SELECTOR_group_1121_136|ws: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_HC_listbody2_137 • COMPOUND_SELECTOR_group_1121_136|>: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_HC_listbody2_137 • COMPOUND_SELECTOR_group_1121_136|+: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_HC_listbody2_137 • COMPOUND_SELECTOR_group_1121_136|~: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_HC_listbody2_137 • COMPOUND_SELECTOR_group_1121_136|||: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_HC_listbody2_137 • COMPOUND_SELECTOR_group_1121_136|$: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_HC_listbody2_137 • COMPOUND_SELECTOR_group_1121_136|id: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_HC_listbody2_137 • COMPOUND_SELECTOR_group_1121_136|key: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_HC_listbody2_137 • COMPOUND_SELECTOR_group_1121_136|-: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_HC_listbody2_137 • COMPOUND_SELECTOR_group_1121_136|*: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_HC_listbody2_137 • COMPOUND_SELECTOR_group_1121_136||: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_HC_listbody2_137 • COMPOUND_SELECTOR_group_1121_136|#: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_HC_listbody2_137 • COMPOUND_SELECTOR_group_1121_136|.: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_HC_listbody2_137 • COMPOUND_SELECTOR_group_1121_136|[: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_HC_listbody2_137 • COMPOUND_SELECTOR_group_1121_136|): 77;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_HC_listbody2_137 • COMPOUND_SELECTOR_group_1121_136|,: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_HC_listbody2_137 • COMPOUND_SELECTOR_group_1121_136|{: 77;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_HC_listbody2_137 • COMPOUND_SELECTOR_group_1121_136|$eof: 77
*/
    _skip(l, const_1_);
    if (l.id == 28/* \: */) {

        $COMPOUND_SELECTOR_group_1121_136(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 76: //COMPOUND_SELECTOR_group_1121_136
                State320(l);
                break;
            case 77/*COMPOUND_SELECTOR_HC_listbody2_137*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function $STYLE_SHEET_HC_listbody1_102(l: Lexer): void {
    /*
STYLE_SHEET_HC_listbody1_102=>• STYLE_SHEET_HC_listbody1_102 STYLE_SHEET_group_03_101|$: 3;
STYLE_SHEET_HC_listbody1_102=>• STYLE_SHEET_group_03_101|$: 3;
STYLE_SHEET_HC_listbody1_102=>• STYLE_SHEET_HC_listbody1_102 STYLE_SHEET_group_03_101|id: 3;
STYLE_SHEET_HC_listbody1_102=>• STYLE_SHEET_group_03_101|id: 3;
STYLE_SHEET_HC_listbody1_102=>• STYLE_SHEET_HC_listbody1_102 STYLE_SHEET_group_03_101|key: 3;
STYLE_SHEET_HC_listbody1_102=>• STYLE_SHEET_group_03_101|key: 3;
STYLE_SHEET_HC_listbody1_102=>• STYLE_SHEET_HC_listbody1_102 STYLE_SHEET_group_03_101|-: 3;
STYLE_SHEET_HC_listbody1_102=>• STYLE_SHEET_group_03_101|-: 3;
STYLE_SHEET_HC_listbody1_102=>• STYLE_SHEET_HC_listbody1_102 STYLE_SHEET_group_03_101|*: 3;
STYLE_SHEET_HC_listbody1_102=>• STYLE_SHEET_group_03_101|*: 3;
STYLE_SHEET_HC_listbody1_102=>• STYLE_SHEET_HC_listbody1_102 STYLE_SHEET_group_03_101||: 3;
STYLE_SHEET_HC_listbody1_102=>• STYLE_SHEET_group_03_101||: 3;
STYLE_SHEET_HC_listbody1_102=>• STYLE_SHEET_HC_listbody1_102 STYLE_SHEET_group_03_101|#: 3;
STYLE_SHEET_HC_listbody1_102=>• STYLE_SHEET_group_03_101|#: 3;
STYLE_SHEET_HC_listbody1_102=>• STYLE_SHEET_HC_listbody1_102 STYLE_SHEET_group_03_101|.: 3;
STYLE_SHEET_HC_listbody1_102=>• STYLE_SHEET_group_03_101|.: 3;
STYLE_SHEET_HC_listbody1_102=>• STYLE_SHEET_HC_listbody1_102 STYLE_SHEET_group_03_101|[: 3;
STYLE_SHEET_HC_listbody1_102=>• STYLE_SHEET_group_03_101|[: 3;
STYLE_SHEET_HC_listbody1_102=>• STYLE_SHEET_HC_listbody1_102 STYLE_SHEET_group_03_101|:: 3;
STYLE_SHEET_HC_listbody1_102=>• STYLE_SHEET_group_03_101|:: 3;
STYLE_SHEET_HC_listbody1_102=>• STYLE_SHEET_HC_listbody1_102 STYLE_SHEET_group_03_101|@: 3;
STYLE_SHEET_HC_listbody1_102=>• STYLE_SHEET_group_03_101|@: 3;
STYLE_SHEET_HC_listbody1_102=>• STYLE_SHEET_HC_listbody1_102 STYLE_SHEET_group_03_101|$eof: 3;
STYLE_SHEET_HC_listbody1_102=>• STYLE_SHEET_group_03_101|$eof: 3
*/
    _skip(l, const__);
    if (idm342.has(l.id)) { idm342.get(l.id)(l); } else if (l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        $STYLE_SHEET_group_03_101(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 3: //STYLE_SHEET_HC_listbody1_102
                if (l.ty == 0/* EOF */) { return; }
                {
                    const cp = l.copy(), m = mark(), p = prod, s = stack_ptr;
                    State144(cp);
                    if (FAILED) {
                        prod = p;
                        FAILED = false;
                        stack_ptr = s;
                        reset(m);
                        return;
                    } else l.sync(cp);
                }
                break;
            case 2: //STYLE_SHEET_group_03_101
                State145(l);
                break;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State144(l: Lexer): void {
    /*
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_HC_listbody1_102 • STYLE_SHEET_group_03_101|$: 3;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_HC_listbody1_102 • STYLE_SHEET_group_03_101|id: 3;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_HC_listbody1_102 • STYLE_SHEET_group_03_101|key: 3;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_HC_listbody1_102 • STYLE_SHEET_group_03_101|-: 3;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_HC_listbody1_102 • STYLE_SHEET_group_03_101|*: 3;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_HC_listbody1_102 • STYLE_SHEET_group_03_101||: 3;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_HC_listbody1_102 • STYLE_SHEET_group_03_101|#: 3;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_HC_listbody1_102 • STYLE_SHEET_group_03_101|.: 3;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_HC_listbody1_102 • STYLE_SHEET_group_03_101|[: 3;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_HC_listbody1_102 • STYLE_SHEET_group_03_101|:: 3;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_HC_listbody1_102 • STYLE_SHEET_group_03_101|@: 3;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_HC_listbody1_102 • STYLE_SHEET_group_03_101|$eof: 3
*/
    _skip(l, const__);
    if (idm342.has(l.id)) { idm342.get(l.id)(l); } else if (l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        $STYLE_SHEET_group_03_101(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 2: //STYLE_SHEET_group_03_101
                State368(l);
                break;
            case 3/*STYLE_SHEET_HC_listbody1_102*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State145(l: Lexer): void {
    /*
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_group_03_101 •|$;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_group_03_101 •|id;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_group_03_101 •|key;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_group_03_101 •|-;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_group_03_101 •|*;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_group_03_101 •||;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_group_03_101 •|#;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_group_03_101 •|.;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_group_03_101 •|[;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_group_03_101 •|:;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_group_03_101 •|@;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_group_03_101 •|$eof
*/
    _skip(l, const__);
    if (idm145r.has(l.id)) { idm145r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProduction(2, 1, 3); stack_ptr -= 1;

        ; return;
    }
    else fail(l);
}
function State147(l: Lexer): void {
    /*
STYLE_SHEET_group_03_101=>AT_RULE •|$;
STYLE_SHEET_group_03_101=>AT_RULE •|id;
STYLE_SHEET_group_03_101=>AT_RULE •|key;
STYLE_SHEET_group_03_101=>AT_RULE •|-;
STYLE_SHEET_group_03_101=>AT_RULE •|*;
STYLE_SHEET_group_03_101=>AT_RULE •||;
STYLE_SHEET_group_03_101=>AT_RULE •|#;
STYLE_SHEET_group_03_101=>AT_RULE •|.;
STYLE_SHEET_group_03_101=>AT_RULE •|[;
STYLE_SHEET_group_03_101=>AT_RULE •|:;
STYLE_SHEET_group_03_101=>AT_RULE •|@;
STYLE_SHEET_group_03_101=>AT_RULE •|$eof
*/
    _skip(l, const__);
    if (idm147r.has(l.id)) { idm147r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProductionPlain(1, 2); stack_ptr -= 1;

        ; return;
    }
    else fail(l);
}
function $declaration_list_HC_listbody2_141(l: Lexer): void {
    /*
declaration_list_HC_listbody2_141=>• declaration_list_HC_listbody2_141 ;|;: 94;
declaration_list_HC_listbody2_141=>• ;|;;
declaration_list_HC_listbody2_141=>• declaration_list_HC_listbody2_141 ;|$: 94;
declaration_list_HC_listbody2_141=>• ;|$;
declaration_list_HC_listbody2_141=>• declaration_list_HC_listbody2_141 ;|id: 94;
declaration_list_HC_listbody2_141=>• ;|id;
declaration_list_HC_listbody2_141=>• declaration_list_HC_listbody2_141 ;|key: 94;
declaration_list_HC_listbody2_141=>• ;|key;
declaration_list_HC_listbody2_141=>• declaration_list_HC_listbody2_141 ;|-: 94;
declaration_list_HC_listbody2_141=>• ;|-;
declaration_list_HC_listbody2_141=>• declaration_list_HC_listbody2_141 ;|$eof: 94;
declaration_list_HC_listbody2_141=>• ;|$eof
*/
    _skip(l, const__);
    if (l.id == 12/* \; */) {

        _no_check(l);; stack_ptr++; State155(l);

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 94: //declaration_list_HC_listbody2_141
                if (l.id == 55/* \- */ || l.id == 56/* \$ */ || l.ty == 0/* EOF */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) { return; }
                {
                    const cp = l.copy(), m = mark(), p = prod, s = stack_ptr;
                    State154(cp);
                    if (FAILED) {
                        prod = p;
                        FAILED = false;
                        stack_ptr = s;
                        reset(m);
                        return;
                    } else l.sync(cp);
                }
                break;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State154(l: Lexer): void {
    /*
declaration_list_HC_listbody2_141=>declaration_list_HC_listbody2_141 • ;|;;
declaration_list_HC_listbody2_141=>declaration_list_HC_listbody2_141 • ;|$;
declaration_list_HC_listbody2_141=>declaration_list_HC_listbody2_141 • ;|id;
declaration_list_HC_listbody2_141=>declaration_list_HC_listbody2_141 • ;|key;
declaration_list_HC_listbody2_141=>declaration_list_HC_listbody2_141 • ;|-;
declaration_list_HC_listbody2_141=>declaration_list_HC_listbody2_141 • ;|$eof
*/
    _skip(l, const__);
    if (l.id == 12/* \; */) {

        _no_check(l);; stack_ptr++; State351(l);

    }
    else fail(l);
}
function State155(l: Lexer): void {
    /*
declaration_list_HC_listbody2_141=>; •|;;
declaration_list_HC_listbody2_141=>; •|$;
declaration_list_HC_listbody2_141=>; •|id;
declaration_list_HC_listbody2_141=>; •|key;
declaration_list_HC_listbody2_141=>; •|-;
declaration_list_HC_listbody2_141=>; •|$eof
*/
    _skip(l, const__);
    if (l.id == 12/* \; */ || l.id == 55/* \- */ || l.id == 56/* \$ */) {

        completeProduction(2, 1, 94); stack_ptr -= 1;

        ; return;
    } else if (l.ty == 0/* EOF */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProduction(2, 1, 94); stack_ptr -= 1;

        ; return;
    }
    else fail(l);
}
function $GROUP_RULE_BODY(l: Lexer): void {
    /*
GROUP_RULE_BODY=>• GROUP_RULE_BODY STYLE_RULE|$: 8;
GROUP_RULE_BODY=>• STYLE_RULE|$: 8;
GROUP_RULE_BODY=>• GROUP_RULE_BODY STYLE_RULE|id: 8;
GROUP_RULE_BODY=>• STYLE_RULE|id: 8;
GROUP_RULE_BODY=>• GROUP_RULE_BODY STYLE_RULE|key: 8;
GROUP_RULE_BODY=>• STYLE_RULE|key: 8;
GROUP_RULE_BODY=>• GROUP_RULE_BODY STYLE_RULE|-: 8;
GROUP_RULE_BODY=>• STYLE_RULE|-: 8;
GROUP_RULE_BODY=>• GROUP_RULE_BODY STYLE_RULE|*: 8;
GROUP_RULE_BODY=>• STYLE_RULE|*: 8;
GROUP_RULE_BODY=>• GROUP_RULE_BODY STYLE_RULE||: 8;
GROUP_RULE_BODY=>• STYLE_RULE||: 8;
GROUP_RULE_BODY=>• GROUP_RULE_BODY STYLE_RULE|#: 8;
GROUP_RULE_BODY=>• STYLE_RULE|#: 8;
GROUP_RULE_BODY=>• GROUP_RULE_BODY STYLE_RULE|.: 8;
GROUP_RULE_BODY=>• STYLE_RULE|.: 8;
GROUP_RULE_BODY=>• GROUP_RULE_BODY STYLE_RULE|[: 8;
GROUP_RULE_BODY=>• STYLE_RULE|[: 8;
GROUP_RULE_BODY=>• GROUP_RULE_BODY STYLE_RULE|:: 8;
GROUP_RULE_BODY=>• STYLE_RULE|:: 8;
GROUP_RULE_BODY=>• GROUP_RULE_BODY STYLE_RULE|}: 8;
GROUP_RULE_BODY=>• STYLE_RULE|}: 8;
GROUP_RULE_BODY=>• GROUP_RULE_BODY STYLE_RULE|$eof: 8;
GROUP_RULE_BODY=>• STYLE_RULE|$eof: 8
*/
    _skip(l, const__);
    if (idm156.has(l.id)) { idm156.get(l.id)(l); } else if (l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        $STYLE_RULE(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 8: //GROUP_RULE_BODY
                if (l.id == 13/* \} */ || l.ty == 0/* EOF */) { return; }
                {
                    const cp = l.copy(), m = mark(), p = prod, s = stack_ptr;
                    State157(cp);
                    if (FAILED) {
                        prod = p;
                        FAILED = false;
                        stack_ptr = s;
                        reset(m);
                        return;
                    } else l.sync(cp);
                }
                break;
            case 6: //STYLE_RULE
                State158(l);
                break;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State157(l: Lexer): void {
    /*
GROUP_RULE_BODY=>GROUP_RULE_BODY • STYLE_RULE|$: 8;
GROUP_RULE_BODY=>GROUP_RULE_BODY • STYLE_RULE|id: 8;
GROUP_RULE_BODY=>GROUP_RULE_BODY • STYLE_RULE|key: 8;
GROUP_RULE_BODY=>GROUP_RULE_BODY • STYLE_RULE|-: 8;
GROUP_RULE_BODY=>GROUP_RULE_BODY • STYLE_RULE|*: 8;
GROUP_RULE_BODY=>GROUP_RULE_BODY • STYLE_RULE||: 8;
GROUP_RULE_BODY=>GROUP_RULE_BODY • STYLE_RULE|#: 8;
GROUP_RULE_BODY=>GROUP_RULE_BODY • STYLE_RULE|.: 8;
GROUP_RULE_BODY=>GROUP_RULE_BODY • STYLE_RULE|[: 8;
GROUP_RULE_BODY=>GROUP_RULE_BODY • STYLE_RULE|:: 8;
GROUP_RULE_BODY=>GROUP_RULE_BODY • STYLE_RULE|}: 8;
GROUP_RULE_BODY=>GROUP_RULE_BODY • STYLE_RULE|$eof: 8
*/
    _skip(l, const__);
    if (idm156.has(l.id)) { idm156.get(l.id)(l); } else if (l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        $STYLE_RULE(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 6: //STYLE_RULE
                State369(l);
                break;
            case 8/*GROUP_RULE_BODY*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State158(l: Lexer): void {
    /*
GROUP_RULE_BODY=>STYLE_RULE •|$;
GROUP_RULE_BODY=>STYLE_RULE •|id;
GROUP_RULE_BODY=>STYLE_RULE •|key;
GROUP_RULE_BODY=>STYLE_RULE •|-;
GROUP_RULE_BODY=>STYLE_RULE •|*;
GROUP_RULE_BODY=>STYLE_RULE •||;
GROUP_RULE_BODY=>STYLE_RULE •|#;
GROUP_RULE_BODY=>STYLE_RULE •|.;
GROUP_RULE_BODY=>STYLE_RULE •|[;
GROUP_RULE_BODY=>STYLE_RULE •|:;
GROUP_RULE_BODY=>STYLE_RULE •|};
GROUP_RULE_BODY=>STYLE_RULE •|$eof
*/
    _skip(l, const__);
    if (idm158r.has(l.id)) { idm158r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProduction(2, 1, 8); stack_ptr -= 1;

        ; return;
    }
    else fail(l);
}
function $TYPE_SELECTOR(l: Lexer): void {
    /*
TYPE_SELECTOR=>• WQ_NAME|#: 89;
TYPE_SELECTOR=>• NS_PREFIX *|#: 89;
TYPE_SELECTOR=>• *|#;
TYPE_SELECTOR=>• WQ_NAME|.: 89;
TYPE_SELECTOR=>• NS_PREFIX *|.: 89;
TYPE_SELECTOR=>• *|.;
TYPE_SELECTOR=>• WQ_NAME|[: 89;
TYPE_SELECTOR=>• NS_PREFIX *|[: 89;
TYPE_SELECTOR=>• *|[;
TYPE_SELECTOR=>• WQ_NAME|:: 89;
TYPE_SELECTOR=>• NS_PREFIX *|:: 89;
TYPE_SELECTOR=>• *|:;
TYPE_SELECTOR=>• WQ_NAME|ws: 89;
TYPE_SELECTOR=>• NS_PREFIX *|ws: 89;
TYPE_SELECTOR=>• *|ws;
TYPE_SELECTOR=>• WQ_NAME|>: 89;
TYPE_SELECTOR=>• NS_PREFIX *|>: 89;
TYPE_SELECTOR=>• *|>;
TYPE_SELECTOR=>• WQ_NAME|+: 89;
TYPE_SELECTOR=>• NS_PREFIX *|+: 89;
TYPE_SELECTOR=>• *|+;
TYPE_SELECTOR=>• WQ_NAME|~: 89;
TYPE_SELECTOR=>• NS_PREFIX *|~: 89;
TYPE_SELECTOR=>• *|~;
TYPE_SELECTOR=>• WQ_NAME|||: 89;
TYPE_SELECTOR=>• NS_PREFIX *|||: 89;
TYPE_SELECTOR=>• *|||;
TYPE_SELECTOR=>• WQ_NAME|$: 89;
TYPE_SELECTOR=>• NS_PREFIX *|$: 89;
TYPE_SELECTOR=>• *|$;
TYPE_SELECTOR=>• WQ_NAME|id: 89;
TYPE_SELECTOR=>• NS_PREFIX *|id: 89;
TYPE_SELECTOR=>• *|id;
TYPE_SELECTOR=>• WQ_NAME|key: 89;
TYPE_SELECTOR=>• NS_PREFIX *|key: 89;
TYPE_SELECTOR=>• *|key;
TYPE_SELECTOR=>• WQ_NAME|-: 89;
TYPE_SELECTOR=>• NS_PREFIX *|-: 89;
TYPE_SELECTOR=>• *|-;
TYPE_SELECTOR=>• WQ_NAME|*: 89;
TYPE_SELECTOR=>• NS_PREFIX *|*: 89;
TYPE_SELECTOR=>• *|*;
TYPE_SELECTOR=>• WQ_NAME||: 89;
TYPE_SELECTOR=>• NS_PREFIX *||: 89;
TYPE_SELECTOR=>• *||;
TYPE_SELECTOR=>• WQ_NAME|): 89;
TYPE_SELECTOR=>• NS_PREFIX *|): 89;
TYPE_SELECTOR=>• *|);
TYPE_SELECTOR=>• WQ_NAME|,: 89;
TYPE_SELECTOR=>• NS_PREFIX *|,: 89;
TYPE_SELECTOR=>• *|,;
TYPE_SELECTOR=>• WQ_NAME|{: 89;
TYPE_SELECTOR=>• NS_PREFIX *|{: 89;
TYPE_SELECTOR=>• *|{;
TYPE_SELECTOR=>• WQ_NAME|$eof: 89;
TYPE_SELECTOR=>• NS_PREFIX *|$eof: 89;
TYPE_SELECTOR=>• *|$eof
*/
    _skip(l, const_1_);
    if (idm159.has(l.id)) { idm159.get(l.id)(l); } else if (l.ty == 3/* \id */ || l.ty == 7/* \key */) {
        let $mark = mark(), sp = stack_ptr, cp = l.copy();
        $WQ_NAME(cp); stack_ptr++;
        if (FAILED) {
            reset($mark); FAILED = false; stack_ptr = sp;
            $NS_PREFIX(l); stack_ptr++;;
        } else l.sync(cp);

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 93: //WQ_NAME
                State90(l);
                break;
            case 92: //NS_PREFIX
                State91(l);
                break;
            case 89/*TYPE_SELECTOR*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function $declaration_list_HC_listbody1_144(l: Lexer): void {
    /*
declaration_list_HC_listbody1_144=>• declaration_list_HC_listbody1_144 ;|;: 97;
declaration_list_HC_listbody1_144=>• ;|;;
declaration_list_HC_listbody1_144=>• declaration_list_HC_listbody1_144 ;|}: 97;
declaration_list_HC_listbody1_144=>• ;|};
declaration_list_HC_listbody1_144=>• declaration_list_HC_listbody1_144 ;|$eof: 97;
declaration_list_HC_listbody1_144=>• ;|$eof
*/
    _skip(l, const__);
    if (l.id == 12/* \; */) {

        _no_check(l);; stack_ptr++; State162(l);

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 97: //declaration_list_HC_listbody1_144
                if (l.id == 13/* \} */ || l.ty == 0/* EOF */) { return; }
                {
                    const cp = l.copy(), m = mark(), p = prod, s = stack_ptr;
                    State161(cp);
                    if (FAILED) {
                        prod = p;
                        FAILED = false;
                        stack_ptr = s;
                        reset(m);
                        return;
                    } else l.sync(cp);
                }
                break;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State161(l: Lexer): void {
    /*
declaration_list_HC_listbody1_144=>declaration_list_HC_listbody1_144 • ;|;;
declaration_list_HC_listbody1_144=>declaration_list_HC_listbody1_144 • ;|};
declaration_list_HC_listbody1_144=>declaration_list_HC_listbody1_144 • ;|$eof
*/
    _skip(l, const__);
    if (l.id == 12/* \; */) {

        _no_check(l);; stack_ptr++; State354(l);

    }
    else fail(l);
}
function State162(l: Lexer): void {
    /*
declaration_list_HC_listbody1_144=>; •|;;
declaration_list_HC_listbody1_144=>; •|};
declaration_list_HC_listbody1_144=>; •|$eof
*/
    _skip(l, const__);
    if (l.id == 12/* \; */ || l.id == 13/* \} */) {

        completeProduction(2, 1, 97); stack_ptr -= 1;

        ; return;
    } else if (l.ty == 0/* EOF */) {

        completeProduction(2, 1, 97); stack_ptr -= 1;

        ; return;
    }
    else fail(l);
}
function $STYLE_RULE(l: Lexer): void {
    /*
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { declaration_list ; }|$: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { ; }|$: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { declaration_list }|$: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { }|$: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { declaration_list ; }|id: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { ; }|id: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { declaration_list }|id: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { }|id: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { declaration_list ; }|key: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { ; }|key: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { declaration_list }|key: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { }|key: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { declaration_list ; }|-: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { ; }|-: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { declaration_list }|-: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { }|-: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { declaration_list ; }|*: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { ; }|*: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { declaration_list }|*: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { }|*: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { declaration_list ; }||: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { ; }||: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { declaration_list }||: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { }||: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { declaration_list ; }|#: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { ; }|#: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { declaration_list }|#: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { }|#: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { declaration_list ; }|.: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { ; }|.: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { declaration_list }|.: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { }|.: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { declaration_list ; }|[: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { ; }|[: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { declaration_list }|[: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { }|[: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { declaration_list ; }|:: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { ; }|:: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { declaration_list }|:: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { }|:: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { declaration_list ; }|@: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { ; }|@: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { declaration_list }|@: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { }|@: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { declaration_list ; }|$eof: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { ; }|$eof: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { declaration_list }|$eof: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { }|$eof: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { declaration_list ; }|}: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { ; }|}: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { declaration_list }|}: 6;
STYLE_RULE=>• STYLE_RULE_HC_listbody2_103 { }|}: 6
*/
    _skip(l, const__);
    if (idm166.has(l.id)) { idm166.get(l.id)(l); } else if (l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        $STYLE_RULE_HC_listbody2_103(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 5: //STYLE_RULE_HC_listbody2_103
                State133(l);
                break;
            case 6/*STYLE_RULE*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function $declaration_value(l: Lexer): void {
    /*
declaration_value=>• declaration_value string_group_0109_128|tok: 103;
declaration_value=>• string_group_0109_128|tok: 103;
declaration_value=>• declaration_value string_group_0109_128|key: 103;
declaration_value=>• string_group_0109_128|key: 103;
declaration_value=>• declaration_value string_group_0109_128|id: 103;
declaration_value=>• string_group_0109_128|id: 103;
declaration_value=>• declaration_value string_group_0109_128|sym: 103;
declaration_value=>• string_group_0109_128|sym: 103;
declaration_value=>• declaration_value string_group_0109_128|num: 103;
declaration_value=>• string_group_0109_128|num: 103;
declaration_value=>• declaration_value string_group_0109_128|ws: 103;
declaration_value=>• string_group_0109_128|ws: 103;
declaration_value=>• declaration_value string_group_0109_128|!: 103;
declaration_value=>• string_group_0109_128|!: 103;
declaration_value=>• declaration_value string_group_0109_128|(: 103;
declaration_value=>• string_group_0109_128|(: 103;
declaration_value=>• declaration_value string_group_0109_128|): 103;
declaration_value=>• string_group_0109_128|): 103;
declaration_value=>• declaration_value string_group_0109_128|;: 103;
declaration_value=>• string_group_0109_128|;: 103;
declaration_value=>• declaration_value string_group_0109_128|}: 103;
declaration_value=>• string_group_0109_128|}: 103;
declaration_value=>• declaration_value string_group_0109_128|$eof: 103;
declaration_value=>• string_group_0109_128|$eof: 103
*/
    _skip(l, const_1_);
    if (l.ty == 1/* \ws */ || l.ty == 2/* \num */ || l.ty == 3/* \id */ || l.ty == 5/* \tok */ || l.ty == 6/* \sym */ || l.ty == 7/* \key */) {

        $string_group_0109_128(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 103: //declaration_value
                if (l.id == 12/* \; */ || l.id == 13/* \} */ || l.id == 17/* \( */ || l.id == 18/* \) */ || l.id == 52/* \! */ || l.ty == 0/* EOF */) { return; }
                {
                    const cp = l.copy(), m = mark(), p = prod, s = stack_ptr;
                    State168(cp);
                    if (FAILED) {
                        prod = p;
                        FAILED = false;
                        stack_ptr = s;
                        reset(m);
                        return;
                    } else l.sync(cp);
                }
                break;
            case 66: //string_group_0109_128
                State169(l);
                break;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State168(l: Lexer): void {
    /*
declaration_value=>declaration_value • string_group_0109_128|tok: 103;
declaration_value=>declaration_value • string_group_0109_128|key: 103;
declaration_value=>declaration_value • string_group_0109_128|id: 103;
declaration_value=>declaration_value • string_group_0109_128|sym: 103;
declaration_value=>declaration_value • string_group_0109_128|num: 103;
declaration_value=>declaration_value • string_group_0109_128|ws: 103;
declaration_value=>declaration_value • string_group_0109_128|!: 103;
declaration_value=>declaration_value • string_group_0109_128|(: 103;
declaration_value=>declaration_value • string_group_0109_128|): 103;
declaration_value=>declaration_value • string_group_0109_128|;: 103;
declaration_value=>declaration_value • string_group_0109_128|}: 103;
declaration_value=>declaration_value • string_group_0109_128|$eof: 103
*/
    _skip(l, const_1_);
    if (l.ty == 1/* \ws */ || l.ty == 2/* \num */ || l.ty == 3/* \id */ || l.ty == 5/* \tok */ || l.ty == 6/* \sym */ || l.ty == 7/* \key */) {

        $string_group_0109_128(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 66: //string_group_0109_128
                State356(l);
                break;
            case 103/*declaration_value*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State169(l: Lexer): void {
    /*
declaration_value=>string_group_0109_128 •|tok;
declaration_value=>string_group_0109_128 •|key;
declaration_value=>string_group_0109_128 •|id;
declaration_value=>string_group_0109_128 •|sym;
declaration_value=>string_group_0109_128 •|num;
declaration_value=>string_group_0109_128 •|ws;
declaration_value=>string_group_0109_128 •|!;
declaration_value=>string_group_0109_128 •|(;
declaration_value=>string_group_0109_128 •|);
declaration_value=>string_group_0109_128 •|;;
declaration_value=>string_group_0109_128 •|};
declaration_value=>string_group_0109_128 •|$eof
*/
    _skip(l, const_1_);
    if (l.id == 12/* \; */ || l.id == 13/* \} */ || l.id == 17/* \( */ || l.id == 18/* \) */ || l.id == 52/* \! */) {

        completeProduction(35, 1, 103); stack_ptr -= 1;

        ; return;
    } else if (l.ty == 0/* EOF */ || l.ty == 1/* \ws */ || l.ty == 2/* \num */ || l.ty == 3/* \id */ || l.ty == 5/* \tok */ || l.ty == 6/* \sym */ || l.ty == 7/* \key */) {

        completeProduction(35, 1, 103); stack_ptr -= 1;

        ; return;
    }
    else fail(l);
}
function $declaration_list_HC_listbody2_143(l: Lexer): void {
    /*
declaration_list_HC_listbody2_143=>• declaration_list_HC_listbody2_143 ; declaration_list_group_1153_142|;: 96;
declaration_list_HC_listbody2_143=>• declaration_list_group_1153_142|;: 96;
declaration_list_HC_listbody2_143=>• declaration_list_HC_listbody2_143 ; declaration_list_group_1153_142|}: 96;
declaration_list_HC_listbody2_143=>• declaration_list_group_1153_142|}: 96;
declaration_list_HC_listbody2_143=>• declaration_list_HC_listbody2_143 ; declaration_list_group_1153_142|$eof: 96;
declaration_list_HC_listbody2_143=>• declaration_list_group_1153_142|$eof: 96
*/
    _skip(l, const__);
    if (l.id == 12/* \; */ || l.id == 55/* \- */ || l.id == 56/* \$ */) {

        $declaration_list_group_1153_142(l); stack_ptr++;

    } else if (l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        $declaration_list_group_1153_142(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 96: //declaration_list_HC_listbody2_143
                if (l.id == 13/* \} */ || l.ty == 0/* EOF */) { return; }
                {
                    const cp = l.copy(), m = mark(), p = prod, s = stack_ptr;
                    State171(cp);
                    if (FAILED) {
                        prod = p;
                        FAILED = false;
                        stack_ptr = s;
                        reset(m);
                        return;
                    } else l.sync(cp);
                }
                break;
            case 95: //declaration_list_group_1153_142
                State172(l);
                break;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State171(l: Lexer): void {
    /*
declaration_list_HC_listbody2_143=>declaration_list_HC_listbody2_143 • ; declaration_list_group_1153_142|;;
declaration_list_HC_listbody2_143=>declaration_list_HC_listbody2_143 • ; declaration_list_group_1153_142|};
declaration_list_HC_listbody2_143=>declaration_list_HC_listbody2_143 • ; declaration_list_group_1153_142|$eof
*/
    _skip(l, const__);
    if (l.id == 12/* \; */) {

        _no_check(l);; stack_ptr++; State357(l);

    }
    else fail(l);
}
function State172(l: Lexer): void {
    /*
declaration_list_HC_listbody2_143=>declaration_list_group_1153_142 •|;;
declaration_list_HC_listbody2_143=>declaration_list_group_1153_142 •|};
declaration_list_HC_listbody2_143=>declaration_list_group_1153_142 •|$eof
*/
    _skip(l, const__);
    if (l.id == 12/* \; */ || l.id == 13/* \} */) {

        completeProduction(2, 1, 96); stack_ptr -= 1;

        ; return;
    } else if (l.ty == 0/* EOF */) {

        completeProduction(2, 1, 96); stack_ptr -= 1;

        ; return;
    }
    else fail(l);
}
function $declaration_values(l: Lexer): void {
    /*
declaration_values=>• declaration_value|!: 101;
declaration_values=>• declaration_values ( declaration_values )|!: 101;
declaration_values=>• declaration_values declaration_value|!: 101;
declaration_values=>• declaration_value|(: 101;
declaration_values=>• declaration_values ( declaration_values )|(: 101;
declaration_values=>• declaration_values declaration_value|(: 101;
declaration_values=>• declaration_value|): 101;
declaration_values=>• declaration_values ( declaration_values )|): 101;
declaration_values=>• declaration_values declaration_value|): 101;
declaration_values=>• declaration_value|tok: 101;
declaration_values=>• declaration_values ( declaration_values )|tok: 101;
declaration_values=>• declaration_values declaration_value|tok: 101;
declaration_values=>• declaration_value|key: 101;
declaration_values=>• declaration_values ( declaration_values )|key: 101;
declaration_values=>• declaration_values declaration_value|key: 101;
declaration_values=>• declaration_value|id: 101;
declaration_values=>• declaration_values ( declaration_values )|id: 101;
declaration_values=>• declaration_values declaration_value|id: 101;
declaration_values=>• declaration_value|sym: 101;
declaration_values=>• declaration_values ( declaration_values )|sym: 101;
declaration_values=>• declaration_values declaration_value|sym: 101;
declaration_values=>• declaration_value|num: 101;
declaration_values=>• declaration_values ( declaration_values )|num: 101;
declaration_values=>• declaration_values declaration_value|num: 101;
declaration_values=>• declaration_value|ws: 101;
declaration_values=>• declaration_values ( declaration_values )|ws: 101;
declaration_values=>• declaration_values declaration_value|ws: 101;
declaration_values=>• declaration_value|;: 101;
declaration_values=>• declaration_values ( declaration_values )|;: 101;
declaration_values=>• declaration_values declaration_value|;: 101;
declaration_values=>• declaration_value|}: 101;
declaration_values=>• declaration_values ( declaration_values )|}: 101;
declaration_values=>• declaration_values declaration_value|}: 101;
declaration_values=>• declaration_value|$eof: 101;
declaration_values=>• declaration_values ( declaration_values )|$eof: 101;
declaration_values=>• declaration_values declaration_value|$eof: 101
*/
    _skip(l, const_1_);
    if (l.ty == 1/* \ws */ || l.ty == 2/* \num */ || l.ty == 3/* \id */ || l.ty == 5/* \tok */ || l.ty == 6/* \sym */ || l.ty == 7/* \key */) {

        $declaration_value(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 103: //declaration_value
                State177(l);
                break;
            case 101: //declaration_values
                if (l.id == 12/* \; */ || l.id == 13/* \} */ || l.id == 18/* \) */ || l.id == 52/* \! */ || l.ty == 0/* EOF */) { return; }
                {
                    const cp = l.copy(), m = mark(), p = prod, s = stack_ptr;
                    State178(cp);
                    if (FAILED) {
                        prod = p;
                        FAILED = false;
                        stack_ptr = s;
                        reset(m);
                        return;
                    } else l.sync(cp);
                }
                break;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State177(l: Lexer): void {
    /*
declaration_values=>declaration_value •|!;
declaration_values=>declaration_value •|(;
declaration_values=>declaration_value •|);
declaration_values=>declaration_value •|tok;
declaration_values=>declaration_value •|key;
declaration_values=>declaration_value •|id;
declaration_values=>declaration_value •|sym;
declaration_values=>declaration_value •|num;
declaration_values=>declaration_value •|ws;
declaration_values=>declaration_value •|;;
declaration_values=>declaration_value •|};
declaration_values=>declaration_value •|$eof;
declaration_value=>declaration_value • string_group_0109_128|!: 103;
declaration_value=>declaration_value • string_group_0109_128|tok: 103;
declaration_value=>declaration_value • string_group_0109_128|key: 103;
declaration_value=>declaration_value • string_group_0109_128|id: 103;
declaration_value=>declaration_value • string_group_0109_128|sym: 103;
declaration_value=>declaration_value • string_group_0109_128|num: 103;
declaration_value=>declaration_value • string_group_0109_128|ws: 103;
declaration_value=>declaration_value • string_group_0109_128|(: 103;
declaration_value=>declaration_value • string_group_0109_128|): 103;
declaration_value=>declaration_value • string_group_0109_128|;: 103;
declaration_value=>declaration_value • string_group_0109_128|}: 103;
declaration_value=>declaration_value • string_group_0109_128|$eof: 103
*/
    _skip(l, const_1_);
    if (l.id == 12/* \; */ || l.id == 13/* \} */ || l.id == 17/* \( */ || l.id == 18/* \) */ || l.id == 52/* \! */) {

        completeProductionPlain(1, 101); stack_ptr -= 1;

        ; return;
    } else if (l.ty == 1/* \ws */ || l.ty == 2/* \num */ || l.ty == 3/* \id */ || l.ty == 5/* \tok */ || l.ty == 6/* \sym */ || l.ty == 7/* \key */) {

        $string_group_0109_128(l); stack_ptr++;

    } else if (l.ty == 0/* EOF */ || l.ty == 1/* \ws */ || l.ty == 2/* \num */ || l.ty == 3/* \id */ || l.ty == 5/* \tok */ || l.ty == 6/* \sym */ || l.ty == 7/* \key */) {

        completeProductionPlain(1, 101); stack_ptr -= 1;

        ; return;
    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 66: //string_group_0109_128
                State356(l);
                break;
            case 101/*declaration_values*/:
            case 103/*declaration_value*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State178(l: Lexer): void {
    /*
declaration_values=>declaration_values • ( declaration_values )|!;
declaration_values=>declaration_values • declaration_value|!: 101;
declaration_values=>declaration_values • ( declaration_values )|(;
declaration_values=>declaration_values • declaration_value|(: 101;
declaration_values=>declaration_values • ( declaration_values )|);
declaration_values=>declaration_values • declaration_value|): 101;
declaration_values=>declaration_values • ( declaration_values )|tok;
declaration_values=>declaration_values • declaration_value|tok: 101;
declaration_values=>declaration_values • ( declaration_values )|key;
declaration_values=>declaration_values • declaration_value|key: 101;
declaration_values=>declaration_values • ( declaration_values )|id;
declaration_values=>declaration_values • declaration_value|id: 101;
declaration_values=>declaration_values • ( declaration_values )|sym;
declaration_values=>declaration_values • declaration_value|sym: 101;
declaration_values=>declaration_values • ( declaration_values )|num;
declaration_values=>declaration_values • declaration_value|num: 101;
declaration_values=>declaration_values • ( declaration_values )|ws;
declaration_values=>declaration_values • declaration_value|ws: 101;
declaration_values=>declaration_values • ( declaration_values )|;;
declaration_values=>declaration_values • declaration_value|;: 101;
declaration_values=>declaration_values • ( declaration_values )|};
declaration_values=>declaration_values • declaration_value|}: 101;
declaration_values=>declaration_values • ( declaration_values )|$eof;
declaration_values=>declaration_values • declaration_value|$eof: 101
*/
    _skip(l, const_1_);
    if (l.id == 17/* \( */) {

        _no_check(l);; stack_ptr++; State359(l);

    } else if (l.ty == 1/* \ws */ || l.ty == 2/* \num */ || l.ty == 3/* \id */ || l.ty == 5/* \tok */ || l.ty == 6/* \sym */ || l.ty == 7/* \key */) {

        $declaration_value(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 103: //declaration_value
                State360(l);
                break;
            case 101/*declaration_values*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function $WQ_NAME(l: Lexer): void {
    /*
WQ_NAME=>• NS_PREFIX identifier|]: 93;
WQ_NAME=>• identifier|]: 93;
WQ_NAME=>• NS_PREFIX identifier|~: 93;
WQ_NAME=>• identifier|~: 93;
WQ_NAME=>• NS_PREFIX identifier|^=: 93;
WQ_NAME=>• identifier|^=: 93;
WQ_NAME=>• NS_PREFIX identifier|$=: 93;
WQ_NAME=>• identifier|$=: 93;
WQ_NAME=>• NS_PREFIX identifier|*=: 93;
WQ_NAME=>• identifier|*=: 93;
WQ_NAME=>• NS_PREFIX identifier|=: 93;
WQ_NAME=>• identifier|=: 93;
WQ_NAME=>• NS_PREFIX identifier|#: 93;
WQ_NAME=>• identifier|#: 93;
WQ_NAME=>• NS_PREFIX identifier|.: 93;
WQ_NAME=>• identifier|.: 93;
WQ_NAME=>• NS_PREFIX identifier|[: 93;
WQ_NAME=>• identifier|[: 93;
WQ_NAME=>• NS_PREFIX identifier|:: 93;
WQ_NAME=>• identifier|:: 93;
WQ_NAME=>• NS_PREFIX identifier|ws: 93;
WQ_NAME=>• identifier|ws: 93;
WQ_NAME=>• NS_PREFIX identifier|>: 93;
WQ_NAME=>• identifier|>: 93;
WQ_NAME=>• NS_PREFIX identifier|+: 93;
WQ_NAME=>• identifier|+: 93;
WQ_NAME=>• NS_PREFIX identifier|||: 93;
WQ_NAME=>• identifier|||: 93;
WQ_NAME=>• NS_PREFIX identifier|$: 93;
WQ_NAME=>• identifier|$: 93;
WQ_NAME=>• NS_PREFIX identifier|id: 93;
WQ_NAME=>• identifier|id: 93;
WQ_NAME=>• NS_PREFIX identifier|key: 93;
WQ_NAME=>• identifier|key: 93;
WQ_NAME=>• NS_PREFIX identifier|-: 93;
WQ_NAME=>• identifier|-: 93;
WQ_NAME=>• NS_PREFIX identifier|*: 93;
WQ_NAME=>• identifier|*: 93;
WQ_NAME=>• NS_PREFIX identifier||: 93;
WQ_NAME=>• identifier||: 93;
WQ_NAME=>• NS_PREFIX identifier|): 93;
WQ_NAME=>• identifier|): 93;
WQ_NAME=>• NS_PREFIX identifier|,: 93;
WQ_NAME=>• identifier|,: 93;
WQ_NAME=>• NS_PREFIX identifier|{: 93;
WQ_NAME=>• identifier|{: 93;
WQ_NAME=>• NS_PREFIX identifier|$eof: 93;
WQ_NAME=>• identifier|$eof: 93
*/
    _skip(l, const_1_);
    if (idm179.has(l.id)) { idm179.get(l.id)(l); } else if (l.ty == 3/* \id */ || l.ty == 7/* \key */) {
        let $mark = mark(), sp = stack_ptr, cp = l.copy();
        $NS_PREFIX(cp); stack_ptr++;
        if (FAILED) {
            reset($mark); FAILED = false; stack_ptr = sp;
            $identifier(l); stack_ptr++;;
        } else l.sync(cp);

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 104: //identifier
                State93(l);
                break;
            case 92: //NS_PREFIX
                State180(l);
                break;
            case 91: //NS_PREFIX_group_0150_140
                State94(l);
                break;
            case 93/*WQ_NAME*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State180(l: Lexer): void {
    /*
WQ_NAME=>NS_PREFIX • identifier|]: 93;
WQ_NAME=>NS_PREFIX • identifier|~: 93;
WQ_NAME=>NS_PREFIX • identifier|^=: 93;
WQ_NAME=>NS_PREFIX • identifier|$=: 93;
WQ_NAME=>NS_PREFIX • identifier|*=: 93;
WQ_NAME=>NS_PREFIX • identifier|=: 93;
WQ_NAME=>NS_PREFIX • identifier|#: 93;
WQ_NAME=>NS_PREFIX • identifier|.: 93;
WQ_NAME=>NS_PREFIX • identifier|[: 93;
WQ_NAME=>NS_PREFIX • identifier|:: 93;
WQ_NAME=>NS_PREFIX • identifier|ws: 93;
WQ_NAME=>NS_PREFIX • identifier|>: 93;
WQ_NAME=>NS_PREFIX • identifier|+: 93;
WQ_NAME=>NS_PREFIX • identifier|||: 93;
WQ_NAME=>NS_PREFIX • identifier|$: 93;
WQ_NAME=>NS_PREFIX • identifier|id: 93;
WQ_NAME=>NS_PREFIX • identifier|key: 93;
WQ_NAME=>NS_PREFIX • identifier|-: 93;
WQ_NAME=>NS_PREFIX • identifier|*: 93;
WQ_NAME=>NS_PREFIX • identifier||: 93;
WQ_NAME=>NS_PREFIX • identifier|): 93;
WQ_NAME=>NS_PREFIX • identifier|,: 93;
WQ_NAME=>NS_PREFIX • identifier|{: 93;
WQ_NAME=>NS_PREFIX • identifier|$eof: 93
*/
    _skip(l, const_1_);
    if (l.id == 55/* \- */ || l.id == 56/* \$ */) {

        $identifier(l); stack_ptr++;

    } else if (l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        $identifier(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 104: //identifier
                State328(l);
                break;
            case 93/*WQ_NAME*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function $string_value(l: Lexer): void {
    /*
string_value=>• string_value string_group_0109_128|): 108;
string_value=>• string_group_0109_128|): 108;
string_value=>• string_value string_group_0109_128|": 108;
string_value=>• string_group_0109_128|": 108;
string_value=>• string_value string_group_0109_128|tok: 108;
string_value=>• string_group_0109_128|tok: 108;
string_value=>• string_value string_group_0109_128|key: 108;
string_value=>• string_group_0109_128|key: 108;
string_value=>• string_value string_group_0109_128|id: 108;
string_value=>• string_group_0109_128|id: 108;
string_value=>• string_value string_group_0109_128|sym: 108;
string_value=>• string_group_0109_128|sym: 108;
string_value=>• string_value string_group_0109_128|num: 108;
string_value=>• string_group_0109_128|num: 108;
string_value=>• string_value string_group_0109_128|ws: 108;
string_value=>• string_group_0109_128|ws: 108;
string_value=>• string_value string_group_0109_128|$eof: 108;
string_value=>• string_group_0109_128|$eof: 108
*/
    _skip(l, const_1_);
    if (l.ty == 1/* \ws */ || l.ty == 2/* \num */ || l.ty == 3/* \id */ || l.ty == 5/* \tok */ || l.ty == 6/* \sym */ || l.ty == 7/* \key */) {

        $string_group_0109_128(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 108: //string_value
                if (l.id == 18/* \) */ || l.id == 27/* \" */ || l.ty == 0/* EOF */) { return; }
                {
                    const cp = l.copy(), m = mark(), p = prod, s = stack_ptr;
                    State186(cp);
                    if (FAILED) {
                        prod = p;
                        FAILED = false;
                        stack_ptr = s;
                        reset(m);
                        return;
                    } else l.sync(cp);
                }
                break;
            case 66: //string_group_0109_128
                State187(l);
                break;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State186(l: Lexer): void {
    /*
string_value=>string_value • string_group_0109_128|): 108;
string_value=>string_value • string_group_0109_128|": 108;
string_value=>string_value • string_group_0109_128|tok: 108;
string_value=>string_value • string_group_0109_128|key: 108;
string_value=>string_value • string_group_0109_128|id: 108;
string_value=>string_value • string_group_0109_128|sym: 108;
string_value=>string_value • string_group_0109_128|num: 108;
string_value=>string_value • string_group_0109_128|ws: 108;
string_value=>string_value • string_group_0109_128|$eof: 108
*/
    _skip(l, const_1_);
    if (l.ty == 1/* \ws */ || l.ty == 2/* \num */ || l.ty == 3/* \id */ || l.ty == 5/* \tok */ || l.ty == 6/* \sym */ || l.ty == 7/* \key */) {

        $string_group_0109_128(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 66: //string_group_0109_128
                State363(l);
                break;
            case 108/*string_value*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State187(l: Lexer): void {
    /*
string_value=>string_group_0109_128 •|);
string_value=>string_group_0109_128 •|";
string_value=>string_group_0109_128 •|tok;
string_value=>string_group_0109_128 •|key;
string_value=>string_group_0109_128 •|id;
string_value=>string_group_0109_128 •|sym;
string_value=>string_group_0109_128 •|num;
string_value=>string_group_0109_128 •|ws;
string_value=>string_group_0109_128 •|$eof
*/
    _skip(l, const_1_);
    if (l.id == 18/* \) */ || l.id == 27/* \" */) {

        completeProduction(35, 1, 108); stack_ptr -= 1;

        ; return;
    } else if (l.ty == 0/* EOF */ || l.ty == 1/* \ws */ || l.ty == 2/* \num */ || l.ty == 3/* \id */ || l.ty == 5/* \tok */ || l.ty == 6/* \sym */ || l.ty == 7/* \key */) {

        completeProduction(35, 1, 108); stack_ptr -= 1;

        ; return;
    }
    else fail(l);
}
function $css_id_symbols(l: Lexer): void {
    /*
css_id_symbols=>• css_id_symbols θid|ws: 105;
css_id_symbols=>• css_id_symbols θkey|ws: 105;
css_id_symbols=>• css_id_symbols _|ws: 105;
css_id_symbols=>• css_id_symbols -|ws: 105;
css_id_symbols=>• css_id_symbols $|ws: 105;
css_id_symbols=>• css_id_symbols θnum|ws: 105;
css_id_symbols=>• $|ws;
css_id_symbols=>• θid|ws;
css_id_symbols=>• θkey|ws;
css_id_symbols=>• -|ws;
css_id_symbols=>• css_id_symbols θid|id: 105;
css_id_symbols=>• css_id_symbols θkey|id: 105;
css_id_symbols=>• css_id_symbols _|id: 105;
css_id_symbols=>• css_id_symbols -|id: 105;
css_id_symbols=>• css_id_symbols $|id: 105;
css_id_symbols=>• css_id_symbols θnum|id: 105;
css_id_symbols=>• $|id;
css_id_symbols=>• θid|id;
css_id_symbols=>• θkey|id;
css_id_symbols=>• -|id;
css_id_symbols=>• css_id_symbols θid|key: 105;
css_id_symbols=>• css_id_symbols θkey|key: 105;
css_id_symbols=>• css_id_symbols _|key: 105;
css_id_symbols=>• css_id_symbols -|key: 105;
css_id_symbols=>• css_id_symbols $|key: 105;
css_id_symbols=>• css_id_symbols θnum|key: 105;
css_id_symbols=>• $|key;
css_id_symbols=>• θid|key;
css_id_symbols=>• θkey|key;
css_id_symbols=>• -|key;
css_id_symbols=>• css_id_symbols θid|_: 105;
css_id_symbols=>• css_id_symbols θkey|_: 105;
css_id_symbols=>• css_id_symbols _|_: 105;
css_id_symbols=>• css_id_symbols -|_: 105;
css_id_symbols=>• css_id_symbols $|_: 105;
css_id_symbols=>• css_id_symbols θnum|_: 105;
css_id_symbols=>• $|_;
css_id_symbols=>• θid|_;
css_id_symbols=>• θkey|_;
css_id_symbols=>• -|_;
css_id_symbols=>• css_id_symbols θid|-: 105;
css_id_symbols=>• css_id_symbols θkey|-: 105;
css_id_symbols=>• css_id_symbols _|-: 105;
css_id_symbols=>• css_id_symbols -|-: 105;
css_id_symbols=>• css_id_symbols $|-: 105;
css_id_symbols=>• css_id_symbols θnum|-: 105;
css_id_symbols=>• $|-;
css_id_symbols=>• θid|-;
css_id_symbols=>• θkey|-;
css_id_symbols=>• -|-;
css_id_symbols=>• css_id_symbols θid|$: 105;
css_id_symbols=>• css_id_symbols θkey|$: 105;
css_id_symbols=>• css_id_symbols _|$: 105;
css_id_symbols=>• css_id_symbols -|$: 105;
css_id_symbols=>• css_id_symbols $|$: 105;
css_id_symbols=>• css_id_symbols θnum|$: 105;
css_id_symbols=>• $|$;
css_id_symbols=>• θid|$;
css_id_symbols=>• θkey|$;
css_id_symbols=>• -|$;
css_id_symbols=>• css_id_symbols θid|num: 105;
css_id_symbols=>• css_id_symbols θkey|num: 105;
css_id_symbols=>• css_id_symbols _|num: 105;
css_id_symbols=>• css_id_symbols -|num: 105;
css_id_symbols=>• css_id_symbols $|num: 105;
css_id_symbols=>• css_id_symbols θnum|num: 105;
css_id_symbols=>• $|num;
css_id_symbols=>• θid|num;
css_id_symbols=>• θkey|num;
css_id_symbols=>• -|num;
css_id_symbols=>• css_id_symbols θid|(: 105;
css_id_symbols=>• css_id_symbols θkey|(: 105;
css_id_symbols=>• css_id_symbols _|(: 105;
css_id_symbols=>• css_id_symbols -|(: 105;
css_id_symbols=>• css_id_symbols $|(: 105;
css_id_symbols=>• css_id_symbols θnum|(: 105;
css_id_symbols=>• $|(;
css_id_symbols=>• θid|(;
css_id_symbols=>• θkey|(;
css_id_symbols=>• -|(;
css_id_symbols=>• css_id_symbols θid|>: 105;
css_id_symbols=>• css_id_symbols θkey|>: 105;
css_id_symbols=>• css_id_symbols _|>: 105;
css_id_symbols=>• css_id_symbols -|>: 105;
css_id_symbols=>• css_id_symbols $|>: 105;
css_id_symbols=>• css_id_symbols θnum|>: 105;
css_id_symbols=>• $|>;
css_id_symbols=>• θid|>;
css_id_symbols=>• θkey|>;
css_id_symbols=>• -|>;
css_id_symbols=>• css_id_symbols θid|<: 105;
css_id_symbols=>• css_id_symbols θkey|<: 105;
css_id_symbols=>• css_id_symbols _|<: 105;
css_id_symbols=>• css_id_symbols -|<: 105;
css_id_symbols=>• css_id_symbols $|<: 105;
css_id_symbols=>• css_id_symbols θnum|<: 105;
css_id_symbols=>• $|<;
css_id_symbols=>• θid|<;
css_id_symbols=>• θkey|<;
css_id_symbols=>• -|<;
css_id_symbols=>• css_id_symbols θid|:: 105;
css_id_symbols=>• css_id_symbols θkey|:: 105;
css_id_symbols=>• css_id_symbols _|:: 105;
css_id_symbols=>• css_id_symbols -|:: 105;
css_id_symbols=>• css_id_symbols $|:: 105;
css_id_symbols=>• css_id_symbols θnum|:: 105;
css_id_symbols=>• $|:;
css_id_symbols=>• θid|:;
css_id_symbols=>• θkey|:;
css_id_symbols=>• -|:;
css_id_symbols=>• css_id_symbols θid|=: 105;
css_id_symbols=>• css_id_symbols θkey|=: 105;
css_id_symbols=>• css_id_symbols _|=: 105;
css_id_symbols=>• css_id_symbols -|=: 105;
css_id_symbols=>• css_id_symbols $|=: 105;
css_id_symbols=>• css_id_symbols θnum|=: 105;
css_id_symbols=>• $|=;
css_id_symbols=>• θid|=;
css_id_symbols=>• θkey|=;
css_id_symbols=>• -|=;
css_id_symbols=>• css_id_symbols θid|): 105;
css_id_symbols=>• css_id_symbols θkey|): 105;
css_id_symbols=>• css_id_symbols _|): 105;
css_id_symbols=>• css_id_symbols -|): 105;
css_id_symbols=>• css_id_symbols $|): 105;
css_id_symbols=>• css_id_symbols θnum|): 105;
css_id_symbols=>• $|);
css_id_symbols=>• θid|);
css_id_symbols=>• θkey|);
css_id_symbols=>• -|);
css_id_symbols=>• css_id_symbols θid|and: 105;
css_id_symbols=>• css_id_symbols θkey|and: 105;
css_id_symbols=>• css_id_symbols _|and: 105;
css_id_symbols=>• css_id_symbols -|and: 105;
css_id_symbols=>• css_id_symbols $|and: 105;
css_id_symbols=>• css_id_symbols θnum|and: 105;
css_id_symbols=>• $|and;
css_id_symbols=>• θid|and;
css_id_symbols=>• θkey|and;
css_id_symbols=>• -|and;
css_id_symbols=>• css_id_symbols θid|,: 105;
css_id_symbols=>• css_id_symbols θkey|,: 105;
css_id_symbols=>• css_id_symbols _|,: 105;
css_id_symbols=>• css_id_symbols -|,: 105;
css_id_symbols=>• css_id_symbols $|,: 105;
css_id_symbols=>• css_id_symbols θnum|,: 105;
css_id_symbols=>• $|,;
css_id_symbols=>• θid|,;
css_id_symbols=>• θkey|,;
css_id_symbols=>• -|,;
css_id_symbols=>• css_id_symbols θid|;: 105;
css_id_symbols=>• css_id_symbols θkey|;: 105;
css_id_symbols=>• css_id_symbols _|;: 105;
css_id_symbols=>• css_id_symbols -|;: 105;
css_id_symbols=>• css_id_symbols $|;: 105;
css_id_symbols=>• css_id_symbols θnum|;: 105;
css_id_symbols=>• $|;;
css_id_symbols=>• θid|;;
css_id_symbols=>• θkey|;;
css_id_symbols=>• -|;;
css_id_symbols=>• css_id_symbols θid|@: 105;
css_id_symbols=>• css_id_symbols θkey|@: 105;
css_id_symbols=>• css_id_symbols _|@: 105;
css_id_symbols=>• css_id_symbols -|@: 105;
css_id_symbols=>• css_id_symbols $|@: 105;
css_id_symbols=>• css_id_symbols θnum|@: 105;
css_id_symbols=>• $|@;
css_id_symbols=>• θid|@;
css_id_symbols=>• θkey|@;
css_id_symbols=>• -|@;
css_id_symbols=>• css_id_symbols θid|*: 105;
css_id_symbols=>• css_id_symbols θkey|*: 105;
css_id_symbols=>• css_id_symbols _|*: 105;
css_id_symbols=>• css_id_symbols -|*: 105;
css_id_symbols=>• css_id_symbols $|*: 105;
css_id_symbols=>• css_id_symbols θnum|*: 105;
css_id_symbols=>• $|*;
css_id_symbols=>• θid|*;
css_id_symbols=>• θkey|*;
css_id_symbols=>• -|*;
css_id_symbols=>• css_id_symbols θid||: 105;
css_id_symbols=>• css_id_symbols θkey||: 105;
css_id_symbols=>• css_id_symbols _||: 105;
css_id_symbols=>• css_id_symbols -||: 105;
css_id_symbols=>• css_id_symbols $||: 105;
css_id_symbols=>• css_id_symbols θnum||: 105;
css_id_symbols=>• $||;
css_id_symbols=>• θid||;
css_id_symbols=>• θkey||;
css_id_symbols=>• -||;
css_id_symbols=>• css_id_symbols θid|#: 105;
css_id_symbols=>• css_id_symbols θkey|#: 105;
css_id_symbols=>• css_id_symbols _|#: 105;
css_id_symbols=>• css_id_symbols -|#: 105;
css_id_symbols=>• css_id_symbols $|#: 105;
css_id_symbols=>• css_id_symbols θnum|#: 105;
css_id_symbols=>• $|#;
css_id_symbols=>• θid|#;
css_id_symbols=>• θkey|#;
css_id_symbols=>• -|#;
css_id_symbols=>• css_id_symbols θid|.: 105;
css_id_symbols=>• css_id_symbols θkey|.: 105;
css_id_symbols=>• css_id_symbols _|.: 105;
css_id_symbols=>• css_id_symbols -|.: 105;
css_id_symbols=>• css_id_symbols $|.: 105;
css_id_symbols=>• css_id_symbols θnum|.: 105;
css_id_symbols=>• $|.;
css_id_symbols=>• θid|.;
css_id_symbols=>• θkey|.;
css_id_symbols=>• -|.;
css_id_symbols=>• css_id_symbols θid|[: 105;
css_id_symbols=>• css_id_symbols θkey|[: 105;
css_id_symbols=>• css_id_symbols _|[: 105;
css_id_symbols=>• css_id_symbols -|[: 105;
css_id_symbols=>• css_id_symbols $|[: 105;
css_id_symbols=>• css_id_symbols θnum|[: 105;
css_id_symbols=>• $|[;
css_id_symbols=>• θid|[;
css_id_symbols=>• θkey|[;
css_id_symbols=>• -|[;
css_id_symbols=>• css_id_symbols θid|$eof: 105;
css_id_symbols=>• css_id_symbols θkey|$eof: 105;
css_id_symbols=>• css_id_symbols _|$eof: 105;
css_id_symbols=>• css_id_symbols -|$eof: 105;
css_id_symbols=>• css_id_symbols $|$eof: 105;
css_id_symbols=>• css_id_symbols θnum|$eof: 105;
css_id_symbols=>• $|$eof;
css_id_symbols=>• θid|$eof;
css_id_symbols=>• θkey|$eof;
css_id_symbols=>• -|$eof;
css_id_symbols=>• css_id_symbols θid|{: 105;
css_id_symbols=>• css_id_symbols θkey|{: 105;
css_id_symbols=>• css_id_symbols _|{: 105;
css_id_symbols=>• css_id_symbols -|{: 105;
css_id_symbols=>• css_id_symbols $|{: 105;
css_id_symbols=>• css_id_symbols θnum|{: 105;
css_id_symbols=>• $|{;
css_id_symbols=>• θid|{;
css_id_symbols=>• θkey|{;
css_id_symbols=>• -|{;
css_id_symbols=>• css_id_symbols θid|+: 105;
css_id_symbols=>• css_id_symbols θkey|+: 105;
css_id_symbols=>• css_id_symbols _|+: 105;
css_id_symbols=>• css_id_symbols -|+: 105;
css_id_symbols=>• css_id_symbols $|+: 105;
css_id_symbols=>• css_id_symbols θnum|+: 105;
css_id_symbols=>• $|+;
css_id_symbols=>• θid|+;
css_id_symbols=>• θkey|+;
css_id_symbols=>• -|+;
css_id_symbols=>• css_id_symbols θid|~: 105;
css_id_symbols=>• css_id_symbols θkey|~: 105;
css_id_symbols=>• css_id_symbols _|~: 105;
css_id_symbols=>• css_id_symbols -|~: 105;
css_id_symbols=>• css_id_symbols $|~: 105;
css_id_symbols=>• css_id_symbols θnum|~: 105;
css_id_symbols=>• $|~;
css_id_symbols=>• θid|~;
css_id_symbols=>• θkey|~;
css_id_symbols=>• -|~;
css_id_symbols=>• css_id_symbols θid|||: 105;
css_id_symbols=>• css_id_symbols θkey|||: 105;
css_id_symbols=>• css_id_symbols _|||: 105;
css_id_symbols=>• css_id_symbols -|||: 105;
css_id_symbols=>• css_id_symbols $|||: 105;
css_id_symbols=>• css_id_symbols θnum|||: 105;
css_id_symbols=>• $|||;
css_id_symbols=>• θid|||;
css_id_symbols=>• θkey|||;
css_id_symbols=>• -|||;
css_id_symbols=>• css_id_symbols θid|i: 105;
css_id_symbols=>• css_id_symbols θkey|i: 105;
css_id_symbols=>• css_id_symbols _|i: 105;
css_id_symbols=>• css_id_symbols -|i: 105;
css_id_symbols=>• css_id_symbols $|i: 105;
css_id_symbols=>• css_id_symbols θnum|i: 105;
css_id_symbols=>• $|i;
css_id_symbols=>• θid|i;
css_id_symbols=>• θkey|i;
css_id_symbols=>• -|i;
css_id_symbols=>• css_id_symbols θid|s: 105;
css_id_symbols=>• css_id_symbols θkey|s: 105;
css_id_symbols=>• css_id_symbols _|s: 105;
css_id_symbols=>• css_id_symbols -|s: 105;
css_id_symbols=>• css_id_symbols $|s: 105;
css_id_symbols=>• css_id_symbols θnum|s: 105;
css_id_symbols=>• $|s;
css_id_symbols=>• θid|s;
css_id_symbols=>• θkey|s;
css_id_symbols=>• -|s;
css_id_symbols=>• css_id_symbols θid|]: 105;
css_id_symbols=>• css_id_symbols θkey|]: 105;
css_id_symbols=>• css_id_symbols _|]: 105;
css_id_symbols=>• css_id_symbols -|]: 105;
css_id_symbols=>• css_id_symbols $|]: 105;
css_id_symbols=>• css_id_symbols θnum|]: 105;
css_id_symbols=>• $|];
css_id_symbols=>• θid|];
css_id_symbols=>• θkey|];
css_id_symbols=>• -|];
css_id_symbols=>• css_id_symbols θid|^=: 105;
css_id_symbols=>• css_id_symbols θkey|^=: 105;
css_id_symbols=>• css_id_symbols _|^=: 105;
css_id_symbols=>• css_id_symbols -|^=: 105;
css_id_symbols=>• css_id_symbols $|^=: 105;
css_id_symbols=>• css_id_symbols θnum|^=: 105;
css_id_symbols=>• $|^=;
css_id_symbols=>• θid|^=;
css_id_symbols=>• θkey|^=;
css_id_symbols=>• -|^=;
css_id_symbols=>• css_id_symbols θid|$=: 105;
css_id_symbols=>• css_id_symbols θkey|$=: 105;
css_id_symbols=>• css_id_symbols _|$=: 105;
css_id_symbols=>• css_id_symbols -|$=: 105;
css_id_symbols=>• css_id_symbols $|$=: 105;
css_id_symbols=>• css_id_symbols θnum|$=: 105;
css_id_symbols=>• $|$=;
css_id_symbols=>• θid|$=;
css_id_symbols=>• θkey|$=;
css_id_symbols=>• -|$=;
css_id_symbols=>• css_id_symbols θid|*=: 105;
css_id_symbols=>• css_id_symbols θkey|*=: 105;
css_id_symbols=>• css_id_symbols _|*=: 105;
css_id_symbols=>• css_id_symbols -|*=: 105;
css_id_symbols=>• css_id_symbols $|*=: 105;
css_id_symbols=>• css_id_symbols θnum|*=: 105;
css_id_symbols=>• $|*=;
css_id_symbols=>• θid|*=;
css_id_symbols=>• θkey|*=;
css_id_symbols=>• -|*=
*/
    _skip(l, const_1_);
    if (idm204.has(l.id)) { idm204.get(l.id)(l); } else if (tym204.has(l.ty)) { tym204.get(l.ty)(l); }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 105: //css_id_symbols
                if (const_7_.includes(l.id) || l.ty == 0/* EOF */ || l.ty == 1/* \ws */) { return; }
                {
                    const cp = l.copy(), m = mark(), p = prod, s = stack_ptr;
                    State205(cp);
                    if (FAILED) {
                        prod = p;
                        FAILED = false;
                        stack_ptr = s;
                        reset(m);
                        return;
                    } else l.sync(cp);
                }
                break;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State205(l: Lexer): void {
    /*
css_id_symbols=>css_id_symbols • θid|ws;
css_id_symbols=>css_id_symbols • θkey|ws;
css_id_symbols=>css_id_symbols • _|ws;
css_id_symbols=>css_id_symbols • -|ws;
css_id_symbols=>css_id_symbols • $|ws;
css_id_symbols=>css_id_symbols • θnum|ws;
css_id_symbols=>css_id_symbols • θid|id;
css_id_symbols=>css_id_symbols • θkey|id;
css_id_symbols=>css_id_symbols • _|id;
css_id_symbols=>css_id_symbols • -|id;
css_id_symbols=>css_id_symbols • $|id;
css_id_symbols=>css_id_symbols • θnum|id;
css_id_symbols=>css_id_symbols • θid|key;
css_id_symbols=>css_id_symbols • θkey|key;
css_id_symbols=>css_id_symbols • _|key;
css_id_symbols=>css_id_symbols • -|key;
css_id_symbols=>css_id_symbols • $|key;
css_id_symbols=>css_id_symbols • θnum|key;
css_id_symbols=>css_id_symbols • θid|_;
css_id_symbols=>css_id_symbols • θkey|_;
css_id_symbols=>css_id_symbols • _|_;
css_id_symbols=>css_id_symbols • -|_;
css_id_symbols=>css_id_symbols • $|_;
css_id_symbols=>css_id_symbols • θnum|_;
css_id_symbols=>css_id_symbols • θid|-;
css_id_symbols=>css_id_symbols • θkey|-;
css_id_symbols=>css_id_symbols • _|-;
css_id_symbols=>css_id_symbols • -|-;
css_id_symbols=>css_id_symbols • $|-;
css_id_symbols=>css_id_symbols • θnum|-;
css_id_symbols=>css_id_symbols • θid|$;
css_id_symbols=>css_id_symbols • θkey|$;
css_id_symbols=>css_id_symbols • _|$;
css_id_symbols=>css_id_symbols • -|$;
css_id_symbols=>css_id_symbols • $|$;
css_id_symbols=>css_id_symbols • θnum|$;
css_id_symbols=>css_id_symbols • θid|num;
css_id_symbols=>css_id_symbols • θkey|num;
css_id_symbols=>css_id_symbols • _|num;
css_id_symbols=>css_id_symbols • -|num;
css_id_symbols=>css_id_symbols • $|num;
css_id_symbols=>css_id_symbols • θnum|num;
css_id_symbols=>css_id_symbols • θid|(;
css_id_symbols=>css_id_symbols • θkey|(;
css_id_symbols=>css_id_symbols • _|(;
css_id_symbols=>css_id_symbols • -|(;
css_id_symbols=>css_id_symbols • $|(;
css_id_symbols=>css_id_symbols • θnum|(;
css_id_symbols=>css_id_symbols • θid|>;
css_id_symbols=>css_id_symbols • θkey|>;
css_id_symbols=>css_id_symbols • _|>;
css_id_symbols=>css_id_symbols • -|>;
css_id_symbols=>css_id_symbols • $|>;
css_id_symbols=>css_id_symbols • θnum|>;
css_id_symbols=>css_id_symbols • θid|<;
css_id_symbols=>css_id_symbols • θkey|<;
css_id_symbols=>css_id_symbols • _|<;
css_id_symbols=>css_id_symbols • -|<;
css_id_symbols=>css_id_symbols • $|<;
css_id_symbols=>css_id_symbols • θnum|<;
css_id_symbols=>css_id_symbols • θid|:;
css_id_symbols=>css_id_symbols • θkey|:;
css_id_symbols=>css_id_symbols • _|:;
css_id_symbols=>css_id_symbols • -|:;
css_id_symbols=>css_id_symbols • $|:;
css_id_symbols=>css_id_symbols • θnum|:;
css_id_symbols=>css_id_symbols • θid|=;
css_id_symbols=>css_id_symbols • θkey|=;
css_id_symbols=>css_id_symbols • _|=;
css_id_symbols=>css_id_symbols • -|=;
css_id_symbols=>css_id_symbols • $|=;
css_id_symbols=>css_id_symbols • θnum|=;
css_id_symbols=>css_id_symbols • θid|);
css_id_symbols=>css_id_symbols • θkey|);
css_id_symbols=>css_id_symbols • _|);
css_id_symbols=>css_id_symbols • -|);
css_id_symbols=>css_id_symbols • $|);
css_id_symbols=>css_id_symbols • θnum|);
css_id_symbols=>css_id_symbols • θid|and;
css_id_symbols=>css_id_symbols • θkey|and;
css_id_symbols=>css_id_symbols • _|and;
css_id_symbols=>css_id_symbols • -|and;
css_id_symbols=>css_id_symbols • $|and;
css_id_symbols=>css_id_symbols • θnum|and;
css_id_symbols=>css_id_symbols • θid|,;
css_id_symbols=>css_id_symbols • θkey|,;
css_id_symbols=>css_id_symbols • _|,;
css_id_symbols=>css_id_symbols • -|,;
css_id_symbols=>css_id_symbols • $|,;
css_id_symbols=>css_id_symbols • θnum|,;
css_id_symbols=>css_id_symbols • θid|;;
css_id_symbols=>css_id_symbols • θkey|;;
css_id_symbols=>css_id_symbols • _|;;
css_id_symbols=>css_id_symbols • -|;;
css_id_symbols=>css_id_symbols • $|;;
css_id_symbols=>css_id_symbols • θnum|;;
css_id_symbols=>css_id_symbols • θid|@;
css_id_symbols=>css_id_symbols • θkey|@;
css_id_symbols=>css_id_symbols • _|@;
css_id_symbols=>css_id_symbols • -|@;
css_id_symbols=>css_id_symbols • $|@;
css_id_symbols=>css_id_symbols • θnum|@;
css_id_symbols=>css_id_symbols • θid|*;
css_id_symbols=>css_id_symbols • θkey|*;
css_id_symbols=>css_id_symbols • _|*;
css_id_symbols=>css_id_symbols • -|*;
css_id_symbols=>css_id_symbols • $|*;
css_id_symbols=>css_id_symbols • θnum|*;
css_id_symbols=>css_id_symbols • θid||;
css_id_symbols=>css_id_symbols • θkey||;
css_id_symbols=>css_id_symbols • _||;
css_id_symbols=>css_id_symbols • -||;
css_id_symbols=>css_id_symbols • $||;
css_id_symbols=>css_id_symbols • θnum||;
css_id_symbols=>css_id_symbols • θid|#;
css_id_symbols=>css_id_symbols • θkey|#;
css_id_symbols=>css_id_symbols • _|#;
css_id_symbols=>css_id_symbols • -|#;
css_id_symbols=>css_id_symbols • $|#;
css_id_symbols=>css_id_symbols • θnum|#;
css_id_symbols=>css_id_symbols • θid|.;
css_id_symbols=>css_id_symbols • θkey|.;
css_id_symbols=>css_id_symbols • _|.;
css_id_symbols=>css_id_symbols • -|.;
css_id_symbols=>css_id_symbols • $|.;
css_id_symbols=>css_id_symbols • θnum|.;
css_id_symbols=>css_id_symbols • θid|[;
css_id_symbols=>css_id_symbols • θkey|[;
css_id_symbols=>css_id_symbols • _|[;
css_id_symbols=>css_id_symbols • -|[;
css_id_symbols=>css_id_symbols • $|[;
css_id_symbols=>css_id_symbols • θnum|[;
css_id_symbols=>css_id_symbols • θid|$eof;
css_id_symbols=>css_id_symbols • θkey|$eof;
css_id_symbols=>css_id_symbols • _|$eof;
css_id_symbols=>css_id_symbols • -|$eof;
css_id_symbols=>css_id_symbols • $|$eof;
css_id_symbols=>css_id_symbols • θnum|$eof;
css_id_symbols=>css_id_symbols • θid|{;
css_id_symbols=>css_id_symbols • θkey|{;
css_id_symbols=>css_id_symbols • _|{;
css_id_symbols=>css_id_symbols • -|{;
css_id_symbols=>css_id_symbols • $|{;
css_id_symbols=>css_id_symbols • θnum|{;
css_id_symbols=>css_id_symbols • θid|+;
css_id_symbols=>css_id_symbols • θkey|+;
css_id_symbols=>css_id_symbols • _|+;
css_id_symbols=>css_id_symbols • -|+;
css_id_symbols=>css_id_symbols • $|+;
css_id_symbols=>css_id_symbols • θnum|+;
css_id_symbols=>css_id_symbols • θid|~;
css_id_symbols=>css_id_symbols • θkey|~;
css_id_symbols=>css_id_symbols • _|~;
css_id_symbols=>css_id_symbols • -|~;
css_id_symbols=>css_id_symbols • $|~;
css_id_symbols=>css_id_symbols • θnum|~;
css_id_symbols=>css_id_symbols • θid|||;
css_id_symbols=>css_id_symbols • θkey|||;
css_id_symbols=>css_id_symbols • _|||;
css_id_symbols=>css_id_symbols • -|||;
css_id_symbols=>css_id_symbols • $|||;
css_id_symbols=>css_id_symbols • θnum|||;
css_id_symbols=>css_id_symbols • θid|i;
css_id_symbols=>css_id_symbols • θkey|i;
css_id_symbols=>css_id_symbols • _|i;
css_id_symbols=>css_id_symbols • -|i;
css_id_symbols=>css_id_symbols • $|i;
css_id_symbols=>css_id_symbols • θnum|i;
css_id_symbols=>css_id_symbols • θid|s;
css_id_symbols=>css_id_symbols • θkey|s;
css_id_symbols=>css_id_symbols • _|s;
css_id_symbols=>css_id_symbols • -|s;
css_id_symbols=>css_id_symbols • $|s;
css_id_symbols=>css_id_symbols • θnum|s;
css_id_symbols=>css_id_symbols • θid|];
css_id_symbols=>css_id_symbols • θkey|];
css_id_symbols=>css_id_symbols • _|];
css_id_symbols=>css_id_symbols • -|];
css_id_symbols=>css_id_symbols • $|];
css_id_symbols=>css_id_symbols • θnum|];
css_id_symbols=>css_id_symbols • θid|^=;
css_id_symbols=>css_id_symbols • θkey|^=;
css_id_symbols=>css_id_symbols • _|^=;
css_id_symbols=>css_id_symbols • -|^=;
css_id_symbols=>css_id_symbols • $|^=;
css_id_symbols=>css_id_symbols • θnum|^=;
css_id_symbols=>css_id_symbols • θid|$=;
css_id_symbols=>css_id_symbols • θkey|$=;
css_id_symbols=>css_id_symbols • _|$=;
css_id_symbols=>css_id_symbols • -|$=;
css_id_symbols=>css_id_symbols • $|$=;
css_id_symbols=>css_id_symbols • θnum|$=;
css_id_symbols=>css_id_symbols • θid|*=;
css_id_symbols=>css_id_symbols • θkey|*=;
css_id_symbols=>css_id_symbols • _|*=;
css_id_symbols=>css_id_symbols • -|*=;
css_id_symbols=>css_id_symbols • $|*=;
css_id_symbols=>css_id_symbols • θnum|*=
*/
    _skip(l, const_1_);
    if (idm205.has(l.id)) { idm205.get(l.id)(l); } else if (tym205.has(l.ty)) { tym205.get(l.ty)(l); }
    else fail(l);
}
function $COMPLEX_SELECTOR_HC_listbody2_133(l: Lexer): void {
    /*
COMPLEX_SELECTOR_HC_listbody2_133=>• COMPLEX_SELECTOR_HC_listbody2_133 COMPLEX_SELECTOR_group_1119_132|>: 72;
COMPLEX_SELECTOR_HC_listbody2_133=>• COMPLEX_SELECTOR_group_1119_132|>: 72;
COMPLEX_SELECTOR_HC_listbody2_133=>• COMPLEX_SELECTOR_HC_listbody2_133 COMPLEX_SELECTOR_group_1119_132|+: 72;
COMPLEX_SELECTOR_HC_listbody2_133=>• COMPLEX_SELECTOR_group_1119_132|+: 72;
COMPLEX_SELECTOR_HC_listbody2_133=>• COMPLEX_SELECTOR_HC_listbody2_133 COMPLEX_SELECTOR_group_1119_132|~: 72;
COMPLEX_SELECTOR_HC_listbody2_133=>• COMPLEX_SELECTOR_group_1119_132|~: 72;
COMPLEX_SELECTOR_HC_listbody2_133=>• COMPLEX_SELECTOR_HC_listbody2_133 COMPLEX_SELECTOR_group_1119_132|||: 72;
COMPLEX_SELECTOR_HC_listbody2_133=>• COMPLEX_SELECTOR_group_1119_132|||: 72;
COMPLEX_SELECTOR_HC_listbody2_133=>• COMPLEX_SELECTOR_HC_listbody2_133 COMPLEX_SELECTOR_group_1119_132|$: 72;
COMPLEX_SELECTOR_HC_listbody2_133=>• COMPLEX_SELECTOR_group_1119_132|$: 72;
COMPLEX_SELECTOR_HC_listbody2_133=>• COMPLEX_SELECTOR_HC_listbody2_133 COMPLEX_SELECTOR_group_1119_132|id: 72;
COMPLEX_SELECTOR_HC_listbody2_133=>• COMPLEX_SELECTOR_group_1119_132|id: 72;
COMPLEX_SELECTOR_HC_listbody2_133=>• COMPLEX_SELECTOR_HC_listbody2_133 COMPLEX_SELECTOR_group_1119_132|key: 72;
COMPLEX_SELECTOR_HC_listbody2_133=>• COMPLEX_SELECTOR_group_1119_132|key: 72;
COMPLEX_SELECTOR_HC_listbody2_133=>• COMPLEX_SELECTOR_HC_listbody2_133 COMPLEX_SELECTOR_group_1119_132|-: 72;
COMPLEX_SELECTOR_HC_listbody2_133=>• COMPLEX_SELECTOR_group_1119_132|-: 72;
COMPLEX_SELECTOR_HC_listbody2_133=>• COMPLEX_SELECTOR_HC_listbody2_133 COMPLEX_SELECTOR_group_1119_132|*: 72;
COMPLEX_SELECTOR_HC_listbody2_133=>• COMPLEX_SELECTOR_group_1119_132|*: 72;
COMPLEX_SELECTOR_HC_listbody2_133=>• COMPLEX_SELECTOR_HC_listbody2_133 COMPLEX_SELECTOR_group_1119_132||: 72;
COMPLEX_SELECTOR_HC_listbody2_133=>• COMPLEX_SELECTOR_group_1119_132||: 72;
COMPLEX_SELECTOR_HC_listbody2_133=>• COMPLEX_SELECTOR_HC_listbody2_133 COMPLEX_SELECTOR_group_1119_132|#: 72;
COMPLEX_SELECTOR_HC_listbody2_133=>• COMPLEX_SELECTOR_group_1119_132|#: 72;
COMPLEX_SELECTOR_HC_listbody2_133=>• COMPLEX_SELECTOR_HC_listbody2_133 COMPLEX_SELECTOR_group_1119_132|.: 72;
COMPLEX_SELECTOR_HC_listbody2_133=>• COMPLEX_SELECTOR_group_1119_132|.: 72;
COMPLEX_SELECTOR_HC_listbody2_133=>• COMPLEX_SELECTOR_HC_listbody2_133 COMPLEX_SELECTOR_group_1119_132|[: 72;
COMPLEX_SELECTOR_HC_listbody2_133=>• COMPLEX_SELECTOR_group_1119_132|[: 72;
COMPLEX_SELECTOR_HC_listbody2_133=>• COMPLEX_SELECTOR_HC_listbody2_133 COMPLEX_SELECTOR_group_1119_132|:: 72;
COMPLEX_SELECTOR_HC_listbody2_133=>• COMPLEX_SELECTOR_group_1119_132|:: 72;
COMPLEX_SELECTOR_HC_listbody2_133=>• COMPLEX_SELECTOR_HC_listbody2_133 COMPLEX_SELECTOR_group_1119_132|): 72;
COMPLEX_SELECTOR_HC_listbody2_133=>• COMPLEX_SELECTOR_group_1119_132|): 72;
COMPLEX_SELECTOR_HC_listbody2_133=>• COMPLEX_SELECTOR_HC_listbody2_133 COMPLEX_SELECTOR_group_1119_132|,: 72;
COMPLEX_SELECTOR_HC_listbody2_133=>• COMPLEX_SELECTOR_group_1119_132|,: 72;
COMPLEX_SELECTOR_HC_listbody2_133=>• COMPLEX_SELECTOR_HC_listbody2_133 COMPLEX_SELECTOR_group_1119_132|{: 72;
COMPLEX_SELECTOR_HC_listbody2_133=>• COMPLEX_SELECTOR_group_1119_132|{: 72;
COMPLEX_SELECTOR_HC_listbody2_133=>• COMPLEX_SELECTOR_HC_listbody2_133 COMPLEX_SELECTOR_group_1119_132|$eof: 72;
COMPLEX_SELECTOR_HC_listbody2_133=>• COMPLEX_SELECTOR_group_1119_132|$eof: 72
*/
    _skip(l, const__);
    if (idm209.has(l.id)) { idm209.get(l.id)(l); } else if (l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        $COMPLEX_SELECTOR_group_1119_132(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 72: //COMPLEX_SELECTOR_HC_listbody2_133
                if (l.id == 10/* \, */ || l.id == 11/* \{ */ || l.id == 18/* \) */ || l.ty == 0/* EOF */) { return; }
                {
                    const cp = l.copy(), m = mark(), p = prod, s = stack_ptr;
                    State210(cp);
                    if (FAILED) {
                        prod = p;
                        FAILED = false;
                        stack_ptr = s;
                        reset(m);
                        return;
                    } else l.sync(cp);
                }
                break;
            case 71: //COMPLEX_SELECTOR_group_1119_132
                State211(l);
                break;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State210(l: Lexer): void {
    /*
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_HC_listbody2_133 • COMPLEX_SELECTOR_group_1119_132|>: 72;
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_HC_listbody2_133 • COMPLEX_SELECTOR_group_1119_132|+: 72;
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_HC_listbody2_133 • COMPLEX_SELECTOR_group_1119_132|~: 72;
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_HC_listbody2_133 • COMPLEX_SELECTOR_group_1119_132|||: 72;
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_HC_listbody2_133 • COMPLEX_SELECTOR_group_1119_132|$: 72;
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_HC_listbody2_133 • COMPLEX_SELECTOR_group_1119_132|id: 72;
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_HC_listbody2_133 • COMPLEX_SELECTOR_group_1119_132|key: 72;
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_HC_listbody2_133 • COMPLEX_SELECTOR_group_1119_132|-: 72;
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_HC_listbody2_133 • COMPLEX_SELECTOR_group_1119_132|*: 72;
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_HC_listbody2_133 • COMPLEX_SELECTOR_group_1119_132||: 72;
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_HC_listbody2_133 • COMPLEX_SELECTOR_group_1119_132|#: 72;
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_HC_listbody2_133 • COMPLEX_SELECTOR_group_1119_132|.: 72;
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_HC_listbody2_133 • COMPLEX_SELECTOR_group_1119_132|[: 72;
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_HC_listbody2_133 • COMPLEX_SELECTOR_group_1119_132|:: 72;
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_HC_listbody2_133 • COMPLEX_SELECTOR_group_1119_132|): 72;
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_HC_listbody2_133 • COMPLEX_SELECTOR_group_1119_132|,: 72;
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_HC_listbody2_133 • COMPLEX_SELECTOR_group_1119_132|{: 72;
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_HC_listbody2_133 • COMPLEX_SELECTOR_group_1119_132|$eof: 72
*/
    _skip(l, const__);
    if (idm209.has(l.id)) { idm209.get(l.id)(l); } else if (l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        $COMPLEX_SELECTOR_group_1119_132(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 71: //COMPLEX_SELECTOR_group_1119_132
                State390(l);
                break;
            case 72/*COMPLEX_SELECTOR_HC_listbody2_133*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State211(l: Lexer): void {
    /*
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_group_1119_132 •|>;
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_group_1119_132 •|+;
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_group_1119_132 •|~;
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_group_1119_132 •|||;
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_group_1119_132 •|$;
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_group_1119_132 •|id;
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_group_1119_132 •|key;
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_group_1119_132 •|-;
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_group_1119_132 •|*;
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_group_1119_132 •||;
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_group_1119_132 •|#;
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_group_1119_132 •|.;
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_group_1119_132 •|[;
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_group_1119_132 •|:;
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_group_1119_132 •|);
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_group_1119_132 •|,;
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_group_1119_132 •|{;
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_group_1119_132 •|$eof
*/
    _skip(l, const__);
    if (idm211r.has(l.id)) { idm211r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProduction(2, 1, 72); stack_ptr -= 1;

        ; return;
    }
    else fail(l);
}
function State249(l: Lexer): void {
    /*
STYLE_SHEET_HC_listbody1_100=>STYLE_SHEET_HC_listbody1_100 import •|@;
STYLE_SHEET_HC_listbody1_100=>STYLE_SHEET_HC_listbody1_100 import •|$;
STYLE_SHEET_HC_listbody1_100=>STYLE_SHEET_HC_listbody1_100 import •|id;
STYLE_SHEET_HC_listbody1_100=>STYLE_SHEET_HC_listbody1_100 import •|key;
STYLE_SHEET_HC_listbody1_100=>STYLE_SHEET_HC_listbody1_100 import •|-;
STYLE_SHEET_HC_listbody1_100=>STYLE_SHEET_HC_listbody1_100 import •|*;
STYLE_SHEET_HC_listbody1_100=>STYLE_SHEET_HC_listbody1_100 import •||;
STYLE_SHEET_HC_listbody1_100=>STYLE_SHEET_HC_listbody1_100 import •|#;
STYLE_SHEET_HC_listbody1_100=>STYLE_SHEET_HC_listbody1_100 import •|.;
STYLE_SHEET_HC_listbody1_100=>STYLE_SHEET_HC_listbody1_100 import •|[;
STYLE_SHEET_HC_listbody1_100=>STYLE_SHEET_HC_listbody1_100 import •|:;
STYLE_SHEET_HC_listbody1_100=>STYLE_SHEET_HC_listbody1_100 import •|$eof
*/
    _skip(l, const__);
    if (idm249r.has(l.id)) { idm249r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProduction(1, 2, 1); stack_ptr -= 2;

        ; return;
    }
    else fail(l);
}
function State251(l: Lexer): void {
    /*
keyframes_HC_listbody4_109=>keyframes_HC_listbody4_109 keyframes_blocks •|from;
keyframes_HC_listbody4_109=>keyframes_HC_listbody4_109 keyframes_blocks •|to;
keyframes_HC_listbody4_109=>keyframes_HC_listbody4_109 keyframes_blocks •|num;
keyframes_HC_listbody4_109=>keyframes_HC_listbody4_109 keyframes_blocks •|};
keyframes_HC_listbody4_109=>keyframes_HC_listbody4_109 keyframes_blocks •|$eof
*/
    _skip(l, const__);
    if (l.id == 13/* \} */ || l.id == 20/* \from */ || l.id == 21/* \to */) {

        completeProduction(1, 2, 16); stack_ptr -= 2;

        ; return;
    } else if (l.ty == 0/* EOF */ || l.ty == 2/* \num */) {

        completeProduction(1, 2, 16); stack_ptr -= 2;

        ; return;
    }
    else fail(l);
}
function State253(l: Lexer): void {
    /*
keyframes_blocks_HC_listbody1_110=>keyframes_blocks_HC_listbody1_110 , • keyframe_selector|{: 19;
keyframes_blocks_HC_listbody1_110=>keyframes_blocks_HC_listbody1_110 , • keyframe_selector|,: 19;
keyframes_blocks_HC_listbody1_110=>keyframes_blocks_HC_listbody1_110 , • keyframe_selector|$eof: 19
*/
    _skip(l, const__);
    if (l.id == 20/* \from */ || l.id == 21/* \to */) {

        $keyframe_selector(l); stack_ptr++;

    } else if (l.ty == 2/* \num */) {

        $keyframe_selector(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 21: //keyframe_selector
                State389(l);
                break;
            case 19/*keyframes_blocks_HC_listbody1_110*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State259(l: Lexer): void {
    /*
supports_condition_HC_listbody2_113=>supports_condition_HC_listbody2_113 supports_condition_group_129_112 •|and;
supports_condition_HC_listbody2_113=>supports_condition_HC_listbody2_113 supports_condition_group_129_112 •|or;
supports_condition_HC_listbody2_113=>supports_condition_HC_listbody2_113 supports_condition_group_129_112 •|);
supports_condition_HC_listbody2_113=>supports_condition_HC_listbody2_113 supports_condition_group_129_112 •|{;
supports_condition_HC_listbody2_113=>supports_condition_HC_listbody2_113 supports_condition_group_129_112 •|$eof
*/
    _skip(l, const__);
    if (l.id == 11/* \{ */ || l.id == 18/* \) */ || l.id == 23/* \and */ || l.id == 24/* \or */) {

        completeProduction(1, 2, 25); stack_ptr -= 2;

        ; return;
    } else if (l.ty == 0/* EOF */) {

        completeProduction(1, 2, 25); stack_ptr -= 2;

        ; return;
    }
    else fail(l);
}
function State269(l: Lexer): void {
    /*
css_id_symbols=>css_id_symbols θid •|ws;
css_id_symbols=>css_id_symbols θid •|id;
css_id_symbols=>css_id_symbols θid •|key;
css_id_symbols=>css_id_symbols θid •|_;
css_id_symbols=>css_id_symbols θid •|-;
css_id_symbols=>css_id_symbols θid •|$;
css_id_symbols=>css_id_symbols θid •|num;
css_id_symbols=>css_id_symbols θid •|(;
css_id_symbols=>css_id_symbols θid •|:;
css_id_symbols=>css_id_symbols θid •|and;
css_id_symbols=>css_id_symbols θid •|,;
css_id_symbols=>css_id_symbols θid •|$eof;
css_id_symbols=>css_id_symbols θid •|;;
css_id_symbols=>css_id_symbols θid •|@;
css_id_symbols=>css_id_symbols θid •|*;
css_id_symbols=>css_id_symbols θid •||;
css_id_symbols=>css_id_symbols θid •|#;
css_id_symbols=>css_id_symbols θid •|.;
css_id_symbols=>css_id_symbols θid •|[;
css_id_symbols=>css_id_symbols θid •|{;
css_id_symbols=>css_id_symbols θid •|<;
css_id_symbols=>css_id_symbols θid •|>;
css_id_symbols=>css_id_symbols θid •|=;
css_id_symbols=>css_id_symbols θid •|+;
css_id_symbols=>css_id_symbols θid •|~;
css_id_symbols=>css_id_symbols θid •|||;
css_id_symbols=>css_id_symbols θid •|);
css_id_symbols=>css_id_symbols θid •|];
css_id_symbols=>css_id_symbols θid •|^=;
css_id_symbols=>css_id_symbols θid •|$=;
css_id_symbols=>css_id_symbols θid •|*=;
css_id_symbols=>css_id_symbols θid •|i;
css_id_symbols=>css_id_symbols θid •|s
*/
    _skip(l, const_1_);
    if (idm269r.has(l.id)) { idm269r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 1/* \ws */ || l.ty == 2/* \num */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProduction(34, 2, 105); stack_ptr -= 2;

        ; return;
    }
    else fail(l);
}
function State270(l: Lexer): void {
    /*
css_id_symbols=>css_id_symbols θkey •|ws;
css_id_symbols=>css_id_symbols θkey •|id;
css_id_symbols=>css_id_symbols θkey •|key;
css_id_symbols=>css_id_symbols θkey •|_;
css_id_symbols=>css_id_symbols θkey •|-;
css_id_symbols=>css_id_symbols θkey •|$;
css_id_symbols=>css_id_symbols θkey •|num;
css_id_symbols=>css_id_symbols θkey •|(;
css_id_symbols=>css_id_symbols θkey •|:;
css_id_symbols=>css_id_symbols θkey •|and;
css_id_symbols=>css_id_symbols θkey •|,;
css_id_symbols=>css_id_symbols θkey •|$eof;
css_id_symbols=>css_id_symbols θkey •|;;
css_id_symbols=>css_id_symbols θkey •|@;
css_id_symbols=>css_id_symbols θkey •|*;
css_id_symbols=>css_id_symbols θkey •||;
css_id_symbols=>css_id_symbols θkey •|#;
css_id_symbols=>css_id_symbols θkey •|.;
css_id_symbols=>css_id_symbols θkey •|[;
css_id_symbols=>css_id_symbols θkey •|{;
css_id_symbols=>css_id_symbols θkey •|<;
css_id_symbols=>css_id_symbols θkey •|>;
css_id_symbols=>css_id_symbols θkey •|=;
css_id_symbols=>css_id_symbols θkey •|+;
css_id_symbols=>css_id_symbols θkey •|~;
css_id_symbols=>css_id_symbols θkey •|||;
css_id_symbols=>css_id_symbols θkey •|);
css_id_symbols=>css_id_symbols θkey •|];
css_id_symbols=>css_id_symbols θkey •|^=;
css_id_symbols=>css_id_symbols θkey •|$=;
css_id_symbols=>css_id_symbols θkey •|*=;
css_id_symbols=>css_id_symbols θkey •|i;
css_id_symbols=>css_id_symbols θkey •|s
*/
    _skip(l, const_1_);
    if (idm269r.has(l.id)) { idm269r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 1/* \ws */ || l.ty == 2/* \num */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProduction(34, 2, 105); stack_ptr -= 2;

        ; return;
    }
    else fail(l);
}
function State271(l: Lexer): void {
    /*
css_id_symbols=>css_id_symbols _ •|ws;
css_id_symbols=>css_id_symbols _ •|id;
css_id_symbols=>css_id_symbols _ •|key;
css_id_symbols=>css_id_symbols _ •|_;
css_id_symbols=>css_id_symbols _ •|-;
css_id_symbols=>css_id_symbols _ •|$;
css_id_symbols=>css_id_symbols _ •|num;
css_id_symbols=>css_id_symbols _ •|(;
css_id_symbols=>css_id_symbols _ •|:;
css_id_symbols=>css_id_symbols _ •|and;
css_id_symbols=>css_id_symbols _ •|,;
css_id_symbols=>css_id_symbols _ •|$eof;
css_id_symbols=>css_id_symbols _ •|;;
css_id_symbols=>css_id_symbols _ •|@;
css_id_symbols=>css_id_symbols _ •|*;
css_id_symbols=>css_id_symbols _ •||;
css_id_symbols=>css_id_symbols _ •|#;
css_id_symbols=>css_id_symbols _ •|.;
css_id_symbols=>css_id_symbols _ •|[;
css_id_symbols=>css_id_symbols _ •|{;
css_id_symbols=>css_id_symbols _ •|<;
css_id_symbols=>css_id_symbols _ •|>;
css_id_symbols=>css_id_symbols _ •|=;
css_id_symbols=>css_id_symbols _ •|+;
css_id_symbols=>css_id_symbols _ •|~;
css_id_symbols=>css_id_symbols _ •|||;
css_id_symbols=>css_id_symbols _ •|);
css_id_symbols=>css_id_symbols _ •|];
css_id_symbols=>css_id_symbols _ •|^=;
css_id_symbols=>css_id_symbols _ •|$=;
css_id_symbols=>css_id_symbols _ •|*=;
css_id_symbols=>css_id_symbols _ •|i;
css_id_symbols=>css_id_symbols _ •|s
*/
    _skip(l, const_1_);
    if (idm269r.has(l.id)) { idm269r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 1/* \ws */ || l.ty == 2/* \num */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProduction(34, 2, 105); stack_ptr -= 2;

        ; return;
    }
    else fail(l);
}
function State272(l: Lexer): void {
    /*
css_id_symbols=>css_id_symbols - •|ws;
css_id_symbols=>css_id_symbols - •|id;
css_id_symbols=>css_id_symbols - •|key;
css_id_symbols=>css_id_symbols - •|_;
css_id_symbols=>css_id_symbols - •|-;
css_id_symbols=>css_id_symbols - •|$;
css_id_symbols=>css_id_symbols - •|num;
css_id_symbols=>css_id_symbols - •|(;
css_id_symbols=>css_id_symbols - •|:;
css_id_symbols=>css_id_symbols - •|and;
css_id_symbols=>css_id_symbols - •|,;
css_id_symbols=>css_id_symbols - •|$eof;
css_id_symbols=>css_id_symbols - •|;;
css_id_symbols=>css_id_symbols - •|@;
css_id_symbols=>css_id_symbols - •|*;
css_id_symbols=>css_id_symbols - •||;
css_id_symbols=>css_id_symbols - •|#;
css_id_symbols=>css_id_symbols - •|.;
css_id_symbols=>css_id_symbols - •|[;
css_id_symbols=>css_id_symbols - •|{;
css_id_symbols=>css_id_symbols - •|<;
css_id_symbols=>css_id_symbols - •|>;
css_id_symbols=>css_id_symbols - •|=;
css_id_symbols=>css_id_symbols - •|+;
css_id_symbols=>css_id_symbols - •|~;
css_id_symbols=>css_id_symbols - •|||;
css_id_symbols=>css_id_symbols - •|);
css_id_symbols=>css_id_symbols - •|];
css_id_symbols=>css_id_symbols - •|^=;
css_id_symbols=>css_id_symbols - •|$=;
css_id_symbols=>css_id_symbols - •|*=;
css_id_symbols=>css_id_symbols - •|i;
css_id_symbols=>css_id_symbols - •|s
*/
    _skip(l, const_1_);
    if (idm269r.has(l.id)) { idm269r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 1/* \ws */ || l.ty == 2/* \num */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProduction(34, 2, 105); stack_ptr -= 2;

        ; return;
    }
    else fail(l);
}
function State273(l: Lexer): void {
    /*
css_id_symbols=>css_id_symbols $ •|ws;
css_id_symbols=>css_id_symbols $ •|id;
css_id_symbols=>css_id_symbols $ •|key;
css_id_symbols=>css_id_symbols $ •|_;
css_id_symbols=>css_id_symbols $ •|-;
css_id_symbols=>css_id_symbols $ •|$;
css_id_symbols=>css_id_symbols $ •|num;
css_id_symbols=>css_id_symbols $ •|(;
css_id_symbols=>css_id_symbols $ •|:;
css_id_symbols=>css_id_symbols $ •|and;
css_id_symbols=>css_id_symbols $ •|,;
css_id_symbols=>css_id_symbols $ •|$eof;
css_id_symbols=>css_id_symbols $ •|;;
css_id_symbols=>css_id_symbols $ •|@;
css_id_symbols=>css_id_symbols $ •|*;
css_id_symbols=>css_id_symbols $ •||;
css_id_symbols=>css_id_symbols $ •|#;
css_id_symbols=>css_id_symbols $ •|.;
css_id_symbols=>css_id_symbols $ •|[;
css_id_symbols=>css_id_symbols $ •|{;
css_id_symbols=>css_id_symbols $ •|<;
css_id_symbols=>css_id_symbols $ •|>;
css_id_symbols=>css_id_symbols $ •|=;
css_id_symbols=>css_id_symbols $ •|+;
css_id_symbols=>css_id_symbols $ •|~;
css_id_symbols=>css_id_symbols $ •|||;
css_id_symbols=>css_id_symbols $ •|);
css_id_symbols=>css_id_symbols $ •|];
css_id_symbols=>css_id_symbols $ •|^=;
css_id_symbols=>css_id_symbols $ •|$=;
css_id_symbols=>css_id_symbols $ •|*=;
css_id_symbols=>css_id_symbols $ •|i;
css_id_symbols=>css_id_symbols $ •|s
*/
    _skip(l, const_1_);
    if (idm269r.has(l.id)) { idm269r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 1/* \ws */ || l.ty == 2/* \num */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProduction(34, 2, 105); stack_ptr -= 2;

        ; return;
    }
    else fail(l);
}
function State274(l: Lexer): void {
    /*
css_id_symbols=>css_id_symbols θnum •|ws;
css_id_symbols=>css_id_symbols θnum •|id;
css_id_symbols=>css_id_symbols θnum •|key;
css_id_symbols=>css_id_symbols θnum •|_;
css_id_symbols=>css_id_symbols θnum •|-;
css_id_symbols=>css_id_symbols θnum •|$;
css_id_symbols=>css_id_symbols θnum •|num;
css_id_symbols=>css_id_symbols θnum •|(;
css_id_symbols=>css_id_symbols θnum •|:;
css_id_symbols=>css_id_symbols θnum •|and;
css_id_symbols=>css_id_symbols θnum •|,;
css_id_symbols=>css_id_symbols θnum •|$eof;
css_id_symbols=>css_id_symbols θnum •|;;
css_id_symbols=>css_id_symbols θnum •|@;
css_id_symbols=>css_id_symbols θnum •|*;
css_id_symbols=>css_id_symbols θnum •||;
css_id_symbols=>css_id_symbols θnum •|#;
css_id_symbols=>css_id_symbols θnum •|.;
css_id_symbols=>css_id_symbols θnum •|[;
css_id_symbols=>css_id_symbols θnum •|{;
css_id_symbols=>css_id_symbols θnum •|<;
css_id_symbols=>css_id_symbols θnum •|>;
css_id_symbols=>css_id_symbols θnum •|=;
css_id_symbols=>css_id_symbols θnum •|+;
css_id_symbols=>css_id_symbols θnum •|~;
css_id_symbols=>css_id_symbols θnum •|||;
css_id_symbols=>css_id_symbols θnum •|);
css_id_symbols=>css_id_symbols θnum •|];
css_id_symbols=>css_id_symbols θnum •|^=;
css_id_symbols=>css_id_symbols θnum •|$=;
css_id_symbols=>css_id_symbols θnum •|*=;
css_id_symbols=>css_id_symbols θnum •|i;
css_id_symbols=>css_id_symbols θnum •|s
*/
    _skip(l, const_1_);
    if (idm269r.has(l.id)) { idm269r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 1/* \ws */ || l.ty == 2/* \num */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProduction(34, 2, 105); stack_ptr -= 2;

        ; return;
    }
    else fail(l);
}
function State282(l: Lexer): void {
    /*
media_query=>media_type media_query_group_144_117 •|,;
media_query=>media_type media_query_group_144_117 •|$eof;
media_query=>media_type media_query_group_144_117 •|;;
media_query=>media_type media_query_group_144_117 •|@;
media_query=>media_type media_query_group_144_117 •|$;
media_query=>media_type media_query_group_144_117 •|id;
media_query=>media_type media_query_group_144_117 •|key;
media_query=>media_type media_query_group_144_117 •|-;
media_query=>media_type media_query_group_144_117 •|*;
media_query=>media_type media_query_group_144_117 •||;
media_query=>media_type media_query_group_144_117 •|#;
media_query=>media_type media_query_group_144_117 •|.;
media_query=>media_type media_query_group_144_117 •|[;
media_query=>media_type media_query_group_144_117 •|:;
media_query=>media_type media_query_group_144_117 •|{
*/
    _skip(l, const__);
    if (idm282r.has(l.id)) { idm282r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProduction(30, 2, 37); stack_ptr -= 2;

        ; return;
    }
    else fail(l);
}
function State284(l: Lexer): void {
    /*
media_and=>media_in_parenths media_and_HC_listbody2_119 •|,;
media_and=>media_in_parenths media_and_HC_listbody2_119 •|$eof;
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 • media_and_group_152_118|,: 42;
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 • media_and_group_152_118|and: 42;
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 • media_and_group_152_118|$eof: 42;
media_and=>media_in_parenths media_and_HC_listbody2_119 •|;;
media_and=>media_in_parenths media_and_HC_listbody2_119 •|@;
media_and=>media_in_parenths media_and_HC_listbody2_119 •|$;
media_and=>media_in_parenths media_and_HC_listbody2_119 •|id;
media_and=>media_in_parenths media_and_HC_listbody2_119 •|key;
media_and=>media_in_parenths media_and_HC_listbody2_119 •|-;
media_and=>media_in_parenths media_and_HC_listbody2_119 •|*;
media_and=>media_in_parenths media_and_HC_listbody2_119 •||;
media_and=>media_in_parenths media_and_HC_listbody2_119 •|#;
media_and=>media_in_parenths media_and_HC_listbody2_119 •|.;
media_and=>media_in_parenths media_and_HC_listbody2_119 •|[;
media_and=>media_in_parenths media_and_HC_listbody2_119 •|:;
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 • media_and_group_152_118|;: 42;
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 • media_and_group_152_118|@: 42;
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 • media_and_group_152_118|$: 42;
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 • media_and_group_152_118|id: 42;
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 • media_and_group_152_118|key: 42;
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 • media_and_group_152_118|-: 42;
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 • media_and_group_152_118|*: 42;
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 • media_and_group_152_118||: 42;
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 • media_and_group_152_118|#: 42;
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 • media_and_group_152_118|.: 42;
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 • media_and_group_152_118|[: 42;
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 • media_and_group_152_118|:: 42;
media_and=>media_in_parenths media_and_HC_listbody2_119 •|{;
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 • media_and_group_152_118|{: 42;
media_and=>media_in_parenths media_and_HC_listbody2_119 •|);
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 • media_and_group_152_118|): 42
*/
    _skip(l, const__);
    if (l.id == 23/* \and */) {

        $media_and_group_152_118(l); stack_ptr++;

    } else if (idm284r.has(l.id)) { idm284r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProduction(22, 2, 43); stack_ptr -= 2;

        ; return;
    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 41: //media_and_group_152_118
                State301(l);
                break;
            case 43/*media_and*/:
            case 42/*media_and_HC_listbody2_119*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State292(l: Lexer): void {
    /*
media_query=>media_query_group_043_116 media_type • media_query_group_144_117|,: 37;
media_query=>media_query_group_043_116 media_type •|,;
media_query=>media_query_group_043_116 media_type • media_query_group_144_117|$eof: 37;
media_query=>media_query_group_043_116 media_type •|$eof;
media_query=>media_query_group_043_116 media_type • media_query_group_144_117|{: 37;
media_query=>media_query_group_043_116 media_type •|{;
media_query=>media_query_group_043_116 media_type • media_query_group_144_117|;: 37;
media_query=>media_query_group_043_116 media_type •|;;
media_query=>media_query_group_043_116 media_type • media_query_group_144_117|@: 37;
media_query=>media_query_group_043_116 media_type •|@;
media_query=>media_query_group_043_116 media_type • media_query_group_144_117|$: 37;
media_query=>media_query_group_043_116 media_type •|$;
media_query=>media_query_group_043_116 media_type • media_query_group_144_117|id: 37;
media_query=>media_query_group_043_116 media_type •|id;
media_query=>media_query_group_043_116 media_type • media_query_group_144_117|key: 37;
media_query=>media_query_group_043_116 media_type •|key;
media_query=>media_query_group_043_116 media_type • media_query_group_144_117|-: 37;
media_query=>media_query_group_043_116 media_type •|-;
media_query=>media_query_group_043_116 media_type • media_query_group_144_117|*: 37;
media_query=>media_query_group_043_116 media_type •|*;
media_query=>media_query_group_043_116 media_type • media_query_group_144_117||: 37;
media_query=>media_query_group_043_116 media_type •||;
media_query=>media_query_group_043_116 media_type • media_query_group_144_117|#: 37;
media_query=>media_query_group_043_116 media_type •|#;
media_query=>media_query_group_043_116 media_type • media_query_group_144_117|.: 37;
media_query=>media_query_group_043_116 media_type •|.;
media_query=>media_query_group_043_116 media_type • media_query_group_144_117|[: 37;
media_query=>media_query_group_043_116 media_type •|[;
media_query=>media_query_group_043_116 media_type • media_query_group_144_117|:: 37;
media_query=>media_query_group_043_116 media_type •|:
*/
    _skip(l, const__);
    if (l.id == 23/* \and */) {

        $media_query_group_144_117(l); stack_ptr++;

    } else if (idm292r.has(l.id)) { idm292r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProduction(31, 2, 37); stack_ptr -= 2;

        ; return;
    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 36: //media_query_group_144_117
                State407(l);
                break;
            case 37/*media_query*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State294(l: Lexer): void {
    /*
import_HC_listbody4_108=>import_HC_listbody4_108 , • media_query|,: 13;
import_HC_listbody4_108=>import_HC_listbody4_108 , • media_query|;: 13;
import_HC_listbody4_108=>import_HC_listbody4_108 , • media_query|@: 13;
import_HC_listbody4_108=>import_HC_listbody4_108 , • media_query|$: 13;
import_HC_listbody4_108=>import_HC_listbody4_108 , • media_query|id: 13;
import_HC_listbody4_108=>import_HC_listbody4_108 , • media_query|key: 13;
import_HC_listbody4_108=>import_HC_listbody4_108 , • media_query|-: 13;
import_HC_listbody4_108=>import_HC_listbody4_108 , • media_query|*: 13;
import_HC_listbody4_108=>import_HC_listbody4_108 , • media_query||: 13;
import_HC_listbody4_108=>import_HC_listbody4_108 , • media_query|#: 13;
import_HC_listbody4_108=>import_HC_listbody4_108 , • media_query|.: 13;
import_HC_listbody4_108=>import_HC_listbody4_108 , • media_query|[: 13;
import_HC_listbody4_108=>import_HC_listbody4_108 , • media_query|:: 13;
import_HC_listbody4_108=>import_HC_listbody4_108 , • media_query|$eof: 13
*/
    _skip(l, const__);
    if (l.id == 17/* \( */ || l.id == 22/* \not */ || l.id == 26/* \only */ || l.id == 55/* \- */ || l.id == 56/* \$ */) {

        $media_query(l); stack_ptr++;

    } else if (l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        $media_query(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 37: //media_query
                State410(l);
                break;
            case 13/*import_HC_listbody4_108*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State299(l: Lexer): void {
    /*
media_queries_group_039_115=>media_queries_group_039_115 , • media_query|,: 33;
media_queries_group_039_115=>media_queries_group_039_115 , • media_query|{: 33;
media_queries_group_039_115=>media_queries_group_039_115 , • media_query|$eof: 33
*/
    _skip(l, const__);
    if (l.id == 17/* \( */ || l.id == 22/* \not */ || l.id == 26/* \only */ || l.id == 55/* \- */ || l.id == 56/* \$ */) {

        $media_query(l); stack_ptr++;

    } else if (l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        $media_query(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 37: //media_query
                State411(l);
                break;
            case 33/*media_queries_group_039_115*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State300(l: Lexer): void {
    /*
media_or_HC_listbody2_121=>media_or_HC_listbody2_121 media_or_group_154_120 •|or;
media_or_HC_listbody2_121=>media_or_HC_listbody2_121 media_or_group_154_120 •|);
media_or_HC_listbody2_121=>media_or_HC_listbody2_121 media_or_group_154_120 •|,;
media_or_HC_listbody2_121=>media_or_HC_listbody2_121 media_or_group_154_120 •|;;
media_or_HC_listbody2_121=>media_or_HC_listbody2_121 media_or_group_154_120 •|@;
media_or_HC_listbody2_121=>media_or_HC_listbody2_121 media_or_group_154_120 •|$;
media_or_HC_listbody2_121=>media_or_HC_listbody2_121 media_or_group_154_120 •|id;
media_or_HC_listbody2_121=>media_or_HC_listbody2_121 media_or_group_154_120 •|key;
media_or_HC_listbody2_121=>media_or_HC_listbody2_121 media_or_group_154_120 •|-;
media_or_HC_listbody2_121=>media_or_HC_listbody2_121 media_or_group_154_120 •|*;
media_or_HC_listbody2_121=>media_or_HC_listbody2_121 media_or_group_154_120 •||;
media_or_HC_listbody2_121=>media_or_HC_listbody2_121 media_or_group_154_120 •|#;
media_or_HC_listbody2_121=>media_or_HC_listbody2_121 media_or_group_154_120 •|.;
media_or_HC_listbody2_121=>media_or_HC_listbody2_121 media_or_group_154_120 •|[;
media_or_HC_listbody2_121=>media_or_HC_listbody2_121 media_or_group_154_120 •|:;
media_or_HC_listbody2_121=>media_or_HC_listbody2_121 media_or_group_154_120 •|$eof;
media_or_HC_listbody2_121=>media_or_HC_listbody2_121 media_or_group_154_120 •|{
*/
    _skip(l, const__);
    if (idm300r.has(l.id)) { idm300r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProduction(1, 2, 45); stack_ptr -= 2;

        ; return;
    }
    else fail(l);
}
function State301(l: Lexer): void {
    /*
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 media_and_group_152_118 •|and;
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 media_and_group_152_118 •|,;
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 media_and_group_152_118 •|;;
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 media_and_group_152_118 •|@;
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 media_and_group_152_118 •|$;
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 media_and_group_152_118 •|id;
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 media_and_group_152_118 •|key;
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 media_and_group_152_118 •|-;
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 media_and_group_152_118 •|*;
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 media_and_group_152_118 •||;
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 media_and_group_152_118 •|#;
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 media_and_group_152_118 •|.;
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 media_and_group_152_118 •|[;
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 media_and_group_152_118 •|:;
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 media_and_group_152_118 •|$eof;
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 media_and_group_152_118 •|{;
media_and_HC_listbody2_119=>media_and_HC_listbody2_119 media_and_group_152_118 •|)
*/
    _skip(l, const__);
    if (idm301r.has(l.id)) { idm301r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProduction(1, 2, 42); stack_ptr -= 2;

        ; return;
    }
    else fail(l);
}
function State303(l: Lexer): void {
    /*
mf_range=>mf_name mf_range_group_074_125 • mf_value|): 57;
mf_range=>mf_name mf_range_group_074_125 • mf_value|$eof: 57
*/
    _skip(l, const__);
    if (l.id == 55/* \- */ || l.id == 56/* \$ */) {

        $mf_value(l); stack_ptr++;

    } else if (l.ty == 2/* \num */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        $mf_value(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 58: //mf_value
                State417(l);
                break;
            case 57/*mf_range*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State308(l: Lexer): void {
    /*
mf_range=>mf_value mf_range_group_074_125 • mf_name|): 57;
mf_range=>mf_value mf_range_group_074_125 • mf_name|$eof: 57
*/
    _skip(l, const__);
    if (l.id == 55/* \- */ || l.id == 56/* \$ */) {

        $mf_name(l); stack_ptr++;

    } else if (l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        $mf_name(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 60: //mf_name
                State419(l);
                break;
            case 57/*mf_range*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State309(l: Lexer): void {
    /*
mf_range=>mf_value mf_range_group_183_126 • identifier mf_range_group_183_126 mf_value|): 57;
mf_range=>mf_value mf_range_group_183_126 • identifier mf_range_group_183_126 mf_value|$eof: 57
*/
    _skip(l, const__);
    if (l.id == 55/* \- */ || l.id == 56/* \$ */) {

        $identifier(l); stack_ptr++;

    } else if (l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        $identifier(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 104: //identifier
                State423(l);
                break;
            case 57/*mf_range*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State310(l: Lexer): void {
    /*
mf_range=>mf_value mf_range_group_188_127 • identifier mf_range_group_188_127 mf_value|): 57;
mf_range=>mf_value mf_range_group_188_127 • identifier mf_range_group_188_127 mf_value|$eof: 57
*/
    _skip(l, const__);
    if (l.id == 55/* \- */ || l.id == 56/* \$ */) {

        $identifier(l); stack_ptr++;

    } else if (l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        $identifier(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 104: //identifier
                State421(l);
                break;
            case 57/*mf_range*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State315(l: Lexer): void {
    /*
STYLE_RULE_HC_listbody2_103=>STYLE_RULE_HC_listbody2_103 , • COMPLEX_SELECTOR|,: 5;
STYLE_RULE_HC_listbody2_103=>STYLE_RULE_HC_listbody2_103 , • COMPLEX_SELECTOR|{: 5;
STYLE_RULE_HC_listbody2_103=>STYLE_RULE_HC_listbody2_103 , • COMPLEX_SELECTOR|$eof: 5
*/
    _skip(l, const__);
    if (idm315.has(l.id)) { idm315.get(l.id)(l); } else if (l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        $COMPLEX_SELECTOR(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 73: //COMPLEX_SELECTOR
                State456(l);
                break;
            case 5/*STYLE_RULE_HC_listbody2_103*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State318(l: Lexer): void {
    /*
COMPOUND_SELECTOR_HC_listbody1_134=>COMPOUND_SELECTOR_HC_listbody1_134 SUBCLASS_SELECTOR •|:;
COMPOUND_SELECTOR_HC_listbody1_134=>COMPOUND_SELECTOR_HC_listbody1_134 SUBCLASS_SELECTOR •|#;
COMPOUND_SELECTOR_HC_listbody1_134=>COMPOUND_SELECTOR_HC_listbody1_134 SUBCLASS_SELECTOR •|.;
COMPOUND_SELECTOR_HC_listbody1_134=>COMPOUND_SELECTOR_HC_listbody1_134 SUBCLASS_SELECTOR •|[;
COMPOUND_SELECTOR_HC_listbody1_134=>COMPOUND_SELECTOR_HC_listbody1_134 SUBCLASS_SELECTOR •|ws;
COMPOUND_SELECTOR_HC_listbody1_134=>COMPOUND_SELECTOR_HC_listbody1_134 SUBCLASS_SELECTOR •|>;
COMPOUND_SELECTOR_HC_listbody1_134=>COMPOUND_SELECTOR_HC_listbody1_134 SUBCLASS_SELECTOR •|+;
COMPOUND_SELECTOR_HC_listbody1_134=>COMPOUND_SELECTOR_HC_listbody1_134 SUBCLASS_SELECTOR •|~;
COMPOUND_SELECTOR_HC_listbody1_134=>COMPOUND_SELECTOR_HC_listbody1_134 SUBCLASS_SELECTOR •|||;
COMPOUND_SELECTOR_HC_listbody1_134=>COMPOUND_SELECTOR_HC_listbody1_134 SUBCLASS_SELECTOR •|$;
COMPOUND_SELECTOR_HC_listbody1_134=>COMPOUND_SELECTOR_HC_listbody1_134 SUBCLASS_SELECTOR •|id;
COMPOUND_SELECTOR_HC_listbody1_134=>COMPOUND_SELECTOR_HC_listbody1_134 SUBCLASS_SELECTOR •|key;
COMPOUND_SELECTOR_HC_listbody1_134=>COMPOUND_SELECTOR_HC_listbody1_134 SUBCLASS_SELECTOR •|-;
COMPOUND_SELECTOR_HC_listbody1_134=>COMPOUND_SELECTOR_HC_listbody1_134 SUBCLASS_SELECTOR •|*;
COMPOUND_SELECTOR_HC_listbody1_134=>COMPOUND_SELECTOR_HC_listbody1_134 SUBCLASS_SELECTOR •||;
COMPOUND_SELECTOR_HC_listbody1_134=>COMPOUND_SELECTOR_HC_listbody1_134 SUBCLASS_SELECTOR •|,;
COMPOUND_SELECTOR_HC_listbody1_134=>COMPOUND_SELECTOR_HC_listbody1_134 SUBCLASS_SELECTOR •|{;
COMPOUND_SELECTOR_HC_listbody1_134=>COMPOUND_SELECTOR_HC_listbody1_134 SUBCLASS_SELECTOR •|$eof;
COMPOUND_SELECTOR_HC_listbody1_134=>COMPOUND_SELECTOR_HC_listbody1_134 SUBCLASS_SELECTOR •|)
*/
    _skip(l, const_1_);
    if (idm318r.has(l.id)) { idm318r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 1/* \ws */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProduction(1, 2, 74); stack_ptr -= 2;

        ; return;
    }
    else fail(l);
}
function State320(l: Lexer): void {
    /*
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_HC_listbody2_137 COMPOUND_SELECTOR_group_1121_136 •|ws;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_HC_listbody2_137 COMPOUND_SELECTOR_group_1121_136 •|:;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_HC_listbody2_137 COMPOUND_SELECTOR_group_1121_136 •|>;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_HC_listbody2_137 COMPOUND_SELECTOR_group_1121_136 •|+;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_HC_listbody2_137 COMPOUND_SELECTOR_group_1121_136 •|~;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_HC_listbody2_137 COMPOUND_SELECTOR_group_1121_136 •|||;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_HC_listbody2_137 COMPOUND_SELECTOR_group_1121_136 •|$;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_HC_listbody2_137 COMPOUND_SELECTOR_group_1121_136 •|id;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_HC_listbody2_137 COMPOUND_SELECTOR_group_1121_136 •|key;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_HC_listbody2_137 COMPOUND_SELECTOR_group_1121_136 •|-;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_HC_listbody2_137 COMPOUND_SELECTOR_group_1121_136 •|*;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_HC_listbody2_137 COMPOUND_SELECTOR_group_1121_136 •||;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_HC_listbody2_137 COMPOUND_SELECTOR_group_1121_136 •|#;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_HC_listbody2_137 COMPOUND_SELECTOR_group_1121_136 •|.;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_HC_listbody2_137 COMPOUND_SELECTOR_group_1121_136 •|[;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_HC_listbody2_137 COMPOUND_SELECTOR_group_1121_136 •|,;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_HC_listbody2_137 COMPOUND_SELECTOR_group_1121_136 •|{;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_HC_listbody2_137 COMPOUND_SELECTOR_group_1121_136 •|$eof;
COMPOUND_SELECTOR_HC_listbody2_137=>COMPOUND_SELECTOR_HC_listbody2_137 COMPOUND_SELECTOR_group_1121_136 •|)
*/
    _skip(l, const_1_);
    if (idm320r.has(l.id)) { idm320r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 1/* \ws */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProduction(1, 2, 77); stack_ptr -= 2;

        ; return;
    }
    else fail(l);
}
function State321(l: Lexer): void {
    /*
NS_PREFIX=>NS_PREFIX_group_0150_140 | •|$;
NS_PREFIX=>NS_PREFIX_group_0150_140 | •|id;
NS_PREFIX=>NS_PREFIX_group_0150_140 | •|key;
NS_PREFIX=>NS_PREFIX_group_0150_140 | •|-;
NS_PREFIX=>NS_PREFIX_group_0150_140 | •|*
*/
    _skip(l, const__);
    if (l.id == 50/* \* */ || l.id == 55/* \- */ || l.id == 56/* \$ */) {

        completeProduction(75, 2, 92); stack_ptr -= 2;

        ; return;
    } else if (l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProduction(75, 2, 92); stack_ptr -= 2;

        ; return;
    }
    else fail(l);
}
function State327(l: Lexer): void {
    /*
TYPE_SELECTOR=>NS_PREFIX * •|#;
TYPE_SELECTOR=>NS_PREFIX * •|.;
TYPE_SELECTOR=>NS_PREFIX * •|[;
TYPE_SELECTOR=>NS_PREFIX * •|:;
TYPE_SELECTOR=>NS_PREFIX * •|ws;
TYPE_SELECTOR=>NS_PREFIX * •|>;
TYPE_SELECTOR=>NS_PREFIX * •|+;
TYPE_SELECTOR=>NS_PREFIX * •|~;
TYPE_SELECTOR=>NS_PREFIX * •|||;
TYPE_SELECTOR=>NS_PREFIX * •|$;
TYPE_SELECTOR=>NS_PREFIX * •|id;
TYPE_SELECTOR=>NS_PREFIX * •|key;
TYPE_SELECTOR=>NS_PREFIX * •|-;
TYPE_SELECTOR=>NS_PREFIX * •|*;
TYPE_SELECTOR=>NS_PREFIX * •||;
TYPE_SELECTOR=>NS_PREFIX * •|,;
TYPE_SELECTOR=>NS_PREFIX * •|{;
TYPE_SELECTOR=>NS_PREFIX * •|$eof;
TYPE_SELECTOR=>NS_PREFIX * •|)
*/
    _skip(l, const_1_);
    if (idm327r.has(l.id)) { idm327r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 1/* \ws */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProduction(72, 2, 89); stack_ptr -= 2;

        ; return;
    }
    else fail(l);
}
function State328(l: Lexer): void {
    /*
WQ_NAME=>NS_PREFIX identifier •|#;
WQ_NAME=>NS_PREFIX identifier •|.;
WQ_NAME=>NS_PREFIX identifier •|[;
WQ_NAME=>NS_PREFIX identifier •|:;
WQ_NAME=>NS_PREFIX identifier •|ws;
WQ_NAME=>NS_PREFIX identifier •|>;
WQ_NAME=>NS_PREFIX identifier •|+;
WQ_NAME=>NS_PREFIX identifier •|~;
WQ_NAME=>NS_PREFIX identifier •|||;
WQ_NAME=>NS_PREFIX identifier •|$;
WQ_NAME=>NS_PREFIX identifier •|id;
WQ_NAME=>NS_PREFIX identifier •|key;
WQ_NAME=>NS_PREFIX identifier •|-;
WQ_NAME=>NS_PREFIX identifier •|*;
WQ_NAME=>NS_PREFIX identifier •||;
WQ_NAME=>NS_PREFIX identifier •|,;
WQ_NAME=>NS_PREFIX identifier •|{;
WQ_NAME=>NS_PREFIX identifier •|$eof;
WQ_NAME=>NS_PREFIX identifier •|);
WQ_NAME=>NS_PREFIX identifier •|];
WQ_NAME=>NS_PREFIX identifier •|^=;
WQ_NAME=>NS_PREFIX identifier •|$=;
WQ_NAME=>NS_PREFIX identifier •|*=;
WQ_NAME=>NS_PREFIX identifier •|=
*/
    _skip(l, const_1_);
    if (idm328r.has(l.id)) { idm328r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 1/* \ws */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProduction(77, 2, 93); stack_ptr -= 2;

        ; return;
    }
    else fail(l);
}
function State330(l: Lexer): void {
    /*
general_enclosed_HC_listbody1_124=>general_enclosed_HC_listbody1_124 general_enclosed_group_067_123 •|tok;
general_enclosed_HC_listbody1_124=>general_enclosed_HC_listbody1_124 general_enclosed_group_067_123 •|sym;
general_enclosed_HC_listbody1_124=>general_enclosed_HC_listbody1_124 general_enclosed_group_067_123 •|id;
general_enclosed_HC_listbody1_124=>general_enclosed_HC_listbody1_124 general_enclosed_group_067_123 •|key;
general_enclosed_HC_listbody1_124=>general_enclosed_HC_listbody1_124 general_enclosed_group_067_123 •|ws;
general_enclosed_HC_listbody1_124=>general_enclosed_HC_listbody1_124 general_enclosed_group_067_123 •|);
general_enclosed_HC_listbody1_124=>general_enclosed_HC_listbody1_124 general_enclosed_group_067_123 •|$eof
*/
    _skip(l, const_1_);
    if (l.id == 18/* \) */) {

        completeProduction(34, 2, 51); stack_ptr -= 2;

        ; return;
    } else if (l.ty == 0/* EOF */ || l.ty == 1/* \ws */ || l.ty == 3/* \id */ || l.ty == 5/* \tok */ || l.ty == 6/* \sym */ || l.ty == 7/* \key */) {

        completeProduction(34, 2, 51); stack_ptr -= 2;

        ; return;
    }
    else fail(l);
}
function State332(l: Lexer): void {
    /*
string_HC_listbody1_129=>string_HC_listbody1_129 string_group_0109_128 •|tok;
string_HC_listbody1_129=>string_HC_listbody1_129 string_group_0109_128 •|key;
string_HC_listbody1_129=>string_HC_listbody1_129 string_group_0109_128 •|id;
string_HC_listbody1_129=>string_HC_listbody1_129 string_group_0109_128 •|sym;
string_HC_listbody1_129=>string_HC_listbody1_129 string_group_0109_128 •|num;
string_HC_listbody1_129=>string_HC_listbody1_129 string_group_0109_128 •|ws;
string_HC_listbody1_129=>string_HC_listbody1_129 string_group_0109_128 •|";
string_HC_listbody1_129=>string_HC_listbody1_129 string_group_0109_128 •|$eof
*/
    _skip(l, const_1_);
    if (l.id == 27/* \" */) {

        completeProduction(34, 2, 67); stack_ptr -= 2;

        ; return;
    } else if (l.ty == 0/* EOF */ || l.ty == 1/* \ws */ || l.ty == 2/* \num */ || l.ty == 3/* \id */ || l.ty == 5/* \tok */ || l.ty == 6/* \sym */ || l.ty == 7/* \key */) {

        completeProduction(34, 2, 67); stack_ptr -= 2;

        ; return;
    }
    else fail(l);
}
function State334(l: Lexer): void {
    /*
string_HC_listbody1_130=>string_HC_listbody1_130 string_group_0109_128 •|tok;
string_HC_listbody1_130=>string_HC_listbody1_130 string_group_0109_128 •|key;
string_HC_listbody1_130=>string_HC_listbody1_130 string_group_0109_128 •|id;
string_HC_listbody1_130=>string_HC_listbody1_130 string_group_0109_128 •|sym;
string_HC_listbody1_130=>string_HC_listbody1_130 string_group_0109_128 •|num;
string_HC_listbody1_130=>string_HC_listbody1_130 string_group_0109_128 •|ws;
string_HC_listbody1_130=>string_HC_listbody1_130 string_group_0109_128 •|';
string_HC_listbody1_130=>string_HC_listbody1_130 string_group_0109_128 •|$eof
*/
    _skip(l, const_1_);
    if (l.id == 37/* \' */) {

        completeProduction(34, 2, 68); stack_ptr -= 2;

        ; return;
    } else if (l.ty == 0/* EOF */ || l.ty == 1/* \ws */ || l.ty == 2/* \num */ || l.ty == 3/* \id */ || l.ty == 5/* \tok */ || l.ty == 6/* \sym */ || l.ty == 7/* \key */) {

        completeProduction(34, 2, 68); stack_ptr -= 2;

        ; return;
    }
    else fail(l);
}
function State337(l: Lexer): void {
    /*
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • declaration_list ; }|$: 6;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • ; }|$;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • declaration_list }|$: 6;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • }|$;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • declaration_list ; }|id: 6;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • ; }|id;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • declaration_list }|id: 6;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • }|id;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • declaration_list ; }|key: 6;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • ; }|key;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • declaration_list }|key: 6;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • }|key;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • declaration_list ; }|-: 6;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • ; }|-;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • declaration_list }|-: 6;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • }|-;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • declaration_list ; }|*: 6;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • ; }|*;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • declaration_list }|*: 6;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • }|*;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • declaration_list ; }||: 6;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • ; }||;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • declaration_list }||: 6;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • }||;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • declaration_list ; }|#: 6;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • ; }|#;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • declaration_list }|#: 6;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • }|#;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • declaration_list ; }|.: 6;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • ; }|.;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • declaration_list }|.: 6;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • }|.;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • declaration_list ; }|[: 6;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • ; }|[;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • declaration_list }|[: 6;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • }|[;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • declaration_list ; }|:: 6;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • ; }|:;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • declaration_list }|:: 6;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • }|:;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • declaration_list ; }|$eof: 6;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • ; }|$eof;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • declaration_list }|$eof: 6;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • }|$eof;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • declaration_list ; }|@: 6;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • ; }|@;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • declaration_list }|@: 6;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • }|@;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • declaration_list ; }|}: 6;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • ; }|};
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • declaration_list }|}: 6;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { • }|}
*/
    _skip(l, const__);
    if (idm337.has(l.id)) { idm337.get(l.id)(l); } else if (l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        $declaration_list(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 98: //declaration_list
                State436(l);
                break;
            case 6/*STYLE_RULE*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State339(l: Lexer): void {
    /*
COMPOUND_SELECTOR_HC_listbody1_135=>COMPOUND_SELECTOR_HC_listbody1_135 PSEUDO_CLASS_SELECTOR •|:;
COMPOUND_SELECTOR_HC_listbody1_135=>COMPOUND_SELECTOR_HC_listbody1_135 PSEUDO_CLASS_SELECTOR •|ws;
COMPOUND_SELECTOR_HC_listbody1_135=>COMPOUND_SELECTOR_HC_listbody1_135 PSEUDO_CLASS_SELECTOR •|>;
COMPOUND_SELECTOR_HC_listbody1_135=>COMPOUND_SELECTOR_HC_listbody1_135 PSEUDO_CLASS_SELECTOR •|+;
COMPOUND_SELECTOR_HC_listbody1_135=>COMPOUND_SELECTOR_HC_listbody1_135 PSEUDO_CLASS_SELECTOR •|~;
COMPOUND_SELECTOR_HC_listbody1_135=>COMPOUND_SELECTOR_HC_listbody1_135 PSEUDO_CLASS_SELECTOR •|||;
COMPOUND_SELECTOR_HC_listbody1_135=>COMPOUND_SELECTOR_HC_listbody1_135 PSEUDO_CLASS_SELECTOR •|$;
COMPOUND_SELECTOR_HC_listbody1_135=>COMPOUND_SELECTOR_HC_listbody1_135 PSEUDO_CLASS_SELECTOR •|id;
COMPOUND_SELECTOR_HC_listbody1_135=>COMPOUND_SELECTOR_HC_listbody1_135 PSEUDO_CLASS_SELECTOR •|key;
COMPOUND_SELECTOR_HC_listbody1_135=>COMPOUND_SELECTOR_HC_listbody1_135 PSEUDO_CLASS_SELECTOR •|-;
COMPOUND_SELECTOR_HC_listbody1_135=>COMPOUND_SELECTOR_HC_listbody1_135 PSEUDO_CLASS_SELECTOR •|*;
COMPOUND_SELECTOR_HC_listbody1_135=>COMPOUND_SELECTOR_HC_listbody1_135 PSEUDO_CLASS_SELECTOR •||;
COMPOUND_SELECTOR_HC_listbody1_135=>COMPOUND_SELECTOR_HC_listbody1_135 PSEUDO_CLASS_SELECTOR •|#;
COMPOUND_SELECTOR_HC_listbody1_135=>COMPOUND_SELECTOR_HC_listbody1_135 PSEUDO_CLASS_SELECTOR •|.;
COMPOUND_SELECTOR_HC_listbody1_135=>COMPOUND_SELECTOR_HC_listbody1_135 PSEUDO_CLASS_SELECTOR •|[;
COMPOUND_SELECTOR_HC_listbody1_135=>COMPOUND_SELECTOR_HC_listbody1_135 PSEUDO_CLASS_SELECTOR •|);
COMPOUND_SELECTOR_HC_listbody1_135=>COMPOUND_SELECTOR_HC_listbody1_135 PSEUDO_CLASS_SELECTOR •|,;
COMPOUND_SELECTOR_HC_listbody1_135=>COMPOUND_SELECTOR_HC_listbody1_135 PSEUDO_CLASS_SELECTOR •|{;
COMPOUND_SELECTOR_HC_listbody1_135=>COMPOUND_SELECTOR_HC_listbody1_135 PSEUDO_CLASS_SELECTOR •|$eof
*/
    _skip(l, const_1_);
    if (idm339r.has(l.id)) { idm339r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 1/* \ws */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProduction(1, 2, 75); stack_ptr -= 2;

        ; return;
    }
    else fail(l);
}
function $STYLE_SHEET(l: Lexer): void {
    /*
STYLE_SHEET=>• STYLE_SHEET_HC_listbody1_100 STYLE_SHEET_HC_listbody1_102|$eof: 4;
STYLE_SHEET=>• STYLE_SHEET_HC_listbody1_102|$eof: 4;
STYLE_SHEET=>• STYLE_SHEET_HC_listbody1_100|$eof: 4;
STYLE_SHEET=>•|$eof
*/
    _skip(l, const__);
    if (idm340.has(l.id)) { idm340.get(l.id)(l); } else if (l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        $STYLE_SHEET_HC_listbody1_102(l); stack_ptr++;

    } else if (l.ty == 0/* EOF */) {

        completeProduction(6, 0, 4); stack_ptr -= 0;

        ; return;
    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 3: //STYLE_SHEET_HC_listbody1_102
                State342(l);
                break;
            case 1: //STYLE_SHEET_HC_listbody1_100
                State341(l);
                break;
            case 4/*STYLE_SHEET*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State341(l: Lexer): void {
    /*
STYLE_SHEET=>STYLE_SHEET_HC_listbody1_100 • STYLE_SHEET_HC_listbody1_102|$eof: 4;
STYLE_SHEET=>STYLE_SHEET_HC_listbody1_100 •|$eof;
STYLE_SHEET_HC_listbody1_100=>STYLE_SHEET_HC_listbody1_100 • import|$: 1;
STYLE_SHEET_HC_listbody1_100=>STYLE_SHEET_HC_listbody1_100 • import|id: 1;
STYLE_SHEET_HC_listbody1_100=>STYLE_SHEET_HC_listbody1_100 • import|key: 1;
STYLE_SHEET_HC_listbody1_100=>STYLE_SHEET_HC_listbody1_100 • import|-: 1;
STYLE_SHEET_HC_listbody1_100=>STYLE_SHEET_HC_listbody1_100 • import|*: 1;
STYLE_SHEET_HC_listbody1_100=>STYLE_SHEET_HC_listbody1_100 • import||: 1;
STYLE_SHEET_HC_listbody1_100=>STYLE_SHEET_HC_listbody1_100 • import|#: 1;
STYLE_SHEET_HC_listbody1_100=>STYLE_SHEET_HC_listbody1_100 • import|.: 1;
STYLE_SHEET_HC_listbody1_100=>STYLE_SHEET_HC_listbody1_100 • import|[: 1;
STYLE_SHEET_HC_listbody1_100=>STYLE_SHEET_HC_listbody1_100 • import|:: 1;
STYLE_SHEET_HC_listbody1_100=>STYLE_SHEET_HC_listbody1_100 • import|@: 1;
STYLE_SHEET_HC_listbody1_100=>STYLE_SHEET_HC_listbody1_100 • import|$eof: 1
*/
    _skip(l, const__);
    if (idm341.has(l.id)) { idm341.get(l.id)(l); } else if (l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        $STYLE_SHEET_HC_listbody1_102(l); stack_ptr++;

    } else if (l.ty == 0/* EOF */) {

        completeProduction(5, 1, 4); stack_ptr -= 1;

        ; return;
    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 14: //import
                State481(l);
                break;
            case 9: //AT_RULE
                State147(l);
                break;
            case 3: //STYLE_SHEET_HC_listbody1_102
                State480(l);
                break;
            case 2: //STYLE_SHEET_group_03_101
                State145(l);
                break;
            case 4/*STYLE_SHEET*/:
            case 1/*STYLE_SHEET_HC_listbody1_100*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State342(l: Lexer): void {
    /*
STYLE_SHEET=>STYLE_SHEET_HC_listbody1_102 •|$eof;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_HC_listbody1_102 • STYLE_SHEET_group_03_101|$eof: 3;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_HC_listbody1_102 • STYLE_SHEET_group_03_101|$: 3;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_HC_listbody1_102 • STYLE_SHEET_group_03_101|id: 3;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_HC_listbody1_102 • STYLE_SHEET_group_03_101|key: 3;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_HC_listbody1_102 • STYLE_SHEET_group_03_101|-: 3;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_HC_listbody1_102 • STYLE_SHEET_group_03_101|*: 3;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_HC_listbody1_102 • STYLE_SHEET_group_03_101||: 3;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_HC_listbody1_102 • STYLE_SHEET_group_03_101|#: 3;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_HC_listbody1_102 • STYLE_SHEET_group_03_101|.: 3;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_HC_listbody1_102 • STYLE_SHEET_group_03_101|[: 3;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_HC_listbody1_102 • STYLE_SHEET_group_03_101|:: 3;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_HC_listbody1_102 • STYLE_SHEET_group_03_101|@: 3
*/
    _skip(l, const__);
    if (idm342.has(l.id)) { idm342.get(l.id)(l); } else if (l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        $STYLE_SHEET_group_03_101(l); stack_ptr++;

    } else if (l.ty == 0/* EOF */) {

        completeProduction(4, 1, 4); stack_ptr -= 1;

        ; return;
    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 2: //STYLE_SHEET_group_03_101
                State368(l);
                break;
            case 4/*STYLE_SHEET*/:
            case 3/*STYLE_SHEET_HC_listbody1_102*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State345(l: Lexer): void {
    /*
AT_RULE=>import ; •|$;
AT_RULE=>import ; •|id;
AT_RULE=>import ; •|key;
AT_RULE=>import ; •|-;
AT_RULE=>import ; •|*;
AT_RULE=>import ; •||;
AT_RULE=>import ; •|#;
AT_RULE=>import ; •|.;
AT_RULE=>import ; •|[;
AT_RULE=>import ; •|:;
AT_RULE=>import ; •|@;
AT_RULE=>import ; •|$eof
*/
    _skip(l, const__);
    if (idm345r.has(l.id)) { idm345r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProductionPlain(2, 9); stack_ptr -= 2;

        ; return;
    }
    else fail(l);
}
function State351(l: Lexer): void {
    /*
declaration_list_HC_listbody2_141=>declaration_list_HC_listbody2_141 ; •|;;
declaration_list_HC_listbody2_141=>declaration_list_HC_listbody2_141 ; •|$;
declaration_list_HC_listbody2_141=>declaration_list_HC_listbody2_141 ; •|id;
declaration_list_HC_listbody2_141=>declaration_list_HC_listbody2_141 ; •|key;
declaration_list_HC_listbody2_141=>declaration_list_HC_listbody2_141 ; •|-;
declaration_list_HC_listbody2_141=>declaration_list_HC_listbody2_141 ; •|$eof
*/
    _skip(l, const__);
    if (l.id == 12/* \; */ || l.id == 55/* \- */ || l.id == 56/* \$ */) {

        completeProduction(1, 2, 94); stack_ptr -= 2;

        ; return;
    } else if (l.ty == 0/* EOF */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProduction(1, 2, 94); stack_ptr -= 2;

        ; return;
    }
    else fail(l);
}
function $media_condition_without_or(l: Lexer): void {
    /*
media_condition_without_or=>• media_not|,: 39;
media_condition_without_or=>• media_and|,: 39;
media_condition_without_or=>• media_in_parenths|,: 39;
media_condition_without_or=>• media_not|;: 39;
media_condition_without_or=>• media_and|;: 39;
media_condition_without_or=>• media_in_parenths|;: 39;
media_condition_without_or=>• media_not|@: 39;
media_condition_without_or=>• media_and|@: 39;
media_condition_without_or=>• media_in_parenths|@: 39;
media_condition_without_or=>• media_not|$: 39;
media_condition_without_or=>• media_and|$: 39;
media_condition_without_or=>• media_in_parenths|$: 39;
media_condition_without_or=>• media_not|id: 39;
media_condition_without_or=>• media_and|id: 39;
media_condition_without_or=>• media_in_parenths|id: 39;
media_condition_without_or=>• media_not|key: 39;
media_condition_without_or=>• media_and|key: 39;
media_condition_without_or=>• media_in_parenths|key: 39;
media_condition_without_or=>• media_not|-: 39;
media_condition_without_or=>• media_and|-: 39;
media_condition_without_or=>• media_in_parenths|-: 39;
media_condition_without_or=>• media_not|*: 39;
media_condition_without_or=>• media_and|*: 39;
media_condition_without_or=>• media_in_parenths|*: 39;
media_condition_without_or=>• media_not||: 39;
media_condition_without_or=>• media_and||: 39;
media_condition_without_or=>• media_in_parenths||: 39;
media_condition_without_or=>• media_not|#: 39;
media_condition_without_or=>• media_and|#: 39;
media_condition_without_or=>• media_in_parenths|#: 39;
media_condition_without_or=>• media_not|.: 39;
media_condition_without_or=>• media_and|.: 39;
media_condition_without_or=>• media_in_parenths|.: 39;
media_condition_without_or=>• media_not|[: 39;
media_condition_without_or=>• media_and|[: 39;
media_condition_without_or=>• media_in_parenths|[: 39;
media_condition_without_or=>• media_not|:: 39;
media_condition_without_or=>• media_and|:: 39;
media_condition_without_or=>• media_in_parenths|:: 39;
media_condition_without_or=>• media_not|$eof: 39;
media_condition_without_or=>• media_and|$eof: 39;
media_condition_without_or=>• media_in_parenths|$eof: 39;
media_condition_without_or=>• media_not|{: 39;
media_condition_without_or=>• media_and|{: 39;
media_condition_without_or=>• media_in_parenths|{: 39;
media_condition_without_or=>• media_not|): 39;
media_condition_without_or=>• media_and|): 39;
media_condition_without_or=>• media_in_parenths|): 39
*/
    _skip(l, const__);
    if (idm352.has(l.id)) { idm352.get(l.id)(l); } else if (l.ty == 3/* \id */ || l.ty == 7/* \key */) {
        let $mark = mark(), sp = stack_ptr, cp = l.copy();
        $media_in_parenths(cp); stack_ptr++;
        if (FAILED) {
            reset($mark); FAILED = false; stack_ptr = sp;
            $media_and(l); stack_ptr++;;
        } else l.sync(cp);

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 47: //media_in_parenths
                State353(l);
                break;
            case 43: //media_and
                State48(l);
                break;
            case 40: //media_not
                State47(l);
                break;
            case 39/*media_condition_without_or*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State353(l: Lexer): void {
    /*
media_condition_without_or=>media_in_parenths •|,;
media_condition_without_or=>media_in_parenths •|;;
media_condition_without_or=>media_in_parenths •|@;
media_condition_without_or=>media_in_parenths •|$;
media_condition_without_or=>media_in_parenths •|id;
media_condition_without_or=>media_in_parenths •|key;
media_condition_without_or=>media_in_parenths •|-;
media_condition_without_or=>media_in_parenths •|*;
media_condition_without_or=>media_in_parenths •||;
media_condition_without_or=>media_in_parenths •|#;
media_condition_without_or=>media_in_parenths •|.;
media_condition_without_or=>media_in_parenths •|[;
media_condition_without_or=>media_in_parenths •|:;
media_condition_without_or=>media_in_parenths •|$eof;
media_condition_without_or=>media_in_parenths •|{;
media_condition_without_or=>media_in_parenths •|);
media_and=>media_in_parenths • media_and_HC_listbody2_119|,: 43;
media_and=>media_in_parenths • media_and_HC_listbody2_119|;: 43;
media_and=>media_in_parenths • media_and_HC_listbody2_119|@: 43;
media_and=>media_in_parenths • media_and_HC_listbody2_119|$: 43;
media_and=>media_in_parenths • media_and_HC_listbody2_119|id: 43;
media_and=>media_in_parenths • media_and_HC_listbody2_119|key: 43;
media_and=>media_in_parenths • media_and_HC_listbody2_119|-: 43;
media_and=>media_in_parenths • media_and_HC_listbody2_119|*: 43;
media_and=>media_in_parenths • media_and_HC_listbody2_119||: 43;
media_and=>media_in_parenths • media_and_HC_listbody2_119|#: 43;
media_and=>media_in_parenths • media_and_HC_listbody2_119|.: 43;
media_and=>media_in_parenths • media_and_HC_listbody2_119|[: 43;
media_and=>media_in_parenths • media_and_HC_listbody2_119|:: 43;
media_and=>media_in_parenths • media_and_HC_listbody2_119|$eof: 43;
media_and=>media_in_parenths • media_and_HC_listbody2_119|{: 43;
media_and=>media_in_parenths • media_and_HC_listbody2_119|): 43
*/
    _skip(l, const__);
    if (l.id == 23/* \and */) {

        $media_and_HC_listbody2_119(l); stack_ptr++;

    } else if (idm353r.has(l.id)) { idm353r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProductionPlain(1, 39); stack_ptr -= 1;

        ; return;
    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 42: //media_and_HC_listbody2_119
                State284(l);
                break;
            case 39/*media_condition_without_or*/:
            case 43/*media_and*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State354(l: Lexer): void {
    /*
declaration_list_HC_listbody1_144=>declaration_list_HC_listbody1_144 ; •|;;
declaration_list_HC_listbody1_144=>declaration_list_HC_listbody1_144 ; •|};
declaration_list_HC_listbody1_144=>declaration_list_HC_listbody1_144 ; •|$eof
*/
    _skip(l, const__);
    if (l.id == 12/* \; */ || l.id == 13/* \} */) {

        completeProduction(1, 2, 97); stack_ptr -= 2;

        ; return;
    } else if (l.ty == 0/* EOF */) {

        completeProduction(1, 2, 97); stack_ptr -= 2;

        ; return;
    }
    else fail(l);
}
function State356(l: Lexer): void {
    /*
declaration_value=>declaration_value string_group_0109_128 •|tok;
declaration_value=>declaration_value string_group_0109_128 •|key;
declaration_value=>declaration_value string_group_0109_128 •|id;
declaration_value=>declaration_value string_group_0109_128 •|sym;
declaration_value=>declaration_value string_group_0109_128 •|num;
declaration_value=>declaration_value string_group_0109_128 •|ws;
declaration_value=>declaration_value string_group_0109_128 •|!;
declaration_value=>declaration_value string_group_0109_128 •|(;
declaration_value=>declaration_value string_group_0109_128 •|);
declaration_value=>declaration_value string_group_0109_128 •|;;
declaration_value=>declaration_value string_group_0109_128 •|};
declaration_value=>declaration_value string_group_0109_128 •|$eof
*/
    _skip(l, const_1_);
    if (l.id == 12/* \; */ || l.id == 13/* \} */ || l.id == 17/* \( */ || l.id == 18/* \) */ || l.id == 52/* \! */) {

        completeProduction(34, 2, 103); stack_ptr -= 2;

        ; return;
    } else if (l.ty == 0/* EOF */ || l.ty == 1/* \ws */ || l.ty == 2/* \num */ || l.ty == 3/* \id */ || l.ty == 5/* \tok */ || l.ty == 6/* \sym */ || l.ty == 7/* \key */) {

        completeProduction(34, 2, 103); stack_ptr -= 2;

        ; return;
    }
    else fail(l);
}
function State357(l: Lexer): void {
    /*
declaration_list_HC_listbody2_143=>declaration_list_HC_listbody2_143 ; • declaration_list_group_1153_142|;: 96;
declaration_list_HC_listbody2_143=>declaration_list_HC_listbody2_143 ; • declaration_list_group_1153_142|}: 96;
declaration_list_HC_listbody2_143=>declaration_list_HC_listbody2_143 ; • declaration_list_group_1153_142|$eof: 96
*/
    _skip(l, const__);
    if (l.id == 12/* \; */ || l.id == 55/* \- */ || l.id == 56/* \$ */) {

        $declaration_list_group_1153_142(l); stack_ptr++;

    } else if (l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        $declaration_list_group_1153_142(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 95: //declaration_list_group_1153_142
                State447(l);
                break;
            case 96/*declaration_list_HC_listbody2_143*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State359(l: Lexer): void {
    /*
declaration_values=>declaration_values ( • declaration_values )|!: 101;
declaration_values=>declaration_values ( • declaration_values )|(: 101;
declaration_values=>declaration_values ( • declaration_values )|): 101;
declaration_values=>declaration_values ( • declaration_values )|tok: 101;
declaration_values=>declaration_values ( • declaration_values )|key: 101;
declaration_values=>declaration_values ( • declaration_values )|id: 101;
declaration_values=>declaration_values ( • declaration_values )|sym: 101;
declaration_values=>declaration_values ( • declaration_values )|num: 101;
declaration_values=>declaration_values ( • declaration_values )|ws: 101;
declaration_values=>declaration_values ( • declaration_values )|;: 101;
declaration_values=>declaration_values ( • declaration_values )|}: 101;
declaration_values=>declaration_values ( • declaration_values )|$eof: 101
*/
    _skip(l, const_1_);
    if (l.ty == 1/* \ws */ || l.ty == 2/* \num */ || l.ty == 3/* \id */ || l.ty == 5/* \tok */ || l.ty == 6/* \sym */ || l.ty == 7/* \key */) {

        $declaration_values(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 101: //declaration_values
                if (l.id == 12/* \; */ || l.id == 13/* \} */ || l.id == 52/* \! */ || l.ty == 0/* EOF */) { return; }
                State448(l);
                break;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State360(l: Lexer): void {
    /*
declaration_values=>declaration_values declaration_value •|!;
declaration_values=>declaration_values declaration_value •|(;
declaration_values=>declaration_values declaration_value •|);
declaration_values=>declaration_values declaration_value •|tok;
declaration_values=>declaration_values declaration_value •|key;
declaration_values=>declaration_values declaration_value •|id;
declaration_values=>declaration_values declaration_value •|sym;
declaration_values=>declaration_values declaration_value •|num;
declaration_values=>declaration_values declaration_value •|ws;
declaration_values=>declaration_values declaration_value •|;;
declaration_values=>declaration_values declaration_value •|};
declaration_values=>declaration_values declaration_value •|$eof;
declaration_value=>declaration_value • string_group_0109_128|!: 103;
declaration_value=>declaration_value • string_group_0109_128|tok: 103;
declaration_value=>declaration_value • string_group_0109_128|key: 103;
declaration_value=>declaration_value • string_group_0109_128|id: 103;
declaration_value=>declaration_value • string_group_0109_128|sym: 103;
declaration_value=>declaration_value • string_group_0109_128|num: 103;
declaration_value=>declaration_value • string_group_0109_128|ws: 103;
declaration_value=>declaration_value • string_group_0109_128|(: 103;
declaration_value=>declaration_value • string_group_0109_128|): 103;
declaration_value=>declaration_value • string_group_0109_128|;: 103;
declaration_value=>declaration_value • string_group_0109_128|}: 103;
declaration_value=>declaration_value • string_group_0109_128|$eof: 103
*/
    _skip(l, const_1_);
    if (l.id == 12/* \; */ || l.id == 13/* \} */ || l.id == 17/* \( */ || l.id == 18/* \) */ || l.id == 52/* \! */) {

        completeProduction(34, 2, 101); stack_ptr -= 2;

        ; return;
    } else if (l.ty == 1/* \ws */ || l.ty == 2/* \num */ || l.ty == 3/* \id */ || l.ty == 5/* \tok */ || l.ty == 6/* \sym */ || l.ty == 7/* \key */) {

        $string_group_0109_128(l); stack_ptr++;

    } else if (l.ty == 0/* EOF */ || l.ty == 1/* \ws */ || l.ty == 2/* \num */ || l.ty == 3/* \id */ || l.ty == 5/* \tok */ || l.ty == 6/* \sym */ || l.ty == 7/* \key */) {

        completeProduction(34, 2, 101); stack_ptr -= 2;

        ; return;
    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 66: //string_group_0109_128
                State356(l);
                break;
            case 101/*declaration_values*/:
            case 103/*declaration_value*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State363(l: Lexer): void {
    /*
string_value=>string_value string_group_0109_128 •|);
string_value=>string_value string_group_0109_128 •|";
string_value=>string_value string_group_0109_128 •|tok;
string_value=>string_value string_group_0109_128 •|key;
string_value=>string_value string_group_0109_128 •|id;
string_value=>string_value string_group_0109_128 •|sym;
string_value=>string_value string_group_0109_128 •|num;
string_value=>string_value string_group_0109_128 •|ws;
string_value=>string_value string_group_0109_128 •|$eof
*/
    _skip(l, const_1_);
    if (l.id == 18/* \) */ || l.id == 27/* \" */) {

        completeProduction(34, 2, 108); stack_ptr -= 2;

        ; return;
    } else if (l.ty == 0/* EOF */ || l.ty == 1/* \ws */ || l.ty == 2/* \num */ || l.ty == 3/* \id */ || l.ty == 5/* \tok */ || l.ty == 6/* \sym */ || l.ty == 7/* \key */) {

        completeProduction(34, 2, 108); stack_ptr -= 2;

        ; return;
    }
    else fail(l);
}
function State368(l: Lexer): void {
    /*
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_HC_listbody1_102 STYLE_SHEET_group_03_101 •|$;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_HC_listbody1_102 STYLE_SHEET_group_03_101 •|id;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_HC_listbody1_102 STYLE_SHEET_group_03_101 •|key;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_HC_listbody1_102 STYLE_SHEET_group_03_101 •|-;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_HC_listbody1_102 STYLE_SHEET_group_03_101 •|*;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_HC_listbody1_102 STYLE_SHEET_group_03_101 •||;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_HC_listbody1_102 STYLE_SHEET_group_03_101 •|#;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_HC_listbody1_102 STYLE_SHEET_group_03_101 •|.;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_HC_listbody1_102 STYLE_SHEET_group_03_101 •|[;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_HC_listbody1_102 STYLE_SHEET_group_03_101 •|:;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_HC_listbody1_102 STYLE_SHEET_group_03_101 •|@;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_HC_listbody1_102 STYLE_SHEET_group_03_101 •|$eof
*/
    _skip(l, const__);
    if (idm368r.has(l.id)) { idm368r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProduction(1, 2, 3); stack_ptr -= 2;

        ; return;
    }
    else fail(l);
}
function State369(l: Lexer): void {
    /*
GROUP_RULE_BODY=>GROUP_RULE_BODY STYLE_RULE •|$;
GROUP_RULE_BODY=>GROUP_RULE_BODY STYLE_RULE •|id;
GROUP_RULE_BODY=>GROUP_RULE_BODY STYLE_RULE •|key;
GROUP_RULE_BODY=>GROUP_RULE_BODY STYLE_RULE •|-;
GROUP_RULE_BODY=>GROUP_RULE_BODY STYLE_RULE •|*;
GROUP_RULE_BODY=>GROUP_RULE_BODY STYLE_RULE •||;
GROUP_RULE_BODY=>GROUP_RULE_BODY STYLE_RULE •|#;
GROUP_RULE_BODY=>GROUP_RULE_BODY STYLE_RULE •|.;
GROUP_RULE_BODY=>GROUP_RULE_BODY STYLE_RULE •|[;
GROUP_RULE_BODY=>GROUP_RULE_BODY STYLE_RULE •|:;
GROUP_RULE_BODY=>GROUP_RULE_BODY STYLE_RULE •|};
GROUP_RULE_BODY=>GROUP_RULE_BODY STYLE_RULE •|$eof
*/
    _skip(l, const__);
    if (idm369r.has(l.id)) { idm369r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProduction(1, 2, 8); stack_ptr -= 2;

        ; return;
    }
    else fail(l);
}
function State389(l: Lexer): void {
    /*
keyframes_blocks_HC_listbody1_110=>keyframes_blocks_HC_listbody1_110 , keyframe_selector •|{;
keyframes_blocks_HC_listbody1_110=>keyframes_blocks_HC_listbody1_110 , keyframe_selector •|,;
keyframes_blocks_HC_listbody1_110=>keyframes_blocks_HC_listbody1_110 , keyframe_selector •|$eof
*/
    _skip(l, const__);
    if (l.id == 10/* \, */ || l.id == 11/* \{ */) {

        completeProduction(7, 3, 19); stack_ptr -= 3;

        ; return;
    } else if (l.ty == 0/* EOF */) {

        completeProduction(7, 3, 19); stack_ptr -= 3;

        ; return;
    }
    else fail(l);
}
function State390(l: Lexer): void {
    /*
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_HC_listbody2_133 COMPLEX_SELECTOR_group_1119_132 •|>;
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_HC_listbody2_133 COMPLEX_SELECTOR_group_1119_132 •|+;
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_HC_listbody2_133 COMPLEX_SELECTOR_group_1119_132 •|~;
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_HC_listbody2_133 COMPLEX_SELECTOR_group_1119_132 •|||;
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_HC_listbody2_133 COMPLEX_SELECTOR_group_1119_132 •|$;
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_HC_listbody2_133 COMPLEX_SELECTOR_group_1119_132 •|id;
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_HC_listbody2_133 COMPLEX_SELECTOR_group_1119_132 •|key;
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_HC_listbody2_133 COMPLEX_SELECTOR_group_1119_132 •|-;
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_HC_listbody2_133 COMPLEX_SELECTOR_group_1119_132 •|*;
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_HC_listbody2_133 COMPLEX_SELECTOR_group_1119_132 •||;
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_HC_listbody2_133 COMPLEX_SELECTOR_group_1119_132 •|#;
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_HC_listbody2_133 COMPLEX_SELECTOR_group_1119_132 •|.;
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_HC_listbody2_133 COMPLEX_SELECTOR_group_1119_132 •|[;
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_HC_listbody2_133 COMPLEX_SELECTOR_group_1119_132 •|:;
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_HC_listbody2_133 COMPLEX_SELECTOR_group_1119_132 •|);
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_HC_listbody2_133 COMPLEX_SELECTOR_group_1119_132 •|,;
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_HC_listbody2_133 COMPLEX_SELECTOR_group_1119_132 •|{;
COMPLEX_SELECTOR_HC_listbody2_133=>COMPLEX_SELECTOR_HC_listbody2_133 COMPLEX_SELECTOR_group_1119_132 •|$eof
*/
    _skip(l, const__);
    if (idm390r.has(l.id)) { idm390r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProduction(1, 2, 72); stack_ptr -= 2;

        ; return;
    }
    else fail(l);
}
function State407(l: Lexer): void {
    /*
media_query=>media_query_group_043_116 media_type media_query_group_144_117 •|,;
media_query=>media_query_group_043_116 media_type media_query_group_144_117 •|$eof;
media_query=>media_query_group_043_116 media_type media_query_group_144_117 •|{;
media_query=>media_query_group_043_116 media_type media_query_group_144_117 •|;;
media_query=>media_query_group_043_116 media_type media_query_group_144_117 •|@;
media_query=>media_query_group_043_116 media_type media_query_group_144_117 •|$;
media_query=>media_query_group_043_116 media_type media_query_group_144_117 •|id;
media_query=>media_query_group_043_116 media_type media_query_group_144_117 •|key;
media_query=>media_query_group_043_116 media_type media_query_group_144_117 •|-;
media_query=>media_query_group_043_116 media_type media_query_group_144_117 •|*;
media_query=>media_query_group_043_116 media_type media_query_group_144_117 •||;
media_query=>media_query_group_043_116 media_type media_query_group_144_117 •|#;
media_query=>media_query_group_043_116 media_type media_query_group_144_117 •|.;
media_query=>media_query_group_043_116 media_type media_query_group_144_117 •|[;
media_query=>media_query_group_043_116 media_type media_query_group_144_117 •|:
*/
    _skip(l, const__);
    if (idm407r.has(l.id)) { idm407r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProduction(29, 3, 37); stack_ptr -= 3;

        ; return;
    }
    else fail(l);
}
function State410(l: Lexer): void {
    /*
import_HC_listbody4_108=>import_HC_listbody4_108 , media_query •|,;
import_HC_listbody4_108=>import_HC_listbody4_108 , media_query •|;;
import_HC_listbody4_108=>import_HC_listbody4_108 , media_query •|@;
import_HC_listbody4_108=>import_HC_listbody4_108 , media_query •|$;
import_HC_listbody4_108=>import_HC_listbody4_108 , media_query •|id;
import_HC_listbody4_108=>import_HC_listbody4_108 , media_query •|key;
import_HC_listbody4_108=>import_HC_listbody4_108 , media_query •|-;
import_HC_listbody4_108=>import_HC_listbody4_108 , media_query •|*;
import_HC_listbody4_108=>import_HC_listbody4_108 , media_query •||;
import_HC_listbody4_108=>import_HC_listbody4_108 , media_query •|#;
import_HC_listbody4_108=>import_HC_listbody4_108 , media_query •|.;
import_HC_listbody4_108=>import_HC_listbody4_108 , media_query •|[;
import_HC_listbody4_108=>import_HC_listbody4_108 , media_query •|:;
import_HC_listbody4_108=>import_HC_listbody4_108 , media_query •|$eof
*/
    _skip(l, const__);
    if (idm410r.has(l.id)) { idm410r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProduction(7, 3, 13); stack_ptr -= 3;

        ; return;
    }
    else fail(l);
}
function State411(l: Lexer): void {
    /*
media_queries_group_039_115=>media_queries_group_039_115 , media_query •|,;
media_queries_group_039_115=>media_queries_group_039_115 , media_query •|{;
media_queries_group_039_115=>media_queries_group_039_115 , media_query •|$eof
*/
    _skip(l, const__);
    if (l.id == 10/* \, */ || l.id == 11/* \{ */) {

        completeProduction(7, 3, 33); stack_ptr -= 3;

        ; return;
    } else if (l.ty == 0/* EOF */) {

        completeProduction(7, 3, 33); stack_ptr -= 3;

        ; return;
    }
    else fail(l);
}
function State417(l: Lexer): void {
    /*
mf_range=>mf_name mf_range_group_074_125 mf_value •|);
mf_range=>mf_name mf_range_group_074_125 mf_value •|$eof
*/
    _skip(l, const__);
    if (l.id == 18/* \) */) {

        completeProduction(39, 3, 57); stack_ptr -= 3;

        ; return;
    } else if (l.ty == 0/* EOF */) {

        completeProduction(39, 3, 57); stack_ptr -= 3;

        ; return;
    }
    else fail(l);
}
function State419(l: Lexer): void {
    /*
mf_range=>mf_value mf_range_group_074_125 mf_name •|);
mf_range=>mf_value mf_range_group_074_125 mf_name •|$eof
*/
    _skip(l, const__);
    if (l.id == 18/* \) */) {

        completeProduction(39, 3, 57); stack_ptr -= 3;

        ; return;
    } else if (l.ty == 0/* EOF */) {

        completeProduction(39, 3, 57); stack_ptr -= 3;

        ; return;
    }
    else fail(l);
}
function State421(l: Lexer): void {
    /*
mf_range=>mf_value mf_range_group_188_127 identifier • mf_range_group_188_127 mf_value|): 57;
mf_range=>mf_value mf_range_group_188_127 identifier • mf_range_group_188_127 mf_value|$eof: 57
*/
    _skip(l, const__);
    if (l.id == 29/* \< */) {

        $mf_range_group_188_127(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 56: //mf_range_group_188_127
                State463(l);
                break;
            case 57/*mf_range*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State423(l: Lexer): void {
    /*
mf_range=>mf_value mf_range_group_183_126 identifier • mf_range_group_183_126 mf_value|): 57;
mf_range=>mf_value mf_range_group_183_126 identifier • mf_range_group_183_126 mf_value|$eof: 57
*/
    _skip(l, const__);
    if (l.id == 31/* \> */) {

        $mf_range_group_183_126(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 55: //mf_range_group_183_126
                State465(l);
                break;
            case 57/*mf_range*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State436(l: Lexer): void {
    /*
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list • ; }|$;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list • }|$;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list • ; }|id;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list • }|id;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list • ; }|key;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list • }|key;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list • ; }|-;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list • }|-;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list • ; }|*;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list • }|*;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list • ; }||;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list • }||;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list • ; }|#;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list • }|#;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list • ; }|.;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list • }|.;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list • ; }|[;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list • }|[;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list • ; }|:;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list • }|:;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list • ; }|$eof;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list • }|$eof;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list • ; }|@;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list • }|@;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list • ; }|};
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list • }|}
*/
    _skip(l, const__);
    if (idm436.has(l.id)) { idm436.get(l.id)(l); }
    else fail(l);
}
function State437(l: Lexer): void {
    /*
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { ; • }|$;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { ; • }|id;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { ; • }|key;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { ; • }|-;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { ; • }|*;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { ; • }||;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { ; • }|#;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { ; • }|.;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { ; • }|[;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { ; • }|:;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { ; • }|$eof;
declaration_list_HC_listbody2_141=>; •|$;
declaration_list_HC_listbody2_141=>; •|id;
declaration_list_HC_listbody2_141=>; •|key;
declaration_list_HC_listbody2_141=>; •|-;
declaration_list_HC_listbody2_141=>; •|;;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { ; • }|@;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { ; • }|}
*/
    _skip(l, const__);
    if (l.id == 13/* \} */) {

        _no_check(l);; stack_ptr++; State472(l);

    } else if (l.id == 12/* \; */ || l.id == 55/* \- */ || l.id == 56/* \$ */) {

        completeProduction(2, 1, 94); stack_ptr -= 1;

        ; return;
    } else if (l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProduction(2, 1, 94); stack_ptr -= 1;

        ; return;
    }
    else fail(l);
}
function State438(l: Lexer): void {
    /*
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { } •|$;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { } •|id;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { } •|key;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { } •|-;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { } •|*;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { } •||;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { } •|#;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { } •|.;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { } •|[;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { } •|:;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { } •|$eof;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { } •|@;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { } •|}
*/
    _skip(l, const__);
    if (idm438r.has(l.id)) { idm438r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProduction(9, 3, 6); stack_ptr -= 3;

        ; return;
    }
    else fail(l);
}
function State447(l: Lexer): void {
    /*
declaration_list_HC_listbody2_143=>declaration_list_HC_listbody2_143 ; declaration_list_group_1153_142 •|;;
declaration_list_HC_listbody2_143=>declaration_list_HC_listbody2_143 ; declaration_list_group_1153_142 •|};
declaration_list_HC_listbody2_143=>declaration_list_HC_listbody2_143 ; declaration_list_group_1153_142 •|$eof
*/
    _skip(l, const__);
    if (l.id == 12/* \; */ || l.id == 13/* \} */) {

        completeProduction(7, 3, 96); stack_ptr -= 3;

        ; return;
    } else if (l.ty == 0/* EOF */) {

        completeProduction(7, 3, 96); stack_ptr -= 3;

        ; return;
    }
    else fail(l);
}
function State448(l: Lexer): void {
    /*
declaration_values=>declaration_values ( declaration_values • )|!;
declaration_values=>declaration_values ( declaration_values • )|(;
declaration_values=>declaration_values ( declaration_values • )|);
declaration_values=>declaration_values ( declaration_values • )|tok;
declaration_values=>declaration_values ( declaration_values • )|key;
declaration_values=>declaration_values ( declaration_values • )|id;
declaration_values=>declaration_values ( declaration_values • )|sym;
declaration_values=>declaration_values ( declaration_values • )|num;
declaration_values=>declaration_values ( declaration_values • )|ws;
declaration_values=>declaration_values ( declaration_values • )|;;
declaration_values=>declaration_values ( declaration_values • )|};
declaration_values=>declaration_values ( declaration_values • )|$eof;
declaration_values=>declaration_values • ( declaration_values )|);
declaration_values=>declaration_values • declaration_value|): 101;
declaration_values=>declaration_values • ( declaration_values )|(;
declaration_values=>declaration_values • declaration_value|(: 101;
declaration_values=>declaration_values • ( declaration_values )|tok;
declaration_values=>declaration_values • ( declaration_values )|key;
declaration_values=>declaration_values • ( declaration_values )|id;
declaration_values=>declaration_values • ( declaration_values )|sym;
declaration_values=>declaration_values • ( declaration_values )|num;
declaration_values=>declaration_values • ( declaration_values )|ws;
declaration_values=>declaration_values • declaration_value|tok: 101;
declaration_values=>declaration_values • declaration_value|key: 101;
declaration_values=>declaration_values • declaration_value|id: 101;
declaration_values=>declaration_values • declaration_value|sym: 101;
declaration_values=>declaration_values • declaration_value|num: 101;
declaration_values=>declaration_values • declaration_value|ws: 101
*/
    _skip(l, const_1_);
    if (idm448.has(l.id)) { idm448.get(l.id)(l); } else if (l.ty == 1/* \ws */ || l.ty == 2/* \num */ || l.ty == 3/* \id */ || l.ty == 5/* \tok */ || l.ty == 6/* \sym */ || l.ty == 7/* \key */) {

        $declaration_value(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 103: //declaration_value
                State360(l);
                break;
            case 101/*declaration_values*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State456(l: Lexer): void {
    /*
STYLE_RULE_HC_listbody2_103=>STYLE_RULE_HC_listbody2_103 , COMPLEX_SELECTOR •|,;
STYLE_RULE_HC_listbody2_103=>STYLE_RULE_HC_listbody2_103 , COMPLEX_SELECTOR •|{;
STYLE_RULE_HC_listbody2_103=>STYLE_RULE_HC_listbody2_103 , COMPLEX_SELECTOR •|$eof
*/
    _skip(l, const__);
    if (l.id == 10/* \, */ || l.id == 11/* \{ */) {

        completeProduction(7, 3, 5); stack_ptr -= 3;

        ; return;
    } else if (l.ty == 0/* EOF */) {

        completeProduction(7, 3, 5); stack_ptr -= 3;

        ; return;
    }
    else fail(l);
}
function State463(l: Lexer): void {
    /*
mf_range=>mf_value mf_range_group_188_127 identifier mf_range_group_188_127 • mf_value|): 57;
mf_range=>mf_value mf_range_group_188_127 identifier mf_range_group_188_127 • mf_value|$eof: 57
*/
    _skip(l, const__);
    if (l.id == 55/* \- */ || l.id == 56/* \$ */) {

        $mf_value(l); stack_ptr++;

    } else if (l.ty == 2/* \num */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        $mf_value(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 58: //mf_value
                State492(l);
                break;
            case 57/*mf_range*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State465(l: Lexer): void {
    /*
mf_range=>mf_value mf_range_group_183_126 identifier mf_range_group_183_126 • mf_value|): 57;
mf_range=>mf_value mf_range_group_183_126 identifier mf_range_group_183_126 • mf_value|$eof: 57
*/
    _skip(l, const__);
    if (l.id == 55/* \- */ || l.id == 56/* \$ */) {

        $mf_value(l); stack_ptr++;

    } else if (l.ty == 2/* \num */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        $mf_value(l); stack_ptr++;

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 58: //mf_value
                State494(l);
                break;
            case 57/*mf_range*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State470(l: Lexer): void {
    /*
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list ; • }|$;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list ; • }|id;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list ; • }|key;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list ; • }|-;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list ; • }|*;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list ; • }||;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list ; • }|#;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list ; • }|.;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list ; • }|[;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list ; • }|:;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list ; • }|$eof;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list ; • }|@;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list ; • }|}
*/
    _skip(l, const__);
    if (l.id == 13/* \} */) {

        _no_check(l);; stack_ptr++; State491(l);

    }
    else fail(l);
}
function State471(l: Lexer): void {
    /*
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list } •|$;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list } •|id;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list } •|key;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list } •|-;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list } •|*;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list } •||;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list } •|#;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list } •|.;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list } •|[;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list } •|:;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list } •|$eof;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list } •|@;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list } •|}
*/
    _skip(l, const__);
    if (idm471r.has(l.id)) { idm471r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProduction(8, 4, 6); stack_ptr -= 4;

        ; return;
    }
    else fail(l);
}
function State472(l: Lexer): void {
    /*
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { ; } •|$;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { ; } •|id;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { ; } •|key;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { ; } •|-;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { ; } •|*;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { ; } •||;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { ; } •|#;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { ; } •|.;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { ; } •|[;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { ; } •|:;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { ; } •|$eof;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { ; } •|@;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { ; } •|}
*/
    _skip(l, const__);
    if (idm472r.has(l.id)) { idm472r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProduction(9, 4, 6); stack_ptr -= 4;

        ; return;
    }
    else fail(l);
}
function State480(l: Lexer): void {
    /*
STYLE_SHEET=>STYLE_SHEET_HC_listbody1_100 STYLE_SHEET_HC_listbody1_102 •|$eof;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_HC_listbody1_102 • STYLE_SHEET_group_03_101|$eof: 3;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_HC_listbody1_102 • STYLE_SHEET_group_03_101|$: 3;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_HC_listbody1_102 • STYLE_SHEET_group_03_101|id: 3;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_HC_listbody1_102 • STYLE_SHEET_group_03_101|key: 3;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_HC_listbody1_102 • STYLE_SHEET_group_03_101|-: 3;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_HC_listbody1_102 • STYLE_SHEET_group_03_101|*: 3;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_HC_listbody1_102 • STYLE_SHEET_group_03_101||: 3;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_HC_listbody1_102 • STYLE_SHEET_group_03_101|#: 3;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_HC_listbody1_102 • STYLE_SHEET_group_03_101|.: 3;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_HC_listbody1_102 • STYLE_SHEET_group_03_101|[: 3;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_HC_listbody1_102 • STYLE_SHEET_group_03_101|:: 3;
STYLE_SHEET_HC_listbody1_102=>STYLE_SHEET_HC_listbody1_102 • STYLE_SHEET_group_03_101|@: 3
*/
    _skip(l, const__);
    if (idm342.has(l.id)) { idm342.get(l.id)(l); } else if (l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        $STYLE_SHEET_group_03_101(l); stack_ptr++;

    } else if (l.ty == 0/* EOF */) {

        completeProduction(3, 2, 4); stack_ptr -= 2;

        ; return;
    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 2: //STYLE_SHEET_group_03_101
                State368(l);
                break;
            case 4/*STYLE_SHEET*/:
            case 3/*STYLE_SHEET_HC_listbody1_102*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}
function State481(l: Lexer): void {
    /*
STYLE_SHEET_HC_listbody1_100=>STYLE_SHEET_HC_listbody1_100 import •|$;
STYLE_SHEET_HC_listbody1_100=>STYLE_SHEET_HC_listbody1_100 import •|id;
STYLE_SHEET_HC_listbody1_100=>STYLE_SHEET_HC_listbody1_100 import •|key;
STYLE_SHEET_HC_listbody1_100=>STYLE_SHEET_HC_listbody1_100 import •|-;
STYLE_SHEET_HC_listbody1_100=>STYLE_SHEET_HC_listbody1_100 import •|*;
STYLE_SHEET_HC_listbody1_100=>STYLE_SHEET_HC_listbody1_100 import •||;
STYLE_SHEET_HC_listbody1_100=>STYLE_SHEET_HC_listbody1_100 import •|#;
STYLE_SHEET_HC_listbody1_100=>STYLE_SHEET_HC_listbody1_100 import •|.;
STYLE_SHEET_HC_listbody1_100=>STYLE_SHEET_HC_listbody1_100 import •|[;
STYLE_SHEET_HC_listbody1_100=>STYLE_SHEET_HC_listbody1_100 import •|:;
STYLE_SHEET_HC_listbody1_100=>STYLE_SHEET_HC_listbody1_100 import •|@;
STYLE_SHEET_HC_listbody1_100=>STYLE_SHEET_HC_listbody1_100 import •|$eof;
AT_RULE=>import • ;|$;
AT_RULE=>import •|$;
AT_RULE=>import • ;|id;
AT_RULE=>import •|id;
AT_RULE=>import • ;|key;
AT_RULE=>import •|key;
AT_RULE=>import • ;|-;
AT_RULE=>import •|-;
AT_RULE=>import • ;|*;
AT_RULE=>import •|*;
AT_RULE=>import • ;||;
AT_RULE=>import •||;
AT_RULE=>import • ;|#;
AT_RULE=>import •|#;
AT_RULE=>import • ;|.;
AT_RULE=>import •|.;
AT_RULE=>import • ;|[;
AT_RULE=>import •|[;
AT_RULE=>import • ;|:;
AT_RULE=>import •|:;
AT_RULE=>import • ;|@;
AT_RULE=>import •|@;
AT_RULE=>import • ;|$eof;
AT_RULE=>import •|$eof
*/
    _skip(l, const__);
    if (l.id == 12/* \; */) {

        _no_check(l);; stack_ptr++; State345(l);

    } else if (idm481r.has(l.id)) { idm481r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {
        let $mark = mark(), sp = stack_ptr, cp = l.copy();
        completeProduction(1, 2, 1); stack_ptr -= 2;
        if (FAILED) {
            reset($mark); FAILED = false; stack_ptr = sp;
            completeProductionPlain(1, 9); stack_ptr -= 1;;
        } else l.sync(cp);

        ; return;
    }
    else fail(l);
}
function State482(l: Lexer): void {
    /*
declaration_values=>declaration_values ( declaration_values ) •|!;
declaration_values=>declaration_values ( declaration_values ) •|(;
declaration_values=>declaration_values ( declaration_values ) •|);
declaration_values=>declaration_values ( declaration_values ) •|tok;
declaration_values=>declaration_values ( declaration_values ) •|key;
declaration_values=>declaration_values ( declaration_values ) •|id;
declaration_values=>declaration_values ( declaration_values ) •|sym;
declaration_values=>declaration_values ( declaration_values ) •|num;
declaration_values=>declaration_values ( declaration_values ) •|ws;
declaration_values=>declaration_values ( declaration_values ) •|;;
declaration_values=>declaration_values ( declaration_values ) •|};
declaration_values=>declaration_values ( declaration_values ) •|$eof
*/
    _skip(l, const_1_);
    if (l.id == 12/* \; */ || l.id == 13/* \} */ || l.id == 17/* \( */ || l.id == 18/* \) */ || l.id == 52/* \! */) {

        completeProduction(80, 4, 101); stack_ptr -= 4;

        ; return;
    } else if (l.ty == 0/* EOF */ || l.ty == 1/* \ws */ || l.ty == 2/* \num */ || l.ty == 3/* \id */ || l.ty == 5/* \tok */ || l.ty == 6/* \sym */ || l.ty == 7/* \key */) {

        completeProduction(80, 4, 101); stack_ptr -= 4;

        ; return;
    }
    else fail(l);
}
function State491(l: Lexer): void {
    /*
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list ; } •|$;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list ; } •|id;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list ; } •|key;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list ; } •|-;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list ; } •|*;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list ; } •||;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list ; } •|#;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list ; } •|.;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list ; } •|[;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list ; } •|:;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list ; } •|$eof;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list ; } •|@;
STYLE_RULE=>STYLE_RULE_HC_listbody2_103 { declaration_list ; } •|}
*/
    _skip(l, const__);
    if (idm491r.has(l.id)) { idm491r.get(l.id)(l); return; } else if (l.ty == 0/* EOF */ || l.ty == 3/* \id */ || l.ty == 7/* \key */) {

        completeProduction(8, 5, 6); stack_ptr -= 5;

        ; return;
    }
    else fail(l);
}
function State492(l: Lexer): void {
    /*
mf_range=>mf_value mf_range_group_188_127 identifier mf_range_group_188_127 mf_value •|);
mf_range=>mf_value mf_range_group_188_127 identifier mf_range_group_188_127 mf_value •|$eof
*/
    _skip(l, const__);
    if (l.id == 18/* \) */) {

        completeProduction(41, 5, 57); stack_ptr -= 5;

        ; return;
    } else if (l.ty == 0/* EOF */) {

        completeProduction(41, 5, 57); stack_ptr -= 5;

        ; return;
    }
    else fail(l);
}
function State494(l: Lexer): void {
    /*
mf_range=>mf_value mf_range_group_183_126 identifier mf_range_group_183_126 mf_value •|);
mf_range=>mf_value mf_range_group_183_126 identifier mf_range_group_183_126 mf_value •|$eof
*/
    _skip(l, const__);
    if (l.id == 18/* \) */) {

        completeProduction(40, 5, 57); stack_ptr -= 5;

        ; return;
    } else if (l.ty == 0/* EOF */) {

        completeProduction(40, 5, 57); stack_ptr -= 5;

        ; return;
    }
    else fail(l);
}
function $media_condition(l: Lexer): void {
    /*
media_condition=>• media_condition_without_or|): 38;
media_condition=>• media_or|): 38;
media_condition=>• media_condition_without_or|,: 38;
media_condition=>• media_or|,: 38;
media_condition=>• media_condition_without_or|;: 38;
media_condition=>• media_or|;: 38;
media_condition=>• media_condition_without_or|@: 38;
media_condition=>• media_or|@: 38;
media_condition=>• media_condition_without_or|$: 38;
media_condition=>• media_or|$: 38;
media_condition=>• media_condition_without_or|id: 38;
media_condition=>• media_or|id: 38;
media_condition=>• media_condition_without_or|key: 38;
media_condition=>• media_or|key: 38;
media_condition=>• media_condition_without_or|-: 38;
media_condition=>• media_or|-: 38;
media_condition=>• media_condition_without_or|*: 38;
media_condition=>• media_or|*: 38;
media_condition=>• media_condition_without_or||: 38;
media_condition=>• media_or||: 38;
media_condition=>• media_condition_without_or|#: 38;
media_condition=>• media_or|#: 38;
media_condition=>• media_condition_without_or|.: 38;
media_condition=>• media_or|.: 38;
media_condition=>• media_condition_without_or|[: 38;
media_condition=>• media_or|[: 38;
media_condition=>• media_condition_without_or|:: 38;
media_condition=>• media_or|:: 38;
media_condition=>• media_condition_without_or|$eof: 38;
media_condition=>• media_or|$eof: 38;
media_condition=>• media_condition_without_or|{: 38;
media_condition=>• media_or|{: 38
*/
    _skip(l, const__);
    if (idm508.has(l.id)) { idm508.get(l.id)(l); } else if (l.ty == 3/* \id */ || l.ty == 7/* \key */) {
        let $mark = mark(), sp = stack_ptr, cp = l.copy();
        $media_or(cp); stack_ptr++;
        if (FAILED) {
            reset($mark); FAILED = false; stack_ptr = sp;
            $media_condition_without_or(l); stack_ptr++;;
        } else l.sync(cp);

    }
    else fail(l);
    const sp: u32 = stack_ptr;
    while (sp <= stack_ptr) {
        switch (prod) {
            case 46: //media_or
                State46(l);
                break;
            case 39: //media_condition_without_or
                State45(l);
                break;
            case 38/*media_condition*/: return;
            default: fail(l); return;
        }
        if (prod >= 0) stack_ptr++;
    }
}

function reset_counters_and_pointers(): void {
    prod = -1;

    stack_ptr = 0;

    error_ptr = 0;

    action_ptr = 0;

    FAILED = false;
}

export default function main(input_string: string): boolean {

    str = input_string;

    const lex = new Lexer();

    lex.next();

    reset_counters_and_pointers();

    $CSS(lex);

    set_action(0);

    set_error(0);

    return FAILED || !lex.END;
}