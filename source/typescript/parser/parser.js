


    import loader from "@assemblyscript/loader";
    import {buildParserMemoryBuffer} from "@candlefw/hydrocarbon";              
    import URL from "@candlefw/url";
    import Lexer from "@candlefw/wind";




const 
    { shared_memory, action_array, error_array } = buildParserMemoryBuffer(),
    fns = [(e,sym)=>sym[sym.length-1], 
(env, sym, pos)=>( ([...sym[0],sym[1]]))/*0*/
,(env, sym, pos)=>( [sym[0]])/*1*/
,(env, sym, pos)=>( {type:env.typ.Stylesheet,imports:sym[0],nodes:sym[1],pos})/*2*/
,(env, sym, pos)=>( {type:env.typ.Stylesheet,imports:null,nodes:sym[0],pos})/*3*/
,(env, sym, pos)=>( {type:env.typ.Stylesheet,imports:sym[0],nodes:null,pos})/*4*/
,(env, sym, pos)=>( {type:env.typ.Stylesheet,imports:null,nodes:null,pos})/*5*/
,(env, sym, pos)=>( ([...sym[0],sym[2]]))/*6*/
,(env, sym, pos)=>( {type:env.typ.Rule,selectors:sym[0],props:new Map((sym[2]||[]).map(s=>[s.name,s])),pos,precedence:0})/*7*/
,(env, sym, pos)=>( {type:env.typ.Rule,selectors:sym[0],props:new Map((null||[]).map(s=>[s.name,s])),pos,precedence:0})/*8*/
,(env, sym, pos)=>( {import:"@import",type:env.typ.Import,nodes:[sym[2],sym[3],...sym[4]],pos})/*9*/
,(env, sym, pos)=>( {import:"@import",type:env.typ.Import,nodes:[sym[2],null,...sym[3]],pos})/*10*/
,(env, sym, pos)=>( {import:"@import",type:env.typ.Import,nodes:[sym[2],sym[3],...null],pos})/*11*/
,(env, sym, pos)=>( {import:"@import",type:env.typ.Import,nodes:[sym[2],null,...null],pos})/*12*/
,(env, sym, pos)=>( {keyframes:"@keyframes",type:env.typ.Keyframes,name:sym[2],nodes:[sym[4]],pos})/*13*/
,(env, sym, pos)=>( {type:env.typ.KeyframeBlock,nodes:[{type:env.type.KeyframeSelectors,nodes:sym[0],pos},sym[2]],pos})/*14*/
,(env, sym, pos)=>( {type:env.typ.KeyframeSelector,val:sym[0],pos})/*15*/
,(env, sym, pos)=>( {type:env.typ.SupportConditions,nodes:sym[0],pos})/*16*/
,(env, sym, pos)=>( {type:env.typ.Supports,nodes:[sym[0],sym[2]],pos})/*17*/
,(env, sym, pos)=>( {type:env.typ.And,nodes:[sym[1]],pos})/*18*/
,(env, sym, pos)=>( {type:env.typ.Or,nodes:[sym[1]],pos})/*19*/
,(env, sym, pos)=>( [{type:env.typ.Not,nodes:[sym[1]],pos}])/*20*/
,(env, sym, pos)=>( [sym[0],...sym[1]])/*21*/
,(env, sym, pos)=>( {type:env.typ.Parenthesis,nodes:[sym[1]],pos})/*22*/
,(env, sym, pos)=>( {type:env.typ.Function,nodes:[sym[0]],pos})/*23*/
,(env, sym, pos)=>( {media:"@media",type:env.typ.Media,nodes:[sym[2],...sym[4]],pos})/*24*/
,(env, sym, pos)=>( {media:"@media",type:env.typ.Media,nodes:[sym[2]],pos})/*25*/
,(env, sym, pos)=>( {queries:true,type:env.typ.MediaQueries,nodes:sym[0],pos})/*26*/
,(env, sym, pos)=>( {type:env.typ.Query,nodes:[sym[0]],pos})/*27*/
,(env, sym, pos)=>( {type:env.typ.Query,condition:sym[0],nodes:[sym[1],sym[2]],pos})/*28*/
,(env, sym, pos)=>( {type:env.typ.Query,nodes:[sym[0],sym[1]],pos})/*29*/
,(env, sym, pos)=>( {type:env.typ.Query,condition:sym[0],nodes:[sym[1]],pos})/*30*/
,(env, sym, pos)=>( {type:env.typ.Not,nodes:[sym[1]],pos})/*31*/
,(env, sym, pos)=>( {type:env.typ.MediaFeature,nodes:[sym[1]],pos})/*32*/
,(env, sym, pos)=>( sym[0]+sym[1])/*33*/
,(env, sym, pos)=>( sym[0]+"")/*34*/
,(env, sym, pos)=>( {type:env.typ.MediaFunction,nodes:[sym[2]],pos})/*35*/
,(env, sym, pos)=>( {type:env.typ.MediaFunction,nodes:[],pos})/*36*/
,(env, sym, pos)=>( {type:env.typ.MediaValue,key:sym[0],val:sym[2],pos})/*37*/
,(env, sym, pos)=>( {type:env.typ.MediaEquality,sym:sym[1],left:sym[0],right:sym[2],pos})/*38*/
,(env, sym, pos)=>( {type:env.typ.MediaRangeDescending,sym:sym[1],max:sym[0],id:sym[2],min:sym[4],pos})/*39*/
,(env, sym, pos)=>( {type:env.typ.MediaRangeAscending,sym:sym[1],min:sym[0],id:sym[2],max:sym[4],pos})/*40*/
,(env, sym, pos)=>( {type:env.typ.Boolean,val:true,pos})/*41*/
,(env, sym, pos)=>( {type:env.typ.Boolean,val:false,pos})/*42*/
,(env, sym, pos)=>( {type:env.typ.MediaType,val:sym[0],pos})/*43*/
,(env, sym, pos)=>( {type:"ratio",num:sym[0],den:sym[2]})/*44*/
,(env, sym, pos)=>( new env.fn.percentage(sym[0]+sym[1]))/*45*/
,(env, sym, pos)=>( new env.fn.length(sym[0]+sym[1]))/*46*/
,(env, sym, pos)=>( new env.fn.url(sym[2]))/*47*/
,(env, sym, pos)=>( sym[1])/*48*/
,(env, sym, pos)=>( {type:env.typ.Combinator,val:sym[0]})/*49*/
,(env, sym, pos)=>( [sym[0],sym[1]])/*50*/
,(env, sym, pos)=>( (sym[0]&&sym[1])?{type:env.typ.ComplexSelector,nodes:[sym[0],...((sym[1]).flat(2))],pos}:sym[0])/*51*/
,(env, sym, pos)=>( (sym[0]&&null)?{type:env.typ.ComplexSelector,nodes:[sym[0]],pos}:sym[0])/*52*/
,(env, sym, pos)=>( {type:env.typ.PseudoSelector,nodes:[sym[0],...sym[1]],pos})/*53*/
,(env, sym, pos)=>( {type:env.typ.PseudoSelector,nodes:[sym[0]],pos})/*54*/
,(env, sym, pos)=>( (sym[0]&&!(sym[1]||sym[2]))?sym[0]:(sym[1]&&sym[1].length==1&&!(sym[0]||sym[2]))?sym[1][0]:{type:env.typ.CompoundSelector,nodes:[sym[0],...sym[1],...sym[2]],pos})/*55*/
,(env, sym, pos)=>( (null&&!(sym[0]||sym[1]))?null:(sym[0]&&sym[0].length==1&&!(null||sym[1]))?sym[0][0]:{type:env.typ.CompoundSelector,nodes:[...sym[0],...sym[1]],pos})/*56*/
,(env, sym, pos)=>( (sym[0]&&!(null||sym[1]))?sym[0]:(null&&null.length==1&&!(sym[0]||sym[1]))?null[0]:{type:env.typ.CompoundSelector,nodes:[sym[0],...sym[1]],pos})/*57*/
,(env, sym, pos)=>( (sym[0]&&!(sym[1]||null))?sym[0]:(sym[1]&&sym[1].length==1&&!(sym[0]||null))?sym[1][0]:{type:env.typ.CompoundSelector,nodes:[sym[0],...sym[1]],pos})/*58*/
,(env, sym, pos)=>( (null&&!(null||sym[0]))?null:(null&&null.length==1&&!(null||sym[0]))?null[0]:{type:env.typ.CompoundSelector,nodes:[...sym[0]],pos})/*59*/
,(env, sym, pos)=>( (null&&!(sym[0]||null))?null:(sym[0]&&sym[0].length==1&&!(null||null))?sym[0][0]:{type:env.typ.CompoundSelector,nodes:[...sym[0]],pos})/*60*/
,(env, sym, pos)=>( (sym[0]&&!(null||null))?sym[0]:(null&&null.length==1&&!(sym[0]||null))?null[0]:{type:env.typ.CompoundSelector,nodes:[sym[0]],pos})/*61*/
,(env, sym, pos)=>( {type:env.typ.IdSelector,val:sym[1],pos,precedence:env.typ.B_SPECIFIER})/*62*/
,(env, sym, pos)=>( {type:env.typ.ClassSelector,val:sym[1],pos,precedence:env.typ.C_SPECIFIER})/*63*/
,(env, sym, pos)=>( {type:env.typ.PseudoClassSelector,id:sym[1],val:sym[2],pos,precedence:env.typ.C_SPECIFIER})/*64*/
,(env, sym, pos)=>( {type:env.typ.PseudoClassSelector,id:sym[1],pos,precedence:env.typ.C_SPECIFIER})/*65*/
,(env, sym, pos)=>( `"`+sym[1]+`"`)/*66*/
,(env, sym, pos)=>( {type:env.typ.AttributeSelector,nodes:[sym[1]],pos,precedence:env.typ.C_SPECIFIER})/*67*/
,(env, sym, pos)=>( {type:env.typ.AttributeSelector,nodes:[sym[1]],match_type:sym[2],match_val:sym[3],mod:sym[4],pos,precedence:env.typ.C_SPECIFIER})/*68*/
,(env, sym, pos)=>( {type:env.typ.AttributeSelector,nodes:[sym[1]],match_type:sym[2],match_val:sym[3],pos,precedence:env.typ.C_SPECIFIER})/*69*/
,(env, sym, pos)=>( {type:env.typ.TypeSelector,nodes:[sym[0]],pos,precedence:env.typ.D_SPECIFIER})/*70*/
,(env, sym, pos)=>( {type:env.typ.TypeSelector,nodes:[{type:env.typ.QualifiedName,ns:sym[0]||"",val:"*",pos,precedence:0}],pos})/*71*/
,(env, sym, pos)=>( {type:env.typ.TypeSelector,nodes:[{type:env.typ.QualifiedName,ns:"",val:"*",pos,precedence:0}],pos})/*72*/
,(env, sym, pos)=>( sym[1].type=env.typ.PseudoElementSelector,sym[1].precedence=env.typ.D_SPECIFIER,sym[1])/*73*/
,(env, sym, pos)=>( sym[0])/*74*/
,(env, sym, pos)=>( null)/*75*/
,(env, sym, pos)=>( {type:env.typ.QualifiedName,ns:sym[0]||"",val:sym[1],pos})/*76*/
,(env, sym, pos)=>( {type:env.typ.QualifiedName,ns:"",val:sym[0],pos})/*77*/
,(env, sym, pos)=>( env.functions.parseDeclaration(sym, env, pos))/*78*/
,(env, sym, pos)=>( sym[0]+sym[1]+sym[2]+sym[3])/*79*/
,(env, sym, pos)=>( {val:parseFloat(sym[0]),type:"hex",original_val:sym[0]})/*80*/
,(env, sym, pos)=>( {val:parseFloat(sym[0]),type:"bin",original_val:sym[0]})/*81*/
,(env, sym, pos)=>( {val:parseFloat(sym[0]),type:"oct",original_val:sym[0]})/*82*/
,(env, sym, pos)=>( {val:parseFloat(sym[0]),type:"sci",original_val:sym[0]})/*83*/
,(env, sym, pos)=>( {val:parseFloat(sym[0]),type:"flt",original_val:sym[0]})/*84*/
,(env, sym, pos)=>( {val:parseFloat(sym[0]),type:"int",original_val:sym[0]})/*85*/];

export default async function loadParser(){ 

    await URL.server();

    const wasmModule = await loader.instantiate(await URL.resolveRelative("./build/wasm/recognizer.wasm").fetchBuffer(), { env: { memory: shared_memory } }),
    
    { main, __newString } = wasmModule.exports;

    return function (str, env = {}) {

        const 
            str_ptr = __newString(str),
            FAILED = main(str_ptr), // call with pointers
            aa = action_array,
            er = error_array,
            stack = [];
        
        let action_length = 0;
    
        if (FAILED) {
            
            let error_off = 0;


            const lexer = new Lexer(str);
            const probes = [];
            //Extract any probes
            for (let i = 0; i < er.length; i++) {
                if (((er[i] & 0xF000000F) >>> 0) == 0xF000000F) {
                    const num = er[i] & 0xFFFFFF0;
                    const sequence = (num >> 4) & 0xFFF;
                    const identifier = (num >> 16) & 0xFFF;
                    const token_type = [
                        "TokenEndOfLine",
                        "TokenSpace",
                        "TokenNumber",
                        "TokenIdentifier",
                        "TokenNewLine",
                        "TokenSymbol",
                        "TypeSymbol",
                        "TokenKeyword",
                    ][er[i + 1]];
                    const id = er[i + 2];
                    const token_length = er[i + 3];
                    const offset = er[i + 4];
                    const prod = er[i + 5] << 0;
                    const stack_ptr = er[i + 6];
                    const FAILED = !!er[i + 7];
                    i += 8;
                    const cp = lexer.copy();
                    cp.off = offset;
                    cp.tl = token_length;
                    probes.push({
                        identifier,
                        str: cp.tx,
                        token_type,
                        id,
                        token_length,
                        offset,
                        stack_ptr,
                        prod,
                        FAILED
                    });
                } else {
                    error_off = Math.max(error_off, er[i]);
                }
            }

            while (lexer.off < error_off && !lexer.END) lexer.next();
            console.table(probes);
            console.log(error_off, str.length);

            lexer.throw(`Unexpected token[${ lexer.tx }]`);
    
        } else {

            let offset = 0, pos = [{ off: 0, tl: 0 }];

            for (const action of aa) {

                action_length++;
                let prev_off = 0;

                if (action == 0) break;

                switch (action & 1) {
                    case 0: //REDUCE;
                        {
                            const
                                DO_NOT_PUSH_TO_STACK = (action >> 1) & 1,
                                body = action >> 16,
                                len = ((action >> 2) & 0x3FFF);

                            const pos_a = pos[pos.length - len - 1];
                            const pos_b = pos[pos.length - 1];
                            pos[stack.length - len] = { off: pos_a.off, tl: 0 };

                            stack[stack.length - len] = fns[body](env, stack.slice(-len), { off: pos_a.off, tl: pos_b.off - pos_a.off + pos_b.tl });

                            //  console.log(stack[stack.length - len], pos);

                            if (!DO_NOT_PUSH_TO_STACK) {
                                stack.length = stack.length - len + 1;
                                pos.length = pos.length - len + 1;
                            } else {
                                stack.length = stack.length - len;
                                pos.length = pos.length - len;
                            }
                            // console.log(pos);

                        } break;

                    case 1: { //SHIFT;
                        const
                            has_len = (action >>> 1) & 1,
                            has_skip = (action >>> 2) & 1,
                            len = action >>> (3 + (has_skip * 15)),
                            skip = has_skip * ((action >>> 3) & (~(has_len * 0xFFFF8000)));
                        offset += skip;
                        if (has_len) {
                            stack.push(str.slice(offset, offset + len));
                            pos.push({ off: offset, tl: len });
                            offset += len;
                        }
                    } break;
                }
            }
        }
    
        return { result: stack, FAILED: !!FAILED, action_length };
    }
    } 