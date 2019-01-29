export default class CSS_Number extends Number {
    static parse(l, rule, r) {
        let tx = l.tx;
        if(l.ty == l.types.num){
            l.next();
            return new CSS_Number(tx);
        }
        return null;
    }
}