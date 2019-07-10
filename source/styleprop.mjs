export default class styleprop {
	constructor(name, original_value, val){
		this.val = val;
        this.name = name.replace(/\-/g, "_");
        this.original_value = original_value;
        this.rule = null;
	}

    get value(){
        return this.val.length > 1 ? this.val : this.val[0];
    }

    get value_string(){
        return this.val.join(" ");        
    }

    toString(offset = 0){
        const 
            str = [],
            off = ("    ").repeat(offset);

        return `${off+this.name.replace(/\_/g, "-")}:${this.value_string}`;
    }

    setValue(...values){
        let i = 0;

        for(const value of values){
            const own_val = this.val[i];


            if(value instanceof own_val.constructor)
                this.val[i] = value;
            else
                this.val[i] = value;
            i++
        }

        this.val.length = values.length;

        if(this.parent){
            this.parent.update();
        }
    }
}
