export default class styleprop {
	constructor(name, original_value, val){
		this.val = val;
        this.name = name;
        this.original_value = original_value;
	}

    toString(offset = 0){
        const 
            str = [],
            off = ("    ").repeat(offset);

        return `${off+this.name.replace(/\_/g, "-")}:${this.val.join(" ")}`;
    }
}