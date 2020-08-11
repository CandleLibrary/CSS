import parseDeclaration from "./parse_declaration.js";
import observer from "@candlefw/observer";
import { CSSNode } from "../css.js";
import { Lexer } from "@candlefw/wind";
/* 	Wraps parseDeclaration with a function that returns a styleprop object or null.
    Uses same args as parseDeclaration */
export class CSSProperty {

	parent: CSSNode;
	val: any;

	name: string;
	original_value: string;

	rule: any;

	ver: number;

	IMPORTANT: boolean;

	pos?: Lexer;


	constructor(name, original_value, val, IMP, pos) {
		this.val = val;
		this.name = name.replace(/\-/g, "_");
		this.original_value = original_value;
		this.rule = null;
		this.ver = 0;
		this.IMPORTANT = IMP;
		this.pos = pos;
	}
	destroy() {
		this.val = null;
		this.name = "";
		this.original_value = "";
		this.rule = null;
		observer.destroy(this);
	}
	get camelName() {
		return this.name
			.split("_")
			.map(
				(v, i) => i > 0 ? v[0].toUpperCase() + v.slice(1) : v
			)
			.join("");
	}

	get css_type() {
		return "styleprop";
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
		const str = [], off = ("    ").repeat(offset);
		return `${off + this.name.replace(/\_/g, "-")}:${this.value_string}`;
	}

	/*setValueFromString(value) {
		const result = parseDeclaration([this.name, null, value]);
		if (result)
			this.setValue(...result.prop);
	}*/
	setValue(...values) {

		if (values[0] instanceof CSSProperty)
			return this.setValue(...values[0].val);

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

	copyVal() {
		if (Array.isArray(this.val)) {
			return this.val.slice();
		}
		else
			return this.val;
	}

	copy() {
		return new CSSProperty(this.name, this.original_value, this.copyVal(), this.IMPORTANT, this.pos);
	}

	set(prop: CSSProperty) {
		if (prop.name == this.name)
			this.val = prop.val.slice();
	}
}
