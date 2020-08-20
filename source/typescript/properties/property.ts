import parsePropertyDefinitionFromHydrocarbon from "./parse_declaration.js";
import { CSSNode } from "../css.js";
import { Lexer } from "@candlefw/wind";
import { PrecedenceFlags } from "../types/precedence_flags.js";
export class CSSProperty {

	parent: CSSNode;

	val: any;

	name: string;

	rule: any;
	precedence: PrecedenceFlags;

	pos?: Lexer;


	constructor(name, original_value, val, IMP, pos) {
		this.val = val;
		this.name = name.replace(/\-/g, "_");
		this.rule = null;
		this.precedence = +(!!IMP) << PrecedenceFlags.IMPORTANT_BIT_SHIFT;
		this.pos = pos;
	}
	destroy() {
		this.name = "";
		this.val = null;
		this.rule = null;
		this.pos = null;
	}

	toString(offset = 0) {
		const str = [], off = ("    ").repeat(offset);
		return `${off + this.name.replace(/\_/g, "-")}:${this.value_string}`;
	}

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
	}

	get IMPORTANT(): boolean {
		return !!(this.precedence & PrecedenceFlags.IMPORTANT_BIT_MASK);
	}

	copyVal() {
		if (Array.isArray(this.val))
			return this.val.slice();
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
	get original_value() {
		return this.pos.slice();
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

	get value() {
		return this.val.length > 1 ? this.val : this.val[0];
	}

	get value_string() {
		return this.val.join(" ");
	}
}
