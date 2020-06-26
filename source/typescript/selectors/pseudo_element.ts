export default class pseudoElementSelector {
	constructor(sym, env) {
		this.val = sym[1].val;
	}

	get type() {
		return "pseudo-element";
	}

	matchReturnElement(element) {
		return element;
	}

	toString() {
		return `:${this.val}`;
	}
}
