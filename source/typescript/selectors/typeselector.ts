export default class type_selector_part {
	constructor(sym) {

		const val = sym[0];

		this.namespace = "";

		if (val.length > 1)
			this.namespace = val[0];

		this.val = ((val.length > 1) ? val[1] : val[0]).toLowerCase();
	}

	get type() {
		return "type";
	}

	matchReturnElement(element, win) {
		return element.tagName.toLowerCase() == this.val ? element : null;
	}

	toString() {
		return this.namespace + " " + this.val;
	}
}
