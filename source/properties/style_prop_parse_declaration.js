import parseDeclaration from "./parse_declaration.js";
import styleprop from "../styleprop.js";

/* 	Wraps parseDeclaration with a function that returns a styleprop object or null. 
	Uses same args as parseDeclaration */

export default function (...v){

	const result = parseDeclaration(...v);

	if(result)
		return new styleprop(
			result.rule_name,
			result.body_string,
			result.prop
		)

	return null;
}
