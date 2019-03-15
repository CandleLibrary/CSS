import {CSSRuleBody} from "../source/css.mjs";
import whind from "@candlefw/whind";


function checkColor(name, val){
	return function(props){
		const prop = checkProp(name)(props)
		prop.toRGBString().should.equal(val)
	}
}

function checkLength(name, val, type){
	return function(props){
		const prop = checkProp(name)(props)
		prop.should.equal(val);
		prop.unit.should.equal(type);
	}
}
function checkProp(name){
	return function(props){
		props.should.have.property(name);
		return props[name];
	}
}
function checkText(name, val){
	return function(props){
		if(typeof(props) == "string"){
			props.should.equal(name);
		}else{
			props.should.have.property(name, val)
		}
	}
}

function textSpread(name, ...rest){
	let prop_name = name.replace(/\-/g, "_");
	for(let i = 0; i < rest.length; i++){
		let text = rest[i];
		test.value = `${name}:${text}`
		test.check = checkText( prop_name, text);
	}
}

function checkArray(name, ...rest){
	return function(props){
		props.should.have.property(name);

		const array = props[name];
		
		array.should.have.property("length", rest.length);

		for(let i = 0; i < rest.length; i++){
			let func = rest[i];
			let prop = array[i];

			func(prop);
		}
	}
}

/*** Font Family ***/
const test = {
	v:null, 
	set value(v){
		this.v = v;
	},
	set check(f){
		if(!this.v)
			throw new Error("Please provide CSS property value before defining a check funcition.")
		const v = this.v;
		it(`Parses property {${v}}`, async ()=>{
			const body = new CSSRuleBody();
			await body.parse(whind(`{${v}}`));
			f(body.rules[0].props);
		});
		this.v = "";
	}
}

describe("CSS Level 1", ()=>{




describe("Font Properties: https://www.w3.org/TR/REC-CSS1/?utm_source=www.uoota.com#font-properties", ()=>{

	test.value = `font-family: Arial, Times New Roman, sans-serif`;
	test.check = checkArray("font_family", checkText("Arial"), checkText("Times New Roman"), checkText("sans-serif"))


	textSpread(
		"font-style", 
		"normal",
		"italic",
		"oblique"
	)

	textSpread(
		"font-variant", 
		"normal",
		"small-caps"
	)
	
	textSpread(
		"font-weight",
		"normal",
		"bold",
		"bolder",
		"lighter",
		"100",
		"200",
		"300",
		"400",
		"500",
		"600",
		"700",
		"800",
		"900"
	)

	textSpread(
		"font-size",
		"xx-small",
		"x-small",
		"small",
		"medium",
		"large",
		"x-large",
		"xx-large",
		"larger",
		"smaller"
	);

	test.value = "font-size:90px"
	test.check = checkLength( "font_size", 90, "px");

	test.value = "font-size:90em"
	test.check = checkLength( "font_size", 90, "em");

	test.value = "font-size:90em"
	test.check = checkLength( "font_size", 90, "em");

	test.value = "font:italic 30px Arial, sans-serif";
})

describe("Color and Background Properties: https://www.w3.org/TR/REC-CSS1/?utm_source=www.uoota.com#color-and-background-properties", ()=>{
	
	test.value = "color: red"
	test.check = checkColor("color", "rgba(255,0,0,1)");
	
	test.value = "color: #FF0000"
	test.check = checkColor("color", "rgba(255,0,0,1)");
	
	test.value = "color: rgb(255, 0, 0)";
	test.check = checkColor("color", "rgba(255,0,0,1)");
	
	test.value = "color: rgb(100%, 0%, 0%)";
	test.check = checkColor("color", "rgba(255,0,0,1)");

	test.value = "background-color: transparent";
	test.check = checkColor("background_color", "rgba(0,0,0,0)");

	test.value = "background-color: #FF0000";
	test.check = checkColor("background_color", "rgba(255,0,0,1)");

	test.value = "background-image:none";
	test.check = checkText("background_color", "none");

	test.value = "background-image:url(test.me:8080)";
	test.check = (props)=> (props.should.have.property("background_image"),props.background_image.host.should.equal("test.me"),props.background_image.port.should.equal(8080));

	test.value = "background-repeat:repeat";
	test.check = (props)=> props.should.have.property("background_repeat", "repeat");

	test.value = "background-repeat:repeat-x";
	test.check = (props)=> props.should.have.property("background_repeat", "repeat-x");

	test.value = "background-repeat:repeat-y";
	test.check = (props)=> props.should.have.property("background_repeat", "repeat-y");

	test.value = "background-repeat:no-repeat";
	test.check = (props)=> props.should.have.property("background_repeat", "no-repeat");

	test.value = "background-attachement:scroll";
	test.check = (props)=> props.should.have.property("background_attachement", "scroll");

	test.value = "background-attachement:fixed";
	test.check = (props)=> props.should.have.property("background_attachement", "fixed");

	test.value = "background-position:0% 0%";
	test.check = (props)=> (props.should.have.property("background_position"));

	test.value = "background-position:top right";
	test.check = (props)=> (props.should.have.property("background_position"));
})

describe("Text Properties: https://www.w3.org/TR/REC-CSS1/?utm_source=www.uoota.com#text-properties", ()=>{

	test.value = "word-spacing:normal";
	test.check = checkText("word_spacing", "normal");

	test.value = "word-spacing:20px";
	test.check = checkLength("word_spacing", 20, "px");

	test.value = "letter-spacing:normal";
	test.check = checkText("letter_spacing", "normal");

	test.value = "letter-spacing:20px";
	test.check = checkLength("letter_spacing", 20, "px");

	test
})

/** Position Tests **/
test.value = `position:absolute`;
test.check = (props)=> props.should.have.property("position", "absolute");

test.value = `position:relative`;
test.check = (props)=> props.should.have.property("position", "relative");
})