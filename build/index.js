let start = async function  (){

	const css_ = await css.CSSParser(`
	.test {
		position:absolute
	}
	`)

	const uic = new ui.default(css_)

	console.log(uic)

	uic.build();
	uic.render();
	debugger
	uic.mount(document.body);
}

start()