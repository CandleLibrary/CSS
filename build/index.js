let start = async function  (){

	const css_ = await css.CSSParser(`
	.test {
		font-family:sans-serif;
	}
	`)

	const uic = new ui.default(css_)

	uic.build();
	uic.render();
	uic.mount(document.body);
}

start()
