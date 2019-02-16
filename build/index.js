let start = async function  (){

	const css_ = await css.CSSParser(`
	.test {
		position:absolute;
		top:8px;
		border-color:red green;
		font-family: cursive
	}
	`)

	const uic = new ui.default(css_)

	uic.build();
	uic.render();
	uic.mount(document.body);
}

start()
