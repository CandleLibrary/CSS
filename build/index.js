let start = async function() {

    let input = document.getElementById("input");

    input.addEventListener("change", async (e) => {
        let value = e.target.value;
        css_.parse(value);
    })

    input.value =
        `.test {
		border-color:red green;
	}`

	const css_ = await css.CSSParser(input.value)
    const uic = new ui.default(css_)
    uic.build();
    uic.render();
    uic.mount(document.body);

    css_.on


}

window.addEventListener("load", start)
