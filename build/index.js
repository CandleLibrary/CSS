let start = async function() {

    let input = document.getElementById("input");

    input.addEventListener("change", async (e) => {
        console.log(1)
        let value = e.target.value;
        css_.parse(value);
    })



    input.value =
        `.test {
        display: block flow list-item;
        border-color:red;
	}`

	const css_ = await css.CSSParser(input.value)
    const uic = new ui.default(css_)
    uic.build();
    uic.render();
    uic.mount(document.body);

    css_.addObserver({
        updatedCSS : ()=>{
            input.value = css_.toString();
        }
    });


}

window.addEventListener("load", start)
