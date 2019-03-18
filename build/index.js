let start = async function() {

    let style = document.createElement("style")
    let input = document.getElementById("input");

    document.head.appendChild(style);

    input.addEventListener("change", async (e) => {
        console.log(1)
        let value = e.target.value;
        css_.parse(value, true);
        
    })




    input.value =
`test div {
    font-family:serif;
    background-color:green;
    color:white;

}`

	const css_ = await css.CSSParser(input.value)
    const uic = new ui.default(css_)
    uic.build();
    uic.render();
    uic.mount(document.body);

    css_.addObserver({
        updatedCSS : ()=>{
            style.innerHTML = input.value = css_.toString();
        }
    });
}

window.addEventListener("load", start)
