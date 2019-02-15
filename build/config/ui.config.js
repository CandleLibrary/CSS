import resolve from 'rollup-plugin-node-resolve';

const output = [{
        name: "ui",
        file: "./build/ui.css.js",
        format: "iife",
        exports:"named"
    },{
        name: "ui_css_cjs",
        file: "./build/ui.css.node.js",
        format: "cjs",
        exports:"named"
    }];

export default {
    input: "./source/ui/builder.mjs",
    treeshake: false,
    output,
    plugins: [resolve({jail:"",modulesOnly: true})]
};
