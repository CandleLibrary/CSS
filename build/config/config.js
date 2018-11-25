import resolve from 'rollup-plugin-node-resolve';

const output = [{
        name: "css",
        file: "./build/css.js",
        format: "iife",
        exports:"named"
    },{
        name: "css_cjs",
        file: "./build/css-cjs.js",
        format: "cjs",
        exports:"named"
    }]

export default {
    input: "./source/css.mjs",
    treeshake: false,
    output,
    plugins: [resolve({jail:"",modulesOnly: true})]
};
