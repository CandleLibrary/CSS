import resolve from 'rollup-plugin-node-resolve';

const output = [{
        name: "css",
        file: "./build/css.js",
        format: "iife"
    },{
        name: "css_cjs",
        file: "./build/css.node.js",
        format: "cjs"
    }];

export default {
    input: "./source/css.js",
    treeshake: false,
    output,
    plugins: [resolve({jail:"",modulesOnly: true})]
};
