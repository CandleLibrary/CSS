# CandleFW CSS

CandleFW CSS is a lightweight css parser, editor, and generator tool.

# Install

### NPM 

```bash
npm install --save @candlefw/css
```

## Usage

>**note**:
>This script uses ES2015 module syntax,  and has the extension ***.mjs***. To include this script in a project, you may need to use the node flag ```--experimental-modules```; or, use a bundler that supports ES modules, such as [rollup](https://github.com/rollup/rollup-plugin-node-resolve).

```javascript
import css from "@candlefw/css";

css(`
    div, a.header, h1 > a {
        color: red;
        height: 100em;
    }
`).then(parsed_css => {
    parser_css.rules;
});

```
