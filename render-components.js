/*
 * This render React components from the command-line. The rendering code is
 * similar to
 * https://github.com/ole-treichel/storybook-react-to-static-markup/blob/master/src/withStaticMarkup.js#L11.
 *
 * In addition to a normal `npm install`, I also run
 *     npm install @babel/core @babel/register --save-dev
 * to be able to execute this file with
 *     node render-components.js
 */

// Use babel/register because the components are ES modules.
require('@babel/register');
var ReactDOMServer = require('react-dom/server');
var pretty = require('pretty');

var Footer = require("./components/Footer/Footer").Footer;

console.log(pretty(ReactDOMServer.renderToStaticMarkup(Footer())));
