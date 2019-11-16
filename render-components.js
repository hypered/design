/*
 * This render React components from the command-line. The rendering code is
 * similar to
 * https://github.com/ole-treichel/storybook-react-to-static-markup/blob/master/src/withStaticMarkup.js#L11.
 *
 * In addition to a normal `npm install`, I also run
 *     npm install @babel/core @babel/register --save-dev
 *
 * The usage is similar to hypered-guide.hs:
 *     node render-components.js footer
 *
 * In addition to component names, it also accepts 'pretty' as
 * argument. The intention is to use to normalize the output of
 * hypered-guide.hs, then be able to compare both programs:
 *
 *     runghc bin/hypered-guide.hs footer | node render-components.js pretty
 */

// Use babel/register because the components are ES modules.
require('@babel/register');
var ReactDOMServer = require('react-dom/server');
var pretty = require('pretty');
var fs = require('fs');

var Footer = require("./components/Footer/Footer").Footer;


var args = process.argv.slice(2);

switch (args[0]) {
case 'footer':
  console.log(pretty(ReactDOMServer.renderToStaticMarkup(Footer())));
  break;
case 'pretty':
  var content = fs.readFileSync(0, 'utf-8');
  console.log(pretty(content));
  break;
default:
  console.log('Unsupported argument.');
  process.exit(1);
  break;
}
