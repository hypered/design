const D3Node = require('d3-node');
const gulp = require('gulp');
const file = require('gulp-file');


function graphs() {
  const d3n = new D3Node();
  d3n.createSVG(10,20).append('g');

  return file('out.svg', d3n.svgString(), {src: true})
    .pipe(gulp.dest('dist/'))
}

exports.build = gulp.parallel(graphs);
exports.default = exports.build;
