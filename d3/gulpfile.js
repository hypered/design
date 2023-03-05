const D3Node = require('d3-node');
const gulp = require('gulp');
const file = require('gulp-file');
const rename = require('gulp-rename');
const through2 = require('through2');


// Given a file, load it and call its `draw()` function, returning
// its result.
function render() {
  return through2.obj(function(file, _, cb) {
    if (file.isBuffer()) {
      const m = require(file.path);
      const d3n = new D3Node();
      m.draw(d3n);
      file.contents = Buffer.from(d3n.svgString());
    }
    cb(null, file);
  });
}

// Render all JS files found in the graphs/ directory to corresponding
// SVG files in dist/.
function graphs() {
  return gulp.src('graphs/*.js')
    .pipe(render())
    .pipe(rename({ extname: '.svg' }))
    .pipe(gulp.dest('dist/'))
}

exports.build = gulp.parallel(graphs);
exports.default = exports.build;
