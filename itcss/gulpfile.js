// This script combines Gulp, Pug, Sass, and BrowserSync to watch files, build
// them, and serve the result.

const bs = require('browser-sync').create();
const path = require('path');
const gulp = require('gulp');
const data = require('gulp-data');
const pug = require('gulp-pug');
const sass = require('gulp-sass')(require('sass'));
const nanocss = require('gulp-cssnano');
// const rename = require('gulp-rename');


// Copy the static files
function assets() {
  return gulp.src('static/**/*')
    .pipe(gulp.dest('dist/static/'))
}

// Define the .css files we want. They're not the same in the
// itcss/ directory or struct/ directory.
const segments = process.cwd().split('/');
const dir = segments[segments.length - 1];
console.log(dir)
var scss_sources = 'scss/**/main.scss'; // for itcss/
if (dir == 'struct') {
  scss_sources = 'scss/**/*.scss'          // for struct/
}

// Build the SCSS files to CSS
function styles() {
  return gulp.src(scss_sources)
    .pipe(sass({
      includePaths: ['scss/'],
      errLogToConsole: true,
      outputStyle: 'expanded', // Or compressed, but we can have PurgeCSS
	                       // and CSSNano afterwards anyway.
      onError: bs.notify
    }))
    .pipe(gulp.dest('dist/static/css'))
    .pipe(bs.stream())
}

// Minify the CSS files
function styles_min() {
  return gulp.src('dist/static/css/struct.css')
    .pipe(nanocss()) // Minify the CSS using NanoCSS
    // .pipe(rename('struct.min.css')) # TODO This crashes.
    .pipe(gulp.dest('dist/static/css/min'))
}

// Build the Pug templates to HTML
const cwd = process.cwd();
function templates() {
  return gulp.src('templates/**/*.pug')
    .pipe(data(function (file) {
      return { require: function (p) { return require(path.join(cwd, p)); } };
    }))
    .pipe(pug())
    .pipe(gulp.dest('dist'))
}

// Serve the dist/ directory and rebuild on changes
function watch() {
  bs.init({
    server: 'dist',
    browser: [], // Don't automatically open a browser.
    port: 3002,
  });

  gulp.watch('static/**/*', assets);
  gulp.watch('scss/**/*.scss', gulp.series(styles, styles_min));
  gulp.watch('templates/**/*.pug', templates);
  gulp.watch('dist/*.html').on('change', bs.reload);
}


exports.assets = assets;
exports.styles = styles;
exports.templates = templates;
exports.watch = watch;

exports.build = gulp.parallel(assets, gulp.series(styles, styles_min), templates);
exports.serve = gulp.series(exports.build, watch);
exports.default = exports.serve;
