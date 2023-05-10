// This script combines Gulp, Pug, Sass, and BrowserSync to watch files, build
// them, and serve the result.

const bs = require('browser-sync').create();
const gulp = require('gulp');
const pug = require('gulp-pug');
const sass = require('gulp-sass')(require('sass'));


// Copy the static files
function assets() {
  return gulp.src('static/**/*')
    .pipe(gulp.dest('dist/static/'))
}

// Build the SCSS files to CSS
function styles() {
  return gulp.src('scss/main.scss')
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

// Build the Pug templates to HTML
function templates() {
  return gulp.src('templates/**/*.pug')
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
  gulp.watch('scss/**/*.scss', styles);
  gulp.watch('templates/**/*.pug', templates);
  gulp.watch('dist/*.html').on('change', bs.reload);
}


exports.assets = assets;
exports.styles = styles;
exports.templates = templates;
exports.watch = watch;

exports.build = gulp.parallel(assets, styles, templates);
exports.serve = gulp.series(exports.build, watch);
exports.default = exports.serve;
