var gulp = require('gulp');
var gutil = require('gulp-util');
var source = require('vinyl-source-stream')
var standalone = "Round"
var browserify = require('browserify');
var watchify = require('watchify');

var sources = ['./src/main.js'];
var destination = '../../public/javascripts/compiled';

var onError = function(error) {
  console.dir(error);
  gutil.log(gutil.colors.red(error.message));
};

gulp.task('dev', function() {
  var opts = watchify.args;
  opts.debug = true;
  opts.standalone = standalone;

  var bundleStream = watchify(browserify(sources, opts))
    .on('update', rebundle)
    .on('log', gutil.log);

  function rebundle() {
      return bundleStream.bundle()
        .on('error', onError)
        .pipe(source('round.js'))
        .pipe(gulp.dest(destination));
    }

  return rebundle();
});

gulp.task('default', ['dev']);
