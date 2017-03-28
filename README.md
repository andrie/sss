# sss: Import triple-s survey files into R

master: [![master build status](https://travis-ci.org/andrie/sss.svg?branch=master)](https://travis-ci.org/andrie/sss)
dev: [![dev build status](https://travis-ci.org/andrie/sss.svg?branch=dev)](https://travis-ci.org/andrie/sss)
[![](http://www.r-pkg.org/badges/version/sss)](http://www.r-pkg.org/pkg/sss)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/sss)](http://www.r-pkg.org/pkg/secret)
[![Coverage Status](https://img.shields.io/codecov/c/github/andrie/sss/master.svg)](https://codecov.io/github/andrie/sss?branch=master)


The `sss` package provides a function to import triple-s `XML` files into R.  The package supports sss files in both `.asc` and `.csv` format.

## The triple-s standard

triple-s is a standard to transfer survey data between applications.

http://www.triple-s.org/

## System dependencies

A previous version of this package imported the `XML` package, but from version 0.1 the package imports `xml2`. The `xml2` package depends on the `libxml2` library.  If you run your code on linux, you may have to manually install `libxml2`:

* `libxml2-dev` (deb)
* `libxml2-devel` (rpm)
  
  
