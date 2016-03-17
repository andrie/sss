# sss

master: [![master build status](https://travis-ci.org/andrie/sss.svg?branch=master)](https://travis-ci.org/andrie/sss)
dev: [![dev build status](https://travis-ci.org/andrie/sss.svg?branch=dev)](https://travis-ci.org/andrie/sss)

The aim of the `sss` package is to provide a function to import triple-s `XML` files into R.

## The triple-s standard

triple-s is a standard to transfer survey data between applications.

http://www.triple-s.org/

## Note about the dependency on the XML package 

The `sss` package depends on the `XML` package.

On Linux, this is straight-forward: get the package from CRAN and install in the usual way.

However, the `XML` package does not have a Windows binary. This means `sss` does not automatically install the `XML` package on your machine.  You will have to do this yourself.  To do this, you have two options:

1. Build and install XML on Windows

    Install and build the XML package yourself.  You can find instructions to do this at     http://cran.r-project.org/web/packages/XML/INSTALL


2. Get a copy of the XML binary from the BioConductor project

    Follow the instructions at http://www.bioconductor.org/install/.  In summary, run the following code:
    
    ```r
    source("http://bioconductor.org/biocLite.R")
    biocLite("EBImage")
    ```
    
  
  
