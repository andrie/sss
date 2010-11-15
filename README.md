#sss

The aim of sss is to provide a function to import triple-s XML files into r.

## The triple-s standard

triple-s is a standard to transfer survey data between applications.

## XML

This packages depends on the XML package.

Since the XML package does not have a Windows binary, sss does not import it.  You will have to do this yourself.

On Linux, this is straight-forward: get the package from CRAN and install in the usual way.

However, there is not a Windows binary of XML at CRAN.  This means you have two options:

### Build and install XML on Windows

The first option using Windows is to install and build the XML package yourself.  You can find instructions to do this here: 

http://cran.r-project.org/web/packages/XML/INSTALL


### Get a copy of the XML binary from the BioConductor project

Follow the instruction at http://www.bioconductor.org/install/

In summary, run the following code:
`source("http://bioconductor.org/biocLite.R")`
`biocLite("EBImage")`



