# getCPS
R Package for Pulling CPS Microdata from the Census API.

`getCPS` consists of a collection of wrappers around the `censusapi` & `cpsR` packages. It is currently in early development.

## Download & Install
### Method 1: Directly from GitHub using `devtools`

If you have the `devtools` package installed, you can directly install `getCPS` from GitHub:

```r
devtools::install_github("aisolori/getCPS", dependencies = TRUE)
```
This command will also ensure that all package dependencies are installed.
If you don't have `devtools` installed yet, you can easily get it from CRAN:

```r
install.packages("devtools")
```
### Method 2: Manual Download and Install
**Download the Package:**
Click on the package name below to download the source file:
getCPS 0.1.1

Install the Package:
After downloading the .tar.gz file, open your R session or RStudio and install the package using the following script:

1. **Download the Package**:  
Click on the package name below to download the source file:  
[getCPS 0.1.1](https://github.com/aisolori/getCPS/releases/download/getCPS-package/getCPS_0.1.1.tar.gz)

2. **Install the Package**:  
Once you've downloaded the `.tar.gz` file, open your R session or RStudio and use the following script to install:

```r
install.packages("path_to_downloaded_file/getCPS_0.1.1.tar.gz", repos = NULL, type = "source")


