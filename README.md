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
If you are unable to use install_github() try **Method 2:**
### Method 2: Manual Download and Install
1. **Download the Package**:  
Click on the package name below to download the source file:  
[getCPS 0.1.1](https://github.com/aisolori/getCPS/releases/download/getCPS-package/getCPS_0.1.1.tar.gz)

2. **Install the Package**:  
Once you've downloaded the `.tar.gz` file, open your R session or RStudio and use the following script to install:

```r
devtools::install_local("path_to_downloaded_file/getCPS_0.1.1.tar.gz")
```
`devtools::install_local()` will ensure that dependencies needed for the package are installed.

### Method 3: ###
If you are unable to use or install `devtools` the code below will also work, but will require manual installation of necesary dependencies. As you will likely hit errors similar to: `ERROR: dependencies 'cpsR', 'censusapi' are not available for package 'getCPS'`
```r
install.packages("path_to_downloaded_file/getCPS_0.1.1.tar.gz", repos = NULL, type = "source")
```
All necessary dependencies can be installed directly via cran by running the following script in R:
```r
# Function to install a package if it's not already installed
install_if_not_present <- function(package, version = NULL) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package)
    message(paste0("Package: ", package, " has been installed."))
  } else {
    message(paste0("Package: ", package, " already installed."))
  }
}

# List of packages from the "Imports" and "Depends" sections of the DESCRIPTION file
packages_to_install <- c(
  "data.table", "dplyr", "jsonlite", "magrittr", "tibble", "tidyr", "rvest",
  "furrr", "cpsR", "censusapi", "pbapply"
)

# Installing the packages
invisible(lapply(packages_to_install, install_if_not_present))

# Print a message indicating that package installations are completed
message("All necessary packages have been installed or were already present.")

```
## Further Documentation
https://aisolori.github.io/getCPS/
