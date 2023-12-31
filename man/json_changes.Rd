% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Custom Functions.R
\name{json_changes}
\alias{json_changes}
\title{Check for Changes in JSON Responses from Census's CPS Basic Microdata Across Years and Months}
\usage{
json_changes(variable_list, year_range)
}
\arguments{
\item{variable_list}{A list of variable names for which the JSON data will be fetched and compared.}

\item{year_range}{A vector of years over which the JSON data should be checked for changes.}
}
\value{
A data frame with columns 'variable', 'year', 'month', 'same_as_previous', and 'found'. Each row corresponds
        to a variable, a year, and a month. 'same_as_previous' is TRUE if the JSON data for that variable, year,
        and month is the same as the previous month's data, and FALSE otherwise. The 'found' column indicates
        if the data was found for that month.
}
\description{
This function checks whether JSON responses for specified variables in the CPS-Basic  Microdata change across a range of years and months in the Census API.
It fetches the JSON data for each variable for each year and month in the given range and compares it
to the previous month's JSON to detect changes.
}
\examples{
\dontrun{
year_seq <- c(2018:2020)
variables <- c("PEMLR", "PERRP")
json_changes(year_seq, variables)
}

}
