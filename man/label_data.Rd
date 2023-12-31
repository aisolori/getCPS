% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Custom Functions.R
\name{label_data}
\alias{label_data}
\title{Label CPS Data Variables}
\usage{
label_data(cps_data, variable_list = NULL, date_column = NULL)
}
\arguments{
\item{cps_data}{A data frame containing CPS data. It must have a date column from which the year can be extracted.
By default, the function looks for a column named "DATE" (case-insensitive). If your date column has a different name,
specify it using the `date_column` argument.}

\item{variable_list}{A character vector specifying which variables to label. If not provided,
the function will label all variables excluding those in the default "variables_not_to_label" list.
variable_list is not required when calling label_data() immediately after get_cps_data_state() or
get_cps_data_all_states().}

\item{date_column}{A character scalar indicating the name of the date column in `cps_data` if it's different
from "DATE". This argument ensures the function can work even if the date column has different cases
(e.g., "Date", "date", "daTe") or entirely different names. If omitted, the function will look for a column
named "DATE" (case-insensitive).}
}
\value{
A data frame with labeled variables. If a label does not exist for a particular variable
        or year, the original value remains unchanged.
}
\description{
This function takes CPS data and labels its variables based on the metadata fetched from JSON files
hosted by the Census Bureau's API. It labels data across a range of years and returns a data frame
with labeled variables.
}
\examples{
\dontrun{
cps_sample_data <- get_cps_data_state(2022,"PEMLR")
labeled_data <- label_data(cps_sample_data)

custom_table <- get_cps_data_all_states(2022,"PEMLR") \%>\% mutate(random_col = "x")
labeled_data_2 <- label_data(custom_table, c("PEMLR"))

# If your date column is named differently:
cps_sample_data_with_custom_date <- get_cps_data_state(2022,"PEMLR")\%>\%rename(customDate = DATE)
labeled_data_3 <- label_data(cps_sample_data_with_custom_date, date_column = "customDate")
}

}
