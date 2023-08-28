#' @importFrom rvest html_node html_table read_html
NULL








#' Retrieve a Table of Available CPS Basic Variables
#'
#' This function retrieves a table of available Current Population Survey (CPS) Basic variables for a given year and month.
#' It scrapes the data from the official Census API webpage.
#'
#' @param year Character string specifying the desired year (default is "2023").
#' @param month A character string specifying the desired month (default is "jan").
#'   It can be provided in lowercase three-letter format (e.g., "jan", "feb", etc.), as a full month name in any case (e.g., "January", "JANUARY", "january"),
#'   or as a numeric value (1 for January, 2 for February, etc.).
#'
#' @return A data frame containing the table of available CPS Basic variables for the specified year and month.
#'
#' @seealso [Census API Documentation](https://api.census.gov/data/)
#'
#' @examples
#' \dontrun{
#'   cps_data <- cps_var_table(year = "2021", month = "apr")
#'   head(cps_data)
#'}
#'
#' @export
cps_var_table <- function(year = "2023", month = "jan") {

  # If month is numeric
  if(is.numeric(month)) {
    month <- month.abb[month]
    if(is.na(month)) {
      stop("Unable to determine month. \n Please set `month` argument to one of the following values: \n jan,feb,mar,apr,may,jun,jul,aug,sep,oct,nov,dec")
    }
  } else if (tolower(month) %in% tolower(month.name)) { # If month is a full month name
    month <- month.abb[match(tolower(month), tolower(month.name))]
  }

  url <- paste0("https://api.census.gov/data/", as.character(year), "/cps/basic/", tolower(month), "/variables.html")

  table_data <- url %>%
    read_html() %>%
    html_node(xpath = '/html/body/table') %>%
    html_table()

  table_data <- table_data[2:nrow(table_data), 1:8]

  return(table_data)
}
