#' @importFrom data.table data.table rbindlist
#' @importFrom dplyr arrange filter left_join mutate rename row_number select
#' @importFrom jsonlite fromJSON
#' @importFrom lubridate ymd year month is.Date
#' @importFrom magrittr %>%
#' @importFrom purrr map_dfr imap_dfr
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_longer pivot_wider
NULL


#' Obtain CPS Data for All States
#'
#' Fetches CPS (Current Population Survey) data across all states from the Census API.
#'
#' @param year_range A vector of years for which data is to be fetched.
#' @param variable_list A list of variables to be included in the fetched data.
#' @param state_filter A character vector specifying which states' data should be retrieved.
#'        If set to FALSE (default), data for all states will be fetched.
#' @param census_api_key A character string representing the user's API key for accessing the Census API.
#'        By default, it retrieves the API key set as the "CENSUS_API_KEY" environment variable.
#'
#' @return A data frame containing the fetched CPS data.
#'
#' @examples
#' \dontrun{
#' get_cps_data_all_states(c(2022, 2023), c("PESEX", "PRTAGE"), state_filter = "32")
#' }
#'
#' @export
get_cps_data_all_states<- function(year_range, variable_list, state_filter = FALSE,
                                   census_api_key = get_key()){
  check_key(census_api_key)
  test_key(census_api_key)
  plan(multisession)
  weight_list<- c()

  print("Getting JSONs")
  for (var in variable_list) {
    var_json <- fromJSON(paste0("https://api.census.gov/data/2023/cps/basic/feb/variables/", toupper(var), ".json"))
    if ("suggested-weight" %in% names(var_json)) {
      weight_var <- var_json$`suggested-weight`
      if (!(weight_var %in% variable_list)) {
        variable_list <- c(variable_list, weight_var)
        weight_list <- c(weight_list, weight_var)
      }
    }
  }


  months = c(1:12)
  variable_list<-c(variable_list)
  print("Getting Data...")
  cps_data <- map_dfr(year_range, function(year){

    # Conditioning to add state column depenidng on year
    if(as.integer(year)>2023){
      variable_list<-c(variable_list, "STATE")
    }else{
      variable_list<-c(variable_list,"gestfips")
    }
    print(paste("Obtaining Data For Year", year))
    #Loop through months in parallel
    data_per_year <- future_map_dfr(months, function(month){
      tryCatch({
        data <- suppressMessages(get_basic(
          year = as.integer(year),
          month = month,
          vars = variable_list,
          convert = FALSE,
          key = census_api_key)) %>%
          mutate(date = ymd(paste(year,month,"01",sep = "-")))



        # Apply state filter only if state_filter is not FALSE
        if (!isFALSE(state_filter)) {
          state_filter<-as.numeric(state_filter)%>%
            as.character()

          # Removing padding for state fips codes

          if(as.integer(year)>2023){
            data <- filter(data, state %in% state_filter)
          }else{
            data <- filter(data, gestfips %in% state_filter)%>%
              rename(state = gestfips)
          }
        }

        return(data)
      }, error = function(e) {
        print(paste("Data not available for", month, year))
        return(NULL)
      })
    },.progress = TRUE)
    return(data_per_year)
  })
  names(cps_data) <- toupper(names(cps_data))

  return(cps_data)
}


#' Obtain CPS Data for a Specific State
#'
#' This function fetches CPS data for a specific state from the Census API.
#'
#' @param year_range A vector of years for which data is to be fetched.
#' @param variable_list A list of variables to be included in the fetched data.
#' @param census_api_key Your Census API key. Defaults to the key stored in the system environment "CENSUS_API_KEY".
#' @param state_code A character string specifying the state FIPS code for which data should be fetched.
#'        Default is "32", which is the FIPS code for Nevada. Only accepts one state code at a time.
#'
#' @return A data frame containing the fetched CPS data for the specified state.
#'
#' @examples
#' \dontrun{
#' get_cps_data_state(c(2022, 2023), c("PESEX", "PRTAGE"))
#' }
#'
#' @export
get_cps_data_state<- function(year_range,variable_list,census_api_key = get_key() ,state_code = "32") {

  check_key(census_api_key)
  test_key(census_api_key)

  weight_list = c()
  plan(multisession)

  for (var in variable_list) {
    var_json <- fromJSON(paste0("https://api.census.gov/data/2023/cps/basic/feb/variables/", var, ".json"))
    if ("suggested-weight" %in% names(var_json)) {
      weight_var <- var_json$`suggested-weight`
      if (!(weight_var %in% variable_list)) {
        variable_list <- c(variable_list, weight_var)
        weight_list <- c(weight_list, weight_var)
      }
    }
  }

  cps_data <- data.table()

  months_list <- tolower(month.abb)

  cps_data <- future_map_dfr(year_range, function(year) {
    data_per_year <- future_map_dfr(months_list, function(month) {
      api_name_constructor <- paste0("cps/basic/", month)
      tryCatch({
        df <- getCensus(api_name_constructor,
                        vintage = year,
                        region = paste0("state:",state_code),
                        vars = variable_list,
                        key = census_api_key) %>%
          mutate(DATE =  ymd(paste(year,month,"01",sep = "-")))

        return(df)
      }, error = function(e) {
        print(paste("Data not available for", month, year))
        return(NULL)
      })
    }, .progress = TRUE)
    return(data_per_year)
  }, .progress = TRUE)


  return(cps_data)
}

#' Check for Changes in JSON Responses from Census's CPS Basic Microdata Across Years and Months
#'
#' This function checks whether JSON responses for specified variables in the CPS-Basic  Microdata change across a range of years and months in the Census API.
#' It fetches the JSON data for each variable for each year and month in the given range and compares it
#' to the previous month's JSON to detect changes.
#'
#' @param variable_list A list of variable names for which the JSON data will be fetched and compared.
#' @param year_range A vector of years over which the JSON data should be checked for changes.
#'
#' @return A data frame with columns 'variable', 'year', 'month', 'same_as_previous', and 'found'. Each row corresponds
#'         to a variable, a year, and a month. 'same_as_previous' is TRUE if the JSON data for that variable, year,
#'         and month is the same as the previous month's data, and FALSE otherwise. The 'found' column indicates
#'         if the data was found for that month.
#'
#' @examples
#' \dontrun{
#' year_seq <- c(2018:2020)
#' variables <- c("PEMLR", "PERRP")
#' json_changes(year_seq, variables)
#' }
#'
#' @export
json_changes <- function(variable_list, year_range) {

  process_variable_year <- function(var, year) {

    results_list <- vector("list", 12)
    months <- tolower(month.abb)

    prev_json <- tryCatch({
      fromJSON(paste0("https://api.census.gov/data/", year - 1, "/cps/basic/", months[12], "/variables/", var, ".json"))
    }, error = function(e) {
      NULL
    },warning=function(w){NULL})

    for (month in 1:12) {
      prev_month <- ifelse(month == 1, 12, month - 1)
      prev_year <- ifelse(month == 1, year - 1, year)

      var_json <- tryCatch({
        fromJSON(paste0("https://api.census.gov/data/", year, "/cps/basic/", months[month], "/variables/", var, ".json"))
      }, error = function(e) {
        NULL
      },warning=function(w){NULL})

      if (!is.null(prev_json) & !is.null(var_json)) {
        prev_items_str <- sort(paste(names(prev_json$values$item), prev_json$values$item, sep = ":"))
        var_items_str <- sort(paste(names(var_json$values$item), var_json$values$item, sep = ":"))
        same_as_previous <- identical(prev_items_str, var_items_str)
        results_list[[month]] <- data.frame(variable = var, year = year, month = month, same_as_previous = same_as_previous, found = TRUE)
      } else if (!is.null(var_json)) {
        results_list[[month]] <- data.frame(variable = var, year = year, month = month, same_as_previous = NA, found = TRUE)
      } else {
        results_list[[month]] <- data.frame(variable = var, year = year, month = month, same_as_previous = NA, found = FALSE)
      }

      prev_json <- var_json
    }

    return(rbindlist(results_list))
  }

  expand_grid_data <- expand.grid(variable = variable_list, year = year_range)

  plan(multisession)

  results <- future_map2_dfr(expand_grid_data$variable, expand_grid_data$year, process_variable_year, .progress = TRUE)

  # results<-results%>%
  #   filter(same_as_previous %in% c(NA,FALSE) | found == FALSE)

  if(nrow(results) == 0){
    return(print(paste0("No changes detected")))
  }

  return(results)
}
#' Label CPS Data Variables
#'
#' This function takes CPS data and labels its variables based on the metadata fetched from JSON files
#' hosted by the Census Bureau's API. It labels data across a range of years and returns a data frame
#' with labeled variables.
#'
#' @param cps_data A data frame containing CPS data. It must have a date column from which the year can be extracted.
#'        By default, the function looks for a column named "DATE" (case-insensitive). If your date column has a different name,
#'        specify it using the `date_column` argument.
#' @param variable_list A character vector specifying which variables to label. If not provided,
#'        the function will label all variables excluding those in the default "variables_not_to_label" list.
#'        variable_list is not required when calling label_data() immediately after get_cps_data_state() or
#'        get_cps_data_all_states().
#' @param date_column A character scalar indicating the name of the date column in `cps_data` if it's different
#'        from "DATE". This argument ensures the function can work even if the date column has different cases
#'        (e.g., "Date", "date", "daTe") or entirely different names. If omitted, the function will look for a column
#'        named "DATE" (case-insensitive).
#'
#' @return A data frame with labeled variables. If a label does not exist for a particular variable
#'         or year, the original value remains unchanged.
#'
#' @examples
#' \dontrun{
#' cps_sample_data <- get_cps_data_state(2022,"PEMLR")
#' labeled_data <- label_data(cps_sample_data)
#'
#' custom_table <- get_cps_data_all_states(2022,"PEMLR") %>% mutate(random_col = "x")
#' labeled_data_2 <- label_data(custom_table, c("PEMLR"))
#'
#' # If your date column is named differently:
#' cps_sample_data_with_custom_date <- get_cps_data_state(2022,"PEMLR")%>%rename(customDate = DATE)
#' labeled_data_3 <- label_data(cps_sample_data_with_custom_date, date_column = "customDate")
#' }
#'
#' @export
label_data <- function(cps_data, variable_list = NULL, date_column = NULL) {
  # Helper function to check if a column is of Date type
  check_date_type <- function(column_name, data) {
    if (is.Date(data[[column_name]])) {
      return(TRUE)
    } else {
      stop(paste0(column_name, " column needs to be converted to <date> data type, its current data type is: ",
                  class(data[[column_name]]), '\n')
      )
    }
    }

  # If date_column is NULL or empty, try to find a 'DATE' column regardless of its case
  if (is.null(date_column)) {
    potential_date_column <- names(cps_data)[which(toupper(names(cps_data)) == "DATE")]

    if (length(potential_date_column) > 0) {
      date_column <- potential_date_column[1]  # assign the first match
    } else {
      stop("No valid date column found")
    }
  }

  # Now check the date_column for Date type
  if (check_date_type(date_column, cps_data)) {
    cps_data$year <- as.character(year(cps_data[[date_column]]))
    year_range <- year(cps_data[[date_column]]) %>% unique()
  }



  #cps_data$year <- as.character(year(cps_data$DATE))
 # year_range <- year(cps_data$DATE) %>% unique()

  if(length(variable_list) == 0){
    variables_not_to_label <- c("HWHHWGT", "PWSSWGT", "DATE", "year", "state")
  variable_list <- names(cps_data)[!(names(cps_data)) %in% variables_not_to_label]}

  cat("Downloading Labels from JSON files\n")
  variable_labels_list <- pblapply(variable_list, function(variable) {
    variable_labels_per_year <- list()

    for (year in year_range) {
      var_json <- fromJSON(paste0("https://api.census.gov/data/", year, "/cps/basic/feb/variables/", variable, ".json"))
      if (!("is-weight" %in% names(var_json)) & ("values" %in% names(var_json))) {
        labels <- var_json$values$item
        pattern <- "^0[0-9]+$"
        var_names <- names(labels)
        if (any(grepl(pattern, var_names))) {
          new_var_names <- sub("^0+", "", var_names)
          names(labels) <- new_var_names
        }
        variable_labels_per_year[[variable]][[as.character(year)]] <- list(labels = labels)
      }
    }
    return(variable_labels_per_year)
  }, cl = 2)


  cat("Labels obtained\n")

  variable_labels_list <- do.call(c, variable_labels_list)
  variable_labels_df <- purrr::imap_dfr(variable_labels_list, function(labels, variable) {
    purrr::map_dfr(names(labels), function(year) {
      if (is.null(labels[[year]]$labels)) {
        tibble::tibble(variable = variable, year = year, code = NA_character_, label = NA_character_)
      } else {
        labels_unlisted <- unlist(labels[[year]]$labels)
        tibble::tibble(variable = variable, year = year, code = names(labels_unlisted), label = labels_unlisted)
      }
    })
  }, .id = "variable")

  variable_labels_df <- variable_labels_df %>%
    filter(!is.na(code))

  variables_to_label <- variable_labels_df$variable %>%
    unique()

  cps_data_labeled <- list()
  cat("Labeling Data...\n")

  cps_data_labeled <- pblapply(year_range, function(year) {

    cps_data_year <- cps_data[cps_data$year == year, ]

    cps_data_long <- cps_data_year %>%
      mutate(row_id = row_number()) %>%
      mutate(across(all_of(variables_to_label), as.character)) %>%
      pivot_longer(
        cols = all_of(variables_to_label),
        names_to = "variable",
        values_to = "code"
      )%>%
      mutate(code = as.character(code))

    cps_data_labeled_long <- cps_data_long %>%
      left_join(variable_labels_df, by = c("year", "variable", "code"))

    cps_data_year_labeled <- cps_data_labeled_long %>%
      pivot_wider(
        names_from = "variable",
        values_from = c("code", "label")
      ) %>%
      select(-c(row_id, year))

    cps_data_year_labeled <- cps_data_year_labeled[,colSums(is.na(cps_data_year_labeled)) != nrow(cps_data_year_labeled)]

    return(cps_data_year_labeled)
  }, cl = 2)

  cat("Labeling Complete!\n")

  return(rbindlist(cps_data_labeled, fill =TRUE))
}


#' Retrieve Suggested Weight for a Given CPS Variable
#'
#' This function fetches the suggested weight for a given CPS variable
#' from the U.S. Census Bureau's API for a specified year.
#'
#' @param variable Character. The census variable for which the suggested weight is to be fetched.
#' @param year Character. The year for which to fetch the data. Default is "2023". Weights don't tend to change over the years.
#'
#' @return If the variable has a suggested weight, it prints the weight. If not, a message is shown indicating that the variable does not have a suggested weight.
#'
#' @examples
#' suggested_weight("PEMLR")
#'
#' @seealso
#' \url{https://api.census.gov/data.html}
#'
#' @export
suggested_weight<-function(variable, year = "2023"){
  variable = as.character(variable)
  var_json <- tryCatch({fromJSON(paste0("https://api.census.gov/data/", year, "/cps/basic/feb/variables/", toupper(variable), ".json"))},
                       error = function(e){message(paste0("Unable to establish connection\n", toupper(variable), " might not exist for year ",year))
                         return(NULL)},
                       warning=function(w){NULL})
  if ("suggested-weight" %in% names(var_json)) {
    weight_var <- var_json$`suggested-weight`
    return(print(paste("Suggested weight for",variable, "is", weight_var)))
  }else if (!is.null(var_json)){message(paste(variable, "does not have suggested weight"))}
}


#' Function to get CPS Basic Variable Labels
#'
#' This function retrieves the labels for a given CPS basic variable for the specified year(s).
#' It constructs a list of labels fetched from the Census API for the given variable(s) and year(s).
#'
#' @param var_name A character vector of variable name(s) to get labels for.
#' @param year_range A character vector specifying the year(s) for which labels are required, default is "2023".
#'
#' @return If a single year and variable are provided, the function returns a data frame.
#'         For multiple years or variables, it returns a nested list of data frames.
#'
#' @examples
#' \dontrun{
#'   labels <- get_labels("PEMLR")
#'   print(labels)
#'
#'   labels_mult_years <- get_labels("PERRP", year_range = c(2019:2020))
#'   print(labels_mult_years)
#' }
#'
#' @export
get_labels<-function(var_name, year_range = "2023"){

  # Check if year_range is using its default value
  if (identical(year_range, "2023")) {
    message("year_range argument defaulting to 2023")
  }

  #creates emtpy list that will store var_labels
  var_labels_per_year<-list()

  for (year in year_range) {
    #var_json is a nested list of the response we get from the api
    var_json <- fromJSON(
      paste0("https://api.census.gov/data/", year, "/cps/basic/jan/variables/", toupper(var_name), ".json"))

    #Construct df from var_json
    var_labels_df<-data.frame(
      code = names(var_json$values$item),
      label = unlist(var_json$values$item, use.names = FALSE)
    )

    var_labels_per_year[[var_name]][[as.character(year)]]<-var_labels_df%>%arrange(code)
  }
  #If only one year and one variable are provided returns a simple frame
  if (length(var_name)==1 & length(year_range) == 1){
    return(var_labels_per_year[[var_name]][[year_range]])
  }else{
    return(var_labels_per_year)}
}

