#' @importFrom httr GET http_type modify_url user_agent
NULL

get_key <- function() {
  key <- Sys.getenv("CENSUS_API_KEY")

  if (key == "") {
    stop(
      "Census API key not found, supply with `census_api_key` argument or env var `CENSUS_API_KEY`",
      call. = FALSE
    )
  }

  key
}


is_string <- function(x) is.character(x) && length(x) == 1

check_key <- function(key) {
  if (!is_string(key) || key == "") {
    stop("`census_api_key` must be a non-empty string", call. = FALSE)
  }
}


test_key <- function(api_key) {
  ua = user_agent("getCPS")



  url <- httr::modify_url(
    url = "https://api.census.gov",
    path = paste("data", "2023", "cps", "basic", "jan", sep = "/"),
    query = list(get = "PEMLR", key = api_key)
  )

  resp<-GET(url,ua)

  if (http_type(resp) != "application/json") {
    stop("Invalid Census API key, provide key via `census_api_key` argument. \n To obtain an API key visit https://api.census.gov/data/key_signup.html ", call. = FALSE)
  }
}
