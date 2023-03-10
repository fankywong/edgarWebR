# User Agent for working with EDGAR
#
# We do this to handle edgar-specific error messages and ensure we set the UA
# and similar configuration properties once


edgar_agent <- function(useragent) {
  if(is.null(useragent) & !exists("EDGARWEBR_USER_AGENT")) {
    stop("give useragent")
  } else if (is.null(useragent)& exists("EDGARWEBR_USER_AGENT")) {
    useragent=httr::user_agent(EDGARWEBR_USER_AGENT)
  } 
  return(useragent)
}
edgar_GET <- function(href,useragent=NULL) {
  useragent=edgar_agent(useragent)
  res <- httr::GET(href, useragent)
  check_result(res)
  return(res)
}


edgar_POST <- function(href, body, encode = "json",useragent=NULL) {
  # res <- httr::POST(href, body = body, encode = encode, httr::verbose())
  useragent=edgar_agent(useragent)
  res <- httr::POST(href, body = body, encode = encode, useragent)
  check_result(res)
  return(res)
}

check_result <- function(res) {
  if (httr::status_code(res) == 200) {
    return()
  }
  text_content <- httr::content(res, "text")
  if (httr::status_code(res) == 403 && grepl("Undeclared Automated Tool", text_content, fixed = TRUE)) {
    stop(paste0(
      "EDGAR request blocked from Undeclared Automated Tool.\n",
      "Please visit https://www.sec.gov/developer for best practices.\n",
      "See https://mwaldstein.github.io/edgarWebR/index.html#ethical-use--fair-access for your responsibilities\n",
      "Consider also setting the environment variable 'EDGARWEBR_USER_AGENT"
    ))
  }
  stop(
       sprintf(
               "EDGAR request failed [%s]\n%s\n<%s>",
               httr::status_code(res),
               httr::content(res, "text"),
               res$url
               )
       )
}
