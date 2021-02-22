#' Download rda files from github
#' @param url URL of the rdata file.
#' @importFrom httr GET stop_for_status content
#' @keywords internal
#' @export
download_data_github <- function(url) {
  new_url <- gsub("https://github", "https://raw.github", url)
  new_url <- gsub("/blob", "", new_url)

  temp_file <- tempfile()
  on.exit(unlink(temp_file))

  request <- httr::GET(new_url)
  httr::stop_for_status(request)
  writeBin(httr::content(request, type = "raw"), temp_file)


  x <- load(temp_file)
  y <- get(x)
  # Remove the old object since you've stored it in y
  rm(x)
  y
}


#' Download circus models
#'
#' @param name Model name.
#' @export
download_model <- function(name) {
  url <- paste0(
    "https://github.com/easystats/circus/blob/master/data/",
    name,
    ".rda"
  )

  download_data_github(url)
}
