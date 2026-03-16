valid_links <- function(links){
  UseMethod("valid_links")
}

#' @export
valid_links.list <- function(links){

  tryCatch(

    error = function(cnd) isvalid <- FALSE,
    {
      lapply(links, function(link) rvest::read_html(link))
      isvalid <- TRUE
    }
  )

  isvalid

}
