valid_links <- function(report_link){
  UseMethod("valid_links")
}

#' @export
valid_links.character <- function(report_link){



  pages_suffix <- c(report_page = "/report/",
                    submission_page = "/submissions/")
  page_links <- lapply(pages_suffix, function(page_suffix) paste0(report_link, page_suffix))
  valid_links <- test_links(page_links)

  valid_links



}

# HELPER -----------------------------------------------------

test_links <- function(links){

  # This function should output a valid link list of 0, if none are found.

  valid_links <- lapply(names(links), function(link_name){

    link <-  links[[link_name]]

    valid_link <- tryCatch(
          error = function(cnd){
            cli::cli_alert_danger("{link} failed. Testing alternatives.")
            if (link_name == "report_page"){
              other_report_page_suffixes <- c(strategy_page = "/strategy/")
              valid_link <- test_alternative_pages(link = link,
                                                   suffix_to_replace = "/report/",
                                                   replacement_suffixes = other_report_page_suffixes)
            } else if (link_name == "submission_page") {
              other_submission_page_suffixes <- c()
              # No known alternative names/links for submission pages.
              valid_link <- test_alternative_pages(link = link,
                                                   suffix_to_replace = "/submissions/",
                                                   replacement_suffixes = other_submission_page_suffixes)
            }

            return(valid_link)

            },
          {
            rvest::read_html(link)
            valid_link <- link
          }
        )

    valid_link
  })

  names(valid_links) <- names(links)
  valid_links

}

test_alternative_pages <- function(link,
                                   suffix_to_replace,
                                   replacement_suffixes){

  alternative_page_links <- lapply(replacement_suffixes, function(suffix) sub(suffix_to_replace, suffix, link))

  valid_link_flags <- vapply(alternative_page_links, function(alternative_page_link){

    tryCatch(
      error = function(cnd){
        cli::cli_alert_warning("{alternative_page_link} failed.")
        valid_link <- FALSE
      },
      {
        test <- rvest::read_html(alternative_page_link)
        valid_link <- TRUE
      }
    )

  }, logical(length(alternative_page_links)))

  if (!any(valid_link_flags)){
    cli::cli_alert_warning("Failed to read {link} alternatives.")
    valid_link <- vector(mode = "character", length = 0)
  } else {
    valid_link <- alternative_page_links[[valid_link_flags]]
    cli::cli_alert_success("{valid_link} alternative is successful.")
  }

  valid_link
}

