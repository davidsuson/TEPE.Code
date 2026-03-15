read_pc_report_details <- function(report_details_page_link){


  report_details_page <- rvest::read_html(report_details_page_link)

  # Check if page does not exist and handle.

  report_released_date <- report_details_page %>%
    rvest::html_elements(xpath = "//label[contains(text(), 'Released')]
                         | //small[contains(text(), 'Released')]
                         | //p[contains(text(), 'Released')]") %>%
    rvest::html_text2() %>%
    gsub("Released", "", .) %>%
    str_remove_all(" ") %>%
    as.Date(date_string, format = "%d/%m/%Y")
  #This function should output a date or string if doesn't exist.

  if (length(report_released_date) == 0){

    report_released_date <- report_details_page %>%
      rvest::html_elements(xpath = "(//p[not(ancestor::nav)])[1]") %>%
      rvest::html_text2() %>%
      sub(".*released on", "", .) %>%
      trimws() %>%
      gsub("\\.", "", .) %>%
      as.Date(format = "%d %B %Y")

  }

  report_released_date

}
