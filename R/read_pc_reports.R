read_pc_reports <- function(reports, ...){
  UseMethod("read_pc_reports")
}

#' @export
read_pc_reports.data.frame <- function(reports){

  report_details <- apply(reports, 1, function(report){


      report_name <- report["Name"]
      report_link <- report["Link"]

      pages_suffix <- c(report_page = "/report/", submission_page = "/submissions/")
      pages <- lapply(pages_suffix, function(page_suffix) paste0(report_link, page_suffix))
      isvalid <- valid_links(pages)

      if (isvalid){
        report_released_date <- read_pc_report_details(pages$report_page)
        submissions <- read_pc_report_submissions(pages$submission_page)

        final_report_details <- submissions %>%
          dplyr::mutate(
            Report_Name = report_name,
            Report_Link = report_link,
            Report_Released = report_released_date,
            Valid = TRUE
          )

        cli::cli_alert_success("{report_name} at {report_link} processed succesfully.")

      } else {

        final_report_details <- data.frame(
          Report_Name = report_name,
          Report_Link = report_link,
          Valid = FALSE
        )

        cli::cli_alert_danger("{report_name} at {report_link} does not have a submission and/or report page.")

      }

      final_report_details

  })

  all_report_details <- report_details %>%
    dplyr::bind_rows()

  all_report_details
}
