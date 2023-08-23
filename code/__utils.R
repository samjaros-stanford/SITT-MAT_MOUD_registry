
require(jsonlite)
require(RCurl)
require(readr)
get_redcap_report <- function(redcap_project, redcap_report_id) {
  result <- postForm(
    "https://redcap.stanford.edu/api/",
    token=fromJSON(here::here("secrets.json"))[[paste0("redcap_",redcap_project,"_key")]],
    content='report',
    format='csv',
    report_id=redcap_report_id
  )
  return(read_csv(I(result), show_col_types=F))
}
