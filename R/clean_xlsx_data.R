#' @title clean data
#' @return regData data frame
#' @export

clean_xlsx_data <- function() {

total <- data %>%
  dplyr::mutate(year = lubridate::year(FormDate),
                inj_iss = ifelse(FormTypeId == 1 & is.na(inj_iss), 0, inj_iss),
                inj_niss = ifelse(FormTypeId == 1 & is.na(inj_niss), 0, inj_niss),
                month = lubridate::floor_date(as.Date(FormDate), "month"),
                quarter = lubridate::quarter(FormDate, type = "date_first"))

return (total)

}


