#' Provide dataframe of fake registry data
#'
#' Provides a dataframe containing built-in data (and not a registry) for demo
#' purposes
#'
#' @return regData data frame
#' @export

getRegData <- function() {
  data <- rapbase::loadRegData("traume", "SELECT * FROM data;")

  variabler_skade <- c(
    "HovedskjemaGUID", "inj_iss", "inj_niss"
  )

  variabler_PROM_6m <- c(
    "HovedskjemaGUID", "PROM_smerte_6m"
  )

  variabler_PROM_12m <- c(
    "HovedskjemaGUID", "PROM_smerte_12m"
  )

  #colnames(prom_6mnd) <- paste(colnames(prom_6mnd),"6m",sep="_")
  #colnames(prom_12mnd) <- paste(colnames(prom_12mnd),"12m",sep="_")

  skade <- data %>% filter(FormTypeId == 9) %>% dplyr::distinct(HovedskjemaGUID, .keep_all = TRUE) %>% dplyr::select(dplyr::all_of(variabler_skade))
  traume <- data %>% filter(FormTypeId == 1) %>% dplyr::distinct(SkjemaGUID, .keep_all = TRUE) %>% dplyr::select(!dplyr::all_of(variabler_skade) & !dplyr::all_of(variabler_PROM_6m) & !dplyr::all_of(variabler_PROM_12m), HovedskjemaGUID)
  prom_6mnd <- data %>% filter(FormTypeId == 12) %>% dplyr::distinct(SkjemaGUID, .keep_all = TRUE) %>% dplyr::select(dplyr::all_of(variabler_PROM_6m))
  prom_12mnd <- data %>% filter(FormTypeId == 14) %>% dplyr::distinct(SkjemaGUID, .keep_all = TRUE) %>% dplyr::select(dplyr::all_of(variabler_PROM_12m))

  total <- dplyr::left_join(traume, skade, by= c("SkjemaGUID" = "HovedskjemaGUID")) %>%
    dplyr::left_join(prom_6mnd, by= c("SkjemaGUID" = "HovedskjemaGUID")) %>%
    dplyr::left_join(prom_12mnd, by= c("SkjemaGUID" = "HovedskjemaGUID")) %>%
    filter(!is.na(HealthUnitShortName),
           HealthUnitShortName != "") %>%
    mutate(year = year(FormDate),
           inj_iss = ifelse(FormTypeId == 1 & is.na(inj_iss), 0, inj_iss),
           inj_niss = ifelse(FormTypeId == 1 & is.na(inj_niss), 0, inj_niss),
           month = floor_date(as.Date(FormDate), "month"),
           quarter = quarter(FormDate, type = "date_first"))

  return(total)
}
