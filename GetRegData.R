#' Provide dataframe of fake registry data
#'
#' Provides a dataframe containing built-in data (and not a registry) for demo
#' purposes
#'
#' @return regData data frame
#' @export

getRegData <- function() {


  data1 <- read.csv2(".csv")
  data2 <- read.csv2(".csv")
  data3 <- read.csv2(".csv")
  data4 <- read.csv2(".csv")
  data5 <- read.csv2(".csv")
  data6 <- read.csv2(".csv")

  data <- dplyr::bind_rows(data1,data2,data3,data4,data5,data6)

  vars_skjema <- readxl::read_xlsx("Felles_variabler_skjema.xlsx")

  skade <- data %>% filter(FormTypeId == 9) %>% distinct(HovedskjemaGUID, .keep_all = TRUE) %>% select(all_of(na.exclude(vars_skjema$skade)))
  traume <- data %>% filter(FormTypeId == 1) %>% distinct(SkjemaGUID, .keep_all = TRUE) %>% select(!all_of(na.exclude(vars_skjema$skade)) & !all_of(na.exclude(vars_skjema$prom)), HovedskjemaGUID)
  prom_6mnd <- data %>% filter(FormTypeId == 12) %>% distinct(SkjemaGUID, .keep_all = TRUE) %>% select(all_of(na.exclude(vars_skjema$prom)))
  prom_12mnd <- data %>% filter(FormTypeId == 14) %>% distinct(SkjemaGUID, .keep_all = TRUE) %>% select(all_of(na.exclude(vars_skjema$prom)))

  colnames(prom_6mnd) <- paste(colnames(prom_6mnd),"6m",sep="_")
  colnames(prom_12mnd) <- paste(colnames(prom_12mnd),"12m",sep="_")

  total <- dplyr::left_join(traume, skade, by= c("SkjemaGUID" = "HovedskjemaGUID")) %>%
    dplyr::left_join(prom_6mnd, by= c("SkjemaGUID" = "HovedskjemaGUID_6m")) %>%
    dplyr::left_join(prom_12mnd, by= c("SkjemaGUID" = "HovedskjemaGUID_12m")) %>%
    filter(!is.na(HealthUnitShortName),
           HealthUnitShortName != "") %>%
    mutate(year = year(FormDate),
           inj_iss = ifelse(FormTypeId == 1 & is.na(inj_iss), 0, inj_iss),
           inj_niss = ifelse(FormTypeId == 1 & is.na(inj_niss), 0, inj_niss),
           month = floor_date(as.Date(FormDate), "month"),
           quarter = quarter(FormDate, type = "date_first"))

  return(total)
}
