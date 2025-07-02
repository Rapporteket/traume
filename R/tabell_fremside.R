#'
#'
#'
#'
#'
#' @return
#' @export

tabell_fremside <- function(total) {
  antall_month <- total %>%
    filter(
      HealthUnitShortName %in% c("Kristiansand"),
      month >= floor_date(Sys.Date() - months(9), "month"),
      acc_overflyttet == 2,
      !is.na(month)
    ) %>%
    group_by(month) %>%
    summarise(
      registrert = sum(n(), na.rm = TRUE),
      traumeteam = sum(ed_tta == 1, na.rm = TRUE),
      undertriage = sum(inj_iss >= 15, ed_tta == 2, na.rm = TRUE),
      utskrevet_dod = sum(hosp_dischg_dest == 3, na.rm = TRUE),
      dod_etter_30_dg = sum(res_survival == 1, na.rm = TRUE),
      nodintervensjon = sum(ed_emerg_proc != -1 & ed_emerg_proc != 999, na.rm = TRUE),
      barn_registrert = sum(PatientAge < 18, na.rm = TRUE),
      over_65 = sum(PatientAge > 65, na.rm = TRUE),
      penetrerende_skader = sum(inj_penetrating == 1, na.rm = TRUE),
      CT = sum(ed_ct == 1, na.rm = TRUE),
      rontgen_thorax = sum(xray_chst == 1, na.rm = TRUE),
      rontgen_bekken = sum(xray_pelv == 1, na.rm = TRUE),
      Fast = sum(ed_ultralyd_fast == 1, na.rm = TRUE),
      ISS_over_16 = sum(inj_iss >= 16, na.rm = TRUE),
      NISS_over_16 = sum(inj_niss >= 16, na.rm = TRUE),
      overflyttet_traumesenter = sum(hosp_dischg_dest == 4, na.rm = TRUE),
      overflyttet_andre_sykehus = sum(hosp_dischg_dest %in% c(5,6,8), na.rm = TRUE),
      utskrevet_rehab = sum(hosp_dischg_dest %in% c(2,7), na.rm = TRUE),
      .groups = 'drop'
    ) %>% arrange(
      desc(month)
    ) %>%
    mutate(
      month = format(month, "%Y-%m"),
      month = as.character(month)
    )

  antall_total <- total %>%
    filter(
      HealthUnitShortName %in% c("Kristiansand"),
      month >= floor_date(Sys.Date() - years(2), "year"),
      !is.na(year)
    ) %>%  # Remove rows where time_period is NA
    group_by(year) %>%
    summarise(
      registrert = sum(n(), na.rm = TRUE),
      traumeteam = sum(ed_tta == 1, na.rm = TRUE),
      undertriage = sum(inj_iss >= 15, ed_tta == 2, na.rm = TRUE),
      utskrevet_dod = sum(hosp_dischg_dest == 3, na.rm = TRUE),
      dod_etter_30_dg = sum(res_survival == 1, na.rm = TRUE),
      nodintervensjon = sum(ed_emerg_proc != -1 & ed_emerg_proc != 999, na.rm = TRUE),
      barn_registrert = sum(PatientAge < 18, na.rm = TRUE),
      over_65 = sum(PatientAge > 65, na.rm = TRUE),
      penetrerende_skader = sum(inj_penetrating == 1, na.rm = TRUE),
      CT = sum(ed_ct == 1, na.rm = TRUE),
      rontgen_thorax = sum(xray_chst == 1, na.rm = TRUE),
      rontgen_bekken = sum(xray_pelv == 1, na.rm = TRUE),
      Fast = sum(ed_ultralyd_fast == 1, na.rm = TRUE),
      ISS_over_16 = sum(inj_iss >= 16, na.rm = TRUE),
      NISS_over_16 = sum(inj_niss >= 16, na.rm = TRUE),
      overflyttet_traumesenter = sum(hosp_dischg_dest == 4, na.rm = TRUE),
      overflyttet_andre_sykehus = sum(hosp_dischg_dest %in% c(5,6,8), na.rm = TRUE),
      utskrevet_rehab = sum(hosp_dischg_dest %in% c(2,7), na.rm = TRUE),
      .groups = 'drop'
    ) %>% arrange(
      -year
    ) %>% mutate(
      month = as.character(year)
    ) %>% select(
      -year
    ) %>% bind_rows(
      antall_month,.
    ) %>% pivot_longer(
      cols = -month, names_to = "Kategori", values_to = "value"
      ) %>% pivot_wider(
      names_from = month, values_from = value
      )

  antall_total <- antall_total %>%
    mutate(Kategori = case_when(
      Kategori == "registrert" ~ "Registrert i registeret",
      Kategori == "traumeteam" ~ "Motatt med traumeteam",
      Kategori == "undertriage" ~ "Undertriagerte",
      Kategori == "utskrevet_dod" ~ "Utskrevet død",
      Kategori == "dod_etter_30_dg" ~ "Død etter 30 dager",
      Kategori == "nodintervensjon" ~ "Utført nødintervensjon",
      Kategori == "barn_registrert" ~ "Barn registrert",
      Kategori == "over_65" ~ "Over 65 registrert",
      Kategori == "penetrerende_skader" ~ "Registrert med penetrerende skade",
      Kategori == "CT" ~ "CT utført",
      Kategori == "rontgen_thorax" ~ "Røntgen thorax utført",
      Kategori == "rontgen_bekken" ~ "Røntgen bekken utført",
      Kategori == "Fast" ~ "Fast utført",
      Kategori == "ISS_over_16" ~ "Pasienter med ISS-score på 16 eller over",
      Kategori == "NISS_over_16" ~ "Pasienter med NISS-score på 16 eller over",
      Kategori == "overflyttet_traumesenter" ~ "Overflyttet til traumesenter",
      Kategori == "overflyttet_andre_sykehus" ~ "Overflyttet til andre sykehus",
      Kategori == "utskrevet_rehab" ~ "Utskrevet til rehabelitering",
           ))




  return(antall_total)
}
