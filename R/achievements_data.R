#' Provide dataframe of fake registry data
#'
#' Provides a dataframe containing built-in data (and not a registry) for demo
#' purposes
#'
#' @return regData data frame
#' @export

achievements_data <- function(input, total_nasjonalt, lokal_enhet) {

  oppnaelse_teller <- total_nasjonalt %>%
    dplyr::filter(HealthUnitShortName %in% c({{lokal_enhet}}, "Nasjonalt"),
                  acc_overflyttet == 2,
                  ed_tta_notrauma == 2,
                  year == input$selected_year
                  ) %>%
    dplyr::group_by(HealthUnitShortName) %>%
    dplyr::summarise(
      registrerte = sum(difftime(CreationDate, FormDate, units="days") <= 90, na.rm = TRUE),
      ct_scans = sum(ed_tta == 1 & ed_ct == 1, na.rm = TRUE),
      ct_iss_15 = sum(ed_tta == 1 & inj_iss > 15 & ed_ct == 1, na.rm = TRUE),
      ct_iss_4 = sum(ed_tta == 1 & inj_iss < 4 & ed_ct == 1, na.rm = TRUE),
      intuberte_prehospitalt = sum(ed_tta == 1 & pre_gcs_sum < 9 & pre_intubated == 1, na.rm = TRUE),
      intuberte_mottak = sum(ed_tta == 1 & ed_gcs_sum < 9 & ed_intubated == 2, na.rm = TRUE),
      mortalitet = sum(res_survival == 1, na.rm = TRUE),
      rontgen_bekken = sum(ed_tta == 1 & xray_pelv == 1, na.rm = TRUE),
      rontgen_bekken_iss15 = sum(ed_tta == 1 & inj_iss > 15 & xray_pelv == 1, na.rm = TRUE),
      rontgen_thorax = sum(ed_tta == 1 & xray_chst == 1, na.rm = TRUE),
      rontgen_thorax_iss15 = sum(ed_tta == 1 & inj_iss > 15 & xray_chst == 1, na.rm = TRUE),
      .groups = 'drop') %>%
    dplyr::mutate(
      HealthUnitShortName = dplyr::case_when(
        HealthUnitShortName == "Nasjonalt" ~ "sykehus_3",
        .default = "sykehus_1"
      )
    )

  oppnaelse_nevner <- total_nasjonalt %>%
    dplyr::filter(HealthUnitShortName %in% c({{lokal_enhet}}, "Nasjonalt"),
                  acc_overflyttet == 2,
                  ed_tta_notrauma == 2,
                  year == input$selected_year
                  ) %>%
    dplyr::group_by(HealthUnitShortName) %>%
    dplyr::summarise(
      registrerte = sum(dplyr::n(),na.rm = TRUE),
      ct_scans = sum(ed_tta == 1 & ed_ct %in% 1:2, na.rm = TRUE),
      ct_iss_15 = sum(ed_tta == 1 & inj_iss > 15 & ed_ct %in% 1:2, na.rm = TRUE),
      ct_iss_4 = sum(ed_tta == 1 & inj_iss < 4 & ed_ct %in% 1:2, na.rm = TRUE),
      intuberte_prehospitalt = sum(ed_tta == 1 & pre_gcs_sum < 9 & pre_intubated %in% 1:2, na.rm = TRUE),
      intuberte_mottak = sum(ed_tta == 1 & ed_gcs_sum < 9 & ed_intubated %in% 2:3, na.rm = TRUE),
      mortalitet = sum(res_survival %in% 1:2, na.rm = TRUE),
      rontgen_bekken = sum(ed_tta == 1 & xray_pelv %in% 1:2, na.rm = TRUE),
      rontgen_bekken_iss15 = sum(ed_tta == 1 & inj_iss > 15 & xray_pelv %in% 1:2, na.rm = TRUE),
      rontgen_thorax = sum(ed_tta == 1 & xray_chst %in% 1:2, na.rm = TRUE),
      rontgen_thorax_iss15 = sum(ed_tta == 1 & inj_iss > 15 & xray_chst %in% 1:2, na.rm = TRUE),
      .groups = 'drop') %>%
    dplyr::mutate(
      HealthUnitShortName = dplyr::case_when(
        HealthUnitShortName == "Nasjonalt" ~ "sykehus_4",
        .default = "sykehus_2"
      )
    )


  oppnaelse <- dplyr::bind_rows(oppnaelse_teller,
                                oppnaelse_nevner) %>%
    tidyr::pivot_longer(-HealthUnitShortName) %>%
    tidyr::pivot_wider(names_from = HealthUnitShortName, values_from = value)

  oppnaelse <- oppnaelse %>%
    dplyr::mutate(PC_lokalt = sykehus_1 / sykehus_2,
                  PC_Nasjonalt = sykehus_3 / sykehus_4,
                  name = dplyr::case_when(
                    name == "registrerte" ~ "Registrert innen 30 dager",
                    name == "ct_scans" ~ "CT ved ankomst",
                    name == "ct_iss_15" ~ "CT ved ankomst ISS > 16",
                    name == "ct_iss_4" ~ "CT ved ankomst ISS < 4",
                    name == "intuberte_mottak" ~ "Intubert inhospitalt gitt GCS > 8",
                    name == "intuberte_prehospitalt" ~ "Intubert prehospitalt gitt GCS > 8",
                    name == "mortalitet" ~ "Død ved 30 dager",
                    name == "rontgen_bekken" ~ "Røntgen bekken ved ankomst",
             name == "rontgen_bekken_iss15" ~ "Røntgen bekken ved ankomst ISS > 16",
             name == "rontgen_thorax" ~ "Røntgen thorax ved ankomst",
             name == "rontgen_thorax_iss15" ~ "Røntgen thorax ved ankomst ISS > 16",
           )) %>%
    dplyr::select(-sykehus_3,-sykehus_4) %>%
    dplyr::rename(Indikatorer = name,
                  "Antall oppfyllt" = sykehus_1,
                  "Antall totalt" = sykehus_2,
                  "Prosent lokalt" = PC_lokalt,
                  "Prosent nasjonalt" = PC_Nasjonalt)
  #
  # colnames(oppnaelse) <- c("Indikatorer", "Antall oppfyllt",
  #                          "Antall totalt", "Prosent lokalt",
  #                          "Prosent nasjonalt")
  #
  #
  # oppnaelse$Indikatorer <- forcats::fct_rev(forcats::fct_inorder(oppnaelse$Indikatorer))

  return(oppnaelse)
}
