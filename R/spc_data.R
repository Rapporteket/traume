#'
#'
#'
#'
#'
#' @export

spc_data <- function(input, total_nasjonalt, lokal_enhet) {


  oppnaelse_teller_inne_spc <- total_nasjonalt %>%
    dplyr::filter(year %in% (as.numeric(input$selected_year_spc)-5):as.numeric(input$selected_year_spc), #input$selected_year,
                  HealthUnitShortName %in% c({{lokal_enhet}}, "Nasjonalt")) %>%
    dplyr::group_by(HealthUnitShortName, quarter) %>%
    dplyr::summarise(
      registrerte_teller = sum(difftime(CreationDate, FormDate, units="days") <= 90, na.rm = TRUE),
      ct_scans_teller = sum(ed_tta == 1 & ed_ct == 1, na.rm = TRUE),
      ct_iss_15_teller = sum(ed_tta == 1 & inj_iss > 15 & ed_ct == 1, na.rm = TRUE),
      ct_iss_4_teller = sum(ed_tta == 1 & inj_iss < 4 & ed_ct == 1, na.rm = TRUE),
      intuberte_prehospitalt_teller = sum(ed_tta == 1 & pre_gcs_sum < 9 & pre_intubated == 1, na.rm = TRUE),
      intuberte_mottak_teller = sum(ed_tta == 1 & ed_gcs_sum < 9 & ed_intubated == 2, na.rm = TRUE),
      mortalitet_teller = sum(res_survival == 1, na.rm = TRUE),
      rontgen_bekken_teller = sum(ed_tta == 1 & xray_pelv == 1, na.rm = TRUE),
      rontgen_bekken_iss15_teller = sum(ed_tta == 1 & inj_iss > 15 & xray_pelv == 1, na.rm = TRUE),
      rontgen_thorax_teller = sum(ed_tta == 1 & xray_chst == 1, na.rm = TRUE),
      rontgen_thorax_iss15_teller = sum(ed_tta == 1 & inj_iss > 15 & xray_chst == 1, na.rm = TRUE),
      .groups = "drop")

  oppnaelse_nevner_inne_spc <- total_nasjonalt %>%
    dplyr::filter(year %in% (as.numeric(input$selected_year_spc)-5):as.numeric(input$selected_year_spc),
                  HealthUnitShortName %in% c({{lokal_enhet}}, "Nasjonalt")) %>%
    dplyr::group_by(HealthUnitShortName, quarter) %>%
    dplyr::summarise(
      registrerte_nevner = sum(dplyr::n(),na.rm = TRUE),
      ct_scans_nevner = sum(ed_tta == 1 & ed_ct %in% 1:2, na.rm = TRUE),
      ct_iss_15_nevner = sum(ed_tta == 1 & inj_iss > 15 & ed_ct %in% 1:2, na.rm = TRUE),
      ct_iss_4_nevner = sum(ed_tta == 1 & inj_iss < 4 & ed_ct %in% 1:2, na.rm = TRUE),
      intuberte_prehospitalt_nevner = sum(ed_tta == 1 & pre_gcs_sum < 9 & pre_intubated %in% 1:2, na.rm = TRUE),
      intuberte_mottak_nevner = sum(ed_tta == 1 & ed_gcs_sum < 9 & ed_intubated %in% 2:3, na.rm = TRUE),
      mortalitet_nevner = sum(res_survival %in% 1:2, na.rm = TRUE),
      rontgen_bekken_nevner = sum(ed_tta == 1 & xray_pelv %in% 1:2, na.rm = TRUE),
      rontgen_bekken_iss15_nevner = sum(ed_tta == 1 & inj_iss > 15 & xray_pelv %in% 1:2, na.rm = TRUE),
      rontgen_thorax_nevner = sum(ed_tta == 1 & xray_chst %in% 1:2, na.rm = TRUE),
      rontgen_thorax_iss15_nevner = sum(ed_tta == 1 & inj_iss > 15 & xray_chst %in% 1:2, na.rm = TRUE),
      .groups = "drop"
    )

  spc_output <- dplyr::full_join(oppnaelse_teller_inne_spc, oppnaelse_nevner_inne_spc, by = c("HealthUnitShortName","quarter"))

  return(spc_output)
}
