#'
#'
#'
#'
#'
#' @title figur registreringsoversikt
#' @export


#---- Registrering figur ----

plot_registrering <- function(input, total, userUnitId) {
  if (input$time_period == "ar") {
    trauma_filtered <- total %>%
      dplyr::filter(year >= lubridate::year(Sys.Date()) - 5,
                    UnitId == {{userUnitId}}) %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(total_traumas = dplyr::n())


  } else {
    trauma_filtered <- total %>%
      dplyr::filter(month >= lubridate::floor_date(Sys.Date() - months(12), "month"),
                    UnitId == {{userUnitId}}) %>%
      dplyr::group_by(month) %>%
      dplyr::summarise(total_traumas = dplyr::n())
  }

  return(trauma_filtered)
}

#---- Registrering tabell ----

#' @title tabell registreringsoversikt
#' @export

table_registrering <- function(input, total) {
  if (input$time_period == "ar") {
    trauma_filtered <- total %>%
      dplyr::filter(year >= lubridate::year(Sys.Date()) - 5,
             acc_overflyttet == 2) %>%
      dplyr::group_by(HealthUnitShortName, year) %>%
      dplyr::summarise(Antall = dplyr::n()) %>%
      dplyr::arrange(desc(year)) %>%
      tidyr::pivot_wider(names_from = year, values_from = Antall, values_fill = 0) %>%
      dplyr::arrange(HealthUnitShortName) %>%
      dplyr::rename(Sykehus = HealthUnitShortName)

  } else {
    trauma_filtered <- total %>%
      dplyr::filter(month >= lubridate::floor_date(Sys.Date() - months(9), "month"),
             acc_overflyttet == 2) %>%
      dplyr::group_by(HealthUnitShortName, month) %>%
      dplyr::summarise(Antall = dplyr::n()) %>%
      dplyr::mutate(month = format(month, "%Y-%m")) %>%
      dplyr::arrange(desc(month)) %>%
      tidyr::pivot_wider(names_from = month, values_from = Antall, values_fill = 0) %>%
      dplyr::arrange(HealthUnitShortName) %>%
      dplyr::rename(Sykehus = HealthUnitShortName)
  }

  return(trauma_filtered)
}
