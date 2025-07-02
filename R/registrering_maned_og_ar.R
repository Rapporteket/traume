#'
#'
#'
#'
#'
#' @return
#' @export


#---- Registrering figur ----

plot_registrering <- function(input,total) {
  if (input$time_period == "ar") {
    trauma_filtered <- total %>%
      filter(year >= year(Sys.Date()) - 5,
             HealthUnitShortName == "Kristiansand") %>%
      group_by(year) %>%
      summarise(total_traumas = n())


  } else {
    trauma_filtered <- total %>%
      filter(month >= floor_date(Sys.Date() - months(12), "month"),
             HealthUnitShortName == "Kristiansand") %>%
      group_by(month) %>%
      summarise(total_traumas = n())
  }

  return(trauma_filtered)
}

#---- Registrering tabell ----

table_registrering <- function(input,total) {
  if (input$time_period == "ar") {
    trauma_filtered <- total %>%
      filter(year >= year(Sys.Date()) - 5,
             acc_overflyttet == 2) %>%
      group_by(HealthUnitShortName, year) %>%
      summarise(Antall = n()) %>%
      arrange(desc(year)) %>%
      pivot_wider(names_from = year, values_from = Antall, values_fill = 0) %>%
      arrange(HealthUnitShortName) %>%
      rename(Sykehus = HealthUnitShortName)

  } else {
    trauma_filtered <- total %>%
      filter(month >= floor_date(Sys.Date() - months(9), "month"),
             acc_overflyttet == 2) %>%
      group_by(HealthUnitShortName, month) %>%
      summarise(Antall = n()) %>%
      mutate(month = format(month, "%Y-%m")) %>%
      arrange(desc(month)) %>%
      pivot_wider(names_from = month, values_from = Antall, values_fill = 0) %>%
      arrange(HealthUnitShortName) %>%
      rename(Sykehus = HealthUnitShortName)
  }

  return(trauma_filtered)
}
