#' Server logic for the rapRegTemplate app
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#'
#' @return A shiny app server object
#' @export

app_server <- function(input, output, session) {

  #---- Databehandling ----
  total <- getRegData()

  total_nasjonalt <- total %>%
    mutate(
      HealthUnitShortName = "Nasjonalt"
    ) %>%
    bind_rows(.,total)

  # Hvilke ar er i datasettet
  years <- total %>%
    filter(HealthUnitShortName == "Kristiansand") %>%
      mutate(year = year(FormDate)) %>%  # Extract year from date
      pull(year) %>%
      unique() %>%
      sort(decreasing = TRUE)


  years_spc <- years[1:(length(years) - 4)]


  updateSelectInput(session, "selected_year", choices = years)
  updateSelectInput(session, "selected_year_spc", choices = years_spc)


  # Brukerinformasjon i menylinja (navbar)
  output$appUserName <- shiny::renderText(
    paste(rapbase::getUserFullName(session),
          rapbase::getUserRole(session), sep = ", ")
  )
  output$appOrgName <- shiny::renderText(rapbase::getUserReshId(session))
  userInfo <-
    rapbase::howWeDealWithPersonalData(session, callerPkg = "rapRegTemplate")
  shiny::observeEvent(input$userInfo, {
    shinyalert::shinyalert("Dette vet Rapporteket om deg:", userInfo,
                           type = "", imageUrl = "rap/logo.svg",
                           closeOnEsc = TRUE, closeOnClickOutside = TRUE,
                           html = TRUE, confirmButtonText = rapbase::noOptOutOk())
  })

  #---- Startside ----
  output$Fremside <- shiny::renderUI({
    rapbase::renderRmd(
      "Fremside.Rmd",
      outputType = "html_fragment"
    )
  })
  output$Fremside_tabell <- DT::renderDataTable({
    DT::datatable(
      tabell_fremside(total),
      options = list(
        pageLength = 18,       # Set number of rows per page
        dom = 't',             # 't' stands for table only, removes other controls like search and show
        searching = FALSE,     # Hide the search field
        lengthChange = FALSE,  # Hide the "Show" field for row length
        ordering = FALSE # Hide the ordering option
      ),
      rownames = FALSE
    )
  })


  #---- Registrering ----

  output$trauma_plot <- renderPlot({
    ggplot(plot_registrering(input,total), aes(x = !!sym(ifelse(input$time_period == "ar", "year", "month")), y = total_traumas)) +
      geom_col(fill = "#6baed6") +
      labs(
        x = input$time_period,
        y = "Antall Traumer",
        title = paste("Antall traumer per", ifelse(input$time_period == "ar", "år", "måned"), "for Kristiansand"),
        subtitle = ifelse(input$time_period == "ar", "Siste 5 år", "Siste 12 måneder")
      ) +
      theme_minimal()
  })

  output$trauma_table <- DT::renderDataTable({
    DT::datatable(table_registrering(input,total), options = list(pageLength = 16), rownames = FALSE)
  })


  #---- Kvalitetsindikatorer ----

  output$Kvalitetsindikatorer_tabell <- DT::renderDataTable({

    inpercent <- achievements_data(input, total_nasjonalt) %>%
      mutate(`Prosent lokalt` = scales::percent(round(`Prosent lokalt`,3)),
             `Prosent nasjonalt` = scales::percent(round(`Prosent nasjonalt`,3)))

    DT::datatable(inpercent,
                  options = list(pageLength = 12,
                                 dom = 't',             # 't' stands for table only, removes other controls like search and show
                                 searching = FALSE,     # Hide the search field
                                 lengthChange = FALSE,  # Hide the "Show" field for row length
                                 ordering = FALSE # Hide the ordering option)
                  ),
                  rownames = FALSE)
  })


output$Kvalitetsindikatorer_plot <- renderPlot({
  ggplot(achievements_data(input,total_nasjonalt), aes(x = Indikatorer, y = `Prosent lokalt`)) +
    geom_col(show.legend = FALSE, position = "dodge", fill = "#6baed6") +
    geom_point(aes(x = Indikatorer, y = `Prosent nasjonalt`), shape = 18, size = 2.5) +
    scale_y_continuous(labels=scales::percent, breaks = seq(0, 1, 0.20), # x axis with percentage
                       limits=c(0, 1)) +
    coord_flip() +
    theme_minimal()
})

#---- Statistisk prosesskontroll ----

output$Kvalitetsindikatorer_spc <- renderPlot({
  library(qicharts2)
  data_spc <- spc_data(input,total_nasjonalt)
  qic(quarter,
      get(paste0(input$spc_options, "_teller")),
      get(paste0(input$spc_options,"_nevner")),
      data      = data_spc,
      facets = ~ HealthUnitShortName ,
      chart     = 'p',
      title     = 'HLR av tilstedev\u00E6rende',
      x.period = "quarter",
      y.expand = c(0,1),
      ylab      = "Andel",
      xlab      = 'År',
      scales = "free_y")

})
}
