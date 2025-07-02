#' Server logic for the traume app
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#'
#' @return A shiny app server object
#' @export

### + eksport i server

app_server <- function(input, output, session) {

  # load in and clean data:

  total <- clean_xlsx_data()


  ######## USER INFO--------------------------------------------------------------

  # Make a df that can be used for mapping between resh-ids and hospital names
  # Must be organized as df with two columns: UnitId and orgname
  # in order for navbarWidgetServer2 to work properly

  map_db_resh <- total %>%
    dplyr::select(HealthUnitShortName, UnitId) %>% # select required columns
    unique() %>% # keep only unique variables
    dplyr::mutate(orgname = HealthUnitShortName) %>% # make new column with new name
    dplyr::select(-c(HealthUnitShortName)) # take out old columns


  user <- rapbase::navbarWidgetServer2("traumeNavbarWidget", # denne skal bli navbarWidgetServer når alt er fikset i rapbase
                                       "traume",
                                       caller = "traume",
                                       map_orgname = shiny::req(map_db_resh))

  ### Lage nasjonalt datasett:

  total_nasjonalt <- total %>%
    dplyr::mutate(
      HealthUnitShortName = "Nasjonalt"
    ) %>%
    dplyr::bind_rows(.,total)

  # Hvilke ar er i datasettet
  years_reactive <- reactive({
    years <- total %>%
      dplyr::filter(UnitId == user$org()) %>%
      dplyr::mutate(year = year(FormDate)) %>%  # Extract year from date
      dplyr::pull(year) %>%
      unique() %>%
      sort(decreasing = TRUE)
  })

  years_spc_reactive <- reactive({
    years_spc <- years_reactive()[1:(length(years_reactive()) - 4)]
  })

  observe({
    updateSelectInput(session, "selected_year", choices = years_reactive())
  })

  observe({
    updateSelectInput(session, "selected_year_spc", choices = years_spc_reactive())
  })


  #---- Startside ----
  output$fremside <- shiny::renderUI({
    rapbase::renderRmd(
      system.file("Fremside.Rmd", package = "traume"),
      outputType = "html_fragment"
    )
  })

  output$Fremside_tabell <- DT::renderDataTable({
    DT::datatable(
      tabell_fremside(total, userUnitId = user$org()),
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

    # få tak i navn til tittel
    navn <- get_HealthUnitShortName(user$org(), map_db_resh)
    ggplot(plot_registrering(input,total, userUnitId = user$org()),
           aes(x = !!sym(ifelse(input$time_period == "ar", "year", "month")), y = total_traumas)) +
      geom_col(fill = "#6baed6") +
      labs(
        x = ifelse(input$time_period == "ar", "År", "Måned"),
        y = "Antall Traumer",
        title = paste("Antall traumer per", ifelse(input$time_period == "ar", "år", "måned"), "for", navn),
        subtitle = ifelse(input$time_period == "ar", "Siste 5 år", "Siste 12 måneder")
      ) +
      theme_minimal()
  })

  output$trauma_table <- DT::renderDataTable({
    DT::datatable(table_registrering(input,total), options = list(pageLength = 16), rownames = FALSE)
  })


}

