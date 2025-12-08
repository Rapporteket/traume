#' Client (ui) for the traume app
#'
#' @return A shiny app ui object
#' @export

## LEGG INN EKSPORT I UI-biten

app_ui <- function() {
  shiny::tagList( # Needed for "about the user" tags
    shiny::navbarPage( # type of page

      ###### Graphics ----------------------------------------------------------
      title = rapbase::title("Rapporteket for traumeregisteret"),
      windowTitle = "Rapporteket for traumeregisteret",
      theme = rapbase::theme(),
      id = "tabs",

      #---- Startside ----
      shiny::tabPanel( # First tab
        title = "Startside",
        shiny::mainPanel(
          width = 12,
          shiny::htmlOutput("fremside", inline = TRUE), # load in the htmloutput wanted. This file is found in folder "inst"
          tabPanel("Tabell",DT::dataTableOutput("Fremside_tabell")),
          rapbase::navbarWidgetInput("traumeNavbarWidget", selectOrganization = TRUE)
        )
      ),

      #---- Registrering ----
      shiny::tabPanel(
        "Registrering",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            width = 3,
            shiny::selectInput(
              inputId = "time_period",
              label = "Tidsperiode",
              choices = c("Måned" = "maned",
                          "År" = "ar")
            )
          ),
          shiny::mainPanel(
            tabsetPanel(
              tabPanel("Tabell",DT::dataTableOutput("trauma_table")),
              tabPanel("Plot",shiny::plotOutput("trauma_plot"))
            )
          )
        )
      ),

      #---- Kvalitetsindikatorer ----
      shiny::tabPanel(
        "Kvalitetsindikatorer",
        shiny::sidebarLayout(
          sidebarPanel(
            selectInput("selected_year", "Select Year:", choices = NULL)  # Initialize with NULL
          ),
          shiny::mainPanel(
            tabsetPanel(
              tabPanel("Tabell", DT::dataTableOutput("Kvalitetsindikatorer_tabell")),
              tabPanel("Plot", shiny::plotOutput("Kvalitetsindikatorer_plot"))
            )
          )
        )
      ),
      #---- Statistisk prosesskontroll ----
      shiny::tabPanel(
        "Statistisk prosesskontroll",
        shiny::sidebarLayout(
          sidebarPanel(
            selectInput("selected_year_spc", "Select year:", choices = NULL), # Initialize with NULL
            selectInput("spc_options", "Select trend:",
                        choices = c("Registrert innen 3 måneder" = "registrerte",
                                    "Andel CT"="ct_scans",
                                    "Andel CT (NISS > 15)"="ct_iss_15",
                                    "Andel CT (NISS < 4)"="ct_iss_4",
                                    "Andel intuberte prehospitalt"="intuberte_prehospitalt",
                                    "Andel intuberte akuttmottak"="intuberte_mottak",
                                    "Mortalitet"="mortalitet",
                                    "Andel røntgen bekken"="rontgen_bekken",
                                    "Andel røntgen bekken (ISS > 15)"="rontgen_bekken_iss15",
                                    "Andel røntgen thorax"="rontgen_thorax",
                                    "Andel røntgen thorax (ISS > 15)"="rontgen_thorax_iss15"))
          ),
          shiny::mainPanel(
            shiny::plotOutput("Kvalitetsindikatorer_spc")
          )
        )
      )

    ) # navbarPage
  ) # tagList
}

