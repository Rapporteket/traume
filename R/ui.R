#' Client (ui) for the traume app
#'
#' @return A shiny app ui object
#' @export

## LEGG INN EKSPORT I UI-biten

app_ui <- function() {
  shiny::addResourcePath("rap", system.file("www", package = "rapbase"))

  shiny::tagList( # Needed for "about the user" tags
    shiny::navbarPage( # type of page

      ###### Graphics ----------------------------------------------------------
      title = shiny::div(shiny::a(shiny::includeHTML(system.file('www/logo.svg', package = 'rapbase'))), # add the logo
                         "Rapporteket for traumeregisteret"),
      windowTitle = "Rapporteket for traumeregisteret",
      theme = "rap/bootstrap.css", # theme of app
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
      )

    ) # navbarPage
  ) # tagList
}

