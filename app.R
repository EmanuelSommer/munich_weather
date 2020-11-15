library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(rvest)
library(tidyverse)
library(robotstxt)

# URLS
path_br <- "https://www.br.de/wetter/action/5_tage_prognose.do?plz=10865&regio=Oberbayern"
path_wetter.com <- "https://www.wetter.com/deutschland/muenchen/DE0006515.html"
path_bergfex <- "https://www.bergfex.de/sommer/muenchen/wetter/"
path_wetteronline <- "https://www.wetteronline.de/wetter/muenchen"

# Color for the boxes
color_chosen <- "blue"




ui <- dashboardPage(
  dashboardHeader(
    title = "München Wetter",
    dropdownMenu(
      type = "notifications",
      icon = icon("link"),
      headerText = "Links",
      notificationItem("Mein Github",
        icon = icon("github"),
        href = "https://github.com/EmanuelSommer/"
      )
    )
  ),
  dashboardSidebar(
    tags$br(),
    actionButton("reset",
      label = "Aktualisiere das Wetter", icon = icon("redo"),
      style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
    )
  ),
  dashboardBody(
    fluidRow(
      column(
        width = 6,
        infoBoxOutput("wetter_br", width = NULL)
      ),
      column(
        width = 6,
        infoBoxOutput("wetter_bergfex", width = NULL)
      )
    ),
    fluidRow(
      column(
        width = 6,
        infoBoxOutput("wetter_com", width = NULL)
      ),
      column(
        width = 6,
        infoBoxOutput("wetter_online", width = NULL)
      )
    )
  ),
  skin = "black"
)

#####################################################################################################################

server <- function(input, output) {
  br_temp <- eventReactive(input$reset,
    {
      br <- read_html(path_br)
      temp_br <- br %>%
        html_nodes("td") %>%
        html_text()
      temp_br <- str_replace_all(temp_br[9], ",", ".")
      as.numeric(str_extract(temp_br, "[:digit:]++.[:digit:]"))
    },
    ignoreNULL = FALSE
  )

  wettercom_temp <- eventReactive(input$reset,
    {
      wetter.com <- read_html(path_wetter.com)
      temp_wetter.com <- wetter.com %>%
        html_nodes(".text--white.beta") %>%
        html_text()
      temp_wetter.com <- str_replace_all(temp_wetter.com, ",", ".")
      as.numeric(str_extract(temp_wetter.com, "[:digit:]++"))
    },
    ignoreNULL = FALSE
  )

  bergfex_temp <- eventReactive(input$reset,
    {
      bergfex <- read_html(path_bergfex)
      temp_bergfex <- bergfex %>%
        html_nodes(".value") %>%
        html_text()
      as.numeric(temp_bergfex)
    },
    ignoreNULL = FALSE
  )

  wetteronline_temp <- eventReactive(input$reset,
    {
      wetteronline <- read_html(path_wetteronline)
      temp_wetteronline <- wetteronline %>%
        html_nodes("#nowcast-card-temperature .value") %>%
        html_text()
      as.numeric(temp_wetteronline)
    },
    ignoreNULL = FALSE
  )

  output$wetter_br <- renderInfoBox({
    infoBox(
      title = "München Temperatur in °C",
      value = br_temp(),
      subtitle = "Quelle: BR-Wetter",
      icon = icon("thermometer-half"),
      width = 6,
      href = path_br,
      color = color_chosen
    )
  })

  output$wetter_bergfex <- renderInfoBox({
    infoBox(
      title = "München Temperatur in °C",
      value = bergfex_temp(),
      subtitle = "Quelle: Bergfex",
      icon = icon("thermometer-half"),
      width = 6,
      href = path_bergfex,
      color = color_chosen
    )
  })

  output$wetter_com <- renderInfoBox({
    infoBox(
      title = "München Temperatur in °C",
      value = wettercom_temp(),
      subtitle = "Quelle: wetter.com",
      icon = icon("thermometer-half"),
      width = 6,
      href = path_wetter.com,
      color = color_chosen
    )
  })

  output$wetter_online <- renderInfoBox({
    infoBox(
      title = "München Temperatur in °C",
      value = wetteronline_temp(),
      subtitle = "Quelle: wetteronline",
      icon = icon("thermometer-half"),
      width = 6,
      href = path_wetteronline,
      color = color_chosen
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
