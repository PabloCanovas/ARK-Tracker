
# Cuando queramos calcular variables dependientes de inputs, tendremos que crear funciones reactivas que las calculen,
# y despues usarlas como funciones.

#Ejemplo:

# add_X <- function(n, X){
#   return(n+X)
# }
# 
# add_X_react <- reactive(add_X(5, input$X))
# o bien directamente
# add_X_react <- reactive(5+input$X)
# 
# output$resultado <- renderPrint(add_X_react())



library(shinydashboard)
library(tidyverse)
library(lubridate)
library(glue)
library(magrittr)
library(DT)
library(shiny)

options(dplyr.summarise.inform=F)

source("Tools.R")


# shiny::runApp(display.mode="showcase")


funds <- c("ARKK", "ARKQ", "ARKW","ARKG","ARKF")
funds <- c("ARKF")

today <- today() %>% Get_last_trading_day()  
today <- today %>% add(days(1))                  # Los primeros archivos que tengo están con fecha de descarga, no del dia
# previous_day <- today %>% add(days(-1)) %>% Get_last_trading_day()




#### UI -------------------------------------------------------------------------------------------------------


ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Altas/Bajas", tabName = "altas", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "altas",
              fluidRow(
                box(
                  offset = 5,
                  width = 3,
                  title = "Fund",
                  status = "warning", solidHeader = F,
                  
                  selectInput(inputId = "fund", multiple = F, label = NULL, selectize = T,
                              choices = list("ALL", "ARKF"), selected = "ALL")
                ),
                box(
                  offset = 5,
                  width = 3,
                  title = "Days back",
                  status = "warning", solidHeader = F,
                  
                  selectInput(inputId = "days_back", multiple = F, label = NULL, selectize = T,
                              choices = list("1D" = 1, "7D" = 7, "30D" = 30), selected = "1D")
                )
              ),
              fluidRow(
                box(title = "ALTAS", status = "success", solidHeader = TRUE,
                    dataTableOutput(outputId = "altas")
                )
              ),
              fluidRow(
                box(title = "BAJAS", status = "danger", solidHeader = TRUE,
                    dataTableOutput(outputId = "bajas")
                )
              ),
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      )
    )
  )
)



#### SERVER -------------------------------------------------------------------------------------------------------


server <- function(input, output) {
  
  
  # TODO Repasar qué variables quiero mostrar en cada tabla. Seguro: "Dias desde alta/baja"
  
  get_previous_date <- reactive(Get_previous_date(today, input$days_back))
  get_altas_bajas <- reactive(Get_altas_bajas(today, get_previous_date(), funds = "all"))
  
  output$altas <- renderDataTable(get_altas_bajas()$altas %>% 
                                    {if(tolower(input$fund) != "all") filter(., fund == input$fund) else .}
  )
  output$bajas <- renderDataTable(get_altas_bajas()$bajas %>% 
                                    {if(tolower(input$fund) != "all") filter(., fund == input$fund) else .}
  )
  
  
  # df <- read_csv("ARK_funds.csv") %>%
  #   select(-cusip) %>%
  #   group_by(date, company, ticker) %>%
  #   summarise(shares = sum(shares),
  #             market_value = sum(market_value)) %>%
  #   mutate(date = mdy(date))
  # 
  # 
  # output$table1 <- renderDataTable(df)
  
  
  # write_csv(today_state, "ARK_funds.csv")
  # 
  
}

shinyApp(ui, server)