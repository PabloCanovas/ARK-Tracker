
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
# library(tidyquant)
library(lubridate)
library(glue)
library(magrittr)
library(ggthemes)
library(DT)
library(shiny)
library(shinyWidgets)
library(patchwork)
library(plotly)

options(dplyr.summarise.inform=F)
setwd("C:/Users/Pablo/Documents/RWork/Investing/ARK/")
source("Tools.R")
# shiny::runApp(display.mode="showcase")

funds <- c("ARKK", "ARKQ", "ARKW","ARKG","ARKF")
today <- today() %>% Get_last_trading_day()  


##### TODO
# En la tabla altas/bajas añadir "Dias desde alta/baja" (o fecha del alta/baja)
# Añadir como quantitys los delta_shares y delta_weights, con sus pct, en las pestañas de fund y stock
# En stocks, cambiar el eje X para que aparezcan las fechas (en vez de trading_day)
# En deltas, pensar si quiero dos tablas separadas por compras y ventas

#####


#### UI -------------------------------------------------------------------------------------------------------


ui <- dashboardPage(
  dashboardHeader(title = "ARK Tool"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Daily Changes", tabName = "Deltas", icon = icon("th")),
      menuItem("Funds", tabName = "Funds", icon = icon("th")),
      menuItem("Altas/Bajas", tabName = "Altas", icon = icon("dashboard")),
      menuItem("Stocks", tabName = "Stocks", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Deltas",
              fluidRow(
                box(
                  offset = 10,
                  width = 2,
                  height = 100,
                  title = "Fund",
                  status = "warning", solidHeader = F,
                  
                  selectInput(inputId = "delta_fund", multiple = F, label = NULL, selectize = T,
                              choices = list("AGGR", "ALL", "ARKK", "ARKQ", "ARKW","ARKG","ARKF"), selected = "AGGR")
                ),
                box(
                  offset = 8,
                  width = 2,
                  height = 100,
                  title = "Days back",
                  status = "warning", solidHeader = F,
                  
                  # selectInput(inputId = "days_back", multiple = F, label = NULL, selectize = T,
                  #             # choices = list("1D" = 1, "7D" = 7, "30D" = 30), selected = "1D")
                  #             choices = list("1D" = 1, "7D" = 7), selected = "7D")
                  
                  shiny::numericInput(inputId = "delta_days_back", label = NULL, value = 1, min = 1, max = 20, step = 1)
                ),
                box(
                  offset = 8,
                  width = 2,
                  height = 100,
                  title = "Operations",
                  status = "warning", solidHeader = F,
                  prettyCheckbox(
                    inputId = "delta_only_trades",
                    label = "Show only traded stocks",
                    value = TRUE,
                    status = "warning",
                    # shape = c("square", "curve", "round"),
                    shape = c("curve"),
                    outline = FALSE,
                    fill = T,
                    thick = T,
                    animation = NULL,
                    icon = icon("check"),
                    plain = FALSE,
                    bigger = T,
                    inline = FALSE,
                    width = NULL
                  )
                )
              ),
              fluidRow(
                box(width = 11, 
                    title = "Deltas", status = "success", solidHeader = TRUE,
                    dataTableOutput(outputId = "deltas")
                )
                # ),
                # fluidRow(
                #   box(title = "BAJAS", status = "danger", solidHeader = TRUE,
                #       dataTableOutput(outputId = "bajas")
                #   )
              )
      ),
      tabItem(tabName = "Altas",
              fluidRow(
                box(
                  offset = 5,
                  width = 3,
                  title = "Fund",
                  status = "warning", solidHeader = F,
                  
                  selectInput(inputId = "fund", multiple = F, label = NULL, selectize = T,
                              choices = list("ALL", "ARKK", "ARKQ", "ARKW","ARKG","ARKF"), selected = "ALL")
                ),
                box(
                  offset = 5,
                  width = 3,
                  title = "Days back",
                  status = "warning", solidHeader = F,
                  
                  # selectInput(inputId = "days_back", multiple = F, label = NULL, selectize = T,
                  #             # choices = list("1D" = 1, "7D" = 7, "30D" = 30), selected = "1D")
                  #             choices = list("1D" = 1, "7D" = 7), selected = "7D")
                  
                  shiny::numericInput(inputId = "days_back", label = NULL, value = 5, min = 1, max = 20, step = 1)
                )
              ),
              fluidRow(
                box(width = 8, 
                    title = "ALTAS", status = "success", solidHeader = TRUE,
                    dataTableOutput(outputId = "altas")
                )
              ),
              fluidRow(
                box(width = 8,
                    title = "BAJAS", status = "danger", solidHeader = TRUE,
                    dataTableOutput(outputId = "bajas")
                )
              )
      ),
      
      tabItem(tabName = "Stocks",
              # h2("Widgets tab content")
              
              column(1,
                     textInput(inputId = "stock", label = "Stock", value = "SQ"),
              ),
              column(1,
                     selectInput(inputId = "quantity", multiple = F, label = "Variable", selectize = T,
                                 choices = list("Shares" = "M_shares", "Weight" = "weight"),
                                 selected = "M_shares"),
              ),
              column(2,
                     selectInput(inputId = "mode", multiple = F, label = "Mode", selectize = T,
                                 choices = list("Aggregated" = "aggregated", "By fund" = "by_fund"), 
                                 selected = "aggregated"),
              ),
              
              fluidRow(
                column(8,
                       offset = 2,
                       box(
                         width = NULL,
                         # width = 10,
                         height = 500,
                         # plotlyOutput("plot"),
                         plotOutput("stock_plot", height = 450),
                         status = "warning",
                         # title = "aaa"
                       )
                )
              )
      ), 
      tabItem(tabName = "Funds",
              # h2("Widgets tab content")
              
              
              column(1,
                     
                     selectInput(inputId = "fund2", multiple = F, label = "Fund", selectize = T,
                                 choices = list("ARKK", "ARKQ", "ARKW", "ARKG", "ARKF"), selected = "ARKK"),
              ),
              
              column(1,
                     
                     selectInput(inputId = "quantity2", multiple = F, label = "Variable", selectize = T,
                                 choices = list("Shares" = "M_shares", "Weight" = "weight"), selected = "M_shares")
              ),
              
              
              
              # column(1,
              #   
              # 
              # checkboxInput(inputId = "scale_free", label = "Free Y scale", value = FALSE), 
              # ),
              
              
              # plot vertical
              
              # fluidRow(
              #   column(8,
              #          offset = 2,
              #          box(
              #            width = NULL,
              #            # width = 10,
              #            # height = 1350,
              #            # plotlyOutput("plot"),
              #            plotOutput("fund_plot_long", height = 4000),
              #            status = "warning",
              #            # title = "aaa"
              #          )
              #   )
              # )
              
              
              # plot horizontal
              
              fluidRow(
                column(12, 
                       box(
                         width = NULL,
                         plotOutput("fund_plot_wide", height = 650),
                         status = "warning",
                         # title = "aaa"
                       )
                )
              )
              
              
      )
    )
  )
)



#### SERVER -------------------------------------------------------------------------------------------------------


server <- function(input, output) {
  
  
  # Las fechas de la tabla están mal 
  
  get_previous_date <- reactive(Get_previous_date(today, input$days_back))
  
  get_altas_bajas <- reactive(Get_altas_bajas(today, get_previous_date(), funds = "all"))
  
  output$altas <- renderDataTable(get_altas_bajas()$altas %>% 
                                    filter(fund != "AGGR") %>% 
                                    {if(tolower(input$fund) != "all") filter(., fund == input$fund) else .})
  
  output$bajas <- renderDataTable(get_altas_bajas()$bajas %>% 
                                    filter(fund != "AGGR") %>% 
                                    {if(tolower(input$fund) != "all") filter(., fund == input$fund) else .})
  
  
  states_df <- Get_states()
  today_state <- states_df %>% filter(date == max(date))
  
  get_deltas <- reactive(
    Get_delta(
      today_state %>% 
        { if(input$delta_fund == "ALL") filter(., fund != "AGGR") else filter(., fund == input$delta_fund) }, 
      
      states_df %>% 
        filter(trading_day == (max(trading_day) - input$delta_days_back)) %>%
        { if(input$delta_fund == "ALL") filter(., fund != "AGGR") else filter(., fund == input$delta_fund) },
      
      arrange_by = "delta_pct_weight",         # default order.   Tb podría ser  *input$delta_quantity*
      only_trades = input$delta_only_trades
    )
  )
  
  output$deltas <- renderDataTable(get_deltas())
  
  
  
  plot_stock <- reactive(Plot_stock(states_df, input$stock, input$quantity, input$mode))
  output$stock_plot <- renderPlot(plot_stock())
  
  
  # plot_fund_long <- reactive(Plot_fund_long(states_df, input$fund2, input$quantity, input$scale_free))
  # output$fund_plot_long <- renderPlot(plot_fund_long())
  
  plot_fund_wide <- reactive(Plot_fund_wide(states_df, input$fund2, input$quantity2))
  output$fund_plot_wide <- renderPlot(plot_fund_wide())
  
}

shinyApp(ui, server)
