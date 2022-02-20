# ---------------- Shiny App Stock Portfolio ----------------


# ----- Setup -----

source("Rpackages.R")
source("setup.R",
       encoding = "UTF-8")

# ----- Data -----

assets <- c("LVMH",  
            "Air Liquide",
            "L'Oréal",  
            "Renault", 
            "Ubisoft", 
            "Orange", 
            "Carrefour",
            "TF1", 
            "EDF", 
            "OVH Groupe")

selection <- french_stocks %>%
  filter(Name %in% assets) %>%
  select(c(Name, Symbol))

tickers <- selection %>%
  pull(Symbol)
names(tickers) <- selection %>%
  pull(Name) 

# ----- User Interface -----

ui <- fluidPage(
  
  useShinydashboard(), 
  
  navbarPage("My Stock Portfolio", 
             theme = shinytheme("lumen"), 
             
             # portfolio value
             tabPanel("Portfolio Value", 
                      fluid = TRUE, 
                      icon = icon("dollar"), 
                      
                      sidebarLayout(
                        
                        sidebarPanel(
                          
                          width = 3, 
                          h4("How many shares per asset?"), 
                          num_shares_input(asset1 = "LVMH", 
                                           asset2 = "Air Liquide", 
                                           val1 = .5, 
                                           val2 = .1), 
                          num_shares_input(asset1 = "L'Oréal", 
                                           asset2 = "Renault", 
                                           val1 = .2),
                          num_shares_input(asset1 = "Ubisoft", 
                                           asset2 = "Orange", 
                                           val1 = 3), 
                          num_shares_input(asset1 = "Carrefour", 
                                           asset2 = "TF1", 
                                           val2 = 2),
                          num_shares_input(asset1 = "EDF", 
                                           asset2 = "OVH Groupe", 
                                           val2 = 5), 
                          br(), 
                          dateInput(inputId = "first_buy_date",
                                    label = h4("When did you buy your first asset?"),
                                    value = "2021-09-01",
                                    max = Sys.Date(),
                                    format = "yyyy-mm-dd")
                          
                        ), 
                        
                        mainPanel(
                          
                          tabsetPanel(
                            
                            tabPanel("Overview",
                                     br(), 
                                     br(), 
                                     plotlyOutput("portfolio_evolution", 
                                                  height = 600, 
                                                  width = 800)
                                     ),
                            
                            tabPanel("Composition",
                                     br(), 
                                     br(), 
                                     plotlyOutput("portfolio_composition", 
                                                  height = 400, 
                                                  width = 800))
                          )
                          
                        )
                        
                      )
                      
             ),
             
             # financial indicators 
             tabPanel("Financial Indicators", 
                      fluid = TRUE, 
                      icon = icon("calculator"), 
                      
                      sidebarLayout(
                        
                        sidebarPanel(
                          width = 3, 
                          selectInput("ticker", 
                                      label = h4("Which asset?"), 
                                      choices = tickers, 
                                      selected = "LVMH")
                          
                        ),
                        
                        mainPanel( 
                          tabsetPanel(
                            tabPanel("Candlestick",
                                     br(), 
                                     br(), 
                                     plotlyOutput("candlestick_plot", 
                                                  height = 400, 
                                                  width = 800)),
                            tabPanel("MACD",
                                     br(), 
                                     br(), 
                                     plotlyOutput("macd_plot", 
                                                  height = 400, 
                                                  width = 800)), 
                            tabPanel("RSI",
                                     br(), 
                                     br(), 
                                     plotlyOutput("rsi_plot", 
                                                  height = 400, 
                                                  width = 800))
                          )
                        )
                      
                      )
                      
             ), 
             
             # stock prediction
             tabPanel("Prediction", 
                      fluid = TRUE, 
                      icon = icon("chart-line")) 
             
  )
  
)

# ----- Server -----

server <- function(input, output) {
  
  observeEvent(
    c(input$num_shares_LVMUY,
      input$num_shares_OR.PA,
      input$num_shares_AI.PA,
      input$num_shares_ORAN,
      input$num_shares_ECIFF,
      input$num_shares_CRERF,
      input$num_shares_RNSDF,
      input$num_shares_UBSFF,
      input$num_shares_OVH.PA,
      input$num_shares_TFI.PA, 
      input$first_buy_date, 
      input$ticker), 
    {
      
      
      # yahoo finance data ---
      assets_data <- get_tq_data(tickers = tickers, 
                                 start_date = input$first_buy_date) 
      
      
      # portfolio ---
      num_shares <- c(input$num_shares_LVMUY,
                      input$num_shares_OR.PA,
                      input$num_shares_AI.PA,
                      input$num_shares_ORAN,
                      input$num_shares_ECIFF,
                      input$num_shares_CRERF,
                      input$num_shares_RNSDF,
                      input$num_shares_UBSFF,
                      input$num_shares_OVH.PA,
                      input$num_shares_TFI.PA)
      
      names(num_shares) <- tickers
      
      # values 
      assets_value <- compute_assets_value(data = assets_data, 
                                           num_shares = num_shares)
      portfolio_value <- get_portfolio_value(assets_value)
      
      # returns 
      returns_data <- assets_value %>%
        compute_daily_returns() %>%
        compute_weighted_returns(num_shares = num_shares) 
      
      portfolio_returns <- ret_data %>%
        compute_cumulative_returns()
     
      # plots
      output$portfolio_evolution <- renderPlotly({
        portfolio_evolution(portfolio_value, 
                            portfolio_returns)
      })
      
      output$portfolio_composition <- renderPlotly({
        assets_value %>%
          portfolio_composition()
      })
      
      
      # financial indicators ---
      prices <- assets_data[[input$ticker]] %>%
        add_moving_avg(window = 20) %>%
        add_moving_avg(window = 50) %>%
        add_macd() %>%
        add_rsi()
      
      output$candlestick_plot <- renderPlotly({
        prices %>%
          candlestick_chart(ticker = input$ticker) 
      })
      
      output$macd_plot <- renderPlotly({
        prices %>%
          macd_chart(ticker = input$ticker) 
      })
      
      output$rsi_plot <- renderPlotly({
        prices %>%
          rsi_chart(ticker = input$ticker) 
      })
      
    }
  )
  
}

shinyApp(ui = ui, server = server)
  
  
