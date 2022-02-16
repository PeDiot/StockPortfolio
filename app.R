# ---------------- Shiny App Stock Portfolio ----------------


# ----- Setup -----

source("Rpackages.R")
source("setup.R", encoding = "UTF-8")

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

assets_data <- get_tq_data(tickers = tickers) 


# ----- User Interface -----

ui <- fluidPage(
  
  navbarPage("My Stock Portfolio", 
             theme = shinytheme("lumen"), 
             
             # portfolio value
             tabPanel("Portfolio Value", 
                      fluid = TRUE, 
                      icon = icon("dollar"), 
                      
                      sidebarLayout(
                        
                        sidebarPanel(
                          
                          width = 3, 
                          h4("Number of shares"), 
                          num_shares_input(asset1 = "LVMH", 
                                           asset2 = "Air Liquide"), 
                          num_shares_input(asset1 = "L'Oréal", 
                                           asset2 = "Renault"),
                          num_shares_input(asset1 = "Ubisoft", 
                                           asset2 = "Orange"), 
                          num_shares_input(asset1 = "Carrefour", 
                                           asset2 = "TF1"),
                          num_shares_input(asset1 = "EDF", 
                                           asset2 = "OVH Groupe")
                          
                        ), 
                        
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Total value",
                                     br(), 
                                     br(), 
                                     plotlyOutput("portfolio_plot", 
                                                  height = 400, 
                                                  width = 800)),
                            tabPanel("Value per asset",
                                     br(), 
                                     br(), 
                                     plotlyOutput("assets_value_plot", 
                                                  height = 600, 
                                                  width = 1000))
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
                          width = 2, 
                          selectInput("choice", 
                                      label = h4("Which asset?"), 
                                      choices = tickers, 
                                      selected = "LVMH")
                          
                        ),
                        
                        mainPanel( 
                          plotlyOutput("indicators_plot", 
                                       height = 800, 
                                       width = 700)
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
  
  # portfolio value 
  
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
      input$num_shares_TFI.PA), 
    {
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
      assets_value <- compute_assets_value(data = assets_data, 
                                           num_shares = num_shares)
      portfolio_value <- get_portfolio_value(assets_value)
      
      title <- paste("Current value of the portfolio:", 
                     portfolio_value[1, ] %>%
                       pull(value) %>%
                       round(2) %>%
                       format(big.mark = ",", 
                               decimal.mark = ".", 
                              scientific = F), 
                     "$")
      
      output$portfolio_plot <- renderPlotly({
        portfolio_value %>%
          portfolio_evolution(title = title)
      })
      
      output$assets_value_plot <- renderPlotly({
        assets_value %>%
          assets_value_evolution()
      })
      
    }
  )
  
  # financial indicators
  
  observeEvent(input$choice, {
    
    prices <- assets_data[[input$choice]] %>%
      add_moving_avg(window = 20) %>%
      add_moving_avg(window = 50) %>%
      add_macd() %>%
      add_rsi()
      
    output$indicators_plot <- renderPlotly({
      start_date <- get_start_date(period = "24m", 
                                   price_data = prices)
      title <- get_title(ticker = input$choice, 
                         start_date = start_date)
      prices %>%
        indicators_plot(title = title)
    })
    
  })
  
}

shinyApp(ui = ui, server = server)
  
  
