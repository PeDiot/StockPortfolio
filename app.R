# Setup -------------------------------------------------------------------

source("Rpackages.R")
source("setup.R",
       encoding = "UTF-8")

# Data --------------------------------------------------------------------

load(paste0(backup, "symbols.RData"))
tickers <- symbols %>% pull(tickers) 
names(tickers) <- rownames(symbols)

# yahoo finance data
date_init <- "2021-01-01"
yf_data <- get_tq_data(tickers = tickers, 
                       start_date = date_init) 

save_data_list(df_list = yf_data)

# Predicition using python script --------------------------------------------------------------------

reticulate::py_run_file("stock_prediction.py")

# User Interface ----------------------------------------------------------


ui <- fluidPage(
  
  useShinydashboard(), 
  html_dependency_awesome(),
  
  tags$head(tags$style(infoBox_dims())),
  
  navbarPage("My Stock Portfolio", 
             theme = shinytheme("lumen"), 
             
             ## portfolio value ----------
             tabPanel("Portfolio Value", 
                      fluid = TRUE, 
                      icon = icon("dollar"), 
                      
                      sidebarLayout(
                        
                        ### inputs -----
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
                          dateInput(inputId = "start_date",
                                    label = h4("Enter your start date"),
                                    width = "200px",  
                                    value = "2021-09-01",
                                    max = Sys.Date(),
                                    format = "yyyy-mm-dd")
                          
                        ), 
                        
                        mainPanel(
                          
                          tabsetPanel(
                            
                            ### portfolio overview -----
                            tabPanel("Overview",
                                     br(), 
                                     br(),
                                     fluidRow(column(width = 2), 
                                              infoBoxOutput("port_current_val"),
                                              column(width = 1), 
                                              infoBoxOutput("port_current_cumret")), 
                                     br(), 
                                     div(plotlyOutput("portfolio_evolution", 
                                                      height = 600, 
                                                      width = 700),
                                         align = "center")),
                            
                            ### portfolio composition -----
                            tabPanel("Composition",
                                     br(), 
                                     h4(paste( "Contribution of each asset to the portfolio value as of",
                                              format(Sys.Date(), "%d/%m/%Y") ), 
                                        align = "center"),
                                     div(plotlyOutput("portfolio_composition", 
                                                      height = 400, 
                                                      width = 800), 
                                         align = "center"), 
                                     br(),
                                     h4("Your best and worst assets", align = "center"),
                                     br(), 
                                     fluidRow(column(width = 2), 
                                              infoBoxOutput("best_asset_cumret"),
                                              column(width = 1), 
                                              infoBoxOutput("worst_asset_cumret"))) 
                          )
                          
                        )
                        
                      )
                      
             ),
             
             ## financial indicators ----------
             
             tabPanel("Financial Indicators", 
                      fluid = TRUE, 
                      icon = icon("calculator"), 
                      
                      sidebarLayout(
                        
                        ### inputs -----
                        sidebarPanel(
                          width = 3, 
                          selectInput("ticker", 
                                      label = h4("Which asset?"), 
                                      choices = tickers, 
                                      selected = "LVMH")
                          
                        ),
                        
                        ### financial data viz -----
                        mainPanel( 
                          tabsetPanel(
                            tabPanel("Candlestick",
                                     br(), 
                                     br(), 
                                     br(), 
                                     div(plotlyOutput("candlestick_plot", 
                                                  height = 400, 
                                                  width = 700), 
                                         align = "center")),
                            
                            tabPanel("MACD",
                                     br(), 
                                     br(), 
                                     br(), 
                                     div(plotlyOutput("macd_plot", 
                                                  height = 600, 
                                                  width = 700), 
                                         align = "center")), 
                            tabPanel("RSI",
                                     br(), 
                                     br(), 
                                     br(), 
                                     div(plotlyOutput("rsi_plot", 
                                                  height = 600, 
                                                  width = 700), 
                                     align = "center")),
                          )
                        )
                      
                      )
                      
             ), 
             
             ## stock prediction ----------
             tabPanel("Prediction", 
                      fluid = TRUE, 
                      icon = icon("chart-line"), 
                      
                      sidebarLayout(
                        
                        ### inputs -----
                        sidebarPanel(
                          width = 3, 
                          dateInput(inputId = "pred_start_date",
                                    label = h4("How far back do you want to go?"),
                                    width = "200px",  
                                    value = "2022-01-01",
                                    min = today() - months(3), 
                                    max = today(),
                                    format = "yyyy-mm-dd")
                        ), 
                        
                        ### predictions data viz -----
                        mainPanel(
                          br(), 
                          br(), 
                          br(), 
                          div(plotlyOutput("portfolio_pred_plot", 
                                           height = 400, 
                                           width = 700), 
                              align = "center")
                        )
                      )
              ) 
             
  )
  
)

# Server ------------------------------------------------------------------

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
      input$start_date, 
      input$ticker, 
      input$pred_start_date), 
    {
      
      ## portfolio ----------
      
      ### inputs -----
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
      
      if (input$start_date >= date_init){
        assets_data <- yf_data
      } 
      else{
        assets_data <- get_tq_data(tickers =  tickers, 
                                   start_date  = input$start_date)
      }
      
      ## value -----
      assets_value_list <- compute_assets_value(data = assets_data, 
                                           num_shares = num_shares) 
      assets_value <- assets_value_list %>%
        bind_rows() %>%
        filter(date >= input$start_date) 
      port_value <- get_portfolio_value(assets_value)
      
      output$port_current_val <- renderInfoBox({
        val <- get_portfolio_current_value(port_value) 
        infoBox(title = "Current value",
                value = val,
                color = "light-blue", 
                icon = tags$i(class = "fas fa-dollar-sign", 
                              style = "font-size: 20px"), 
                fill = F)
        
      })
      
      ### cumulative returns -----
      returns <- assets_value %>%
        compute_daily_returns() %>%
        compute_weighted_returns(num_shares = num_shares) 
      
      port_cumret <- returns %>%
        compute_cumulative_returns()
      
      ### data viz -----
      output$portfolio_evolution <- renderPlotly({
        portfolio_evolution(port_value, 
                            port_cumret)
      })
      
      output$port_current_cumret <- renderInfoBox({
        last_cumret <- get_current_cumret(port_cumret)
        infoBox_port_cumret(last_cumret)
      })
      
      ## composition ----------
      
      ### current date -----
      output$current_date <- renderText({
        current_date <- assets_value %>%
          pull(date) %>%
          max() 
        format(current_date, "%B %d, %Y")
      })
      
      ### cumulative returns -----
      assets_cumret <- returns %>%
        compute_cumulative_returns(all = F)
      
      ### data viz -----
      output$portfolio_composition <- renderPlotly({
        assets_value %>%
          filter(date >= input$start_date) %>%
          portfolio_composition()
      })
      
      output$best_asset_cumret <- renderInfoBox({
        best_asset <- get_best_asset(assets_cumret) 
        infoBox_asset_cumret(best_asset, type = "best")
      })
      
      output$worst_asset_cumret <- renderInfoBox({
        worst_asset <- get_worst_asset(assets_cumret) 
        infoBox_asset_cumret(worst_asset, type = "worst")
      })
      
      
      
      ## financial indicators ----------
      prices <- assets_data[[input$ticker]] %>%
        filter(date >= input$start_date) %>%
        add_moving_avg(window = 20) %>%
        add_moving_avg(window = 50) %>%
        add_moving_avg(window = 100) %>%
        add_macd() %>%
        add_rsi()
      
      ### candlestick with MAs -----
      output$candlestick_plot <- renderPlotly({
        prices %>%
          candlestick_chart(ticker = input$ticker) 
      })
      
      ### MACD -----
      output$macd_plot <- renderPlotly({
        prices %>%
          macd_chart(ticker = input$ticker) 
      })
      
      ### RSI -----
      output$rsi_plot <- renderPlotly({
        prices %>%
          rsi_chart(ticker = input$ticker) 
      })
      
      
      ## predictions ----------
      
      ### format data -----
      load(file = pred_path)
      predictions <- get_predicted_data(predictions, 
                                        tickers,
                                        num_shares)
      final_assets_value <- add_predictions(observed_dat = assets_value_list, 
                                           predicted_dat = predictions, 
                                           ticker = tickers)
      
      ### portfolio predicted value ----
      portfolio_value_pred <- get_portfolio_value(assets_value = final_assets_value)
      
      output$portfolio_pred_plot <- renderPlotly({
        plot_portfolio_predictions(portfolio_value_pred, 
                                   start_date = input$pred_start_date) 
      })
      
    }
  )
  
}

shinyApp(ui = ui, server = server)
