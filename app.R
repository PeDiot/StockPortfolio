# Setup -------------------------------------------------------------------

source("Rpackages.R")
source("setup.R", 
       encoding = "UTF-8")

# Data --------------------------------------------------------------------

date_init <- today() - years(5)
yf_data <- get_tq_data(tickers = tickers, 
                       start_date = date_init) 

# save_data_list(df_list = yf_data)

# Predict new values for each stock --------------------------------------------------------------------

## Virtual environment -----

# source("virtualenv.R", encoding = "UTF-8")

## Launch Python script -----

# reticulate::py_run_file("stock_prediction.py")

## Predictions -----

load(file = "./backup/stock_predictions.RData") 

# User Interface ----------------------------------------------------------


ui <- fluidPage(
  
  useShinydashboard(), 
  html_dependency_awesome(),
  
  tags$head(tags$style(infoBox_dims())),
  
  navbarPage("Your Stock Portfolio", 
             theme = shinytheme("lumen"), 
             
             ## home ----------
             tabPanel(title = "", 
                      icon = icon("home"), 
                      h1("Welcolme To Your Stock Portfolio!", align = "center"),
                      br(), 
                      br(), 
                      br(), 
                      br(), 
                      br(), 
                      br(), 
                      column(width = 12, 
                             imageOutput("home_img"), 
                             align = "center"), 
                      br(),
                      br(), 
                      br(), 
                      br(), 
                      br(), 
                      br(), 
                      br(), 
                      br(), 
                      br(), 
                      br(),
                      br(), 
                      br(), 
                      hr(), 
                      br(), 
                      br(), 
                      br(), 
                      br(), 
                      br(), 
                      br(), 
                      hr(), 
                      h4(strong("About the app")),
                      p(style="text-align: justify; font-size = 25px",
                        "This stock portfolio ",
                        strong("monitoring tool"), 
                        " will help you ", 
                        strong("boosting your returns."),  
                        "The app provides multiple ", 
                        strong("data vizualisations "),
                        "and ", 
                        strong("financial analysis."), 
                        " The input data comes from the ", 
                        a(href = "https://fr.finance.yahoo.com/", "Yahoo Finance"),
                        " website. Ten French companies have been selected.
                        Feel free to modify the source code in order to select your own assets. 
                        Go to ",
                        a(href = "https://github.com/PeDiot/StockPortfolio",
                          icon("github")),
                        " to find more details on the project."), 
                      hr()), 
             
             ## portfolio value ----------
             tabPanel("Portfolio Value", 
                      fluid = TRUE, 
                      icon = icon("dollar"), 
                      
                      sidebarLayout(
                        
                        ### inputs -----
                        sidebarPanel( 
                          width = 3, 
                          h3("Your portfolio"), 
                          br(), 
                          br(),
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
                                    value = today() - months(6),
                                    min = date_init, 
                                    max = today(),
                                    format = "yyyy-mm-dd")
                          
                        ), 
                        
                        mainPanel(
                          
                          tabsetPanel(
                            
                            ### portfolio overview -----
                            tabPanel("Overview",
                                     br(), 
                                     br(),
                                     fluidRow(column(width = 2), 
                                              infoBoxOutput("port_last_val"),
                                              column(width = 1), 
                                              infoBoxOutput("port_last_cumret")), 
                                     br(), 
                                     div(plotlyOutput("portfolio_evolution", 
                                                      height = 600, 
                                                      width = 600),
                                         align = "center")),
                            
                            ### portfolio composition -----
                            tabPanel("Composition",
                                     br(), 
                                     h4(paste( "Contribution of each asset to the portfolio value as of",
                                              format(Sys.Date(), "%d/%m/%Y") ), 
                                        align = "center"),
                                     div(plotlyOutput("portfolio_composition", 
                                                      height = 500, 
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
             
             ## financial analysis ----------
             
             tabPanel("Financial Analysis", 
                      fluid = TRUE, 
                      icon = icon("chart-line"), 
                      
                      sidebarLayout(
                        
                        ### inputs -----
                        sidebarPanel(
                          width = 3, 
                          h3(strong("Stock monitoring")), 
                          br(), 
                          br(), 
                          selectInput("ticker", 
                                      label = h4("Which asset?"), 
                                      width = "200px", 
                                      choices = tickers, 
                                      selected = "LVMH"),
                          br(), 
                          br(), 
                          br(), 
                          br(), 
                          br(), 
                          h4("When did you buy the stock?"), 
                          dateInput(inputId = "buy_date",
                                    label = "",
                                    width = "200px",  
                                    value = today() - months(6),
                                    min = date_init, 
                                    max = today(),
                                    format = "yyyy-mm-dd")
                        ),
                        
                        ### financial data viz -----
                        mainPanel( 
                          tabsetPanel(
                            tabPanel("Price & Returns", 
                                     br(), 
                                     br(), 
                                     fluidRow(infoBoxOutput("asset_num_shares", 
                                                            width = 4),
                                              infoBoxOutput("asset_last_price", 
                                                            width = 4),
                                              infoBoxOutput("asset_last_cumret", 
                                                            width = 4)), 
                                     br(), 
                                     div(plotlyOutput("asset_evolution", 
                                                      height = 600, 
                                                      width = 600),
                                         align = "center")),
                            tabPanel("Candlestick",
                                     br(), 
                                     br(), 
                                     br(), 
                                     br(), 
                                     br(), 
                                     br(), 
                                     div(plotlyOutput("candlestick_plot", 
                                                  height = 500, 
                                                  width = 700), 
                                         align = "center")),
                            tabPanel("Volatily",
                                     br(), 
                                     br(), 
                                     br(), 
                                     div(plotlyOutput("bbands_plot", 
                                                      height = 700, 
                                                      width = 700), 
                                         align = "center")),
                            
                            tabPanel("MACD",
                                     br(), 
                                     br(), 
                                     br(), 
                                     div(plotlyOutput("macd_plot", 
                                                  height = 700, 
                                                  width = 700), 
                                         align = "center")), 
                            tabPanel("RSI",
                                     br(), 
                                     br(), 
                                     br(), 
                                     div(plotlyOutput("rsi_plot", 
                                                  height = 700, 
                                                  width = 700), 
                                     align = "center"))
                          )
                        )
                      
                      )
                      
             ), 
          
             
             ## stock prediction ----------
             tabPanel("Prediction", 
                      fluid = TRUE, 
                      icon = icon("calendar"), 
                      
                      sidebarLayout(
                        
                        ### inputs -----
                        sidebarPanel(
                          width = 3, 
                          h3(strong("Prediction & Time Series Analysis")),
                          br(), 
                          br(), 
                          selectInput("ticker_pred", 
                                      label = h4("Which asset?"), 
                                      width = "200px", 
                                      choices = pred_input_choices, 
                                      selected = "All"),
                          br(), 
                          br(), 
                          br(), 
                          br(), 
                          br(), 
                          br(), 
                          dateInput(inputId = "pred_start_date",
                                    label = h4("How far back do you want to go?"),
                                    width = "200px",  
                                    value = today() - months(1),
                                    min = today() - months(3), 
                                    max = today(),
                                    format = "yyyy-mm-dd")
                        ), 
                        
                        ### predictions data viz -----
                        mainPanel(
                          br(), 
                          br(), 
                          br(),
                          fluidRow(column(width = 4),
                                   infoBoxOutput("avg_pred")),
                          br(),
                          br(), 
                          br(), 
                          br(), 
                          div(plotlyOutput("portfolio_pred_plot",
                                           height = 500, 
                                           width = 700), 
                              align = "center"), 
                          br(), 
                          br(), 
                          p(strong("Note"), 
                            ": the model used to predict the stock data is a neural networks implemented in Python with ", 
                            a(href = "https://www.tensorflow.org/?hl=fr", "tensorflow"), 
                            ". More information in the documentation.")
                        )
                      ) 
                      
              ), 
             
             ## Data ----------
             tabPanel(
               "Data", 
               fluid = TRUE, 
               icon = icon("database"), 
               
               sidebarLayout(
                 sidebarPanel(
                   width = 3, 
                   h3(strong("Financial data collection")),
                   br(), 
                   br(), 
                   selectInput("ticker_dat", 
                               label = h4("Which asset?"), 
                               width = "200px", 
                               choices = tickers, 
                               selected = "LVMH"),
                   br(), 
                   br(), 
                   br(), 
                   br(), 
                   br(), 
                   br(), 
                   dateInput(inputId = "dat_start_date",
                             label = h4("How far back do you want to go?"),
                             width = "200px",  
                             value = today() - months(6),
                             min = date_init, 
                             max = today(),
                             format = "yyyy-mm-dd"), 
                   br(), 
                   br(), 
                   br(), 
                   br(), 
                   br(), 
                   br(),  
                   downloadButton("download", "Download the data"),
                   br(), 
                   p("Data is extracted from ",
                     a(href = "https://fr.finance.yahoo.com/", "Yahoo Finance", .noWS = "outside"),
                     " with the ",
                     a(href = "https://www.rdocumentation.org/packages/tidyquant/versions/0.3.0", "tidyquant", .noWS = "outside"),
                     " package.", 
                     .noWS = c("after-begin", "before-end")) 
                 ), 
                 
                 mainPanel(
                   tabsetPanel(
                     tabPanel("Yahoo Finance", 
                              dataTableOutput(outputId = "data")), 
                     tabPanel("Returns", 
                              dataTableOutput(outputId = "ret_data")), 
                     tabPanel("Indicators", 
                              dataTableOutput(outputId = "indicators_data"))
                   ) 
                 )
                
              )
              
            ), 
            
            ## Documentation ----------
            navbarMenu(
              "Documentation", 
              icon = icon("book"), 
              
              tabPanel("Financial Analysis"), 
              tabPanel("Neural Networks")
              
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
      input$buy_date, 
      input$ticker_pred, 
      input$pred_start_date, 
      input$ticker_dat, 
      input$dat_start_date, 
      input$checkbox_returns), 
    {
      
      ## home image ----------
      output$home_img <- renderImage({
        
        list(src = "home_img.png", 
             width = 350,
             height = 350)
        
      }, deleteFile = F)
      
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

      assets_value_list <- compute_assets_value(data = yf_data, 
                                                num_shares = num_shares) 
      
      assets_value <- assets_value_list %>%
        bind_rows()
      
      ## portfolio value -----
      port_value <- get_portfolio_value(assets_value %>%
                                          filter(date >= input$start_date))
      
      output$port_last_val <- renderInfoBox({
        val <- get_current_value(data = port_value) 
        infoBox_last_value(last_val = val)
        
      })
      
      ### portfolio returns -----
      port_ret <- port_value %>%
        compute_daily_returns(asset_dat = NULL)
      
      port_cumret <- port_ret %>%
        compute_cumulative_returns()
      
      ### data viz -----
      output$portfolio_evolution <- renderPlotly({
        plot_evolution(price_dat = port_value, 
                       returns_dat = port_cumret) 
      })
      
      output$port_last_cumret <- renderInfoBox({
        last_cumret <- get_current_cumret(port_cumret)
        infoBox_port_cumret(last_cumret)
      })
      
      ## portfolio composition ----------
      
      ### best and worst assets -----
      assets_cumret <- assets_value %>%
        filter(date >= input$start_date) %>%
        compute_daily_returns() %>%
        compute_weighted_returns(num_shares = num_shares) %>%
        compute_cumulative_returns(all = F)
      
      best_asset <- get_best_asset(assets_cumret) 
      worst_asset <- get_worst_asset(assets_cumret) 
      
      ### data viz -----
      output$portfolio_composition <- renderPlotly({
        assets_value %>%
          filter(date >= input$start_date) %>%
          portfolio_composition()
      })
      
      output$best_asset_cumret <- renderInfoBox({
        infoBox_asset_cumret(best_asset, type = "best")
      })
      
      output$worst_asset_cumret <- renderInfoBox({
        infoBox_asset_cumret(worst_asset, type = "worst")
      })
      
      
      ## financial indicators per asset----------
      
      ### asset data with indicators -----
      prices <- assets_value_list[[input$ticker]] %>%
        filter(date >= input$buy_date) %>%
        add_moving_avg(window = 20) %>%
        add_moving_avg(window = 50) %>%
        add_moving_avg(window = 100) %>%
        add_macd() %>%
        add_rsi()
      
      ### asset returns -----
      daily_ret <- prices %>%
        compute_daily_returns() 
      weighted_ret <- daily_ret %>%
        compute_weighted_returns(num_shares = num_shares)
      asset_cumret <- weighted_ret %>%
        compute_cumulative_returns(all = F)
      
      ### asset global evolution -----
      output$asset_evolution <- renderPlotly({
        plot_evolution(price_dat = prices %>%
                         dplyr::select( c(date, close) ) %>%
                         rename(value = close), 
                       returns_dat = asset_cumret)
      })
      
      ### asset last price -----
      output$asset_last_price <- renderInfoBox({
        val <- get_current_price(data = prices)  
        infoBox_last_price(last_price = val)
        
      })
      
      ### asset number of shares ----
      output$asset_num_shares <- renderInfoBox({
        infoBox_num_shares(num_shares = num_shares[input$ticker])
      })
      
      
      ### asset last cumulative returns -----
      output$asset_last_cumret <- renderInfoBox({
        last_cumret <- get_current_cumret(asset_cumret)
        infoBox_port_cumret(last_cumret)
      })
      
      ### candlestick with MAs -----
      output$candlestick_plot <- renderPlotly({
        prices %>%
          candlestick_chart(ticker = input$ticker) 
      })
      
      ### bollinger bands -----
      bbands_dat <- calculate_bbands(price_data = prices)
      output$bbands_plot <- renderPlotly({
        bbands_chart(bbands_dat, input$ticker)
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
      predictions <- get_predicted_data(predictions, 
                                        tickers,
                                        num_shares)
      final_assets_value <- add_predictions(observed_dat = assets_value_list, 
                                            predicted_dat = predictions, 
                                            ticker = tickers, 
                                            merge = F)
      
      ### predicted values ----
      if (input$ticker_pred == "All"){
        plot_dat <- get_portfolio_value(assets_value = final_assets_value %>%
                                          bind_rows())
      } 
      else {
        plot_dat <- final_assets_value[[input$ticker_pred]]
      }
      
      ### visualize predicitons ----
      output$portfolio_pred_plot <- renderPlotly({
        plot_predictions(plot_dat, 
                         start_date = input$pred_start_date,
                         ticker = input$ticker_pred) 
      })
      
      ### prediction infoBox -----
      avg_pred <- compute_avg_pred(pred_dat = plot_dat, 
                                   ticker = input$ticker_pred)
      output$avg_pred <- renderInfoBox({
        infoBox_avg_pred(avg_pred, 
                         ticker = input$ticker_pred)
      })
      
      ## data -----
      dat <- assets_value_list[[input$ticker_dat]] %>%
        filter(date >= input$dat_start_date) %>%
        select(-c(n_shares, value))
      
      ret_dat <- dat %>%
        compute_daily_returns() %>%
        compute_weighted_returns(num_shares = num_shares) %>%
        compute_cumulative_returns(all = F) %>%
        select(c(ticker, 
                 date, 
                 ret, 
                 wt_return, 
                 cr)) %>%
        rename(returns = ret, 
               weighted_returns = wt_return, 
               cumulative_returns = cr)
      
      indicators_dat <- dat %>% 
        add_moving_avg(window = 20) %>%
        add_moving_avg(window = 50) %>%
        add_moving_avg(window = 100) %>%
        add_macd() %>%
        add_rsi() %>%
        select(c(ticker, 
                 date,
                 close, 
                 MA20:RSI)) 
      
      output$data <- renderDataTable( { dat %>% arrange(desc(date)) }, 
                                      options = list(pageLength = 10,
                                                    lengthMenu = c(10, 25, 50, 100)) )
      
      output$ret_data <- renderDataTable( { ret_dat %>% arrange(desc(date)) }, 
                                          options = list(pageLength = 10,
                                                        lengthMenu = c(10, 25, 50, 100)) )
      
      output$indicators_data <- renderDataTable( { indicators_dat %>% arrange(desc(date)) }, 
                                                 options = list(pageLength = 10,
                                                               lengthMenu = c(10, 25, 50, 100)) )
      
      output$download <- downloadHandler(
        filename <- function() {
          paste0(input$ticker_dat,
                 "_", 
                 input$dat_start_date, 
                 "_", 
                 today(), 
                 ".xlsx")
        },
        content <- function(file) {
          write_xlsx(x = list("Yahoo Finance Data" = dat, 
                              "Returns" = ret_dat, 
                              "Financial Indicators" = indicators_dat), 
                     path = file)
          }
      )
      
    }
  )
  
}

shinyApp(ui = ui, server = server)
