ui <- fluidPage(
  
  useShinydashboard(), 
  html_dependency_awesome(),
  
  tags$head(tags$style(infoBox_dims())),
  
  navbarPage("Your Stock Portfolio", 
             theme = shinytheme("lumen"), 
             

# home --------------------------------------------------------------------
             
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
                      strong("data visualizations "),
                      "and ", 
                      strong("financial analysis."), 
                      " The input data comes from the ", 
                      a(href = "https://fr.finance.yahoo.com/", "Yahoo Finance"),
                      " website. 
                                Feel free to modify the source code in order to select your own assets. 
                                Go to ",
                      a(href = "https://github.com/PeDiot/StockPortfolio",
                      icon("github")),
                      " to find more details on the project."), 
                      hr()), 


# portfolio value ---------------------------------------------------------

             
              tabPanel("Portfolio Value", 
                      fluid = TRUE, 
                      icon = icon("euro"), 
                      
                      sidebarLayout(
                        
## inputs ---------------------------------------------------------
                        sidebarPanel( 
                          width = 3, 
                          h3(strong("Your portfolio")), 
                          hr(),
                          h5(strong("LVMH")), 
                          asset_inputs(asset = "LVMH"),
                          h5(strong("L'Oréal")), 
                          asset_inputs(asset = "L'Oréal", 
                                       buying_date = today() - months(1)),
                          h5(strong("Air Liquide")), 
                          asset_inputs(asset = "Air Liquide"),
                          h5(strong("Orange")), 
                          asset_inputs(asset = "Orange"), 
                          br(), 
                          hr(), 
                          p(a(href = "https://github.com/PeDiot/StockPortfolio", icon("github")), 
                            " Feel free to modify the source code to select your own assets.")
                        ), 
                        
                        mainPanel(
                          
                          tabsetPanel(
                            
## portfolio overview ---------------------------------------------------------
                            tabPanel("Overview",
                                     br(), 
                                     br(),
                                     fluidRow(column(width = 2), 
                                              infoBoxOutput("port_last_val"),
                                              column(width = 1), 
                                              infoBoxOutput("port_last_cumret")), 
                                     hr(), 
                                     div(plotlyOutput("portfolio_evolution", 
                                                      height = 630, 
                                                      width = 750),
                                         align = "center")),
                            
## portfolio composition ---------------------------------------------------------
                            tabPanel("Composition",
                                     br(), 
                                     h4("Contribution of each asset to the portfolio value", 
                                        align = "center"),
                                     div(plotlyOutput("portfolio_composition", 
                                                      height = 500, 
                                                      width = 800), 
                                         align = "center"), 
                                     br(),
                                     hr(), 
                                     h4("Your best and worst assets", 
                                        align = "center"),
                                     br(), 
                                     fluidRow(column(width = 2), 
                                              infoBoxOutput("best_asset_cumret"),
                                              column(width = 1), 
                                              infoBoxOutput("worst_asset_cumret"))) 
                          )
                          
                        )
                        
                      )
                      
             ),
             
# financial analysis ---------------------------------------------------------
             
             tabPanel("Financial Analysis", 
                      fluid = TRUE, 
                      icon = icon("chart-line"), 
                      
                      sidebarLayout(
                        
                        ## inputs ---------------------------------------------------------
                        sidebarPanel(
                          width = 3, 
                          h3(strong("Stock monitoring")), 
                          hr(), 
                          br(), 
                          pickerInput("ticker", 
                                      label = h4("Which asset?"), 
                                      width = "200px", 
                                      choices = list(Stocks = stock_tickers,
                                                     Cryptocurrencies = crypto_tickers), 
                                      choicesOpt = list(style = picker_inputs_font_weight()),
                                      multiple = F, 
                                      options = list(`live-search` = TRUE)),
                          br(), 
                          br(), 
                          br(), 
                          br(), 
                          br(), 
                          h4("Enter your start date"), 
                          dateInput(inputId = "buy_date",
                                    label = "",
                                    width = "200px",  
                                    value = today() - months(6),
                                    min = date_init, 
                                    max = today() - days(35),
                                    format = "yyyy-mm-dd")
                        ),
                        
                        ## financial data viz ---------------------------------------------------------
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
                                     hr(), 
                                     div(plotlyOutput("asset_evolution", 
                                                      height = 630, 
                                                      width = 750),
                                         align = "center")),
                            tabPanel("Candlestick",
                                     br(), 
                                     br(), 
                                     br(), 
                                     br(), 
                                     div(plotlyOutput("candlestick_plot", 
                                                      height = 600, 
                                                      width = 800), 
                                         align = "center"), 
                                     br(), 
                                     hr(), 
                                     p(icon(name = "book"), 
                                       "  More information on candlestick charts can be found ", 
                                       a(href = "https://www.investopedia.com/trading/candlestick-charting-what-is-it/", "here"), 
                                       ".")),
                            tabPanel("Volatily",
                                     br(), 
                                     br(),
                                     div(plotlyOutput("bbands_plot", 
                                                      height = 660, 
                                                      width = 800), 
                                         align = "center"), 
                                     hr(), 
                                     p(icon(name = "book"), 
                                       "  More information on Bollinger bands can be found ", 
                                       a(href = "https://www.investopedia.com/terms/b/bollingerbands.asp", "here"), 
                                       ".")),
                            
                            tabPanel("MACD",
                                     br(), 
                                     br(), 
                                     div(plotlyOutput("macd_plot", 
                                                      height = 660, 
                                                      width = 800), 
                                         align = "center"),
                                     hr(), 
                                     p(icon(name = "book"), 
                                       "  More information on the MACD indicator can be found ", 
                                       a(href = "https://www.investopedia.com/terms/m/macd.asp", "here"), 
                                       ".")),
                            tabPanel("RSI",
                                     br(), 
                                     br(), 
                                     div(plotlyOutput("rsi_plot", 
                                                      height = 660, 
                                                      width = 800), 
                                         align = "center"),
                                     hr(), 
                                     p(icon(name = "book"), 
                                       "  More information on the RSI indicator can be found ", 
                                       a(href = "https://www.investopedia.com/terms/r/rsi.asp", "here"), 
                                       ".")),
                            
                          )
                        )
                        
                      )
                      
             ), 

# prediction ---------------------------------------------------------

              tabPanel("Prediction", 
                       fluid = TRUE, 
                       icon = icon("calendar"), 
                       
                       sidebarLayout(
                         
## inputs ---------------------------------------------------------
                         sidebarPanel(
                           width = 3, 
                           h3(strong("Stock Prediction")),
                           hr(), 
                           br(), 
                           pickerInput("ticker_pred", 
                                       label = h4("Which asset?"), 
                                       width = "200px", 
                                       choices = pred_input_choices, 
                                       selected = "All", 
                                       multiple = F, 
                                       options = list(`live-search` = TRUE)), 
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
                         
## predictions data viz ---------------------------------------------------------
                         mainPanel(
                           br(), 
                           br(), 
                           fluidRow(column(width = 4),
                                    infoBoxOutput("avg_pred")),
                           hr(),
                           br(), 
                           div(plotlyOutput("portfolio_pred_plot",
                                            height = 450, 
                                            width = 700), 
                               align = "center"), 
                           br(), 
                           br(),
                           br(), 
                           br(),
                           br(), 
                           br(),
                           hr(),
                           p(icon(name = "book"), 
                             " The model used to predict the stock data is a neural network implemented in Python with ", 
                             a(href = "https://www.tensorflow.org/?hl=fr", "tensorflow"), 
                             ".")
                         )
                       ) 
                       
              ), 
             
# data ---------------------------------------------------------
             tabPanel(
               "Data", 
               fluid = TRUE, 
               icon = icon("database"), 
               
               sidebarLayout(
                 sidebarPanel(
                   width = 3, 
                   h3(strong("Financial data collection")),
                   hr(), 
                   br(), 
                   pickerInput("ticker_dat", 
                               label = h4("Which asset?"), 
                               width = "200px", 
                               choices = list(Stocks = stock_tickers,
                                              Cryptocurrencies = crypto_tickers), 
                               choicesOpt = list(style = picker_inputs_font_weight()),
                               multiple = F, 
                               options = list(`live-search` = TRUE)),
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
               
             )
             
  )
  
)
