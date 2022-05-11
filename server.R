server <- function(input, output, session) {
  

# User Authentification ---------------------------------------------------
#  res_auth <- secure_server(
#    check_credentials = check_credentials(credentials)
#  )
  
#  output$auth_output <- renderPrint({
#    reactiveValuesToList(res_auth)
#  })
  
# home page --------------------------------------------------------------
  
## home image --------------------------------------------------------------
  output$home_img <- renderImage({
    list(src = "home_img.png", 
         width = 300,
         height = 300)
    
  }, deleteFile = F)
  
# portfolio --------------------------------------------------------------
      
## inputs --------------------------------------------------------------
      observeEvent(
        c(# number of shares 
          input$`num_shares_BTC-EUR`,
          input$`num_shares_ETH-EUR`,
          input$`num_shares_MATIC-EUR`,
          input$`num_shares_MANA-EUR`,
          input$`num_shares_1QZ.F`, 
          input$`num_shares_AMZ.F`, 
          # buying dates
          input$`buying_date_BTC-EUR`,
          input$`buying_date_ETH-EUR`,
          input$`buying_date_MATIC-EUR`,
          input$`buying_date_MANA-EUR`,
          input$`buying_date_1QZ.F`, 
          input$`buying_date_AMZ.F`),
        {
          num_shares <- c(input$`num_shares_BTC-EUR`,
                          input$`num_shares_ETH-EUR`,
                          input$`num_shares_MATIC-EUR`,
                          input$`num_shares_MANA-EUR`,
                          input$`num_shares_1QZ.F`, 
                          input$`num_shares_AMZ.F`)
          names(num_shares) <- my_tickers
          
          buying_dates <- c(input$`buying_date_BTC-EUR`,
                            input$`buying_date_ETH-EUR`,
                            input$`buying_date_MATIC-EUR`,
                            input$`buying_date_MANA-EUR`, 
                            input$`buying_date_1QZ.F`, 
                            input$`buying_date_AMZ.F`)
          names(buying_dates) <- my_tickers
          
          assets_value_list <- compute_assets_value(data = yf_data, 
                                                    num_shares = num_shares) 
          
          my_assets_value <- my_tickers %>%
            lapply(FUN = query_assets_since_buying_date, 
                   assets_dat = assets_value_list[my_tickers],
                   buying_dates = buying_dates) %>%
            bind_rows()
           
## portfolio value --------------------------------------------------------------
          port_value <- get_portfolio_value(my_assets_value)
          
          output$port_last_val <- renderInfoBox({
            val <- get_current_value(data = port_value) 
            infoBox_last_value(last_val = val)
            
          })
          
### portfolio returns --------------------------------------------------------------
          date_selection <- buying_dates[buying_dates != min(buying_dates)]
          port_daily_ret <- port_value %>%
            compute_daily_returns(asset_dat = NULL) %>%
            filter( !(date %in% date_selection) )
          port_cumret <- port_daily_ret %>%
            compute_cumulative_returns()
          
### data viz --------------------------------------------------------------
          output$portfolio_evolution <- renderPlotly({
            plot_evolution(price_dat = port_value, 
                           cum_ret_dat = port_cumret) 
          })
          
          max_date <- max(buying_dates)
          output$port_last_cumret <- renderInfoBox({
            last_cumret <- get_current_cumret(port_cumret)
            infoBox_last_cumret(last_cumret)
          })
          
## portfolio composition --------------------------------------------------------------
          
### best and worst assets --------------------------------------------------------------
          assets_cumret <- my_assets_value %>%
            compute_daily_returns() %>%
            compute_cumulative_returns(all = F)
          
          best_asset <- get_best_asset(assets_cumret) 
          worst_asset <- get_worst_asset(assets_cumret) 
          
### data viz --------------------------------------------------------------
          output$portfolio_composition <- renderPlotly({
            my_assets_value %>%
              portfolio_composition()
          })
          
          output$best_asset_cumret <- renderInfoBox({
            infoBox_asset_cumret(best_asset, type = "best")
          })
          
          output$worst_asset_cumret <- renderInfoBox({
            infoBox_asset_cumret(worst_asset, type = "worst")
          })
          
## portfolio data table --------------------------------------------------------------
          port_dat <- merge(x = port_value, 
                            y = port_cumret, 
                            by = "date") %>%
            arrange(desc(date)) %>%
            rename(`daily returns` = ret, 
                   `cumulative returns` = cr)
          output$port_data <- renderDataTable({port_dat}, 
                                              options = list(pageLength = 10,
                                                             lengthMenu = c(10, 25, 50, 100)) )
        }
      )
      
# financial indicators per asset --------------------------------------------------------------
      observeEvent(
        c(# number of shares 
          input$`num_shares_BTC-EUR`,
          input$`num_shares_ETH-EUR`,
          input$`num_shares_MATIC-EUR`,
          input$`num_shares_MANA-EUR`,
          input$`num_shares_1QZ.F`, 
          input$`num_shares_AMZ.F`, 
          # buying dates
          input$`buying_date_BTC-EUR`,
          input$`buying_date_ETH-EUR`,
          input$`buying_date_MATIC-EUR`,
          input$`buying_date_MANA-EUR`,
          input$`buying_date_1QZ.F`, 
          input$`buying_date_AMZ.F`, 
          # 
          input$ticker_fin_analysis, 
          input$start_date_fin_analysis),
        {
          
## inputs --------------------------------------------------------------
          
          num_shares <- c(input$`num_shares_BTC-EUR`,
                          input$`num_shares_ETH-EUR`,
                          input$`num_shares_MATIC-EUR`,
                          input$`num_shares_MANA-EUR`,
                          input$`num_shares_1QZ.F`, 
                          input$`num_shares_AMZ.F`)
          names(num_shares) <- my_tickers
          
          buying_dates <- c(input$`buying_date_BTC-EUR`,
                            input$`buying_date_ETH-EUR`,
                            input$`buying_date_MATIC-EUR`,
                            input$`buying_date_MANA-EUR`, 
                            input$`buying_date_1QZ.F`, 
                            input$`buying_date_AMZ.F`)
          names(buying_dates) <- my_tickers
          
          assets_value_list <- compute_assets_value(data = yf_data, 
                                                    num_shares = num_shares) 
          
## start date financial analysis --------------------------------------------------------------
          output$start_date_fin_analysis_out <- renderUI({
            
            if (input$ticker_fin_analysis %in% my_tickers){
              date <- today() - days(35)
              if (buying_dates[input$ticker_fin_analysis] < date){
                date <- buying_dates[input$ticker_fin_analysis] 
              }
            }
            else{
              date <- today() %m-% months(6)
            }
            
            airDatepickerInput(
              inputId = "start_date_fin_analysis",
              label = h5("How far back do you want to go?", 
                         style = "color:#76787B;"),
              value = date,
              minDate = date_init, 
              maxDate = today() - days(35),
              width = "200px",
              placeholder = "",
              multiple = F, 
              clearButton = F)
            
          })
          
## asset data with indicators --------------------------------------------------------------
          req(input$start_date_fin_analysis)
          prices <- assets_value_list[[input$ticker_fin_analysis]] %>%
            filter( date >= input$start_date_fin_analysis) %>%
            add_moving_avg(window = 20) %>%
            add_moving_avg(window = 50) %>%
            add_moving_avg(window = 100) %>%
            add_macd() %>%
            add_rsi()
          
## asset returns --------------------------------------------------------------
          daily_ret <- prices %>%
            compute_daily_returns() 
          asset_cumret <- daily_ret %>%
            compute_cumulative_returns(all = F)
          
## asset global evolution --------------------------------------------------------------
          output$asset_evolution <- renderPlotly({
            plot_evolution(price_dat = prices %>%
                             dplyr::select(c(date, close)) %>%
                             rename(value = close), 
                           cum_ret_dat = asset_cumret)
          })
          
## asset last price --------------------------------------------------------------
          output$asset_last_price <- renderInfoBox({
            val <- get_current_price(data = prices)  
            infoBox_last_price(last_price = val)
            
          })
          
## asset number of shares --------------------------------------------------------------
          output$asset_num_shares <- renderInfoBox({
            if (input$ticker_fin_analysis %in% my_tickers){
              num_shares <- num_shares[input$ticker_fin_analysis]
            }
            else{
              num_shares <- 0
            }
            infoBox_num_shares(num_shares)
          })
          
          
## asset last cumulative returns --------------------------------------------------------------
          output$asset_last_cumret <- renderInfoBox({
            last_cumret <- get_current_cumret(asset_cumret)
            infoBox_last_cumret(last_cumret)
          })
          
## candlestick with MAs --------------------------------------------------------------
          output$candlestick_plot <- renderPlotly({
            prices %>%
              candlestick_chart(ticker = input$ticker_fin_analysis) 
          })
          
## bollinger bands --------------------------------------------------------------
          bbands_dat <- calculate_bbands(price_data = prices)
          output$bbands_plot <- renderPlotly({
            bbands_chart(bbands_dat, input$ticker_fin_analysis)
          })
          
## MACD --------------------------------------------------------------
          output$macd_plot <- renderPlotly({
            prices %>%
              macd_chart(ticker = input$ticker_fin_analysis) 
          })
          
## RSI --------------------------------------------------------------
          output$rsi_plot <- renderPlotly({
            prices %>%
              rsi_chart(ticker = input$ticker_fin_analysis) 
          })
          
        }
      )
      
# data --------------------------------------------------------------
      observeEvent(
        c(# number of shares 
          input$`num_shares_BTC-EUR`,
          input$`num_shares_ETH-EUR`,
          input$`num_shares_MATIC-EUR`,
          input$`num_shares_MANA-EUR`,
          input$`num_shares_1QZ.F`, 
          input$`num_shares_AMZ.F`, 
          # buying dates
          input$`buying_date_BTC-EUR`,
          input$`buying_date_ETH-EUR`,
          input$`buying_date_MATIC-EUR`,
          input$`buying_date_MANA-EUR`,
          input$`buying_date_1QZ.F`, 
          input$`buying_date_AMZ.F`, 
          # 
          input$ticker_dat, input$start_date_dat),
        {
          
## inputs --------------------------------------------------------------
          
          num_shares <- c(input$`num_shares_BTC-EUR`,
                          input$`num_shares_ETH-EUR`,
                          input$`num_shares_MATIC-EUR`,
                          input$`num_shares_MANA-EUR`,
                          input$`num_shares_1QZ.F`, 
                          input$`num_shares_AMZ.F`)
          names(num_shares) <- my_tickers
          
          buying_dates <- c(input$`buying_date_BTC-EUR`,
                            input$`buying_date_ETH-EUR`,
                            input$`buying_date_MATIC-EUR`,
                            input$`buying_date_MANA-EUR`, 
                            input$`buying_date_1QZ.F`, 
                            input$`buying_date_AMZ.F`)
          names(buying_dates) <- my_tickers
          
          assets_value_list <- compute_assets_value(data = yf_data, 
                                                    num_shares = num_shares) 
          
## start date data --------------------------------------------------------------
          
          output$start_date_dat_out <- renderUI({
            
            if (input$ticker_dat %in% my_tickers){
              date <- today() - days(35)
              if (buying_dates[input$ticker_dat] < date){
                date <- buying_dates[input$ticker_dat] 
              }
            }
            else{
              date <- today() %m-% months(6)
            }
            
            airDatepickerInput(
              inputId = "start_date_dat",
              label = h5("How far back do you want to go?", 
                         style = "color:#76787B;"),
              value = date,
              minDate = date_init, 
              maxDate = today(),
              width = "200px", 
              placeholder = "",
              multiple = F, 
              clearButton = F)
            
          })
          
## yf data --------------------------------------------------------------
          req(input$start_date_dat)
          dat <- assets_value_list[[input$ticker_dat]] %>%
            filter(date >= input$start_date_dat) %>%
            select(-c(n_shares, value))
          
## returns --------------------------------------------------------------
          ret_dat <- dat %>%
            compute_daily_returns() %>%
            compute_cumulative_returns(all = F) %>%
            select(c(ticker, 
                     date, 
                     ret,
                     cr)) %>%
            rename(`daily returns` = ret, 
                   `cumulative returns` = cr)
          
## indicators --------------------------------------------------------------      
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
          
## data tables --------------------------------------------------------------
          output$data <- renderDataTable( { dat %>% arrange(desc(date)) }, 
                                          options = list(pageLength = 10,
                                                         lengthMenu = c(10, 25, 50, 100)) )
          
          output$ret_data <- renderDataTable( { ret_dat %>% arrange(desc(date)) }, 
                                              options = list(pageLength = 10,
                                                             lengthMenu = c(10, 25, 50, 100)) )
          
          output$indicators_data <- renderDataTable( { indicators_dat %>% arrange(desc(date)) }, 
                                                     options = list(pageLength = 10,
                                                                    lengthMenu = c(10, 25, 50, 100)) )
          
## download -------------------------------------------------------------- 
          output$download <- downloadHandler(
            filename <- function() {
              paste0(input$ticker_dat,
                     "_", 
                     input$start_date_dat, 
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
      
# stock recommender --------------------------------------------------------------
      
      observeEvent(
        c(# number of shares 
          input$`num_shares_BTC-EUR`,
          input$`num_shares_ETH-EUR`,
          input$`num_shares_MATIC-EUR`,
          input$`num_shares_MANA-EUR`,
          input$`num_shares_1QZ.F`, 
          input$`num_shares_AMZ.F`, 
          # buying dates
          input$`buying_date_BTC-EUR`,
          input$`buying_date_ETH-EUR`,
          input$`buying_date_MATIC-EUR`,
          input$`buying_date_MANA-EUR`,
          input$`buying_date_1QZ.F`, 
          input$`buying_date_AMZ.F`,
          #
          input$indicator, 
          input$recommendation_start_date, 
          input$action), 
        {
          
## inputs --------------------------------------------------------------
          
          num_shares <- c(input$`num_shares_BTC-EUR`,
                          input$`num_shares_ETH-EUR`,
                          input$`num_shares_MATIC-EUR`,
                          input$`num_shares_MANA-EUR`,
                          input$`num_shares_1QZ.F`, 
                          input$`num_shares_AMZ.F`)
          names(num_shares) <- my_tickers
          
          buying_dates <- c(input$`buying_date_BTC-EUR`,
                            input$`buying_date_ETH-EUR`,
                            input$`buying_date_MATIC-EUR`,
                            input$`buying_date_MANA-EUR`, 
                            input$`buying_date_1QZ.F`, 
                            input$`buying_date_AMZ.F`)
          names(buying_dates) <- my_tickers
          
## compute recommendations --------------------------------------------------------------
          if (length(input$indicator) == 2){
            criterion <- "MACD+RSI"
          }
          else{
            criterion <- input$indicator
          }
          
          recommendation <- stock_recommender(stock_data = yf_data, 
                                              action = input$action,       
                                              start_date = input$recommendation_start_date, 
                                              criterion = criterion, 
                                              num_shares = num_shares, 
                                              buying_dates = buying_dates) %>% select(-date)
          recommended_tickers <- recommendation$ticker 
          
## recommended stocks table --------------------------------------------------------------
          output$recommendation_data <- renderDataTable( { 
            new_tickers <- lapply(recommended_tickers, 
                                  FUN = function(x){
                                    sym <- symbols %>% filter(tickers == x) %>% rownames()
                                    paste0(sym, " (", x, ")")
                                  }) %>% unlist()
            recommendation %>% 
              mutate(ticker = new_tickers) 
          }, 
          options = list(pageLength = 5,
                         lengthMenu = c(1, 3, 5)) 
          )
          
## recommendation table title --------------------------------------------------------------
          output$recommendation_tab_title <- renderUI({
            if (input$action == "Buy"){
              title <- "Interesting stocks to buy as of"
            }
            else{
              title <- "Stocks you could sell for a profit as of"
            }
            h4(strong(paste(title, format(today(), "%B %d, %Y"))), 
               align = "center", 
               style = "color:#76787B;")
          })
          
## cumulative returns chart --------------------------------------------------------------
          
          output$recommendation_chart <- renderPlotly({
            
            if (length(recommended_tickers) == 0){
              plotly_empty()
            }
            
            else{
              
              cumrets <- yf_data[recommended_tickers] %>%
                bind_rows() %>% 
                filter(date >= input$recommendation_start_date) %>% 
                compute_daily_returns() %>%
                compute_cumulative_returns(all = F)
              
              if (length(recommended_tickers) == 1){
                cumrets %>%
                  plot_ly() %>%
                  plot_cumulative_returns(legend_group = NULL)
              }
              else{
                cumrets %>%
                  plot_ly() %>%
                  plot_cumulative_returns(multiple = T,
                                          legend_group = NULL)
              }
            }
            
          })
          
          
        }
      )
    
}
      
      