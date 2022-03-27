server <- function(input, output) {
  
# home page --------------------------------------------------------------
  
## home image --------------------------------------------------------------
  output$home_img <- renderImage({
    list(src = "./figs/home_img.png", 
         width = 300,
         height = 300)
    
  }, deleteFile = F)
  
# observe events --------------------------------------------------------------
  observeEvent(
    c(input$num_shares_LVMUY,
      input$num_shares_OR.PA,
      input$num_shares_AI.PA,
      input$num_shares_ORAN,
      input$buying_date_LVMUY, 
      input$buying_date_OR.PA, 
      input$buying_date_AI.PA, 
      input$buying_date_ORAN, 
      input$ticker,
      input$buy_date, 
      input$ticker_pred, 
      input$pred_start_date, 
      input$ticker_dat, 
      input$dat_start_date, 
      input$checkbox_returns),
    {
      
# portfolio --------------------------------------------------------------
      
## inputs --------------------------------------------------------------
      num_shares <- c(input$num_shares_LVMUY,
                      input$num_shares_OR.PA,
                      input$num_shares_AI.PA,
                      input$num_shares_ORAN)
      names(num_shares) <- my_tickers
      
      buying_dates <- c(input$buying_date_LVMUY,
                       input$buying_date_OR.PA,
                       input$buying_date_AI.PA,
                       input$buying_date_ORAN)
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
      port_ret <- port_value %>%
        compute_daily_returns(asset_dat = NULL)
      
      port_cumret <- port_ret %>%
        compute_cumulative_returns()
      
### data viz --------------------------------------------------------------
      output$portfolio_evolution <- renderPlotly({
        plot_evolution(price_dat = port_value, 
                       returns_dat = port_cumret) 
      })
      
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
      
      
# financial indicators per asset --------------------------------------------------------------
      
## asset data with indicators --------------------------------------------------------------
      prices <- assets_value_list[[input$ticker]] %>%
        filter(date >= input$buy_date) %>%
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
                         dplyr::select( c(date, close) ) %>%
                         rename(value = close), 
                       returns_dat = asset_cumret)
      })
      
## asset last price --------------------------------------------------------------
      output$asset_last_price <- renderInfoBox({
        val <- get_current_price(data = prices)  
        infoBox_last_price(last_price = val)
        
      })
      
## asset number of shares --------------------------------------------------------------
      output$asset_num_shares <- renderInfoBox({
        if (input$ticker %in% my_tickers){
          num_shares <- num_shares[input$ticker]
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
          candlestick_chart(ticker = input$ticker) 
      })
      
## bollinger bands --------------------------------------------------------------
      bbands_dat <- calculate_bbands(price_data = prices)
      output$bbands_plot <- renderPlotly({
        bbands_chart(bbands_dat, input$ticker)
      })
      
## MACD --------------------------------------------------------------
      output$macd_plot <- renderPlotly({
        prices %>%
          macd_chart(ticker = input$ticker) 
      })
      
## RSI --------------------------------------------------------------
      output$rsi_plot <- renderPlotly({
        prices %>%
          rsi_chart(ticker = input$ticker) 
      })
      
# predictions --------------------------------------------------------------
      
## format data --------------------------------------------------------------
      predictions <- get_predicted_data(predictions, 
                                        my_tickers,
                                        num_shares)
      final_assets_value <- add_predictions(observed_dat = assets_value_list[my_tickers], 
                                            predicted_dat = predictions, 
                                            ticker = my_tickers, 
                                            merge = F)
      
## predicted values --------------------------------------------------------------
      if (input$ticker_pred == "All"){
        plot_dat <- get_portfolio_value(assets_value = final_assets_value %>%
                                          bind_rows())
      } 
      else {
        plot_dat <- final_assets_value[[input$ticker_pred]]
      }
      
## visualize predictions --------------------------------------------------------------
      output$portfolio_pred_plot <- renderPlotly({
        plot_predictions(plot_dat, 
                         start_date = input$pred_start_date,
                         ticker = input$ticker_pred) 
      })
      
## prediction infoBox --------------------------------------------------------------
      avg_pred <- compute_avg_pred(pred_dat = plot_dat, 
                                   ticker = input$ticker_pred)
      output$avg_pred <- renderInfoBox({
        infoBox_avg_pred(avg_pred, 
                         ticker = input$ticker_pred)
      })
      
# data --------------------------------------------------------------
      
## yf data --------------------------------------------------------------
      dat <- assets_value_list[[input$ticker_dat]] %>%
        filter(date >= input$dat_start_date) %>%
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
