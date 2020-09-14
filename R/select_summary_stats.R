#' Generate A Table Of Selected Summary Statistics for Potential Investments
#'
#' @param return_series : The return series of the asset or fund that you are planning to invest in
#' @param target_return : The target return you have for the asset or fund that you are planning to invest in
#' @param rf_rate: The rf rate on your investment
#'
#' @return
#' @export
#'
#' @examples # Not Run generate_select_summary_statis
generate_select_summary_statistics <- function(return_series, target_returns, rf_rate){
   # Target and Rf Rate
  monthly_target_return <- target_returns
  monthly_rf_rate <- rf_rate

  # Monthly Vol
  monthly_vol <- format(PerformanceAnalytics::StdDev(return_series), digits = 2)

  # Annual Vol
  annual_vol <- format(PerformanceAnalytics::StdDev.annualized(return_series), digits = 2)

  # Avg Monthly Return
  monthly_average_return <- format(mean(return_series), digits = 2)

  # Avg Annual Return
  annual_average_return <- format(mean(xts::apply.yearly(1+return_series, prod)-1), digits = 2)

  # Sharpe Ratio
  # TODO: For some reason forces me to choose one element of the list
  sr <- format(PerformanceAnalytics::SharpeRatio(return_series, Rf= rf_rate[1], FUN = "StdDev"), digits = 2)
  annualized_sr <- format(PerformanceAnalytics::SharpeRatio.annualized(return_series, Rf= rf_rate[1]), digits = 2)

  # Sortino Ratio
  # Using the target return as the MAR
  # TODO: For some reason forces me to choose one element of the list
  sortino <- format(PerformanceAnalytics::SortinoRatio(return_series, target_returns[1]), digits = 2)

  # Drawdowns- The Table makes much more sense to include individual
  draw_downs <- t(PerformanceAnalytics::table.Drawdowns(return_series, top = 1))
  result <- format(as.data.frame(rbind(monthly_vol,
                                       annual_vol,
                                       monthly_average_return,
                                       annual_average_return,
                                       sr,
                                       annualized_sr,
                                       sortino,
                                       draw_downs)), nsmall = 2)
  # Betas to

  rownames(result) <- c("Monthly Volatility",
                        "Annual Volatility",
                        "Average Monthly Return",
                        "Average Annual Return",
                        "Monthly Sharpe Ratio",
                        "Annual Sharpe Ratio",
                        "Sortino Ratio",
                        "Drawdown Start",
                        "Drawdown Trough",
                        "Drawdown End",
                        "Drawdown Length",
                        "Drawdown",
                        "Periods to Drawdown Trough",
                        "Periods to Recovery")

  colnames(result) <- c("value")
  result <- tibble::rownames_to_column(result, "stat")

}
