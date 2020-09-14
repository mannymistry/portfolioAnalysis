#' ConeChart for the asset or fund under investigation
#' Evaluate the returns of an investment manager giventheir return and volatility
#' expectations. Calculates 1 and 2 standard deviations from the expected return.
#' @param return_series : The return series of the asset or fund that you are planning to invest in
#' @param target_return : The target return you have for the asset or fund that you are planning to invest in
#' @param expected_volatility: The expected volatility you have for the asset or fund that you are planning to invest in
#' @param timeseries_frequency: Daily, Monthly, Weekly, Yearly
#'
#' @return
#' @export
#'
#' @examples Not Run
generate_cone_chart <- function(return_series, target_return, expected_volatility, timeseries_frequency){

  # Number of Observations
  n_obs <-nrow(return_series)

  # Calculate the Cumulative Sum of the 1+Log Returns = Actual Returns
  actual_returns <- cumsum(log(1+return_series))
  actual_return_df <- data.frame(date = zoo::index(actual_returns), zoo::coredata(actual_returns))

  # Store Annualized Target Return
  annualized_target_return <- target_return
  expected_vol <- expected_volatility

  # Target Return Adjusted For the Frequency of the data
  adjusted_target_return <- (1+annualized_target_return)^(1/PerformanceAnalytics::Frequency(actual_returns))-1

  # Adjusted Expected Volatility - Take into the frequency
  adjusted_expected_vol <- expected_vol/sqrt(PerformanceAnalytics::Frequency(actual_returns))

  # Cumulative Sum of the 1+Log target returns
  target_returns <- cumprod(rep(adjusted_target_return+1, n_obs))-1

  # One Standard Deviation
  standard_deviation <- c(adjusted_expected_vol * sqrt(1:n_obs))

dat <- tibble::tibble(
  date = actual_return_df$date,
  'return path' = actual_return_df[,2],
  'expected return path' = log(1+target_returns),
  'up 1 sigma' = (log((1+target_returns)) + standard_deviation),
  'up 2 sigma' = (log((1+target_returns)) + (2*standard_deviation)),
  'down 1 sigma' = (log((1+target_returns)) - standard_deviation),
  'down 2 sigma' = (log((1+target_returns)) - (2*standard_deviation)))


# TODO: Adjust the titles on the graph
# TODO: Make it look a bit prettier.
plot_dat <- reshape2::melt(dat, id = "date")
plot_dat$variable <- factor(x = plot_dat$variable,
                            levels = c("return path", "expected return path", "up 1 sigma",
                                       "up 2 sigma", "down 1 sigma","down 2 sigma"))

ggplot2::ggplot(data = plot_dat, aes(x = date, y = value , color = variable))+
  geom_line()+
  ggtitle("Cummulative Returns in a Cone")+
  labs(subtitle = paste0("ER = 0.10, EVol = 0.10"))+
    theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

}


