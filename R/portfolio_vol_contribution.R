# Much of this adapted from
# https://rviews.rstudio.com/2017/09/13/asset-contribution-to-portfolio-volatility/

#' Calculate and Graph "Percent contribution to volatility"
#'
#' @param weights: A numeric vector indicating the weight of the investment in asset
#' @param portfolioComponentReturns: an xts of portfolio returns - ensure that
#'
#' @return
#' @export
#'
#' @examples Not Run
portfolio_vol_contribution <- function(weights, portfolioComponentReturns){
# Get a nice even dataframe
portfolioComponentReturns <- na.omit(portfolioComponentReturns)
# Build the Covariance matrix
covariance_matrix <- cov(portfolioComponentReturns)
# Calculate the portfolio standard deviation
sd_portfolio <- sqrt(t(weights) %*% covariance_matrix %*% weights)
# Marginal contribution of each asset.
marginal_contribution <- weights %*% covariance_matrix / sd_portfolio[1, 1]
# Component contributions to risk are the weighted marginal contributions
component_contribution <- marginal_contribution * weights
# To get the percentage contribution, divide component contribution by total sd.
component_percentages <- component_contribution / sd_portfolio[1, 1]

percentage_tibble_by_hand <-
  tibble::tibble(colnames(managers), weights, as.vector(component_percentages)) %>%
  rename(asset = "colnames(managers)", 'portfolio weight' = weights,
         'risk contribution' = `as.vector(component_percentages)`)

percentage_tibble_by_hand_gather <-
  percentage_tibble_by_hand %>%
  gather(type, percent, -asset)

ggplot2::ggplot(percentage_tibble_by_hand_gather, aes(x = asset, y = percent, fill = type)) +
  geom_col(position = 'dodge') +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Percent Contribution to Volatility",
          subtitle = "") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))


# ggplot2::ggplot(percentage_tibble_by_hand, aes(asset, `risk contribution`)) +
#   geom_col(fill = 'blue', colour = 'red') +
#   scale_y_continuous(labels = scales::percent) +
#   ggtitle("Percent Contribution to Volatility",
#           subtitle = "") +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   theme(plot.subtitle = element_text(hjust = 0.5)) +
#   xlab("Asset") +
#   ylab("Percent Contribution to Risk")
}







