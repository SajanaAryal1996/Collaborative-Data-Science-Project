#’@FACTOR
#’@does exploratory factor analysis for two group and calculate cronbach alpha value
#’@param - Factor analysis is used for dimension reduction
#’@keywords - Aplha, cronbach, factor
#’ @export - makes the function available for others to use when your package is loaded
#’@examples - sample code

calculate_alpha_and_loadings <- function(data, group_name) {
  numeric_data <- data[sapply(data, is.numeric)]
  factor_analysis <- psych::fa(numeric_data, nfactors = 1, rotate = "varimax")
  factor_loadings <- factor_analysis$loadings
  print(paste("Factor Loadings for", group_name))
  print(factor_loadings)
  cronbach_alpha <- psych::alpha(numeric_data)$total$raw_alpha
  print(paste("Cronbach's Alpha for", group_name, ":", cronbach_alpha))
}


perform_CFA <- function(model_input, data_input, group_name) {
  model <- paste(model_input)
  fit <- lavaan::cfa(model, data = data_input, std.lv = TRUE)
  print(paste("Standardized Factor Loadings for", group_name))
  print(lavaan::inspect(fit, "std")$lambda)
  factor_loadings <- lavaan::inspect(fit, "std")$lambda
  cronbach_alpha <- psych::alpha(data_input)$total$raw_alpha
  print(paste("Cronbach's Alpha for", group_name, ":", cronbach_alpha))
}