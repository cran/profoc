# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

batch_rcpp <- function(y, experts, tau, affine, positive, intercept, debias, lead_time, initial_window, rolling_window, loss_function, loss_parameter, qw_crps, param_grid, forget_past_performance, allow_quantile_crossing, trace) {
    .Call(`_profoc_batch_rcpp`, y, experts, tau, affine, positive, intercept, debias, lead_time, initial_window, rolling_window, loss_function, loss_parameter, qw_crps, param_grid, forget_past_performance, allow_quantile_crossing, trace)
}

loss <- function(y, x, pred = 0, method = "quantile", tau = 0.5, a = 1, gradient = TRUE) {
    .Call(`_profoc_loss`, y, x, pred, method, tau, a, gradient)
}

loss_grad_wrt_w <- function(expert, pred, truth, tau, loss_function, a, w) {
    .Call(`_profoc_loss_grad_wrt_w`, expert, pred, truth, tau, loss_function, a, w)
}

optimize_weights <- function(truth, experts, affine = FALSE, positive = FALSE, intercept = FALSE, debias = TRUE, loss_function = "quantile", tau = 0.5, forget = 0, loss_scaling = 1) {
    .Call(`_profoc_optimize_weights`, truth, experts, affine, positive, intercept, debias, loss_function, tau, forget, loss_scaling)
}

optimize_betas <- function(truth, experts, affine, positive, intercept, debias, loss_function, tau_vec, forget, loss_scaling, basis, beta, qw_crps) {
    .Call(`_profoc_optimize_betas`, truth, experts, affine, positive, intercept, debias, loss_function, tau_vec, forget, loss_scaling, basis, beta, qw_crps)
}

#' @template function_oracle
#'
#' @template param_y
#' @template param_experts
#' @template param_tau
#' @template param_affine
#' @template param_positive
#' @template param_intercept
#' @template param_debias
#' @template param_loss_function
#' @template param_loss_parameter
#' @template param_forget
#' @usage oracle(y, experts, tau, affine = FALSE,
#' positive = FALSE, intercept = FALSE, debias = TRUE,
#' loss_function = "quantile", loss_parameter = 1, forget = 0)
#' @examples
#' \dontrun{
#' T <- 50 # Observations
#' N <- 2 # Experts
#' P <- 9 # Quantiles
#' prob_grid <- 1:P / (P + 1)
#'
#' y <- rnorm(n = T) # Realized
#' experts <- array(dim = c(T, P, N)) # Predictions
#' for (t in 1:T) {
#'     experts[t, , 1] <- qnorm(prob_grid, mean = -1, sd = 1)
#'     experts[t, , 2] <- qnorm(prob_grid, mean = 3, sd = sqrt(4))
#' }
#'
#' model <- oracle(
#'     y = matrix(y),
#'     experts = experts
#' )
#' }
#'
#' @export
oracle <- function(y, experts, tau = as.numeric( c()), affine = FALSE, positive = FALSE, intercept = FALSE, debias = TRUE, loss_function = "quantile", loss_parameter = 1, forget = 0) {
    .Call(`_profoc_oracle`, y, experts, tau, affine, positive, intercept, debias, loss_function, loss_parameter, forget)
}

make_knots <- function(kstep, a = 1, deg = 3L, even = FALSE) {
    .Call(`_profoc_make_knots`, kstep, a, deg, even)
}

make_hat_matrix <- function(x, kstep, lambda, bdiff, deg, a, even) {
    .Call(`_profoc_make_hat_matrix`, x, kstep, lambda, bdiff, deg, a, even)
}

make_basis_matrix <- function(x, kstep, deg, a, even) {
    .Call(`_profoc_make_basis_matrix`, x, kstep, deg, a, even)
}

make_basis_matrix2 <- function(x, knots, deg) {
    .Call(`_profoc_make_basis_matrix2`, x, knots, deg)
}

make_hat_matrix2 <- function(x, knots, deg, bdiff, lambda) {
    .Call(`_profoc_make_hat_matrix2`, x, knots, deg, bdiff, lambda)
}

