#' @template function_online
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' @template param_y
#' @param experts An array of predictions with dimension T x D x P x K
#' (Observations x Variables x Quantiles x Experts) or T x D x K or T x P x K.
#' @template param_tau
#'
#' @template param_lead_time
#'
#' @template param_loss_function
#' @template param_loss_parameter
#' @param loss_gradient Determines if a linearized version of the loss is used.
#'
#' @template param_method
#'
#' @param b_smooth_pr A named list determining how the B-Spline matrices for
#' probabilistic smoothing are created. Default corresponds to no probabilistic
#' smoothing. See details.
#' @param p_smooth_pr A named list determining how the hat matrices  for
#' probabilistic P-Spline smoothing are created. Default corresponds to
#' no smoothing. See details.
#' @param b_smooth_mv A named list determining how the B-Spline matrices  for
#' multivariate smoothing are created. Default corresponds to no probabilistic
#' smoothing. See details.
#' @param p_smooth_mv A named list determining how the hat matrices  for
#' probabilistic P-Spline smoothing are created. Default corresponds to
#' no smoothing. See details.
#'
#' @param forget_regret Share of past regret not to be considered, resp. to be
#' forgotten in every iteration of the algorithm. Defaults to 0.
#'
#' @template param_soft_threshold
#' @template param_hard_threshold
#'
#' @template param_fixed_share
#'
#'
#' @param gamma Scaling parameter for the learning rate.
#'
#' @template param_parametergrid_max_combinations
#' @template param_parametergrids
#' @template param_forget_past_performance
#'
#' @param save_past_performance Whether or not the past performance w.r.t to the
#' considered parameter grid should be reported or not. Defaults to `FALSE` to
#' save memory. Setting it to `TRUE` can be memory intensive depending on the
#' data and the considered grid.
#' @param save_predictions_grid Whether or not all predictions w.r.t to the
#' considered parameter grid should be reported or not. Defaults to `FALSE`.
#' Setting it to `TRUE` can be memory intensive depending on the data
#'  and the considered grid.
#'
#' @template param_allow_quantile_crossing
#'
#' @param init A named list containing "init_weights": Array of dimension
#' DxPxK used as starting weights. "R0" a matrix of dimension PxK or 1xK
#' used as starting regret.
#' @param loss User specified loss array. Can also be a list with elements
#' "loss_array"
#' and "share", share mixes the provided loss with the loss calculated by
#' profoc. 1 means, only the provided loss will be used. share can also be
#' vector of shares to consider.
#' @param regret User specified regret array. If specific, the regret will
#'  not be calculated by profoc. Can also be a list with elements "regret_array"
#' and "share", share mixes the provided regret with the regret calculated by
#' profoc. 1 means, only the provided regret will be used. share can also be
#' vector of shares to consider.
#' @template param_trace
#' @param get_timings Whether or not to return timings. Defaults to `FALSE`. If
#' set to true a dataframe `times` will be written to your global environment.
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
#'   experts[t, , 1] <- qnorm(prob_grid, mean = -1, sd = 1)
#'   experts[t, , 2] <- qnorm(prob_grid, mean = 3, sd = sqrt(4))
#' }
#'
#' model <- online(
#'   y = matrix(y),
#'   experts = experts,
#'   tau = prob_grid,
#'   p_smooth_pr = list(lambda = 10)
#' )
#'
#' print(model)
#' plot(model)
#'
#' new_y <- matrix(rnorm(1)) # Realized
#' new_experts <- experts[T, , , drop = FALSE]
#'
#' # Update will update the models weights etc if you provide new realizations
#' model <- update(model, new_y = new_y, new_experts = new_experts)
#'
#' # Predict will expand `model$predictions` by default
#' model <- predict(model, new_experts = new_experts, update_model = TRUE)
#' }
#'
#' @importFrom abind adrop asub
#'
#' @export
online <- function(y, experts, tau,
                   lead_time = 0,
                   loss_function = "quantile",
                   loss_parameter = 1,
                   loss_gradient = TRUE,
                   method = "bewa",
                   b_smooth_pr = list(
                     knots = P,
                     mu = 0.5,
                     sigma = 1,
                     nonc = 0,
                     tailweight = 1,
                     deg = 1,
                     periodic = FALSE
                   ),
                   p_smooth_pr = list(
                     knots = P,
                     mu = 0.5,
                     sigma = 1,
                     nonc = 0,
                     tailweight = 1,
                     deg = 1,
                     ndiff = 1.5,
                     lambda = -Inf,
                     periodic = FALSE
                   ),
                   b_smooth_mv = list(
                     knots = D,
                     mu = 0.5,
                     sigma = 1,
                     nonc = 0,
                     tailweight = 1,
                     deg = 1,
                     periodic = FALSE
                   ),
                   p_smooth_mv = list(
                     knots = D,
                     mu = 0.5,
                     sigma = 1,
                     nonc = 0,
                     tailweight = 1,
                     deg = 1,
                     ndiff = 1.5,
                     lambda = -Inf,
                     periodic = FALSE
                   ),
                   forget_regret = 0,
                   soft_threshold = -Inf,
                   hard_threshold = -Inf,
                   fixed_share = 0,
                   gamma = 1,
                   parametergrid_max_combinations = 100,
                   parametergrids = list(
                     general = NULL,
                     b_smooth_pr = NULL,
                     p_smooth_pr = NULL,
                     b_smooth_mv = NULL,
                     p_smooth_mv = NULL
                   ),
                   forget_past_performance = 0,
                   save_past_performance = FALSE,
                   save_predictions_grid = FALSE,
                   allow_quantile_crossing = FALSE,
                   init = NULL,
                   loss = NULL,
                   regret = NULL,
                   trace = TRUE,
                   get_timings = FALSE) {
  if (requireNamespace("rcpptimer", quietly = TRUE)) {
    registerS3method("print", "rcpptimer", rcpptimer:::print.rcpptimer)
  }

  model_instance <- new(conline)
  model_instance$trace <- trace
  model_instance$get_timings <- get_timings
  model_instance$forget_past_performance <- forget_past_performance
  model_instance$save_past_performance <- save_past_performance
  model_instance$save_predictions_grid <- save_predictions_grid

  if (is.vector(y)) {
    y <- matrix(y)
  }

  model_instance$y <- y

  # preserve names
  names <- list(y = dimnames(y))

  # Prepare experts

  e_list <- init_experts_list(
    experts = experts,
    y = y,
    output_with_names = TRUE
  )
  model_instance$experts <- e_list$experts

  names$experts <- list(NULL)
  names$experts[[2]] <- e_list$dnames
  names$experts[[3]] <- tau
  names$experts[[4]] <- e_list$enames

  model_instance$tau <- tau

  # Define dimensions for convenience
  T <- dim(e_list$experts)[1]
  D <- dim(e_list$experts[[1]])[1]
  P <- dim(e_list$experts[[1]])[2]
  K <- dim(e_list$experts[[1]])[3]

  if (nrow(e_list$experts) - nrow(y) < 0) {
    stop("Number of provided expert predictions has to match or exceed observations.")
  }

  if (nrow(y) <= lead_time) {
    stop("Number of expert predictions need to exceed lead_time.")
  }

  if (is.null(names$y[[2]])) names$y[[2]] <- paste0(1:D, "D")

  model_instance$lead_time <- lead_time

  if (is.null(loss)) {
    loss_share <- 0
  } else if (is.array(loss)) {
    loss_array <- list()
    for (i in 1:T) {
      loss_array[[i]] <- array(loss[i, , , ], dim = c(D, P, K))
    }
    dim(loss_array) <- c(T, 1)
    loss_share <- 1
    model_instance$loss_array <- loss_array
  } else if (is.list(loss)) {
    loss_array <- list()
    for (i in 1:T) {
      loss_array[[i]] <- array(loss$loss[i, , , ], dim = c(D, P, K))
    }
    dim(loss_array) <- c(T, 1)
    loss_share <- loss$share
    model_instance$loss_array <- loss_array
  }

  if (is.null(regret)) {
    regret_share <- 0
  } else if (is.array(regret)) {
    regret_array <- list()
    for (i in 1:T) {
      regret_array[[i]] <- array(regret[i, , , ], dim = c(D, P, K))
    }
    dim(regret_array) <- c(T, 1)
    regret_share <- 1
    model_instance$regret_array <- regret_array
  } else if (is.list(regret)) {
    regret_array <- list()
    for (i in 1:T) {
      regret_array[[i]] <- array(regret$regret[i, , , ],
        dim = c(D, P, K)
      )
    }
    dim(regret_array) <- c(T, 1)
    regret_share <- regret$share
    model_instance$regret_array <- regret_array
  }

  # Create parameter grid
  pars_basis_pr <- list(
    n = val_or_def(b_smooth_pr$knots, P),
    mu = val_or_def(b_smooth_pr$mu, 0.5),
    sigma = val_or_def(b_smooth_pr$sigma, 1),
    nonc = val_or_def(b_smooth_pr$nonc, 0),
    tailw = val_or_def(b_smooth_pr$tailweight, 1),
    deg = val_or_def(b_smooth_pr$deg, 1),
    periodic = val_or_def(b_smooth_pr$periodic, FALSE)
  )
  pars_basis_pr_n <- prod(sapply(pars_basis_pr, length))

  pars_basis_mv <- list(
    n = val_or_def(b_smooth_mv$knots, D),
    mu = val_or_def(b_smooth_mv$mu, 0.5),
    sigma = val_or_def(b_smooth_mv$sigma, 1),
    nonc = val_or_def(b_smooth_mv$nonc, 0),
    tailw = val_or_def(b_smooth_mv$tailweight, 1),
    deg = val_or_def(b_smooth_mv$deg, 1),
    periodic = val_or_def(b_smooth_mv$periodic, FALSE)
  )
  pars_basis_mv_n <- prod(sapply(pars_basis_mv, length))

  pars_hat_pr <- list(
    n = val_or_def(p_smooth_pr$knots, P),
    mu = val_or_def(p_smooth_pr$mu, 0.5),
    sigma = val_or_def(p_smooth_pr$sigma, 1),
    nonc = val_or_def(p_smooth_pr$nonc, 0),
    tailw = val_or_def(p_smooth_pr$tailweight, 1),
    deg = val_or_def(p_smooth_pr$deg, 1),
    ndiff = val_or_def(p_smooth_pr$ndiff, 1.5),
    lambda = val_or_def(p_smooth_pr$lambda, -Inf),
    periodic = val_or_def(p_smooth_pr$periodic, FALSE)
  )
  pars_hat_pr_n <- prod(sapply(pars_hat_pr, length))

  pars_hat_mv <- list(
    n = val_or_def(p_smooth_mv$knots, D),
    mu = val_or_def(p_smooth_mv$mu, 0.5),
    sigma = val_or_def(p_smooth_mv$sigma, 1),
    nonc = val_or_def(p_smooth_mv$nonc, 0),
    tailw = val_or_def(p_smooth_mv$tailweight, 1),
    deg = val_or_def(p_smooth_mv$deg, 1),
    ndiff = val_or_def(p_smooth_mv$ndiff, 1.5),
    lambda = val_or_def(p_smooth_mv$lambda, -Inf),
    periodic = val_or_def(p_smooth_mv$periodic, FALSE)
  )
  pars_hat_mv_n <- prod(sapply(pars_hat_mv, length))

  if (is.null(parametergrids$general)) {
    parametergrid <- expand_grid_sample(
      list(
        forget_regret = forget_regret,
        soft_threshold = soft_threshold,
        hard_threshold = hard_threshold,
        fixed_share = fixed_share,
        basis_pr_idx = seq_len(pars_basis_pr_n),
        basis_mv_idx = seq_len(pars_basis_mv_n),
        hat_pr_idx = seq_len(pars_hat_pr_n),
        hat_mv_idx = seq_len(pars_hat_mv_n),
        gamma = gamma,
        loss_share = loss_share,
        regret_share = regret_share
      ),
      n = parametergrid_max_combinations,
      verbose = TRUE
    )
  } else {
    parametergrid <- parametergrids$general
  }

  # Create basis and hat matix lists

  # # Basis matrices for probabilistic smoothing
  tmp <- make_basis_mats(
    x = 1:P / (P + 1),
    n = val_or_def(b_smooth_pr$knots, P),
    mu = val_or_def(b_smooth_pr$mu, 0.5),
    sigma = val_or_def(b_smooth_pr$sigma, 1),
    nonc = val_or_def(b_smooth_pr$nonc, 0),
    tailw = val_or_def(b_smooth_pr$tailweight, 1),
    deg = val_or_def(b_smooth_pr$deg, 1),
    periodic = val_or_def(b_smooth_pr$periodic, FALSE),
    idx = sort(unique(parametergrid[, "basis_pr_idx"])),
    params = parametergrids$b_smooth_pr
  )
  model_instance$basis_pr <- tmp$basis
  model_instance$params_basis_pr <- tmp$params

  # Basis matrices for multivariate smoothing
  tmp <- make_basis_mats(
    x = 1:D / (D + 1),
    n = val_or_def(b_smooth_mv$knots, D),
    mu = val_or_def(b_smooth_mv$mu, 0.5),
    sigma = val_or_def(b_smooth_mv$sigma, 1),
    nonc = val_or_def(b_smooth_mv$nonc, 0),
    tailw = val_or_def(b_smooth_mv$tailweight, 1),
    deg = val_or_def(b_smooth_mv$deg, 1),
    periodic = val_or_def(b_smooth_mv$periodic, FALSE),
    idx = sort(unique(parametergrid[, "basis_mv_idx"])),
    params = parametergrids$b_smooth_mv
  )
  model_instance$basis_mv <- tmp$basis
  model_instance$params_basis_mv <- tmp$params

  tmp <- make_hat_mats(
    x = 1:P / (P + 1),
    n = val_or_def(p_smooth_pr$knots, P),
    mu = val_or_def(p_smooth_pr$mu, 0.5),
    sigma = val_or_def(p_smooth_pr$sigma, 1),
    nonc = val_or_def(p_smooth_pr$nonc, 0),
    tailw = val_or_def(p_smooth_pr$tailweight, 1),
    deg = val_or_def(p_smooth_pr$deg, 1),
    ndiff = val_or_def(p_smooth_pr$ndiff, 1.5),
    lambda = val_or_def(p_smooth_pr$lambda, -Inf),
    periodic = val_or_def(p_smooth_pr$periodic, FALSE),
    idx = sort(unique(parametergrid[, "hat_pr_idx"])),
    params = parametergrids$p_smooth_pr
  )
  model_instance$hat_pr <- tmp$hat
  model_instance$params_hat_pr <- tmp$params

  tmp <- make_hat_mats(
    x = 1:D / (D + 1),
    n = val_or_def(p_smooth_mv$knots, D),
    mu = val_or_def(p_smooth_mv$mu, 0.5),
    sigma = val_or_def(p_smooth_mv$sigma, 1),
    nonc = val_or_def(p_smooth_mv$nonc, 0),
    tailw = val_or_def(p_smooth_mv$tailweight, 1),
    deg = val_or_def(p_smooth_mv$deg, 1),
    ndiff = val_or_def(p_smooth_mv$ndiff, 1.5),
    lambda = val_or_def(p_smooth_mv$lambda, -Inf),
    periodic = val_or_def(p_smooth_mv$periodic, FALSE),
    idx = sort(unique(parametergrid[, "hat_mv_idx"])),
    params = parametergrids$p_smooth_mv
  )
  model_instance$hat_mv <- tmp$hat
  model_instance$params_hat_mv <- tmp$params

  # Fix basis / hat idx while retaining original order
  parametergrid[, "basis_pr_idx"] <- match(
    parametergrid[, "basis_pr_idx"],
    sort(unique(parametergrid[, "basis_pr_idx"]))
  )

  parametergrid[, "basis_mv_idx"] <- match(
    parametergrid[, "basis_mv_idx"],
    sort(unique(parametergrid[, "basis_mv_idx"]))
  )

  parametergrid[, "hat_pr_idx"] <- match(
    parametergrid[, "hat_pr_idx"],
    sort(unique(parametergrid[, "hat_pr_idx"]))
  )

  parametergrid[, "hat_mv_idx"] <- match(
    parametergrid[, "hat_mv_idx"],
    sort(unique(parametergrid[, "hat_mv_idx"]))
  )

  model_instance$params <- parametergrid
  model_instance$allow_quantile_crossing <- allow_quantile_crossing
  model_instance$loss_function <- loss_function
  model_instance$loss_gradient <- loss_gradient
  model_instance$loss_parameter <- loss_parameter
  model_instance$method <- method

  # Populate default values for w0, R0 etc.
  model_instance$set_defaults()

  # Overwrite defaults if user specified explicitly
  if (!is.null(init$init_weights)) {
    init$init_weights <- pmax(init$init_weights, exp(-350))
    for (d in 1:D) {
      for (p in 1:P) {
        init$init_weights[d, p, ] <-
          init$init_weights[d, p, ] / sum(init$init_weights[d, p, ])
      }
    }
    model_instance$w0 <- init$init_weights
  }

  if (!is.null(init$R0)) model_instance$R0 <- init$R0

  # Populates default values to grid-sized objects
  model_instance$set_grid_objects()

  # Execute online learning
  model_instance$learn()

  model <- post_process_model(model_instance, names)

  model_instance$get_times()
  rm(model_instance)
  gc()
  return(model)
}
