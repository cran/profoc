## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  # dev = "svg",
  warning = FALSE,
  message = FALSE,
  comment = "#>"
)
Sys.setenv("OMP_THREAD_LIMIT" = 2)

set.seed(1)
T <- 2^5 # Observations
D <- 1 # Numer of variables
N <- 2 # Experts
P <- 99 # Size of probability grid
probs <- 1:P / (P + 1)

y <- matrix(rnorm(T)) # Realized observations

# Experts deviate in mean and standard deviation from true process
experts_mu <- c(-1, 3)
experts_sd <- c(1, 2)

experts <- array(dim = c(T, P, N)) # Expert predictions

for (t in 1:T) {
  experts[t, , 1] <- qnorm(probs, mean = experts_mu[1], sd = experts_sd[1])
  experts[t, , 2] <- qnorm(probs, mean = experts_mu[2], sd = experts_sd[2])
}

## -----------------------------------------------------------------------------
library(profoc)
model <- new(conline)

## -----------------------------------------------------------------------------
model$y <- y
tau <- 1:P / (P + 1)
model$tau <- tau

## -----------------------------------------------------------------------------
experts_list <- init_experts_list(experts, y)
model$experts <- experts_list

## -----------------------------------------------------------------------------
hat <- make_hat_mats(
  x = tau,
  mu = 0.2, # Put more knots in the lower tail
  periodic = TRUE
)
str(hat)

## -----------------------------------------------------------------------------
model$hat_pr <- hat$hat

## -----------------------------------------------------------------------------
model$basis_mv <- list(Matrix::sparseMatrix(i = 1:D, j = 1:D, x = 1))
model$basis_pr <- list(Matrix::sparseMatrix(i = 1:P, j = 1:P, x = 1))
model$hat_mv <- list(Matrix::sparseMatrix(i = 1:D, j = 1:D, x = 1))

## -----------------------------------------------------------------------------
parametergrid <- as.matrix(
  expand.grid(
    forget_regret = 0,
    soft_threshold = -Inf,
    hard_threshold = -Inf,
    fixed_share = 0,
    basis_pr_idx = 1,
    basis_mv_idx = 1,
    hat_pr_idx = 1,
    hat_mv_idx = 1,
    gamma = 1,
    loss_share = 0,
    regret_share = 0
  )
)

model$params <- parametergrid

## -----------------------------------------------------------------------------
model$set_defaults()

## -----------------------------------------------------------------------------
model$set_grid_objects()

## -----------------------------------------------------------------------------
model$learn()

## -----------------------------------------------------------------------------
head(model$weights[[T]][, , 1])

## -----------------------------------------------------------------------------
names <- list(y = dimnames(y))
names$experts <- list(
  1:T,
  paste("Marginal", 1:D),
  tau,
  paste("Expert", 1:N)
)

output <- post_process_model(model, names)

## -----------------------------------------------------------------------------
rm(model)

