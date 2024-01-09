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

library(profoc)

combination <- online(
  y = y,
  experts = experts,
  tau = probs
)

## -----------------------------------------------------------------------------
new_experts <- experts[T, , , drop = FALSE]

## -----------------------------------------------------------------------------
dim(combination$predictions)

# Predict will expand combination$predictions
combination <- predict(combination,
  new_experts = new_experts
)

dim(combination$predictions)

## -----------------------------------------------------------------------------
predictions <- predict(combination,
  new_experts = new_experts,
  update_model = FALSE
)

dim(predictions)

## -----------------------------------------------------------------------------
# New observation
new_y <- matrix(rnorm(1))

## -----------------------------------------------------------------------------
dim(combination$weights)

# Model Update
combination <-
  update(combination,
    new_y = new_y
  )

dim(combination$weights)

