## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  # dev = "svg",
  warning = FALSE,
  message = FALSE,
  comment = "#>"
)
Sys.setenv("OMP_THREAD_LIMIT" = 2)

## -----------------------------------------------------------------------------
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

## ----echo = FALSE, out.width='100%', fig.width=7, fig.height = 4, dpi = 300----
library(ggplot2)
library(tibble)

text_size <- 16
width <- 12
height <- 6

col_lightgray <- "#e7e7e7"
col_blue <- "#F24159"
col_b_smooth <- "#5391AE"
col_p_smooth <- "#85B464"
col_pointwise <- "#E2D269"
col_b_constant <- "#7A4E8A"
col_p_constant <- "#BC677B"
col_optimum <- "#666666"
col_auto <- "#EA915E"

df <- data.frame(x = sort(y), y = seq(from = 1 / T, to = 1, by = 1 / T))
df$xend <- c(df$x[2:nrow(df)], df$x[nrow(df)])
df$yend <- df$y
df[T, "xend"] <- 7.5

data_plot <-
  ggplot(df, aes(x = x, y = y, xend = xend, yend = yend)) +
  stat_function(
    fun = pnorm, n = 10000,
    args = list(mean = experts_mu[2], sd = experts_sd[2]),
    aes(col = "Expert 2"), linewidth = 1.5
  ) +
  stat_function(
    fun = pnorm, n = 10000,
    args = list(mean = experts_mu[1], sd = experts_sd[1]),
    aes(col = "Expert 1"), linewidth = 1.5
  ) +
  stat_function(
    fun = pnorm,
    n = 10000,
    linewidth = 1.5, aes(col = "DGP") # , linetype = "dashed"
  ) +
  geom_point(aes(col = "ECDF"), linewidth = 1.5, show.legend = FALSE) +
  geom_segment(aes(col = "ECDF")) +
  geom_segment(data = tibble(
    x_ = -5,
    xend_ = min(y),
    y_ = 0,
    yend_ = 0
  ), aes(x = x_, xend = xend_, y = y_, yend = yend_)) +
  theme_minimal() +
  theme(
    # text = element_text(size = text_size),
    legend.position = "bottom"
  ) +
  ggtitle("Data generating Process") +
  ylab("Probability p") +
  xlab("Value") +
  scale_colour_manual(NULL, values = c("#969696", "#252525", col_auto, col_blue)) +
  guides(color = guide_legend(
    # nrow = 2,
    # byrow = FALSE
  )) +
  scale_x_continuous(limits = c(-5, 7.5))
data_plot

## -----------------------------------------------------------------------------
library(profoc)

combination <- online(
  y = y,
  experts = experts,
  tau = probs
)

## -----------------------------------------------------------------------------
print(combination)

## -----------------------------------------------------------------------------
dim(combination$weights)

## ----echo = TRUE, out.width='100%', fig.width=7, fig.height = 4, dpi = 300----
autoplot(combination)

## ----echo = TRUE, out.width='100%', fig.width=7, fig.height = 4, dpi = 300----
library(dplyr)
library(ggplot2)

tidy(combination$weights) |>
  filter(p %in% c(0.05, 0.5, 0.95)) |>
  ggplot(aes(x = t, y = w, col = k)) +
  geom_line(linewidth = 1) +
  facet_wrap(~p, ncol = 1)

## ----echo = TRUE, out.width='100%', fig.width=7, fig.height = 4, dpi = 300----
tidy(combination$predictions)

tidy(combination$predictions) |>
  ggplot(aes(x = t, y = prediction, group = p, colour = p)) +
  geom_line() +
  scale_color_continuous(low = "#FFDD00", high = "#0057B7") +
  # A little hacky way to add the realized values
  geom_line(aes(x = t, y = rep(y, each = 99)),
    linetype = "dashed", col = "black", linewidth = 1
  )

