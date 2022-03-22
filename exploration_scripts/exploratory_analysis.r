library(tidyverse)
library(dplyr)
library(moderndive)
library(gapminder)
library(sjPlot)
library(stats)
library(jtools)
library(ggplot2)
library(MASS)
library(knitr)
library(janitor)
library(combinat)
library(MuMIn)
require(MuMIn)

options(na.action = "na.fail")

films <- read.csv("dataset4.csv")

films$over7 <- 0

films$over7[films$rating>=7.0] <- 1
films$over7[films$rating<7.0] <- 0
films <- films[c(-1, -7)]

films <- films %>% 
  na.omit()

names <- names(films[-6])

# iterate through all possible models and find best fit based on 
# aic, bic, etc also if models have close scores choose most simple model

out <- unlist(lapply(1:5, function(n) {
  # get combinations
  combinations <- t(combn(names,n))
  # collapse them into usable formulas:
  formulas <- apply(combinations, 1, 
                    function(row) paste0("over7 ~ ", paste0(row, collapse = "+")))}))

mods = lapply(out, function(frml) glm(frml, data=films, family = binomial(link = "logit")))

all_models <- bind_rows(lapply(out, function(frml) {
  a = glance(glm(frml, data=films, family = binomial(link = "logit")))
  a$frml = frml
  return(a)
}))

all_models <- all_models %>% 
  arrange(AIC)

# based on simplicity and AIC i would say this is best model

best <- glm(over7 ~ length+budget+genre,data=films, family = binomial(link = "logit"))
summary(best)

plot_model(best, show.values = TRUE, transform = NULL,
           title = "Log-Odds (Over 7)", show.p = FALSE)   #confidence interval plot for log-odds
predict(best)[1:20]

best_data <- films %>% 
  dplyr::select(length, budget, genre,over7)

log_best = best_data %>%
  mutate(logodds_over7 = predict(best)) %>%
  mutate(odds_over7 = exp(logodds_over7)) %>%
  mutate(probs_over7 = fitted(best))


