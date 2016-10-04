## Not yet working


#theta logistic growth model script
rm(list=ls())

#load the libraries
library(tidyverse)
library(broom)

#import the data and transform day variable so that first day of the experiment is 0
all_d <- read.csv("/Volumes/LCS ORANGE/temperature choice/pop_dens_t_choice.csv") %>% select(-X) %>% mutate(day0 = day-min(day))

#filter the data, only until day 9
all_d <- filter(all_d, day0 <= 9)

#plot population density through time
ggplot(data = all_d, aes(x=day0, y=density, group=sample, color=as.factor(treatment))) +  
  geom_line() + 
  geom_point() +
  scale_y_log10() +
  facet_wrap(~treatment, nrow=2)

#initial guess carrying capacity
K_start <- 500000

#initial guess growth rate
r_start <- 2.5

#initial theta
theta_start=1.5

#create function to fit the theta logistic model using nonlinear least squares(nls)
theta_logistic_model <- function(df) {
  nls(density ~ (K^(-theta)+(density[1]^(-theta)-K^(-theta))*exp(-r*theta*day0))^(-1/theta), 
      data=df, 
      start=list(K=K_start, r=r_start, theta=theta_start),
      trace = 2)
}

#use the function with safely, so that all results will be returned, even errors
safe_t_logistic_model <- safely(theta_logistic_model)

#fit the theta logistic regression and store the results
#in this case, fit the model to each sample (group_by(treatment, sample))
models <- all_d %>% group_by(treatment, sample) %>% 
  nest() %>% 
  mutate(model=map(data, safe_t_logistic_model)) %>%
  mutate(no_error = model %>% map_lgl(.f = ~ is.null(.x$error))) %>%
  filter(no_error) %>% 
  mutate(params=model %>% map(~ tidy(.$result)), 
         preds=model %>% map(~ predict(.$result, newdata = data.frame(day0=seq(0,10, .1)))))

#plot K, r and theta estimates
ggplot(models %>% select(treatment, sample, params) %>% unnest(),
       aes(x=treatment, y=estimate)) +
  facet_wrap(~term, scales = "free") +
  geom_point()

#plot the predictions of the model and the data
preds <- cbind(models %>% select(treatment, sample, preds) %>% unnest(), day0=seq(0, 10, 0.1))

ggplot(data = all_d, aes(x=day0, y=density, group=sample, color=as.factor(treatment))) +  
  geom_line() + 
  geom_point() +
  scale_y_log10() +
  facet_wrap(~treatment, nrow=2) +
  geom_line(data=preds, aes(x=day0, y=preds))


#fit the theta logistic model per treatment (group_by(treatment))
models_t <- all_d %>% group_by(treatment) %>% 
  nest() %>% 
  mutate(model=map(data, safe_t_logistic_model)) %>%
  mutate(no_error = model %>% map_lgl(.f = ~ is.null(.x$error))) %>%
  filter(no_error) %>% 
  mutate(params=model %>% map(~ tidy(.$result)), 
         preds=model %>% map(~ predict(.$result, newdata = data.frame(day0=seq(0,10, .1)))))

#plot K, r and theta estimates of both models (per sample and per treatment)
ggplot(models %>% select(treatment, sample, params) %>% unnest(),
       aes(x=treatment, y=estimate)) +
  facet_wrap(~term, scales = "free") +
  geom_point(size=3) +
  geom_point(data=models_t %>% select(treatment, params) %>% unnest(), color="red", size=3)

#plot the predictions of the model and the data
preds_t <- cbind(models_t %>% select(treatment, preds) %>% unnest(), day0=seq(0, 10, 0.1))

ggplot(data = all_d, aes(x=day0, y=density, group=sample, color=as.factor(treatment))) +  
  geom_line() + 
  geom_point() +
  scale_y_log10() +
  facet_wrap(~treatment, nrow=2) +
  geom_line(data=preds_t, aes(x=day0, y=preds, group=treatment)) +
  xlab("Day")
