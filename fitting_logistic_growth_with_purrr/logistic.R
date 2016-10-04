# logistic growth model script
rm(list=ls())

# load the libraries
library(tidyverse)
library(broom)

# import the data
dd <- read.csv("demo_data.csv")


# plot population density through time
ggplot(data = dd, aes(x=Day, y=dpml, group=as.factor(Replicate))) +  
  geom_line() + 
  geom_point() +
  scale_y_log10() +
  facet_wrap(~Temperature)

# create function to fit the logistic model using nonlinear least squares(nls)
# note that the variable names (here dpml and Day) must match those in the dataset
logistic_model <- function(df) {
  nls(dpml ~ dpml[1] * K/ (dpml[1] + ((K - dpml[1]) * exp(-r * Day))), 
      data=df, 
      start=list(K=K_start, r=r_start))
}

#initial guess carrying capacity
K_start <- 200

#initial guess growth rate
r_start <- 2.5

#use the function with safely, so that all results will be returned, even errors
safe_logistic_model <- safely(logistic_model)

# fit the logistic regression and store various results
# in this case, fit the model to each replicate
# note that in the predict function, the variable must be same name as in the dataset (here Day)
models <- dd %>% group_by(Temperature, Replicate) %>% 
  nest() %>% 
  mutate(model=map(data, safe_logistic_model)) %>%
  mutate(no_error = model %>% map_lgl(.f = ~ is.null(.x$error))) %>%
  filter(no_error) %>% 
  mutate(params=model %>% map(~ tidy(.$result)), 
         preds=model %>% map(~ predict(.$result, newdata = data.frame(Day=seq(0,20, .1)))))


# plot K and r estimates
ggplot(models %>% select(Temperature, Replicate, params) %>% unnest(),
       aes(x=Temperature, y=estimate)) +
  facet_wrap(~term, scales = "free") +
  geom_jitter(width=0.2, size=2)


# plot the predictions of the model and the data
# first bundle the predictions with a vector of Days
preds <- cbind(models %>% select(Temperature, Replicate, preds) %>% unnest(), Day=seq(0, 20, 0.1))
ggplot(data = dd, aes(x=Day, y=dpml, group=Replicate)) +  
  geom_line() + 
  geom_point() +
  scale_y_log10() +
  facet_wrap(~Temperature) +
  geom_line(data=preds, aes(x=Day, y=preds), col="blue") +
  xlab("Day")


#fit the logistic model per treatment (here Temperature)
models_t <- dd %>% group_by(Temperature) %>% 
  nest() %>% 
  mutate(model=map(data, safe_logistic_model)) %>%
  mutate(no_error = model %>% map_lgl(.f = ~ is.null(.x$error))) %>%
  filter(no_error) %>% 
  mutate(params=model %>% map(~ tidy(.$result)), 
         preds=model %>% map(~ predict(.$result, newdata = data.frame(Day=seq(0,20, .1)))))

#plot K and r estimates of both models (per sample and per treatment)
ggplot(models %>% select(Temperature, Replicate, params) %>% unnest(),
       aes(x=Temperature, y=estimate)) +
  facet_wrap(~term, scales = "free") +
  geom_point(size=3) +
  geom_point(data=models_t %>% select(Temperature, params) %>% unnest(), color="red", size=3)

#plot the predictions of the model and the data
preds_t <- cbind(models_t %>% select(Temperature, preds) %>% unnest(), Day=seq(0, 20, 0.1))

ggplot(data = dd, aes(x=Day, y=dpml, group=Replicate)) +  
  geom_line() + 
  geom_point() +
  scale_y_log10() +
  facet_wrap(~Temperature) +
  geom_line(data=preds_t, aes(x=Day, y=preds, group=Temperature), col="blue", lwd=2) +
  xlab("Day")
