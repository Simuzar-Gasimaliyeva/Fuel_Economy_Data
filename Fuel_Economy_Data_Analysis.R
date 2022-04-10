library(tidyverse)
library(ggplot2)
library(data.table)
library(rstudioapi)
library(skimr)
library(inspectdf)
library(mice)
library(plotly)
library(highcharter)
library(recipes) 
library(caret) 
library(purrr) 
library(graphics) 
library(Hmisc) 
library(glue)
library(readr)
library(h2o)

# task 1-----Bringing dataset
head(mpg) %>% as.data.frame()

mpg

# task 2----- Preprocessing, Filling NA's ------
mpg %>% 
  inspect_na() %>% 
  filter(pcnt<30) %>% 
  pull(col_name) -> variables

mpg <- mpg %>% select(variables)

df.num <- mpg %>%
  select_if(is.numeric)

df.chr <- mpg %>%
  select_if(is.character)


df.num %>% inspect_na()

df.num_mice <- df.num %>% mice(method='rf', seed=123)
df.num <- df.num_mice %>% complete()


df.chr %>% inspect_na()

rec_obj <- recipe(~ ., data = df.chr) %>%
step_modeimpute(all_nominal()) %>%
prep(stringsAsFactors = FALSE)

df.chr <- bake(rec_obj, df.chr)


df.chr <- dummyVars(" ~ .", data = df.chr) %>% 
  predict(newdata = df.chr) %>% 
  as.data.frame()

df <- cbind(df.chr,df.num)

names(df) <- names(df) %>% 
  str_replace_all(" ","_") %>%
  str_replace_all("-","_") %>%
  str_replace_all("\\(","") %>% 
  str_replace_all("\\)","") %>% 
  str_replace_all("\\'","")


# task 3-4---Fitting h2o model---
h2o.init()

h2o_data <- df %>% as.h2o()

h2o_data <- h2o_data %>% h2o.splitFrame(ratios = 0.8, seed = 123)
train <- h2o_data[[1]]
test <- h2o_data[[2]]

target <- 'cty'
features <- mpg %>% select(year,cyl,displ) %>% names()

model <- h2o.glm(
  x = features, y = target,
  training_frame = train,
  validation_frame = test,
  nfolds = 10, seed = 123,
  lambda = 0, compute_p_values = T)

model@model$coefficients_table %>%
  as.data.frame() %>%
  dplyr::select(names,p_value) %>%
  mutate(p_value = round(p_value,3)) %>%
  .[-1,] %>%
  arrange(desc(p_value))

y_pred <- model %>% h2o.predict(newdata = test) %>% as.data.frame()
y_pred$predict

# task 5-------
test_set <- test %>% as.data.frame()
residuals = test_set$cty - y_pred$predict

RMSE = sqrt(mean(residuals^2))

y_test_mean = mean(test_set$cty)

tss = sum((test_set$cty - y_test_mean)^2)
rss = sum(residuals^2) 

R2 = 1 - (rss/tss); R2

n <- test_set %>% nrow() 
k <- features %>% length() 
Adjusted_R2 = 1-(1-R2)*((n-1)/(n-k-1))

tibble(RMSE = round(RMSE,1),
       R2, Adjusted_R2)

