

#------------------------“Fuel_Economy_Data”----------------------------------------

library(tidyverse)
library(h2o)
library(inspectdf)
library(skimr)
library(ggplot2)
library(plotly)
library(patchwork)

#-----------------Data understanding and data preprocessing--------------------------

ggplot2::mpg

eco <- mpg %>% view()

eco %>% glimpse()
eco %>% view()
eco %>% str()
eco %>% inspect_na()
eco %>% skim
eco_num <- eco %>% select_if(is.numeric) %>% select(cty,everything()) %>% view()
eco_chr <- eco %>% select_if(is.character) 
eco_num [,-1] <- eco_num [,-1] %>% scale() %>% as.data.frame() 
eco_chr <- fastDummies::dummy_cols(eco_chr,remove_first_dummy = T) %>% as.data.frame() %>% 
  select(-c(class,manufacturer,model,trans,drv,fl))
eco_chr %>% view()
eco <- cbind(eco_num,eco_chr) %>% select(cty,everything())
eco %>% view()


 
#defining target and features

target <- 'cty'

feature <- eco %>% select(year,cyl,displ) %>% names()
f <- as.formula(paste(target,paste(feature,collapse = '+'),sep='~'))
glm <- glm(f,data=eco)
glm %>% summary()

coef_na <- attributes(alias(glm)$complete)$dimnames[[1]]
feature <- feature[!feature %in% coef_na]


while (glm %>% faraway::vif() %>% sort(decreasing = T) %>% .[1]>=3){
  aftervif <- glm %>% faraway::vif()%>% sort(decreasing = T) %>% .[-1] %>% names() 
  f <- as.formula(paste(target,paste(aftervif,collapse = '+'),sep = '~'))
  glm <- glm(f,data=eco)
}

glm %>% faraway::vif() %>% sort(decreasing = T) %>% names() -> feature

eco <-  eco %>% select(target,feature)
h2o.init()
h2o_data <- eco %>% as.h2o()
h2o_data <- h2o_data %>% h2o.splitFrame(ratios = 0.8, seed=123)

train <- h2o_data[[1]]
test <- h2o_data[[2]]


#-------------------------------Modelling------------------------------------------------------

model <- h2o.glm(x=feature,y=target,training_frame = train,validation_frame = test,
                 nfolds = 5,seed = 123,lambda = 0,compute_p_values = T)
model@model$coefficients_table %>% as.data.frame() %>% 
 dplyr::select(names,p_value) %>% mutate(p_value=round(p_value,3)) %>% 
   .[-1,] %>% arrange(desc(p_value))

while (model@model$coefficients_table %>% as.data.frame() %>% 
  dplyr::select(names,p_value) %>% mutate(p_value=round(p_value,3)) %>% 
.[-1,] %>% arrange(desc(p_value)) %>% .[1,2]>0.05) {
  model@model$coefficients_table %>% as.data.frame() %>% 
     dplyr::select(names,p_value) %>% mutate(p_value=round(p_value,3)) %>% 
     filter(!is.nan(p_value)) %>% .[-1,] %>% 
    arrange(desc(p_value)) %>% .[1,1] -> v
  feature <- feature[feature!=v]
  
  model <- h2o.glm(
    x=feature,y=target,
    training_frame=train,
    validation_frame = test,
    nfolds=7,seed=123,
    lambda=0, compute_p_values = T)
  
}
 model@model$coefficients_table %>% as.data.frame() %>% 
   dplyr::select(names,p_value) %>% mutate(p_value=round(p_value,3))
 
 y_pred <- model %>% h2o.predict(newdata = test) %>% as.data.frame()
y_pred$predict 

#--------------------Checking metrics for evaluation of model-----------------------------------


test_s <- test %>% as.data.frame()
residuals = test_s$cty-y_pred$predict

#RMSE
RMSE <- sqrt(mean(residuals^2))

#R2
y_test_mean <- mean(test_s$cty)
total_sum_sq <- sum((test_s$cty-y_test_mean)^2)
residual_sum_sq <- sum((residuals)^2)
R2=1-(residual_sum_sq/total_sum_sq)

#Adj_R2
n <- test_s %>% nrow()
m <- feature %>% length()
Adj_R2 <- 1-(1-R2)*((n-1)/(n-m-1))

tibble(RMSE=round(RMSE,2),R2,Adj_R2)


new_e <- cbind(predicted=y_pred$predict ,observed=test_s$cty) %>% as.data.frame()



plot_model <- new_e %>% ggplot(aes(predicted,observed))+geom_point(color='darkblue')+
  geom_smooth(method=lm)+labs(x='Predicted value',y='Observed value')

plot_model %>% ggplotly()



y_pred <- model %>% h2o.predict(newdata = train) %>% as.data.frame()
y_pred$predict 


train_s <- train %>% as.data.frame()
residuals = train_s$cty-y_pred$predict

#RMSE
RMSE_tr <- sqrt(mean(residuals^2))

#R2
y_train_mean <- mean(train_s$cty)
total_sum_sq <- sum((train_s$cty-y_train_mean)^2)
residual_sum_sq <- sum((residuals)^2)
R2_tr=1-(residual_sum_sq/total_sum_sq)

#Adj_R2
n <- train_s %>% nrow()
m <- feature %>% length()

Adj_R2_tr <- 1-(1-r2)*((n-1)/(n-m-1))

tibble(RMSE_tr=round(RMSE_tr,2),R2_tr,Adj_R2_tr)


data_train <- cbind(predicted=y_pred$predict ,observed=train_s$cty) %>% as.data.frame()



plot_model_train <- data_train %>% ggplot(aes(predicted,observed,color='red'))+geom_point(color='black')+
  geom_smooth(method=lm)+labs(x='Predicted value',y='Observed value')

plot_model_train %>% ggplotly()


#Analyzing train and test results in one visual

plot_model+plot_model_train

tibble(RMSE_tr=round(RMSE_tr,1),
       RMSE=round(RMSE,1),
       Adj_R2_tr,
       Adj_R2=Adjusted_R2)


