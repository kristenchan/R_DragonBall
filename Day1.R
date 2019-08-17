library(knitr)
library(rmdformats)
library(DT)
library(data.table)
library(Metrics)

## Global options
options(max.print="75", digits = 4, scipen = 999)

opts_chunk$set(echo=TRUE,
               cache=TRUE,
               prompt=FALSE,
               tidy=TRUE,
               comment=NA,
               message=FALSE,
               warning=FALSE)
opts_knit$set(width=75)

# Impute monthly mean in Ozone
for (i in 1:nrow(airquality)){
  if(is.na(airquality[i,"Ozone"])){
    airquality[i,"Ozone"]<- mean(airquality[which(airquality[,"Month"]==airquality[i,"Month"]),"Ozone"],na.rm = TRUE)
  }
  # Impute monthly mean in Solar.R
  if(is.na(airquality[i,"Solar.R"])){
    airquality[i,"Solar.R"]<- mean(airquality[which(airquality[,"Month"]==airquality[i,"Month"]),"Solar.R"],na.rm = TRUE)
  }
  
}

library(caTools)
set.seed(1)
split = sample.split(dataset$Ozone, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting Multiple Linear Regression to the Training set
regressor = lm(formula = Ozone ~ .,
               data = training_set)

# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)

rmse(test_set$Ozone, y_pred)