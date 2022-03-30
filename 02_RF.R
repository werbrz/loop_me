source('01_Feature_eng.R')

### Prepare ML model and forecasts using h2o

library(h2o)

h2o.init(nthreads=2)
h2o.removeAll()

# transfer data to h2o

train_h2o <- as.h2o(train)
test_h2o  <- as.h2o(test)

# prepare the model

h2o_rf <- h2o.randomForest(
  x                        = names(train_h2o)[!names(train_h2o) == 'Weekly_Sales'],
  y                        = 'Weekly_Sales',
  training_frame           = train_h2o,
  ntrees                   = 70,
  sample_rate              = 0.8,
  stopping_metric          = "mae",
  stopping_rounds          = 5,
  nfolds                   = 5,
  seed                     = 1234,
  keep_cross_validation_predictions = TRUE
)

h2o.performance( h2o_rf, xval = TRUE)


# generate forecasts

forecast <- as.data.table(h2o.predict(h2o_rf, test_h2o))
forecast <- cbind(forecast,test[, list(Date, Store)] )

h2o.varimp_plot(h2o_rf)
setDT(forecast)
forecast <- merge(forecast, std, all = TRUE, by = 'Store')
forecast <- forecast[is.na(predict) == FALSE,]

h2o.shutdown(prompt = FALSE)



### evaluate forecasts -----------------------------------------------------------------------------


forecast[, predict := predict*sd + mean]

check_QA <- setDT(merge(forecast[, list(Date, predict, Store)], 
                        data[, list(Date, Weekly_Sales, Store)], all.x =  TRUE, by = c('Date', 'Store')))

check_QA[, .(MAPE = mean(abs((Weekly_Sales - predict)/Weekly_Sales)) * 100,
             MAE  = mean(abs(Weekly_Sales - predict)))]



