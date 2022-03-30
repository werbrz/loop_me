### upload libraries -------------------------------------------------------------------------------


library(data.table)
library(ggplot2)

### read data --------------------------------------------------------------------------------------

data <- setDT(data.table::fread('data/Walmart.csv'))
data[, ':='(Store = as.factor(Store),
            Date  = as.Date(Date, origin = '1970-01-01', tryFormats = c("%d-%m-%Y")))]



### Asumptions -------------------------------------------------------------------------------------
# let's assume that we need to generate forecasts for the next 4 week

test_start <- '2012-10-05'
test_end   <- '2012-10-26'

test  <- data[Date >= test_start &
             Date < test_end]
train <- data[Date < test_start]


test <- test[, Weekly_Sales := NA] 
train <- rbind(train, test) 


### Standarization ---------------------------------------------------------------------------------
# standarize data to make them consistent

std <- train[, .(mean = mean(Weekly_Sales[Date > '2012-01-01' & Date < test_start], na.rm = TRUE),
                     sd   = sd(Weekly_Sales[Date > '2012-01-01' & Date < test_start], na.rm = TRUE)), by = Store]


train[, Weekly_Sales := (Weekly_Sales - mean(Weekly_Sales[Date > '2012-01-01' & Date < test_start],na.rm = TRUE))/sd(Weekly_Sales[Date > '2012-01-01' & Date < test_start],na.rm  = TRUE), by = Store]
train[, Temperature := (Temperature - mean(Temperature[Date > '2012-01-01' & Date < test_start], na.rm = TRUE))/sd(Temperature[Date > '2012-01-01' & Date < test_start], na.rm = TRUE)]
train[, Unemployment := (Unemployment - mean(Unemployment[Date > '2012-01-01' & Date < test_start], na.rm = TRUE))/sd(Unemployment[Date > '2012-01-01' & Date < test_start], na.rm = TRUE)]
train[, CPI := (CPI - mean(CPI[Date > '2012-01-01' & Date < test_start], na.rm = TRUE))/sd(CPI[Date > '2012-01-01' & Date < test_start], na.rm = TRUE)]



### Feature engineering ----------------------------------------------------------------------------
# Since there are no NA values in the dataset, we can skip the imputation step

# Creating variables

train <- train[order(Date)]


train[, `:=`(cum_mean    = cumsum(Weekly_Sales)/seq_along(Weekly_Sales),
                 week        = lubridate::week(Date),
                 month       = lubridate::month(Date)), by = list(Store, Holiday_Flag)]


train[, `:=`(roll_mean_store_week  = frollapply(Weekly_Sales, 4, 'median'),
                 cum_mean_week    = cumsum(Weekly_Sales)/seq_along(Weekly_Sales)), by = list(Store, Holiday_Flag, week)]


train[, `:=`(mean_store_month  = mean(Weekly_Sales, na.rm = TRUE),
                 cum_mean_month    = cumsum(Weekly_Sales)/seq_along(Weekly_Sales)), by = list(Store, Holiday_Flag, month)]


test  <- train[Date >= test_start]
train <- train[Date < test_start]

