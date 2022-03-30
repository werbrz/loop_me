### upload libraries -------------------------------------------------------------------------------

library(data.table)
library(ggplot2)


### read data --------------------------------------------------------------------------------------

data <- setDT(data.table::fread('data/Walmart.csv'))
  
head(data)

data[, ':='(Store = as.factor(Store),
            Date  = as.Date(Date, origin = '1970-01-01', tryFormats = c("%d-%m-%Y")))]


### quick data check -------------------------------------------------------------------------------

data.frame(lapply(data,function(x){sum(is.na(x) == TRUE)}))  # NA data
data.frame(lapply(data,function(x){sum(is.na(x) == FALSE)})) # non-NA data
data.frame(lapply(data, function(x){length(unique(x))}))     # unique values each column

data[, .(min_date = min(Date),
         max_date = max(Date)), by = Store][,.(n_store = length(unique(Store))), keyby = list(min_date,max_date)] # date range

ggplot(data = data[, .(mean_sale = mean(Weekly_Sales)), by = Store],
      aes(mean_sale)) + geom_histogram(bins = 10) + theme_bw()      # mean_sales



### kmean - cluster stores -------------------------------------------------------------------------
# cluster stores to facilitate analysis

library(factoextra)

# create and format table
kmeans_table <- data[,.(
  sd    = sd(Weekly_Sales),
  v     = mean(Weekly_Sales)/sd(Weekly_Sales),
  skew  = (sum(Weekly_Sales - mean(Weekly_Sales))^3)/length(Weekly_Sales),
  kurt  = (sum(Weekly_Sales - mean(Weekly_Sales))^4)/length(Weekly_Sales),
  IQR   = quantile(Weekly_Sales, probs = 0.75) - quantile(Weekly_Sales, probs = 0.25),
  range = max(Weekly_Sales) - min(Weekly_Sales)), by = Store]

rownames(kmeans_table) <- kmeans_table$Store
kmeans_table$Store <- NULL


# check numbers of clusters
fviz_nbclust(kmeans_table, FUNcluster = kmeans)


store_clusters <- data.frame(clusters = as.factor(kmeans(kmeans_table, centers = 4)$cluster),
                             Store    = as.factor(rownames(kmeans_table)))
data           <- merge(x = data, y = store_clusters, by = 'Store', all.x = TRUE)


ggplot(data = data,
       aes(x = Weekly_Sales, group = Store)) +geom_density(aes(fill = Store), alpha = 0.25, show.legend = FALSE) + theme_bw() + facet_wrap(~clusters)


### analysis - other columns -----------------------------------------------------------------------

ggplot(data = reshape2::melt(data[, .(
  CPI_unique_val  = length(unique(CPI)),
  Fuel_unique_val = length(unique(Fuel_Price)),
  Unem_unique_val = length(unique(Unemployment))), by = Date],
id.vars = 'Date'),
aes(x = Date, y = value, color= variable)) + geom_line() + theme_bw() + facet_wrap(~variable)

cor(Filter(is.numeric, data), method = 'spearman')


### time series types ------------------------------------------------------------------------------
# using https://frepple.com/blog/demand-classification/ methodology

ts <- data[order(Date, Store)][, .(ADI = length(Date)/length(Date[Weekly_Sales != 0]),
                                   CV  = sd(Weekly_Sales)/mean(Weekly_Sales)), by = Store][,`:=`(ts =
                                                                                                   ifelse(ADI < 1.32  & CV < 0.49, 'Smooth',
                                                                                                          ifelse(ADI >= 1.32 & CV < 0.49, 'Intermittent',
                                                                                                                 ifelse(ADI < 1.32  & CV >= 0.49,'Erratic', 
                                                                                                                        'Lumpy')))), by = Store]



