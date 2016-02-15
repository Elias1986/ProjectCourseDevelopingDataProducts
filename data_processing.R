# Load required libraries
require(data.table)
# library(sqldf)
library(dplyr)
library(DT)
library(rCharts)

# Read data
data <- fread("sets.csv")
head(data)
setnames(data, "b1", "brand")
setnames(data, "b2", "product no.")
setnames(data, "cl", "client")
length(unique(data$client)) # 1534
table(data$week) # 0 - 70
length(table(data$week)) # 65
weeks <- sort(unique(data$week))
length(table(data$brand)) # 11
min(data$price) # 5
max(data$price) # 150
brands <- sort(unique(data$brand))

groupByweekAll <- function(dt, minweek, maxweek, minprice,
                           maxprice, brands) {
        result <- dt %>% filter(week >= minweek, week <= maxweek,
                                price >= minprice, price <= maxprice,
                                brand %in% brands) 
        return(result)
}

groupByweek <- function(dt, minweek, maxweek) {
        result <- dt %>% filter(week >= minweek, week <= maxweek) 
        return(result)
}

groupByweekclient <- function(dt, minweek, maxweek, 
                           minprice, maxprice, brands) {
        dt <- groupByweek(dt, minweek, maxweek)
        result <- dt %>% 
                group_by(week) %>% 
                summarise(total_sets = n_distinct(client)) %>%
                arrange(week)
        return(result) 
}

groupBybrand <- function(dt, minweek, maxweek, 
                         minprice, maxprice, brands) {
        dt <- groupByweekAll(dt, minweek, maxweek, minprice,
                             maxprice, brands) 
        result <- datatable(dt, options = list(iDisplayLength = 50))
        return(result)
       
}

groupByweekAgg <- function(dt, minweek, maxweek, minprice,
                           maxprice, brands) {
        dt <- groupByweek(dt, minweek, maxweek)
        result <- dt %>% 
                group_by(week)  %>% 
                summarise(count = n_distinct(brand)) %>%
                arrange(week)
        return(result)
}


groupBypriceAvg <- function(dt,  minweek, maxweek, minprice,
                            maxprice, brands) {
        dt <- groupByweekAll(dt, minweek, maxweek, minprice,
                             maxprice, brands)
        result <- dt %>% 
                group_by(week) %>% 
                summarise(avg = mean(price)) %>%
                arrange(week)
        return(result)      
}


groupBypricebrandAvg <- function(dt,  minweek, maxweek, minprice,
                                 maxprice, brands) {
        dt <- groupByweekAll(dt, minweek, maxweek, minprice,
                             maxprice, brands)
        result <- dt %>% 
                group_by(brand) %>%
                summarise(avgprice = mean(price)) %>%
                arrange(brand)
        return(result)
}


plotclientCountByweek <- function(dt, dom = "setsByweek", 
                                xAxisLabel = "week",
                                yAxisLabel = "Number of Sets") {
        setsByweek <- nPlot(
                total_sets ~ week,
                data = dt,
                type = "stackedAreaChart",
                dom = dom, width = 650
        )
        setsByweek$chart(margin = list(left = 100))
        setsByweek$chart(color = c('purple', 'blue', 'green'))
        setsByweek$chart(tooltipContent = "#! function(key, x, y, e){ 
                         return '<h5><b>week</b>: ' + e.point.week + '<br>' + '<b>Total Sets</b>: ' 
                         + e.point.total_sets + '<br>'
                         + '</h5>'
} !#")
        setsByweek$yAxis(axisLabel = yAxisLabel, width = 80)
        setsByweek$xAxis(axisLabel = xAxisLabel, width = 70)
        setsByweek 
        }

plotbrandsCountByweek <- function(dt, dom = "brandsByweek", 
                                  xAxisLabel = "week",
                                  yAxisLabel = "Number of brands") {
        brandsByweek <- nPlot(
                count ~ week,
                data = dt,
                type = "multiBarChart",
                dom = dom, width = 650
        )
        brandsByweek$chart(margin = list(left = 100))
        brandsByweek$yAxis(axisLabel = yAxisLabel, width = 80)
        brandsByweek$xAxis(axisLabel = xAxisLabel, width = 70)
        brandsByweek$chart(tooltipContent = "#! function(key, x, y, e){ 
                           return '<h5><b>week</b>: ' + e.point.week + '<br>' + '<b>Total brands</b>: ' + e.point.count + '<br>'
                           + '</h5>'
} !#")
        brandsByweek
        }


plotpriceByweek <- function(dt, dom = "priceByweek", 
                             xAxisLabel = "week", 
                             yAxisLabel = "Number of price") {
        priceByweek <- nPlot(
                price ~ week,
                data = dt,
                type = "scatterChart",
                dom = dom, width = 650
        )
        priceByweek$chart(margin = list(left = 100), 
                           showDistX = TRUE,
                           showDistY = TRUE)
        priceByweek$chart(color = c('green', 'orange', 'blue'))
        priceByweek$chart(tooltipContent = "#! function(key, x, y, e){ 
                           return '<h5><b>Set Name</b>: ' + e.point.name + '<br>'
                           + '<b>Set ID</b>: ' + e.point.setId + '<br>'
                           + '<b>brand</b>: ' + e.point.brand
                           + '</h5>'
} !#")
        priceByweek$yAxis(axisLabel = yAxisLabel, width = 80)
        priceByweek$xAxis(axisLabel = xAxisLabel, width = 70)
        #     priceByweek$chart(useInteractiveGuideline = TRUE)
        priceByweek
        }

plotpriceByweekAvg <- function(dt, dom = "priceByweekAvg", 
                                xAxisLabel = "week",
                                yAxisLabel = "Number of price") {
        
        priceByweekAvg <- nPlot(
                avg ~ week,
                data = dt,
                type = "lineChart",
                dom = dom, width = 650
        )
        priceByweekAvg$chart(margin = list(left = 100))
        priceByweekAvg$chart(color = c('orange', 'blue', 'green'))
        priceByweekAvg$yAxis(axisLabel = yAxisLabel, width = 80)
        priceByweekAvg$xAxis(axisLabel = xAxisLabel, width = 70)
        priceByweekAvg
}

plotpriceBybrandAvg <- function(dt, dom = "priceBybrandAvg", 
                                 xAxisLabel = "brands", 
                                 yAxisLabel = "Number of price") {
        priceBybrandAvg <- nPlot(
                avgprice ~ brand,
                data = dt,
                type = "multiBarChart",
                dom = dom, width = 650
        )
        priceBybrandAvg$chart(margin = list(left = 100))
        priceBybrandAvg$chart(color = c('pink', 'blue', 'green'))
        priceBybrandAvg$yAxis(axisLabel = yAxisLabel, width = 80)
        priceBybrandAvg$xAxis(axisLabel = xAxisLabel, width = 200,
                               rotateLabels = -20, height = 200)
        priceBybrandAvg
}