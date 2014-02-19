xCharts = setRefClass('xCharts', contains = 'rCharts', methods = list(
  initialize = function(){
    callSuper()
    container <<- 'figure'
  },
  layer = function(...){
    params <<- merge_list(getLayer(...), params)
  },  
  getPayload = function(chartId){
    list(
      chartParams = toJSON(params[!(names(params) %in% c('type'))]), 
      chartId = chartId,
      chartType = params$type
    )
  }
))

xPlot <- function(x, ...){
  UseMethod('xPlot')
}

xPlot.default <- function(x, y, dataL, ...){
  myChart <- xCharts$new()
  myChart$layer(x = x, y = y, data = data, ...)
  return(myChart$copy())
}

xPlot.formula <- function(x, data, ...){
  myChart <- xCharts$new()
  myChart$layer(x, data, ...)
  return(myChart$copy())
}
