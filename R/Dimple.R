dPlot <- dimplePlot <- function(x, data, ...){
  myChart <- Dimple$new()
  myChart$getChartParams(x, data, ...)
  return(myChart$copy())
}

Dimple <- setRefClass('Dimple', contains = 'rCharts', methods = list(
  initialize = function(){
    callSuper(); 
    params <<- c(
      params,
      list(
        chart = list(),
        xAxis = list(type="addCategoryAxis", showPercent = FALSE),
        yAxis = list(type="addMeasureAxis", showPercent = FALSE),
        zAxis = list(),
        colorAxis = list(),
        layers = list(),
        legend = list()
    ))
  },
  chart = function(..., replace = F){
    params$chart <<- setSpec(params$chart, ..., replace = replace)
  },
  xAxis = function(..., replace = F){
    params$xAxis <<- setSpec(params$xAxis, ..., replace = replace)
  },
  yAxis = function(..., replace = F){
    params$yAxis <<- setSpec(params$yAxis, ..., replace = replace)
  },
  zAxis = function(..., replace = F){
    params$zAxis <<- setSpec(params$zAxis, ..., replace = replace)
  },
  colorAxis = function(...){
    .self$set(colorAxis = list(...))
  },
  legend = function(...){
    .self$set(legend = list(...))
  },
  getChartParams = function(...){
    params <<- modifyList(params, getLayer(...))
  },
  #add layer functionality from polycharts
  #dimple allows us to add series
  layer = function(..., copy_layer = F){
    len = length(params$layers)
    if (!copy_layer){
      params$layers[[len + 1]] <<- getLayer(...)
    } else {
      params$layers[[len + 1]] <<- merge_list(list(...), params$layers[[len]])
    }
  },
  getPayload = function(chartId){
    data = toJSONArray(params$data)
    #there is potential to  chain the entire thing
    #making much cleaner
    #need to explore this
    #as of now thought chart is not being used
    chart = toChain(params$chart, 'myChart')
    opts = rjson::toJSON(params[!(names(params) %in% c('data', 'chart'))])
    list(opts = opts, data = data, 
         chart = chart, chartId = chartId)
  }
))