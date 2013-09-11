dPlot <- dimplePlot <- function(x, data, ...){
  myChart <- Dimple$new()
  myChart$getChartParams(x, data, ...)
  return(myChart$copy())
}

Dimple <- setRefClass('Dimple', contains = 'rCharts', methods = list(
  initialize = function(){
    callSuper(); 
    params <<- c(params, list(controls = list(),
      chart = list(), xAxis = list(type="addCategoryAxis", showPercent = FALSE),
      yAxis = list(type="addMeasureAxis", showPercent = FALSE),
      zAxis = list(), colorAxis = list(), defaultColors = list(),
      legend = list(), facet = list()
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
  defaultColors = function(..., replace = F){
    params$defaultColors <<- setSpec(params$defaultColors, ..., replace = replace)
  },    
  colorAxis = function(...){
    .self$set(colorAxis = list(...))
  },
  legend = function(...){
    .self$set(legend = list(...))
  },
  facet = function(...){
    .self$set(facet = list(...))
  },
  getChartParams = function(...){
    params <<- modifyList(params, getLayer(...))
  },
  getPayload = function(chartId){
    #l = plyr::alply(params$data, 1, as.list)
    #names(l) = NULL
    #data = rjson::toJSON(l)
    data = toJSONArray(params$data)
    #there is potential to  chain the entire thing
    #making much cleaner
    #need to explore this
    #as of now thought chart is not being used
    chart = toChain(params$chart, 'myChart')
       #cannot eliminate so changed toChain to toJSON
    #but need to revert back to toChain for the axes
    xAxis = toJSON(params$xAxis) #toChain(params$xAxis, 'chart.xAxis')
    yAxis = toJSON(params$yAxis) #toChain(params$yAxis, 'chart.yAxis')
    zAxis = toJSON(params$zAxis)
    colorAxis = toJSON(params$colorAxis)
    defaultColors = toJSON2(params$defaultColors)
    controls_json = toJSON(params$controls)
    controls = setNames(params$controls, NULL)     
    legend = toJSON(params$legend)
    opts = toJSON(params[!(names(params) %in% c('data', 'chart', 'xAxis', 'yAxis', 'zAxis', 'colorAxis', 'defaultColors', 'legend','controls'))])
    list(opts = opts, xAxis = xAxis, yAxis = yAxis, zAxis = zAxis,
         colorAxis = colorAxis, defaultColors = defaultColors,
         legend = legend, controls = controls, 
         controls_json = controls_json, data = data, 
         chart = chart, chartId = chartId, CODE = srccode)
  }
))