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
        defaultColors = list(),
        layers = list(),
        legend = list(),
        controls = list(),
        filters = list()
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
  defaultColors = function(..., replace = T){
    params$defaultColors <<- setSpec(params$defaultColors, ..., replace = replace)
  },   
  legend = function(...){
    .self$set(legend = list(...))
  },
  addFilters = function(...){
    .self$setTemplate(
      page = 'rChartControls2.html',
      script = system.file('libraries', lib, 'controls', 
                           'chart_multiselect.html', package = 'rCharts')
    )
    .self$set(width = 700)
    params$filters <<- toJSONArray2(ldply(list(...), function(y) {
      data.frame(variable = y, value = unique(params$data[[y]]))
    }), json = F)
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
    data = to_json(params$data, orient="records")
    #there is potential to  chain the entire thing
    #making much cleaner
    #need to explore this
    #as of now though chart is not being used
    #offer chart chain if a user might like to take advantage
    chart = toChain3(params$chart, 'myChart')
    controls_json = toJSON(params$controls)
    controls = setNames(params$controls, NULL)
    filters_json = toJSON(params$filters)
    opts = toJSON2(params[!(names(params) %in% c('data', 'chart', 'controls','filters'))])
    list(
      opts = opts
      , data = data
      , chart = chart, chartId = chartId
      , controls = controls, controls_json = controls_json
      , filters_json = filters_json, hasFilter = (length(params$filters) > 0)
    )
  }
))