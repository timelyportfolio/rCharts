
<!-- saved from url=(0060)https://raw.github.com/ramnathv/rCharts/dev/R/rChartsClass.R -->
<html><head><meta http-equiv="Content-Type" content="text/html; charset=UTF-8"></head><body><pre style="word-wrap: break-word; white-space: pre-wrap;">rCharts = setRefClass('rCharts', list(params = 'list', lib = 'character', 
    LIB = 'list', srccode = 'ANY', tObj = 'list', container = 'character', 
    html_id = 'character', templates = 'list'), methods = list(
  initialize = function(){
    srccode &lt;&lt;- NULL     # source code to create the chart
    html_id &lt;&lt;- ""       # no id initially
    tObj &lt;&lt;- list()      # 
    lib &lt;&lt;- tolower(as.character(class(.self)))
    LIB &lt;&lt;- get_lib(lib) # library name and url to library folder
    container &lt;&lt;- 'div'  # type of container holding the chart
    params &lt;&lt;- list(
      dom = basename(tempfile('chart')),       # id of dom element of chart
      width = getOption('RCHART_WIDTH', 800),  # width of the container
      height = getOption('RCHART_HEIGHT', 400) # height of the container
    )
    templates &lt;&lt;- list(page = 'rChart.html', chartDiv = NULL, 
      script =  file.path(LIB$url, 'layouts', 'chart.html'))
    templates$chartDiv &lt;&lt;- "
      &lt;{{container}} id = '{{ chartId }}' class = 'rChart {{ lib }}'&gt;
      &lt;/{{ container}}&gt;"
  },
  addParams = function(...){
    params &lt;&lt;- modifyList(params, list(...))
  },
  addControls = function(nm, value, values, label = paste("Select ", nm, ":")){
    .self$setTemplate(
      page = 'rChartControls.html',
      script = system.file('libraries', lib, 'controls', 
        'script.html', package = 'rCharts')
    )
    .self$set(width = 700)
    control = list(name = nm, value = value, values = values, label = label)
    params$controls[[nm]] &lt;&lt;- control
  },
  setTemplate = function(...){
    templates &lt;&lt;- modifyList(templates, list(...))
  },
  setLib = function(lib, ...){
    lib &lt;&lt;- lib; LIB &lt;&lt;- get_lib(lib)
    templates &lt;&lt;- modifyList(list(
      page = 'rChart.html', 
      chartDiv = "&lt;{{container}} id = '{{ chartId }}' class = 'rChart {{ lib }}'&gt;&lt;/{{ container}}&gt;", 
      script =  file.path(LIB$url, 'layouts', 'chart.html')
    ), list(...))
  },
  set = function(...){
    # this is a hack, currently for external libraries
    # idea is to initialize LIB, since the set method will always be used.
    if (length(LIB) == 0 || LIB$url == ""){
      LIB &lt;&lt;- get_lib(lib)
    }
    params &lt;&lt;- modifyList(params, list(...))
  },
  getPayload = function(chartId){
    list(chartParams = toJSON(params), chartId = chartId, lib = basename(lib))
  },
  html = function(chartId = NULL){
    params$dom &lt;&lt;- chartId %||% params$dom
    params$id &lt;&lt;- params$dom
    # template = read_file(templates$script)
    html = render_template(templates$script, getPayload(params$dom))
    return(html)
  },
  print = function(chartId = NULL, include_assets = F, ...){
    params$dom &lt;&lt;- chartId %||% params$dom
    assetHTML &lt;- ifelse(include_assets, add_lib_assets(lib, ...), "")
    # if (is.null(templates$chartDiv)){
    #   chartDiv =  sprintf("&lt;%s id='%s' class='rChart %s'&gt;&lt;/%s&gt;", 
    #     container, params$dom, LIB$name, container)
    # } else {
    #   chartDiv = render_template(templates$chartDiv, 
    #       list(chartId = params$dom))
    # }
    chartDiv = render_template(templates$chartDiv, list(
      chartId = params$dom,
      lib = LIB$name,
      container = container
    ))
    writeLines(c(assetHTML, chartDiv, .self$html(params$dom)))
  },
  render = function(chartId = NULL, cdn = F){
    params$dom &lt;&lt;- chartId %||% params$dom
    template = read_template(getOption('RCHART_TEMPLATE', templates$page))
    html = render_template(template, list(
      params = params,
      assets = get_assets(LIB, static = T, cdn = cdn),
      chartId = params$dom,
      script = .self$html(params$dom),
      CODE = srccode,
      lib = LIB$name,
      tObj = tObj,
      container = container
    ))
  },
  save = function(destfile = 'index.html', ...){
    'Save chart as a standalone html page'
    writeLines(.self$render(...), destfile)
  },
  show = function(mode_ = NULL, ...){
    mode_ = getMode(mode_)
    switch(mode_, 
      static = {
        writeLines(.self$render(...), tf &lt;- tempfile(fileext = '.html'))
        browseURL(tf)
      },
      server = {
        shiny_copy = .self$copy()
        shiny_copy$params$dom = 'show'
        assign(".rChart_object", shiny_copy, envir = .GlobalEnv)
        shiny::runApp(file.path(system.file(package = "rCharts"), "shiny"))
      },
      inline = {
        add_ext_widgets(lib)
        return(.self$print(...))
      },
      iframe = {
        chunk_opts_ = opts_current$get() 
        file_ = knitr:::fig_path('.html', chunk_opts_)
        if (!file.exists(dirname(file_))){
          dir.create(dirname(file_))
        }
        cdn = !(chunk_opts_$rcharts %?=% 'draft')
        .self$save(file_, cdn = cdn)
        cat(sprintf("&lt;iframe src=%s seamless&gt;&lt;/iframe&gt;", file_))
        return(invisible())
      }    
    )
  },
  show2 = function(static = T, ...){
    if (!is.null(getOption('rcharts.vis.tag')) &amp;&amp;
          getOption("rcharts.vis.tag") == 'iframe'){
      file_ = sprintf("assets/img/%s.html", params$dom)
      .self$save(file_)
      cat(sprintf("&lt;iframe src=%s&gt;&lt;/iframe&gt;", file_))
      return(invisible())
    }
    if (!is.null(getOption("knitr.in.progress")) &amp;&amp; 
        getOption("knitr.in.progress")){
      add_ext_widgets(lib)
      return(.self$print())
    }
    if (static){
      writeLines(.self$render(...), tf &lt;- tempfile(fileext = '.html'))
      browseURL(tf)
    } else {
      shiny_copy = .self$copy()
      shiny_copy$params$dom = 'show'
      assign(".rChart_object", shiny_copy, envir = .GlobalEnv)
      shiny::runApp(file.path(system.file(package = "rCharts"), "shiny"))
    }
  },
  publish = function(description = "", id = NULL, ..., host = 'gist'){
    htmlFile = file.path(tempdir(), 'index.html'); on.exit(unlink(htmlFile))
    .self$save(destfile = htmlFile, cdn = T)
    # imgFile = file.path(tempdir(), 'thumbnail.png'); on.exit(unlink(imgFile))
    # take_screenshot(htmlFile, tools::file_path_sans_ext(imgFile))
    if (!is.null(.self$srccode)){
      codeFile = file.path(tempdir(), 'code.R'); on.exit(unlink(codeFile))
      writeLines(.self$srccode, con = codeFile)
      files = c(htmlFile, codeFile)
    } else {
      files = htmlFile
    }
    class(files) = host
    if (is.null(id) &amp;&amp; (html_id != "")){
      id = html_id
    }
    html_id &lt;&lt;- publish_(files = files, description = description, id = id, ...)
  }
))

add_ext_widgets &lt;- function(lib){
  libpath = paste('libraries', lib, sep = "/")
  if (exists('.SLIDIFY_ENV') &amp;&amp; 
      !(libpath %in% .SLIDIFY_ENV$ext_widgets$rCharts)){
    rcharts_widgets = .SLIDIFY_ENV$ext_widgets$rCharts
    len = length(rcharts_widgets)
    .SLIDIFY_ENV$ext_widgets$rCharts[[len + 1]] &lt;&lt;- libpath
  }
}

# getMode = function(mode_){
#   # if mode_ is specified as argument, just return it
#   if(!is.null(mode_)){
#     return(mode_)
#   }
#   # if mode_ is specified as options, just return it
#   if(!is.null(getOption('rcharts.mode'))){
#     return(getOption('rcharts.mode'))
#   }
#   # if knitr is in progress, return mode = iframe, else static
#   if(!is.null(getOption('knitr.in.progress'))){
#     mode_ = 'iframe'
#   } else {
#     mode_ = 'static'
#   }
#   return(mode_)
# }

getMode = function(mode_){
  default = ifelse(getOption('knitr.in.progress') %?=% TRUE, 'iframe', 'static')
  mode_ = mode_ %||% getOption('rcharts.mode') %||% default
  return(mode_)
}

`%?=%` &lt;- function(x, y){
  ifelse(!is.null(x), x == y, FALSE)
}

</pre></body></html>