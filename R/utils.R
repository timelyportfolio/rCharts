open_notebook <- function(rmdFile = NULL){
  if (!is.null(rmdFile)) {
    options(NOTEBOOK_TO_OPEN = normalizePath(rmdFile))
    on.exit(options(NOTEBOOK_TO_OPEN = NULL))
  }
  app <- system.file('apps', 'notebook', package = 'rCharts')
  shiny::runApp(app)
}

open_notebook_list <- function (rmdFile = NULL) {
  require(shiny)
  
  #if no rmd is provided, use example.Rmd in rCharts package
  if (is.null(rmdFile)) {
    rmdFile <- system.file("/apps/notebook/www/example.Rmd",package="rCharts")
  }
  
  #libraries to include from rCharts
  #include all libraries, but might want to parameterize
  libs = list.dirs(system.file("libraries",package="rCharts"),recursive=FALSE)
  foo <- function(libs){
    require(rCharts)
    add_rCharts(libs)
    tagList(lapply(libs, get_rCharts_assets))
  }
  
  options(device.ask.default = FALSE)
  allow_knit = TRUE
  
  getHead <- function() {
    addResourcePath("assets",system.file("/apps/notebook/www/assets",package="rCharts"))
    tagList(
      singleton(
        tags$head(
          tags$title('An R Notebook in Shiny'),
          # tags$script(src = 'http://ace.ajax.org/build/src-min-noconflict/ace.js',
          #  type = 'text/javascript', charset = 'utf-8'),
          tags$script(src='assets/ace/js/ace.js', type = 'text/javascript', charset = 'utf-8'),
          tags$script(src='assets/app.js', type = 'text/javascript', charset = 'utf-8'),
          tags$link(rel = 'stylesheet', type = 'text/css', href = 'assets/ace-shiny.css'),
          tags$link(rel = 'stylesheet', type = 'text/css', href = 'assets/styles.css'),
          tags$script(src = 'assets/highlight.pack.js', type = 'text/javascript'),
          tags$script('hljs.initHighlightingOnLoad();'),
          tags$link(rel = 'stylesheet', type = 'text/css', href = 'assets/tomorrow.css'),
          tags$script(src = 'https://c328740.ssl.cf1.rackcdn.com/mathjax/2.0-latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML', type = 'text/javascript'),
          suppressWarnings(foo(libs))
        )
      )
    )
  }
  
  app <- list(
    ui = bootstrapPage(
      getHead(),
      tags$body(
        includeHTML(system.file("/apps/notebook/www/header.html",package="rCharts")),
        tags$section(id = "main",
                     div(id = 'notebook', title = 'Compile notebook: F4\nInsert chunk: Ctrl+Alt+I',
                         paste(readLines(rmdFile), collapse = '\n')),
                     # uiOutput('notebook'),
                     tags$textarea(id = 'nbSrc', style = 'display: none;'),
                     div(id = 'nbOut', class='shiny-html-output')
        )
      ),
      tags$script(src = 'assets/ace-shiny.js', type = 'text/javascript')
    ),
    server = function(input, output) {
      output$nbOut2 = reactive({
        src = input$nbSrc
        chart <- source(src, local = TRUE)$value
        paste(chart$render(), collapse = '\n')
      })
      
      output$downloadRmd <- downloadHandler(
        filename = 'myapp.Rmd',
        content = function(file){
          cat(input$nbSrc, file = file)
        }
      )
      
      output$nbOut = reactive({
        src = input$nbSrc
        if (input$modevariable == "R"){
          src = paste0(c(
            "```{r echo = F, results = 'asis', message = F, warning = F, comment = NA}", 
            input$nbSrc, 
            "\n```", collapse = "\n")
          )
        }
        if (allow_knit == TRUE){
          library(knitr)
          figdir = tempdir(); on.exit(unlink(figdir))
          opts_knit$set(progress = FALSE, fig.path = figdir)
          if (length(src) == 0L || src == '')
            return('Nothing to show yet...')
          on.exit(unlink('figure/', recursive = TRUE)) # do not need the figure dir
          src = paste(c(readLines(system.file("/apps/notebook/www/highlight.html",package="rCharts")), src), collapse = '\n')
          paste(try(knit2html(text = src, fragment.only = TRUE)))
        } else {
          paste(c(readLines(system.file("/apps/notebook/www/example.html",package="rCharts")), readLines(system.file("/apps/notebook/www/highlight.html",package="rCharts"))),
                collapse = '\n')
        }
      })
    }
  )
  shiny::runApp(app)
}

add_rCharts <- function(libs){
  LIBS <- lapply(libs, get_lib)
  invisible(lapply(LIBS, function(LIB){
    suppressMessages(singleton(addResourcePath(LIB$name, LIB$url)))
  }))
  return(NULL)
}

get_rCharts_assets <- function(lib){
  LIB <- get_lib(lib)
  assets = get_assets_shiny(LIB)
  assets[!grepl('jquery', assets)]
}

get_lib <- function(lib){
  if (grepl("^http", lib)){
    return(list(name = basename(lib), url = lib))
  }
  if (file.exists(lib)){
    lib_url <- normalizePath(lib)
    lib <- basename(lib_url)
  } else {
    lib_url <- system.file('libraries', lib, package = 'rCharts')
  }
  return(list(name = basename(lib), url = lib_url))
}

get_assets <- function(LIB, static = T, cdn = F){
  config = yaml.load_file(file.path(LIB$url, 'config.yml'))[[1]]
  if (cdn) {
    config$cdn 
  } else {
    assets = config[names(config) != 'cdn']
    prefix = ifelse(static, LIB$url, LIB$name)
    lapply(assets, function(asset) paste(prefix, asset, sep = '/'))
  }
}

#' Add library assets (useful in knitr documents)
add_lib_assets <- function(lib, cdn = F){
  assets = get_assets(get_lib(lib), cdn = cdn)
  styles <- lapply(assets$css, function(style){
    sprintf("<link rel='stylesheet' href=%s>", style)
  })
  scripts <- lapply(assets$jshead, function(script){
    sprintf("<script type='text/javascript' src=%s></script>", script)
  })
  paste(c(styles, scripts), collapse = '\n')
}

#' Set a default value for an object
#' 
#' This function sets the value of an object to a default value if it is not defined. 
#' @params x object
#' @params y object
#' @keywords internal
#' @noRd
`%||%` <- function(x, y){
  if (is.null(x)) y else x
}


merge_list <- function (x, y, ...){
  if (length(x) == 0) 
    return(y)
  if (length(y) == 0) 
    return(x)
  i = match(names(y), names(x))
  i = is.na(i)
  if (any(i)) 
    x[names(y)[which(i)]] = y[which(i)]
  return(x)
}

setSpec = function(spec, ... , replace = F){
  if (replace){
    list(...)
  } else {
    modifyList(spec, list(...))
  }
}

addSpec <- function(...){
  UseMethod('addSpec')
}

addSpec.default <- function(...){
  list(...)
}

addSpec.character <- function(...){
  yaml::yaml.load(...)
}

addLayer <- function(x, ...){
  UseMethod('addLayer')
}

addLayer.default <- function(...){
  
}

#' Read contents of a file into a character string
#' 
#' @params file path to text file that needs to be read
#' @params warn logical. Warn if a text file is missing a final EOL
#' @params ... other parameters to be passed to \code{\link{readLines}}
#' @keywords internal
#' @noRd
read_file <- function(file, warn = F, ...){
  paste(readLines(file, warn = warn, ...), collapse = "\n")
}

#' Read contents of a system file into a character string
#'
#' @params ... character vectors, specifying subdirectory and file(s) within some package. 
#' @params package name of the package
#' 
#' @keywords internal
#' @noRd
#  TODO: Rename this to read_sysfile to better convey what it does.
#  This function needs to be refactored
read_template <- function(..., package = 'rCharts'){
  if (is.null(package)){
    template = file.path(...)
  } else {
    template = system.file(..., package = package)
  }
  read_file(template)
}

#' Render mustache template and capture output ready to be written into a file
#'
#' @params ... arguments to be passed to 
#' @keywords internal
#' @import whisker
#' @noRd
render_template <- function(..., data = parent.frame(1)){
  paste(capture.output(cat(whisker.render(...))), collapse = "\n")
}


# tpl <- '{{# items }} {{{.}}}\n {{/ items}}'
# items <- letters[1:5]
# render_template(tpl)
# render_template <- function(template, data = parent.frame(1), ...){
#   paste(capture.output(cat(whisker.render(template, data = data, ...))), collapse = '\n')
# }