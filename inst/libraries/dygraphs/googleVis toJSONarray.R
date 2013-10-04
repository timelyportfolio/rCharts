toJSONarray <- function(dtf){
  ## Thanks to Sebastian Kranz for this function
  ## Thanks also to Wei Luo: http://theweiluo.wordpress.com/2011/09/30/r-to-json-for-d3-js-and-protovis
  
  ## I subsequently took this from the googleVis package
  ## and modified to add labels for dygraphs
  
  #restore.point("toJSONarray")
  clnms <- colnames(dtf)
  
  # Transforms a vector into a vector of JSON strings that
  # can be pasted together afterwards
  toJSONvec <- function(vec) {
    #restore.point("name.value")
    na.row <- is.na(vec)
    if(is(vec,'integer')){
      ret <- vec
    } else if (is(vec,'numeric')) {
      # Round to 10 points after the decimal as before
      ret <- as.character(signif(vec,digits=10))
    } else if (is(vec,'logical')) {
      ret <- tolower(as.character(vec))
    } else if (is(vec,'Date')) {
      y <- format(vec,"%Y")
      m <- as.numeric(format(vec,"%m")) -1
      d <- as.numeric(format(vec,"%d"))
      ret <- paste("new Date(",y,",",m,",",d,")",sep="")
    } else if (is(vec,'POSIXct') | is(vec,'POSIXlt')) {
      y <- format(vec,"%Y")
      m <- as.numeric(format(vec,"%m")) -1
      d <- as.numeric(format(vec,"%d"))
      H <- as.numeric(format(vec,"%H"))
      M <- as.numeric(format(vec,"%M"))
      S <- as.numeric(format(vec,"%S"))
      ret <- paste("new Date(",y,",",m,",",d,",",H,",",M,",",S,")",sep="")
    } else {
      quote <- '"';
      vec <- gsub('"', '\\\\"', vec)
      ret <- paste(quote, vec, quote, sep='')
    }
    ret[na.row] <- "null"
    ret
  }
  # Transform columns depending on data type and store in a list
  objs <- lapply(dtf,toJSONvec)
  # Remove names just for the case that one column name was sep or collapse
  names(objs) <- NULL
  # Paste columns together
  str <- do.call(paste,c(objs,list(sep=",\n")))
  
  # Add [ ] and paste rows together
  res <- paste(
    '[\n ',
    paste("[\n",str,"\n]",collapse=',\n'),
    ' \n]',
    sep=""
  )
  return(res)
}


#rjson::toJSON(list( labels = clnms ))