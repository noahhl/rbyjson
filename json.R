# Adapated from https://github.com/jeffreyhorner/rRack/blob/master/Rook/inst/exampleApps/RJSONIO.R

library(Rook)
library(RJSONIO)
app <- function(env){
  req <- Request$new(env)
  res <- Response$new()

  obj <- sub('^/','',req$path_info())

  # Get out of here fast if no object exists
  if (!exists(obj)) return(res$finish())

  # Gather args from one of three sources: GET, POST as a x-www-urlencoded,
  # or POST as JSON payload. params() squishes GET and POST together when
  # POST is x-www-urlencoded.
  if (!is.null(req$params())) {
    args <- req$params()
  } else {
    # TODO: Collect POST payload and pass to RJSONIO
    args <- list()
  }

  # Normalize arguments to R types if necessary. Integers to integer, Numerics to numeric, etc.
  # Maybe we can propose a vector syntax in CGI that's coherent, too.
  for (i in names(args)){
    # Keep as character anything that starts with a quote char
    if (grepl('^[\'"]',args[[i]]))
        next
    # Integer
    if (grepl('^[+-]?\\d+$',args[[i]]))
        args[[i]] <- as.integer(args[[i]])

    # Numeric. need to add scientific notation
    if (grepl('^[+-]?[0-9.]+$',args[[i]]))
        args[[i]] <- as.numeric(args[[i]])
  }
    
  if (is.function(get(obj)))
    result <- do.call(obj, args)
  else
    result <- get(obj)
  
  payload <- tryCatch(toJSON(result), error=function(e) {tryCatch(toJSON(unlist(result)), error=function(e) {toJSON(as.character(result))})})
  res$write(payload)
  res$finish()
}



rook <- Rhttpd$new()
rook$add(app, "calculate")
rook$start(listen="0.0.0.0", port=as.numeric(Sys.getenv("PORT")))

while(T) {
  Sys.sleep(10000)
}

