#' Register functions to enable git provenance tracking
#'
#' Utility functions to assist working with git_prov.  Each of these
#' reassigns the original function to a new function in the provRmd
#' namespace.  The new function just adds an automatic call to git_prov()
#' when appropriate.
#'
#' To override these functions, use the double-colon notation  (e.g.
#' utils::write.csv() or readr::read_csv()) to call the function directly
#' from the original package; alternately, add an argument
#' of nogit = TRUE to suppress the git_prov() call.
#' @name git_prov_funs
#'

### functions from base:
#' @rdname git_prov_funs
#' @export
source <- function(source_fn, ..., nogit = FALSE) {
  ### prov_parent_id will change within this script to point to the sourced file.
  ### didn't seem to work with local change - so setting it globally

  ### save the current prov_parent_id value temporarily.
  prov_parent_id_temp <- prov_parent_id

  ### reset the prov_parent_id value to the sourced file
  prov_parent_id <<- source_fn

  base::source(file = source_fn, ...)

  ### reset prov_parent_id back to original value
  prov_parent_id <<- prov_parent_id_temp
  if(!nogit) git_prov(source_fn, filetype = 'sourced_script')
}

#' @rdname git_prov_funs
#' @export
read.csv <- function(file, stringsAsFactors = FALSE, nogit = FALSE, ...) {
  x <- utils::read.csv(file, ..., stringsAsFactors = stringsAsFactors)
  if(!nogit) git_prov(file, filetype = 'input')
  return(x)
}

#' @rdname git_prov_funs
#' @export
write.csv <- function(x, file, row.names = FALSE, nogit = FALSE, ...) {
  utils::write.csv(x, file = file, ..., row.names = row.names)
  if(!nogit) git_prov(file, filetype = 'output')
}

### functions from readr:
#' @rdname git_prov_funs
#' @export
read_csv <- function(file, nogit = FALSE, ...) {
  x <- readr::read_csv(file, ...)
  if(!nogit) git_prov(file, filetype = 'input')
  return(x)
}

#' @rdname git_prov_funs
#' @export
write_csv <- function(x, path, nogit = FALSE, ...) {
  readr::write_csv(x, path = path, ...)
  if(!nogit) git_prov(path, filetype = 'output')
}

### functions to read/write shapefiles:
#' @rdname git_prov_funs
#' @export
readOGR <- function(dsn, layer, stringsAsFactors = FALSE, nogit = FALSE, ...) {
  x <- rgdal::readOGR(dsn = dsn, layer = layer, ..., stringsAsFactors = stringsAsFactors)
  if(!nogit) git_prov(sprintf('%s/%s.shp', dsn, layer), filetype = 'input')
  return(x)
}

#' @rdname git_prov_funs
#' @export
writeOGR <- function(obj, dsn, layer, driver = 'ESRI Shapefile', nogit = FALSE, ...) {
  rgdal::writeOGR(obj, dsn = dsn, layer = layer, ..., driver = driver)
  if(!nogit) git_prov(sprintf('%s/%s.shp', dsn, layer), filetype = 'output')
}

#' @rdname git_prov_funs
#' @export
readShapePoly <- function(fn, nogit = FALSE, ...) {
  x <- maptools::readShapePoly(fn, ...)
  if(!nogit) git_prov(paste(fn, '.shp', sep = ''), filetype = 'input')
  return(x)
}

#' @rdname git_prov_funs
#' @export
writePolyShape <- function(x, fn, nogit = FALSE, ...) {
  maptools::writePolyShape(x, fn, ...)
  if(!nogit) git_prov(paste(fn, '.shp', sep = ''), filetype = 'output')
}

### functions to read/write rasters:
#' @rdname git_prov_funs
#' @export
raster <- function(x, nogit = FALSE, ...) {
  if(is.character(x) & !nogit) {
    y <- raster::raster(x, ...)
    git_prov(x, filetype = 'input')
    return(y)
  } else {
    raster::raster(x, ...)
  }
}

#' @rdname git_prov_funs
#' @export
brick <- function(x, nogit = FALSE, ...) {
  if(is.character(x) & !nogit) {
    y <- raster::brick(x, ...)
    git_prov(x, filetype = 'input')
    return(y)
  } else {
    raster::brick(x, ...)
  }
}

#' @rdname git_prov_funs
#' @export
stack <- function(x, nogit = FALSE, ...) {
  if(is.character(x) & !nogit) {
    y <- raster::stack(x, ...)
    git_prov(x, filetype = 'input')
    return(y)
  } else {
    raster::stack(x, ...)
  }
}

#' @rdname git_prov_funs
#' @export
writeRaster <- function(x, filename, bylayer = FALSE, nogit = FALSE, ...) {
  raster::writeRaster(x, filename, ..., bylayer = bylayer)
  if(bylayer == TRUE & !nogit) {
    message('please run git_prov() manually on individual output layers')
  } else {
    if(!nogit) git_prov(filename, filetype = 'output')
  }
}

#' @rdname git_prov_funs
#' @export
gdal_rasterize <- function(..., nogit = FALSE) {
  message("Don't forget to run git_prov() on the inputs and outputs...")
  gdalUtils::gdal_rasterize(...)
}

#' @rdname git_prov_funs
#' @export
rasterize <- function(x, y, filename = '', nogit = FALSE, ...) {
  z <- raster::rasterize(x, y, ..., filename = filename)
  if(filename != '' & !nogit) {
    git_prov(filename, filetype = 'output')
  }
  return(z)
}


