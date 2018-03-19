readSDMX2 <- function (file = NULL, isURL = TRUE, isRData = FALSE, provider = NULL,
          providerId = NULL, providerKey = NULL, agencyId = NULL, resource = NULL,
          resourceId = NULL, version = NULL, flowRef = NULL, key = NULL,
          key.mode = "R", start = NULL, end = NULL, dsd = FALSE, validate = FALSE,
          verbose = TRUE)
{
  .rsdmx.options$validate <- validate
  .rsdmx.options$followlocation <- TRUE
  if (!(key.mode %in% c("R", "SDMX"))) {
    stop("Invalid value for key.mode argument. Accepted values are 'R', 'SDMX' ")
  }
  buildRequest <- FALSE
  if (!missing(provider)) {
    if (class(provider) != "SDMXServiceProvider") {
      stop("Provider should be an instance of 'SDMXServiceProvider'")
    }
    else {
      providerId = slot(provider, "agencyId")
    }
    buildRequest <- TRUE
  }
  if (!missing(providerId)) {
    provider <- findSDMXServiceProvider(providerId)
    if (is.null(provider)) {
      stop("No provider with identifier ", providerId)
    }
    buildRequest <- TRUE
  }
  if (buildRequest) {
    if (is.null(resource))
      stop("SDMX service resource cannot be null")
    requestHandler <- provider@builder@handler
    if ((resource %in% provider@builder@unsupportedResources) ||
        !(resource %in% names(requestHandler)))
      stop("Unsupported SDMX service resource for this provider")
    if (key.mode == "R" && !missing(key) && !is.null(key)) {
      key <- paste(sapply(key, paste, collapse = "+"),
                   collapse = ".")
    }
    requestParams <- SDMXRequestParams(regUrl = provider@builder@regUrl,
                                       repoUrl = provider@builder@repoUrl, accessKey = providerKey,
                                       providerId = providerId, agencyId = agencyId, resource = resource,
                                       resourceId = resourceId, version = version, flowRef = flowRef,
                                       key = key, start = start, end = end, compliant = provider@builder@compliant)
    requestFormatter <- provider@builder@formatter
    requestParams <- switch(resource, dataflow = requestFormatter$dataflow(requestParams),
                            datastructure = requestFormatter$datastructure(requestParams),
                            data = requestFormatter$data(requestParams))
    file <- switch(resource, dataflow = requestHandler$dataflow(requestParams),
                   datastructure = requestHandler$datastructure(requestParams),
                   data = requestHandler$data(requestParams))
    if (verbose)
      message(paste0("-> Fetching '", file, "'"))
  }
  if (is.null(file))
    stop("Empty file argument")
  if (buildRequest)
    isURL = TRUE
  if (isRData)
    isURL = FALSE
  status <- 0
  if (isURL == FALSE) {
    isXML <- !isRData
    if (isXML) {
      if (!file.exists(file))
        stop("File ", file, "not found\n")
      content <- readChar(file, min(file.info(file)$size, .Machine$integer.max))
      content2 <- readChar(file, .Machine$integer.max))
    }
  }
  else {
    rsdmxAgent <- paste("rsdmx/", as.character(packageVersion("rsdmx")),
                        sep = "")
    h <- RCurl::basicHeaderGatherer()
    content <- RCurl::getURL(file, httpheader = list(`User-Agent` = rsdmxAgent),
                             ssl.verifypeer = FALSE, .encoding = "UTF-8", headerfunction = h$update)
    if (as.numeric(h$value()["status"]) >= 400) {
      stop("HTTP request failed with status: ", h$value()["status"],
           " ", h$value()["statusMessage"])
    }
  }
  status <- tryCatch({
    if ((attr(regexpr("<!DOCTYPE html>", content), "match.length") ==
         -1) && (attr(regexpr("<html>", content), "match.length") ==
                 -1)) {
      BOM <- "ÿ"
      if (attr(regexpr(BOM, content), "match.length") !=
          -1) {
        content <- gsub(BOM, "", content)
      }
      content <- gsub("<!--.*?-->", "", content)
      content <- gsub("&ldquo;", "&quot;", content)
      content <- gsub("&rdquo;", "&quot;", content)
      content <- gsub("&lsquo;", "'", content)
      content <- gsub("&rsquo;", "'", content)
      xmlObj <- xmlTreeParse(content, useInternalNodes = TRUE)
      status <- 1
    }
    else {
      stop("Invalid SDMX-ML file")
    }
  }, error = function(err) {
    print(err)
    status <<- 0
    return(status)
  })
  getSDMXStructureObject <- function(xmlObj, ns, resource) {
    strTypeObj <- SDMXStructureType(xmlObj, ns, resource)
    strType <- getStructureType(strTypeObj)
    strObj <- switch(strType, DataflowsType = SDMXDataFlows(xmlObj,
                                                            ns), ConceptsType = SDMXConcepts(xmlObj, ns), CodelistsType = SDMXCodelists(xmlObj,
                                                                                                                                        ns), DataStructuresType = SDMXDataStructures(xmlObj,
                                                                                                                                                                                     ns), DataStructureDefinitionsType = SDMXDataStructureDefinition(xmlObj,
                                                                                                                                                                                                                                                     ns), NULL)
    return(strObj)
  }
  obj <- NULL
  if (status) {
    ns <- namespaces.SDMX(xmlObj)
    if (isSoapRequestEnvelope(xmlObj, ns)) {
      xmlObj <- getSoapRequestResult(xmlObj)
    }
    if (isRegistryInterfaceEnvelope(xmlObj, TRUE)) {
      xmlObj <- getRegistryInterfaceResult(xmlObj)
    }
    type <- SDMXType(xmlObj)@type
    obj <- switch(type, StructureType = getSDMXStructureObject(xmlObj,
                                                               ns, resource), GenericDataType = SDMXGenericData(xmlObj,
                                                                                                                ns), CompactDataType = SDMXCompactData(xmlObj, ns),
                  UtilityDataType = SDMXUtilityData(xmlObj, ns), StructureSpecificDataType = SDMXStructureSpecificData(xmlObj,
                                                                                                                       ns), StructureSpecificTimeSeriesDataType = SDMXStructureSpecificTimeSeriesData(xmlObj,
                                                                                                                                                                                                      ns), CrossSectionalDataType = SDMXCrossSectionalData(xmlObj,
                                                                                                                                                                                                                                                           ns), MessageGroupType = SDMXMessageGroup(xmlObj,
                                                                                                                                                                                                                                                                                                    ns), NULL)
    if (is.null(obj)) {
      if (type == "StructureType") {
        strTypeObj <- SDMXStructureType(xmlObj, ns, resource)
        type <- getStructureType(strTypeObj)
      }
      stop(paste("Unsupported SDMX Type '", type, "'",
                 sep = ""))
    }
    else {
      footer <- slot(obj, "footer")
      footer.msg <- slot(footer, "messages")
      if (length(footer.msg) > 0) {
        invisible(lapply(footer.msg, function(x) {
          code <- slot(x, "code")
          severity <- slot(x, "severity")
          lapply(slot(x, "messages"), function(msg) {
            warning(paste(severity, " (Code ", code,
                          "): ", msg, sep = ""), call. = FALSE)
          })
        }))
      }
    }
  }
  else {
    if (isRData) {
      if (!file.exists(file))
        stop("File ", file, "not found\n")
      obj <- readRDS(file, refhook = XML::xmlDeserializeHook)
    }
  }
  embeddedDSD <- FALSE
  if (is(obj, "SDMXData")) {
    strTypeObj <- SDMXStructureType(obj@xmlObj, ns, NULL)
    if (!is.null(strTypeObj@subtype)) {
      if (strTypeObj@subtype %in% c("CodelistsType", "DataStructureDefinitionsType")) {
        dsd <- TRUE
        embeddedDSD <- TRUE
      }
    }
  }
  if (dsd) {
    dsdObj <- NULL
    if (embeddedDSD) {
      dsdObj <- SDMXDataStructureDefinition(obj@xmlObj,
                                            ns)
      slot(obj, "dsd") <- dsdObj
    }
    if (buildRequest && resource %in% c("data", "dataflow")) {
      if (resource == "data" && providerId %in% c("ESTAT",
                                                  "ISTAT", "WBG_WITS", "UIS2")) {
        if (verbose)
          message("-> Attempt to fetch DSD ref from dataflow description")
        flow <- readSDMX(providerId = providerId, providerKey = providerKey,
                         resource = "dataflow", resourceId = flowRef,
                         verbose = TRUE)
        dsdRef <- slot(slot(flow, "dataflows")[[1]],
                       "dsdRef")
        rm(flow)
      }
      else {
        dsdRef <- NULL
        if (resource == "data") {
          dsdRef <- slot(obj, "dsdRef")
        }
        else if (resource == "dataflow") {
          dsdRef <- lapply(slot(obj, "dataflows"), slot,
                           "dsdRef")
        }
        if (!is.null(dsdRef)) {
          if (verbose)
            message(paste0("-> DSD ref identified in dataset = '",
                           dsdRef, "'"))
          if (verbose)
            message("-> Attempt to fetch & bind DSD to dataset")
        }
        else {
          dsdRef <- flowRef
          if (verbose)
            message("-> No DSD ref associated to dataset")
          if (verbose)
            message("-> Attempt to fetch & bind DSD to dataset using 'flowRef'")
        }
      }
      if (resource == "data") {
        dsdObj <- readSDMX(providerId = providerId, providerKey = providerKey,
                           resource = "datastructure", resourceId = dsdRef,
                           verbose = verbose)
        if (is.null(dsdObj)) {
          if (verbose)
            message(sprintf("-> Impossible to fetch DSD for dataset %s",
                            flowRef))
        }
        else {
          if (verbose)
            message("-> DSD fetched and associated to dataset!")
          slot(obj, "dsd") <- dsdObj
        }
      }
      else if (resource == "dataflow") {
        dsdObj <- lapply(1:length(dsdRef), function(x) {
          flowDsd <- readSDMX(providerId = providerId,
                              providerKey = providerKey, resource = "datastructure",
                              resourceId = dsdRef[[x]], verbose = verbose)
          if (is.null(flowDsd)) {
            if (verbose)
              message(sprintf("-> Impossible to fetch DSD for dataflow %s",
                              resourceId))
          }
          else {
            if (verbose)
              message("-> DSD fetched and associated to dataflow!")
            slot(slot(obj, "dataflows")[[x]], "dsd") <<- flowDsd
          }
        })
      }
    }
  }
  return(obj)
}
