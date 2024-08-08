#' @include core.R
.rsdmxfixer = function (tprovs=NULL) {
  if (!require(rsdmx)) stop('needs packages rsdmx installed')

  #suppressWarnings(utils::data('providertable'))
  if (is.null(tprovs)) tprovs =  providertable #.loadproviders()
  oProvs=rsdmx::getSDMXServiceProviders()
  #oProvs@providers[[2]]@builder@regUrl<- "https://data-api.ecb.europa.eu/service"
  #oProvs@providers[[2]]@builder@repoUrl<- "https://data-api.ecb.europa.eu/service"
  urlProvs=unlist(lapply(oProvs@providers,function(x) x@builder@repoUrl))
  oProvs@providers=oProvs@providers[!duplicated(urlProvs)]
  urlProvs=urlProvs[!duplicated(urlProvs)]
  idProvs=unlist(lapply(oProvs@providers,function(x) x@agencyId))
  if (sum(grepl('^ESTAT$',idProvs))>1) {
    oProvs@providers[[which(grepl('europa.eu/grow',urlProvs))]]@agencyId = 'EC_GROW'
    oProvs@providers[[which(grepl('europa.eu/empl',urlProvs))]]@agencyId = 'EC_EMPL'
    oProvs@providers[[which(grepl('europa.eu/comp',urlProvs))]]@agencyId = 'EC_COMP'
    oProvs@providers[[which(grepl('comext',urlProvs))]]@agencyId = 'EC_COMEXT'
    oProvs@providers[[which(grepl('nomisweb',urlProvs))]]@agencyId = 'ONS_NOMIS'
  }
  idProvs=unlist(lapply(oProvs@providers,function(x) x@agencyId))

  names(urlProvs) = idProvs

  for (i in tprovs[[1]]) {
    if (!match(i,idProvs,nomatch=0)) { next }
    ix=which(idProvs==i)
    if (tprovs[i,'PrimRepoUrl']!=oProvs@providers[[ix]]@builder@repoUrl) {
    oProvs@providers[[ix]]@builder@regUrl = oProvs@providers[[ix]]@builder@repoUrl = tprovs[i,'PrimRepoUrl']
    oProvs@providers[[ix]]@name = tprovs[i,'Longname']
    }

  }


  oProvs@providers[[which(idProvs=='EC_COMEXT')]]@builder@handler$datastructure = function (obj){ if (is.null(obj@resourceId))  obj@resourceId = "all";  if (is.null(obj@version))  obj@version = "latest";  req <- sprintf("https://ec.europa.eu/eurostat/api/comext/dissemination/sdmx/2.1/datastructure/ALL/%s", obj@resourceId);  req <- paste0(req, "?references=children");  return(req)}

  oProvs@providers[[which(idProvs=='EC_COMP')]]@builder@handler$datastructure = function (obj){ if (is.null(obj@resourceId))  obj@resourceId = "all";  if (is.null(obj@version))  obj@version = "latest";  req <- sprintf("%s/datastructure/COMP/%s/%s/", obj@regUrl, obj@resourceId, obj@version);  req <- paste0(req, "?references=children");  return(req)}
  oProvs@providers[[which(idProvs=='EC_GROW')]]@builder@handler$datastructure = function (obj){ if (is.null(obj@resourceId))  obj@resourceId = "all";  if (is.null(obj@version))  obj@version = "latest";  req <- sprintf("%s/datastructure/GROW/%s/%s/", obj@regUrl, obj@resourceId, obj@version);  req <- paste0(req, "?references=children");  return(req)}


  oProvs@providers[[which(idProvs=='EC_COMP')]]@builder@handler$dataflow      = function (obj) { if (is.null(obj@resourceId)) obj@resourceId = "all"; if (is.null(obj@version)) obj@version = "latest"; return(sprintf("%s/dataflow/COMP/%s/%s/", obj@regUrl, obj@resourceId, obj@version))}
  oProvs@providers[[which(idProvs=='EC_GROW')]]@builder@handler$dataflow      = function (obj) { if (is.null(obj@resourceId)) obj@resourceId = "all"; if (is.null(obj@version)) obj@version = "latest"; return(sprintf("%s/dataflow/GROW/%s/%s/", obj@regUrl, obj@resourceId, obj@version))}




  oProvs@providers[[which(idProvs=='EC_GROW')]]@builder@handler$dataflow =  function (obj)   {

    if (is.null(obj@resourceId))
      obj@resourceId = "all"

    req <- sprintf("%s/dataflow/%s/", obj@regUrl,
                   obj@resourceId)

    return(req)
  }

  oProvs@providers[[which(idProvs=='IMF')]]@builder@handler$datastructure =  function (obj) {

    if (is.null(obj@resourceId))
      obj@resourceId = "all"
    req <- sprintf("%s/DataStructure/%s", obj@regUrl,
                   obj@resourceId)
    #if (forceProviderId)
    #  req <- paste(req, obj@providerId, sep = "/")
    req <- paste0(req, "?references=children")
    #if (!is.null(accessKey)) { if (!is.null(obj@accessKey)) { if (length(grep("\\?", req)) == 0) req <- paste0(req, "?"); req <- paste(req, sprintf("%s=%s", accessKey, obj@accessKey), sep = "&")}};
    return(req)
    }




  oProvs@providers[[which(idProvs=='IMF')]]@builder@handler$dataflow = function (obj)
  {
    if (is.null(obj@resourceId))
      obj@resourceId = "all"
    req <- sprintf("%s/DataStructure/%s", obj@regUrl, obj@resourceId)
    return(req)
  }



  oProvs@providers[[which(idProvs=='IMF')]]@builder@handler$data = function (obj)
  {
    if (is.null(obj@flowRef))
      stop("Missing flowRef value")
    #message(key)
    if (is.null(obj@key))
      obj@key = "all"
    req <- sprintf("%s/CompactData/%s/%s", obj@repoUrl, obj@flowRef,
                   obj@key)

      req <- paste0(req, "/")
    addParams = FALSE
    if (!is.null(obj@start)) {
      req <- paste0(req, "?")
      addParams = TRUE
      req <- paste0(req, "startPeriod=", obj@start)
    }
    if (!is.null(obj@end)) {
      if (!addParams) {
        req <- paste0(req, "?")
      }
      else {
        req <- paste0(req, "&")
      }
      req <- paste0(req, "endPeriod=", obj@end)
    }

    return(req)
  }


<<<<<<< HEAD
  oProvs@providers[[which(idProvs=='OECD')]]@builder@handler$dataflow           = function (obj) { if (is.null(obj@resourceId)) obj@resourceId = "ALL"; req <- sprintf("%s/dataflow/%s/", obj@regUrl, gsub(',','/',obj@resourceId)); return(req) }
  oProvs@providers[[which(idProvs=='OECD')]]@builder@handler$datastructure      = function (obj) { if (is.null(obj@resourceId)) obj@resourceId = "ALL"; req <- sprintf("%s/dataflow/%s/?references=descendants&format=structure&detail=referencepartial", obj@regUrl, gsub(',','/',obj@resourceId)); return(req) }
=======
  oProvs@providers[[which(idProvs=='OECD')]]@builder@handler$dataflow      = function (obj) { if (is.null(obj@resourceId)) obj@resourceId = "ALL"; req <- sprintf("%s/dataflow/%s/", obj@regUrl, gsub(',','/',obj@resourceId)); if (!is.null(accessKey)) { if (!is.null(obj@accessKey)) { if (length(grep("\\?", req)) == 0)  req <- paste0(req, "?"); req <- paste(req, sprintf("%s=%s", accessKey, obj@accessKey), sep = "&") }}; return(req) }

>>>>>>> 64a932f90916acc6f96e76c46a8d6eeac782a130

  oProvs@providers[[which(idProvs=='ISTAT')]]@builder@handler$datastructure = function (obj)
  {

    if (is.null(obj@resourceId)) obj@resourceId = "all"
    if (is.null(obj@version)) obj@version = "latest"
    req <- sprintf("%s/rest/datastructure/IT1/%s/%s/", obj@regUrl,
                   obj@resourceId, obj@version)
    return(req)
  }



  assign('providers',oProvs,envir = rsdmx:::.rsdmx.options )


}


#.rsdmxfixer()





.rsdmxurl = function(providerId = NULL, resource = NULL,
                     resourceId = NULL, keyaschar = NULL, version = NULL,
                     start = NULL, end = NULL) {
  provider=rsdmx::findSDMXServiceProvider(providerId)
  requestHandler <- provider@builder@handler
  if ((resource %in% provider@builder@unsupportedResources) ||
      !(resource %in% names(requestHandler)))
    stop("Unsupported SDMX service resource for this provider")
  flowRef=NULL; if (resource=='data') { flowRef=resourceId; resourceId=NULL }
  requestParams <- rsdmx::SDMXRequestParams(regUrl = provider@builder@regUrl,
                                            repoUrl = provider@builder@repoUrl, accessKey = NULL,
                                            providerId = providerId, agencyId = NULL, resource = resource,
                                            resourceId = resourceId, version = version, flowRef = flowRef,
                                            key = keyaschar, start = start, end = end, compliant = provider@builder@compliant)
  requestFormatter <- provider@builder@formatter
  url <- switch(resource, dataflow = requestHandler$dataflow(requestFormatter$dataflow(requestParams)),
                datastructure = requestHandler$datastructure(requestFormatter$datastructure(requestParams)),
                data = requestHandler$data(requestFormatter$data(requestParams)))

  url
}


providertable=NULL

.onLoad = function (libname, pkgname) {
  require(rsdmx,quietly = TRUE);

  utils::data("providertable", package=pkgname, envir=parent.env(environment()))
  providertable<<-.dprovs
  tryit=try(.rsdmxfixer(.dprovs),silent=TRUE)
  if (is(tryit,'try-error')) {message('Fixing the rsdmx package did not work out, which impairs access to some SDMX sourves, notably IMF.\nTry to run library(MDstats) again')}

  assign('.mdstats_providers', .mdstats_providerscreate(providertable), envir = topenv())
}

if (!exists('.mdstats_providers')) .mdstats_providers = .mdstats_providerscreate(providertable)
#providertable=tprovs
#.mdstats_providers=.mdstats_providerscreate(providertable)
