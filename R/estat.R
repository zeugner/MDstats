#oo=download.file("https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/bop_c6_a?compressed=true",tt)




.sdmxgetDimNames = function(query) {
  vq=.fixSdmxCode(query,asvector = TRUE)


  dfmeta = rsdmx::readSDMX(providerId = vq[1], resource = "dataflow",
                           resourceId = vq[2], verbose = FALSE)
  if (any("dataflows" %in% slotNames(dfmeta)))
    rid = dfmeta@dataflows[[1]]@dsdRef
  else rid = vq[2]
  if (toupper(vq[1]) == "OECD")
    rid = vq[2]


  dfdsd = rsdmx::readSDMX(gsub("references=children", "references=none",
                               .rsdmxurl(vq[1], resource = "datastructure", resourceId = rid)),
                          verbose = FALSE)


  if (isS4(dfdsd@datastructures)) {
    dfdsdds = dfdsd@datastructures@datastructures[[1]]
  } else dfdsdds = dfdsd@datastructures[[1]]
  unlist(lapply(dfdsdds@Components@Dimensions, function(x) {
    x@conceptRef
  }))

}

.EstatLoadFull = function(code,startPeriod='', endPeriod = "", drop = TRUE,
                          labels = FALSE, as = c("md3", "array", "numeric", "data.table", "zoo", "2d", "1d", "pdata.frame", "data.frame"),
                          ccode = getOption("defaultcountrycode",NULL), verbose = TRUE) {


  iddf=gsub('/$','',code)
  if (any(grepl('/',iddf))) {
    iddf=.fixSdmxCode(iddf,asvector = TRUE)[2]
  }

  rtdir=tempdir(check=TRUE)
  ttt=paste0(file.path(rtdir,iddf),'.csv')
  ttgz=paste0(ttt,'.gz')

  if (!file.exists(ttt)) {
    myurl=paste0(.mdstats_providers$table()['ESTAT','PrimGetdata'],iddf,'/?format=SDMX-CSV&compressed=true')
    download.file(myurl,ttgz,mode='wb', quiet = !verbose)
  }

  if (verbose) message('Working with a file size of ',round(file.size(ttgz)/1e6,2), ' MB. Decompressing now...')
  aaa=try(data.table::fread(ttgz, drop=c('DATAFLOW', 'LAST UPDATE'),verbose = FALSE),silent=TRUE)
  if (any(grepl('err',class(aaa)))) {
    if (verbose) message('Fast decompression failed, trying decompressing the slow way now ...')

    if (!nchar(ttt)) stop('could not decompress ',ttgz)
    cat(readLines(gzfile(ttgz)),file = ttt,sep='\n',append = FALSE)
    aaa=data.table::fread(ttt, drop=c('DATAFLOW', 'LAST UPDATE'),verbose = FALSE)
    if (verbose) message('Decompression done, result in ',ttt,' ...')
  }


  ix2lose= tolower(colnames(aaa)) %in% c('dataflow', 'last update','freq')
  ixobs = tolower(colnames(aaa)) %in% c('conf_status','obs_flag',tolower(MD3:::.md3resnames()))
  if (any(grepl('conf_status',colnames(aaa)[ixobs], ignore.case = TRUE))) {
    aaa[CONF_STATUS=='',CONF_STATUS:=NA_character_]
  }
  aaa[['TIME_PERIOD']]=MD3:::.char2timo(aaa[['TIME_PERIOD']], frq=aaa[['freq']], guess=FALSE)
  colnames(aaa)[toupper(colnames(aaa))=='TIME_PERIOD'] ='TIME'

  ndn = toupper(.sdmxgetDimNames(paste0('ESTAT/',iddf)))
  colnames(aaa)=toupper(colnames(aaa))
  aaa=aaa[,c(c(ndn,'TIME'),setdiff(colnames(aaa),c(ndn,'TIME'))),with=FALSE]
  if (verbose) message('Loaded via stacked CSV ...')
  return(MD3:::.stackeddf2md3(aaa))


}





.mdEstat = function (code = NULL, startPeriod='', endPeriod='', drop=TRUE,
           labels = FALSE, as = c("md3", "array", "numeric", "data.table", "zoo", "2d", "1d", "pdata.frame", "data.frame"),
           ccode = getOption("defaultcountrycode", NULL), verbose = TRUE) {


  vc=MDstats:::.fixSdmxCode(code,asvector = TRUE)
  if (!nchar(vc['Filter'])) {
    return(.EstatLoadFull(code,startPeriod=startPeriod,endPeriod=endPeriod,drop=drop,labels=labels,as=as,ccode=ccode,verbose=verbose))
  }


  return(mdSdmx(code,startPeriod=startPeriod,endPeriod=endPeriod,drop=drop,labels=labels,as=as,ccode=ccode,verbose = verbose))

}
