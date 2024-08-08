
#library(data.table);library(XML);library(rsdmx); library(MD3)
#  .loadproviders=function() {
#    providertable=utils::read.csv('data/providers.csv',stringsAsFactors = FALSE,header = TRUE, na.strings='')
#    rownames(providertable) = providertable[[1]]
#
#    colnames(providertable)[[1]]='AgencyID'; rownames(providertable)=providertable[[1]]
#    return(providertable)
# }
.mdstats_providerscreate=function(.dprovs=NULL){
  #.dprovs =  data.loadproviders()
  #if (!length(.dprovs))  {utils::data('providertable'); .dprovs=providertable}  #.loadproviders()
  if (!length(.dprovs))  {.dprovs=providertable}  #.loadproviders()
  #colnames(.dprovs)[[1]]='AgencyID'; rownames(.dprovs)=.dprovs[[1]]


  cachedir=""
  p2=rsdmx:::.rsdmx.options$providers
  sproviders=sapply(p2@providers, function(x) x@agencyId)


  surls=sapply(as.list(sproviders),function(x) rsdmx::findSDMXServiceProvider(x)@builder@regUrl)
  surls=paste0(surls,'/data/')

  names(surls)=sproviders
  sproviders = sproviders[!duplicated(sproviders)]

  #surls['OECD'] = gsub('/data/','/GetData/',surls['OECD'])
  ssuffix=character(length(surls)); names(ssuffix)=names(surls)


  #BBDY1) with the specified key (e.g. A.B10.N.G100.P0010.A).
  surls['IMF'] = 'http://dataservices.imf.org/REST/SDMX_XML.svc/CompactData/'
  surls['ECB'] = 'https://data-api.ecb.europa.eu/service/data/'
  surls['ESTAT'] = 'https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/'
  surls['ISTAT'] = 'https://esploradati.istat.it/SDMXWS/data/'
  surls['BBK'] = 'https://api.statistiken.bundesbank.de/rest/data/'
  surls['NBB']=gsub('/data/','/GetData/',surls['NBB'])
  ssuffix['NBB']='/all?detail=serieskeysonly&format=compact_v2'
  ssuffix['WB']='/?startperiod=1960&endPeriod=2100'

  if (!exists('.dprovs')) {.dprovs=providertable} #else {warning('could not find providertable of class ',class(.dprovs)) }
  .buildaliases=function() {
    if (!length(.dprovs)) .dprovs <<- providertable
    anames0=strsplit(.dprovs[,'AltNicks'],split=',')
    names(anames0) = .dprovs[,'AgencyID']
    anames=unlist(lapply(as.list(names(anames0)),function(x) rep(x,length(anames0[[x]]))))
    names(anames) = unlist(anames0)
    anames=unlist(anames,use.names = TRUE); anames=anames[!is.na(names(anames))]
    anames1=.dprovs[,'AgencyID']; names(anames1)=.dprovs[,'AgencyID']
    anames=c(anames1,anames); names(anames) = toupper(names(anames))
    anames
  }

  valiases=character()
  cacheddatas=list()
  ldf= as.list(.dprovs$AgencyID); names(ldf)=unlist(ldf); ldf=lapply(ldf,function(x) data.table())
  #ldsds=lapply(ldf,function(x) list())
  lcl=as.list(.dprovs$AgencyID); names(lcl)=unlist(lcl); lcl=lapply(lcl,function(x) list())
  ldn=as.list(.dprovs$AgencyID); names(ldn)=unlist(ldn); ldn=lapply(ldn,function(x) list())
  #ldsdids=as.list(.dprovs$AgencyID); names(ldsdids)=unlist(ldsdids); ldsdids=lapply(ldsdids,function(x) charc())

  outlist=list()
  outlist$sources = data.frame(prov=sproviders,dataurl=surls[sproviders],datasuffix=ssuffix[sproviders],stringsAsFactors = FALSE, row.names = sproviders)
  outlist$sayah = function() message("ah")
  #outlist$overview=.dprovs
  outlist$table = function() {
    if (!length(.dprovs)) .dprovs <<- providertable
    return(.dprovs)
  }
  outlist$alias = function(x) {if (!length(valiases)) {valiases<<- .buildaliases()}; gout=unname(valiases[toupper(gsub('/.*$','',x))]); if (!anyNA(gout)) {return(gout)}; warning('Provider ',gsub('/.*$','',x), ' not available from mdStats'); return(x)}
  outlist$dataflows = function(sprovider,verbose=TRUE) {
    sprovider=valiases[toupper(sprovider)]
    if (!NROW(ldf[[sprovider]])) {
      message('loading dataflows from ',sprovider, ', this might take some time')
      ldf[[sprovider]] <<- .fetchDataflows(sprovider,verbose=verbose)
      if (sprovider=='OECD') { ldf[[sprovider]][[1]]=paste0(ldf[[sprovider]][['agencyid']],',',ldf[[sprovider]][['id']])} #to be reviewed !?!
    }
    ldf[[sprovider]]
  }
  outlist$dfdims = function(sprovider,mdStataflow,verbose=FALSE) {
    if (missing(sprovider)) { return(ldn)}
    sprovider=valiases[toupper(sprovider)]
    if (missing(mdStataflow)) { return(ldn[[sprovider]])}

    if (!NROW(ldn[[sprovider]][[mdStataflow]])) {
      if (verbose) message('loading dimension metadata from ',sprovider, ', this might take some time')
      ldn[[sprovider]][[mdStataflow]] <<- .fetchdnwcodelist(paste0(sprovider,'/',mdStataflow),verbose = verbose)
    }
    ldn[[sprovider]][[mdStataflow]]
  }
  # outlist$dsddc = function(sprov,dsdid) {
  #   sprov=valiases[toupper(sprov)]
  #   if (!length(ldsds[[sprov]][[dsdid]])) {
  #     message('loading metadata for ', dsdid, ' from ',sprov, ', this might take some time')
  #     ldsds[[sprov]][[dsdid]] <<- .extractDimcodes(rsdmx::readSDMX(providerId = sprov,resource = 'datastructure',resourceId = dsdid))
  #   }
  #   ldsds[[sprov]][[dsdid]]
  # }
  outlist$codelists = function(prov,clid=NULL) {
    #if (!length(clid)) { clid=outlist$alias(prov }
    sprov=valiases[toupper(prov)]

    if (!length(ldsds[[prov]][[clid]])) {
      message('loading metadata for ', clid, ' from ',prov, ', this might take some time')
      #ldsds[[sprov]][[dsdid]] <<- .extractDimcodes(rsdmx::readSDMX(providerId = sprov,resource = 'datastructure',resourceId = dsdid))
    }
    #ldsds[[sprov]][[clid]]
  }
  outlist$allcodelists = function(newlcl=NULL) {
   if (!is.null(newlcl)) { lcl<<-newlcl}
   lcl
  }
  outlist$cachepath = function(x="") {
    x=trimws(x)
    if (nchar(x)) {
      x=normalizePath(x)
      if (file.access(x)<0) {stop('Path ',x,' is not writable.') }
      cachedir <<- x
    }
    if (!nchar(cachedir)) { cachedir <<- .cachelocation()}
    return(cachedir)
  }
  outlist$cachedmd3s = function(x,id='') {
    if (missing(x)) return(cacheddatas)
    if (!is.null(x) & is.character(x)) {
      if (nchar(x)>0) {return(cacheddatas[[x]])} else return(cacheddatas)
    }

    if (MD3:::.md3_is(x)) { cacheddatas[[id]]<<-x} else if (is.list(x)) { cacheddatas[names(x)]<<-x}
    if (nchar(id)>0) {return(invisible(cacheddatas[[id]]))}
    return(invisible(cacheddatas))
  }
  return(outlist)
}


.stackedsdmx =function(mycode,justurl=FALSE,justxml=FALSE,verbose=FALSE,startPeriod='',endPeriod='') {

  mycode=trimws(mycode)
  if (substr(mycode,0,4)=='http') {
    myurl=mycode
  } else {
    myprov=gsub('/.*$','',mycode)
    myurl=paste0(.mdstats_providers$sources[myprov,'dataurl'],gsub("^[^/]*/","",mycode))
    myurl=paste0(myurl,.mdstats_providers$sources[myprov,'datasuffix'])
    if (nchar(startPeriod)) myurl=paste0(myurl,ifelse(any(grepl('\\?',myurl)),'&','?'),'startPeriod=',startPeriod)
    if (nchar(endPeriod)) myurl=paste0(myurl,ifelse(any(grepl('\\?',myurl)),'&','?'),'endPeriod=',endPeriod)
  }

  if (justurl) return(myurl)
  if (verbose) cat('\nreading from ',myurl,' ...\n')
  ressdmx <- try(rsdmx::readSDMX(myurl,verbose = verbose),silent=TRUE)
  if (is(ressdmx,'try-error')) stop('Could not fetch data for query code ',mycode,'.\nTry running helpmds("',mycode,") to find out why.\n")
  if (!length(ressdmx)) { stop('The web request to ',myurl, ' returned NULL, perhaps for latency resons. Please try again.' ) }
  if (justxml) return(ressdmx)
  if (class(ressdmx)=='SDMXCompactData') {
    res=rsdmx:::as.data.frame.SDMXCompactData(ressdmx)
  } else if (class(ressdmx)=='SDMXGenericData') {
    res=rsdmx:::as.data.frame.SDMXGenericData(ressdmx)
  } else {
    res=as.data.frame(ressdmx)
  }
  data.table:::as.data.table(res)


}

.xml2md3 = function(ressdmx,nbdimsexcltime=0,dims=character()) {

  if (!length(dims) && is.character(nbdimsexcltime)) {
    nbdimsexcltime=nbdimsexcltime[[1L]]
    nbdimsexcltime=length(strsplit(gsub('^.*/','',paste0(nbdimsexcltime,' ')),split = '\\.')[[1L]])
  }
  if (!length(dims)) { dims = seq_len(nbdimsexcltime)}
  if (class(ressdmx)[1L] %in% c('SDMXCompactData','SDMXStructureSpecificData')  ) {
    dcodes2=xmlApply(xmlChildren(slot(ressdmx,'xmlObj'))[[1]][["DataSet"]],xmlAttrs)
    if (!length(dcodes2)) {stop('SDMX result contains 0 time series')}
    if (nbdimsexcltime<1) stop('cant find proper dimension codes')
    dcodes2=dcodes2[names(dcodes2)=='Series']
    dcodes2=lapply(dcodes2,function(x) x[dims])
  } else {
    dcodes0=xmlApply(xmlChildren(slot(ressdmx,'xmlObj'))[[1]][["DataSet"]],"[[","SeriesKey")
    dcodes0=dcodes0[as.logical(unlist(lapply(dcodes0,length)))]
    if (!length(dcodes0)) stop('the query returned a result that is empty')
    dcodes1=lapply(dcodes0,function(x) xmlApply(x,xmlAttrs))
    dcodes2=lapply(dcodes1,function(x) {ds=as.data.frame(x); ovec=as.character(ds[2,]);names(ovec)=ds[1,]; ovec})
  }
  if (!all(unlist(lapply(lapply(dcodes2,names),identical,names(dcodes2[[1]]))))) stop('dimension names are not the same across series')
  #ldn=lapply(as.list(names(dcodes2[[1]])),function(x) character(0)); names(ldn)=names(dcodes2[[1]])
  dn=names(dcodes2[[1]])
  if (class(ressdmx)=='SDMXCompactData') {
    res=rsdmx:::as.data.frame.SDMXCompactData(ressdmx)
  } else if (class(ressdmx)=='SDMXGenericData') {
    res=rsdmx:::as.data.frame.SDMXGenericData(ressdmx)
  } else {
    res=as.data.frame(ressdmx)
  }


  dmd3=data.table:::as.data.table(res)
  tname = grep('time_period',tolower(colnames(dmd3)))
  if(!length(tname)) tname = setdiff(grep('time',tolower(colnames(dmd3))),grep('format',tolower(colnames(dmd3))))
  if(!length(tname)) tname = grep('time',tolower(colnames(dmd3)))
  if (length(tname)>1L) { ix=grepl('period',tolower(colnames(dmd3))[tname]); if (any(ix)) tname=tname[ix]}
  if (length(tname)>1L) { ix=grepl('obs',tolower(colnames(dmd3))[tname]); if (any(ix)) tname=tname[ix]}
  if (length(tname)>1L) { ix=grepl('^time',tolower(colnames(dmd3))[tname]); if (any(ix)) tname=tname[ix]}
  if (length(tname)>1L) { tname=tname[which.min(nchar(colnames(dmd3)[tname]))]}
  if (length(tname)) {
    colnames(dmd3)[tname]<-'TIME';

  }


  dnames=c(setdiff(match(colnames(dmd3), dn, nomatch=0),c(0,tname)),tname)
  onames = setdiff(grep('^obs',colnames(dmd3),ignore.case = TRUE),dnames)
  anames=setdiff(seq_along(dmd3),c(dnames,onames))
  dnames=colnames(dmd3)[dnames]; onames=colnames(dmd3)[onames]; anames=colnames(dmd3)[anames];

  dmd3=dmd3[,c(dnames,onames),with=FALSE]

  if (length(tname)) {
    fname=dnames[tolower(dnames) %in% c('freq','frq','frequency')][1]
    dmd3[['TIME']] = MD3::as.timo(dmd3[['TIME']],frq = dmd3[[fname]])
  }

  on2=unlist(lapply(as.list(onames),MD3:::.md3resnames))
  on2[grepl('value',on2)]='_.obs_value'
  colnames(dmd3)[colnames(dmd3) %in% onames]=on2; onames=on2
  nov=MD3:::.md3resnames('value')

  if (!is.double(dmd3[[nov]])) {dmd3[[nov]] = suppressWarnings(as.numeric(dmd3[[nov]]))}

  mmd3=MD3:::.stackeddf2md3(dmd3,isdf = FALSE)
  mmd3
}

.sdmxasmd3 = function(mycode,drop=TRUE,metadata=FALSE,verbose=FALSE, ccode=NULL, startPeriod='', endPeriod='') {
  mycode=.fixSdmxCode(mycode,asvector = FALSE)
  #dfmeta=rsdmx::readSDMX(providerId=.fixSdmxCode(mycode)[1],resource='dataflow',resourceId = .fixSdmxCode(mycode)[2],verbose=verbose)
  #if ('dataflows' %in% slotNames(dfmeta)) mydsdref=dfmeta@dataflows[[1]]@dsdRef else mydsdref= dfmeta@datastructures@datastructures[[1]]@id
  if (metadata) {
   #possdn=.mdstats_providers$dsddc(.fixSdmxCode(mycode)[1],dfmeta@dataflows[[1]]@dsdRef)
    #possdn=.fetchfullcodelists(mycode,verbose=verbose,sdmxdataflow=dfmeta)
    possdn=.fetchfullcodelists(mycode,verbose=verbose)
  } else {


    #dfdsd=rsdmx::readSDMX(gsub('references=children','references=none',.rsdmxurl(.fixSdmxCode(mycode)[1],resource='datastructure',resourceId = mydsdref)))
    #if (isS4(dfdsd@datastructures)) { dfdsd=dfdsd@datastructures}
    #possdn=unlist(lapply(dfdsd@datastructures[[1]]@Components@Dimensions, function(x) {yy=x@codelist; names(yy)=x@conceptRef; yy}))

    possdn = .mdstats_providers$dfdims(.fixSdmxCode(mycode)[[1]],.fixSdmxCode(mycode)[[2]],verbose)
  }

  mxml=.stackedsdmx(mycode,justxml=TRUE,verbose=verbose,startPeriod = startPeriod,endPeriod = endPeriod)
  mout=.xml2md3(mxml,mycode,names(possdn))
  if (metadata) {


    actdn=MD3:::.getdimnames(mout)
    nxt=setdiff(names(actdn),'TIME')
    if (length(nxt)) {
    mydc=lapply(as.list(names(actdn[nxt])), function(i) as.data.frame(as.data.frame(possdn[[i]],stringsAsFactors=FALSE)[actdn[[i]],,drop=FALSE],stringsAsFActors=FALSE))
    names(mydc)=names(actdn[nxt])
     if (anyNA(unlist(lapply(mydc,function(x) x[,1])))) {
       #in case the codelist was incomplete
       if (exists('actdn',parent.frame())) { warning('it was not possible to provide complete labels')
        for (j in names(mydc)) { mydc[[j]][,1]=actdn[[j]][,1]}
       } else {
        trhowaway=.fetchfullcodelists(mycode,verbose=verbose,forcerefresh = TRUE)
        return(.sdmxasmd3(mycode=mycode,drop=drop,metadata=metadata,verbose=verbose, ccode=ccode, startPeriod=startPeriod, endPeriod=endPeriod))
       }
     }

    if (any(unlist(lapply(mydc,is.null)))) mydc[unlist(lapply(mydc,is.null))]=actdn[unlist(lapply(mydc,is.null))]
    #browser()
    ixempt=!unlist(lapply(mydc,length))
    if (any(ixempt)) mydc[ixempt]=actdn[nxt][ixempt]
    if (length(mydc)==length(actdn)-1) {mydc[['TIME']]=actdn[['TIME']]}
    mout=MD3:::.setdimcodes(mout,mydc)
    }
  }
  if (drop) mout=MD3:::drop.md3(mout)
  if (!length(ccode)) return(mout)
  ixgeodim=intersect(names(attr(mout,'dcstruct')),.findgeodim(possdn,.fixSdmxCode(mycode)[[1]]))
  if (length(ixgeodim)) mout=.countrycodefixer(mout,tocode = ccode,whichdim=ixgeodim)
  mout
}



#xx=.stackedsdmx("ECB/EXR/A.JPY+PLN.EUR.SP00.A",justxml = TRUE); ii=.sdmxasmd3("ECB/EXR/A.JPY+PLN.EUR.SP00.A")
#ii=.sdmxasmd3("OECD/TIVA_2021_C1/FFD_DVA.WLD+AUT+BEL.WLD+AUT+BEL.DTOTAL");  ii=.sdmxasmd3("OECD/EA/AUT.GDP.Q")
#ii=.sdmxasmd3('BBK/BBFBOPV/M.N.DE.W1.S1.S1.T.B.G+S+CA._Z._Z._Z.EUR._T._X.N.ALL')
#ii=.sdmxasmd3("BIS/WS_EER_M/M.N.B.AT+FR+BE")
#ii=.sdmxasmd3('ESTAT/prc_hpi_q/Q..I10_Q.AT+BE')
#ii=.sdmxasmd3('IMF/FSI/A.FI+DK+AT.FSASDLD_EUR+FSANL_PT')







.fetchDataflows = function(sprovider,verbose=TRUE) {
  if (is.character(sprovider)) {
    if (verbose) cat('\nReading from ',.mdstats_providers$table()[sprovider,'PrimDataFlows'],'\n')
    oflows=rsdmx::readSDMX(.mdstats_providers$table()[sprovider,'PrimDataFlows'])
  } else {
    oflows = sprovider
  }

  extractinfo = function(sdf) {
    out=c(id=sdf@id, agencyid=sdf@agencyID, dsdref=slot(sdf,ifelse(anyNA(match('dsdRef',slotNames(sdf))),'id','dsdRef')))
    tname=unlist(sdf@Name)
    names(tname)=paste0("name:",names(tname))
    data.table::as.data.table(as.list(c(out,tname)))
  }
  if ("dataflows" %in% slotNames(oflows)) {oflows=oflows@dataflows}
  if ("datastructures" %in% slotNames(oflows)) {oflows=oflows@datastructures}
  rbindlist(lapply(oflows,extractinfo),fill = TRUE, use.names = TRUE)
}


.extractDimcodes = function(prov,dsd=NULL, dnonly=FALSE,dimclonly=FALSE){
  if (!isS4(prov)) {
    stop('this is not foreseen')
    #prov=.mdstats_providers$alias(prov)
    #oo=.mdstats_providers$dsd(prov,dsd)
    return(oo)
  } else {
    oo=prov
    prov=oo@header@Sender$id
  }
  ods=oo@datastructures
  if (match('datastructures',slotNames(ods),nomatch=0)) { ods=ods@datastructures}
  ocl=oo@codelists
  if (match('codelists',slotNames(ocl),nomatch=0)) { ocl=ocl@codelists}


  o1=unlist(lapply(slot(slot(ods[[1]],'Components'),'Dimensions'),function(x) paste0(ifelse(anyNA(x@conceptAgency),ods[[1]]@agencyID,.mdstats_providers$alias(x@conceptAgency)),'/',x@codelist)))
  names(o1)=unlist(lapply(slot(slot(ods[[1]],'Components'),'Dimensions'),slot,'conceptRef'))
  if (dimclonly) {
    return(o1)
  }
  o2=lapply(oo@codelists@codelists, function(z) unlist(lapply(z@Code,function(x) x@id)))
  names(o2)=unlist(lapply(oo@codelists@codelists,function(z) z@id))
  if (dnonly) {
    mydn=o2[gsub('^.*/','',o1)]
    names(mydn)=names(o1)
    return(mydn)
  }
  o0=oo@codelists@codelists

  o0=o0[unlist(lapply(oo@codelists@codelists,function(x) length(x@Code)))>0]
  olabels =lapply(o0,function(x) unlist(x@Name))
  olabels =lapply(olabels,function(x) trimws(gsub('code {0,1}list', '',x)))
  for (j in seq_along(olabels)) {
    names(olabels[[j]]) = paste0("label:",names(o0[[j]]@Name))
  }


  o3=lapply(o0, function(z) {tump=unlist(lapply(z@Code,function(y) {templabel=unlist(y@label); names(templabel)=paste0('label:',names(templabel)); return(c(code=y@id,templabel))} ));tomp=matrix(tump,ncol=1+length(z@Code[[1]]@label),byrow=TRUE); colnames(tomp)=c('code',paste0('label:',names(z@Code[[1]]@label)));rownames(tomp)=tomp[,1]; return(tomp)})
  names(olabels)=names(o3)= lapply(o0, function(z) z@id)
  o3=o3[gsub('^.*/','',o1)]
  olabels=olabels[names(o3)]
  o3=lapply(o3,function(x) {if (!any('label:en'==colnames(x))) return(x); if (which('label:en'==colnames(x))==2) { return(x) }; return(x[,c('code','label:en',setdiff(colnames(x),c('code','label:en')))])})
  names(olabels) = names(o3) = names(o1)
  for (i in names(o3)) {
    attr(o3[[i]],'codelist') = unname(o1[i])
    attr(o3[[i]],'label')  = olabels[[i]]
  }

  o3


}



.fixSdmxCode =function(mycode,asvector=TRUE){
  if (!any(grepl('/|\\.',mycode))) { mycode=paste0(mycode,'/')}
  if (grepl('\\.',mycode)) {
    if (nchar(gsub('[^/]','',mycode))!=2) {
       warning('The query ',mycode, ' contains dots but not two slashes in the right place')
    }
  }
  sprov=gsub('/.*$','',mycode)

  sflow=gsub('/.*$','',gsub('^[^/]*/','',substr(mycode,1+nchar(sprov),1000)))
  sprov=.mdstats_providers$alias(sprov)
  sfilter=gsub('/$','',gsub('^.*/','',mycode))
  if (sfilter %in% c(sprov,sflow)) {sfilter=''  }

  if (asvector) return(c(Provider=sprov,Dataflow=sflow,Filter=sfilter))
  return(paste0(sprov,'/',sflow,'/',sfilter))
}



#  ccode placeholder fo future useIf not \code{ccode==NULL}, then the function attempts to convert to ccode destination such as 'iso2c', 'EC', 'iso3c'. See \code{\link[MDcountrycode]{ccode}} for permissible values.  \code{\link[MDcountrycode]{defaultcountrycode}} for defining this value as a session-wide option.
# startPeriod character string (e.g. "2011") or date denoting at what time period the result should start
# endPeriod character string (e.g. "2018") or date denoting at what time period the result should end



#' Get data from an SDMX source
#'
#' @param code a character query string with a RestFul SDMX query (such as ECB/EXR/A.GBP+CHF.EUR.SP00.A). See \code{\link{mds}} for an example
#' @param startPeriod optional, character, integer, timo or date-like class. default empty string means to fetch data including  the first available period
#' @param endPeriod placeholder fo optional, character, integer, timo or date-like class. default empty string means to fetch data including  the last available observation
#' @param drop if TRUE, drop any singleton dimensions (see also drop.md3)
#' @param labels add the descriptions for each dimension element, e.g. "Belgium" for "BEL". You can check those with function \code{\link[MD3]{dimcodes}}
#' @param as how to output the result: \code{md3}: as md3 object with full metadata, \code{2d}: as a data.table with periods as column names, \code{1d} resp. \code{data.table}: as fully stacked data.table, \code{array}: as multi-dim array, \code{zoo}: as \code{\link[zoo]{zooreg}} time series object.
#' @param ccode ccode If not \code{ccode==NULL}, then the function attempts to convert to ccode destination such as 'iso2c', 'EC', 'iso3c'. See \code{\link[MDcountrycode]{ccode}} for permissible values.  \code{\link[MDcountrycode]{defaultcountrycode}} for defining this value as a session-wide option.
#' @param verbose if TRUE, this function chatters about what it is doing, notably contacting servers
#' @return depending on \code{as}, an \code{md3}, \code{array}, \code{numeric}, \code{data.table}, \code{zoo}, \code{2d} dat.table, \code{1d} stacked data.table, or data.frame    containing the requested data
#' @details \code{mdSdmx} is a function for loading from SDMX data sources. \code{\link{mds}} encompasses that one, and will be broadened to other data sources at a later stage.
#'
#'
#' @seealso \code{\link{helpmds}}, \code{\link{DTstat}}, \code{\link[MD3]{Nomics}}, \code{\link[MDcountrycode]{defaultcountrycode}}, \code{\link{mdAmeco}}, \code{\link{mdWEO}}
#' @examples
#'
#' foo=mdSdmx("ESTAT/prc_hpi_q/Q..I15_Q.")
#' foo[AT:BG.TOTAL.y2020:y]
#'
#' mdSdmx("BBK/BBFBOPV/M.N.DE.W1.S1.S1.T.B.G+S+CA._Z._Z._Z.EUR._T._X.N.ALL")
#' mdSdmx('IMF/FSI/A.FI+DK+AT.FSASDLD_EUR+FSANL_PT')
#'
#' mdSdmx("BIS/WS_EER_M/M.N.B.AT+FR+BE",startPeriod=2021)
#' #is the same as
#' mds("BIS/WS_EER_M/M.N.B.AT+FR+BE",startPeriod=2021)
#'
#' #but mds can also do providers that helpmds() marks with an asterisk, such as AMECO
#' @export
mdSdmx = function(code, startPeriod='',endPeriod='', drop=TRUE, labels=FALSE,
                  as = c("md3", "array", "numeric", "data.table", "zoo", "2d", "1d", "pdata.frame",'data.frame'),
                  ccode=getOption('defaultcountrycode','EC'), verbose=FALSE) {
  if (missing(code)) return(helpmds())
  if (match(.fixSdmxCode(code,asvector = TRUE)[1],.mdstats_providers$table()[[1]],nomatch=0)) {
    mout=.sdmxasmd3(code,drop=drop,metadata=labels,verbose=verbose,ccode=ccode)
  } else {
    stop('Provider not available')
  }
  MD3:::.getas(mout,as[[1L]])

}


#' Get data from an SDMX or another statistical source
#'
#' @param code a character query string with a RestFul SDMX query (such as ECB/EXR/A.GBP+CHF.EUR.SP00.A). See Details
#' @param startPeriod optional, character, integer, timo or date-like class. default empty string means to fetch data including  the first available period
#' @param endPeriod placeholder fo optional, character, integer, timo or date-like class. default empty string means to fetch data including  the last available observation
#' @param drop if TRUE, drop any singleton dimensions (see also drop.md3)
#' @param labels add the descriptions for each dimension element, e.g. "Austria" for "AUT". You can check those with function \code{\link[MD3]{dimcodes}}
#' @param as how to output the result: \code{md3}: as md3 object with full metadata, \code{2d}: as a data.table with periods as column names, \code{1d} resp. \code{data.table}: as fully stacked data.table, \code{array}: as multi-dim array, \code{zoo}: as \code{\link[zoo]{zooreg}} time series object.
#' @param ccode ccode If not \code{ccode==NULL}, then the function attempts to convert to ccode destination such as 'iso2c', 'EC', 'iso3c'. See \code{\link[MDcountrycode]{ccode}} for permissible values.  \code{\link[MDcountrycode]{defaultcountrycode}} for defining this value as a session-wide option.
#' @param verbose if TRUE, this function chatters about what it is doing, notably contacting servers
#' @return depending on \code{as}, an \code{md3}, \code{array}, \code{numeric}, \code{data.table}, \code{zoo}, \code{2d} dat.table, \code{1d} stacked data.table, or data.frame    containing the requested data
#' @details This function tweaks the package \code{rsdmx}  to load data.
#' @section SDMX query:
#' A restful SDMX query combines Provider, Dataflow and Dimension selectors. Take the example 'ECB/EXR/A.GBP+CHF.EUR.SP00.A'
#' \itemize{
#'   \item 'ECB' designates the provider ECB (see helpmds() for a list of providers)
#'   \item 'EXR' designates the ECB's dataflow 'EXR' about exchange rates (see helpmds('ECB') for a list of ECB dataflows)
#'   \item A.GBP+CHF.EUR.SP00.A is the dimension selector: See helpmds('ECB/EXR') for what dimensions exist in ECB/EXR)
#' }
#' Here,
#' \itemize{
#' \item 'A' denotes annual frequency from the FREQ dimension
#' \item 'GBP+CHF' denote British pound and Swiss frank from the currency dimension
#' \item 'EUR' denotes the currency denominator
#' \item 'SP00' denotes spot rates
#' \item 'A' denotes average over periods (here annual averages)
#' }
#' Omit dimension codes to load all elements of a dimension, i.e. ECB/EXR/A..EUR.SP00.A to download exchange ratesw of the euro vis-a-vis all currencies
#' Note that a query is normally taken as Provider/Dataflow/Selectors. The combination with dots Provider.Dataflow.Selectors is not tolerated here.
#' Note that dataflow codes may be case-sensitive
#'
#' To see which providers are available, and to find query codes, use \code{\link{helpmds}}, or check the websites of the providers, such as \url{https://data.ecb.europa.eu/data/datasets}. Looking on \url{https://db.nomics.world} is a faster alternative for many providers.
#'
#' \code{mdSdmx} is a function for loading from SDMX data sources. \code{mds} encompasses that one, and will be broadened to other data sources at a later stage.
#' @seealso \code{\link{helpmds}}, \code{\link{DTstat}}, \code{\link[MD3]{Nomics}}, \code{\link[MDcountrycode]{defaultcountrycode}}, \code{\link{mdAmeco}}, \code{\link{mdWEO}}
#' @examples
#' mds("ECB/EXR/A.GBP+JPY+USD.EUR.SP00.A")
#' mds("ECB/EXR/Q.PLN+CZK+SEK.EUR.SP00.A",as = '2d')
#'
#' o1=mds("ECB/EXR/A.GBP+JPY+USD.EUR.SP00.A",labels=TRUE)
#' dimcodes(o1)
#'
#'
#' mds("OECD/EO/AUT+FRA.GDP.Q")
#' foo=mds("ESTAT/prc_hpi_q/Q..I15_Q.")
#'
#' foo[AT:BG.TOTAL.y2020:y]
#'
#' mds("BBK/BBFBOPV/M.N.DE.W1.S1.S1.T.B.G+S+CA._Z._Z._Z.EUR._T._X.N.ALL")
#' mds('IMF/FSI/A.FI+DK+AT.FSASDLD_EUR+FSANL_PT')
#'
#' mds("BIS/WS_EER_M/M.N.B.AT+FR+BE",startPeriod=2021)
#'
#' mds("AMECO/A/AT+FR+BE.1_0_0_0_UVGD", as='data.table')
#'
#' @export
mds = function(code, startPeriod='', endPeriod='', drop=TRUE, labels=FALSE,
                    as = c("md3", "array", "numeric", "data.table", "zoo", "2d", "1d", "pdata.frame",'data.frame'),
                    ccode=getOption('defaultcountrycode','EC'),verbose=FALSE) {
  if (!length(startPeriod)) {startPeriod=''}; if (!length(endPeriod)) {endPeriod=''}
  if (is.numeric(startPeriod)) { if (startPeriod<1) {startPeriod=''} else { startPeriod=as.character(startPeriod)}}
  if (is.numeric(endPeriod)) { if (endPeriod<1) {endPeriod=''} else { endPeriod=as.character(endPeriod)}}

  if (missing(code)) return(helpmds())
  ixprov=match(.fixSdmxCode(code,asvector = TRUE)[1],.mdstats_providers$table()[[1]],nomatch=0)
  if (ixprov>0) {
    provtype=.mdstats_providers$table()[['PrimType']][ixprov]; if (is.na(provtype)) provtype=''
    if (provtype=='function') {
      return(get(.mdstats_providers$table()[['DataProcessingFunction']][ixprov])(code=code,startPeriod=startPeriod,endPeriod=endPeriod,drop=drop,labels=labels,as=as,ccode=ccode,verbose=verbose))

    }
    mout=.sdmxasmd3(code,startPeriod=startPeriod, endPeriod=endPeriod, drop=drop,metadata=labels,verbose=verbose,ccode=ccode)
    return(MD3:::.getas(mout,as))
  } else {
    if (any(grepl('nomics',tolower('code')))) {stop('DBnomics not available via this route. Try function Nomics, or helpNomics()')}
    if (any(grepl('fred',tolower('code')))) {stop('FRED of the St. Louis Fed is yet not available. Keep a watch on this package though, it is the next in line')}
    stop('Provider not available')
  }

}




#' Get data from an SDMX source as a data.table
#'
#' @param code a character query string with a RestFul SDMX query (such as ECB/EXR/A.GBP+CHF.EUR.SP00.A). See \code{\link{mdSdmx}} for details
#' @param reshape a formula combining dimension names in order to reshape the result. Default is \code{...~ TIME} which means each time period to be in a column, plus extra identifier columns at the beginning. See \code{\link[data.table]{dcast}} for details.
#' @param drop if TRUE, drop any singleton dimensions (see also drop.md3)
#' @param labels add extra columns with the descriptions for each dimension element, e.g. "Austria" for "AUT"
#' @param ccode If not \code{ccode==NULL}, then the function attempts to convert to ccode destination such as 'iso2c', 'EC', 'iso3c'. See \code{\link[MDcountrycode]{ccode}} for permissible values.  \code{\link[MDcountrycode]{defaultcountrycode}} for defining this value as a session-wide option.
#' @param startPeriod placeholder for future use
#' @param endPeriod placeholder for future use
#' @return a \code{data.table}
#' @details This function tweaks the package \code{rsdmx}  to load data. For formulating SDMX query codes, check \code{help(\link{mdSdmx})}
#' @seealso \code{\link{helpmds}}, \code{\link{DTstat}}, \code{\link[MD3]{Nomics}}, \code{\link[MDcountrycode]{defaultcountrycode}}, \code{\link{mdAmeco}}, \code{\link{mdWEO}}
#' @examples
#' mdSdmx("ECB/EXR/A.GBP+JPY+USD.EUR.SP00.A")
#' mdSdmx("ECB/EXR/A.GBP+JPY+USD.EUR.SP00.A",as = 'zoo')
#'
#' o1=mdSdmx("ECB/EXR/A.GBP+JPY+USD.EUR.SP00.A",labels=TRUE)
#' dimcodes(o1)
#' @export
DTstat= function(code, reshape=as.formula(...~ TIME), drop=TRUE, labels=FALSE,
                 ccode=defaultcountrycode(),startPeriod=NULL,endPeriod=NULL) {
  x =mds(code,drop,labels, as='data.table',ccode = ccode)
  mydc=attr(x,'dcstruct')

  #browser()
  if (length(reshape)) {
    x=x[,c(names(MD3:::.getdimnames(x)),MD3:::.md3resnames('value')),with=FALSE]
    x=data.table::dcast(x,formula = reshape,value.var = MD3:::.md3resnames('value'))
  } else {
    colnames(x) = gsub('^_.','',colnames(x))
  }
  attr(x,'dcstruct') <- mydc
  if (!labels) return(x)
  for (i in setdiff(names(mydc),'TIME')) {
    x[[paste0('label:',i)]] <- mydc[[i]][x[[i]],match('label:en',colnames(mydc[[1]]),nomatch=2),drop=TRUE]

  }
  x
}



#ff=mds('ECB/EXR/M.RON.EUR.SP00.',labels = TRUE)
#dimcodes(ff)

#PROBLEMMMMMMMMMM!
#ii=mds('Estat/prc_hicp_midx/M.I15.CP00.')
#ii[.y2023m07,as='numeric']
#as.numeric(ii[.y2023m07]/ii[.y2022m07]-1)
.fetchdnwcodelist = function(mycode,verbose=FALSE) {
  vq=suppressWarnings(.fixSdmxCode(mycode))
  dfmeta=rsdmx::readSDMX(providerId=vq[1],resource='dataflow',resourceId = vq[2],verbose = verbose)
  if (any('dataflows' %in% slotNames(dfmeta))) rid=dfmeta@dataflows[[1]]@dsdRef else rid =vq[2]
  if (toupper(vq[1])=='OECD') rid=vq[2]
  dfdsd=rsdmx::readSDMX(gsub('references=children','references=none',.rsdmxurl(vq[1],resource='datastructure',resourceId = rid)),verbose=verbose)
  if (isS4(dfdsd@datastructures)) { dfdsd=dfdsd@datastructures}
  possdn=unlist(lapply(dfdsd@datastructures[[1]]@Components@Dimensions, function(x) {yy=x@codelist; names(yy)=x@conceptRef; yy}))
  attr(possdn,'dsdRef') =rid
  possdn
}

.fetchfullcodelists = function(mycode,verbose=FALSE,sdmxdataflow=NULL,forcerefresh=FALSE) {
  supersilent=FALSE;if (!length(verbose)) { supersilent=TRUE; verbose=FALSE}
  lcode=.fixSdmxCode(mycode)
  #browser()
  possdn = .mdstats_providers$dfdims(lcode[1],lcode[2])
  dsdid = attr(possdn,'dsdRef')

  biglcl=.mdstats_providers$allcodelists()
  isthere=unlist(lapply(biglcl[[lcode[1]]][paste0(lcode[1],'/',possdn)],length))
  if (!length(isthere) || !all(isthere) || forcerefresh) {
    if (!supersilent) message('Fetching dimension metadata for ',lcode[2],' from ',lcode[1], ', this might take some time')
    xurl=.rsdmxurl(lcode[1],resource='datastructure',resourceId = dsdid)
    mysuffix=try(.mdstats_providers$table()[lcode[1],'RefDetailSuffix'],silent=TRUE)
    if (is(mysuffix,'try-error')) mysuffix='' else if (!length(mysuffix)) mysuffix='' else if (is.na(mysuffix)) mysuffix=''
    if (nchar(mysuffix)) xurl=paste0(xurl,mysuffix)

    if (verbose) cat(xurl,'\n')
    dfdsd=rsdmx::readSDMX(xurl,verbose=verbose)
    dfcl=.extractDimcodes(dfdsd)

    biglcl[[lcode[1]]][unlist(lapply(dfcl,attr,'codelist'))] <- dfcl
    biglcl=.mdstats_providers$allcodelists(biglcl)
  } else {
    dfcl = biglcl[[lcode[1]]][paste0(lcode[1],'/',possdn)]
  }
  names(dfcl) = names(possdn)
  dfcl

}



.helpsdmx = function (query, pattern = "", dim = NULL, verbose = TRUE) {

  if (missing(query)) {
    if (!nchar(pattern)) {
    cat('The following SDMX providers are available: \n')
    cat(capture.output(print(.mdstats_providers$table()[,c(1,2,4)])),sep = '\n')
    cat('\nTo see which dataflows are available e.g. for provider BIS, run helpmds("BIS") \n')
    return(invisible(.mdstats_providers$table()[,c(1:4,7)]))
    }
    tempix=apply(.mdstats_providers$table()[,1:7],1,function(x) any(grepl(pattern,x,ignore.case = TRUE)))
    if (!any(tempix)) {cat('No provider matching your pattern "',pattern,'" has been found.\n'); return(character())}
    cat('The following SDMX providers match pattern: "',pattern, '"\n',sep='')
    cat(capture.output(print(.mdstats_providers$table()[tempix,c(1,2,4)])),sep = '\n')
    cat('\nTo see which dataflows are available e.g. for provider ',.mdstats_providers$table()[which(tempix)[1],1],
        ', run helpmds("',.mdstats_providers$table()[which(tempix)[1],1],'") \n',sep='')
    return(invisible(.mdstats_providers$table()[tempix,c(1:4,7)]))
  }
  pattern=as.character(pattern)[1]
  query=as.character(query)[1]

  vq=.fixSdmxCode(query,asvector = TRUE)


  if (!nchar(vq['Filter']) && !nchar(vq[2])) {

    mydf=.mdstats_providers$dataflows(vq[1])
    if (!is.na(.mdstats_providers$table()[vq[1],'LookatDSDref'])) { mydf[,'id']=mydf[,'dsdref']}
    mydf=mydf[,-(2:3)]
    if (!nchar(pattern)) {
      cat('Provider ',vq[1],  ' has the following data flows available:\n')

    } else {
      ix = apply(mydf,1,function(x) any(grepl(pattern,x,ignore.case = TRUE)))
      if (!any(ix)) {ix=logical()}
      if (length(ix)) mydf=mydf[which(ix),] else {
        stop('none of the dataflow codes or labels matches pattern "',pattern,'"')
      }
      cat('Provider ',vq[1],  ' has the following data flows matching pattern ',pattern,':\n')


    }

    if (NROW(mydf)>50) {
      cat(capture.output(print(mydf[sample(NROW(mydf),50),])),sep='\n')
      cat('plus ',NROW(mydf)-50, ' more.')
    } else {

      cat(capture.output(print(head(mydf,50))),sep='\n')
    }
    cat('Run helpmds("',vq[1],', pattern="YOUREXPRESSION") to search for specific terms\n',sep = '')
    cat('Run xx=helpmds("',vq[1],'") to load the list of dataflows into variable xx\n',sep='')
    return(invisible(mydf))

  }


  if (nchar(vq['Filter'])) {
    testdf=try(rsdmx::readSDMX(paste0(.rsdmxurl(vq[1],'data',vq[2],keyaschar = vq[3]),'?detail=serieskeysonly')),silent=TRUE)
    if (!any(grepl('err',class(testdf)))) {
      cat("The query ", query, " is fine and should return results\n...")

      temp=mdSdmx(query,drop=FALSE,labels=TRUE,as='md3',verbose = verbose)
      mydc=MD3:::.getdimcodes(temp)
      mydn=names(.fetchdnwcodelist(paste0(vq[[1]],'/',vq[[2]])))
      if (length(dim)) {
        if (is.character(dim)){ dim=mydn[match(tolower(dim),tolower(mydn))]} else {      dim=mydn[dim]}
        cat('It has the following elements in dimension "',dim,'":\n',sep='')
        browser()
        if (is.na(match(dim,names(mydc)))) {
          mytbl=.fetchfullcodelists(paste0(vq[[1]],'/',vq[[2]]),verbose = verbose)[[dim]]
        } else {
        mytbl=mydc[[dim]]
        }
        cat(capture.output(mytbl),sep='\n')
        return(invisible(mytbl))
      }


      if ('TIME' %in% names(mydc)) { mydc=mydc[-match('TIME',names(mydc))]}
      cat("It contains the following dimensions with more than one element: ")
      for (i in which(unlist(lapply(mydc,NROW))>1)) {
        cat('\nDimension ',names(mydc)[[i]],':\n')
        cat(capture.output(mydc[[i]]), sep='\n')
      }
      return(invisible(mydc))
    }
    codeshere=strsplit((MD3:::.mdrest2codes(paste0(' ',vq[3],' '),10)),split='\\+')
    mydims=try(.fetchdnwcodelist(query),silent=TRUE)
    if (any(grepl('err',class(mydims)))) { return(helpmds(paste0(vq[1],'/',vq[2]))) }
    if (length(codeshere)!=length(mydims)) { stop('Your query ', query, ' suggests that the dataflow ', vq[1],'/',vq[2],' has ',
                                length(codeshere),' dimensions. But ', vq[2], ' contains ',length(mydims), 'dimensions.')}

    fcl=.fetchfullcodelists(paste0(vq[1],'/',vq[2]),verbose = verbose)

    for (i in seq_along(codeshere)) {
      thismatch=match(trimws(codeshere[[i]]),fcl[[i]],nomatch=NA)
      if (anyNA(thismatch)) {
        stop('The code ', paste(head(codeshere[[i]][is.na(thismatch)],3),collapse=', '), '  does not exist in ',vq[1],'/',vq[2],"'s ",i,'th dimension ', names(fcl)[i])
      }
    }


  }


  #so its about a dataflow
  #mydims=try(.fetchdnwcodelist(query),silent=TRUE)
  dfmeta=rsdmx::readSDMX(providerId=vq[1],resource='dataflow',resourceId = vq[2],verbose = FALSE)
  if (any('dataflows' %in% slotNames(dfmeta))) rid=dfmeta@dataflows[[1]]@dsdRef else rid =vq[2]
  if (toupper(vq[1])=='OECD') rid=vq[2]
  dfdsd=rsdmx::readSDMX(gsub('references=children','references=none',.rsdmxurl(vq[1],resource='datastructure',resourceId = rid)),verbose=verbose)

  if (length(dim)) {
    #its about a dimension:
    mydn=.fetchfullcodelists(paste0(vq[1],'/',vq[2]),verbose = verbose )
    if (is.character(dim)){
      mydim=mydn[[match(tolower(dim),tolower(names(mydn)),nomatch=NA)]]
    } else {
      mydim=mydn[[dim]]
    }
    #browser()
    if (!length(mydim)) stop('dimension ',dim,'does not exist in dataflow ',vq[1],'/',vq[2],'.')
    #tempnomics=try(MD3::helpNomics(paste0(.mdstats_providers$table()[vq[1],'nomicsID'],'/',vq[2]),dim=dim,verbose = FALSE),silent=TRUE)
    dfinfo=suppressWarnings(try(MD3:::.NomicmdStataflowinfo(.mdstats_providers$table()[vq[1],'nomicsID'],vq[[2]]),silent=TRUE))

    if (!nchar(pattern)) {

      if (any(grepl('err',class(dfinfo))))      {


      cat('dimension',dim, 'has ',NROW(mydim),' codes:')
      cat(capture.output(print(head(mydim,50))),sep='\n')
      if (NROW(mydim)>50) cat('\nand ',NROW(mydim)-50,'more')
      return(invisible(mydim))
      } else {
        tempdim0=dfinfo$dn[[which(toupper(names(dfinfo$dn))==toupper(dim))]]
        tempdim=matrix(unlist(lapply(tempdim0,'[[',1L)),length(tempdim0)); rownames(tempdim)=names(tempdim0); colnames(tempdim)=paste0(vq[[1]],'/',vq[[2]],': ',dim)

        cat('db.nomics suggest that dimension ',dim, 'contains ',NROW(tempdim),' codes that have data:\n',sep='')
        cat(capture.output(print(head(tempdim,50))),sep='\n')
        if (NROW(tempdim)>50) cat('\nand ',NROW(tempdim)-50,'more with data')
        if (NROW(mydim)>NROW(tempdim)) { cat('\nIn addition there are ',NROW(mydim)-NROW(tempdim),'permissible codes for that dimension with empty data') }
      }
      cat('\nRun e.g. helpmds("',vq[1],'/',vq[2],'", dim="',names(mydn)[2],'", pattern="MYSEARCHTERM") to search the codes and descriptions for dimension ',names(mydn)[2],sep='')
      cat('\nRun e.g. xx=helpmds("',vq[1],'/',vq[2],'", dim="',names(mydn)[2],'") to load all codes and descriptions for dimension ',names(mydn)[2],' into variable xx\n',sep='')

            return(invisible(tempdim))
    } else {

      mydim=mydim[,-1,drop=FALSE]
      if (any(grepl('err',class(dfinfo))))    {
          tempdim=mydim

      }
      ix=union(grep(pattern,rownames(mydim),ignore.case = TRUE),which(apply(mydim,1,function(x) any(grepl(pattern,x,ignore.case = TRUE)))))
      if (length(ix)) {
        mydim=mydim[ix,,drop=FALSE]
        splural='s'; if (NROW(mydim)<2) splural=''
        cat(NROW(mydim), ' code',splural,' or label',splural,' match',ifelse(splural=='','es',''),' the pattern "',pattern,'":\n',sep='')
        cat(capture.output(print(head(mydim,50),right=FALSE)),sep='\n')
      } else {
        cat('No label or code matching "',pattern,'" could be found')
      }
      return(invisible(mydim))
    }

  }

  if (isS4(dfdsd@datastructures)) {
    dfdsdds=dfdsd@datastructures@datastructures[[1]]
  } else  dfdsdds=dfdsd@datastructures[[1]]

  tempname=unlist(dfdsdds@Name);
  cat('Dataflow', vq[2],' has the following name:\n');
  cat(paste(paste0(names(tempname),": ",tempname),collapse = '\n')); rm(tempname)
  mydims=unlist(lapply(dfdsdds@Components@Dimensions, function(x) {x@conceptRef}))
  cat('\nand has ',length(mydims), ' dimensions excl TIME:\n',paste(mydims, collapse=', '))



  dfinfo=suppressWarnings(try(MD3:::.Nomicsdataflowinfo(.mdstats_providers$table()[vq[1],'nomicsID'],vq[[2]]),silent=TRUE))

  if (!any(grepl('err',class(dfinfo)))) {
    xmpl = dfinfo$example_query
    if (.mdstats_providers$table()[vq[1],'nomicsID']!=vq[1]) { xmpl= paste0(vq[1],'/',gsub('^[^/]*/','',xmpl))}
    cat('\nExample query: ',xmpl)
  }

  cat('\nRun e.g. helpmds("',vq[1],'/',vq[2],'", dim="',mydims[2],'") to see the codes for dimension ',mydims[2],sep='')
  cat('\nRun e.g. helpmds("',vq[1],'/',vq[2],'", dim="',mydims[2],'", pattern="MYSEARCHTERM") to search the codes and descriptions for dimension ',mydims[2],sep='')



}



#' Find an SDMX query code, or find out why yours does not work
#'
#' @param query a character query string with a RestFul SDMX query: either empty string, or a provider such as ('ECB' or 'LTNSI'), or provider.dataflow (such as 'ECB/EXR' ) or a full query  (such as ECB/EXR/A.GBP+CHF.EUR.SP00.A). below for details
#' @param pattern a character string to search for in the result that \code{query} provides
#' @param dim a character string denoting a certain dimension of the dataflow indicated by query (or the integer number of that dimension),
#' @param verbose if TRUE, the function will chat about servers requests and warn when it might take some time
#' @return the function prints explanations and invisibly returns a list or data.frame in addition
#' @details This function works with the metadata of the SDMX providers, which can be slow. Note that in many cases \code{\link[MD3]{helpNomics}} might be faster to use and yield the same result.
#' @section SDMX query:
#' A restful SDMX query combines Provider, Dataflow and Dimension selectors. Take the example 'ECB/EXR/A.GBP+CHF.EUR.SP00.A'
#' \itemize{
#'   \item 'ECB' designates the provider ECB (see helpmds() for a list of providers)
#'   \item 'EXR' designates the ECB's dataflow 'EXR' about exchange rates (see helpmds('ECB') for a list of ECB dataflows)
#'   \item A.GBP+CHF.EUR.SP00.A is the dimension selector: See helpmds('ECB/EXR') for what dimensions exist in ECB/EXR)
#' }
#' Here,
#' \itemize{
#' \item 'A' denotes annual frequency from the FREQ dimension
#' \item 'GBP+CHF' denote British pound and Swiss frank from the currency dimension
#' \item 'EUR' denotes the currency denominator
#' \item 'SP00' denotes spot rates
#' \item 'A' denotes average over periods (here annual averages)
#' }
#' Omit dimension codes to load all elements of a dimension, i.e. ECB/EXR/A..EUR.SP00.A to download exchange ratesw of the euro vis-a-vis all currencies
#' Note that a query is normally taken as Provider/Dataflow/Selectors. The combination with dots Provider.Dataflow.Selectors is not tolerated here.
#' Note that dataflow codes may be case-sensitive
#'
#'
#' @section Finding query codes:
#'
#' To find query codes, the easiest is to go to \url{http://db.nomics.world} and search for codes there
#' Alternatively, you can use \code{\link{helpNomics}}
#'
#' \itemize{
#'  \item \code{helpmds()} returns a vector with available providers
#'  \item \code{helpmds("PROVIDER")} returns a vector with the dataflows available for the provider
#'  \item \code{helpmds("PROVIDER", pattern="PATTERN")} looks for a dataflow whose name or code matches the string pattern
#'  \item \code{helpNomics("PROVIDER/DATAFLOW")} prints the structure of the dataflow dimensions, as well as an example query, and invisible returns the structure of code names within the dataflow
#'  \item \code{helpNomics("PROVIDER/DATAFLOW/SELECTORS")} invisibly returns the query result. If not successful, it provides clues whwere the query might be wrong
#'  \item \code{helpNomics("PROVIDER/DATAFLOW", dim="NumberOrName")} prints the code names and labels of the selected dimension, and invidibly returns a data.frame with those codes and labels
#'  \item \code{helpNomics("PROVIDER/DATAFLOW", dim="NumberOrName", pattern="PATTERN")} searches the codes and labels of that dimension for a certain pattern
#' }
#'
#' @seealso \code{\link{mds}} to load data,  \code{\link[MD3]{helpNomics}} for a similar function
#' @examples
#' #lets find house price data from the Bank of International Settlements
#' helpmds() #BIS is a provider that is available
#' helpmds(pattern='bis.org') #indeed
#'
#' helpmds('BIS') #list all available dataflows
#'
#' helpmds('BIS',pat='property') #so the dataflow we need is called 'WS_SPP'
#'
#' helpmds('BIS/WS_SPP') # so there are 4 dimensions (apart form time): FREQ, REF_AREA, VALUE, UNIT_MEASURE
#'
#' #lets check what is in ALUE
#' helpmds('BIS/WS_SPP',dim='VALUE') #only two dimensions in VALUE
#' oo=helpmds('BIS/WS_SPP',dim='REF_AREA') #but lots of countries. I guess not all of them will be filled
#' oo # show all countries in there
#'
#' helpmds('BIS/WS_SPP',dim='FREQ') #all kinds of frequencies too that are permissible. But not all of them will have data.
#'
#' foo=mds('BIS/WS_SPP/Q.FR.N.', labels=TRUE) #lets look for nominal quarterly house prices from France
#' foo #hmm, what does 628 mean?
#'
#' dimcodes(foo)[[1]] #one way to find out
#'
#' foo=helpmds('BIS/WS_SPP/Q.FR.N.') # another way to find out
#' #note that foo now contains the dimcodes for that code
#'
#' helpmds('BIS/WS_SPP/Q.FR.N.', dim='UNIT_MEASURE') # another way to find out
#'
#' helpmds('BIS/WS_SPP',dim=4,pattern='628' ) \ another way to find out
#'
#'
#' #find out why another formulation does not work:
#' helpmds('BIS/WS_SPP/Q.FRA.N.')
#'
#' @export
helpmds = function(query='', pattern = "", dim = NULL, verbose = TRUE){
  query=trimws(query[1])
  if (nchar(query)) {
    sprov=.fixSdmxCode(query,asvector = TRUE)[[1]]

    stype=.mdstats_providers$table()[sprov,'PrimType']
    if (is.na(stype)) return(.helpsdmx(query,pattern,dim,verbose))
    if (grepl('SDMX',stype,ignore.case = TRUE)) return(.helpsdmx(query,pattern,dim,verbose))
    return(get(.mdstats_providers$table()[sprov,'PrimRepoUrl'])(query,pattern,dim,verbose,sdmxlike=TRUE))
  }

  temp=.mdstats_providers$table()

  temp[is.na(temp[,'PrimType']),'PrimType']=''
  ix=(temp[,'PrimType']=='') | grepl('SDMX',temp[,'PrimType'],ignore.case = TRUE)
  temp[!ix,1]=paste0(temp[!ix,1],'*')


  if (!nchar(pattern)) {
    cat('The following SDMX providers are available: \n')
    cat(capture.output(print(temp[,c(1,2,4)])),sep = '\n')
    cat('\nTo see which dataflows are available e.g. for provider BIS, run helpmds("BIS") \n')
    return(invisible(temp))
  }
  tempix=apply(.mdstats_providers$table()[,1:7],1,function(x) any(grepl(pattern,x,ignore.case = TRUE)))
  if (!any(tempix)) {cat('No provider matching your pattern "',pattern,'" has been found.\n'); return(character())}
  cat('The following SDMX providers match pattern: "',pattern, '"\n',sep='')
  cat(capture.output(print(temp[tempix,c(1,2,4)])),sep = '\n')
  cat('\nTo see which dataflows are available e.g. for provider ',.mdstats_providers$table()[which(tempix)[1],1],
      ', run helpmds("',.mdstats_providers$table()[which(tempix)[1],1],'") \n',sep='')
  return(invisible(temp))

}


.cachelocation = function(usestored=FALSE) {
  if (usestored) {
    if (exists('.mdstats_providers')) {return(.mdstats_providers$cachepath())}
  }

  tempcachepath=function() {
    CacheDirLocal=NULL
      tmpdirpath = normalizePath(paste0(tempdir(), "/.."))
      tmpdirs = list.dirs(tmpdirpath, full.names = TRUE, recursive = FALSE)
      olddirs = tmpdirs[file.exists(paste0(tmpdirs, "/RtmpmdStats.txt"))]
      if (length(olddirs) > 0) {
        lastatime = difftime(Sys.time(), file.info(olddirs)[["atime"]],
                             units = "hours")
        if (min(lastatime) > 48) {
          unlink(olddirs)
        }
        else {
          CacheDirLocal = olddirs[which.min(lastatime)]
          if (length(olddirs) > 1)
            unlink(olddirs[olddirs != CacheDirLocal],
                   force = TRUE, recursive = TRUE)
        }
      }

    if (is.null(CacheDirLocal)) {
      CacheDirLocal = paste0(tempdir(), "mdStats")
      dir.create(CacheDirLocal)
      bres = file.create(paste0(CacheDirLocal, "/RtmpmdStats.txt"))

    }
    return(CacheDirLocal)
  }
  temppkgs=installed.packages()
  if ('mdStats' %in% rownames(temppkgs)) mypath=paste0(temppkgs['mdStats','LibPath'],'/mdStats') else return(tempcachepath())
  if (file.access(mypath)<0) {return(tempcachepath()) }
  mypath=paste0(mypath,'/data/cache')
  if (!dir.exists(mypath)) {dir.create(mypath)}

}


.findgeodim = function(possdn,provider=NULL) {
  #this function takes possdn, which is a character vector of codelists associated with dimensions, and named with dimensionm names
  #\ anmd tries to guess which one of those dimensiosn refers to countrycodes
  z0=.mdstats_providers$table()
  if (!length(provider)) {
    z1=paste(na.omit(toupper(z0[['GeoCLsandDims']])),collapse=',')
  } else {
    z1=z0[toupper(z0[[1]])==toupper(provider[1]),'GeoCLsandDims']
    if (anyNA(z1)) return(character())
  }

  z2=as.list(gsub('^.*/','',strsplit(toupper(z1),split=',')[[1L]]))
  rescl=(lapply(z2,function(x) grepl(x,toupper(possdn))))
  if (is.null(names(possdn))) {names(possdn)<-possdn}
  resdn=(lapply(z2,function(x) grepl(x,toupper(names(possdn)))))
  ix=which(apply(as.data.frame(c(resdn,rescl)),1,any))
  names(possdn)[ix]
}



.countrycodefixer = function(inmd3ordt,provider=NULL,whichdim=0,tocode=defaultcountrycode()) {





    x=MD3:::.dt_class(inmd3ordt)
    mydc=attr(x, "dcstruct")
    if (any(whichdim<1)) {
      if (is.null(mydc)) {
        whichdim=.findgeodim(colnames(x),provider)
      } else{
        whichdim=.findgeodim(names(mydc),provider)
      }
    }
    if (is.numeric(whichdim)) whichdim=colnames(x)[whichdim]
    notgood=character()
    if (!is.null(mydc)) {

      for (i in whichdim) {
        mydic=MDcountrycode:::.fixcountrycode(mydc[[i]],tocode = tocode,cols2fix = 'code')
        if (NCOL(mydic)>1) { myvec=mydic[,1,drop=TRUE]} else {myvec=mydic}
        if (anyDuplicated(myvec)) {
          if (!length(notgood)) {warning('Not converting country codes due to duplicates relating to ', utils::head(myvec[duplicated(myvec)],1),', etc')}
          notgood = c(notgood, whichdim)
          next;
        }
        rownames(mydic)=myvec
        mydc[[i]] = mydic;
      }
    }
    if (length(setdiff(whichdim,notgood)))  {y=MDcountrycode:::.fixcountrycode(x,tocode = tocode,cols2fix = setdiff(whichdim,notgood))} else {y=x}

    if (!is.null(mydc)) {attr(y,'dcstruct') =mydc}
    if (MD3:::.md3_is(inmd3ordt))  y=MD3:::.md3_class(y)
    return(y)
}


#https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/dataflow/ESTAT/namq_10_gdp/latest?detail=referencepartial&references=descendants



