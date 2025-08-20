#' @include data.R

.mdAMECOfilepath = function (year=0,release=c('spring','autumn'), quiet=TRUE,conttrying=TRUE)
{
  if (year<2010 & year>0) stop("Ameco vintages pre-2011 are not available ")

  amecourl="https://ec.europa.eu/economy_finance/db_indicators/ameco/documents/ameco0.zip"
  if (year>2010) {#
    #release=switch(substr(tolower(release[[1L]])),'a'=)
    amecourl=.vintageAmecourl(year,release)
  } else {
    hurl=httr::HEAD(amecourl)
    dtcurameco=hurl$all_headers[[length(hurl$all_headers)]]$headers$`last-modified`
    amecotime=format(as.POSIXct(dtcurameco,format="%a, %d %b %Y %H:%M",tz='UTC'),"%F")
  }
  release=switch(substr(tolower(release[[1]]),0,1),a='AF',s='SF')


  datasetcode = paste0('ameco_',year,release)
  if (year<1) datasetcode = paste0('ameco_latest')
  if (!dir.exists(paste0(.cachelocation(TRUE), "/ameco"))) dir.create(paste0(.cachelocation(TRUE), "/ameco"))
  filepath = suppressWarnings(normalizePath(paste0(.cachelocation(TRUE),
                                                   "/ameco/", datasetcode, ".zip")))



  if (file.exists(filepath)) {
    if (file.mtime(filepath) > amecotime) { return(filepath)}
  }

  ret = try(download.file(amecourl,destfile = filepath, quiet = quiet,mode = 'wb'),silent = TRUE)
  if (!conttrying) {return(filepath) }
  if (!is.list(ret)) { return(filepath) }

  return(.mdAMECOfilepath(year=ifelse(release==1,year-1,year),release=ifelse(release==1,2,1), quiet=quiet, conttrying=FALSE))


}


.parseAmeco= function(year=0,release=0,verbose=TRUE) {
  #downloads IMF WEO data
  #ARGUMENTS
  # year: e.g. 2009
  # release: either 1 or 2
  zipfilepath=.mdAMECOfilepath(year,release,quiet = !verbose)
  vfiles=utils::unzip(zipfilepath,exdir = gsub("\\.zip","",zipfilepath),overwrite=TRUE)


  lameco=lapply(as.list(vfiles),fread,fill=TRUE,header =TRUE)
  unlink(gsub("\\.zip","",zipfilepath),recursive = TRUE)
  dameco=rbindlist(lameco)
  if (anyDuplicated(dameco[[1]])) dameco=dameco[!duplicated(dameco[[1]]),]
  tempcodes=MD3:::.mdrest2codes(dameco[[1]])
  tempcodes=cbind(COUNTRY=tempcodes[,1],TRAFO=apply(tempcodes[,2:5],1,paste,collapse='_'),INDICATOR=tempcodes[,6])
  if (!grepl('^0-9',colnames(dameco)[[NCOL(dameco)]])) { dameco=dameco[,(1:(NCOL(dameco)-1)),with=FALSE]}

  d2d=cbind(tempcodes,dameco[,grepl('^[0-9]*$',colnames(dameco)),with=FALSE])
  dstacked=data.table::melt(d2d,id.vars=1:3, variable.name='TIME', value.name = MD3:::.md3resnames('value'), variable.factor=FALSE,na.rm = TRUE)
   dstacked[['TIME']]=MD3:::as.timo(as.integer(dstacked[['TIME']]))
  #if (any(colnames(dstacked)=='COUNTRY')) colnames(dstacked)[colnames(dstacked)=='COUNTRY'] = 'countrylabel'

   clc=cbind(tempcodes[,1],`label:en`=dameco[,'COUNTRY']); clc=as.data.frame(clc[!duplicated(clc[,1]),])
   clt=cbind(tempcodes[,2],dameco[,'UNIT']); clt=as.data.frame(clt[!duplicated(clt[,1]),])
   cli=cbind(tempcodes[,3],dameco[,'TITLE']); cli=as.data.frame(cli[!duplicated(cli[,1]),])
   dcdraft=list(clc,clt,cli,unique(dstacked$TIME)); names(dcdraft)=c(colnames(tempcodes),'TIME')
   dcdraft=MD3:::.dimcodesrescue(dcdraft)
   for (i in 1:3) { colnames(dcdraft[[i]])[[2]]<-'label:en'}

   attr(dstacked,'dcstruct') =  dcdraft
   attr(dstacked,'dcsimp') =  lapply(dcdraft,'[[',1)
   return(MD3:::.md3_class(dstacked))

}

.vintageAmecourl = function(year,release) {
  #year must be >2011. release must be a or s

    release=switch(substr(tolower(trimws(release)),0,1),a='autumn',s='spring','1'='spring','2'='autumn')

  baseurl='https://economy-finance.ec.europa.eu/economic-research-and-databases/economic-databases/ameco-database/ameco-archive_en'
  #src=httr::GET(baseurl)
  src=readLines(baseurl)
  sline=src[grep(paste0('<h2>\\s*',year),src)]
  slin2=sub(paste0('^.*<h2>\\s*',year),'',sline)
  slin3=gsub("\\<h2.*$",'',substr(slin2,15,10000))
  ix=attr(regexpr(paste0('^.*',tolower(release)),tolower(slin3)),'match.length')
  slin4=substr(slin3,ix-200,ix+100)
  if (!length(slin4)) {stop('The Ameco file for ',year,' ',release,' is not available at\n',baseurl)}
  slin5=paste0(gsub('^.*href="','',gsub('\\.zip".*$','',slin4)),'.zip')
  surl=paste0(gsub('\\.eu/.*$','',baseurl),'.eu',slin5)
  return(surl)

}

#' Load data from publicly available Ameco, and is past vintages
#'
#' @param code a RestFul code combination respecting Ameco dimensions COUNTRY.x.x.x.x.INDICATOR, e.g. \code{CAN.1.0.99+0.0.UVGD}
#' @param year year of the release. Default 0 means current/latest release
#' @param release character singleton 'autumn' or 'spring'. Ignored if year is 0 and thus points to current release.
#' @param as how to output the result: \code{md3}: as md3 object with full metadata, \code{2d}: as a data.table with periods as column names, \code{1d} resp. \code{data.table}: as fully stacked data.table, \code{array}: as multi-dim array, \code{zoo}: as \code{\link[zoo]{zooreg}} time series object.
#' @param drop if TRUE, drop any singleton dimensions (see also drop.md3)
#' @param ccode If not \code{ccode==NULL}, then the function attempts to convert to ccode destination such as 'iso2c', 'EC', 'iso3c'. See \code{\link[MDcountrycode]{ccode}} for permissible values.  \code{\link[MDcountrycode]{defaultcountrycode}} for defining this value as a session-wide option.
#' @param startPeriod startyear (1960 if empty)
#' @param endPeriod end year (latest available if left empty)
#' @param nbdim number of dimensions to return: either \code{3} for COUNTRY, VARIABLE and TIME, where VARIABLE denotes codes such as \code{1_0_99_0_UVGD}, \code{'4'} for splitting VARIABLE into TRAFO (like \code{1_0_99_0}) and INDICATOR (like \code{UVGD}), \code{7} for splitting TRAFO further into its 4 subcomponents
#' @param inclaggreg default FALSE. In that case, if all countries are requested, the function returns individual counties as well as the latest EU and EA aggregate. If TRUE, it also returns other aggregates from Ameco, such as EA12.
#' @param verbose not active
#' @param \ldots further arguemtns passed from mds
#' @return an md3 object or other as specified by \code{as}
#' @details This function works by caching Ameco vintages on the local drive
#' @seealso \code{\link{mds}} for loading from major data sources
#' @examples
#'
#' mdAmeco('CAN.1.0.99+0.0.UVGD') #Canadian GDP in EUR and CAD
#'
#' foo=mdAmeco('.1.0.0.0.ZCPIH') #consumer price index in all countries
#' \dontrun{foo['.y2019:y2023']}
#'
#' foo=mdAmeco()
#' \dontrun{foo['DEU+ESP.1_0_310_0.UBCA.']} #German and Spanish current account as percent of GDP
#'
#' \dontrun{growth(mdAmeco('EST+CZE.1.0.0.0.ZCPIH',startPeriod=2020))} #inflation rates in Estonia and Czechia from 2021
#'
#' \dontrun{mdAmeco("AT+BE.1.0.0.0.UVGD",2017,2)}
#'
#' @export
mdAmeco = function(code="",year=0,release=0,as=c("md3", "array", "numeric","data.table","zoo","2d","1d"),
                   drop=TRUE, ccode=getOption('defaultcountrycode',NULL), startPeriod=NULL, endPeriod=NULL,
                   nbdim=3,inclaggreg=FALSE,verbose=TRUE,...) {

  if (is.character(startPeriod)) if (!nchar(startPeriod)) startPeriod=character()
  if (is.character(endPeriod  )) if (!nchar(endPeriod  )) endPeriod  =character()



  if (length(code)>1) { warning('code has to be singleton'); code=code[1]}
  if (is.character(code)) {
    if (any(grepl('/',code)) & release <1) {
      if (nchar(gsub('[^/]','',code))>1) { code=gsub('^[^/]*/','',code)}
        srel=toupper(gsub('/.*$','',code))
        code=gsub('^.*/','',code)
        if (!grepl('[0-9]',srel)) {
          if (exists('iAmeco')) { return(get('iAmeco')(code,startPeriod=startPeriod,endPeriod=endPeriod,as=as,ccode=ccode,revision=substr(srel,0,1),drop ))}
        } else {
          sfcst='A'; if (grepl('S',srel)) sfcst='S'
          return(mdAmeco(code,year = gsub('[^0-9]','',srel),release = sfcst,as=as,drop=drop,ccode = ccode,startPeriod = startPeriod, endPeriod = endPeriod,nbdim = 3 ))
        }



      }
    if (nchar(code[1])==0) code=character() else { code=gsub('_','.',code)}
  }
  if (length(code)) {
    nbpoints=nchar(gsub("[^\\.]","",code))
    if (nbpoints==5) {
      temp=strsplit(code,split='\\.')[[1L]];
      code =paste(temp[[1]],paste(temp[2:5],collapse = '_'),temp[[6]],sep='.')
      if (any(grepl('_[0-9]*\\+[0-9]*_',code))) code=paste(temp[1],paste(apply(as.matrix(as.data.frame(strsplit(temp[2:5],split ='\\+'))),1,paste,collapse='_'),collapse = '+'),temp[6],sep='.')
    }
    if (nchar(gsub("[^\\.]","",code))!= 2) stop('cannot interpret code ',code)
    temp=strsplit(code,split='\\.')[[1L]];
    if (any(grepl('\\+',temp[[1L]]))) {

      temp[[1L]]=paste(MDcountrycode::ccode(strsplit(temp[[1L]],split='\\+')[[1L]],c('iso2m','iso3c','iso2c'),'ameco',leaveifNA = TRUE,warn = FALSE),collapse='+')
      code=paste(temp,collapse = '.')
    }
  } else {temp=''}

  tempameco=.mdstats_providers$cachedmd3s(paste0('AMECO',year,release))
  if (is.null(tempameco)) {
    if (verbose) cat('\nLoading Ameco from source, this might take some time...')
    tempameco=.mdstats_providers$cachedmd3s(.parseAmeco(year=year,release = release, verbose=verbose),paste0('AMECO',year,release))
  }
  if (!inclaggreg) {

    selcc=MD3:::.getdimnames(tempameco)[['COUNTRY']];
    selcc=selcc[c(head(grep('^EU',selcc),1),head(grep('^EA',selcc),1),grep('[0-9]|_',selcc,invert = TRUE))]
    if (any(grepl('\\+',temp[[1]]))) {   selcc=unique(c(strsplit(temp[[1]],split='\\+')[[1]],selcc))}
    #browser();
    #tempameco=MD3:::.md3get(tempameco,list(COUNTRY=selcc,TRAFO=character(),INDICATOR=character(),TIME=character()))
    tempdt=MD3:::.dt_class(tempameco)
    tempameco=MD3:::.md3_class(tempdt[tempdt$COUNTRY %in% selcc,])
    #tempameco=MD3:::.md3_class(MD3:::.dt_class(tempameco)[COUNTRY %in% selcc,])
    mydc=attr(tempameco,'dcstruct'); mydc[['COUNTRY']]=mydc[['COUNTRY']][selcc,]; attr(tempameco,'dcstruct')<-mydc

  }
  if (!length(code)) return(tempameco)


  if (length(startPeriod)) { startPeriod=as.integer(startPeriod)  }
  if (length(endPeriod)) { endPeriod=as.integer(endPeriod)  }
  timeper=paste0('.',ifelse(length(startPeriod),startPeriod,""),":",ifelse(length(endPeriod),endPeriod,""))
  if (timeper=='.:') timeper='.'

  if (nchar(timeper)>7) if (endPeriod<startPeriod) {stop('startPeriod cant be after endPeriod')}
  mout=MD3:::.md3get(tempameco,paste(code,timeper),drop = FALSE)
  if (length(ccode)) { mout=.countrycodefixer(mout,NULL,'COUNTRY',ccode)}
  if (nbdim==4) {
    if (drop) mout=MD3:::drop.md3(mout)
    return(MD3:::.getas(mout,as))
  }
  if (nbdim==3) {
   # browser()
    dout=MD3:::.dt_class(mout)
    dout=data.table::copy(dout)
     dout[['VARIABLE']] = paste0(dout[['TRAFO']],'_',dout[['INDICATOR']])
     dout=dout[,c('COUNTRY','VARIABLE','TIME','_.obs_value'),with=FALSE]
     dnm= unique(dout[['VARIABLE']]) #attr(dout,'dcstruct')
     tempvbl=data.frame(dnm,attr(mout,'dcstruct')[['INDICATOR']][gsub('^.*_','',dnm),2],stringsAsFactors = FALSE)
     colnames(tempvbl)=c('code','label:en'); rownames(tempvbl)=dnm

     mydc=list(COUNTRY=attr(mout,'dcstruct')[['COUNTRY']],VARIABLE=tempvbl,TIME=attr(mout,'dcstruct')[['TIME']])
      attr(dout,'dcstruct')=NULL
     rm(mout); mout=data.table::copy(dout)
     mout=MD3:::.md3_class(mout);
     attr(mout,'dcstruct') = mydc
     if (drop) mout=MD3:::drop.md3(mout)
     return(MD3:::.getas(mout,as))
  }
  stop('not implemented yet')
}

.helpmdAmeco = function(query='', pattern = "", dim = NULL, verbose = TRUE, sdmxlike=FALSE, ...) {
  #stop("helpmdAmeco not  ready yet")
  vcode=suppressWarnings(.fixSdmxCode(query))
  vcode=vcode[nchar(vcode)>0]




  if (length(dim)) {
    #its about a dimension:
    if (is.numeric(dim)) dim=c('COUNTRY','TRAFO','INDICATOR')[dim]
    if (grepl('^geo|^cou',tolower(dim))) { dim='COUNTRY'}
    if (grepl('^trafo',tolower(dim))) { dim='TRAFO'}
    if (grepl('^indic|^varia|^subj',tolower(dim))) { dim='INDICATOR'}
    if (!(dim %in% c('COUNTRY','TRAFO','INDICATOR'))) stop('dimension ',dim,' is not available from Ameco')
    thatameco=names(.mdstats_providers$cachedmd3s()); thatameco=thatameco[grepl('AMECO',thatameco)][1]
    if (!length(thatameco)) { temp=mdAmeco(); thatameco='AMECO00' }
    odn=MD3:::.getdimcodes(.mdstats_providers$cachedmd3s(thatameco))
    odn=odn[[dim]]
    #return(invisible(odn[[dim]]))


    if (!nchar(pattern)) {


        cat('Dimension ',dim, 'contains ',NROW(odn),' codes that have data:\n',sep='')
        cat(capture.output(print(head(odn,50))),sep='\n')
        if (NROW(odn)>50) cat('\nand ',NROW(odn)-50,'more ')

      cat('\nRun e.g. helpmdAmeco("", dim="',dim,'", pattern="MYSEARCHTERM") to search the codes and descriptions for dimension ',dim,sep='')
      cat('\nRun e.g. xx=helpmdAmeco(dim="',dim,'") to load all codes and descriptions for dimension ',dim,' into variable xx\n',sep='')

      return(invisible(odn))
    } else {

      ix=union(grep(pattern,rownames(odn),ignore.case = TRUE),which(apply(odn,1,function(x) any(grepl(pattern,x,ignore.case = TRUE)))))
      if (length(ix)) {
        mydim=odn[ix,,drop=FALSE]
        splural='s'; if (NROW(mydim)<2) splural=''
        cat(NROW(mydim), ' code',splural,' or label',splural,' match',ifelse(splural=='','es',''),' the pattern "',pattern,'":\n',sep='')
        cat(capture.output(print(head(mydim,50),right=FALSE)),sep='\n')
      } else {
        cat('No label or code matching "',pattern,'" could be found')
        return(invisible(odn))
      }
      return(invisible(mydim))
    }

  }

  if (toupper(vcode[1])=='AMECO') {vcode=character()}
  if (!length(vcode)) {
    if (sdmxlike) {
      if (exists('iAmeco')) {
        cat('\nAMECO has three  key dataflow available via mds: "AMECO/C" is current Ameco, AMECO/P the latest publicly available Ameco, and AMECO/R restricted Ameco\n')
      } else {
        cat('\nAMECO has one key dataflow available via mds: "AMECO/A"\n')
      }
      cat('It may be used like mds("AMECO/A/FRA+AUT.1_0_0_0_UVGD")\n')
      cat('Use function mdAmeco() for more options, in particular to split Ameco data into more dimnesions, or to retrieve earlier vintages \n')
    }

    cat('\nAmeco has the following dimensions excl TIME:\n')
    cat('COUNTRY, INDICATOR\n')
    cat('Run e.g. helpmdAmeco(dim="INDICATOR", pattern="MYSEARCHTERM") to search the codes and descriptions for dimension INDICATOR')
    cat('in addition there are the following dataflows, denoting spring forecast (SF) and autumn foreacast (AF) of the respective year\n')
    cat(paste(unlist(lapply(as.list(2011:2021),function(x) c(paste0('SF',x),paste0('AF',x)))),collapse=', '),'\n')
    return(invisible(NULL))
  } else stop('???')
}

