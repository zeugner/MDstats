#' @include data.R

.mdWEOfilepath = function (year=0,release=0, ctrygroups = FALSE, quiet=FALSE, conttrying=TRUE)
{


  if (year<1) {
    if (Sys.Date() < as.Date(paste0(format(Sys.time(),"%Y"),'-04-20')))  {
      year=as.numeric(format(Sys.time(),"%Y"))-1
      release=2
    } else {
      year=as.numeric(format(Sys.time(),"%Y"))
      if (Sys.Date() < as.Date(paste0(format(Sys.time(),"%Y"),'-10-15'))) {
        release=1
      } else {
        release=2
      }
    }}

  if (year<2006 | (year==2006 & release==1)) stop("no data available")
  if (!(release %in% 0:2)) stop("wrong release number")
  temp=readLines(paste("https://www.imf.org/en/Publications/WEO/weo-database/",year,"/",ifelse(release==1,"April","October"),"/download-entire-database",sep=""),warn = FALSE)

  temp=temp[c(grep('By Countries',temp)[1],grep('By Country Groups',temp)[1])]
  temp=gsub('">.*$','',gsub('^.*href="','',temp[1]))
  weourl=paste0("http://www.imf.org",temp[1+ctrygroups])
  #weourl=paste("http://www.imf.org/external/pubs/ft/weo/",year,"/0",release,"/weodata/WEO",ifelse(release==1,"Apr","Oct"),year,"all",ifelse(ctrygroups,"a",""),".xls",sep="")
  #if (year==2011) weourl=sub("WEOOct","WEOSep",weourl)
  if (year==2011) weourl=sub("October","September",weourl)
  #if (year==2006) weourl=sub("weodata/WEOOct","data/WEOSep",weourl)
  if (year < 2006) {
    #weourl = paste0("https://www.imf.org/~/media/Files/Publications/WEO/WEO-Database/",year,"/0",release,"/WEO",ifelse(release==1,"Apr","Oct"),year,"all",ifelse(ctrygroups,"a",""),".ashx")
    #weourl = paste0("https://www.imf.org/~/media/Files/Publications/WEO/WEO-Database/",year,"/WEO",ifelse(release==1,"Apr","Oct"),year,"all",ifelse(ctrygroups,"a",""),".ashx")
    stop('WEO Year ',year,' not available via this function')
  }

  datasetcode = paste0('mdWEO_',year,'_',release,ifelse(ctrygroups,'_agg',''))
  if (!dir.exists(paste0(.cachelocation(TRUE), "/weo"))) dir.create(paste0(.cachelocation(TRUE), "/weo"))
  filepath = suppressWarnings(normalizePath(paste0(.cachelocation(TRUE),
                                                   "/weo/", datasetcode, ".tsv")))

  if (file.exists(filepath)) {
    if (difftime(Sys.time(), file.info(filepath)[["mtime"]],
                 unit = "days") < 15) {

      if (!any(grepl('html PUBLIC',readLines(filepath,n = 2, warn = FALSE)))) {
        if (!quiet) message(paste0("loading ", datasetcode, " from cache..."));
        return(filepath)
      }
    }
  }

  if (!conttrying) {
    download.file(weourl,destfile = filepath, quiet = quiet,mode = 'wb')
    return(filepath)
  }
  ret = try(download.file(weourl,destfile = filepath, quiet = quiet),silent = TRUE)
  if (!is.list(ret)) { return(filepath) }

  return(.mdWEOfilepath(year=ifelse(release==1,year-1,year),release=ifelse(release==1,2,1), ctrygroups = ctrygroups, quiet=quiet, conttrying=FALSE))


}


.parseWEO= function(year=0,release=0,ctrygroups = FALSE, quiet=TRUE) {
  #downloads IMF WEO data
  #ARGUMENTS
  # year: e.g. 2009
  # release: either 1 or 2


  jj=read.table(.mdWEOfilepath(year, release, ctrygroups, quiet),sep="\t",head=TRUE,
                stringsAsFactors=FALSE,fill=TRUE,na.strings=c("n/a",""),comment.char="",quote="\"",skipNul = TRUE,
                check.names = FALSE)


  for (xname in grep("^[0-9]*$",names(jj))) {
    jj[,xname]=suppressWarnings(as.numeric(gsub(",","",jj[,xname])))
  }
  jj[jj[,"Scale"] %in% "Billions",grep("^[0-9]",names(jj))]=jj[jj[,"Scale"] %in% "Billions",grep("^[0-9]",names(jj))]*1000
  jj[jj[,"Scale"] %in% "Billions","Scale"]="Millions"

  ctries=jj[,'Country']; names(ctries) = trimws(jj[,'ISO']); ctries=na.omit(ctries[!duplicated(ctries)]);
  attr(ctries,'na.action')=NULL; ctries=ctries[!is.na(names(ctries))]
  #subj=paste0(jj[,"Subject Descriptor"],', ',jj[,"Units"])
  subj=apply(jj[,c("Subject Descriptor","Units","Scale")],1,paste,collapse=', ')
  names(subj)=trimws(jj[,"WEO Subject Code"])
  subj=na.omit(subj[!duplicated(subj)]); attr(subj,'na.action')=NULL; subj=subj[!is.na(names(subj))]
  ddd=data.table(jj[,c(grep('^ISO$',names(jj)),grep('Subject.*ode',names(jj)),grep('^[0-9]*$',names(jj)))])
  #yyy=names(ddd)[-(1:2)]
  #yy2=as.timo(yyy); names(yy2)=yyy
  ddd=ddd[!is.na(ddd[[1]]) & !is.na(ddd[[2]]),]
  emptycol=apply(ddd,2,function(x) all(is.na(x)))
  if (any(emptycol)) ddd=ddd[,!emptycol,with=FALSE]

  dstacked=data.table::melt(data.table(ddd),id.vars=1:2, value.name = MD3:::.md3resnames('value'),variable.factor=FALSE)
  colnames(dstacked)[1:3]=c('COUNTRY','SUBJECT','TIME')
  dstacked=data.table::copy(dstacked[!is.na(dstacked[[4]]),])
  dstacked[['TIME']]=MD3:::.char2timo(dstacked[['TIME']],frq='A',guess = FALSE)
  attr(dstacked,'dcstruct')=(list(COUNTRY=data.frame(code=names(ctries),'label:en'=ctries,stringsAsFactors = FALSE,check.names = FALSE),
                                                       SUBJECT=data.frame(code=names(subj),'label:en'=subj,stringsAsFactors = FALSE,check.names = FALSE),
                                                       TIME=sort(unique(dstacked[['TIME']]))))
  oo=MD3:::.md3_class(dstacked)


}

#' Load data from IMF World Economic Outlook, and its past vintages
#'
#' @param code a RestFul code combination respecting IMF WEO dimensions GEO.INDICATOR, e.g. \code{CAN+AUS.NID_NGDP}. If set to NULL.empty vector then the cfunction will retuner everything - the entire WEO
#' @param year year of the release. Default 0 means current/latest release
#' @param release numeric singleton 1 (April release) or 2 (October release). Ignored if year is 0 and thus points to current release.
#' @param as how to output the result: \code{md3}: as md3 object with full metadata, \code{2d}: as a data.table with periods as column names, \code{1d} resp. \code{data.table}: as fully stacked data.table, \code{array}: as multi-dim array, \code{zoo}: as \code{\link[zoo]{zooreg}} time series object.
#' @param drop if TRUE, drop any singleton dimensions (see also drop.md3)
#' @param ccode If not \code{ccode==NULL}, then the function attempts to convert to ccode destination such as 'iso2c', 'EC', 'iso3c'. See \code{\link[MDcountrycode]{ccode}} for permissible values.  \code{\link[MDcountrycode]{defaultcountrycode}} for defining this value as a session-wide option.
#' @param startPeriod startyear (1960 if empty)
#' @param endPeriod end year (latest available if left empty)
#' @param \ldots further arguments passed from mds
#' @return an md3 object or other as specified by \code{as}
#' @details This function works by caching IMF WEO vintages on the local drive
#' @seealso \code{\link{mds}} for loading from major data sources
#' @examples
#'
#' mdWEO('CA+US+MX.LP') #Population of Canada, US and Mexico
#' mdWEO('CHN+JPN+KOR.LP') #also works with three-letter codes
#'
#'
#' foo=mdWEO(ccode='iso2m')
#' foo[DE+ES.NGDP.] #German and Spanish  GDP
#'
#'
#' @export
mdWEO = function(code=NULL,year=0,release=0,
                  as=c("md3", "array", "numeric","data.table","zoo","2d","1d"),
                  drop=TRUE,ccode=getOption('defaultcountrycode',NULL),startPeriod,endPeriod,...) {
  tempweo=.mdstats_providers$cachedmd3s(paste0('WEO',year,release))
  if (is.null(tempweo)) tempweo=.mdstats_providers$cachedmd3s(.parseWEO(year=year,release = release),paste0('WEO',year,release))
  if (is.character(code)) {
    if (any(grepl('/',code))) code=gsub('^.*/','',code)
    if (nchar(code)==0) code=character()
  }
  if (!length(code)) {
    if (length(ccode)) { tempweo=.countrycodefixer(tempweo,NULL,'COUNTRY',ccode)}
    return(tempweo)
  }
  if (nchar(gsub('[^\\.]','',code))>2) stop('Could not interpret query code',code)

  temp=trimws(strsplit(paste0(' ',code, ' '),split='\\.')[[1L]]);
  if (any(grepl('\\+',temp[[1L]]))) {
    temp[[1L]]=paste(MDcountrycode::ccode(strsplit(temp[[1L]],split='\\+')[[1L]],c('iso2m','iso3c','iso2c'),'iso3c',leaveifNA = TRUE,warn = FALSE),collapse='+')
    code=paste(temp,collapse = '.')
  }
  if (nchar(gsub('[^\\.]','',code))==1) code=paste0(code,'.')

  mout=MD3:::.md3get(tempweo,code,drop = FALSE)
  if (length(ccode))  mout=.countrycodefixer(mout,NULL,'COUNTRY',ccode)
  if (drop) {mout=MD3:::drop(mout)}
  MD3:::.getas(mout,as)
}






.helpmdWEO = function(query='', pattern = "", dim = NULL, verbose = TRUE, sdmxlike=FALSE, ...) {
  #stop("helpmdAmeco not  ready yet")
  vcode=suppressWarnings(.fixSdmxCode(query))
  vcode=vcode[nchar(vcode)>0]




  if (length(dim)) {
    #its about a dimension:
    if (is.numeric(dim)) dim=c('COUNTRY','SUBJECT')[dim]
    if (grepl('^geo|^cou',tolower(dim))) { dim='COUNTRY'}
    if (grepl('^indic|^varia|^subj',tolower(dim))) { dim='SUBJECT'}
    if (!(dim %in% c('COUNTRY','SUBJECT'))) stop('dimension ',dim,' is not available from IMF WEO')
    thatweo=names(.mdstats_providers$cachedmd3s()); thatweo=thatweo[grepl('WEO',thatweo)][1]; if (anyNA(thatweo)) {thatweo=NULL}
    if (!length(thatweo)) { temp=mdWEO(); thatweo='WEO00' }
    odn=MD3:::.getdimcodes(.mdstats_providers$cachedmd3s(thatweo))
    odn=odn[[dim]]
    #return(invisible(odn[[dim]]))


    if (!nchar(pattern)) {


      cat('Dimension ',dim, 'contains ',NROW(odn),' codes that have data:\n',sep='')
      cat(capture.output(print(head(odn,50))),sep='\n')
      if (NROW(odn)>50) cat('\nand ',NROW(odn)-50,'more ')

      cat('\nRun e.g. helpmdWEO("", dim="',dim,'", pattern="MYSEARCHTERM") resp helpmds("IMFWEO/WEO/", dim="',dim,'", pattern="MYSEARCHTERM") to search the codes and descriptions for dimension ',dim,sep='')
      cat('\nRun e.g. xx=helpmdWEO(dim="',dim,'") to load all codes and descriptions for dimension ',dim,' into variable xx\n',sep='')

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

  if (vcode[1]=='IMFWEO') {vcode=character()}
  if (!length(vcode)) {
    if (sdmxlike) {
      cat('\nIMFWEO has only one dataflow available via mds: "IMFWEO/WEO"\n')
      cat('It may be used like mds("IMFWEO/WEO/FRA.NGDP")\n')
      cat('Use function mdWEO() for more options, in particular to retrieve earlier vintages \n')
    }
    cat('\nWEO has the following dimensions excl TIME:\n')
    cat('COUNTRY, SUBJECT\n')
    cat('A typical request would look like mdWEO("AUT.NGDP")\n')
    cat('Run e.g. helpmdWEO(dim="SUBJECT", pattern="MYSEARCHTERM") to search the codes and descriptions for dimension SUBJECT')
    return(invisible(NULL))
  } else stop('???')
}

