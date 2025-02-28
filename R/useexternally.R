




.writescript = function(defaultoutpath='C:/Users/Public/statautils/queryresult.dta',interact=interactive()) {


  .rspath=gsub('library\\\\base$','bin\\\\Rscript',normalizePath(system.file()[1],winslash = '\\'))
  .basicscript = "
  #save this in C:/Users/Public/statautils
  defaultoutpath='C:/Users/Public/statautils/queryresult.dta'
  args = commandArgs(trailingOnly=TRUE)


  suppressWarnings(require(MDecfin,quietly = TRUE))
  #MDcountrycode::defaultcountrycode('EC') # uncomment this if you want all country codes to be the same

  loadExternally(as.list(args))

  #examples to be run from shell:
  #C:/Users/Public/statautils/loadmd.cmd ECB/EXR/A.RON+SEK.EUR.SP00.A
  #C:/Users/Public/statautils/loadmd.cmd ECB/EXR/A.RON+SEK.EUR.SP00.A 0
  #C:/Users/Public/statautils/loadmd.cmd ECB/EXR/A.JPY+PLN.EUR.SP00.A 1 C:/Users/Public/statautils/test.csv
  #C:/Users/Public/statautils/loadmd.cmd ESTAT/prc_hpi_a/A.TOTAL..BE 0 C:/Users/Public/statautils/test.csv
  #C:/Users/Public/statautils/loadmd.cmd AMECO/C/BE+AT.1_0_0_0_UVGD+1_0_0_0_OVGD
  #C:/Users/Public/statautils/loadmd.cmd nomics:ECB/EXR/A.JPY+USD.EUR.SP00.E+A
  #C:/Users/Public/statautils/loadmd.cmd ECB/QSA/Q.N..W0.S1M.S1.N.L.LE.F3+F4.T._Z.XDC._T.S.V.N._T
  #C:/Users/Public/statautils/loadmd.cmd ameco:AT+BE.1.0.310+0.0.UBCA+UVGD
  #
  #or without the cmd:
  #Rscript --arch x64 --no-save C:/Users/Public/statautils/loadMD.R AMECO/C/BE+AT.1_0_0_0_UVGD+1_0_0_0_OVGD
  #Everything until --no-save denotes the call to R. Eveything after that are the 1 to 3 arguments
  #Argument 1 (mandatory): code to be loaded via mds, like ESTAT/prc_hpi_a/A.TOTAL..BE. Can be prefixed with nomics:, ameco: or imfweo: to acess specfic functions other than mds
  #Argument 2 (optional): Boolean whether to 1 (default): make the final file a country panel, with geo and time dimensions in the first column
  #   and all other dimensions forming separate variables. Or 0: make the result fully stacked, with n identifieer columns and one obs_value column
  #Argument 3 (optional): the filepath where to save the result. Must be either .csv or .dta (Stata) file. Default is defaultoutpath
 "

  pkgxst=installed.packages()
  hasMDecfin=TRUE
  if (!'MDstats' %in% pkgxst) {stop('needs package MDstats installed.')}
  if (!'MDecfin' %in% pkgxst) {
    hasMDecfin=FALSE
    .basicscript=gsub('require\\(MDecfin','require(MDstats',.basicscript)
  }



  if (interactive() & missing(defaultoutpath)) {
    readline(paste0('This creates a script that allows to call ',ifelse(hasMDecfin,'MDecfin','MDstats'),' from external appliations such as Stata\n',
             'To cancel, hit Esc. To continue press Enter'))
    ures= substr(trimws(tolower(readline(paste0('If want to this to produce .dta files to be loaded from Stata, type "s" now and hit Enter.\n',
                                      'If instead you want the routine to produce CSV files, tyepe any other character and hit enter.')))),0,1)
    if (ures=="") stop('please try again')
    if (ures!="s") {.basicscript=gsub('queryresult\\.dta','queryresult.csv',.basicscript)}
  }





  .basicscript=gsub('#Rscript',gsub('\\\\','\\\\\\\\',paste0('#',.rspath)),.basicscript)
  if (!nchar(defaultoutpath)) return(.basicscript)

  if (defaultoutpath!='C:/Users/Public/statautils/queryresult.dta') {
    defaultoutpath=suppressWarnings(normalizePath(defaultoutpath,winslash = '/',mustWork = FALSE))
  }
    opdir=dirname(defaultoutpath)
    opfile=basename(defaultoutpath)
    if (!dir.exists(opdir)) {if (!dir.create(opdir,recursive = TRUE)) stop('Directory ',opdir,' does not exist or cannot write there')}
    .basicscript =gsub('C:/Users/Public/statautils',opdir,.basicscript)
    .basicscript =gsub('queryresult.dta',opfile,.basicscript)



  if (file.exists(paste0(opdir,'/loadMD.R'))) {cat('Overwriting ',opdir,'/loadMD.R and',opdir,'/loadMD.cmd' , ' \n',sep = "")} else {cat('Creating ',opdir,'/loadMD.R and',opdir,'/loadMD.cmd' , '\n',sep = "")}
  cat(.basicscript,file=paste0(opdir,'/loadMD.R'))
  cat('Check the comments at the end of that file for usage instructions and examples')
  cat(paste0(.rspath," --arch x64 --no-save ",paste0(opdir,'/loadMD.R')," %1 %2 %3"), file=paste0(opdir,'/loadMD.cmd'))

  return(invisible(.basicscript))
}




#' Function and script to make mds callable form other applications, such as Stata
#'
#' This function is meant to be called from other applications via RScript and arguments. If you don't know R, look below under the section Stata Users resp Non-Stata Users
#' @param \dots either a character call (such as "ECB/EXR/A.JPY.EUR.SP00.A") to mds or a related function , or a list containing that call, as well as outpath and ascountrypanel
#' @param outpath Character. the dta or csv file to be created be the function (default C:/Users/Public/statautils/queryresult.dta). If empty, NULL, or empty string, then loadExternally simply returns the object on the R console
#' @param ascountrypanel logical (Default TRUE). If TRUE, then this attempts to reshape the result into a panel form with "variables" being the columns, and countries and TIME being identified in the first column. If \code{FALSE}, then the result is fully stacked, and contains observation attributes.
#' @return the file path to outpath, or a data.table (if outpath=='')
#' @details if left empty, then this function creates the script loadMD.R to be called with R script.
#' The default path for that script is C:/Users/Public/statautils/loadMD.R but than be changed using outpath.
#' (In the latter case like \code{loadExternally(outpath='c:/balbal.R')} can also be used in non-interactive mode)
#'
#' this also creates the wrapper file loadMD.cmd which is easier to handle than using loadMD.R directly
#' You can call it by \code{C:/Users/Public/statutils/loadMD Arg1 Arg2 Arg3}. The first Argument \code{Arg1} is mandatory, whereas  \code{Arg2}  and  \code{Arg3} are optional
#'
#' loadMD accepts the following arguments
#' \itemize{
#' \item{Arg1}{ (mandatory): code to be loaded via mds, like ESTAT/prc_hpi_a/A.TOTAL..BE. Can be prefixed with \code{nomics:}, \code{ameco:} or \code{imfweo}: to access specific functions other than \code{mds}}
#' \item{Arg2}{ (optional): 1 or 0.  Setting this to 1 (default) makes the final file a country panel, with geo and time dimensions in the first column
#'   and all other dimensions forming separate variables. Or 0:make the result fully stacked, with n identifier columns and one \code{obs_value} column}
#' \item{Arg3}{  (optional): the filepath where to save the result. Must be either .csv or .dta (Stata) file. Default is the \code{defaultoutpath} variable defined in loadMD.R}
#' }
#'
#'  Pro-tip: IF you want all country codes to look the same (e.g. all are UK rather than GB or GBR), then uncomment the line with \code{defaultcountrycode} in C:/Users/Public/statautils/loadMD.R
#'  check \code{\link{defaultcountrycode}} for help on that.
#'
#' @section For Stata Users that dont know R:
#'
#'  \itemize{
#'   \item{1)}{ make sure you have internet connection from R. Run e.g. \code{readLines('https://example.com',n = 1)} to check that}
#'   \item{2)}{ make sure you have readstata13 installed. if \code{library(readstata13)} does not work, call \code{install.packages('readstata13')}}
#'   \item{3)}{ run \code{loadExternally()}. Press Enter, and then type \code{s} and Enter}
#'  }
#'
#'  Now from Stata, type e.g.
#'
#'  \code{shell "C:\\Users\\Public\\statautils\\loadmd" ECB/EXR/A.JPY+PLN.EUR.SP00.A}
#'
#'  \code{use "C:\\Users\\Public\\statautils\\queryresult.dta", clear}
#'
#'  or use the second argument to load this  in 'fully stacked' form rather than as a country panel:
#'
#'  \code{shell "C:\\Users\\Public\\statautils\\loadmd" AMECO/C/BE+AT.1_0_0_0_UVGD 0}
#'
#'  \code{use "C:\\Users\\Public\\statautils\\queryresult.dta", clear}
#'
#' See section 'Details' above for explaining those arguments. See \code{\link{helpmds}} for understanding the structure of dataset codes and dimensions
#'
#'
#' @section For non-Stata Users that dont know R:
#'  \itemize{
#'   \item{1)}{ make sure you have internet connection from R. Run e.g. \code{readLines('https://example.com',n = 1)} to check that}
#'   \item{2)}{ run\code{loadExternally()}. Press Enter, and then type \code{a} and Enter}
#'  }
#'  Now from DOS/Shell/Console (Windows+R, then CMD), type e.g.
#'
#'  \code{C:\\Users\\Public\\statautils\\loadmd ECB/EXR/A.JPY+PLN.EUR.SP00.A}
#'
#'  or
#'
#'  \code{C:\\Users\\Public\\statautils\\loadmd AMECO/C/BE+AT.1_0_0_0_UVGD 1 c:/Users/Public/test.csv}
#'  to create a CSV file called \code{test.csv}
#'
#'  See section 'Details' above for explaining those arguments. See \code{\link{helpmds}} for understanding the structure of dataset codes and dimensions
#'
#' @seealso  \code{\link{helpmds}} if something does not work. e.g. \code{helpmds('ESTAT/prc_hpi_a/A.TOTAL..AT.')} to find out why that code does not work
#'  \code{\link{mds}} for loading from major data sources.
#'
#' @examples
#' #creates the script loadMD.R:
#' \dontrun{loadExternally()}
#'
#' loadExternally('nomics:ECB/EXR/A.JPY+USD.EUR.SP00.E+A',outpath='')
#' loadExternally('ECB/EXR/A.JPY+PLN+HUF.EUR.SP00.A+E',outpath='')
#' loadExternally('ECB/EXR/A.JPY+PLN+HUF.EUR.SP00.A+E',outpath='', ascountrypanel=FALSE)
#'
#' loadExternally('ESTAT/prc_hpi_a/A.TOTAL..BE')
#' loadExternally(list('ESTAT/prc_hpi_a/A.TOTAL..BE','',FALSE))
#' loadExternally('ECB/QSA/Q.N..W0.S1M.S1.N.L.LE.F3+F4.T._Z.XDC._T.S.V.N._T')
#'
#'
#'
#'
#'
#' @export
loadExternally = function(...,outpath='C:/Users/Public/statautils/queryresult.dta',ascountrypanel=TRUE) {



  #args=c('ameco:AT+BE.1.0.310+0.0.UBCA+UVGD') #to test
  #args=c('ECB/EXR/A.JPY+USD.EUR.SP00.E+A')
  #args=c('nomics:ECB/EXR/A.JPY+USD.EUR.SP00.E+A') #to test
  #args='ESTAT/prc_hpi_a/A.TOTAL..BE'
  #args='ECB/QSA/Q.N..W0.S1M.S1.N.L.LE.F3+F4.T._Z.XDC._T.S.V.N._T' #to test
  #args=c('ECB/QSA/Q.N..W0.S1M.S1.N.L.F.F3+F4.T._Z.XDC._T.S.V.N._T','C:/Users/zeugnst/Documents/eutemp')
  args=MD3:::.dotsaslist(...)
  if (!length(args))
    if ((interactive()|| !missing(outpath)) ) {
    if (!missing(outpath)) return(.writescript(outpath))
    return(.writescript())
  }
  if (is.list(args[[1L]])) { args=args[[1L]]}
  if (!require(data.table,quietly = TRUE)) stop('you need to install package data.table')
  if (!suppressWarnings(require(MDecfin,quietly = TRUE))) if (!require(MDstats,quietly = TRUE)) stop('you need to install package MDstats')
  query=trimws(as.character(args[1]))

  if (!grepl(':',args[1])) {myf='mds'; query=query} else {

    rfun=tolower(gsub(':.*$','',query))
    query=gsub('^.*:','',query)
    if (grepl('ameco',rfun)) { myf='iAmeco'} else
    if (grepl('nomics',rfun)) { myf='Nomics' } else
    if (grepl('weo',rfun)) { myf='mdWEO' } else
    { myf='mds'}
  }

  if (length(args)>1) {
    ascountrypanel=as.logical(as.numeric(trimws(args[[2]])))
  }

  if (length(args)>2) {
    outpath=trimws(as.character(args[[3]]))
    outpath=suppressWarnings(normalizePath(outpath,winslash = '/'))
  } else if (exists('defaultoutpath')) {
    outpath=defaultoutpath
  }


  if (!length(outpath)) {outpath=''} else {outpath=trimws(outpath)}
  if (nchar(outpath)) {
    if (!dir.exists(dirname(outpath))) {dir.create(dirname(outpath))  }
    if (file.exists(outpath)) { if (!file.remove(outpath)) stop('Problem accessing file ',outpath)  }
    asstata=FALSE
    if (grepl('\\.dta$',outpath)) {
      if (!require(readstata13,quietly = TRUE)) stop('you need to install package readstata13')
      asstata=TRUE
    }
  }

  result=try(get(myf)(query,as='1d',drop=FALSE),silent=TRUE)
  if (is(result,'try-error')) stop('query ',query,' did not work: ',attr(result,"condition"))
  if (!is.data.table(result)) result=as.data.table(result)

  if (ascountrypanel) {

    ngeodim=MDstats:::.findgeodim(colnames(result));
    if ('TIME' %in% colnames(result)) ntimedim='TIME' else ntimedim=character()
    sreshp=paste0(paste(c(ngeodim,ntimedim),collapse='+'),'~...')
    if (nchar(sreshp)<5) {
      dout=result
      names(dout)[grep('obs_value',names(dout))] = query
    } else {
      dout=data.table::dcast(unflag(result,asDT=TRUE, ignoreNA = TRUE),sreshp,value.var=colnames(result)[grep('obs_value',colnames(result))])
      emptycols=apply(dout,2,function(x) all(is.na(x)))
      if (any(emptycols)) dout=dout[,!emptycols,with=FALSE]
    }
  } else {
    dout=result
  }

  if ('TIME' %in% colnames(result)) ntimedim='TIME' else ntimedim=character()
  if (length(ntimedim)) { dout[['TIME']] = as.character(dout[['TIME']])}


  if (outpath=='') { return(dout)}
  if (!asstata) {
    fwrite(dout,file = outpath)
    message('saved in ',outpath)
    return(invisible(normalizePath(outpath)))
  }
  readstata13::save.dta13(as.data.frame(dout),file = outpath)

  message('saved in ',outpath)
  return(invisible(normalizePath(outpath)))
  #run this from shell:
  #message(paste0(gsub('/library','/bin/Rscript',.libPaths()[2]),''),' --arch x64 --no-save c:/Users/Public/statautils/loadMD.R AMECO/C/BE+AT.1_0_0_0_UVGD+1_0_0_0_OVGD')

  #######################################
  #to set up, and make sure .connect2internet works and get all relevant packages installed in R. use this commands
  #.connect2internet('YOURPROXYPWASSWORD')
  #source('https://s-ecfin-web.net1.cec.eu.int/directorates/db/u1/R/routines/installMDecfin.txt')
}
