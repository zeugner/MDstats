#' MDstats: A package providing for multi-dimensional data manipulation
#'
#'
#'
#' @section MDstats functionality:
#' Blabla
#'
#' @section Further work:
#'
#'
#' This packge is still under development; there are certainly some bugs lurking around
#'
#' Conversion to and from \code{data.table}, tibble, and \code{pdata.frame} is to be added
#'
#' @section Author:
#' Stefan Zeugner ECFIN.B1
#'
#' @import data.table
#' @import bit64
#' @import XML
#' @import rsdmx
#' @import MDcountrycode
#' @import MD3
#' @name MDstats
NULL

.onLoad = function (libname, pkgname) {
  require(data.table); require(XML); require(MD3)
  #message(libname,pkgname)
     providertablepath=dir(getwd(),pattern = 'providers.csv',recursive = TRUE,full.names = TRUE)[1L]
     providertable=utils::read.csv(providertablepath,stringsAsFactors = FALSE,header = TRUE, na.strings='')
     rownames(providertable) = providertable[[1]]

     colnames(providertable)[[1]]='AgencyID'; rownames(providertable)=providertable[[1]]
  assign('providertable', providertable, envir = topenv())
  .rsdmxfixer()
  assign('.mdstats_providers', .mdstats_providerscreate(), envir = topenv())

}

