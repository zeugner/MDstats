#' MDstats: A package providing for loading from SDMX and other macro data sources for multi-dimensional data manipulation
#'
#'
#'
#' @section MDstats functionality:
#' Use function \code{mds} and function \code{helpmds} to access all
#'
#' @section Further work:
#'
#'
#' This package is still under development; there are certainly some bugs lurking around
#' Build date: 2024-08-08
#'
#'
#' Conversion to and from \code{data.table}, tibble, and \code{pdata.frame} is to be added
#'
#' @section Author:
#' Stefan Zeugner
#
#' @import data.table
#' @import utils
#' @import bit64
#' @import XML
#' @import rsdmx
#' @import MDcountrycode
#' @import MD3
#' @name MDstats
NULL

#.onLoad = function (libname, pkgname) {
  #require(data.table); require(XML); require(MD3)
  #message(libname,pkgname)

  #warning('provtbl ',exists('providertable', envir = topenv()))
 # utils::data('providertable', envir=topenv())
  #assign('providertable',  envir = topenv())

  # assign('.mdstats_providers', .mdstats_providerscreate(providertable), envir = topenv())

  # .rsdmxfixer()

#}


.onLoad = function (libname, pkgname) {
  requiresilent=function(...) {
   loadres=suppressWarnings(suppressMessages(suppressPackageStartupMessages(require(...,quietly = TRUE))))
   if (!loadres) {suppressPackageStartupMessages(require(...,quietly = FALSE))}
   return(invisible(loadres))
  }

  requiresilent(data.table);  requiresilent(MDcountrycode); requiresilent(MD3); requiresilent(MDstats)
}



#require(data.table);  require(MDcountrycode); suppressMessages(require(MD3,quietly = TRUE))

.onLoad = function (libname, pkgname) {
  requiresilent=function(...) {
    suppressWarnings(suppressMessages(suppressPackageStartupMessages(require(...))))
  }

  requiresilent(data.table);  requiresilent(MDcountrycode); requiresilent(MD3);

}
