#' @include data.R
#'
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
#' This package is still under development; there are certainly some bugs lurking around
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

#.onLoad = function (libname, pkgname) {
  #require(data.table); require(XML); require(MD3)
  #message(libname,pkgname)

  #warning('provtbl ',exists('providertable', envir = topenv()))
 # utils::data('providertable', envir=topenv())
  #assign('providertable',  envir = topenv())

  # assign('.mdstats_providers', .mdstats_providerscreate(providertable), envir = topenv())

  # .rsdmxfixer()

#}


require(data.table);  require(MDcountrycode); require(MD3)
