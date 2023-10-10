

#' Table with basic information about SMDX providers
#'
#' to be expanded later
#'
#' @docType data
#'
#' @usage data(providertable)
#'
#' @format An object of class \code{"data.frame"}.
#'
#'
#' @source \href{http://example.com}{example},#'
#'
#' @examples
#' data(providertable)
#' @export
"providertable"


load('data/providertable.rda')

# providertablepath=dir(paste0(libname,'/',pkgname),pattern = 'providers.csv',recursive = TRUE,full.names = TRUE)[1L]
# providertable=utils::read.csv(providertablepath,stringsAsFactors = FALSE,header = TRUE, na.strings='')
# rownames(providertable) = providertable[[1]]
# colnames(providertable)[[1]]='AgencyID'; rownames(providertable)=providertable[[1]]
#assign('providertable', providertable, envir = topenv())

#assign('.mdstats_providers', .mdstats_providerscreate(providertable), envir = topenv())



.mdstats_providers = .mdstats_providerscreate(providertable)
