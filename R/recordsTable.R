#' Extract records table from class \code{records}
#' @param object object of class \code{records}
#' @aliases recordsTable
#' @include records-class.R
#' @return data.frame
#' @author Franz-Sebastian Krah
#' @export

setMethod("recordsTable", signature = "records",
          function(object) {
            object@records
          })
