#' Create a records objects
#'
#' @param nr.records A numeric giving the number of records retrieved
#' @param citation A character string with the recommended citation from the website
#' @param query A list of the user arguments used
#' @param records A data.frame with the query records results
#' @param db A character string specifying the database
#' @include records-class.R
#' @import methods
#' @examples
#' \dontrun{
#' help("records-class")
#' }
#' @author Franz-Sebastian Krah


"records" <- function(nr.records, citation, query, records, db){

  new("records",
      nr.records = nr.records,
      citation = citation,
      query = query,
      records = records,
      db = db
  )
}

## setMethod: show

setMethod("show", signature = "records",
          function(object){
            cat("Distribution table with", object@nr.records, "records for ", object@query$taxon, "\n")
            cat("Citation: ", object@citation, "\n")
            cat("Extract records using *recordsTable*")
          })

## seGeneric: extracting records table

setGeneric("recordsTable", function(object) {object})
