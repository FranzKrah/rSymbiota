#' "records" class
#' An S4 Class to represent query result from the function \link{symbiota}
#' \code{records} holds a records table together with the query meta data and recommended citation
#' @slot nr.records A numeric giving the number of records retrieved
#' @slot citation A character string with the recommended citation from the website
#' @slot query A list of the user arguments used
#' @slot records A data.frame with the query records results
#' @slot db A character string specifying the database
#' @examples showClass("records")
#' @author Franz-Sebastian Krah

setClass("records",
         representation = list(
           nr.records = "numeric",
           citation = "character",
           query = "list",
           records = "data.frame",
           db = "character")
)
