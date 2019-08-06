#' Start Docker
#' @param verbose logical
#' @param wait Waiting time for system call to finish
#' @import sys
#' @details Docker available for download at: https://www.docker.com
#' @examples
#' \dontrun{
#' ## Usually these functions are only used internally.
#' ## However, if there are errors in the function leading to problems running 'symbiota' again
#' ## it might be neccessary to stop_docker.
#' start_docker() # starts docker
#' stop_docker() # stops docker
#' }
#' @export

start_docker <- function(verbose = TRUE, wait = 2){

  out <- exec_internal("docker", args = c("pull", "selenium/standalone-chrome"))
  Sys.sleep(wait)

  tmp <- tempdir()
  std.out <- paste0(tmp, "/out.txt")
  std.err <- paste0(tmp, "/err.txt")
  out <- exec_background("docker", args = c("run", "-d", "-p", "4445:4444", "selenium/standalone-chrome"),
                         std_out = std.out, std_err = std.err)

  Sys.sleep(wait)

  err <- readLines(std.err)
  err.le <- length(grep("Error", err))
  noerr <- length(grep("already", err))

  if(out != 0){
    if(err.le == 1 & noerr == 1)
    {
      message("Port is allocated\n")
    }else{
      stop("Error")
    }
  }
  unlink(std.out)
  unlink(std.err)
}

#' Stop Docker
#' @param sleep waiting time for system call to finish
#' @details This should run for Unix platforms (e.g., Mac) and Windows. Docker available for download at: https://www.docker.com
#' @export
stop_docker <- function(sleep = 2){

  out <- exec_internal("docker", args = c("ps", "-q"))

  stdo <- tempfile()
  out <- exec_wait("docker", "ps", std_out = stdo)
  out <- readLines(stdo)
  out[2] <- gsub("\\s+", " ", out[2])
  out[2] <- stringr::str_split(out[[2]], "\\s")
  nam <- out[[2]][length(out[[2]])]

  out <- exec_internal("docker", args = c("stop", nam))

  Sys.sleep(sleep)
}


