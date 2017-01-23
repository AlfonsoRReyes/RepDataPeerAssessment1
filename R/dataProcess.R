#' Downloads a zip file, unpacks it on selected folder
#'
#' By default, if not indicated, the file will download in ./data.
#'
#' @param fileUrl the web link to the file
#' @param outDir the folder where the file will be placed
#' @return None
#'
#' @importFrom utils download.file unzip
#' @export
#'
#'
downloadZip <- function(fileUrl, outDir="./data") {
  temp <- tempfile()
  download.file(fileUrl, temp, mode = "wb")
  unzip(temp, exdir = outDir)
}

