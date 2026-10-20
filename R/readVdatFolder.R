#' @title readVdatFolder
#'
#' @description Convert folder of VDAT files into detection df. Wraps glatos::vdat_convert to iterate through folders.
#'
#' @param wd Folder where VDAT files are held
#' @param data_type Full name of data type to be read, e.g., "DET.csv"
#'
#' @return Dataframe with fish detections from VDAT files.
#' @examples readVdatFolder("~/Fathom Connect/2026-08-14", "DET.csv")
#' @export
#' @importFrom glatos "vdat_convert"
#'

readVdatFolder <- function(wd, data_type) {

  if(system.file(package = "glatos") == "") { stop ("This function requires the glatos package. Please install before proceeding.") }

  all_detects <- c()

  glatos::vdat_convert(wd,
                       output_format = "csv.fathom.split",
                       diagn = TRUE,
                       vdat_exe_path = "C:/Program Files/Innovasea/Fathom Connect")

  telem_filestring <- list.files(path = wd, pattern = ".vdat")

  for(i in 1:length(unique(telem_filestring))) {

    newfolder <- paste(sub("\\..*", "", telem_filestring[i]),
                       ".csv-fathom-split", sep = "") ## replace vdat with .csv-fathom-split
    setwd(paste(wd, newfolder, sep = "/"))

    detects <- read.csv(data_type, skip = 2) ## first two rows of VDAT-gen CSVs are not readable
    all_detects <- rbind(all_detects, detects)

  }

  return(all_detects)

}
