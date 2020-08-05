SIMSValidationScript <- function(out_dir,filename,file_type,idScheme,dataElementIdScheme,orgUnitIdScheme,isoPeriod,fileHasHeader,secrets){
require(datimvalidation)

datimvalidation::loadSecrets(secrets)

bad_data_values <- SIMS4Validation::simsValidator(out_dir,filename,file_type,idScheme,dataElementIdScheme,orgUnitIdScheme,isoPeriod,fileHasHeader)

path <- paste0(out_dir, filename)

#if dataElementIdScheme is id, construct map of data element ID and name
de_map = vector(mode = "list")
if(dataElementIdScheme %in% c("id")){
  data_elements <- read.csv(path, header = fileHasHeader)
  distinct_dataElements <- data_elements[!duplicated(data_elements[,1]),]
  for(row in 1:length(distinct_dataElements[,1])) {
    url <- paste0(getOption("baseurl"), "api/",
                  "dataElements/",distinct_dataElements[row,1],".json?fields=name")
    r <- httr::GET(url, httr::timeout(60))
    r <- httr::content(r, "text")
    de <- jsonlite::fromJSON(r, flatten = TRUE)$name
    key <- paste0("",distinct_dataElements[row,1])
    if(is.null(de_map[[key]])){
      de_map[[key]] <- de
    }
  }
}

incomplete_CS <- SIMS4Validation::checkCoverSheetCompleteness(path,fileHasHeader,de_map)
if(!is.null(incomplete_CS) && nrow(incomplete_CS) != 0) {
  write.csv(incomplete_CS,file=paste0(out_dir, filename, "_incomplete_CS.csv"))
}


wrongType <- SIMS4Validation::checkForWrongAssessmentType(path,fileHasHeader,de_map)
if(!is.null(wrongType) && nrow(wrongType) != 0) {
  write.csv(wrongType,file=paste0(out_dir, filename, "_wrongToolType.csv"))
}

if(remove && file.exists(paste0(path, "_assessmentRemoved.csv"))){
  path <- paste0(path, "_assessmentRemoved.csv")
}

inValidCEE <- SIMS4Validation::checkForCEEValidity(path,fileHasHeader,de_map,bad_data_values)
if(!is.null(inValidCEE) && nrow(inValidCEE) != 0) {
  write.csv(inValidCEE,file=paste0(out_dir, filename, "_inValidCEE.csv"))
}
}
