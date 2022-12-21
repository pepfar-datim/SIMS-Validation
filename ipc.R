require(devtools)
install_github("pepfar-datim/SIMS-Validation", force=TRUE)
require(SIMS4Validation)

secrets <- "path to credentials file"

# folder where file xzx is located, and where output files will be written to
out_dir <- "path to location of file"

# name of the file to validate
filename <- "file.csv"

# type of the file (json, xml, or csv)
file_type <- "csv"

# identifier scheme used in the input file
#mechanism identifier: id or code
idScheme <- "code"
#id, code or name
dataElementIdScheme <- "name"
#id or code
orgUnitIdScheme <-"id"

# calendar period (quarter) covered by the input file in YYYYQN format, e.g. 2019Q3 for July-September 2019
isoPeriod <- "2022Q4"

# whether the input file has the header as the first line
fileHasHeader <- TRUE

dataSets <- c("dT9xKGbcXLK")
#beginings of all coversheet and IPC data elements
ipc_all <- c("SIMS.CS","SIMS.S_01_06","SIMS.S_01_07","SIMS.S_01_08","SIMS.S_10_02","SIMS.S_01_27","SIMS.S_01_28","SIMS.S_01_29","SIMS.S_01_30")
#beginings of all IPC data elements
ipc_cees <- c("SIMS.S_01_06","SIMS.S_01_07","SIMS.S_01_08","SIMS.S_10_02","SIMS.S_01_27","SIMS.S_01_28","SIMS.S_01_29","SIMS.S_01_30")

path <- paste0(out_dir, filename)

data_elements <- read.csv(path, header = fileHasHeader)


#if dataElementIdScheme is id, construct map of data element ID and name
de_map = vector(mode = "list")
if(dataElementIdScheme %in% c("id")){
  d2s <- datimutils::loginToDATIM(config_path = secrets)
  assign("d2_default_session", d2s, parent.frame())
  
  data_elements <- read.csv(path, header = fileHasHeader)
  distinct_dataElements <- data_elements[!duplicated(data_elements[,1]),]
  for(row in 1:length(distinct_dataElements[,1])) {
    url <- paste0(d2_default_session$base_url, "api/",
                  "dataElements/",distinct_dataElements[row,1],".json?fields=name&filter=name:like:SIMS.C&filter=name:like:SIMS.S&paging=false&rootJunction=OR")
    r <- httr::GET(url, httr::timeout(60), handle = d2_default_session$handle)
    r <- httr::content(r, "text")
    de <- jsonlite::fromJSON(r, flatten = TRUE)$name
    key <- paste0("",distinct_dataElements[row,1])
    if(is.null(de_map[[key]])){
      de_map[[key]] <- de
    }
  }
  for(row in 1:nrow(data_elements)){
    data_elements[row,1] <- de_map[[data_elements[row,1]]]
  }
}
#remove all rows with data elements that are neither coversheet nor ipc
data_elements2 <- data_elements[grepl(paste(ipc_all, collapse = "|"), data_elements[,1]),]

#grouping data by assessment
data_elements_by_assessment<-split(data_elements2, data_elements2[,7])

assessments_to_delete = vector(mode = "list")
# If for a given assessment, there are no ipc data elements, add that assessments to assessments_to_delete
for(i in 1:length(data_elements_by_assessment)){
  keep <- FALSE
  for(j in 1:length(data_elements_by_assessment[[i]][,1])){
    if(length(de_map) > 0){
      data_elements_by_assessment[[i]][j,1] <- de_map[[data_elements_by_assessment[[i]][j,1]]]
    }
    if(substr(data_elements_by_assessment[[i]][j,1], 1, 12) %in% ipc_cees){
      keep <- TRUE
      break
    }
  }
  if(!keep){
    assessments_to_delete <- append(assessments_to_delete, data_elements_by_assessment[[i]][,7])
  }
}
#delete rows that have assessments in assessments_to_delete
if(length(assessments_to_delete) > 0){
  data_elements2 <- data_elements2[!grepl(paste(assessments_to_delete, collapse = "|"), data_elements2[,7]),]
}
#write data to a new file
if(!is.null(data_elements2) && nrow(data_elements2) != 0) {
  write.csv(data_elements2,file=paste0(out_dir, filename, "_ipc.csv"),row.names=FALSE)
}
#validate the new file
SIMS4Validation::SIMSValidationScript(out_dir,paste0(filename, "_ipc.csv"),file_type,idScheme,dataElementIdScheme,orgUnitIdScheme,isoPeriod,fileHasHeader,secrets, dataSets)