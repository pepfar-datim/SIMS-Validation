checkCoverSheetCompleteness <- function(folder,fileHasHeader,de_map){
  #get coversheet data elements
  url <- paste0(getOption("baseurl"), "api/dataElements.json?fields=code&filter=code:ilike:SIMS.CS&filter=dataSetElements.dataSet.code:like:SIMS4_1&paging=false")
  r <- httr::GET(url, httr::timeout(60))
  r <- httr::content(r, "text")
  data_dictionary_CS_data_elements <- as.list(jsonlite::fromJSON(r, flatten = TRUE)$dataElements$code)

  #data elements in file to validate
  #data_elements <- read.csv(folder, header = fileHasHeader)[ ,1:1]
  data_elements <- read.csv(folder, header = fileHasHeader)
  #data_elements <- dplyr::select(data_elements,1,6)

  data_elements_by_assessment<-split(data_elements, data_elements[,7])
  d = NULL


  for(i in 1:length(data_elements_by_assessment)){
    list_of_CS <- vector("list", length(data_dictionary_CS_data_elements))
    index <- 1
    for(j in data_elements_by_assessment[[i]][,1]){
      if(length(de_map) > 0){
        j <- de_map[[j]]
      }
      if(startsWith( j, 'SIMS.CS')) {
        list_of_CS[[index]] <- j
        index <- index + 1
      }
    }
    #	If assessment type is missing, cover sheet is incomplete
    if(!('SIMS.CS_ASMT_TYPE' %in% list_of_CS)){
      d = rbind(d, data.frame('Missing CS Data Elements'='SIMS.CS_ASMT_TYPE', Assessment=names(data_elements_by_assessment)[i], '#', '#'))
    }
    else{
      for(j in 1:length(data_elements_by_assessment[[i]][,1])){
        de <- NULL
        if(length(de_map) > 0){
          de <- de_map[[data_elements_by_assessment[[i]][j,1]]]
        }
        else{
          de <- data_elements_by_assessment[[i]][j,1]
        }
        if(de %in% c('SIMS.CS_ASMT_TYPE')){
          #Comprehensive assessment
          if(data_elements_by_assessment[[i]][j,6] %in% c('1')){
            ###
            url <- paste0(getOption("baseurl"), "api/", api_version(),
                          "/organisationUnits/",'orgunit'=data_elements_by_assessment[[i]][j,3],".json?fields=ancestors[name],name")
            r <- httr::GET(url, httr::timeout(60))
            r <- httr::content(r, "text")
            ou <- jsonlite::fromJSON(r, flatten = TRUE)$ancestors$name[3]
            if(is.null(ou) || is.na(ou)){
              ou <- jsonlite::fromJSON(r, flatten = TRUE)$name
            }
            ###
            for(k in data_dictionary_CS_data_elements){
              if(!startsWith(k, 'SIMS.CS_ASMT_REASON')){
                if(!(k %in% list_of_CS)){
                  d = rbind(d, data.frame('Missing CS Data Elements'=k, Assessment=names(data_elements_by_assessment)[i], ou, 'Type'='Comprehensive'))
                }
              }
            }

            #if list doesn't have at least one reason
            #if(!(list_of_CS %like% 'SIMS.CS_ASMT_REASON%')){
            # d = rbind(d, data.frame('Missing CS Data Elements'=k, Assessment=names(data_elements_by_assessment)[i]))
            # }
            sub_list <- grep("REASON", list_of_CS)
            if(length(sub_list) < 1){
              d = rbind(d, data.frame('Missing CS Data Elements'='SIMS.CS_ASMT_REASON*', Assessment=names(data_elements_by_assessment)[i], ou, 'Type'='Comprehensive'))
            }

        }
        #Followup assessment
        else if(data_elements_by_assessment[[i]][j,6] %in% c('2')){
          ###
          url <- paste0(getOption("baseurl"), "api/", api_version(),
                        "/organisationUnits/",'orgunit'=data_elements_by_assessment[[i]][j,3],".json?fields=ancestors[name],name")
          r <- httr::GET(url, httr::timeout(60))
          r <- httr::content(r, "text")
          ou <- jsonlite::fromJSON(r, flatten = TRUE)$ancestors$name[3]
          if(is.na(ou)){
            ou <- jsonlite::fromJSON(r, flatten = TRUE)$name
          }
          ###
          for(k in data_dictionary_CS_data_elements){
            if(!startsWith(k, 'SIMS.CS_ASMT_REASON')){
              if(!(k %in% list_of_CS)){
                d = rbind(d, data.frame('Missing CS Data Elements'=k, Assessment=names(data_elements_by_assessment)[i], ou, 'Type'='Follow-up'))
              }
            }
          }

          #if list has at least one reason
          sub_list <- grep("REASON", list_of_CS)
          if(length(sub_list) > 0){
            d = rbind(d, data.frame('Missing CS Data Elements'='SIMS.CS_ASMT_REASON*', Assessment=names(data_elements_by_assessment)[i], ou, 'Type'='Follow-up'))
          }
         }
        }
      }

      }
  }

  return (d)
}
