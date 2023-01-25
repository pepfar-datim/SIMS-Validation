checkCoverSheetCompleteness <- function(folder,fileHasHeader,de_map,d2_default_session){
  #get coversheet data elements
  url <- paste0(d2_default_session$base_url, "api/dataElements.json?fields=code&filter=code:ilike:SIMS.CS&filter=dataSetElements.dataSet.code:like:SIMS4_&paging=false")
  r <- httr::GET(url, httr::timeout(60), handle = d2_default_session$handle)
  r <- httr::content(r, "text")
  data_dictionary_CS_data_elements <- as.list(jsonlite::fromJSON(r, flatten = TRUE)$dataElements$code)
  optional_CS <- c("SIMS.CS_KP","SIMS.CS_ASSR_TeamLd","SIMS.CS_ASMT_PT_NAME")
  
  # remove optional cs from list of cs
  data_dictionary_CS_data_elements <- data_dictionary_CS_data_elements[!(data_dictionary_CS_data_elements %in% optional_CS)]
  #data elements in file to validate
  #data_elements <- read.csv(folder, header = fileHasHeader)[ ,1:1]
  data_elements <- read.csv(folder, header = fileHasHeader)
  #data_elements <- dplyr::select(data_elements,1,6)

  data_elements_by_assessment<-split(data_elements, data_elements[,7])
  d = NULL


  for(i in 1:length(data_elements_by_assessment)){
    #list_of_CS <- vector("list", length(data_dictionary_CS_data_elements))
    list_of_CS <- matrix(,length(data_dictionary_CS_data_elements) , ncol = 2)
    index <- 1
    for(j in 1:length(data_elements_by_assessment[[i]][,1])){
      if(length(de_map) > 0){
        data_elements_by_assessment[[i]][j,1] <- de_map[[data_elements_by_assessment[[i]][j,1]]]
      }
      if(startsWith(data_elements_by_assessment[[i]][j,1], 'SIMS.CS') && data_elements_by_assessment[[i]][j,1] %in% data_dictionary_CS_data_elements) {
        list_of_CS[index,1] <- data_elements_by_assessment[[i]][j,1]
        list_of_CS[index,2] <- data_elements_by_assessment[[i]][j,6]
        index <- index + 1
      }
    }
    list_of_CS <- list_of_CS[rowSums(is.na(list_of_CS)) == 0,]
    #	If assessment type is missing, cover sheet is incomplete
    if(!('SIMS.CS_ASMT_TYPE' %in% list_of_CS[,1])){
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
            url <- paste0(d2_default_session$base_url, "api/", datimvalidation::api_version(),
                          "/organisationUnits/",'orgunit'=data_elements_by_assessment[[i]][j,3],".json?fields=ancestors[name],name")
            r <- httr::GET(url, httr::timeout(60), handle = d2_default_session$handle)
            r <- httr::content(r, "text")
            ou <- jsonlite::fromJSON(r, flatten = TRUE)$ancestors$name[3]
            if(is.null(ou) || is.na(ou)){
              ou <- jsonlite::fromJSON(r, flatten = TRUE)$name
            }
            ###
            for(k in data_dictionary_CS_data_elements){
              if(!startsWith(k, 'SIMS.CS_ASMT_REASON')){
                if(!(k %in% list_of_CS[,1])){
                  d = rbind(d, data.frame('Missing CS Data Elements'=k, Assessment=names(data_elements_by_assessment)[i], ou, 'Type'='Comprehensive'))
                }
              }
            }

            #if list doesn't have at least one reason
            #if(!(list_of_CS %like% 'SIMS.CS_ASMT_REASON%')){
            # d = rbind(d, data.frame('Missing CS Data Elements'=k, Assessment=names(data_elements_by_assessment)[i]))
            # }
            sub_list <- grep("REASON", list_of_CS[,1])
            if(length(sub_list) < 1){
              d = rbind(d, data.frame('Missing CS Data Elements'='SIMS.CS_ASMT_REASON*', Assessment=names(data_elements_by_assessment)[i], ou, 'Type'='Comprehensive'))
            }

        }
        #Followup assessment
        else if(data_elements_by_assessment[[i]][j,6] %in% c('2')){
          ###
          url <- paste0(d2_default_session$base_url, "api/", api_version(),
                        "/organisationUnits/",'orgunit'=data_elements_by_assessment[[i]][j,3],".json?fields=ancestors[name],name")
          r <- httr::GET(url, httr::timeout(60), handle = d2_default_session$handle)
          r <- httr::content(r, "text")
          ou <- jsonlite::fromJSON(r, flatten = TRUE)$ancestors$name[3]
          if(is.na(ou)){
            ou <- jsonlite::fromJSON(r, flatten = TRUE)$name
          }
          ###
          for(k in data_dictionary_CS_data_elements){
            if(!startsWith(k, 'SIMS.CS_ASMT_REASON')){
              if(!(k %in% list_of_CS[,1])){
                d = rbind(d, data.frame('Missing CS Data Elements'=k, Assessment=names(data_elements_by_assessment)[i], ou, 'Type'='Follow-up'))
              }
            }
          }

          #if list has at least one reason
          sub_list <- grep("REASON", list_of_CS[,1])
          if(length(sub_list) > 0){
            for(l in 1:length(list_of_CS[,1])){
              if(grepl("REASON", list_of_CS[l,1], fixed = TRUE) && list_of_CS[l,2] %in% c(TRUE)){
                d = rbind(d, data.frame('Missing CS Data Elements'=list_of_CS[l,1], Assessment=names(data_elements_by_assessment)[i], ou, 'Type'='Follow-up'))
              }
            }
          }
         }
        }
      }

      }
  }

  return (d)
}
