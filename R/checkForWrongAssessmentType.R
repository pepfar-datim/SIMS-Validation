checkForWrongAssessmentType <- function(folder,fileHasHeader,de_map){
  require(datimvalidation)

  #data elements in file to validate
  data_elements <- read.csv(folder, header = fileHasHeader)
  if(nrow(data_elements) == 0)
    return(NULL)

  data_elements_by_assessment<-split(data_elements, data_elements[,7])
  d = NULL

  for(i in 1:length(data_elements_by_assessment)){
    list_of_S <- vector("list")
    list_of_AS <- vector("list")
    index <- 1


    tool_type <- subset(data_elements_by_assessment[[i]], (data_elements_by_assessment[[i]][,1] %in% c('SIMS.CS_ASMT_TOOL_TYPE')))
    #Site assessment
    if('1' %in% tool_type[,6]){
      for(j in data_elements_by_assessment[[i]][,1]){
        if(length(de_map) < 1){
       if(startsWith( j, 'SIMS.AS')){
        list_of_AS[[index]] <- j
        index <- index + 1
       }
        }
        else{
          if(startsWith( de_map[[j]], 'SIMS.AS')){
            list_of_AS[[index]] <- j
            index <- index + 1
          }
        }
      }

      if(length(list_of_AS) > 0){
        for(j in list_of_AS){
          d = rbind(d, data.frame('CEE of wrong tool type'=j, Assessment=names(data_elements_by_assessment)[i]))
      }
    }
    #Above Site assessment
    if('2' %in% tool_type[,6]){
      for(j in data_elements_by_assessment[[i]][,1]){
        if(length(de_map) < 1){
        if(startsWith( j, 'SIMS.S')) {
          list_of_S[[index]] <- j
          index <- index + 1
        }
        }
        else{
          if(startsWith( de_map[[j]], 'SIMS.S')) {
            list_of_S[[index]] <- j
            index <- index + 1
          }
        }
      }
      if(length(list_of_S) > 0){
        for(j in list_of_S){
          d = rbind(d, data.frame('CEE of wrong tool type'=j, Assessment=names(data_elements_by_assessment)[i]))
      }
    }
  }

  return (d)
    }
  }
}
