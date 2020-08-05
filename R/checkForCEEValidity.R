checkForCEEValidity <- function(folder,fileHasHeader, de_map, bad_data_values){

  #data elements in file to validate
  data_elements <- read.csv(folder, header = fileHasHeader)
  if(nrow(data_elements) == 0)
    return(NULL)

  data_elements_by_assessment<-split(data_elements, data_elements[,7])
  d = NULL

  for(i in 1:length(data_elements_by_assessment)){
    # if dataElementIdScheme = code or name
    if(length(de_map) < 1){
      #check for presence of any CEEs
      site_CEE <- grep("SIMS.S", data_elements_by_assessment[[i]][,1])
      abovesite_CEE <- grep("SIMS.AS", data_elements_by_assessment[[i]][,1])
      if(length(site_CEE) < 1 && length(abovesite_CEE) < 1){
          d = rbind(d, data.frame('Invalid Data'='No CEEs', value='N/A', Assessment=names(data_elements_by_assessment)[i]))
      }
      else{
        #check for presence of SCORE
        score_DEs <- grep("SCORE", data_elements_by_assessment[[i]][,1])
        if(length(score_DEs) < 1){
          d = rbind(d, data.frame('Invalid Data'='SCORE Missing', value='N/A', Assessment=names(data_elements_by_assessment)[i]))
        }
        #check for validity of values
        else{
          if(!is.null(bad_data_values)){
            assmt <- grep(names(data_elements_by_assessment)[i], bad_data_values[,8])
            if(length(assmt) > 0){
              for(row in 1:length(bad_data_values)){
                if(bad_data_values[row,8] %in% c(names(data_elements_by_assessment)[i])){
                  d = rbind(d, data.frame('Invalid Data'= 'Wrong data type', value=bad_data_values[row,6], Assessment=names(data_elements_by_assessment)[i]))
                  dataElement <- de_map[[bad_data_values[row,2]]]
                  period <- bad_data_values[row,2]
                  orgUnit <- bad_data_values[row,3]
                  coc <- bad_data_values[row,4]
                  aoc <- bad_data_values[row,5]
                  comment <- bad_data_values[row,8]
                  }
              }
              # check whether all remaining CEEs are valid or remove assessment
              data_elements_by_assessment2<-split(data_elements, data_elements[,7])
              if(!is.null(data_elements_by_assessment2[[i]])){
                #check for presence of any CEEs
                site_CEE <- grep("SIMS.S", data_elements_by_assessment2[[i]][,1])
                abovesite_CEE <- grep("SIMS.AS", data_elements_by_assessment2[[i]][,1])
                if(length(site_CEE) < 1 && length(abovesite_CEE) < 1){
                  d = rbind(d, data.frame('Invalid Data'='No CEEs', value='N/A', Assessment=names(data_elements_by_assessment2)[i]))
                }
                else{
                  #check for presence of SCORE
                  score_DEs <- grep("SCORE", data_elements_by_assessment2[[i]][,1])
                  if(length(score_DEs) < 1){
                    d = rbind(d, data.frame('Invalid Data'='SCORE Missing', value='N/A', Assessment=names(data_elements_by_assessment2)[i]))
                  }
              }
            }
          }
        }
      }
      }
    }
    # if dataElementIdScheme = id
    else{
      #check for presence of any CEEs
      index <- 1
      CEEs <- vector(mode = "list")
      for(j in data_elements_by_assessment[[i]][,1]){
        if(startsWith(de_map[[j]],'SIMS.S') || startsWith(de_map[[j]],'SIMS.AS')){
          CEEs[[index]] <- j
          index <- index + 1
        }
      }
      if(length(CEEs) < 1){
        d = rbind(d, data.frame('Invalid Data'='No CEEs', value='N/A', Assessment=names(data_elements_by_assessment)[i]))
        #remove assessment
        if(remove)
          data_elements <- data_elements[data_elements[,7] != names(data_elements_by_assessment)[i],]
      }
      else{
      #check for presence of SCORE
      index <- 1
      score_DEs <- vector(mode = "list")
      for(j in data_elements_by_assessment[[i]][,1]){
        if(endsWith(de_map[[j]],'SCORE')){
          score_DEs[[index]] <- j
          index <- index + 1
        }
      }
    }
    if(length(score_DEs) < 1){
      d = rbind(d, data.frame('Invalid Data'='SCORE Missing', value='N/A', Assessment=names(data_elements_by_assessment)[i]))
      #remove assessment
      if(remove)
        data_elements <- data_elements[data_elements[,7] != names(data_elements_by_assessment)[i],]
    }
    #check for validity of values
    else{
      if(!is.null(bad_data_values)){
        assmt <- grep(names(data_elements_by_assessment)[i], bad_data_values[,8])
        if(length(assmt) > 0){
          for(row in 1:length(bad_data_values)){
            if(bad_data_values[row,8] %in% c(names(data_elements_by_assessment)[i])){
              d = rbind(d, data.frame('Invalid Data'='Wrong data type', value=bad_data_values[row,6], Assessment=names(data_elements_by_assessment)[i]))
              dataElement <- bad_data_values[row,2]
              period <- bad_data_values[row,2]
              orgUnit <- bad_data_values[row,3]
              coc <- bad_data_values[row,4]
              aoc < bad_data_values[row,5]
              comment <- bad_data_values[row,8]
              #remove CEE
              if(remove)
                data_elements <- data_elements[!(data_elements[,1] == dataElement && data_elements[,2] == period && data_elements[,3] == orgUnit && data_elements[,4] == coc && data_elements[,5] == aoc && data_elements[,7] == comment),]
              }
          }
          # check whether all remaining CEEs are valid or remove assessment
          data_elements_by_assessment2<-split(data_elements, data_elements[,7])
          if(!is.null(data_elements_by_assessment2[[i]])){
            #check for presence of any CEEs
            index <- 1
            CEEs <- vector(mode = "list")
            for(j in data_elements_by_assessment2[[i]][,1]){
              if(startsWith(de_map[[j]],'SIMS.S') || startsWith(de_map[[j]],'SIMS.AS')){
                CEEs[[index]] <- j
                index <- index + 1
              }
            }
            if(length(CEEs) < 1){
              d = rbind(d, data.frame('Invalid Data'='No CEEs', value='N/A', Assessment=names(data_elements_by_assessment2)[i]))
              #remove assessment
              if(remove)
                data_elements <- data_elements[data_elements[,7] != names(data_elements_by_assessment2)[i],]
            }
            else{
              #check for presence of SCORE
              index <- 1
              score_DEs <- vector(mode = "list")
              for(j in data_elements_by_assessment2[[i]][,1]){
                if(endsWith(de_map[[j]],'SCORE')){
                  score_DEs[[index]] <- j
                  index <- index + 1
                }
              }
            }
            if(length(score_DEs) < 1){
              d = rbind(d, data.frame('Invalid Data'='SCORE Missing', value='N/A', Assessment=names(data_elements_by_assessment2)[i]))
              #remove assessment
              if(remove)
                data_elements <- data_elements[data_elements[,7] != names(data_elements_by_assessment2)[i],]
            }
          }
        }
      }
    }
    }
  }
  if(remove)
    write.csv(data_elements, paste0(folder, "_assessmentRemoved.csv"), row.names=FALSE, na="")
  return(d)

}
