
simsValidator <-
  function(folder,
           filename,
           file_type,
           idScheme,
           dataElementIdScheme,
           orgUnitIdScheme,
           isoPeriod,
           fileHasHeader) {
    require(datimvalidation)

    dataSets <- c("O392zMXtwar", "rnEToFucnJ9")
    path <- paste0(folder, filename)

    file_summary <- c()
    file_summary["file"] <- filename

    # parse using regular parser, used to identify period shifts and overlapping assessments
    d <- datimvalidation::d2Parser(file = path, type = file_type, dataElementIdScheme = dataElementIdScheme, orgUnitIdScheme = orgUnitIdScheme, idScheme = idScheme, invalidData = TRUE)

    if(any(class(d) == "data.frame")){
      # no issues
    } else {
      print(d)
    }

    #
    # VALIDATION
    #

    #data <- sims2Parser(
    #  filename = "/Users/hchichaybelu/Documents/SIMS4/DoD/2/S_TANZANIA_2019_Q1_20191121.csv",
    #  dataElementIdScheme = "code",
    #  orgUnitIdScheme = "id",
    #  idScheme = "code",
    #  invalidData = TRUE,
    #  hasHeader = TRUE, # The file has header
    #  isoPeriod = "2020Q1" # Period to validate
    #)


    # 1. parse input file

    # parse using SIMS parser - this parser does period shifting of overlapping SIMS assessments
    d2 <- datimvalidation::sims2Parser(file=path, dataElementIdScheme = dataElementIdScheme, orgUnitIdScheme = orgUnitIdScheme, idScheme = idScheme, invalidData=TRUE, hasHeader=fileHasHeader, isoPeriod=isoPeriod)

    file_summary["record count"] = length(d2$comment)
    file_summary["assessment count"] = length(unique(d2$comment))

    #Count of assessments per operating unit (based on the assessment id column);
    #ou_map <- getOrganisationUnitMap()
    #assmt_per_ou = sqldf('select ou_map.ancestors.name as operatingUnit, count(comment) from d2 join ou_map on d2.orgUnit = ou_map.id group by ou_map.ancestors.name')

    assmt_per_ou = sqldf::sqldf('select orgUnit, count(distinct(comment)) from d2 group by orgUnit')
    file_summary["assessment count per operating unit"] = "------"
    ou_map = vector(mode = "list")
    for(col in 1:length(assmt_per_ou$orgUnit)) {
      url <- paste0(getOption("baseurl"), "api/", api_version(),
                    "/organisationUnits/",assmt_per_ou[col,1],".json?fields=ancestors[name],name")
      r <- httr::GET(url, httr::timeout(60))
      r <- httr::content(r, "text")
      ou <- jsonlite::fromJSON(r, flatten = TRUE)$ancestors$name[3]
      if(is.na(ou)){
        ou <- jsonlite::fromJSON(r, flatten = TRUE)$name
      }
      if(is.null(ou_map[[ou]])){
        ou_map[[ou]] <- 0
      }
      ou_map[[ou]] <- ou_map[[ou]] + as.numeric(assmt_per_ou[col,2])
    }
    for(i in 1:length(ou_map)){
      ou <- names(ou_map)[i]
      file_summary[ou] <- ou_map[[ou]]
    }
    #Count of assessments per mechanism (based on the assessment id column);
    #assmt_per_aoc = sqldf('select attributeOptionCombo, count(comment) from d2 group by attributeOptionCombo')
    #file_summary["assessment count per mechanism"] = "------"
    #for(col in 1:length(assmt_per_aoc$attributeOptionCombo)) {
    #  url <- paste0(getOption("baseurl"), "api/", api_version(),
    #                "/categoryOptionCombos/",assmt_per_aoc[col,1],".json?fields=name")
    #    r <- httr::GET(url, httr::timeout(60))
    #    r <- httr::content(r, "text")
    #    aoc <- jsonlite::fromJSON(r, flatten = TRUE)$name
    #  file_summary[aoc] = assmt_per_aoc[col,2]
    #}
    mech_map <- getMechanismsMap()
    assmt_per_aoc = sqldf::sqldf('select mech_map.code as attributeOptionCombo, count(distinct(d2.comment)) from d2 join mech_map on mech_map.id = d2.attributeOptionCombo group by d2.attributeOptionCombo')
    file_summary["assessment count per mechanism"] = "------"
    for(col in 1:length(assmt_per_aoc$attributeOptionCombo)) {
      file_summary[assmt_per_aoc[col,1]] = assmt_per_aoc[col,2]
    }


    #Count of unique assessment id coversheet data element values;
    de_map <- getDataElementMap() # used to produce post-shift duplicates with codes
    assmt_per_unique_cs_de = sqldf::sqldf("select de_map.code as dataElement, count(distinct(d2.value)) from d2 join de_map on de_map.id = d2.dataElement where de_map.code = 'SIMS.CS_ASMT_ID' group by d2.dataElement")
    file_summary["assessment count per unique cs data elements"] = "------"
    for(col in 1:length(assmt_per_unique_cs_de$dataElement)) {
      file_summary[assmt_per_unique_cs_de[col,1]] = assmt_per_unique_cs_de[col,2]
    }

    #Count of assessment id coversheet data element values;
    assmt_per_cs_de = sqldf::sqldf("select de_map.code as dataElement, count(d2.value) from d2 join de_map on de_map.id = d2.dataElement where de_map.code = 'SIMS.CS_ASMT_ID' group by d2.dataElement")
    file_summary["assessment count per cs data elements"] = "------"
    for(col in 1:length(assmt_per_cs_de$dataElement)) {
      file_summary[paste0((assmt_per_cs_de[col,1])," ")] = assmt_per_cs_de[col,2]
    }

    # identify overlapping assessments, and if any write out details
    overlapping_assessment <- sqldf::sqldf('select period, orgUnit, attributeOptionCombo, count(distinct(storedby)) as assessment_count from d group by period, orgUnit, attributeOptionCombo having count(distinct(storedby)) > 1')
    if(nrow(overlapping_assessment) != 0) {
      write.csv(overlapping_assessment,file=paste0(folder, filename, "_overlapping_assessment.csv"))
      overlapping_assessment_list <- sqldf::sqldf('select distinct d.period, d.orgUnit, d.attributeOptionCombo, d.storedby from d join overlapping_assessment o on d.period=o.period and d.orgUnit=o.orgUnit and d.attributeOptionCombo = o.attributeOptionCombo')
      write.csv(overlapping_assessment_list,file=paste0(folder, filename, "_overlapping_assessment_list.csv"))
    }
    file_summary["overlapping PE/OU/IM count"] = length(overlapping_assessment$period)

    # identify period shifts resulting from shifting assessments
    d_unique = sqldf::sqldf('select period, storedby from d group by period, storedby')
    d2_unique = sqldf::sqldf('select period, comment from d2 group by period, comment')
    shifts_made = sqldf::sqldf('select comment as assessment, d_unique.period as old_period, d2_unique.period as new_period from d_unique join d2_unique on d_unique.storedby = d2_unique.comment where d_unique.period != d2_unique.period order by old_period')
    if(nrow(shifts_made) != 0) write.csv(shifts_made,file=paste0(folder, filename, "_shifts_made.csv"))
    file_summary["shifted_assessment_count"] = nrow(shifts_made)

    # identify any exact duplicates after period shifting
    post_shift_duplicates <- getExactDuplicates(d2)
    post_shift_duplicates_w_code <- sqldf::sqldf('select de_map.code, post_shift_duplicates.* from  post_shift_duplicates left join de_map on de_map.id = post_shift_duplicates.dataElement order by dataElement, period, orgUnit, attributeOptionCombo')
    if(nrow(post_shift_duplicates_w_code) != 0) write.csv(post_shift_duplicates_w_code,file=paste0(folder, filename, "_post_shift_duplicates.csv"))
    file_summary["post shift duplicate count"] = length(post_shift_duplicates_w_code$comment)

    # 2. verify mechanism validity
    mechs <- checkMechanismValidity(d2)
    if(any(class(mechs) == "data.frame")){
      if(nrow(mechs) != 0){
        mech2 <- sqldf::sqldf("select mechs.*, m2.comment as assessment_id from mechs join (select distinct period, attributeOptionCombo, comment from d2) m2 on mechs.period = m2.period and mechs.attributeOptionCombo = m2.attributeOptionCombo")
        write.csv(mech2,file=paste0(folder, filename, "_mechs.csv"))
      }
      file_summary["invalid period mechanisms"] = length(mechs$attributeOptionCombo)
    } else {
      file_summary["invalid period mechanisms"] = 0
    }

    # 3. identify invalid data value types
    bad_data_values <- checkValueTypeCompliance2(d2)
    if(any(class(bad_data_values) == "data.frame")){
      if(nrow(bad_data_values) != 0) write.csv(bad_data_values,file=paste0(folder, filename, "_bad_data_values.csv"))
      file_summary["bad data values"] = length(bad_data_values$dataElement)
    } else {
      file_summary["bad data values"] = 0
    }

    # 4. identify invalid orgunits
    invalid_orgunits <- checkDataElementOrgunitValidity(data=d2, datasets=dataSets)
    if(any(class(invalid_orgunits) == "data.frame")){
      if(nrow(invalid_orgunits) > 0){
        #      print("Invalid data element/org unit pairs encountered. Printing out summaries.")
        #      write.csv(invalid_orgunits, paste0(folder, filename, '_invalid_de_ou.csv'), na="")

        invalidOUs <- sqldf::sqldf('select distinct orgUnit from invalid_orgunits')
        invalidOUAssessments <- sqldf::sqldf('select comment as assessment_id, period, orgUnit from d2 where orgunit in (select orgUnit from invalidOUs) group by comment, period, orgUnit')
        if(nrow(invalid_orgunits) != 0) {
          write.csv(invalid_orgunits,file=paste0(folder, filename, "_invalid_orgunits.csv"))
          write.csv(invalidOUAssessments,file=paste0(folder, filename, "_invalid_orgunit_list.csv"))
        }
        file_summary["invalid org units"] = length(invalidOUs$orgUnit)
        file_summary["invalid ou assessments"] = length(invalidOUAssessments$orgUnit)
      } else {
        file_summary["invalid org units"] = 0
        file_summary["invalid ou assessments"] = 0
      }
    } else {
      file_summary["invalid org units"] = 0
      file_summary["invalid ou assessments"] = 0
    }

    #incomplete_assessments <- checkCoverSheetCompleteness(data_dictionary,path)
    # write out validation summary
    write.table(as.data.frame(file_summary), file = paste0(folder, filename, "_summary.txt"))

    # write out normalized data - data has periods shifter for overlapping assessments, and has metadata in UID format. In case of any overlapping assessments in the input file, normalized file should be used for import into DATIM
    write.csv(d2[, c("dataElement","period","orgUnit","categoryOptionCombo","attributeOptionCombo","value", "storedby", "timestamp", "comment")], paste0(folder, filename, "_normalized.csv"), row.names=FALSE, na="")

    # to use in CEE validity check
    if(any(class(bad_data_values) == "data.frame")){
      if(nrow(bad_data_values) != 0) return(bad_data_values)
    } else {
      return(NULL)
    }
  }
