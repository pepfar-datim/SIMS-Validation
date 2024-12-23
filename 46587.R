#
# R script for validating SIMS 4.0 payload using datim-validation library
# 
# Script can be run as a whole, or incrementally. It is recommended to run it incrementally. If it is run as a whole, and a step fails, the rest of the code might also encounter errors.
#
# Following variable are required and need to be altered to meet your needs:
# folder - path to the folder where input file resides
# filname - name of the file
#
# 2019-07-17 Vladimer.Shioshvili@icf.com

# Loading of the libraries is required once, and does not need to be run once libraries are loaded; however in order to keep the datim-validation library up to date, it should be run periodically;
# To check when the datim-validation library was last updated, go to https://github.com/jason-p-pickering/datim-validation and make note of the Latest commit date.
install.packages("sqldf")
require(sqldf)
#install.packages("devtools")
require(devtools)
install_github("jason-p-pickering/datim-validation", force=TRUE, )
require(datimutils)
require(datimvalidation)
install.packages("httptest")
##
#load_all("/Users/vshioshvili/icf/projects/datim-validation_forked")


# Clear any environment variables, and cache
datimvalidation::clearCache(force=TRUE)
rm(list=ls())

### VARIABLES - update the variables below as needed

# load secrets file with url, username, and password of the target DATIM server - 
datimutils::loginToDATIM(config_path = "/Users/hchichaybelu/Documents/datim-validation/datim.json")

# folder where file is located, and where output files will be written to
folder <- "/Users/hchichaybelu/Documents/SIMS4/2023/Q2/1/DOD/"
out_dir <- folder
# name of the file to validate
filename <- "all_dod.csv"

# type of the file (json, xml, or csv)
file_type <- "csv"

# identifier scheme used in the input file
idScheme <- "code"
dataElementIdScheme <- "name"
orgUnitIdScheme <-"id"

# calendar period (quarter) covered by the input file in YYYYQN format, e.g. 2019Q3 for July-September 2019
isoPeriod <- "2023Q1"

# whether the input file has the header as the first line
fileHasHeader <- FALSE

# data sets; if the import file does not include both types of assessments, only retain the identifier of the data set that is applicable
#SIMS4 Above site data set - O392zMXtwar, SIMS 4.1 - lvfFcexh1nB
#SIMS4 Site data set - rnEToFucnJ9, SIMS 4 - VP0uG6tzB5l
dataSets <- c("dT9xKGbcXLK")

#### =====================


path <- paste0(folder, filename)

file_summary <- c()
file_summary["file"] <- filename

# parse using regular parser, used to identify period shifts and overlapping assessments
d <- d2Parser(file = path, type = file_type, dataElementIdScheme = dataElementIdScheme, orgUnitIdScheme = orgUnitIdScheme, idScheme = idScheme, invalidData = TRUE, csv_header = fileHasHeader)

if(any(class(d) == "data.frame")){
  # no issues
} else {
  print(d)
}

#
# VALIDATION
#


# 1. parse input file

options("organisationUnit"="ybg3MO3hcf4")
# parse using SIMS parser - this parser does period shifting of overlapping SIMS assessments
d2 <- sims2Parser(file=path, dataElementIdScheme = dataElementIdScheme, orgUnitIdScheme = orgUnitIdScheme, idScheme = idScheme, invalidData=TRUE, hasHeader=fileHasHeader, isoPeriod=isoPeriod)

file_summary["record count"] = length(d2$comment)
file_summary["assessment count"] = length(unique(d2$comment))

# identify overlapping assessments, and if any write out details
overlapping_assessment <- sqldf('select period, orgUnit, attributeOptionCombo, count(distinct(storedby)) as assessment_count from d group by period, orgUnit, attributeOptionCombo having count(distinct(storedby)) > 1')
if(nrow(overlapping_assessment) != 0) {
  write.csv(overlapping_assessment,file=paste0(out_dir, filename, "_overlapping_assessment.csv"))
  overlapping_assessment_list <- sqldf('select distinct d.period, d.orgUnit, d.attributeOptionCombo, d.storedby from d join overlapping_assessment o on d.period=o.period and d.orgUnit=o.orgUnit and d.attributeOptionCombo = o.attributeOptionCombo')
  write.csv(overlapping_assessment_list,file=paste0(out_dir, filename, "_overlapping_assessment_list.csv"))
}
file_summary["overlapping PE/OU/IM count"] = length(overlapping_assessment$period)

# identify period shifts resulting from shifting assessments
d_unique = sqldf('select period, storedby from d group by period, storedby')
d2_unique = sqldf('select period, comment from d2 group by period, comment')
shifts_made = sqldf('select comment as assessment, d_unique.period as old_period, d2_unique.period as new_period from d_unique join d2_unique on d_unique.storedby = d2_unique.comment where d_unique.period != d2_unique.period order by old_period')
if(nrow(shifts_made) != 0) write.csv(shifts_made,file=paste0(out_dir, filename, "_shifts_made.csv"))
file_summary["shifted_assessment_count"] = nrow(shifts_made)

# identify any exact duplicates after period shifting
post_shift_duplicates <- getExactDuplicates(d2)
de_map <- getDataElementMap() # used to produce post-shift duplicates with codes
post_shift_duplicates_w_code <- sqldf('select de_map.code, post_shift_duplicates.* from  post_shift_duplicates left join de_map on de_map.id = post_shift_duplicates.dataElement order by dataElement, period, orgUnit, attributeOptionCombo')
if(nrow(post_shift_duplicates_w_code) != 0) write.csv(post_shift_duplicates_w_code,file=paste0(out_dir, filename, "_post_shift_duplicates.csv"))
file_summary["post shift duplicate count"] = length(post_shift_duplicates_w_code$comment)

# 2. verify mechanism validity  
mechs <- checkMechanismValidity(d2)
if(any(class(mechs) == "data.frame")){
  if(nrow(mechs) != 0){
    mech2 <- sqldf("select mechs.*, m2.comment as assessment_id from mechs join (select distinct period, attributeOptionCombo, comment from d2) m2 on mechs.period = m2.period and mechs.attributeOptionCombo = m2.attributeOptionCombo")
    write.csv(mech2,file=paste0(out_dir, filename, "_mechs.csv"))
  }
  file_summary["invalid period mechanisms"] = length(mechs$attributeOptionCombo)
} else {
  file_summary["invalid period mechanisms"] = 0
}

# 3. identify invalid data value types
bad_data_values <- checkValueTypeCompliance(d2)
if(any(class(bad_data_values) == "data.frame")){
  if(nrow(bad_data_values) != 0) write.csv(bad_data_values,file=paste0(out_dir, filename, "_bad_data_values.csv"))
  file_summary["bad data values"] = length(bad_data_values$dataElement)
} else {
  file_summary["bad data values"] = 0
}

# 4. identify invalid orgunits  
invalid_orgunits <- checkDataElementOrgunitValidity(data=d2, datasets=dataSets)
if(any(class(invalid_orgunits) == "data.frame")){
  if(nrow(invalid_orgunits) > 0){
    #      print("Invalid data element/org unit pairs encountered. Printing out summaries.")
    #      write.csv(invalid_orgunits, paste0(out_dir, filename, '_invalid_de_ou.csv'), na="")
    
    invalidOUs <- sqldf('select distinct orgUnit from invalid_orgunits')
    invalidOUAssessments <- sqldf('select comment as assessment_id, period, orgUnit from d2 where orgunit in (select orgUnit from invalidOUs) group by comment, period, orgUnit')
    if(nrow(invalid_orgunits) != 0) {
      write.csv(invalid_orgunits,file=paste0(out_dir, filename, "_invalid_orgunits.csv"))
      write.csv(invalidOUAssessments,file=paste0(out_dir, filename, "_invalid_orgunit_list.csv"))
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

# write out validation summary

write.table(as.data.frame(file_summary), file = paste0(folder, filename, "_summary.txt"))

# write out normalized data - data has periods shifter for overlapping assessments, and has metadata in UID format. In case of any overlapping assessments in the input file, normalized file should be used for import into DATIM
write.csv(d2[, c("dataElement","period","orgUnit","categoryOptionCombo","attributeOptionCombo","value", "storedby", "timestamp", "comment")], paste0(folder, filename, "_normalized.csv"), row.names=FALSE, na="")


header <- c("de","pe", "ou", "coc", "aoc", "value", "comment")
x <- read.csv(path, stringsAsFactors = FALSE, header=TRUE)
colnames(x) <- header

x1 <-  sqldf("
select comment, sum(coversheet), sum(core) from (
select 
             comment, case when de like 'SIMS.CS%' then 1 else 0 end as coversheet, case when de not like 'SIMS.CS%' then 1 else 0 end as core from x
) group by comment
             ")


mech_file <- "/Users/hchichaybelu/Documents/SIMS4/2023/Q1/1/combined/targets_and_results.csv"
mech_data <- read.csv(mech_file, stringsAsFactors = FALSE)
colnames(mech_data) <- c("mech", "tr", "value")

# result - Jh0jDM5yQ2E
# target - W8imnja2Owd

m <- getMechanismsMap()

has_mer <- sqldf("select comment, m.name,
  case when sum(t.value) is NULL then 'No' else 'Yes' end as has_targets,
  case when sum(r.value) is NULL then 'No' else 'Yes' end as has_results
from d2 join m on d2.attributeOptionCombo = m.id
      left join mech_data t on m.code = t.mech and t.tr='W8imnja2Owd'
      left join mech_data r on m.code = r.mech and r.tr='Jh0jDM5yQ2E'
      group by comment, m.name
")
missing_mer <- has_mer[has_mer$has_result != 'Yes' & has_mer$has_target != 'Yes',]

if(nrow(missing_mer) != 0){
  write.csv(missing_mer,file=paste0(out_dir, filename, "_missing_mer.csv"))
}

