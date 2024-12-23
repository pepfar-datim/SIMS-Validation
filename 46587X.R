# 1. validate/normalize agency files
# 2. put them in to_import folder
#   a. create files2.txt - used for batch import
#   b. create csv_files.txt - used for ?
# 3. get latest mechanism listing from: https://www.datim.org/api/sqlViews/fgUtV6e9YIX/data.csv   to   Mechanisms partners agencies OUS Start End.csv
# 3.1 get data elements in csv format
# 4. ger previously imported data from: https://www.datim.org/api/dataValueSets.csv?dataSet=O392zMXtwar&dataSet=rnEToFucnJ9&orgUnit=ybg3MO3hcf4&children=true&startDate=2020-07-01&endDate=2020-09-30   to fy20q4.csv
#
#
# 5. load sites
# 6. read in mechanismx
# 7. 
#
#

#download mechanism values:
#
# to targets_and_results.csv

require(sqldf)

rm(list=ls())

base_path <- "/Users/41250/Documents/SIMS4/2024/Q4/2/combined/"
config_path <- "/Users/41250/Documents/datim-validation/datim.json"

datimutils::loginToDATIM(config_path)

mechanism_targets_and_results_data_url <- "https://www.datim.org/api/analytics.csv?dimension=SH885jaRe0o&dimension=IeMmjHyBUpi:Jh0jDM5yQ2E;W8imnja2Owd&filter=ou:ybg3MO3hcf4&filter=pe:2023Oct&displayProperty=SHORTNAME&outputIdScheme=CODE"
httr::GET(mechanism_targets_and_results_data_url, httr::write_disk(paste0(base_path, "targets_and_results.csv"), overwrite=TRUE), handle = d2_default_session$handle)

#gets all mechanisms for agency association 
mechanism_list_url <- "https://www.datim.org/api/sqlViews/fgUtV6e9YIX/data.csv" 
httr::GET(mechanism_list_url, httr::write_disk(paste0(base_path, "mechanisms.csv"), overwrite=TRUE), handle = d2_default_session$handle)

data_elements_url <- "https://www.datim.org/api/dataElements.csv?paging=false&fields=id,name,code&filter=dataSetElements.dataSet.id:in:[dT9xKGbcXLK]"
httr::GET(data_elements_url, httr::write_disk(paste0(base_path, "dataElements.csv"), overwrite=TRUE), handle = d2_default_session$handle)

# previous_data_url <- "https://www.datim.org/api/dataValueSets.csv?dataSet=lvfFcexh1nB&dataSet=VP0uG6tzB5l&orgUnit=ybg3MO3hcf4&children=true&startDate=2022-07-01&endDate=2022-09-30"
# httr::GET(previous_data_url, httr::write_disk(paste0(base_path, "previously_imported_data.csv"), overwrite=TRUE), handle = d2_default_session$handle)


#generate summaries

# org units - this is an long running block, try to reuse as much as possible
# get all org units, with ancestry
url <- paste0(d2_default_session$base_url, "api/organisationUnits.json?fields=ancestors[name,code,id],id,name,code,level&paging=false")
r <- httr::GET(url, httr::timeout(480), handle = d2_default_session$handle)
r <- httr::content(r, "text")
sites<-jsonlite::fromJSON(r,flatten=TRUE)[[1]]
# flatten out ancestry, if level is 1, 2, or 3, use actual code/name/id, otherwise grab third ancestor/lvl 3 (operating unit)
for(x in 1:length(sites$id)){
  if(sites$level[[x]] < 4){
    sites$operating_unit_code[x] <- sites$code[[x]]
    sites$operating_unit_id[x] <- sites$id[[x]]
    sites$operating_unit_name[x] <- sites$name[[x]]
  } else {
    sites$operating_unit_code[x] <- sites$ancestor[[x]]$code[3]
    sites$operating_unit_id[x] <- sites$ancestor[[x]]$id[3]
    sites$operating_unit_name[x] <- sites$ancestor[[x]]$name[3]
  }
}
# remove no longer needed ancestors
sites$ancestors <- NULL


#d <- read.csv(paste0(base_path, "previously_imported_data.csv"), stringsAsFactors = FALSE)
mechz <- read.csv(paste0(base_path, "mechanisms.csv"), stringsAsFactors = FALSE)

all_data <- NULL

# UPDATE agencies whose data needs to be deleted/kept
#to_delete <- sqldf("select * from d where attributeoptioncombo in (select uid from mechz where agency in ('HHS/CDC', 'USAID')) and (period >= 20210401 and period < 20210701)")
#write.csv(to_delete, paste0(base_path, "/to_delete/to_delete.csv"), row.names=FALSE, na = '')
#writeLines("to_delete.csv", file(paste0(base_path, "/to_delete/to_delete.txt")))

#to_keep <- sqldf("select dataelement,period,orgunit,categoryoptioncombo,attributeoptioncombo,value,storedby,lastupdated,comment from d where attributeoptioncombo not in (select uid from mechz where agency in ('HHS/CDC', 'USAID')) ")


dir <- paste0(base_path, "to_import/")

#all_data <<- rbind(all_data, to_keep)

files <- read.csv(paste0(dir, "csv_files.txt"), stringsAsFactors = FALSE)

apply(files, 1, function(file){
  filename <- file["name"]
  print(paste0(dir,filename))
  d <- read.csv(paste0(dir,filename), stringsAsFactors = FALSE)
#  names(d)  <- names(to_keep)
  all_data <<- rbind(all_data, d)
})

des <- read.csv(paste0(base_path, "dataElements.csv"), stringsAsFactors = FALSE)

all_data2 <- sqldf("
  select 
    case when period >= 20221001 and period < 20230101 then 'FY23Q1' else
    case when period >= 20230101 and period < 20230401 then 'FY23Q2' else
    case when period >= 20230401 and period < 20230701 then 'FY23Q3' else
    case when period >= 20230701 and period < 20231001 then 'FY23Q4' else
    case when period >= 20231001 and period < 20240101 then 'FY24Q1' else
    case when period >= 20240101 and period < 20240401 then 'FY24Q2' else
    case when period >= 20240401 and period < 20240701 then 'FY24Q3' else
    case when period >= 20240701 and period < 20241001 then 'FY24Q4' else
       'Unkown' end end end end end end end end as reporting_period,
    operating_unit_code,
    operating_unit_id as operating_unit_uid,
    orgunit,
    agency,
    mechz.code as mechanism,
    comment as assessment_id,
    case when value is null then 'dummy' else value end as v,
    case when storedby is null then '' else 'existing' end as type
  from all_data left join mechz on all_data.attributeoptioncombo = mechz.uid left join sites on all_data.orgunit = sites.id                   
  
")


assessment_type <- 
sqldf("
select comment, 
    case when value = '1' then 
        'Comprehensive'
    else
      case when value ='2' then 
        'Follow Up' 
        else
         case when value ='3' then 
        'Concentrated'
        else 
        'Uknown'
    end end end as asmt_type
from all_data join des on all_data.dataElement = des.id
      where des.name = 'SIMS.CS_ASMT_TYPE'
")


all_data2 <- sqldf("select ad2.*, asmt_type from all_data2 ad2 left join assessment_type at on ad2.assessment_id = at.comment")

assessments_with_coversheet <- 
  sqldf("
select comment, max(has_cs) as has_cs, max(has_assessment_details) as has_assessment_details from (

select comment,
  case when des.name like 'SIMS.CS_%' then 1 else 0 end as has_cs,
  case when des.name like 'SIMS.S_%' or des.name like 'SIMS.AS_%' then 1 else 0 end as has_assessment_details
from all_data join des on all_data.dataElement = des.id
) group by comment
        ")


  
# case when des.name like 'SIMS.CS%' then 1 else 0 end as coversheet,
# case when des.name not like 'SIMS.CS%' then 1 else 0 end as core




counts <- sqldf("select reporting_period, agency, count(distinct(assessment_id)) assessment_count, count(*) as record_count, type from all_data2 group by reporting_period, agency, type order by reporting_period, agency")
by_mechanism <- sqldf("select agency, reporting_period,  mechanism, count(distinct(assessment_id)) assessment_count, count(*) as record_count from all_data2 group by agency, reporting_period, mechanism order by reporting_period, agency, mechanism")
by_operating_unit <- sqldf("select agency, reporting_period, operating_unit_code, operating_unit_uid, count(distinct(assessment_id)) assessment_count, count(*) as record_count from all_data2 group by agency, reporting_period, operating_unit_code, operating_unit_uid order by reporting_period, agency, operating_unit_code")

write.csv(counts, paste0(dir, "by_agency_summary.csv"), row.names=FALSE, na="")
write.csv(by_mechanism, paste0(dir, "by_mechanism_summary.csv"), row.names=FALSE, na="")
write.csv(by_operating_unit, paste0(dir, "by_operating_unit_summary.csv"), row.names=FALSE, na="NA")


counts <- sqldf("select reporting_period, agency, asmt_type, count(distinct(assessment_id)) assessment_count, count(*) as record_count, type from all_data2 group by reporting_period, agency, asmt_type, type order by reporting_period, agency")
by_mechanism <- sqldf("select agency, reporting_period, asmt_type, mechanism, count(distinct(assessment_id)) assessment_count, count(*) as record_count from all_data2 group by agency, reporting_period, asmt_type, mechanism order by reporting_period, agency, mechanism")
by_operating_unit <- sqldf("select agency, reporting_period, asmt_type, operating_unit_code, operating_unit_uid, count(distinct(assessment_id)) assessment_count, count(*) as record_count from all_data2 group by agency, reporting_period, asmt_type, operating_unit_code, operating_unit_uid order by reporting_period, agency, operating_unit_code")

write.csv(counts, paste0(dir, "by_agency_by_type_summary.csv"), row.names=FALSE, na="")
write.csv(by_mechanism, paste0(dir, "by_mechanism_by_type_summary.csv"), row.names=FALSE, na="")
write.csv(by_operating_unit, paste0(dir, "by_operating_unit_by_type_summary.csv"), row.names=FALSE, na="NA")



# missing MER data:

# tool type: RpqaKUXGtDS
# assessment type: axQrLFHH0Nl

mech_data <- read.csv(paste0(base_path, "targets_and_results.csv"), stringsAsFactors = FALSE)
colnames(mech_data) <- c("mech", "tr", "value")

# result - Jh0jDM5yQ2E
# target - W8imnja2Owd

m <- datimvalidation::getMechanismsMap()

has_mer <- sqldf("select m2.agency as agency, s.operating_unit_name as site_ou, m2.ou as mechanism_ou, d2.comment as assesment_id, 
case when d3.value = '1' then 'site' else case when d3.value = '3' then 'above site' else 'unknown' end end as tool_type,
case when d4.value = '1' then 'full assessment' else case when d4.value = '2' then 'follow-up' else case when d4.value = '3' then 'concentrated' else 'unknown' end end end as assessment_type,
m.name as mechanism_name,  
  case when sum(t.value) is NULL then 'No' else 'Yes' end as has_targets,
  case when sum(r.value) is NULL then 'No' else 'Yes' end as has_results
from all_data d2 join m on d2.attributeOptionCombo = m.id
      left join mech_data t on m.code = t.mech and t.tr='W8imnja2Owd'
      left join mech_data r on m.code = r.mech and r.tr='Jh0jDM5yQ2E'
      left join mechz m2 on m.id = m2.uid
      left join sites s on s.id = d2.orgUnit
      
      left join all_data d3 on d2.comment = d3.comment and d3.dataElement = 'RpqaKUXGtDS'
      left join all_data d4 on d2.comment = d4.comment and d4.dataElement = 'axQrLFHH0Nl'
      group by d2.comment, m.name
")
missing_mer <- has_mer[has_mer$has_result != 'Yes' & has_mer$has_target != 'Yes',]

if(nrow(missing_mer) != 0){
  write.csv(missing_mer,file=paste0(dir, "assessments_missing_mer_data.csv"), row.names = FALSE)
}











# generate a list   
#m <- sqldf(paste0("select '", agency, "' as agency, '", isoPeriod, "' as reporting_period, m.code as mechanism, count(distinct(comment)) as assessment_count, count(case when value is null then 1 else value end) as record_count from d left join mech_map m on d.attributeOptionCombo = m.id group by m.code"))
#mechs <<- rbind(mechs, m)

#o <- sqldf(paste0("select '", agency, "' as agency, '", isoPeriod, "' as reporting_period,  o.operating_unit_code as operating_unit_code, o.operating_unit_id as operating_unit_uid, count(distinct(comment)) as assessment_count, count(case when value is null then 1 else value end) as record_count from d left join sites o on d.orgUnit = o.id group by o.operating_unit_code"))
#operating_units <<- rbind(operating_units, o)



# get mechanism map (for codes)
#mech_map <- datimvalidation::getMechanismsMap()
mech_map <- m




# list of files in csv format. header: path, isoPeriod (fy period), agency
# files in the folder are expected to be the normalized one, ready for import - with normalized headers and UIDs for all metadata
files <- read.csv(paste0(dir, "csv_files.txt"), stringsAsFactors = FALSE)

# create mech and ou data frames for all files
mechs <- NULL
operating_units <- NULL
counts <- NULL

# process files
apply(files, 1, function(file){
  filename <- file["name"]
  isoPeriod <- file["isoPeriod"]
  agency <- file["agency"]
  
  d <- read.csv(paste0(dir,filename), stringsAsFactors = FALSE)
  
  
  assessments_with_coversheet <- 
    sqldf("
select comment, max(has_cs) as has_cs, max(has_assessment_details) as has_assessment_details from (

select d.comment,
  case when des.name like 'SIMS.CS_%' then 1 else 0 end as has_cs,
  case when des.name like 'SIMS.S_%' or des.name like 'SIMS.AS_%' then 1 else 0 end as has_assessment_details
from d join des on d.dataElement = des.id
) group by comment
        ")
  
  
  sql <- paste0("select '", agency, "' as file, '", isoPeriod, "' as pe, count(*) as record_count, count(distinct(d.comment)) as assessment_count, 
                case when sum(has_cs) - sum(has_assessment_details) = 0 then 'No' else 'Yes' end as has_incomplete_assessments  from d join assessments_with_coversheet a on d.comment = a.comment")
  print(sql)
  counts <<- rbind(counts, sqldf(sql))
  
  # generate a list   
  m <- sqldf(paste0("select '", agency, "' as agency, '", isoPeriod, "' as reporting_period, m.code as mechanism, count(distinct(comment)) as assessment_count, count(case when value is null then 1 else value end) as record_count from d left join mech_map m on d.attributeOptionCombo = m.id group by m.code"))
  mechs <<- rbind(mechs, m)
  
  o <- sqldf(paste0("select '", agency, "' as agency, '", isoPeriod, "' as reporting_period,  o.operating_unit_code as operating_unit_code, o.operating_unit_id as operating_unit_uid, count(distinct(comment)) as assessment_count, count(case when value is null then 1 else value end) as record_count from d left join sites o on d.orgUnit = o.id group by o.operating_unit_code"))
  operating_units <<- rbind(operating_units, o)
})

write.csv(mechs, paste0(dir, "by_mechanism_summary.csv"), row.names=FALSE, na="")
write.csv(operating_units, paste0(dir, "by_operating_unit_summary.csv"), row.names=FALSE, na="NA")
write.csv(counts, paste0(dir, "_counts.csv"), row.names=FALSE, na="")



 

#generate summaries

# org units - this is an long running block, try to reuse as much as possible
# get all org units, with ancestry
url <- paste0(d2_default_session$base_url, "api/organisationUnits.json?filter=level:in:[3,4]&fields=id,name,code&paging=false")
r <- httr::GET(url, httr::timeout(120), handle = d2_default_session$handle)
r <- httr::content(r, "text")
operating_unit_names<-jsonlite::fromJSON(r,flatten=TRUE)[[1]]
# flatten out ancestry, if level is 1, 2, or 3, use actual code/name/id, otherwise grab third ancestor/lvl 3 (operating unit)

summary <- read.csv(paste0(dir, "by_operating_unit_by_type_summary.csv"), stringsAsFactors = FALSE)

country_count_summary <- sqldf("
select ou as 'Operating Unit' from (
select o.name || ' (' || sum(assessment_count) || ')' as ou, 1 as SortOrder from operating_unit_names o join summary s on o.id=s.operating_unit_uid group by operating_unit_uid
      union select 'Total (' || sum(assessment_count) || ')', 99 as SortOrder from summary
) order by SortOrder, ou

      ")


write.table(country_count_summary, file=paste0(dir, "_country_counts_pretty.txt"), row.names=FALSE, quote=FALSE)




files <- read.csv(paste0(dir, "csv_files.txt"), stringsAsFactors = FALSE)

apply(files, 1, function(file){
  filename <- file["name"]
  agency <- file["agency"]
  print(paste0(dir,filename))
  d <- read.csv(paste0(dir,filename), stringsAsFactors = FALSE)
  sqldf(paste0("select distinct d.comment, m.code, m.partner, m.agency from d join mechz m on d.attributeOptionCombo = m.uid where m.agency != '", agency, "'"))
})
