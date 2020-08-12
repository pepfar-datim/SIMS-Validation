require(devtools)
install_github("pepfar-datim/SIMS-Validation", force=TRUE)
require(SIMS4Validation)

secrets <- "~/secret.json"

# folder where file xzx is located, and where output files will be written to
out_dir <- "~/USAID"

# name of the file to validate
filename <- "USAIS.csv"

# type of the file (json, xml, or csv)
file_type <- "csv"

# identifier scheme used in the input file
#mechanism identifier: id or code
idScheme <- "id"
#id, code or name
dataElementIdScheme <- "name"
#id or code
orgUnitIdScheme <-"id"

# calendar period (quarter) covered by the input file in YYYYQN format, e.g. 2019Q3 for July-September 2019
isoPeriod <- "2020Q2"

# whether the input file has the header as the first line
fileHasHeader <- TRUE


SIMS4Validation::SIMSValidationScript(out_dir,filename,file_type,idScheme,dataElementIdScheme,orgUnitIdScheme,isoPeriod,fileHasHeader,secrets)