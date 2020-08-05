checkValueTypeCompliance2 <- function (d){
  library(dplyr)
  url <- URLencode(paste0(getOption("baseurl"), "api/", api_version(),
                          "/system/info"))
  r <- httr::GET(url, httr::timeout(300))
  r <- httr::content(r, "text")
  sysInfo <- jsonlite::fromJSON(r, flatten = TRUE)
  version <- as.numeric(strsplit(sysInfo$version, "\\.")[[1]][2])
  if (version < 21) {
    print("API version not supported. Aborting.")
    stop()
  }
  patterns <- list(NUMBER = "^(-?[0-9]+)(\\.[0-9]+)?$", INTEGER = "^(0|-?[1-9]\\d*)$",
                   INTEGER_POSITIVE = "^[1-9]\\d*$", INTEGER_ZERO_OR_POSITIVE = "(^0$)|(^[1-9]\\d*$)",
                   INTEGER_NEGATIVE = "^-[1-9]\\d*$", ZERO_PATTERN = "^0(\\.0*)?$",
                   BOOLEAN = "^(true|false|True|False|0|1)$", TRUE_ONLY = "^(true|True|1)$",
                   PERCENTAGE = "^([0-9]|[1-9][0-9]|100)(\\.[0-9]+)?$",
                   UNIT_INTERVAL = "^(0(\\.[0-9]+)?)$|^1$", DATE = "^(19|20)\\d\\d[- /.](0[1-9]|1[012])[- /.](0[1-9]|[12][0-9]|3[01])$",
                   DATETIME = "^(19|20)\\d\\d-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01]) (0[0-9]|1[0-9]|2[0-4]):([0-5][0-9]):([0-9][0-9])(\\.\\d{2,3})?$")
  patterns <- reshape2::melt(patterns)
  names(patterns) <- c("regex", "valueType")
  des <- getDataElementMap()
  des <- merge(des, patterns, by = "valueType", all.x = T)
  d <- merge(d, des, by.x = "dataElement", by.y = "id")
  d_regex_validation <- d[!is.na(d$regex), ]
  if (NROW(d_regex_validation) > 0) {
    d_regex_validation$is_valid_pattern <- mapply(grepl,
                                                  d_regex_validation$regex, as.character(d_regex_validation$value))
    d_regex_validation$is_valid_zero <- !(d_regex_validation$value ==
                                            "0" & !d_regex_validation$zeroIsSignificant)
    d_regex_validation$is_valid_value <- d_regex_validation$is_valid_pattern &
      d_regex_validation$is_valid_zero
    d_regex_validation <- d_regex_validation %>% dplyr::select(dataElement,
                                                               period, orgUnit, categoryOptionCombo, attributeOptionCombo,
                                                               value, is_valid_value, comment) %>% dplyr::filter(!is_valid_value)
  }
  else {
    d_regex_validation <- data.frame(dataElement = character(),
                                     period = character(), categoryOptionCombo = character(),
                                     attributeOptionCombo = character(), value = character(),
                                     is_valid_value = logical(), comment = charachter(), stringsAsFactors = FALSE)
  }
  d_option_sets <- checkOptionSetCompliance(d)
  d <- dplyr::bind_rows(d_regex_validation, d_option_sets)
  if (NROW(d) > 0) {
    d
  }
  else {
    TRUE
  }
}
