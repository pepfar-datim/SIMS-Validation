checkValueTypeCompliance2 <- function (d, d2session = d2_default_session){
  library(dplyr)
  url <- URLencode(paste0(d2session$base_url, "api/", datimvalidation::api_version(),
                          "/system/info"))
  r <- httr::GET(url, httr::timeout(300), handle = d2session$handle)
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
  des <- datimvalidation::getDataElementMap(d2session = d2session)
  des <- merge(des, patterns, by = "valueType", all.x = T)
  dnew <- merge(d$data$import, des, by.x = "dataElement", by.y = "id")
  d_regex_validation <- dnew[!is.na(dnew$regex), ]
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
  d_option_sets <- checkOptionSetCompliance2(d, d2session = d2session)
  if(!is.null(d_option_sets$tests$invalid_option_set_values)){
    d <- dplyr::bind_rows(d_regex_validation, d_option_sets$tests$invalid_option_set_values)
    if (NROW(d) > 0) {
      d
    }
    else {
      TRUE
    }
  }
  else{
    TRUE
  }
  
}

checkOptionSetCompliance2 <- function(d,
                                      d2session = dynGet("d2_default_session",
                                                         inherits = TRUE)) {
  
  data <- d$data$import
  
  option_sets_des <- datimvalidation::getDataElementMap(d2session = d2session) %>%
    dplyr::filter(!is.na(optionSet.id)) %>%
    dplyr::select(dataElement = id, optionSetID = optionSet.id) %>%
    dplyr::distinct()
  
  data <-  dplyr::inner_join(data, option_sets_des, by = "dataElement")
  
  if (NROW(data) == 0) {
    return(d)
  }
  
  option_set_map <- datimvalidation::getOptionSetMap(d2session = d2session)
  
  getOptionSetValues <- function(x) {
    list_index <- which(option_set_map$id == x)
    option_set_map[list_index, "options"][[1]] %>% dplyr::pull("code")
  }
  
  option_set_values_list <- lapply(data$optionSetID, getOptionSetValues)
  
  is_valid_value <- mapply(function(x, y) x %in% y,
                           data$value,
                           option_set_values_list)
  
  invalid_option_set_values <- data %>%
    dplyr::filter(!is_valid_value)  %>%
    dplyr::select(dataElement,
                  period,
                  orgUnit,
                  categoryOptionCombo,
                  attributeOptionCombo,
                  value,comment)
  
  if (NROW(invalid_option_set_values) > 0) {
    d$tests$invalid_option_set_values <- invalid_option_set_values
    msg <- paste(NROW(invalid_option_set_values), "invalid option set values detected.")
    d$info$messages <- appendMessage(d$info$messages, msg, "ERROR")
    
  } else  {
    msg <- "All option set values were valid."
    d$info$messages <- appendMessage(d$info$messages, msg, "ERROR")
  }
  d
  
}
