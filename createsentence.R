# The VALUETYPE for AREANAME in TIMEPERIOD was VALUE.
# The recent trend is RECENTREND.
# Compared to the goal AREANAME was COMPAREDTOGOAL.
# This is COMPAREDTOENGLAND than England.
# This is COMPAREDTOREGION than the Region.
# Compared to GEOTYPE, AREANAME was statistically significantly POLARITY than
#   count(_HIGHER) (_HIGHER) and statistically significantly POLARITY then
#   count(_LOWER) (_LOWER)
#        **REPEAT LAST AS MANY TIMES AS SPECIFIED WITH DIFFERENT AREAS**
# Definitions: 95% CI Compare area lists 

# Ideas: order of sentences, add category, add values of other areas (e.g. England)

textanalysis <- function(data, metadata, trend = TRUE, goal = TRUE,
                         compare = TRUE, compare_cols = c(
                            'ComparedtoEnglandvalueorpercentiles',
                            'ComparedtoRegionvalueorpercentiles',
                            'ComparedtoCCGvalueorpercentiles'),
                         custom_areas = FALSE) {
  
  # get the value and unit for the indicator
  valueunit <- metadata1 %>%
    filter(metadata1[['IndicatorID']] == test1[['IndicatorID']]) %>%
    select('Value type', 'Unit') %>%
    unite(valueunit, c('Value type', 'Unit'), remove=TRUE, sep=" ") %>%
    tolower()
  
  # generate text for the area of primary interest
  value_txt <- paste0("The ", valueunit, " of ", data['IndicatorName'],
                      " for ", tolower(data['Sex']), "s aged ", data['Age'],
                      " in ", data['AreaName'], " in ", data['Timeperiod'],
                      " was ", round(as.numeric(data['Value']), 2), ".")
  
  # generate text on the trend, if applicable
  trend_txt <- if (trend == FALSE) {""} else {
    if (is.na(data['RecentTrend'])) {""} else {
    paste0("It is ", tolower(data['RecentTrend']), ".")
  } }
  
  # generate text in relation to the goal, if applicable
  goal_txt <-  if (goal == FALSE) {""} else {
    if (is.na(data['Comparedtogoal'])) {""} else {
    paste(data['AreaName'], "is performing",
           tolower(data['Comparedtogoal']), "than the goal.")
  } }
  
  # sub function to generate text using comparisons stored in the data
  compare_txt <- if (compare == FALSE) {""} else {
    sapply(colnames(data),function(c){
      if(c %in% data) if(is.na(data[c])) {""} else {
        paste0(data['AreaName'], " is statistically ", tolower(data[c]),
               " than ", str_remove(c, "Comparedto|valueorpercentile"), ". ")
  }})}
  
  # generate text in relation to custom areas (e.g. CIPFA), if applicable
  cus_compare_txt <- if (custom_areas == FALSE) {""} else {
    if (is.na(data['CIPFA_higher']) & is.na(data['CIPFA_lower'])) {""} else {
      paste(data['CIPFA_higher'], "are statistically signficantly higher than",
           data['AreaName'], "while", data['CIPFA_lower'],
           "are statistically signficantly lower.")
  }  }
  
  # combines all setences together
  sentence_list <- unlist(paste0(value_txt, trend_txt, goal_txt, compare_txt,
                                cus_compare_txt))
}
