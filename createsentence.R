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

textanalysis <- function(data, trend = TRUE, country = TRUE,
                         region = TRUE, goal = TRUE, definitions = TRUE) {
  
  value_txt <- paste0("The ", "VALUETYPE", " of ", data['IndicatorName'], " for ",
                      tolower(data['Sex']), "s aged ", data['Age'], " in ", data['AreaName'], " in ",
                      data['Timeperiod'], " was ", round(as.numeric(data['Value']), 2), ". ")
  
  trend_txt <- if (trend == FALSE) {""} else {
    if (is.na(data['RecentTrend'])) {""} else {
    paste0("It is ", tolower(data['RecentTrend']), ". ")
  } }
  
  goal_txt <-  if (goal == FALSE) {""} else {
    if (is.na(data['Comparedtogoal'])) {""} else {
    paste0(data['AreaName'], " is performing ", tolower(data['Comparedtogoal']), " than the goal. ")
  } }
  
  country_txt <-  if (country == FALSE) {""} else {
    if (is.na(data['ComparedtoEnglandvalueorpercentiles'])) {""} else {
    paste0("This is statistically ", tolower(data['ComparedtoEnglandvalueorpercentiles']), " than England. ")
  } }
  
  region_txt <- if (region == FALSE) {""} else {
    if (is.na(data['ComparedtoRegionvalueorpercentiles'])) {""} else {
    paste0("This is statistically ", tolower(data['ComparedtoRegionvalueorpercentiles']), " to the Region. ")
  } }s
  
  comp_txt <- 1
  
  sentence_list <- unlist(paste0(value_txt, trend_txt, goal_txt, country_txt, region_txt, comp_txt))
  
}

