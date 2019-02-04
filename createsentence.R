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

textanalysis <- function(data, metadata, trend = TRUE, country = TRUE,
                         region = TRUE, goal = TRUE, custom_areas = FALSE) {
  
  # get the value and unit for the indicator (e.g. rate per 1,000)
  valueunit <- metadata1 %>%
    filter(metadata1['IndicatorID'] == "108") %>%
    select('Value type', 'Unit') %>%
    unite(valueunit, c('Value type', 'Unit'), sep = " ", remove = TRUE) %>%
    tolower()
  
  # generate text for the area of primary interest
  value_txt <- paste0("The ", valueunit, " of ", data['IndicatorName'],
                      " for ", tolower(data['Sex']), "s aged ", data['Age'],
                      " in ", data['AreaName'], " in ", data['Timeperiod'],
                      " was ", round(as.numeric(data['Value']), 2), ". ")
  
  # generate text on the trend, if applicable
  trend_txt <- if (trend == FALSE) {""} else {
    if (is.na(data['RecentTrend'])) {""} else {
    paste0("It is ", tolower(data['RecentTrend']), ". ")
  } }
  
  # generate text in relation to the goal, if applicable
  goal_txt <-  if (goal == FALSE) {""} else {
    if (is.na(data['Comparedtogoal'])) {""} else {
    paste0(data['AreaName'], " is performing ",
           tolower(data['Comparedtogoal']), " than the goal. ")
  } }
  
  # generate text in relation to the country, if applicable
  country_txt <-  if (country == FALSE) {""} else {
    if (is.na(data['ComparedtoEnglandvalueorpercentiles'])) {""} else {
    paste0("This is statistically ",
           tolower(data['ComparedtoEnglandvalueorpercentiles']),
           " than England. ")
  } }
  
  # generate text in relation to the region, if applicable
  region_txt <- if (region == FALSE) {""} else {
    if (is.na(data['ComparedtoRegionvalueorpercentiles'])) {""} else {
    paste0("This is statistically ",
           tolower(data['ComparedtoRegionvalueorpercentiles']),
           " to the Region. ")
  } }
  
  # generate text in relation to custom areas (e.g. CIPFA), if applicable
  compare_txt <- if (custom_areas == FALSE) {""} else {
    if (is.na(data['CIPFA_higher'])) {""} else {
    paste0(data['CIPFA_higher'], " are statistically signficantly higher than "
           , data['AreaName'], " while ", data['CIPFA_lower'],
           " are statistically signficantly lower.")
  }  }
  
  # combines all setences together
  sentence_list <- unlist(paste0(value_txt, trend_txt, goal_txt, country_txt,
                                 region_txt, compare_txt))

}
