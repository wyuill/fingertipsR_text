comparetoareas <- function(df, from_area, to_areas, colname) {
  
  # Extract data for area to compre from and areas to compare to
  from_data = df[df$AreaCode == from_area, ]
  to_data = df[df$AreaCode %in% to_areas, ]
  
  from_data[, paste(c(colname, "_higher"), collapse="")] <- mapply(function(f_IndicatorID, f_Sex, f_Age, f_Category,
                                                            f_Timeperiod, f_UCI){
    differences = to_data %>%
      filter(IndicatorID == f_IndicatorID &
               Sex == f_Sex &
               Age == f_Age &
               Timeperiod == f_Timeperiod &
               Value > f_UCI &
               (if (is.na(f_Category)) {
                 is.na(Category)
               } else {Category == f_Category
               })) %>%
      select(AreaName) %>%
      unlist() %>%
      paste(collapse = ", ") %>%
      str_replace(",(?!.*?,)", " and")
  }, from_data$IndicatorID, from_data$Sex, from_data$Age, from_data$Category, from_data$Timeperiod,
  from_data$UpperCI95.0limit)
  
  from_data[, paste(c(colname, "_lower"), collapse="")] <- mapply(function(f_IndicatorID, f_Sex, f_Age, f_Category,
                                                           f_Timeperiod, f_LCI){
    differences = to_data %>%
      filter(IndicatorID == f_IndicatorID &
               Sex == f_Sex &
               Age == f_Age &
               Timeperiod == f_Timeperiod &
               Value < f_LCI &
               (if (is.na(f_Category)) {
                 is.na(Category)
               } else {Category == f_Category
               })) %>%
      select(AreaName) %>%
      unlist() %>%
      paste(collapse = ", ") %>%
      str_replace(",(?!.*?,)", " and")
  }, from_data$IndicatorID, from_data$Sex, from_data$Age, from_data$Category, from_data$Timeperiod,
  from_data$LowerCI95.0limit)  
  
  return = from_data
}
