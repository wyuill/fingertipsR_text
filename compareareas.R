#' Compare an area to others areas for statistically signficant differences
#'
#' \code{comparetoareas} returns the supplied dataframe filtered to the compared
#' from area, with additional columns containing the names of areas that are
#' signficantly lower and higher. Data and columns should map to the fingertipsR
#' layout
#'
#' This function takes a dataframe containing data on the areas to be compared.
#' This dataframe is filtered into a dataframe of the area to compare from
#' (from_data) and a dataframe of the area(s) to compare to (to_data). For each
#' of the rows in the from_data dataframe, the to_data is filtered on the
#' indicator, sex, age, category and time period, producing a dataframe of the
#' data for areas to compare to for the specific indicator breakdown. The
#' confidence intervals for the compared from area are then compared to the
#' values of the areas to be compared to. The compared areas are treated as
#' fixed values (see PHE Fingertips technical guidance for more details). These
#' comparisons create two columns, one for significantly higher values and one
#' for signficantly lower values.
#'
#' @param df A dataframe with columns matching those produced by the fingertipsR
#'   package
#' @param from_area The area code to be compared from (e.g. a Local Authority)
#' @param to_areas The areas codes to be compared to (e.g. similar areas)
#' @param colname The prefix to apply to the returned columns
#'
#' @return A dataframe containing the data for the area from compare to with two
#'   addtional columns containing names of areas compared to ('_higher' and
#'   '_lower'), prefexed by colname.
#'
#' @examples
#' comparetoarea(data, "E10000015", c("E07000095","E07000096", "E07000098"), "LTLA")
#' comparetoarea(data, "E10000015", nearest_neighbours(p_area, 102, "CIPFA"), "CIPFA")
#' 

comparetoareas <- function(df, from_area, to_areas, colname) {
  
  if (!is.data.frame(df)) {
    warning("df must be a dataframe")
  }
  
  requiredcols <- c("IndicatorID", "Sex", "Age", "Category", "Timeperiod",
                    "LowerCI95.0limit","UpperCI95.0limit")
  if (!all(requiredcols %in% colnames(df))) {
    warning("one or more required columns are not in the df" )
  }
  
  if (!from_area %in% df$AreaCode) {
    warning("area to compare from has not been found in df")
  } else {from_data = df[df$AreaCode == from_area, ]}
  
  if (ncol(data1[, to_areas %in% df$AreaCode]) == 0) {
    warning("areas to compare to have not been found in df")
  } else {to_data = df[df$AreaCode %in% to_areas, ]}

  from_data[, paste(c(colname, "_higher"), collapse="")] <- 
    mapply(function(f_IndicatorID, f_Sex, f_Age, f_Category, f_Timeperiod, f_UCI){
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
  }, from_data$IndicatorID, from_data$Sex, from_data$Age, from_data$Category,
  from_data$Timeperiod, from_data$UpperCI95.0limit)
  
  from_data[, paste(c(colname, "_lower"), collapse="")] <-
    mapply(function(f_IndicatorID,f_Sex, f_Age, f_Category,f_Timeperiod, f_LCI){
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
  }, from_data$IndicatorID, from_data$Sex, from_data$Age, from_data$Category,
  from_data$Timeperiod, from_data$LowerCI95.0limit)  
  
  return = from_data
}
