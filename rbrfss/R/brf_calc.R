#' A brfss full calc function
#'
#' This function combines the creation of the the brfss survey design (with year identification), variable recoding, and common summary means calculations.
#' @param brf_df A brfss raw data object as a dataframe (entered as a character).
#' @param variable The main variable of interest for summary calculations (entered as a character).
#' @param recode_statement A character text in CAR recode format for recoding the variable parameter. 
#' @param by_variable A secondary variable defining a subet for summary calculation (entered as character). 
#' @param by_variable_recode A character text in CAR recode format for recoding the by_variable parameter.
#' @param num_years A integer specifying the years to include in aggregate for creating the survey design.
#' @param years The specific years to include from yr column in survey design (entered as combined character vector).
#' @param finwt_use If TRUE, uses a combined final weight calculated from aggregating years in weighting process. Defaults to FALSE.
#' @param bin_filter A character match for quickly keeping/filtering a single level of the primary variable. Must match recode label.
#' @keywords rbrffs
#' @export
#' @examples
#' brf_calc()


brf_calc <- function(brf_df, variable, recode_statement = NULL, by_variable = NULL, 
                    by_variable_recode = NULL, num_years = 5, years = NULL, finwt_use = FALSE, bin_filter = NULL)  {
  
  brf_des <- brf_srv_des(brf_df, variable, by_variable, num_years, years, finwt_use)
  
  brf_srv_by(brf_des, variable, recode_statement, by_variable, by_variable_recode, bin_filter, finwt_use)
}