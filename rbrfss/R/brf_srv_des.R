#' A brfss brfs_srv_des survey design function
#'
#' This function allows you to create the brfss survey design and the prevalence table
#' @param brf_df A brfss raw data object as a dataframe (entered as a character).
#' @param variable The main variable of interest for summary calculations (entered as a character).
#' @param by_variable A secondary variable defining a subet for summary calculation (entered as character). 
#' @param num_years A integer specifying the years to include in aggregate for creating the survey design.
#' @param years The specific years to include from yr column in survey design (entered as combined character vector).
#' @param finwt_use If TRUE, uses a combined final weight calculated from aggregating years in weighting process. Defaults to FALSE.
#' @keywords rbrffs
#' @export
#' @examples
#' brfs_srv_des()


brf_srv_des <- function(brf_df, variable, by_variable = NULL, num_years = 5, years = NULL, 
                           finwt_use = FALSE) {
  brf_df_ch <- brf_df
  variable_ch <- variable
  
  #brf_df_ch <- "brf_df"
  #variable_ch <- "variable"
  #by_variable_ch <- "by_variable" 
  
  if(!is.null(by_variable)) {
    by_variable_ch <- by_variable 
  }
  
  
  ### find years for variable of interest
  xvaryr <- as.data.frame.matrix(
    table(eval(parse(text = paste(brf_df_ch, "$", variable_ch))), 
          eval(parse(text=paste(brf_df_ch, "$", "yr")))
    )
  ) 
  lvar <- lapply(xvaryr, sum)
  lvar2 <- which(lvar > 5)
  nvar <- names(lvar2)
  
  
  ### find years for by_variableiable of interest
  if (!is.null(by_variable)) {
    xvaryr_by <- as.data.frame.matrix(
      table(eval(parse(text = paste(brf_df_ch, "$", by_variable_ch))), 
            eval(parse(text=paste(brf_df_ch, "$", "yr")))
      )
    )  
    lvar_by <- lapply(xvaryr_by, sum)
    lvar2_by <- which(lvar_by > 5)
    nvar_by <- names(lvar2_by)
  }
  
  
  
  ###autoselect years available
  if(!is.null(by_variable)) {
    nvar1 <- intersect(nvar, nvar_by)
  } else {
    nvar1 <- nvar
  }
  
  
  nvar2 <- (sort(as.numeric(nvar1), decreasing = T))[1:num_years]
  nvar3 <- sort(as.character(nvar2[!is.na(nvar2)]))
  
  pullyears <- nvar3
  
  if(!is.null(years)){
    pullyears <- years
  }
  #yn <- nvar
  phrase1 <- paste(c("Data will be pulled from BRFSS years: ", pullyears), 
                   collapse = " ")
  message(phrase1)
  ### part where x2 dwindles the primary dataframe to variable, xPSU, xSTSTR, and finwt
  x2 <- eval(parse(text = paste0(brf_df_ch, 
                                 "[c('", 
                                 variable_ch, 
                                 "', ",
                                 "'xstate', ",
                                 "'yr', ",
                                 #"'ctyname', ",
                                 "'fips', ", 
                                 "'xpsu', ", 
                                 "'xststr', ", 
                                 "'xllcpwt'", ")]")))
  
  
  if (!is.null(by_variable)){
    x2 <- eval(parse(text = paste0(brf_df_ch, "[c('", 
                                   variable_ch, "', '",
                                   by_variable_ch, "', ",
                                   "'xstate', ",
                                   "'yr', ",
                                   #"'ctyname', ",
                                   #"'FIPS', ", ### TO REMOVE DUPLICATES
                                   "'xpsu', ", 
                                   "'xststr', ", 
                                   "'xllcpwt'", ")]")))
  }
  
  
  
  # x2 <- subset(brf_df_ch, c(variable_ch, by_variable_ch, 
  #                          "yr", "FIPS", "CTYName", "xPSU", "xSTSTR", "xLLCPWT"))
  x1 <- x2[which(x2$yr %in% pullyears),]
  tabx1yr <- as.data.frame(table(x1$yr))
  names(tabx1yr) <- c("yr", "freq")
  x1count <- nrow(x1)
  x1$finwt <- x1$xllcpwt
  fclist <- list()
  for (i in tabx1yr$yr){
    fclist[[i]]  <- (as.numeric(as.character(
      tabx1yr[tabx1yr$yr == i,"freq"] )) / x1count)
    x1$finwt <- ifelse(x1$yr == i, x1$xllcpwt * fclist[[i]], x1$finwt)
  }
  
  if (!is.null(by_variable)) {
    if (by_variable_ch == "fips" | finwt_use == TRUE) {
      bdes <- survey::svydesign(ids = ~xpsu, strata = ~xststr, weights = ~finwt, data = x1)
    } else {
      bdes <- survey::svydesign(ids = ~xpsu, strata = ~xststr, weights = ~xllcpwt, data = x1)
    }
  } else {
    if (finwt_use == TRUE) {
      bdes <- survey::svydesign(ids = ~xpsu, strata = ~xststr, weights = ~finwt, data = x1)
    } else {
      bdes <- survey::svydesign(ids = ~xpsu, strata = ~xststr, weights = ~xllcpwt, data = x1)
    }
    
  }
  
  
  bdes2 <- srvyr::as_survey_design(bdes)
  return(bdes2)
}