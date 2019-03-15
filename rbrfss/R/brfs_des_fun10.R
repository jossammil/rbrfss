#' A brfss brfs_des_fun10 calc function
#'
#' This function allows you to create the brfss survey design and the prevalence table
#' @param b_tab Do you love rbrfss? Defaults to TRUE.
#' @param b_var Do you love rbrfss? Defaults to TRUE.
#' @param by_var Do you love rbrfss? Defaults to TRUE.
#' @param num_years Do you love rbrfss? Defaults to TRUE.
#' @param years Do you love rbrfss? Defaults to TRUE.
#' @param finwt_use Do you love rbrfss? Defaults to TRUE.
#' @keywords rbrffs
#' @export
#' @examples
#' brfs_des_fun10()


brfs_des_fun10 <- function(b_tab, b_var, by_var = NULL, num_years = 5, years = NULL, 
                           finwt_use = FALSE) {
  b_tab_ch <- b_tab
  b_var_ch <- b_var
  
  #b_tab_ch <- "b_tab"
  #b_var_ch <- "b_var"
  #by_var_ch <- "by_var" 
  
  if(!is.null(by_var)) {
    by_var_ch <- by_var 
  }
  
  
  ### find years for variable of interest
  xvaryr <- as.data.frame.matrix(
    table(eval(parse(text = paste(b_tab_ch, "$", b_var_ch))), 
          eval(parse(text=paste(b_tab_ch, "$", "yr")))
    )
  ) 
  lvar <- lapply(xvaryr, sum)
  lvar2 <- which(lvar > 5)
  nvar <- names(lvar2)
  
  
  ### find years for by_variable of interest
  if (!is.null(by_var)) {
    xvaryr_by <- as.data.frame.matrix(
      table(eval(parse(text = paste(b_tab_ch, "$", by_var_ch))), 
            eval(parse(text=paste(b_tab_ch, "$", "yr")))
      )
    )  
    lvar_by <- lapply(xvaryr_by, sum)
    lvar2_by <- which(lvar_by > 5)
    nvar_by <- names(lvar2_by)
  }
  
  
  
  ###autoselect years available
  if(!is.null(by_var)) {
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
  x2 <- eval(parse(text = paste0(b_tab_ch, 
                                 "[c('", 
                                 b_var_ch, 
                                 "', ",
                                 "'xstate', ",
                                 "'yr', ",
                                 #"'ctyname', ",
                                 "'fips', ", 
                                 "'xpsu', ", 
                                 "'xststr', ", 
                                 "'xllcpwt'", ")]")))
  
  
  if (!is.null(by_var)){
    x2 <- eval(parse(text = paste0(b_tab_ch, "[c('", 
                                   b_var_ch, "', '",
                                   by_var_ch, "', ",
                                   "'xstate', ",
                                   "'yr', ",
                                   #"'ctyname', ",
                                   #"'FIPS', ", ### TO REMOVE DUPLICATES
                                   "'xpsu', ", 
                                   "'xststr', ", 
                                   "'xllcpwt'", ")]")))
  }
  
  
  
  # x2 <- subset(b_tab_ch, c(b_var_ch, by_var_ch, 
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
  
  if (!is.null(by_var)) {
    if (by_var_ch == "fips" | finwt_use == TRUE) {
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