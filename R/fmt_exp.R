#' Make expense part of 10k/10q
#'
#' This function reads a csv of expenses and formats them for an income
#'   statement.
#' @param exp_data dataframe of expenses
#' @param tenQ should the table be quarterly or annual ? Default to annual
#' @param date_var string. The purchase date variable name
#' @param cat_var string. Category of the purchse for detailed exp reports.
#' @param price_var variable containing the price
#' @param date_fmt string. The format of the date. See lubridate for options
#' @keywords uber accounting
#' @import magrittr
#' @import dplyr
#' @import plyr
#' @import lubridate
#' @export
#' @examples
#' fmt_exp( myexp.df, tenQ = TRUE)

fmt_exp <- function( exp_data, tenQ = FALSE, date_var, cat_var = NULL,
                     price_var, date_fmt = NA ) {

  # this section formats the data ---------------------------------------------
  # make sure there is no dollar sign in the price
  exp_data %<>% mutate_( "px" = as.character( price_var ) ) %>%
    mutate( px2 = gsub("$", "", px, fixed = TRUE),
            cost = as.numeric( px2 ) ) %>%
    filter( is.na( cost ) == FALSE )

  # does the date need to be formatted?
  if ( is.na(date_fmt) == FALSE ) {
    date_fmt <- tolower( date_fmt )
    exp_data %<>%
      mutate_( "purch_date.str" = as.character( date_var ),
               "purch_date" = paste0(date_fmt, "(", "purch_date.str",")" ) )
  }

  # if category is true, make sure that it is a string
  if( is.null( cat_var) == FALSE) {
    exp_data %<>% mutate_( "item" = as.character( cat_var) ) %>%
      group_by( item, add = TRUE )
  }

  # this section sums up the expenses by grouping input -----------------------
  # if 10q, make quarters and add to grouping list
  if( tenQ == TRUE ) {
    exp_data %<>% mutate( purch_qtr = lubridate::quarter( purch_date) ) %>%
      group_by( purch_qtr, add = TRUE)
  }

  # group data and summarize the results
  exp_table <- exp_data %>%
    dplyr::summarise( cost = sum(cost ))

  # when category = TRUE, item is factor.. not sure why. Make string
  if( is.null( cat_var) == FALSE ) {
    exp_table %<>% mutate( item = as.character( item ))
  }

  # split out into a table depending upon grouping criteria
  if( tenQ == TRUE ) {

    # category loop here...
    if( is.null( cat_var) == FALSE) {
      exp_cats <- table( exp_table$item ) %>% as.data.frame() %>% select(1)
      q_table <- data_frame( )

      for(c in 1:nrow(exp_cats) ) {
        q_row <- data_frame( item = as.character( exp_cats[c,]) )
        cat_table <- exp_table %>% filter( item == exp_cats[c,])

        for(q in 1:4 ) {
          q_cost <- cat_table %>%
            ungroup() %>%
            filter( purch_qtr == q) %>%
            select( cost )
          names( q_cost ) <- paste0("Q", q)

          # df will be empty if no purchases in cat dur period
          if( nrow(q_cost) == 0) { q_cost[1,1] <- 0 }

          q_row <- bind_cols( q_row, q_cost)
        }
        q_table <- bind_rows( q_table, q_row )
      }

    } else {
      # still need quarterly expenses
      q_table <- data_frame( item = "EXPENSES")
      for(q in 1:4 ) {
        q_cost <- exp_table %>%
          ungroup() %>%
          filter( purch_qtr == q) %>%
          select( cost )
        names( q_cost ) <- paste0("Q", q)

        # df will be empty if no purchases in cat dur period
        if( nrow(q_cost) == 0) { q_cost[1,1] <- 0 }

        q_table <- bind_cols( q_table, q_cost)
      }
    }
    exp_table <- q_table  # replace the object with the wide version
  }

  # add a line for total expenditure ------------------------------------------
  if (tenQ == FALSE) {
    if( is.null(cat_var) == TRUE) {
      exp_table %<>%
        plyr::mutate( item = "EXPENSES") %>%
        dplyr::select( item, cost )
    } else{
      exp_table <- bind_rows(
        exp_table,
        data_frame( item = "EXPENSES", cost = sum(exp_table$cost) )
      )
    }
  } else {
    if( is.null(cat_var) == FALSE) {
      exp_table <- bind_rows(
        exp_table,
        data_frame( item = "EXPENSES", Q1 = sum(exp_table$Q1),
                    Q2 = sum(exp_table$Q2), Q3 = sum(exp_table$Q3),
                    Q4 = sum(exp_table$Q4) ) )
    }
  }
  # %>%
  #   plyr::mutate( item = "EXPENSES") %>%
  #   dplyr::select( item, cost, everything())

  return( exp_table )
}

