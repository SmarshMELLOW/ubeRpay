#' Make an income statement
#'
#' This function takes the delivery data and makes an income statement.
#'   Optionally, an expense sheet can be included. The statement can be
#'   either quarterly or annual (10k or 10q).
#' @param income_data dataframe of income data generated by table trips
#' @param tenQ should the table be quarterly or annual ? Default to annual
#' @keywords uber accounting
#' @import magrittr
#' @import dplyr
#' @import plyr
#' @import lubridate
#' @export
#' @examples
#' fmt_inc( mydeliveries.df, tenQ = TRUE)

fmt_inc <- function( income_data, tenQ = FALSE ) {

  # make the annual income statement
  if (tenQ == FALSE) {
    statement <- .inc_call( income_data )
  } else {
    income_data %<>% mutate( trip_mon = month( Date.Time ) )

    for( i in 1:4 ) {
      m_s <- 3 * i - 2
      m_e <- 3 * i
      inc.Q <- income_data %>% filter( trip_mon %in% c( m_s:m_e) ) %>%
        .inc_call( )

      if( i == 1) {
        names( inc.Q )[2] <- paste0("Q", i)
        statement <- inc.Q
      } else {
        inc.Q %<>% select( -item )
        names( inc.Q ) <- paste0("Q", i)
        statement <- bind_cols( statement, inc.Q )
      }
    }
  }
  return( statement )
}



# call to make the income section -----------------------------------
.inc_call <- function( income.df ) {

  # if df is empty return zeros
  if ( nrow(income.df) == 0) {
    inc.sec <- bind_rows(
      data_frame( item = "Fare", amt = 0 ),
      data_frame( item = "Boost", amt = 0 ),
      data_frame( item = "Tips", amt = 0 ),
      data_frame( item = "Misc", amt = 0),
      data_frame( item = "INCOME", amt = 0 ) )
  } else {
    # split out the misc items
    inc.df <- income.df %>% filter( Type != "Misc.")
    misc.df <- income.df %>% filter( Type == "Misc.")

    if( nrow( inc.df) != 0 ) {
      inc.sec <- bind_rows(
        data_frame( item = "Fare", amt = sum(inc.df$Fare) ),
        data_frame( item = "Boost", amt = sum(inc.df$Boost) ),
        data_frame( item = "Tips", amt = sum(inc.df$Tips) ) )

    } else {  # if no deliveries during period
      inc.sec <- bind_rows(
        data_frame( item = "Fare", amt = 0 ),
        data_frame( item = "Boost", amt = 0 ),
        data_frame( item = "Tips", amt = 0 ) )
    }
    # add in misc fees or bonuses if any
    if( nrow(misc.df) != 0) {
      inc.sec <- bind_rows(
        inc.sec,
        data_frame( item = "Misc", amt = sum(misc.df$Total) ) )
    } else {
      inc.sec <- bind_rows(
        inc.sec,
        data_frame( item = "Misc", amt = 0) )
    }

    # add in the total
    inc.sec <- bind_rows(
      inc.sec,
      data_frame( item = "INCOME", amt = sum(income.df$Total) ) )
  }

  return(inc.sec)
}
