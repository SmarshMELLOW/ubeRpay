#' Make a table of payments from a batch of pay stubs
#'
#' This function takes pdfs of pay statement that are downloaded from uber
#'   https://partners.uber.com/p3/money/statements/all
#'   The settings must correspond to: portrait mode (i.e. wide), paper size A4,
#'   margins none. It returns a table of the trip data.
#' @param pdf_stub_list the pdf files with the payment info
#' @keywords uber accounting pdf
#' @import pdftools
#' @import stringr
#' @import magrittr
#' @import dplyr
#' @import plyr
#' @import lubridate
#' @export batch_table_trips()
#' @examples
#' batch_table_trips()

batch_table_trips <- function( pdf_stub_list ) {

  trip_table <- data_frame()

  for(i in 1:length(pdf_stub_list) ) {
    trip_table <- bind_rows( trip_table, table_trips( pdf_stub_list[i] ) )
  }

  # sometimes tips come in late, so add these to the original payments
  table_out <- trip_table %>%
    group_by( Date.Time, Trip_ID, Type ) %>%
    dplyr::summarise( Fare = sum(Fare),
               Tips = sum(Tips),
               Boost = sum(Boost),
               Total = sum(Total) )

  return(table_out)
}
