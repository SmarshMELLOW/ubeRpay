#' Read pdf pay statement and return trip of tables
#'
#' This function takes a pdf pay statement that is downloaded from uber
#'   https://partners.uber.com/p3/money/statements/all
#'   The settings must correspond to: portrait mode (i.e. wide), paper size A4,
#'   margins none. It returns a table of the trip data.
#' @param pdf_stub the pdf file with the payment info
#' @keywords uber accounting pdf
#' @import pdftools
#' @import stringr
#' @import magrittr
#' @import dplyr
#' @import plyr
#' @import lubridate
#' @export table_trips()
#' @examples
#' table_trips()

table_trips <- function( pdf_stub ) {

  txt <- pdf_text( pdf_stub )

  txt.all <- data_frame( )  # initialize the df of text
  for( i in 1: length(txt) ) {
    txt.page <- unlist( str_split(txt[i], "\\n") )

    for( j in 1:length(txt.page ) ) {
      txt.all <- bind_rows(
        txt.all, data_frame( line.txt = as.character( txt.page[j] ) )
      )
    }
  }

  # find the start of the earnings data and remove all lines above it
  for(k in 1:nrow(txt.all) ) {
    row.data <- unlist( str_split( txt.all[k,], " ") )
    row.data <- row.data[! row.data == ""]
    if( length(row.data) == 0 ) {  # avoid breaking on empty rows
      next
    }
    if( row.data[1] == "Date/Time") {
      earnings.start <- k
      break
    }
  }

  e.data <- txt.all[earnings.start:nrow(txt.all),]

  data.names <- unlist( str_split( e.data[1,], " ") )
  # are there miscellaneous items? If so then make a table of those
  if( "Misc." %in% data.names ) {
    misc.data <- table_misc( txt.all )
    misc.earn <- TRUE
  } else { misc.earn <- FALSE }

  data.names <- data.names[ ! data.names %in% c("", "ID", "Earnings", "Misc.")]
  data.names[1] <- "Date.Time"
  data.names[2] <- "Trip_ID"
  # note Boost is Earnings Boost

  # Will need these when not all data is populated
  data.names.5 <- data.names[ ! data.names %in% c("Tips", "Boost") ]
  data.names.6t <- data.names[ ! data.names == "Boost" ]
  data.names.6b <- data.names[ ! data.names == "Tips" ]

  if( ! "Tips" %in% c(data.names) ) {
    my.trips <- table_historical( e.data, incl.tips = TRUE )
  } else {
    #----- loop through each line in e.data to get the trips --------------------
    # set the new_page token
    new_page <- FALSE

    # initialize the return df
    my.trips <- data_frame( )

    for( i in 1:nrow(e.data) ) {
      deliv.data <- unlist( str_split( e.data[i,], " ") )
      deliv.data <- deliv.data[ ! deliv.data %in% ""]

      # different lengths correspond to different objects
      # length = 0: empty line, skip
      if( length( deliv.data ) == 0) {
        next
      }

      # new pages start with var names and then totals, skip these using new_page token
      if( substring( deliv.data[1], 1, 4) == "Date" ) {
        new_page <- TRUE
        next
      }

      # the first line just contains totals, so skip this
      if( new_page == TRUE ) {
        new_page <- FALSE
        next
      }

      # length = 4 and made it this far, then it is a date, get this value
      if( length( deliv.data ) == 4 ) {
        deliv.date <- paste( deliv.data[1], deliv.data[2], deliv.data[3], sep = " ")
        next
      }

      # if you made it this far, then the line is a delivery of length 6,7,8
      deliv.data[1] <- paste( deliv.date, deliv.data[1], deliv.data[2], sep = " ")
      deliv.data <- deliv.data[ -2 ]

      # make into df first for binding rows
      deliv.df <- as.data.frame( t( deliv.data ) )

      # length = 5 (6-1) if no tip and no boost, need to add
      if( ncol( deliv.df ) == 5) {
        names( deliv.df ) <- data.names.5
        deliv.df %<>% mutate( Tips = 0,
                              Boost = 0) %>%
          mutate_at( vars(Fare, Total),
                     funs( as.numeric( as.character(.))) ) %>%
          mutate_at( vars(Date.Time, Trip_ID, Type),
                     funs( as.character(.)) ) %>%
          dplyr::select( data.names )  # make sure it is in the right order
      }

      # length = 6 (7-1) missing either tip or boost, need to add
      if( ncol( deliv.df) == 6) {
        # The item in question, it5, could either be a tip or boost. If it is a
        #   tip, then there will be more white space between it5 and Total than
        #   between Fare and it5. The opposite will be true if it is Boost
        # 11/22/17- patch locate. Values can show up in Trip ID, so get all and
        #  use the last one.
        fare.pos <- str_locate_all(e.data[i,], deliv.data[4]) %>% as.data.frame()
        it5.pos <- str_locate_all(e.data[i,], deliv.data[5]) %>% as.data.frame()
        total.pos <- str_locate_all(e.data[i,], deliv.data[6]) %>% as.data.frame()
        white.1 <- it5.pos[nrow(it5.pos),1] - fare.pos[nrow(fare.pos),2]
        white.2 <- total.pos[nrow(total.pos),1] - it5.pos[nrow(it5.pos),2]

        # this won't work if it is a tip being added to a trip from a previous
        #   week because the total and the tip will be equal, so total.pos will
        #   return the position of it5
        if( deliv.data[5] == deliv.data[6] ) {
          names( deliv.df ) <- data.names.6t
          deliv.df %<>% mutate( Boost = 0)
        } else{
          if( white.1 > white.2 ) {
            # its boost...
            names( deliv.df ) <- data.names.6b
            deliv.df %<>% mutate( Tips = 0)
          } else {
            names( deliv.df ) <- data.names.6t
            deliv.df %<>% mutate( Boost = 0)
          }
        }

        deliv.df %<>%
          mutate_at( vars(Fare, Boost, Total),
                     funs( as.numeric( as.character(.))) ) %>%
          mutate_at( vars(Date.Time, Trip_ID, Type),
                     funs( as.character(.)) ) %>%
          dplyr::select( data.names )  # make sure it is in the right order
      }

      # length = 7 (8-1)-- all data, name and mutate to numeric
      if( ncol( deliv.df) == 7) {
        names( deliv.df ) <- data.names
        deliv.df %<>%
          mutate_at( vars(Fare, Boost, Total, Tips),
                     funs( as.numeric( as.character(.))) ) %>%
          mutate_at( vars(Date.Time, Trip_ID, Type),
                     funs( as.character(.)) )
      }

      my.trips <- bind_rows( my.trips, deliv.df )
    }

    my.trips %<>%
      mutate( Date.Time = mdy_hm( Date.Time ) )
  }

  # add misc earnings if they are present
  if( misc.earn == TRUE ) {
    my.trips <- bind_rows( my.trips, misc.data )
  }

  return( my.trips )
}

# -----------------------------------------------------------------------------
# This makes the table for stubs from before tipping --------------------------
table_historical <- function( txt_data, incl.tips = TRUE ) {

  data.names <- unlist( str_split( txt_data[1,], " ") )
  data.names <- data.names[ ! data.names %in% c("", "ID", "Earnings", "Misc.")]
  data.names[1] <- "Date.Time"
  data.names[2] <- "Trip_ID"
  data.names.5 <- data.names[ ! data.names == "Boost" ]

  # set the new_page token
  new_page <- FALSE

  # initialize the return df
  my.trips <- data_frame( )

  for( i in 1:nrow(txt_data) ) {
    deliv.data <- unlist( str_split( txt_data[i,], " ") )
    deliv.data <- deliv.data[ ! deliv.data %in% ""]

    # different lengths correspond to different objects
    # length = 0: empty line, skip
    if( length( deliv.data ) == 0) {
      next
    }

    # new pages start with var names and then totals, skip these using new_page token
    if( substring( deliv.data[1], 1, 4) == "Date" ) {
      new_page <- TRUE
      next
    }

    # the first line just contains totals, so skip this
    if( new_page == TRUE ) {
      new_page <- FALSE
      next
    }

    # length = 4 and made it this far, then it is a date, get this value
    if( length( deliv.data ) == 4 ) {
      deliv.date <- paste( deliv.data[1], deliv.data[2], deliv.data[3], sep = " ")
      next
    }

    # if you made it this far, then the line is a delivery of length 6,7,8
    deliv.data[1] <- paste( deliv.date, deliv.data[1], deliv.data[2], sep = " ")
    deliv.data <- deliv.data[ -2 ]

    # make into df first for binding rows
    deliv.df <- as.data.frame( t( deliv.data ) )

    # length = 5 (6-1) if no boost, need to add
    if( ncol( deliv.df ) == 5) {
      names( deliv.df ) <- data.names.5
      deliv.df %<>% mutate( Boost = 0) %>%
        mutate_at( vars(Fare, Total),
                   funs( as.numeric( as.character(.))) ) %>%
        mutate_at( vars(Date.Time, Trip_ID, Type),
                   funs( as.character(.)) ) %>%
        dplyr::select( data.names )  # make sure it is in the right order
    }

    # length = 6 (7-1)-- all data, name and mutate to numeric
    if( ncol( deliv.df) == 6) {
      names( deliv.df ) <- data.names
      deliv.df %<>%
        mutate_at( vars(Fare, Boost, Total ),
                   funs( as.numeric( as.character(.))) ) %>%
        mutate_at( vars(Date.Time, Trip_ID, Type),
                   funs( as.character(.)) )
    }

    my.trips <- bind_rows( my.trips, deliv.df )
  }

  my.trips %<>%
    mutate( Date.Time = mdy_hm( Date.Time ) )

  if( incl.tips == TRUE ) {
    my.trips %<>% mutate( Tips = 0) %>%
      select( Date.Time, Trip_ID, Type, Fare, Tips, Boost, Total)
  }
  return( my.trips )
}

# -----------------------------------------------------------------------------
# This makes a table for miscellaneous earnings -------------------------------
table_misc <- function( txt_df ) {

  # ----- select the miscellaneous payments portion of the stub ----------
  # find the start of the miscellaneous items and get the pay date
  for(i in 1:nrow(txt_df) ) {
    row.data <- unlist( str_split( txt_df[i,], " ") )
    row.data <- row.data[! row.data == ""]
    if( row.data[1] == "Period") {
      misc.date <- str_c(row.data[4:length(row.data)], collapse = " ")
    }
    if( row.data[1] == "Miscellaneous") {
      misc.start <- i
      break
    }
  }

  # find the end of the misc items
  for(j in misc.start:nrow(txt_df) ) {
    row.data <- unlist( str_split( txt_df[j,], " ") )
    row.data <- row.data[! row.data == ""]
    if( length(row.data) == 0 ) {
      next
    }
    if( row.data[1] == "Date/Time") {
      misc.end <- j
      break
    }
  }

  misc.data <- txt_df[misc.start:(misc.end-1),]

  #----- loop through the data and make a row for each item ----------
  misc.df <- data_frame()  # init df
  run.over <- 0 # init the skip token

  for( k in 1:nrow(misc.data) ) {

    misc.item <- unlist( str_split( misc.data[k,], " ") )
    misc.item <- misc.item[! misc.item == ""]
    item.len <- length( misc.item )

    if( item.len == 0 ) { next }  # skip empty lines

    if( misc.item[1] == "Miscellaneous" ) { next } # skip new page headers

    if( k <= run.over ) { next }  # skip runover lines


    if( is.na( as.numeric(misc.item[item.len]) ) == FALSE ) {
      misc.desc <- str_c(misc.item[1: item.len-1], collapse = " ")
      misc.pay <- as.numeric( misc.item[item.len])
    } else {
      # if the description is too long, it will run on to the next line and the
      #   payment will be on its own line. This deals with that
      misc.desc <- str_c(misc.item, collapse = " ")

      for(n in (k+1):nrow(misc.data)) {
        misc.item <- unlist( str_split( misc.data[n,], " ") )
        misc.item <- misc.item[! misc.item == ""]
        item.len <- length( misc.item )
        # get the payment
        if( item.len == 1 & is.numeric( as.numeric(misc.item)) == TRUE) {
          misc.pay <- as.numeric( misc.item[1] )
        }
        # now I want to just get rid of run on text on extra lines
        #  so I need to find where this descriptions ends
        if( item.len > 1 & is.na( as.numeric(misc.item[item.len])) == FALSE ) {
          run.over <- n
          break
        }
        # if it didn't break then it is the last item in the data
        run.over <- n
      }
    }


    # format data and add it to the misc.df
    misc.df <- bind_rows(
      misc.df,
      data_frame( Date.Time = mdy_hm(misc.date),
                  Trip_ID = substring(misc.desc,1,36),
                  Type = "Misc.", Fare = 0, Tips = 0, Boost = 0,
                  Total = misc.pay ) )
  }

  return( misc.df )
}


