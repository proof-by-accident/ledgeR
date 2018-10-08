library(tidyr)
library(dplyr)

#' Returns account stringth to a given depth
#'
#' @param string input account name
#' @param numeric desired account depth (defaults to null, which will return the original account)
#' @return string shortened account name
acct.at.depth <- function(acct, depth = NULL ){
    if (!is.character(acct)){
        stop( 'acct.at.depth() expected a string, but got something else' )
    }

    else { 
        accts <- regmatches(acct, gregexpr( "[A-z0-9[:space:]]+", acct))[[1]]
        ret <- paste( accts[ 1:min(depth,length(accts)) ], collapse = ":" )	
    }

    return( ret )
    
}

balance.journal <- function( journal, depth = NULL, date.start = as.Date('0001/01/01'), date.end = Sys.Date() ){
    df <- rbindlist(
        lapply( journal, function(x) {
            date <- x$date
            entries <- x$entries
            
            entries$date <- date

            return(entries)
        }
        )
    )

    df <- dplyr::filter(df, date.start <= date & date <= date.end )
    
    accts <- sapply( df$account, function(x) {acct.at.depth( as.character(x), depth )} )
    
    bal.df <- aggregate(df$amount, by=list(accts), FUN= function(x) {round(sum(x),2)})

    names(bal.df) <- c("account","bal")
    class(bal.df) <- append( class(bal.df), 'balsheet')

    
    
    attributes( bal.df )$depth = depth

    
    return( bal.df %>% filter( bal != 0.0 ) )
}


# implement pretty printing for balsheets

