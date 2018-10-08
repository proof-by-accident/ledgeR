#' Converts journal to a dataframe of entries
#'
#' @param journal input
#' @return dataframe giving rows corresponding to each element in input journal
#' @seealso \code{\link{read.journal}}
#' @export
#' @example

as.data.frame.journal <- function( journal  ){
	df <- data.frame()
	df <- data.frame()
	for (trans in journal){
            for (entry in trans$entries){
                new.row <- list( title=trans$title, date=trans$date, id=trans$id, acct=entry$acct.name, val=entry$value )
                new.row.lengths <- lapply(new.row, length)

                if ( any( new.row.lengths == 0) ) { new.row[ which( new.row.lengths == 0) ] <- NA }
                
                df <- rbind(df, new.row)	
            }
	}
	return (df)
}
