library(stringr)
library(dplyr)
library(digest)

read.ledger <- function(filename){

    journal <- file( filename, "r" )

    source('./ledger_res.R')
    source('./acct_at_depth.R')
    source('./as_dataframe_journal.R')
    source('./balance.R')

    ## create empty journal list
    journal.parsed <- list()
    class(journal.parsed) <- append( class(journal.parsed), "journal" )

    #Pull first line, assuming headless .ledger file for now    
    transac.head <- readLines(journal, n=1)
    
    transac.lines <- str_trim( paste( regmatches( transac.head, regexpr( comment, transac.head ), invert=TRUE )[[1]], collapse="" ) )

    transac.meta.lines <- regmatches( transac.head, regexpr( comment, transac.head ) )

    parse.complete <- FALSE
    journal.ind <- 1
    while ( !parse.complete ) {    
        # pull lines into a transaction block until a new transaction is started
        new.transac = FALSE
        while( !new.transac ){
            line <- readLines(journal, n=1)
            # checks if line is of length 0, ie. if parsing is complete
            if ( length( line ) == 0 ) {
                parse.complete <- TRUE
                new.transac <- TRUE
            }
            
            # checks if line starts with a YYYY/MM/DD date, if so it stops pulling lines and holds the matching line to start a new transaction
            else if (grepl( paste("^[[:space:]]*",date, sep=""), line) ){
                transac.head <- line
                new.transac <- TRUE
            }

            # dump the line if it contains no text or is an ignored comment (any line starting with a comment character w/o preceeding whitespace)
            else if ( nchar(line) == 0 | grepl( ignore.comment, line ) ) {}
        
            # if line does not contain a non-commented date and is not an ignored comment then we seperate the line into commented and non-commented data
            else {
                transac.lines <- append( transac.lines, str_trim( paste(regmatches( line, regexpr( comment, line ), invert=TRUE )[[1]],collapse="") ) )
                transac.meta.lines <- append( transac.meta.lines, str_trim( regmatches( line, regexpr( comment, line ) ) ) )
            }
        }

    ## parse block of lines into an R transaction data type
    transac <- list()
    class(transac) <- append( class(transac), "transaction" )
    
    # pull info from transaction head (date, audit status, transaction title, and member email if one is present)    
    transac$date <- regmatches( transac.lines[1], regexpr( date, transac.lines[1] ) )
    transac$audit.flag <- regmatches( transac.lines[1], regexpr( audit.flag, transac.lines[1] ) )
    transac$title <- str_trim( paste( regmatches( transac.lines[1], gregexpr( paste(date,email,audit.flag,"[()]",sep="|") , transac.lines[1] ), invert=TRUE)[[1]], collapse="") )

    transac$email <- regmatches( transac.lines[1], regexpr( email, transac.lines[1] ) )
    
	#transac$id <- digest( paste( transac$date, transac$title, collapse = "" ) )
    transac$id <- digest( transac.lines )

    if ( length(transac$email) == 0 ) { transac$email <- NA }

    # pull transaction entries 
    transac$entries <- list()

    line.ind <- 1
    for (l in transac.lines[-(1)]){
        # dump any lines without text
        if ( nchar(l) == 0 ) { }

        # parse the transaction details
        else {
            entry <- list()
            class(entry) <- append( class(entry), "entry" )

            entry$acct.name <- regmatches( l, regexpr( acct.name, l ))
            entry$value <- as.double( gsub( "\\$", "", regmatches( l, regexpr(entry.val,l) ) ) )
	    entry$id <- transac$id
            transac$entries[[line.ind]] <- entry
            line.ind <- line.ind + 1            
            }

    }

    # pull transaction tags and notes
    transac$tags <- list()
    transac$notes <- list()

    tag.ind <- 1
    for (l in transac.meta.lines){
        if (nchar(l) == 0) {}

        else if ( grepl(tags,l) ) {
            transac$tags[[tag.ind]] <- list( tag=regmatches( l, regexpr(tags, l) ), value=str_trim(paste(regmatches( l, gregexpr(paste(tags,comment.start,sep="|"), l), invert=TRUE)[[1]], collapse=" ")) )
            tag.ind <- tag.ind + 1

        }

        else { transac$notes <- append( transac$notes, str_trim( gsub( ";", "",l) )) }
        
        }

    # go through entries, and if any are missing a value it'll infer the value from the remaining entries
    new.val <- 0 
    for (i in 1:length(transac$entries)){
	new.val = new.val + max(0, transac$entries[[i]]$value)
	} 
    missing.vals <- 0
	
    #for (i in 1:length(transac$entries)){
	#if (length(transac$entries[[i]]$value)==0) { print(new.val);transac$entries[[i]]$value <- -new.val; missing.vals <- missing.vals + 1 }

#	} 

    #if (missing.vals > 1) { print(paste('error: missing entry in transaction:', transac$date, transac$title, 'not inferable from context', sep=' ')); break() }

    # save the assembled transaction
    journal.parsed[[journal.ind]] <- transac
    journal.ind <- journal.ind + 1

    # reset line blocks
    transac.lines <- str_trim( paste( regmatches( transac.head, regexpr( comment, transac.head ), invert=TRUE )[[1]], collapse="" ) )
    transac.meta.lines <- regmatches( transac.head, regexpr( comment, transac.head ) )

}


ledger <- list()
class(ledger) <- append( class(ledger), 'ledger' ) 
ledger$journal <- journal.parsed
ledger$balance.sheet <- balance( journal.parsed )
return(ledger)
}


