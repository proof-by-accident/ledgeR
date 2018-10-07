library(data.table)
library(digest)

# some definitions (in descending order of inclusion):
# "entry": specifies a single quantity change to a single account, belongs in a...
# "transaction": a collection of entries, in addition it contains a date, title, flags, and rich text tags.  Entries in the same transaction should sum to 0.  Belongs in a... 
# "journal": a collection of transactions
# "balance sheet": a list of all account balances, calculated from a journal
# "ledger": a journal and a corresponding balance sheet

# an "entry" is a dataframe row with the following columns:
#     id - the transaction id to which the entry belongs
#     account - the account which is being paid from or to
#     amount - the amount value of the change (negative if this is a source entry, positive if it is a sink entry
#     currency - the unit currency which is being changed

# a "transaction" is a list with the following elements:
#     date - the date when the transaction occurred
#     title - the transaction title
#     id - the transaction id (a hash of the concatenated date and title)
#     flags - any flags for the transaction
#     entries - a data frame of entries belonging to the transaction
#     tags - a list of tags belonging to the transaction


read.ledger <- function(filename){
    
    # convert .ledger file to ledger.tmp, a csv file
    make.csv.cmd <- paste0('ledger csv -f ', filename)    
    journal.raw <- fread(make.csv.cmd, header=FALSE) # read in the journal as a .csv file
    names(journal.raw) <- c('date','V2','title','account','currency','amount','flags','tags')
    
    # create empty journal list
    journal <- list()
    class(journal) <- append( class(journal), "journal" )

    # we cannot determine the length of the final journal ahead of time, because an unknown number of csv lines belong to each transaction
    # instead we scrub through the csv and check the transaction id of each line
    # if the transaction id exists in the journal already then we just add the line to that transaction's entries
    # otherwise a new transaction is created with that id

    for (i in 1:dim(journal.raw)[1]){
        line <- journal.raw[i,]
        transaction.id <- digest( paste0( line$date, line$title ) )

        if (transaction.id %in% names(journal)){
            entry = data.frame(
                id = transaction.id,
                account = line$account,
                amount = as.numeric(as.character(line$amount)),
                currency = line$currency
            )
            class(entry) <- append(class(entry),'entry')

            transaction <- journal[[ transaction.id ]]
            transaction$flags[[ length(transaction$flags) + 1 ]] <- line$flags
            transaction$entries <- rbind( transaction$entries, entry )
            transaction$tags[[ length(transaction$tags) + 1 ]] <- line$tags

            journal[[ transaction.id ]] = transaction

        }

        else {
            entry <- data.frame(
                id = transaction.id,
                account = line$account,
                amount = as.numeric(as.character(line$amount)),
                currency = line$currency
            )
            class(entry) <- append(class(entry),'entry')

            transaction <- list(
                date = as.Date( line$date ),
                title = line$title,
                id = transaction.id,
                flags = list( line$flags ),
                entries = entry,
                tags = list( line$tags )
            )
            class(transaction) <- append( class(transaction), 'transaction' )

            journal[[ transaction.id ]] <- transaction

        }
        
            
    }

    # run over the assembled journal and clean up the flags and tags
    for (transaction.id in names(journal)){
        transaction <- journal[[ transaction.id ]]

        transaction$flags <- unique(unlist( transaction$flags ))
        transaction$tags <- unique(unlist(
            lapply( transaction$tags, function(t) {
                return( strsplit( t, split='\\n', fixed=TRUE ) )
            })
        ))

        journal[[transaction.id]] <- transaction
        
    }
    
    return( journal )
}
