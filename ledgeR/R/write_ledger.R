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


write.ledger <- function(journal, filename){
       

}
