## define regexs
# regexs for pulling transac entries
entry.line <- "^[[:space:]]+([A-z0-9_[:space:]]*:*)+([A-z0-9_[:space:]]*)\\b([[:space:]]{2,}|\\t)-?\\$[[:digit:]]*\\.[[:digit:]]{2}" # matches any line containing a journal entry
acct.name <- "\\b\\w([A-z0-9_[:space:]]*:*)+([A-z0-9_[:space:]]*)\\b" # matches the account name format ACCT_1:ACCT_2:...:ACCT_N
entry.val <- "-?\\$[[:digit:]]*\\.[[:digit:]]{2}" # pulls dollar amount from transaction

# regexs for comments
comment.start <- "[;#%\\|]" # matches characters used to indicate the start of a comment
audit.flag <- "[!?*]" # matches audit flags
comment <- paste0("\\b[[:space:]]+",comment.start,".*") # matches a comment whose content we may want to pull (ie. membership info, date of registration, etc.)
ignore.comment <- paste0("^",comment.start,".*") # matches a comment that we want to ignore (ie. an untabbed one)

# regexs for titles
date <-"\\b[0-9]{4}/[0-9]{2}/[0-9]{2}\\b" # matches YYYY/MM/DD or YYYY/DD/MM formatted dates
commented.date <- paste0(comment.start,".+",date,".+") # tbh I dunno
email <- "[^[:space:]()]+@.+\\.(edu|com|org|net)"# matches standard email formats

# regexs for tags
tags <- c("Receipt:", ":EnteredOnRoster:", "PaypalTransactionID:", "MembershipExpires::") # currently hardcoding the tags that we care about, later this should be passed as an arg
tags <- paste( tags, collapse="|")
