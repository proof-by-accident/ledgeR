source('../ledgeR/R/read_ledger.R')
source('../ledgeR/R/balance_journal.R')

journ <- read.ledger('../crc.ledger')
bal <- balance.journal( journ, depth=2)

expenses <- filter( bal, grepl('Expenses', account) )
expenses <- expenses[ order(-expenses$bal), ]

expense_types <- sapply( expenses$account, function(x) { return( strsplit(as.character(x),':')[[1]][2] )} )
expenses$account <- factor( expense_types, levels = expense_types[ order(expenses$bal) ] )

pdf('expense_plot')
ggplot(expenses, aes(x=account,y=bal))  + geom_col(aes(fill=account)) + coord_flip()+ylab('Spending ($)')+xlab('')+guides(fill=FALSE)
dev.off()
