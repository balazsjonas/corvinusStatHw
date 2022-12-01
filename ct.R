d<-data.frame(university=c(19,44,42), high_school=c(16,20,9) )#, elementary=c(12,31,7))
row.names(d)=c("low", "middle", "high")
print(d)
chisq.test(d) 
CV<-sqrt(chisq.test(d)$statistic/(sum(d)*(min(dim(d))-1)))
print(CV)


contingency_table <- d
ct_to_raw <- function(contingency_table) {
  dims <- dim(contingency_table)
  nrows <- dims[1]
  ncols <- dims[2]
  columns <- colnames(contingency_table)
  rows <- rownames(contingency_table)
  
  
  raw <- data.frame()#school=NA, level=NA)# list() # vector(mode='numeric', length= sum(contingency_table))
  for(col.ix in 1:ncols) {
    for(row.ix in 1:nrows) {
      new.row <- data.frame(school=columns[col.ix], level=rows[row.ix])
      for(i in 1:contingency_table[row.ix, col.ix]) {
        raw <- rbind(raw, new.row)
      }
    }
  }
  raw <- na.omit(raw)
  columns <- colnames(raw)
  for(col.ix in 1:length(columns)) {
    
    raw[,col.ix] <- as.factor(raw[,col.ix])
  }
  raw
}
raw_data <- ct_to_raw(d)
print(table(raw_data))
