##first define the function to apply
Chsq <- function(x){
  ## input is a row of your data
  ## creating a table from each row
  x <- matrix(x,byrow =TRUE,nrow=3)
  ### this will return the p value
  return(chisq.test(x)$p.value)
}
## Now apply this function
data = read.table ("test_chisq_allelefrq.txt", header=T, sep="\t",row.names=1)
## by using as.vector convert the output into a vector
P_Values <- as.vector(apply(data,1,Chsq))
result <- cbind(rownames(data),P_Values)
write.table (results,  file = "chisq-test_output.txt", append=F, quote = F, sep = "\t ",eol = "\n", na = "NA", dec = ".", row.names = F, col.names = T)

asthma2 <- NULL
for (i in 1:length(inter.asthma)) {
  if(inter.asthma[!is.na(inter.asthma)][[i]]==4) {
    asthma2[i] <- 1
  } else {
    asthma2[i] <- 0
  }
}