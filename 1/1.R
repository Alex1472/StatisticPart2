smart_test <-  function(x){
   table <- table(x)
   if (min(table) < 5) {
     return(fisher.test(table)$p.value)
   } else {
     result <- chisq.test(table)
     return(c(result$statistic, result$parameter, result$p.value))
   }
}