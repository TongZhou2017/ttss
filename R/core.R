#' second structure summary
#' @description second structure summary
#' @import dplyr
#' @import stringr
#' @importFrom utils write.table
#' @param str second structure string
#' @param file_paired output file paired
#' @param file_unpaired output file unpaired
#' @export

ttss_summary <- function(str,file_paired,file_unpaired){
  anno <- str_split(str,"")[[1]]
  set <- list(set1 = c("{","}","[","]","(",")","<",">"), set2 = c("-","_",",",":"))
  tab <- data.frame(id=seq(length(anno)),anno=anno)
  for(i in 1:length(tab$anno)){if(tab$anno[i] %in% set$set1){tab$pair[i] = "Paired"}else{if(tab$anno[i] %in% set$set2){tab$pair[i] = "Unpaired"}else{tab$pair[i] = "-"}}}
  tab <- tab %>% arrange(pair,id)
  tab.paired <- tab %>% filter( pair == "Paired" )
  tab.unpaired <- tab %>% filter( pair == "Unpaired" )
  write.table(paste(tab.paired$id, collapse=","), file= file_paired, sep=",", row.names=F, col.names=F, quote=F)
  write.table(paste(tab.unpaired$id, collapse=","), file= file_unpaired, sep=",", row.names=F, col.names=F, quote=F)
}


