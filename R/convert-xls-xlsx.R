#  Convert to xls to xlsx --------------------------------------------------
#' install.packages("RDCOMClient")
#' @references https://indranilgayen.wordpress.com/2016/05/11/batch-convert-xls-to-xlsx-using-r/

library(RDCOMClient)
convert_xls_to_xlsx<- function(in_folder,out_folder, delete_xls=F){
  
  if(missing(out_folder)){
    out_folder<- in_folder
  }
  
  all_xls<- list.files(in_folder, pattern = ".xls$")
  
  if(length(all_xls)>0){
    
    all_xls_out<- gsub(".xls$",".xlsx", all_xls)
    
    try({
      
      xls <- COMCreate("Excel.Application")
      
      lapply(1:length(all_xls), function(i){
        cat(i,"\n")
        wb = xls[["Workbooks"]]$Open( normalizePath(paste(in_folder, all_xls[i], sep="\\")) )
        wb$SaveAs( suppressWarnings( normalizePath(paste(out_folder, all_xls_out[i], sep="\\"))) , 51)
        wb$Close()
      })
      
      xls$Quit()
      
    }, silent = T)
    
    if(delete_xls){
      all_xlsx_now<- list.files(in_folder, pattern = ".xlsx$")
      test<- setdiff(gsub(".xls$","", all_xls), gsub(".xlsx$","", all_xlsx_now))
      if(length(test)==0){
        try(unlink(paste(in_folder, all_xls, sep="\\")),silent = T)
      }
    }
  }
  
  return(invisible(0))
}

