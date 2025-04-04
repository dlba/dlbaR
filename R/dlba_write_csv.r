dlba_write_csv <- function(df, folder, filename, ext) {
  myfile <- file.path(folder, paste0(format(Sys.time(),"%Y-%m-%d"), #date header of file name
                                     filename,ext)) #name of file and extension
  
  write.csv(df, myfile)
  
}
