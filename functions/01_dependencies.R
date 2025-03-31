# install and/or load packages
packages = c("lubridate","tosca","writexl", "dplyr", "utf8", "ggplot2", "deeplr")
for(package in packages){
  load_package = T
  if(!require(package, quietly=T, character.only=T)){
    
    cat("Package", package, "wird benoetigt, ist aber nicht installiert. Soll es installiert werden?\n0: nein\n1: ja")
    install = as.logical(as.numeric(readline("Print 0 or 1:  ")))
    
    if(install) install.packages(package, dependencies = TRUE) else load_package = F
  }
  if(load_package){ library(package, character.only=TRUE) }
}
