# set wd
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# read script files
script_files = list.files(path="functions", pattern=".R", full.names=T)
for(file in script_files) source(file)
