library(rlog)
dir_of_interest = "Downloads/dragen-reports-4.5.0_a.140"
output_dir = "dragen_reports-4.5.0_a.140_layout_mapping"
json_files = list.files(dir_of_interest,pattern="*json",recursive = T,full.names=T)
if(length(json_files) <1){
  stop(paste("Check [",dir_of_interest,"]","\nCould not find any JSON files here"))
}

### subset JSON files found in this directory to manifest jsons and the jsons that are found exactly 
### 1 level below the manifest folder
### we'll also store manifest jsons for debugging
manifest_jsons = json_files[apply(t(json_files),2,function(x) "manifests" %in% strsplit(x,"/")[[1]])]
##############################
manifest_jsons = manifest_jsons[apply(t(manifest_jsons),2,function(x) !"tests" %in% strsplit(x,"/")[[1]])]
#############################
report_types_jsons = manifest_jsons[apply(t(manifest_jsons),2,function(x) !"sections" %in% strsplit(x,"/")[[1]] & !"views" %in% strsplit(x,"/")[[1]])]
report_types_jsons = report_types_jsons[apply(t(report_types_jsons),2, function(x) basename(dirname(x)) == "manifests")]

################
for(i in 1:length(report_types_jsons)){
  full_cmd = c("Rscript dragen_reports.json_mapper.R","--input_json",report_types_jsons[i],"--output_dir",output_dir)
  rlog::log_info(paste("RUNNING:",paste(full_cmd,collapse = " ",sep = " ")))
  system(paste(full_cmd,collapse = " ",sep = " "))
}