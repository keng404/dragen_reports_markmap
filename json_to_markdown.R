### convert JSON to markdown for markmap visualization
library(rlog)
library(jsonlite)
options(stringsAsFactors=FALSE)
suppressPackageStartupMessages(library("argparse"))
source("json_finalize_names.R")
#############
# create parser object
parser <- ArgumentParser()
parser$add_argument("-i","--input-json","--input_json", default=NULL, required = TRUE,
                    help = "input JSON file --- DRAGEN reports manifest file")
parser$add_argument("-o","--output-dir","--output_dir", default=NULL, required = FALSE,
                    help = "output directory path where markdown will be written to")
args <- parser$parse_args()
#############################
input_json = args$input_json
output_dir = args$output_dir
### set output directory to HOME if not configured
if(is.null(output_dir)){
  ##output_dir = Sys.getenv("HOME")
  output_dir = dirname(input_json)
}
#input_json = "/Users/keng/dragen_reports-4.5.0_a.140_layout_mapping/spatial.dragen_reports.json"
rlog::log_info(paste("Reading in",input_json))
json_data = jsonlite::read_json(input_json)

if(length(names(json_data))<0){
  rlog::log_error(paste("Issue parsing",input_json))
  rlog::log_error("stopping")
  stop()
}

#### perform renaming
all_keys = names(unlist(json_data))
keys_of_interest = get_keys_of_interest(all_keys)
if(length(names(keys_of_interest)) > 0){
  rlog::log_info(paste("Found some keys to rename:[",paste(keys_of_interest,sep=", "),"]"))
}
related_keys = get_related_keys(all_keys)
rename_candidates = list()
if(length(names(related_keys)) > 0) {
  rename_candidates = get_rename_candidates(related_keys)
}
if(length(names(rename_candidates)) > 0){
  json_data1 = perform_rename(json_data,rename_candidates)
  ###rlog::log_info(paste("RENAMED FIELDS",paste(names(unlist(json_data1)),collapse=", ")))
  output_file = paste(output_dir,gsub(".json$",".v3.json",basename(input_json)),sep="/")
  rlog::log_info(paste("Rewrite out full DRAGEN report annotation to",output_file))
  jsonlite::write_json(json_data1,path=output_file,auto_unbox=T,pretty=T)
  json_data = jsonlite::read_json(output_file)
  all_keys = names(unlist(json_data))
  rlog::log_info(paste("RENAMED FIELDS",paste(all_keys,collapse=", ")))
} else{
  rlog::log_info(paste("No renaming performed"))
}
#######################

fields_to_bold = c("path","source")
fields_ignore =  c("name","displayName")

##### Add boilerplate
##
##---
##title: markmap
##markmap:
##  initialExpandLevel: 2
##---
###########
markdown_lines = c()
basename_split = strsplit(basename(input_json),"\\.")[[1]]

### add boilerplate
markmap_title = paste(basename_split[1:(length(basename_split)-1)],collapse=" ")
markmap_boilerplate = paste("---\ntitle:",markmap_title,"\nmarkmap:\n colorFreezeLevel: 2\n initialExpandLevel: 2\n---\n")
markdown_lines = c(markdown_lines,markmap_boilerplate)

### add first line
first_line = paste("#",paste(basename_split[1:(length(basename_split)-1)],collapse=" "),"HTML layout\n",collapse=" ")
markdown_lines = c(markdown_lines,first_line)

############

levels_idx = 2
num_tabs = 1

for(subject_idx in 1:length(names(json_data))){
  current_subject = names(json_data)[subject_idx]
  add_line = paste(paste(rep("#",levels_idx),collapse = ""),current_subject,"\n")
  rlog::log_info(paste("adding subject line [",add_line,"]"))
  markdown_lines = c(markdown_lines,add_line)
  sub_topics = names(json_data[[current_subject]])
  for(sub_topic_idx in 1:length(sub_topics)){
    current_sub_topic = sub_topics[sub_topic_idx]
    add_line = paste("-",current_sub_topic)
    rlog::log_info(paste("adding sub-topic line v1 [",add_line,"]"))
    markdown_lines = c(markdown_lines,add_line)
    sub_topic_fields = names(json_data[[current_subject]][[current_sub_topic]])
    if(length(sub_topic_fields) == 0){
      if(json_data[[current_subject]][[current_sub_topic]] != ""){
        add_line = paste("-",current_sub_topic,":",json_data[[current_subject]][[current_sub_topic]])
        rlog::log_info(paste("adding sub-topic line v2 [",add_line,"]"))
        markdown_lines = c(markdown_lines,add_line)
      }
    } else{
      for(sub_topic_fields_idx in 1:length(sub_topic_fields)){
        num_tabs = 1
        current_sub_topic_field = sub_topic_fields[sub_topic_fields_idx]
        #########
        if(!current_sub_topic_field %in% fields_ignore){
          add_line = paste(rep("\t",num_tabs),"-",current_sub_topic_field)
          rlog::log_info(paste("adding sub-topic-item line v1 [",add_line,"]"))
          markdown_lines = c(markdown_lines,add_line)
        }
        #######
        sub_topic_fields_info = names(json_data[[current_subject]][[current_sub_topic]][[current_sub_topic_field]])
        if(length(sub_topic_fields_info) == 0){
          if(json_data[[current_subject]][[current_sub_topic]][[current_sub_topic_field]] != ""){
            add_line = paste(paste(rep("\t",num_tabs),collapse=""),"-",current_sub_topic_field,":",json_data[[current_subject]][[current_sub_topic]][[current_sub_topic_field]])
            rlog::log_info(paste("adding sub-topic-field line v1 [",add_line,"]"))
            markdown_lines = c(markdown_lines,add_line)
          }
        } else{
          num_tabs = 2
          flattend_sub_topic = unlist(json_data[[current_subject]][[current_sub_topic]][[current_sub_topic_field]])
          flattend_sub_topic_fields_to_add = names(flattend_sub_topic)
          ##### determine whether to bold or not
          bold_or_not1 = apply(t(flattend_sub_topic_fields_to_add),2,function(x) sum(fields_to_bold %in% x) > 0)
          bold_or_not2 = apply(t(flattend_sub_topic_fields_to_add),2,function(x) grepl("source_data|path|source",x))
          bold_or_not = bold_or_not1 | bold_or_not2
          #########
          for(j in 1:length(flattend_sub_topic_fields_to_add)){
            my_field = flattend_sub_topic_fields_to_add[j]
            if(flattend_sub_topic[[my_field]] != ""){
              add_line = paste(paste(rep("\t",num_tabs),collapse=""),"-",paste(my_field,":",flattend_sub_topic[[my_field]],sep=" "))
              ### add bold prefix and suffix to line if it needs to be bolded
              if(bold_or_not[j]){
                add_line = paste(paste(rep("\t",num_tabs),collapse=""),"-",paste("**",my_field,":",flattend_sub_topic[[my_field]],"**",sep=""))
              }
              rlog::log_info(paste("adding sub-topic-field line v2 [",add_line,"]"))
              markdown_lines = c(markdown_lines,add_line)
            }
          }
        }
      }
    }
  }
  markdown_lines = c(markdown_lines,"\n")
}
############# write output
output_name = paste(paste(basename_split[1:(length(basename_split)-1)],collapse="."),"markmap.md",sep=".")
output_file = paste(output_dir,output_name,sep="/")
rlog::log_info(paste("Creating markmap markdown file [",output_file,"]"))
write.table(markdown_lines,file=output_file,quote=F,col.names=F,row.names = F,sep = "\n")