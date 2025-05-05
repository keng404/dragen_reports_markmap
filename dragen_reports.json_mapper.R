library(rlog)
library(jsonlite)
options(stringsAsFactors=FALSE)
suppressPackageStartupMessages(library("argparse"))
## goal of this is to map a field to a DRAGEN file and understand properties of 
## that field in the DRAGEN HTML report
# workflowViews
# sampleViews
#########

### don't currently use this function, but could use this for refactoring code
parse_json <- function(list_of_interest){
  config_type = ""
  if(length(grepl("view",list_of_interest$type))>0){
    config_type = "views"
  } else{
    config_type = "sections"
  }
  full_path = paste(base_dir,config_type,list_of_interest$path,sep="/")
  y = jsonlite::read_json(full_path)
}
#######
find_other_fields_of_interest <- function(all_fields){
  fields_bool1 = grepl("path|source",all_fields)
  fields_bool2 = grepl("sampleSources|workflowSources",all_fields)
  found_fields = sum(fields_bool1 & !fields_bool2) > 0
  if(found_fields){
    field_subset = all_fields[fields_bool1 & !fields_bool2]
    fields_of_interest = unique(apply(t(field_subset),2,function(xx) strsplit(xx,"\\.")[[1]][1]))
    return(fields_of_interest)
  } else{
    rlog::log_error(paste("Could not find any fields of interest to parse",paste(all_fields,sep = ", ",collapse = ", ")))
    return(NULL)
  }
}
####
add_source_to_list <- function(sourcesList,key_list){
  sourcesList1 = sourcesList
  if(!key_list$name %in% names(sourcesList1)){
    new_key = key_list$name
    rlog::log_info(paste("Adding new_key:",new_key,"to sourcesList"))
    sourcesList1[[new_key]] = list()
    sourcesList1[[new_key]]$type = key_list$type
    sourcesList1[[new_key]]$file_ext = key_list$path
  } else{
    rlog::log_info(paste("Nothing to update"))
    return(sourcesList1)
  }
  return(sourcesList1)
}

metric_deep_map <- function(manifest_base_dir,key_lookup,metric_of_interest){
  key_lookup_split = strsplit(key_lookup,"/")[[1]]
  if(!"plots" %in% key_lookup_split){
    full_path = paste(manifest_base_dir,"metrics",key_lookup,sep="/")
  } else{
    full_path = paste(manifest_base_dir,"sections",key_lookup,sep="/")
  }
  rlog::log_info(paste("updating metrics path [",key_lookup, "] to",full_path))
  if(grepl(".json$",basename(full_path))){
    y = jsonlite::read_json(full_path)
    metrics_list = y$metrics
    if(metric_of_interest %in% names(metrics_list)){
      return(metrics_list[[metric_of_interest]])
    } else{
      rlog::log_info(paste("Could not find",metric_of_interest,"in",full_path))
      return(NULL)
    }
  } else{
    rlog::log_info(paste("Could not find",metric_of_interest,"in",full_path))
    return(NULL)
  }
}
sources_deep_map  <- function(manifest_base_dir,key_lookup,metric_of_interest,sourcesList){
  sourcesList1 = sourcesList
  key_lookup_split = strsplit(key_lookup,"/")[[1]]
  if(!"plots" %in% key_lookup_split){
    full_path = paste(manifest_base_dir,"metrics",key_lookup,sep="/")
  } else{
    full_path = paste(manifest_base_dir,"sections",key_lookup,sep="/")
  }
  rlog::log_info(paste("updating sources path [",key_lookup, "] to",full_path))
  if(grepl(".json$",basename(full_path))){
    y = jsonlite::read_json(full_path)
    metrics_list = y$metrics
    if(metric_of_interest %in% names(metrics_list)){
      if(length(y$workflowSources)>0){
        for(idx in 1:length(y$workflowSources)){
          sourcesList1 = add_source_to_list(sourcesList1, y$workflowSources[[idx]])
        }
      }
      if(length(y$sampleSources)>0){
        for(idx in 1:length(y$sampleSources)){
          sourcesList1 = add_source_to_list(sourcesList1, y$sampleSources[[idx]])
        }
      }
      return(sourcesList1)
    } else{
      rlog::log_info(paste("Could not find",metric_of_interest,"in",full_path))
      return(sourcesList1)
    }
  } else{
    rlog::log_info(paste("Could not find",metric_of_interest,"in",full_path))
    return(sourcesList1)
  }
}
#############
### update source data if we can
map_source_data <- function(base_dir,list_of_interest,sources_list,fields_to_update){
  list_of_interest1 = list_of_interest
  flattened_list_of_interest = unlist(list_of_interest1)
  for(update_idx in 1:length(fields_to_update)){
    field_to_update = fields_to_update[update_idx]
    field_to_update_namesplit = strsplit(field_to_update,"\\.")[[1]]
    reference_string = paste()
    for(zz in 1:length(field_to_update_namesplit)){
      reference_string = paste(reference_string,paste("[[","\"",field_to_update_namesplit[zz],"\"","]]",sep=""),sep="")
    }
    final_string = paste("list_of_interest1",reference_string,sep="")
    rlog::log_info(paste("Looking at ->",final_string))
    my_val = flattened_list_of_interest[[field_to_update]]
    rlog::log_info(print(my_val))
    if(my_val %in% names(sources_list)){
      data_source = sources_list[[my_val]]
      ##########################
      field_to_update_namesplit1 = field_to_update_namesplit[1:(length(field_to_update_namesplit)-1)]
      field_to_update_namesplit1 = c(field_to_update_namesplit1,"source_data")
      reference_string1 = paste()
      for(zz in 1:length(field_to_update_namesplit1)){
        reference_string1 = paste(reference_string1,paste("[[","\"",field_to_update_namesplit1[zz],"\"","]]",sep=""),sep="")
      }
      update_string1 = paste(paste("list_of_interest1",reference_string1,sep=""),"=","data_source")
      rlog::log_info(paste("Updating ->",update_string1))
      eval(parse(text = update_string1))
      ###################
      update_string2 = paste(paste("list_of_interest1",reference_string,sep=""),"=","NULL")
      eval(parse(text = update_string2))
    } else{
      
      if(grepl("\\.json$",basename(my_val))){
        rlog::log_warn(paste("Trying to update JSON path",reference_string,"my_val [",my_val,"]"))
        secondary_key = field_to_update_namesplit[length(field_to_update_namesplit)-1]
        deep_metric_map = metric_deep_map(base_dir,my_val,secondary_key)
        sources_list1 = sources_deep_map(base_dir,my_val,secondary_key,sources_list)
        sources_list = sources_list1
        new_value = deep_metric_map$path
        ############################
        if(is.null(new_value)){
          rlog::log_warn(paste("SKIPPING UPDATE for ",reference_string))
          next
        }
        if(new_value == ""){
          rlog::log_warn(paste("SKIPPING UPDATE for ",reference_string))
          next
        }
        ################################
        if(new_value %in% names(sources_list)){
          data_source = sources_list[[new_value]]
          ##########################
          field_to_update_namesplit1 = field_to_update_namesplit[1:(length(field_to_update_namesplit)-1)]
          field_to_update_namesplit1 = c(field_to_update_namesplit1,"source_data")
          reference_string1 = paste()
          for(zz in 1:length(field_to_update_namesplit1)){
            reference_string1 = paste(reference_string1,paste("[[","\"",field_to_update_namesplit1[zz],"\"","]]",sep=""),sep="")
          }
          update_string1 = paste(paste("list_of_interest1",reference_string1,sep=""),"=","data_source")
          rlog::log_info(paste("Updating ->",update_string1))
          eval(parse(text = update_string1))
          ###################
          update_string2 = paste(paste("list_of_interest1",reference_string,sep=""),"=","NULL")
          eval(parse(text = update_string2))
        } else{
          rlog::log_warn(paste("Cannot update",reference_string,"new_value [",new_value,"]"))
        }
      } else{
        rlog::log_warn(paste("Cannot update",reference_string,"my_val [",my_val,"]"))
      }
    }
  }
  return(list_of_interest1)
}
### flag items with path or source
second_pass_flagger <- function(list_of_interest){
  fields = names(unlist(list_of_interest))
  flag_fields = c("path","source")
  flag_bool1 = apply(t(fields),2,function(zz) grepl("path",zz) )
  flag_bool2 = apply(t(fields),2,function(zz) grepl("workflowSources|sampleSources",zz) )
  flag_bool = flag_bool1 & !flag_bool2
  if(sum(flag_bool)>0){
    rlog::log_warn(paste("Found fields to take a second look at",paste(fields[flag_bool],collapse = ", ",sep = ", ")))
    return(fields[flag_bool])
  } else{
    return(NULL)
  }
}

#########
# create parser object
parser <- ArgumentParser()
parser$add_argument("-i","--input-json","--input_json", default=NULL, required = TRUE,
                    help = "input JSON file --- DRAGEN reports manifest file")
parser$add_argument("-o","--output-dir","--output_dir", default=NULL, required = TRUE,
                    help = "output directory path where results will be written to")
args <- parser$parse_args()
#############################
input_json = args$input_json
output_dir = args$output_dir
######################
#input_json = "Downloads/dragen-reports-4.5.0_a.140/manifests/germline_wgs.json"
#for(report_type_idx in 1:length(report_types_jsons)){
#  input_json = report_types_jsons[report_type_idx]
  x = jsonlite::read_json(input_json)
  base_dir = dirname(input_json)
  keys_of_interest = c('workflowViews','sampleViews')
  if(!"workflowViews" %in% names(x)){
    rlog::log_error(paste("Could not find workflowViews in",input_json))
    stop()
  }
  sampleSourcesList = list()
  #### workflowViews iteration
  #items_of_interest = x$workflowViews
  items_of_interest = unique(c(x$workflowViews,x$sampleViews))
  dragenReportsList = list()
  dragenReportsList1 = list()
  keys_of_interest = c("path","source")
  for(i in 1:length(items_of_interest)){
    data_source_key = "path"
    view_name = items_of_interest[[i]]$name
    rlog::log_info(paste("VIEW_NAME:",view_name))
    config_type = ""
    if("type" %in% names(items_of_interest[[i]])){
      if(grepl("view",items_of_interest[[i]]$type)){
        config_type = "views"
      } else if(grepl("metric",items_of_interest$type)){
        config_type = "metric"
      }  else if(grepl("plot",items_of_interest$type)){
        config_type = "sections"
      } else{
        config_type = "sections"
      }
    }
    if(!"path" %in% names(items_of_interest[[i]])){
      rlog::log_info(paste("No files to parse for",view_name))
      #break
    }
    if(config_type != ""){
      full_path1 = paste(base_dir,config_type,items_of_interest[[i]]$path,sep="/")
    } else{
      full_path1 = paste(base_dir,items_of_interest[[i]]$path,sep="/")
    }
    if(grepl(".json$",basename(full_path1))){
      rlog::log_info(paste("Reading in full_path1:",full_path1))
      y = jsonlite::read_json(full_path1)
    } else{
      rlog::log_info(paste("full_path1 not a JSON:",full_path1))
      rlog::log_info(print(items_of_interest[[i]]))
      y = x
    }
    
    section_name = y$displayName
    sections = y$sections
    #### add-on to help with parsing edge-case objects
    if(!"sections" %in% names(y)){
      y = items_of_interest[[i]]
      section_name = y$displayName
      sections = y$sections
    }
    for(j in 1:length(sections)){
      z = sections[[j]]
      z_name = z$displayName
      config_type = ""
      if("type" %in% names(z)){
        if(grepl("view",z$type)){
          config_type = "views"
        } else if(grepl("metric",z$type)){
          config_type = "metric"
        } else{
          config_type = "sections"
        }
      }
      default_parse  = TRUE
      full_path2 = NULL
      #z1 = NULL
      if(!"path" %in% names(z)){
        rlog::log_warn(paste("No files to parse for object\n"))
        rlog::log_warn(paste("names",paste(names(z),collapse=", ",sep=", ")))
        rlog::log_warn(paste(z,collapse="\n",sep="\n"))
       ####
        default_parse = FALSE
      } else{
        if(config_type != ""){
          if("path" %in% names(z)){
            full_path2 = paste(base_dir,config_type,z$path,sep="/")
          }
        } else{
          if("path" %in% names(z)){
            full_path2 = paste(base_dir,z$path,sep="/")
          }
        }
        if(grepl('.json$',basename(full_path2))){
          rlog::log_info(paste("Reading in full_path2:",full_path2))
          z1 = jsonlite::read_json(full_path2)
        } else{
          rlog::log_error(paste("full_path2 is not a JSON:",full_path2))
          z1 = z
          ###rlog::log_error(print(z1))
        }
      }
      #############
     # if(is.null(z1)){
    #    z1 = z
    #  }
      ###
      # names(z1)
      #[1] "type"          "name"          "displayName"   "showNA"        "metrics"       "sampleSources"
      ### craft list for sample sources based on 'path' -- this alias 
      ### is linked to the specific metric/table that is displayed in the resulting HTML
      ### z1$metrics[[1]]$path === z1$sampleSources[[1]]$name
      #sampleSourcesList = list()
      #if(default_parse){

         # } 
       # } else{
          if(length(z$sampleSources) > 0 ){
              for(source_idx in 1:length(z$sampleSources)){
                rlog::log_info(paste("Looking at",source_idx,"of",length(z$sampleSources),"keys"))
                source_of_interest = z$sampleSources[[source_idx]]
                key = source_of_interest$name
                if(!key %in% names(sampleSourcesList)){
                  rlog::log_info(paste("Adding key:",key,"to sampleSourcesList"))
                  sampleSourcesList[[key]] = list()
                  sampleSourcesList[[key]]$type = source_of_interest$type
                  sampleSourcesList[[key]]$file_ext = source_of_interest$path  
                }
              }
          }
           # } else{
             if( length(z$workflowSources) > 0){
                for(source_idx in 1:length(z$workflowSources)){
                  rlog::log_info(paste("Looking at",source_idx,"of",length(z$workflowSources),"keys"))
                  source_of_interest = z$workflowSources[[source_idx]]
                  key = source_of_interest$name
                  if(!key %in% names(sampleSourcesList)){
                    rlog::log_info(paste("Adding key:",key,"to sampleSourcesList"))
                    sampleSourcesList[[key]] = list()
                    sampleSourcesList[[key]]$type = source_of_interest$type
                    sampleSourcesList[[key]]$file_ext = source_of_interest$path 
                  }
                }
             }
            #}
        #}
      ######################### ONE last try to get the data source
      #if(length(sampleSourcesList) < 1){
        if( length(x$sampleSources) > 0){
          for(source_idx in 1:length(x$sampleSources)){
            rlog::log_info(paste("Looking at",source_idx,"of",length(x$sampleSources),"keys"))
            source_of_interest = x$sampleSources[[source_idx]]
            key = source_of_interest$name
            if(!key %in% names(sampleSourcesList)){
              rlog::log_info(paste("Adding key:",key,"to sampleSourcesList"))
              sampleSourcesList[[key]] = list()
              sampleSourcesList[[key]]$type = source_of_interest$type
              sampleSourcesList[[key]]$file_ext = source_of_interest$path                         
            }
          }
        }
        #} else{
          if( length(x$workflowSources) > 0){
            for(source_idx in 1:length(x$workflowSources)){
              rlog::log_info(paste("Looking at",source_idx,"of",length(x$workflowSources),"keys"))
              source_of_interest = x$workflowSources[[source_idx]]
              key = source_of_interest$name
              if(!key %in% names(sampleSourcesList)){
                rlog::log_info(paste("Adding key:",key,"to sampleSourcesList"))
                sampleSourcesList[[key]] = list()
                sampleSourcesList[[key]]$type = source_of_interest$type
                sampleSourcesList[[key]]$file_ext = source_of_interest$path
              }
            }
          }
      ######################################################################
      if(length(y$sampleSources) > 0){
        for(source_idx in 1:length(y$sampleSources)){
          rlog::log_info(paste("Looking at",source_idx,"of",length(y$sampleSources),"keys"))
          source_of_interest = y$sampleSources[[source_idx]]
          key = source_of_interest$name
          if(!key %in% names(sampleSourcesList)){
            rlog::log_info(paste("Adding key:",key,"to sampleSourcesList"))
            sampleSourcesList[[key]] = list()
            sampleSourcesList[[key]]$type = source_of_interest$type
            sampleSourcesList[[key]]$file_ext = source_of_interest$path                         
          }
        }
      }
      #} else{
      if(length(y$workflowSources) > 0){
        for(source_idx in 1:length(y$workflowSources)){
          rlog::log_info(paste("Looking at",source_idx,"of",length(y$workflowSources),"keys"))
          source_of_interest = y$workflowSources[[source_idx]]
          key = source_of_interest$name
          if(!key %in% names(sampleSourcesList)){
            rlog::log_info(paste("Adding key:",key,"to sampleSourcesList"))
            sampleSourcesList[[key]] = list()
            sampleSourcesList[[key]]$type = source_of_interest$type
            sampleSourcesList[[key]]$file_ext = source_of_interest$path
          }
        }
      }
        #}
      #}
      
      #if(length(sampleSourcesList)<1 ){
        #### only set z1 if it hasn't been initialized at this point
        #rlog::log_info(paste("Initialize z1"))
        if(!"z1" %in% ls()){
          rlog::log_warn("Using parent object to obtain source data")
          z1 = z
        }
        if(length(z1$sampleSources) > 0){
          for(source_idx in 1:length(z1$sampleSources)){
            rlog::log_info(paste("Looking at",source_idx,"of",length(z1$sampleSources),"keys"))
            source_of_interest = z1$sampleSources[[source_idx]]
            key = source_of_interest$name
            if(!key %in% names(sampleSourcesList)){
              rlog::log_info(paste("Adding key:",key,"to sampleSourcesList"))
              sampleSourcesList[[key]] = list()
              sampleSourcesList[[key]]$type = source_of_interest$type
              sampleSourcesList[[key]]$file_ext = source_of_interest$path
            }
          }
        }
        #} else{
        if( length(z1$workflowSources) > 0){
          for(source_idx in 1:length(z1$workflowSources)){
            rlog::log_info(paste("Looking at",source_idx,"of",length(z1$workflowSources),"keys"))
            source_of_interest = z1$workflowSources[[source_idx]]
            key = source_of_interest$name
            if(!key %in% names(sampleSourcesList)){
              rlog::log_info(paste("Adding key:",key,"to sampleSourcesList"))
              sampleSourcesList[[key]] = list()
              sampleSourcesList[[key]]$type = source_of_interest$type
              sampleSourcesList[[key]]$file_ext = source_of_interest$path                         
            }
          }
        }
     # }
      #}
      
      no_data_source_mapping = FALSE
      if(length(sampleSourcesList) < 1){
        if(is.null(full_path2)){
          rlog::log_error(paste("Could not create mapping for full_path1",full_path1))
        } else{
          rlog::log_error(paste("Could not create mapping for full_path2",full_path2))
        }
        #break
        no_data_source_mapping = TRUE
      }
      if(no_data_source_mapping){
        if(is.null(full_path2)){
          rlog::log_info(paste("Skipping parsing for full_path1",full_path1))
        } else{
          rlog::log_info(paste("Skipping parsing for full_path2",full_path2))
        }
        #next
      } else{
        rlog::log_info(paste("data_sources_keys:",names(sampleSourcesList)))
      }
        ### for each metric --- record name of metric and description 
        ### ---- then tie metric to DRAGEN file
        ### craft metrics metadata
        ### primary key is section name
        ### secondary key is metric name, will store description
      #### only set z1 if it hasn't been initialized at this point
        metricsMetadataList = list()
        field_of_interest = NULL
        fields_of_interest = NULL
        if(!"metrics" %in% names(z1)){
          if(is.null(full_path2)){
            rlog::log_info(paste("Cannot parse through full_path1",full_path1,"of","metrics"))
          } else{
            rlog::log_info(paste("Cannot parse through full_path2",full_path2,"of","metrics"))
          }
          rlog::log_info(paste("other_fields_of_interest:",names(z1)))
          fields_of_interest = find_other_fields_of_interest(names(z1))
        } else{
          field_of_interest = "metrics"
        }
        ############################################
        if(!is.null(fields_of_interest)){
          if(length(fields_of_interest) > 1){
            rlog::log_info(paste("FOUND more than 1 field to parse --- not implemented yet",paste(fields_of_interest,sep=", ",collapse=", ")))
          }
          field_of_interest = fields_of_interest[1]
          rlog::log_info(paste("Parsing a different field ; not metrics", "[", field_of_interest,"]"))
        }
        parsing_needed = TRUE
        if(!is.null(field_of_interest)){
          if(field_of_interest == "source" | field_of_interest == "path"){
            rlog::log_info(paste("z1 is our parsed_object_to_update"))
            parsed_object_to_update = z1
            parsing_needed = FALSE
            num_objects_to_parse = 1
          } else{
            if(!is.null(full_path2)){
              rlog::log_info(paste("z1[[field_of_interest]] is our parsed_object_to_update"))
              parsed_object_to_update = z1[[field_of_interest]]
            } else{
              rlog::log_info(paste("z[[field_of_interest]] is our parsed_object_to_update"))
              parsed_object_to_update = z[[field_of_interest]]
            }
            num_objects_to_parse = length(parsed_object_to_update)
            ##### workaround to rescue assignment for parsed_object_to_update when it is NULL
            if(num_objects_to_parse == 0){
              if(!is.null(full_path2)){
                rlog::log_info(paste("z1 is our parsed_object_to_update"))
                parsed_object_to_update = z1
              } else{
                rlog::log_info(paste("z is our parsed_object_to_update"))
                parsed_object_to_update = z
              }
              parsing_needed = FALSE
              num_objects_to_parse = length(parsed_object_to_update)
            }
          }
        } else{
          rlog::log_info(paste("z1 is our parsed_object_to_update"))
          parsed_object_to_update = z1
          parsing_needed = FALSE
          num_objects_to_parse = 1
        }
        ##################
        rlog::log_info(paste("field_of_interest:",field_of_interest))
        ########################
        for(metrics_idx in 1:num_objects_to_parse){
          if(is.null(full_path2)){
            rlog::log_info(paste("parsing through full_path1",metrics_idx,"of",num_objects_to_parse, "metrics","in",full_path1))
          } else{
            rlog::log_info(paste("parsing through full_path2",metrics_idx,"of",num_objects_to_parse, "metrics","in",full_path2))
          }
          if(parsing_needed){
            metric_of_interest = parsed_object_to_update[[metrics_idx]]
          } else{
            metric_of_interest = parsed_object_to_update
          }
          ### some metrics are a combination of multiple metrics
          #primary_key = metric_of_interest$section
          #secondary_key = metric_of_interest$display
          secondary_key = NULL
          #metric_description = metric_of_interest$longDisplay
          metric_description = ""
          #DRAGEN_field = metric_of_interest$metric
          if(is.null(secondary_key)){
            if("metric" %in% names(metric_of_interest)){
              secondary_key = metric_of_interest$metric
            } else if('name' %in% names(metric_of_interest)){
              secondary_key = metric_of_interest$name 
            } else if('display' %in% names(metric_of_interest)){
              secondary_key = metric_of_interest$display 
            } 
          }
          if(is.null(secondary_key)){
            rlog::log_error(paste("Could not find secondary key"))
            rlog::log_error(print(metric_of_interest))
            ##metricsMetadataList[[]] = metric_of_interest
            next
            #break
          }
          ### copy and then delete 'path'
          ### some metrics are a combination of multiple metrics
          metricsMetadataList[[secondary_key]] = metric_of_interest
          if('path' %in% names(metric_of_interest)){
            if(!metric_of_interest$path %in% names(sampleSourcesList)){
              rlog::log_info(paste("Could not initially map ", secondary_key, "[",metric_of_interest$path,"]", "to a DRAGEN file via path field"))
              ##break
              deep_metric_map = metric_deep_map(base_dir,metric_of_interest$path,secondary_key)
              #####rlog::log_info(paste(print(deep_metric_map)))
              rlog::log_info(paste("Converting", "[",metric_of_interest$path,"]","to",deep_metric_map$path))
              sampleSourcesList1 = sources_deep_map(base_dir,metric_of_interest$path,secondary_key,sampleSourcesList)
              sampleSourcesList = sampleSourcesList1
              #if(!is.null(deep_metric_map$path)){
                metric_of_interest$path = deep_metric_map$path
                if(!"path" %in% names(deep_metric_map)){
                  rlog::log_info(paste("Appending data to",secondary_key))
                  metric_of_interest = append(metric_of_interest,deep_metric_map)
                  rlog::log_info(print(metric_of_interest))
                  metricsMetadataList[[secondary_key]] = metric_of_interest
                }
              #} 
              if(!is.null(metric_of_interest$path)){
                if(!metric_of_interest$path %in% names(sampleSourcesList)){
                  rlog::log_error(paste("Could not  map ", secondary_key, "[",metric_of_interest$path,"]", "to a DRAGEN file via path field"))
                  rlog::log_error(paste("source keys found:",paste(names(sampleSourcesList),sep=", ",collapse=", ")))
                }
              } else{
                metric_of_interest[["path"]] = NULL
              }
              
            } 
          }
          if('source' %in% names(metric_of_interest)){
            if(!metric_of_interest$source %in% names(sampleSourcesList)){
              rlog::log_info(paste("Could not initially  ", secondary_key, "[",metric_of_interest$source,"]", "to a DRAGEN file via source field"))
              ##break
            } 
          }
          #  else{
              if('path' %in% names(metric_of_interest)){
                metricsMetadataList[[secondary_key]]$source_data = sampleSourcesList[[metric_of_interest$path]]
                metricsMetadataList[[secondary_key]][["path"]] = NULL
              } else if ("source" %in% names(metric_of_interest)){
                metricsMetadataList[[secondary_key]]$source_data = sampleSourcesList[[metric_of_interest$source]]
                metricsMetadataList[[secondary_key]][["source"]] = NULL
              }
            #}
         # } else{
            ### some metrics are a combination of multiple metrics
            rlog::log_info(paste("parsing [",metric_of_interest$type,"]",metric_of_interest$display))
            metric_name = metric_of_interest$name
            metric_collection = metric_of_interest$metrics
            #if(!is.null(metric_collection)){
            #  rlog::log_info(print(metric_collection))
            #}
            ### some metrics are a combination of multiple metrics
            for(mult_metric_idx in 1:length(metric_collection)){
              if(sum("metric" %in% names(metric_collection[[mult_metric_idx]])) == 1){
                if("path" %in% names(metric_collection[[mult_metric_idx]])){
                  if(!metric_collection[[mult_metric_idx]]$path %in% names(sampleSourcesList)){
                    rlog::log_info(paste("Could not map initially [",metric_of_interest$type,"]", metric_collection[[mult_metric_idx]]$metric,"[",metric_collection[[mult_metric_idx]]$path,"]", "to a DRAGEN file via the field path"))
                   ## break
                    deep_metric_map = metric_deep_map(base_dir,metric_collection[[mult_metric_idx]]$path,metric_collection[[mult_metric_idx]]$metric)
                    rlog::log_info(paste("Converting", "[",metric_collection[[mult_metric_idx]]$path,"]","to",deep_metric_map$path))
                    sampleSourcesList1 = sources_deep_map(base_dir,metric_collection[[mult_metric_idx]]$path,metric_collection[[mult_metric_idx]]$metric,sampleSourcesList)
                    sampleSourcesList = sampleSourcesList1
                    #if(!is.null(deep_metric_map$path)){
                      metric_collection[[mult_metric_idx]]$path= deep_metric_map$path
                      if(!"path" %in% names(deep_metric_path)){
                        metric_collection[[mult_metric_idx]] = append(metric_collection[[mult_metric_idx]],deep_metric_map)
                      }
                    #}
                    if(!metric_collection[[mult_metric_idx]]$path %in% names(sampleSourcesList)){
                      rlog::log_info(paste("Could not map [",metric_of_interest$type,"]", metric_collection[[mult_metric_idx]]$metric,"[",metric_collection[[mult_metric_idx]]$path,"]", "to a DRAGEN file via the field path"))
                    }
                  }
                  if(metric_collection[[mult_metric_idx]]$path %in% names(sampleSourcesList)){
                    metric_collection[[mult_metric_idx]]$source_data  = sampleSourcesList[[metric_collection[[mult_metric_idx]]$path]]
                    metric_collection[[mult_metric_idx]]$path = NULL
                  }
              
                }
                if("source" %in% names(metric_collection[[mult_metric_idx]])){
                  if(!metric_collection[[mult_metric_idx]]$source %in% names(sampleSourcesList)){
                    rlog::log_info(paste("Could not map [",metric_of_interest$type,"]", metric_collection[[mult_metric_idx]]$metric,"[",metric_collection[[mult_metric_idx]]$source,"]", "to a DRAGEN file via the field source"))
                    ## break
                  } 
                  if(metric_collection[[mult_metric_idx]]$source %in% names(sampleSourcesList)){
                    metric_collection[[mult_metric_idx]]$source_data  = sampleSourcesList[[metric_collection[[mult_metric_idx]]$source]]
                  }
                  metric_collection[[mult_metric_idx]]$source = NULL
                }
              } else{
                ### some multiple metrics are a combination of multiple metrics 
                sub_metric_collection = metric_collection[[mult_metric_idx]]$metrics
                for(mult_sub_metric_idx in 1:length(sub_metric_collection)){
                  if("path" %in% names(sub_metric_collection[[mult_sub_metric_idx]])){
                    if(!sub_metric_collection[[mult_sub_metric_idx]]$path %in% names(sampleSourcesList)){
                      rlog::log_info(paste("Could not initially map  sub-metric [",metric_of_interest$type,"]", sub_metric_collection[[mult_sub_metric_idx]]$metric,"[",sub_metric_collection[[mult_sub_metric_idx]]$path,"]", "to a DRAGEN file via the field path"))
                      ## break
                      deep_metric_map = metric_deep_map(base_dir,sub_metric_collection[[mult_sub_metric_idx]]$path,sub_metric_collection[[mult_sub_metric_idx]]$metric)
                      rlog::log_info(paste("Converting","[",sub_metric_collection[[mult_sub_metric_idx]]$path,"]","to",deep_metric_map$path))
                      sampleSourcesList1 = sources_deep_map(base_dir,sub_metric_collection[[mult_sub_metric_idx]]$path,sub_metric_collection[[mult_sub_metric_idx]]$metric,sampleSourcesList)
                      sampleSourcesList = sampleSourcesList1
                      #if(!is.null(deep_metric_map$path)){
                        metric_collection[[mult_metric_idx]]$path = deep_metric_map$path
                        if(!"path" %in% names(deep_metric_map)){
                          metric_collection[[mult_metric_idx]] = append(metric_collection[[mult_metric_idx]],deep_metric_map)
                        }
                     # }
                      if(!sub_metric_collection[[mult_sub_metric_idx]]$path %in% names(sampleSourcesList)){
                        rlog::log_info(paste("Could not  map  sub-metric [",metric_of_interest$type,"]", sub_metric_collection[[mult_sub_metric_idx]]$metric,"[",sub_metric_collection[[mult_sub_metric_idx]]$path,"]", "to a DRAGEN file via the field path"))
                      }
                    }
                  }
                  if("source" %in% names(sub_metric_collection[[mult_sub_metric_idx]])){
                    if(!sub_metric_collection[[mult_sub_metric_idx]]$source %in% names(sampleSourcesList)){
                      rlog::log_info(paste("Could not map  sub-metric [",metric_of_interest$type,"]", sub_metric_collection[[mult_sub_metric_idx]]$metric,"[",sub_metric_collection[[mult_sub_metric_idx]]$source,"]", "to a DRAGEN file via the field source"))
                      ## break
                    } 
                  }
                  if(sum("path" %in% names(sub_metric_collection[[mult_sub_metric_idx]])) > 0){
                    sub_metric_collection[[mult_sub_metric_idx]]$source_data  = sampleSourcesList[[sub_metric_collection[[mult_sub_metric_idx]]$path]]
                    sub_metric_collection[[mult_sub_metric_idx]]$path = NULL
                  } else if(sum("source" %in% names(sub_metric_collection[[mult_sub_metric_idx]])) > 0){
                    sub_metric_collection[[mult_sub_metric_idx]]$source_data  = sampleSourcesList[[sub_metric_collection[[mult_sub_metric_idx]]$source]]
                    sub_metric_collection[[mult_sub_metric_idx]]$source = NULL
                  }
                  
                }
                metric_collection[[mult_metric_idx]] = sub_metric_collection
              }
            }
            metric_of_interest$metrics = metric_collection
          }
    ###############
        rlog::log_info(paste("keys_to_choose_from:",names(parsed_object_to_update)))
        rlog::log_info(paste("z_name:",z_name,"view_name:",view_name,"section_name:",section_name,"metric_name:",metric_name))
      #############
        if(sum(c("name","displayName") %in% names(parsed_object_to_update)) < 1){
          rlog::log_info(paste("Pulling info from parent object"))
          metricsMetadataList$showNA = y$showNA
          metricsMetadataList$name = y$name
          metricsMetadataList$displayName = y$displayName
        } else{
          metricsMetadataList$showNA = parsed_object_to_update$showNA
          metricsMetadataList$name = parsed_object_to_update$name
          metricsMetadataList$displayName = parsed_object_to_update$displayName
        }
        rlog::log_info(paste("Added [",metricsMetadataList$displayName,"]","length of",length(metricsMetadataList)))
        rlog::log_info(paste("To",view_name,"->",metricsMetadataList$displayName))
        if(!is.null(view_name) & !is.null(metricsMetadataList$displayName)){
          if(!metricsMetadataList$displayName %in% names(dragenReportsList[[view_name]])){
            dragenReportsList[[view_name]][[metricsMetadataList$displayName]] = metricsMetadataList 
          } else{
            newList = modifyList(dragenReportsList[[view_name]][[metricsMetadataList$displayName]],metricsMetadataList)
            dragenReportsList[[view_name]][[metricsMetadataList$displayName]]  = newList
          }
        } else{
          rlog::log_error(paste("Issue adding metadata for",view_name))
          rlog::log_error(print(metricsMetadataList))
          if(length(metricsMetadataList) > 0){
            if(! metricsMetadataList$name %in% names(dragenReportsList[[view_name]])){
              dragenReportsList[[view_name]][[metricsMetadataList$name]] = metricsMetadataList 
            } else{
              newList = modifyList(dragenReportsList[[view_name]][[metricsMetadataList$name]],metricsMetadataList)
              dragenReportsList[[view_name]][[metricsMetadataList$name]]  = newList
            }
          }
        }
    
    }
    #### more of a debugger for now
    fields_to_update = second_pass_flagger(dragenReportsList)
    ### update fields as a second pass if needed
    if(length(fields_to_update) > 0){
      dragenReportsList1 = map_source_data(base_dir,dragenReportsList,sampleSourcesList,fields_to_update)
      dragenReportsList = dragenReportsList1
    }
    #######################
    output_file = paste(output_dir,gsub(".json$",".dragen_reports.json",basename(input_json)),sep="/")
    rlog::log_info(paste("Writing out full DRAGEN report annotation to",output_file))
    jsonlite::write_json(dragenReportsList,path=output_file,auto_unbox=T,pretty=T)
  }
  
  #### more of a debugger for now
  fields_to_update = second_pass_flagger(dragenReportsList)
  ### update fields as a second pass if needed
  if(length(fields_to_update) > 0){
    dragenReportsList1 = map_source_data(base_dir,dragenReportsList,sampleSourcesList,fields_to_update)
    dragenReportsList = dragenReportsList1
  }
  #######################
  output_file = paste(output_dir,gsub(".json$",".dragen_reports.json",basename(input_json)),sep="/")
  rlog::log_info(paste("Writing out full DRAGEN report annotation to",output_file))
  jsonlite::write_json(dragenReportsList,path=output_file,auto_unbox=T,pretty=T)
  ###### SECOND PASS
  rlog::log_info(paste("Second pass on DRAGEN report annotation ",output_file))
  generatedDragenReportsList = jsonlite::read_json(output_file)
  fields_to_update = second_pass_flagger(generatedDragenReportsList)
  ### update fields as a second pass if needed
  if(length(fields_to_update) > 0){
    dragenReportsList1 = map_source_data(base_dir,generatedDragenReportsList,sampleSourcesList,fields_to_update)
    dragenReportsListFinal = dragenReportsList1
    output_file2 = paste(output_dir,gsub(".json$",".dragen_reports.v2.json",basename(input_json)),sep="/")
    rlog::log_info(paste("Rewrite out full DRAGEN report annotation to",output_file2))
    jsonlite::write_json(dragenReportsListFinal,path=output_file2,auto_unbox=T,pretty=T)
  } else{
    output_file2 = paste(output_dir,gsub(".json$",".dragen_reports.v2.json",basename(input_json)),sep="/")
    rlog::log_info(paste("No need to rewrite out full DRAGEN report annotation for",output_file))
    rlog::log_info(paste("RUNNING:","cp",output_file,output_file2))
    system(paste("cp",output_file,output_file2))
  }
  

  