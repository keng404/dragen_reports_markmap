library(rlog)
##test_data = jsonlite::read_json("dragen_reports-4.5.0_a.140_layout_mapping/germline_wgs.dragen_reports.v2.json")
##############
##all_keys = names(unlist(test_data))
get_keys_of_interest <- function(all_keys){
    keysplit = apply(t(all_keys),2,function(z) strsplit(z,"\\.")[[1]] )
    key_lengths = apply(t(all_keys),2,function(z) length(strsplit(z,"\\.")[[1]]) )
    keys_of_interest_check = apply(t(all_keys),2,function(z) strtoi(strsplit(z,"\\.")[[1]]) )
    ########## identify keys where we may want to rename or restructure nodes in the graph
    keys_of_interest = c()
    for(i in 1:length(keys_of_interest_check)){
        find_numbers = !is.na(keys_of_interest_check[[i]])
        if(sum(find_numbers) > 0){
            idxs_of_interest = (1:length(find_numbers))[find_numbers]
            if(length(idxs_of_interest) > 0){
                key_of_interest = NULL
                for(j in 1:length(idxs_of_interest)){
                    end_idx = idxs_of_interest[j]
                    key_of_interest = paste(keysplit[[i]][1:end_idx],collapse=".")
                    if(!is.null(key_of_interest)){
                        keys_of_interest = c(keys_of_interest,key_of_interest)
                    }
                }
            }
        }
    }
    keys_of_interest = unique(keys_of_interest)
    return(keys_of_interest)
}

#### find related keys to keys of interest
get_related_keys<- function(all_keys){
    related_keys = list()
    keysplit = apply(t(all_keys),2,function(z) strsplit(z,"\\.")[[1]] )
    key_lengths = apply(t(all_keys),2,function(z) length(strsplit(z,"\\.")[[1]]) )
    keys_of_interest_check = apply(t(all_keys),2,function(z) strtoi(strsplit(z,"\\.")[[1]]) )
    for(i in 1:length(keys_of_interest_check)){
        find_numbers = !is.na(keys_of_interest_check[[i]])
        if(sum(find_numbers) > 0){
            idxs_of_interest = (1:length(find_numbers))[find_numbers]
            full_length = length(find_numbers)
            if(length(idxs_of_interest) > 0){
                key_of_interest = NULL
                for(j in 1:length(idxs_of_interest)){
                    end_idx = idxs_of_interest[j]
                    key_of_interest = paste(keysplit[[i]][1:end_idx],collapse=".")
                    if(!is.null(key_of_interest) & full_length > max(idxs_of_interest)){
                        if(!key_of_interest %in% names(related_keys)){
                            related_keys[[key_of_interest]] = c(paste(keysplit[[i]][1:full_length],collapse="."))
                        } else{
                            related_keys[[key_of_interest]] = c(related_keys[[key_of_interest]],paste(keysplit[[i]][1:full_length],collapse="."))
                        }
                    }
                }
            }
        }
    }
    return(related_keys)
}
###########################

get_rename_candidates <- function(related_keys){
    rename_keys_list = list()
    ### check key length
    ### check for presence of naming fields
    naming_fields = c("metric","name","displayName","display")
    second_pass_keys = names(related_keys)
    for(i in 1:length(second_pass_keys)){
        second_pass_key = second_pass_keys[i]
        naming_keys = apply(t(naming_fields),2,function(z) paste(second_pass_key,z,sep="."))
        related_keys_subset = related_keys[[second_pass_key]]
        if(sum(naming_keys %in% related_keys_subset) > 0){
            rename_candidate = naming_keys[naming_keys %in% related_keys_subset][1]
            ###rlog::log_info(paste("RENAMING",second_pass_key,"to","[",rename_candidate,"]"))
            rename_keys_list[[second_pass_key]] = rename_candidate
        }
    }
    return(rename_keys_list)
}
###################
perform_rename <- function(og_data_list,rename_keys_list){
    og_data_list1 = og_data_list
    flattend_og_list = unlist(og_data_list)
    rename_keys = names(rename_keys_list)
    for(i in 1:length(rename_keys)){
        rename_key = rename_keys[i]
        rename_key_split = strsplit(rename_key,"\\.")[[1]]
        new_key = rename_keys_list[[rename_key]]
        new_key_split = strsplit(new_key,"\\.")[[1]]
        ### create key with new name
        field_to_update_namesplit1 = rename_key_split[1:(length(rename_key_split)-1)]
        field_to_update_namesplit1 = c(field_to_update_namesplit1,new_key_split[length(new_key_split)])
        reference_string1 = paste()
        for(zz in 1:length(field_to_update_namesplit1)){
            reference_string1 = paste(reference_string1,paste("[[","\"",field_to_update_namesplit1[zz],"\"","]]",sep=""),sep="")
        }
        object1 = paste("og_data_list1",reference_string1,sep="")
        
        field_to_update_namesplit2 = rename_key_split[1:(length(rename_key_split)-1)]
        field_to_update_namesplit2 = c(field_to_update_namesplit2,rename_key_split[length(rename_key_split)])
        reference_string2 = paste()
        for(zz in 1:length(field_to_update_namesplit2)){
            reference_string2 = paste(reference_string2,paste("[[","\"",field_to_update_namesplit2[zz],"\"","]]",sep=""),sep="")
        }
        object2 = paste("og_data_list1",reference_string2,sep="")
        
        
        ############
        #set_new_key = paste("new_key","=",object1)
        #rlog::log_info(paste("SETTING NEW_KEY:",set_new_key))
        #eval(parse(text = set_new_key))
        new_key = flattend_og_list[[new_key]]
        rlog::log_info(paste("SETTING NEW_KEY:",object1,"is",new_key))
        ###############
        field_to_update_namesplit3 = rename_key_split[1:(length(rename_key_split)-1)]
        field_to_update_namesplit3 = c(field_to_update_namesplit3,"new_key")
        reference_string3 = paste()
        for(zz in 1:length(field_to_update_namesplit3)){
          if(zz < length(field_to_update_namesplit3)){
            reference_string3 = paste(reference_string3,paste("[[","\"",field_to_update_namesplit3[zz],"\"","]]",sep=""),sep="")
          } else{
            reference_string3 = paste(reference_string3,paste("[[",field_to_update_namesplit3[zz],"]]",sep=""),sep="")
          }
        }
        object3 = paste("og_data_list1",reference_string3,sep="")        
        
        ############################
        update_string1 = paste(object3,"=",object2)
        ###og_data_list1[[new_key]] = og_data_list[[rename_key]]
        rlog::log_info(paste("Updating ->",update_string1))
        eval(parse(text = update_string1))
        
        ### delete old key
        update_string2 = paste(object2,"=","NULL")
        ###og_data_list1[[rename_key]] = NULL
        rlog::log_info(paste("Running",update_string2))
        eval(parse(text = update_string2))
    }
    return(og_data_list1)
}