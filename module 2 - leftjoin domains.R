source("module 0 - read all csv in a folder to dfs.R")
source("module 1 - tx_dm_ds to root_index.R")

domain_list <- Filter(function(x) is.data.frame(get(x)), ls(envir = globalenv()))

finding_domains <-domain_list[domain_list %in% finding_d_ref]
event_domains <-domain_list[domain_list %in% event_d_ref]
intervention_domains <- domain_list[domain_list %in% intervention_d_ref]
relationship_domains <- domain_list[domain_list %in% relationship_d_ref]
special_domains <- domain_list[domain_list %in% special_d_ref]
trial_design_domains <- domain_list[domain_list %in% trial_design_d_ref]

list = c("finding_domains","event_domains","intervention_domains","relationship_domains","special_domains")
for (domainlist in list){
  if (domainlist == "finding_domains"){
    for (domain in get(domainlist)){
      finding_domain_variables <- names(get(domain))
      if ("POOLID" %in% finding_domain_variables) {
        df <- root_index[, c("USUBJID", "SEX","SPGRPCD","GRPLBL")]
        pooldef<-merge(pooldef, df, by ="USUBJID")
        pooldef$STUDYID =NULL
        pooldef$USUBJID =NULL
        df <-merge(get(domain), pooldef, by ="POOLID" )
        assign(paste0(domain), df)
      }else{
        df <- merge(get(domain), root_index, by = "USUBJID")
        assign(paste0(domain), df)}
    }
  }else{
    for (domain in get(domainlist)){
        df <- merge(get(domain), root_index, by = "USUBJID")
        assign(paste0(domain), df)
    
          }
        }
}
domain_list <- Filter(function(x) is.character(get(x)), ls(envir = globalenv()))
rm(df)
rm(list = domain_list)