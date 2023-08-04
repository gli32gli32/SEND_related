library(dplyr)
#load tx domain for testing purpose only

#TRANSFORM TX DOMAIN to an index dataframe
#capture unique TXPARMCD values in tx domain and save in a df_temp
txPARMCD_unique <- data.frame(A = unique(tx$TXPARMCD))
#In these unique PARMCD, only keep ARMCD, GRPLBL, TRTDOS and TRTDOSU
txPARMCD_unique <- subset(txPARMCD_unique, A %in% c("ARMCD","GRPLBL","SPGRPCD", "TRTDOS","TRTDOSU"))
#select the first value in a txPARMCD_unique
var_name  <-txPARMCD_unique$A[1]
#select those records in tx with TXPARMCD value equals to var_name value
tx_root <- subset(tx,TXPARMCD == var_name)
#change the name of TXVAL variable to var_name value
names(tx_root)[names(tx_root) == "TXVAL"] <- var_name
#assign variable names in tx_root to a list dataframe
tx_root <- tx_root[, names(tx_root) %in% c("SETCD", "SET", var_name)]
for (i in 2:nrow(txPARMCD_unique)) {
  var_name  <-txPARMCD_unique$A[i]
  df_temp <- subset(tx,TXPARMCD == var_name)
  names(df_temp)[names(df_temp) == "TXVAL"] <- var_name
  df_temp <- df_temp[, names(df_temp) %in% c("SETCD", var_name)]
  tx_root <-merge(tx_root, df_temp, by = "SETCD", all.x = TRUE)
  }
#TRANSFORM dm domain
dm_root <- dm[, names(dm) %in% c("USUBJID","SUBJID","SEX","SETCD","ARM")]
#TRANSFORM ds domain
ds_root <- ds[,names(ds) %in% c("USUBJID","DSDECOD","DSNOMDY")]
#merge dmroot, dsroot, txroot
root_index <- merge(dm_root, ds_root, by = "USUBJID", all.x =TRUE)
root_index <- merge(root_index,tx_root, by = "SETCD")
new_order <- c("USUBJID",
               "SUBJID",
               "SEX",
               "DSDECOD",
               "DSNOMDY",
               "SPGRPCD",
               "GRPLBL",
               "TRTDOS",
               "TRTDOSU",
               "ARMCD",
               "ARM",
               "SETCD",
               "SET")
root_index <- select(root_index,all_of(new_order))

finding_d_ref <- c("bw","bg","cl","dd","fw","lb","ma","mi","om","pm","pc","pp","sc","tf","eg","vs","cv","re")
intervention_d_ref <- "ex"
special_d_ref <- c("dm","co","se")
event_d_ref <- "ds"
trial_design_d_ref <-c("te","ta","ts","ts")
relationship_d_ref <-c("relrec","suppma","suppmi","supptf","suppcl","supppc","supppp")
other_d_ref <-"pooldef"

#remove intermediate df or value
rm(df_temp, txPARMCD_unique, i, var_name, new_order, tx_root, dm_root, ds_root)

