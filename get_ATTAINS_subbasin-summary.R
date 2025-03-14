
################################################################################
# Direct ATTAINS download for assessment information for AUs per subbasin.
# 
# Lily Conrad, IDEQ State Office
# last update: 3/14/2025
#
# Package citation: 
# Schramm, Michael (2021). rATTAINS: Access EPA 'ATTAINS' Data. R
# package version 1.0.0. doi:10.5281/zenodo.5469911
# https://CRAN.R-project.org/package=rATTAINS
################################################################################

# To get started, provide the user inputs below following the outlined format.


### User inputs ----------------------------------------------------------------

# Enter the eight digit HUC that you'd like to download data for. If you want to
# look at multiple HUCs, separate them by a comma inside the parentheses. 
huc <- c(17010305, 17010301)

# Enter your username (the name at the beginning of your computer's file explorer
# path) in quotations.
my_name <- "jdoe"


# Now that you've entered the values above, click on "Source" and watch
# your console for errors. If the script ran successfully, there will be a new
# Excel file for each HUC in your Downloads folder. 


################################################################################
#                                 START
################################################################################

### Load packages and data -----------------------------------------------------

my_packages <- c("dplyr", "tidyr", "xlsx", "rATTAINS")
install.packages(my_packages, repos = "http://cran.rstudio.com")

library(dplyr)
library(tidyr)
library(xlsx)
library(rATTAINS)


### Retrieve the ATTAINS data --------------------------------------------------

# Data query for all assessment units in the State and basic assessment unit information.            
AUs_State <- assessment_units(organization_id = "IDEQ", 
                              tidy = TRUE, 
                              .unnest = TRUE)

# Unnest locations to get the HUC8 code and clean up the table.
AUs_State <- AUs_State %>%
  unnest(locations, 
         names_repair = "check_unique") %>%
  select(assessment_unit_identifier, 
         assessment_unit_name,
         status_indicator, 
         water_type_code, 
         water_size_number, 
         units_code, 
         location_text) %>% 
  rename(HUC8 = location_text) %>%
  mutate(status_indicator = ifelse(status_indicator == "A", 
                                   "Active",
                                   ifelse(status_indicator == "R", 
                                          "Retired", 
                                          NA)))


# Pull out AUs and HUC8s to use later.
AU_HUCs <- AUs_State %>% 
  select(assessment_unit_identifier,
         status_indicator,
         HUC8) %>%
  unique()


# Data query for all assessment decision for each assessment unit. Data are
# reported from the current IR cycle. 
Assessments_State <- assessments(state_code = "ID")

# Select the list of interest, unnest pertinent columns, and clean up the table. 
Assessments_State <- Assessments_State$use_assessment %>% 
  as.data.frame() %>%
  select(reporting_cycle_text, 
         assessment_unit_identifier,
         use_attainments,
         parameters,
         EPA_IR_category, 
         overall_status, 
         cycle_last_assessed_text) %>% 
  unnest(use_attainments, # beneficial uses
         names_repair = "check_unique", 
         keep_empty = TRUE) %>% 
  unnest(parameters, # causes of impairment
         names_repair = "check_unique", 
         keep_empty = TRUE) %>%                        
  unnest(associated_uses, # indicator for if criteria is met or not
         names_sep = "_", 
         keep_empty = TRUE) %>% 
  select(reporting_cycle_text, 
         assessment_unit_identifier, 
         use_name, 
         parameter_name, 
         associated_uses_associated_use_name, 
         associated_uses_parameter_attainment_code,
         use_attainment_code_name, 
         EPA_IR_category, 
         overall_status, 
         cycle_last_assessed_text) %>%
  mutate(
    # If an AU is unassessed for fully supporting, no parameter should be listed 
    # in parameter_name.
    parameter_name = ifelse(use_attainment_code_name == "Not Assessed" | 
                                   use_attainment_code_name == "Fully Supporting", 
                                 NA, 
                                 parameter_name),
    # If the beneficial use is not the same as the impaired use, no parameter 
    # should be listed in parameter_name.   
    parameter_name = ifelse(use_name != associated_uses_associated_use_name, 
                                 NA, 
                                 parameter_name),
    # Qualifier column to remove duplicate beneficial use rows where one row holds
    # the cause of impairment and the other says NA.
    cleaning = ifelse(use_attainment_code_name == "Not Supporting" & is.na(parameter_name), 
                      "remove", 
                      NA)) %>%
  select(!associated_uses_associated_use_name) %>%
  unique() %>%
  filter(is.na(cleaning)) %>%
  select(!cleaning)


# Combine assessment decisions and basic assessment unit information into one table
# for all pollutants, assessment decisions, IR categories etc for all AUs in the state. 
Assessments_State <- merge(x = Assessments_State, 
                           y = AUs_State, 
                           by = "assessment_unit_identifier", 
                           all.x = TRUE) 

Assessments_State <- Assessments_State %>% 
  select(assessment_unit_identifier, 
         assessment_unit_name, 
         reporting_cycle_text, 
         cycle_last_assessed_text, 
         use_name, parameter_name, 
         use_attainment_code_name,
         associated_uses_parameter_attainment_code, 
         EPA_IR_category,
         overall_status, 
         status_indicator, 
         water_type_code, 
         water_size_number,
         units_code, 
         HUC8) %>%
  mutate(HUC8 = as.numeric(HUC8)) %>% 
  rename(parameter_attainment_code = associated_uses_parameter_attainment_code)



### Filter for the HUC(s) of interest ------------------------------------------

# Loop to save a seprate spreadsheet per subbasin. 
huc_list <- huc
for (i in huc_list) {
  
  # Dataframe with all AUs in the HUC(s)
  HUC_AUs.temp <- Assessments_State %>% 
    filter(HUC8 == i)
  
  # Summary dataframe of overall AU status and AU counts per category.
  HUC_OverallStatus.temp <- Assessments_State %>%
    filter(HUC8 == i) %>% 
    select(assessment_unit_identifier, 
           overall_status) %>%
    unique() %>%
    group_by(overall_status) %>% 
    summarise(beneficial_use_attainment = n())
  
  # Summary dataframe of beneficial status within AUs and AU counts per category.
  HUC_UseStatus.temp <- Assessments_State %>%
    filter(HUC8 == i) %>%
    select(assessment_unit_identifier, 
           use_name, 
           use_attainment_code_name) %>%
    group_by(use_name, 
             use_attainment_code_name) %>% 
    summarise(AU_count = n())
  
  HUC_UseStatus.temp <- as.data.frame(pivot_wider(test2, 
                                                  id_cols = use_attainment_code_name,
                                                  names_from = use_name,
                                                  values_from = AU_count))
  
  # Summary dataframe with causes of impairment and AU counts per pollutant.  
  HUC_Causes.temp <- Assessments_State %>%
    filter(HUC8 == i) %>%
    filter(use_attainment_code_name == "Not Supporting" & 
             parameter_attainment_code == "Not meeting criteria") %>% 
    select(assessment_unit_identifier, 
           use_name, 
           parameter_name) %>%
    group_by(use_name, 
             parameter_name) %>% 
    summarise(AU_count = n())
  
  HUC_Causes.temp <- pivot_wider(test3, 
                         id_cols = parameter_name,
                         names_from = use_name,
                         values_from = AU_count)
  
  # Summary dataframe of IR categories and AU counts per category. 
  HUC_IRCats.temp <- assessments.merge %>%
    filter(HUC8 == i) %>%
    select(assessment_unit_identifier, 
           EPA_IR_category) %>%
    unique() %>%
    group_by(EPA_IR_category) %>% 
    summarise(AU_count = n())
  
  # Dataframe of TMDLs and related documents that have been approved for the subbasin. 
  TMDLs.temp <- actions(organization_id = "IDEQ")$actions %>% # finalized actions with AUs and document code
    rename(assessment_unit_identifier = asessment_unit_identifier)
  
  HUC_TMDLs.temp <- merge(x = TMDLs.temp, 
                          y = AU_HUCs,
                          by = "assessment_unit_identifier",
                          all.x = TRUE)
  
  HUC_TMDLs.temp <- HUC_TMDLs.temp %>%
    filter(HUC8 == i) %>% 
    unnest(parameters, 
           names_repair = "check_unique", 
           keep_empty = TRUE) %>%
    select(action_identifier,
           action_name,
           action_type_code,
           action_status_code,
           completion_date,
           parameters_name,
           TMDL_date,
           HUC8) %>%
    rename(approval_date = TMDL_date) %>%
    as.data.frame() %>%
    unique()
  
  text <- data.frame(matrix(ncol = 8,
                            nrow = 1))
  
  colnames(text) <- c("action_identifier",
                      "action_name",
                      "action_type_code",
                      "action_status_code",
                      "completion_date",
                      "parameters_name",
                      "approval_date",
                      "HUC8")
  
  text <- text %>%
    mutate(action_identifier = ifelse(is.na(action_identifier), "Please note that older TMDLs did not have the information needed to attach the HUC and may not be included in this table. Always double check the list of approved TMDLs.",
                                      action_identifier))
  
  HUC_TMDLs.temp <- rbind(HUC_TMDLs.temp, text)
  
  # compile all dataframes into a spreadsheet with multiple tabs. 
  write.xlsx(HUC_AUs.temp, 
             file = paste0("C:/Users/",my_name,"/Downloads/",Sys.Date(),"_ATTAINS_","HUC",i,".xlsx"), 
             sheetName = "All AUs")
  write.xlsx(HUC_OverallStatus.temp, 
             file = paste0("C:/Users/",my_name,"/Downloads/",Sys.Date(),"_ATTAINS_","HUC",i,".xlsx"), 
             sheetName = "Summary - AU Overall Support", 
             append = TRUE)
  write.xlsx(HUC_UseStatus.temp, 
             file = paste0("C:/Users/",my_name,"/Downloads/",Sys.Date(),"_ATTAINS_","HUC",i,".xlsx"), 
             sheetName = "Summary - Beneficial Use Status", 
             append = TRUE)
  write.xlsx(HUC_Causes.temp, 
             file = paste0("C:/Users/",my_name,"/Downloads/",Sys.Date(),"_ATTAINS_","HUC",i,".xlsx"), 
             sheetName = "Summary - Causes of Impairment", 
             append = TRUE)
  write.xlsx(HUC_IRCats.temp, 
             file = paste0("C:/Users/",my_name,"/Downloads/",Sys.Date(),"_ATTAINS_","HUC",i,".xlsx"), 
             sheetName = "Summary - IR Categories", 
             append = TRUE)
  write.xlsx(HUC_TMDLs.temp, 
             file = paste0("C:/Users/",my_name,"/Downloads/",Sys.Date(),"_ATTAINS_","HUC",i,".xlsx"), 
             sheetName = "Summary - Approved TMDLs", 
             append = TRUE)
}


################################################################################
#                                 END
################################################################################



