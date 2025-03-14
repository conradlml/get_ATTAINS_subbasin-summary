# get_ATTAINS_subbasin-summary
Script to query EPA's ATTAINS database to retrieve AU-beneficial use support status within a user-specified subbasin. The script only pulls ATTAINS data for Idaho assessment units and is compatible with querying multiple HUCs at a time. 

# Background Information
EPA's ATTAINS database houses the assessment status and outcomes of all Idaho surface water assessment units. This script utilizes the rATTAINS package developed and supported by EPA. 

# Getting Started 
To get started, begin by specifying the user inputs listed at the top of the file: eight digit HUC code, and your computer username. Be sure to follow the formatting guidelines provided within the script. Once the user-specified inputs are provided, click on "Source" and watch the console to see if you run into any errors. If the script ran successfully, there will be an Excel file for each HUC in your Downloads folder. Within the Excel file, there will be several sheets: 
* All AUs: a list of all the AUs and their status/assessments in the subbasin.
* Summary - AU Overall Support: overall AU support status for the subbasin with values reported as count of AUs.
* Summary - Beneficial Use Status: beneficial use statuses for each AU with values reported as count of AUs.
* Summary - Causes of Impairment: causes of impairment within the subbasin regardless of IR category with values reported as count of AUs.
* Summary - IR Categories: a summary of IR categories in the subbasin with values reported as count of AUs.
* Summary - Approved TMDLs: TMDLs that have been approved in the subbasin. Please note that older TMDLs that lacked the metadata to link the HUC code may not be captured in sheet 6. 

Always verify the results of your outputs. 
