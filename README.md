# BIDS_Dashboard
This is a BIDS App that reads the header files and creates you a Dashboard including study information and data overview.

## General function
1. Finds in a given folder all .json files. 
2. Extracts from each file the attributes and builds a data structure containing each unique attribute
3. Extraction and merging of information of from each attribute in the structure derived in step 2.
  - It is then stored in the file 'metadata.csv'
4. Dashboards are built on the information of the 'metadata.csv', as well as on 