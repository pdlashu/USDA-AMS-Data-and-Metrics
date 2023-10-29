setwd("D:/agecon_masters_uga/summer_sem/data_visualization_challenge/data_wrangling")

library(tidyverse)
library(janitor)
library(writexl)

df <- read.delim("D:/agecon_masters_uga/summer_sem/data_visualization_challenge/data_wrangling/2017_cdqt_data.txt")
df <- clean_names(df) #apply synchronous change to all the variable names(i.e it makes it lowecase sep by _)


################################# CALIFORNIA #################################################
################################# CALIFORNIA #################################################


ca <- filter(df, state_name == 'CALIFORNIA') #extract all the observations for California 

#unique(df$state_name) # get all the unique entries in the "state_name" variable
# names(df) ---- gives the variable names in a df
#colnames(df) ---- gives the variable names in a df

#1.extract total number of agland operations in california counties
ca_agland_num <- ca%>%
  filter(census_chapter == 2, census_table == 1, short_desc == 'AG LAND, CROPLAND - NUMBER OF OPERATIONS')%>%
  dplyr::select(state_name, county_name, agland_num = value)

#2 extract total agland acres in california counties 
ca_agland_acres <- ca%>%
  filter(census_chapter == 2, census_table == 1, short_desc == 'AG LAND, CROPLAND - ACRES')%>%
  dplyr::select(county_name, agland_acres= value)

#3 extract total harvested agland acres
ca_agland_harv_acres <- ca%>%
  filter(census_chapter == 2, census_table == 1, short_desc == 'AG LAND, CROPLAND, HARVESTED - ACRES')%>%
  dplyr::select(county_name, agland_harv_acres= value)

#4 extract total operations (cattle, hog, and sheep)

ca_chs_num <- ca %>%
  filter(census_chapter == 2, census_table == 1, short_desc == 'CATTLE, INCL CALVES - OPERATIONS WITH INVENTORY' | short_desc == 'HOGS - OPERATIONS WITH INVENTORY'|short_desc == 'SHEEP, INCL LAMBS - OPERATIONS WITH INVENTORY')%>%
  pivot_wider(id_cols = c(county_name, state_name), names_from = short_desc, values_from = value) %>%
  clean_names() %>% 
  mutate(
    cattle_incl_calves_operations_with_inventory = ifelse(is.na(cattle_incl_calves_operations_with_inventory), 0, cattle_incl_calves_operations_with_inventory),
    hogs_operations_with_inventory = ifelse(is.na(hogs_operations_with_inventory), 0, hogs_operations_with_inventory),
    sheep_incl_lambs_operations_with_inventory =   ifelse(is.na(sheep_incl_lambs_operations_with_inventory), 0, sheep_incl_lambs_operations_with_inventory)
  )%>%
  mutate(
    cattle_incl_calves_operations_with_inventory = as.numeric(gsub(",", "",cattle_incl_calves_operations_with_inventory, fixed = TRUE)),
    hogs_operations_with_inventory= as.numeric(gsub(",", "",hogs_operations_with_inventory, fixed = TRUE)),
    sheep_incl_lambs_operations_with_inventory =   as.numeric(gsub(",", "",sheep_incl_lambs_operations_with_inventory, fixed = TRUE))
  )%>%
  mutate(
    chs_num = cattle_incl_calves_operations_with_inventory + hogs_operations_with_inventory + sheep_incl_lambs_operations_with_inventory
  ) %>% 
  dplyr::select(county_name, chs_num)


#5. extract total farm operating expense in $

ca_total_expense <- ca %>%
  filter(census_chapter == 2, census_table == 3, short_desc == 'EXPENSE TOTALS, OPERATING - EXPENSE, MEASURED IN $') %>% 
  dplyr::select(county_name, total_expense= value)

#6. extracting fuel operating expense in $
ca_fuel_expense <- ca %>%
  filter(census_chapter == 2, census_table == 3, short_desc == 'FUELS, INCL LUBRICANTS - EXPENSE, MEASURED IN $') %>% 
  dplyr::select(county_name, fuel_expense = value)

#7. acres treated with chemical fertilizer (excl pastureland)
ca_cf_excpasture_acre = ca %>%
  filter(census_chapter == 2, census_table == 40, short_desc == 'AG LAND, CROPLAND, (EXCL PASTURED) - TREATED, MEASURED IN ACRES', domaincat_desc == 'FERTILIZER: (TOTAL)') %>% 
  dplyr::select(county_name, cf_excpasture_acre= value)

#8. acres treated with insecticides(excl nematicides)
ca_insect_excnem_acre = ca %>%
  filter(census_chapter == 2, census_table == 40, short_desc == 'AG LAND - TREATED, MEASURED IN ACRES', domaincat_desc == 'CHEMICAL, INSECTICIDE: (EXCL NEMATICIDES)') %>% 
  dplyr::select(county_name, insect_excnem_acre= value)

#9. acres treated with herbicide 
ca_herbi_acre = ca %>%
  filter(census_chapter == 2, census_table == 40, short_desc == 'AG LAND - TREATED, MEASURED IN ACRES', domaincat_desc == 'CHEMICAL, HERBICIDE: (TOTAL)') %>% 
  dplyr::select(county_name, herbi_acre= value)

#10 acres treated with fungicide
ca_fungi_acre = ca %>%
  filter(census_chapter == 2, census_table == 40, short_desc == 'AG LAND - TREATED, MEASURED IN ACRES', domaincat_desc == 'CHEMICAL, FUNGICIDE: (TOTAL)') %>% 
  dplyr::select(county_name, fungi_acre= value)

#11. acres under conservation easement 
ca_conserve_acre <- ca %>%
  filter(census_chapter == 2, census_table == 41, short_desc == 'PRACTICES, LAND USE, CONSERVATION EASEMENT - ACRES') %>% 
  dplyr::select(county_name, conserve_acre= value)

#12. acres under no till 
ca_notill_acre <- ca %>%
  filter(census_chapter == 2, census_table == 41, short_desc == 'PRACTICES, LAND USE, CROPLAND, CONSERVATION TILLAGE, NO-TILL - ACRES') %>% 
  dplyr::select(county_name, notill_acre= value)

#13 number of farms practicing rotational or mgmt intensive grazing
ca_rgraze_num <- ca %>%
  filter(census_chapter == 2, census_table == 43, short_desc == 'PRACTICES, ROTATIONAL OR MGMT INTENSIVE GRAZING - NUMBER OF OPERATIONS') %>% 
  dplyr::select(county_name, rgraze_num= value)

#14. number of farms practicing alley cropping and silvapasuture
ca_alleysilva_num <- ca %>%
  filter(census_chapter == 2, census_table == 43, short_desc == 'PRACTICES, ALLEY CROPPING & SILVAPASTURE - NUMBER OF OPERATIONS') %>% 
  dplyr::select(county_name, alleysilva_num = value)

#15.number of  organic farms (organic, exempt, and transitioning)
ca_organic_num <- ca %>%
  filter(census_chapter == 2, census_table == 42, short_desc == 'FARM OPERATIONS, ORGANIC - NUMBER OF OPERATIONS'|short_desc == 'AG LAND, ORGANIC, TRANSITIONING - NUMBER OF OPERATIONS' )%>%
  pivot_wider(id_cols = c(state_name, county_name), names_from = domaincat_desc, values_from = value) %>%
  clean_names() %>%
  mutate(
    organic_status_nop_usda_certified = ifelse(is.na(organic_status_nop_usda_certified), 0, organic_status_nop_usda_certified),
    organic_status_nop_usda_exempt = ifelse(is.na(organic_status_nop_usda_exempt), 0, organic_status_nop_usda_exempt),
    organic_status_transitioning =   ifelse(is.na(organic_status_transitioning), 0, organic_status_transitioning)
    )%>%
  mutate(
    organic_status_nop_usda_certified = as.numeric(gsub(",", "",organic_status_nop_usda_certified, fixed = TRUE)),
    organic_status_nop_usda_exempt = as.numeric(gsub(",", "",organic_status_nop_usda_exempt, fixed = TRUE)),
    organic_status_transitioning =   as.numeric(gsub(",", "",organic_status_transitioning, fixed = TRUE))
  )%>%
  mutate(
    organic_cet_num = organic_status_nop_usda_certified + organic_status_nop_usda_exempt + organic_status_transitioning
  ) %>% 
  dplyr::select(county_name, organic_num= organic_cet_num)  

#16 extract acres of plant planted to cover crops 
ca_cover_acre <- ca %>% 
  filter(census_chapter == 2, census_table == 41,
         short_desc == 'PRACTICES, LAND USE, CROPLAND, COVER CROP PLANTED, (EXCL CRP) - ACRES') %>% 
  dplyr::select(county_name, cover_acre = value)



# merge the 16 selected variables into a new dataframe
ca_df_list <- list(ca_agland_num, ca_agland_acres, ca_agland_harv_acres, ca_chs_num, ca_total_expense, ca_fuel_expense, ca_cf_excpasture_acre, ca_insect_excnem_acre, ca_herbi_acre, ca_fungi_acre, ca_conserve_acre, ca_notill_acre, ca_rgraze_num, ca_alleysilva_num, ca_organic_num, ca_cover_acre )
ca_selected_merged <- Reduce(function(x, y) left_join(x, y, by = "county_name"), ca_df_list)




################################# TEXAS #################################################
################################# TEXAS #################################################

tx <- filter(df, state_name == 'TEXAS') #extract all the observations for Texas


#1.extract total number of agland operations in texas counties
tx_agland_num <- tx%>%
  filter(census_chapter == 2, 
         census_table == 1, 
         short_desc == 'AG LAND, CROPLAND - NUMBER OF OPERATIONS')%>%  
  dplyr::select(state_name, county_name, agland_num = value) #only select the three columns i.e 'state_name', 
                                                             #'county_name', and 'value' from filtered df. 
                                                             #rename value as agland_num 


#2 extract total agland acres in texas counties 
tx_agland_acres <- tx %>%
  filter(census_chapter == 2, census_table == 1, short_desc == 'AG LAND, CROPLAND - ACRES') %>% 
  dplyr::select(county_name, agland_acres = value) #only select "county_name" and "value" columns
                                                  #from the filtered df, rename value as agland_acres

#3 extract total harvested agland acres
tx_agland_harv_acres <- tx%>%
  filter(census_chapter == 2, census_table == 1, short_desc == 'AG LAND, CROPLAND, HARVESTED - ACRES')%>%
  dplyr::select(county_name, agland_harv_acres= value) 

#4 extract total operations (cattle, hog, and sheep)

tx_chs_num <- tx %>%
  filter(census_chapter == 2, #only select "value' such that it contains either cattle, hogs, or sheeep farms num. 
         census_table == 1,
         short_desc == 'CATTLE, INCL CALVES - OPERATIONS WITH INVENTORY' | short_desc == 'HOGS - OPERATIONS WITH INVENTORY'|short_desc == 'SHEEP, INCL LAMBS - OPERATIONS WITH INVENTORY')%>%
  pivot_wider(id_cols = c(county_name, state_name), names_from = short_desc, values_from = value) %>%  #make separate column for cattle, sheep and hogs farm count
  clean_names() %>% 
  mutate( # replace missing values with 0. 
    cattle_incl_calves_operations_with_inventory = ifelse(is.na(cattle_incl_calves_operations_with_inventory), 0, cattle_incl_calves_operations_with_inventory),
    hogs_operations_with_inventory = ifelse(is.na(hogs_operations_with_inventory), 0, hogs_operations_with_inventory),
    sheep_incl_lambs_operations_with_inventory =   ifelse(is.na(sheep_incl_lambs_operations_with_inventory), 0, sheep_incl_lambs_operations_with_inventory)
  )%>%
  mutate( # remove the ',' in the numeric character so that when you change it into string, you don't get NA. 
    cattle_incl_calves_operations_with_inventory = as.numeric(gsub(",", "",cattle_incl_calves_operations_with_inventory, fixed = TRUE)),
    hogs_operations_with_inventory= as.numeric(gsub(",", "",hogs_operations_with_inventory, fixed = TRUE)),
    sheep_incl_lambs_operations_with_inventory =   as.numeric(gsub(",", "",sheep_incl_lambs_operations_with_inventory, fixed = TRUE))
  )%>%
  mutate( # make a new column which is the sum of cattle, sheep and hogs farm count. 
    chs_num = cattle_incl_calves_operations_with_inventory + hogs_operations_with_inventory + sheep_incl_lambs_operations_with_inventory
  ) %>%
  dplyr::select(county_name, chs_num) #select two columns and no renaming is done



#5. extract total farm operating expense in $

tx_total_expense <- tx %>%
  filter(census_chapter == 2, census_table == 3, short_desc == 'EXPENSE TOTALS, OPERATING - EXPENSE, MEASURED IN $') %>% 
  dplyr::select(county_name, total_expense = value)

#6. extracting fuel operating expense in $
tx_fuel_expense <- tx %>%
  filter(census_chapter == 2, census_table == 3, short_desc == 'FUELS, INCL LUBRICANTS - EXPENSE, MEASURED IN $') %>% 
  dplyr::select(county_name, fuel_expense = value)

#7. acres treated with chemical fertilizer (excl pastureland)
tx_cf_excpasture_acre = tx %>%
  filter(census_chapter == 2, census_table == 40, short_desc == 'AG LAND, CROPLAND, (EXCL PASTURED) - TREATED, MEASURED IN ACRES', domaincat_desc == 'FERTILIZER: (TOTAL)') %>% 
  dplyr::select(county_name, cf_excpasture_acre= value)

#8. acres treated with insecticides(excl nematicides)
tx_insect_excnem_acre = tx %>%
  filter(census_chapter == 2, census_table == 40, short_desc == 'AG LAND - TREATED, MEASURED IN ACRES', domaincat_desc == 'CHEMICAL, INSECTICIDE: (EXCL NEMATICIDES)') %>% 
  dplyr::select(county_name, insect_excnem_acre= value)

#9. acres treated with herbicide 
tx_herbi_acre = tx %>%
  filter(census_chapter == 2, census_table == 40, short_desc == 'AG LAND - TREATED, MEASURED IN ACRES', domaincat_desc == 'CHEMICAL, HERBICIDE: (TOTAL)') %>% 
  dplyr::select(county_name, herbi_acre= value)

#10 acres treated with fungicide
tx_fungi_acre = tx %>%
  filter(census_chapter == 2, census_table == 40, short_desc == 'AG LAND - TREATED, MEASURED IN ACRES', domaincat_desc == 'CHEMICAL, FUNGICIDE: (TOTAL)') %>%
  dplyr::select(county_name, fungi_acre= value)

#11. acres under conservation easement 
tx_conserve_acre <- tx %>%
  filter(census_chapter == 2, census_table == 41, short_desc == 'PRACTICES, LAND USE, CONSERVATION EASEMENT - ACRES') %>% 
  dplyr::select(county_name, conserve_acre= value)

#12. acres under no till 
tx_notill_acre <- tx %>%
  filter(census_chapter == 2, census_table == 41, short_desc == 'PRACTICES, LAND USE, CROPLAND, CONSERVATION TILLAGE, NO-TILL - ACRES') %>% 
  dplyr::select(county_name, notill_acre= value)

#13 number of farms practicing rotational or mgmt intensive grazing
tx_rgraze_num <- tx %>%
  filter(census_chapter == 2, census_table == 43, 
         short_desc == 'PRACTICES, ROTATIONAL OR MGMT INTENSIVE GRAZING - NUMBER OF OPERATIONS') %>% 
  dplyr::select(county_name, rgraze_num = value)

#14. number of farms practicing alley cropping and silvapasuture
tx_alleysilva_num <- tx %>%
  filter(census_chapter == 2, census_table == 43, short_desc == 'PRACTICES, ALLEY CROPPING & SILVAPASTURE - NUMBER OF OPERATIONS') %>% 
  dplyr::select(county_name, alleysilva_num = value)

#15.number of  organic farms (organic, exempt, and transitioning)
tx_organic_num <- tx %>%
  filter(census_chapter == 2, census_table == 42, short_desc == 'FARM OPERATIONS, ORGANIC - NUMBER OF OPERATIONS'|short_desc == 'AG LAND, ORGANIC, TRANSITIONING - NUMBER OF OPERATIONS' )%>%
  pivot_wider(id_cols = c(state_name, county_name), names_from = domaincat_desc, values_from = value) %>%
  clean_names() %>%
  mutate(
    organic_status_nop_usda_certified = ifelse(is.na(organic_status_nop_usda_certified), 0, organic_status_nop_usda_certified),
    organic_status_nop_usda_exempt = ifelse(is.na(organic_status_nop_usda_exempt), 0, organic_status_nop_usda_exempt),
    organic_status_transitioning =   ifelse(is.na(organic_status_transitioning), 0, organic_status_transitioning)
  )%>%
  mutate(
    organic_status_nop_usda_certified = as.numeric(gsub(",", "",organic_status_nop_usda_certified, fixed = TRUE)),
    organic_status_nop_usda_exempt = as.numeric(gsub(",", "",organic_status_nop_usda_exempt, fixed = TRUE)),
    organic_status_transitioning =   as.numeric(gsub(",", "",organic_status_transitioning, fixed = TRUE))
  )%>%
  mutate(
    organic_cet_num = organic_status_nop_usda_certified + organic_status_nop_usda_exempt + organic_status_transitioning
  ) %>% 
  dplyr::select(county_name, organic_num= organic_cet_num)

#16 extract acres of plant planted to cover crops 
tx_cover_acre <- tx %>% 
  filter(census_chapter == 2, census_table == 41,
         short_desc == 'PRACTICES, LAND USE, CROPLAND, COVER CROP PLANTED, (EXCL CRP) - ACRES') %>% 
  dplyr::select(county_name, cover_acre = value)


# merge the 15 selected variables into a new dataframe
tx_df_list <- list(tx_agland_num, tx_agland_acres, tx_agland_harv_acres, 
                   tx_chs_num, tx_total_expense, tx_fuel_expense,
                   tx_cf_excpasture_acre, tx_insect_excnem_acre, tx_herbi_acre,
                   tx_fungi_acre, tx_conserve_acre, tx_notill_acre, tx_rgraze_num,
                   tx_alleysilva_num, tx_organic_num, tx_cover_acre)

tx_selected_merged <- Reduce(function(x, y) left_join(x, y, by = "county_name"), tx_df_list)



################################# USA #################################################
################################# USA #################################################

us <- filter(df, state_name == 'US TOTAL') #extract all the observations for Texas



#1.extract total number of agland operations in texas counties
us_agland_num <- us%>%
  filter(census_chapter == 2, 
         census_table == 1, 
         short_desc == 'AG LAND, CROPLAND - NUMBER OF OPERATIONS')%>%  
  dplyr::select(state_name, county_name, agland_num = value) #only select the three columns i.e 'state_name', 
#'county_name', and 'value' from filtered df. 
#rename value as agland_num 


#2 extract total agland acres in usa
us_agland_acres <- us %>%
  filter(census_chapter == 2, census_table == 1, short_desc == 'AG LAND, CROPLAND - ACRES') %>% 
  dplyr::select(county_name, agland_acres = value) #only select "county_name" and "value" columns
#from the filtered df, rename value as agland_acres

#3 extract total harvested agland acres
us_agland_harv_acres <- us%>%
  filter(census_chapter == 2, census_table == 1, short_desc == 'AG LAND, CROPLAND, HARVESTED - ACRES')%>%
  dplyr::select(county_name, agland_harv_acres= value) 

#4 extract total operations (cattle, hog, and sheep)

us_chs_num <- us %>%
  filter(census_chapter == 2, #only select "value' such that it contains either cattle, hogs, or sheeep farms num. 
         census_table == 1,
         short_desc == 'CATTLE, INCL CALVES - OPERATIONS WITH INVENTORY' | short_desc == 'HOGS - OPERATIONS WITH INVENTORY'|short_desc == 'SHEEP, INCL LAMBS - OPERATIONS WITH INVENTORY')%>%
  pivot_wider(id_cols = c(county_name, state_name), names_from = short_desc, values_from = value) %>%  #make separate column for cattle, sheep and hogs farm count
  clean_names() %>% 
  mutate( # replace missing values with 0. 
    cattle_incl_calves_operations_with_inventory = ifelse(is.na(cattle_incl_calves_operations_with_inventory), 0, cattle_incl_calves_operations_with_inventory),
    hogs_operations_with_inventory = ifelse(is.na(hogs_operations_with_inventory), 0, hogs_operations_with_inventory),
    sheep_incl_lambs_operations_with_inventory =   ifelse(is.na(sheep_incl_lambs_operations_with_inventory), 0, sheep_incl_lambs_operations_with_inventory)
  )%>%
  mutate( # remove the ',' in the numeric character so that when you change it into string, you don't get NA. 
    cattle_incl_calves_operations_with_inventory = as.numeric(gsub(",", "",cattle_incl_calves_operations_with_inventory, fixed = TRUE)),
    hogs_operations_with_inventory= as.numeric(gsub(",", "",hogs_operations_with_inventory, fixed = TRUE)),
    sheep_incl_lambs_operations_with_inventory =   as.numeric(gsub(",", "",sheep_incl_lambs_operations_with_inventory, fixed = TRUE))
  )%>%
  mutate( # make a new column which is the sum of cattle, sheep and hogs farm count. 
    chs_num = cattle_incl_calves_operations_with_inventory + hogs_operations_with_inventory + sheep_incl_lambs_operations_with_inventory
  ) %>%
  dplyr::select(county_name, chs_num) #select two columns and no renaming is done



#5. extract total farm operating expense in $

us_total_expense <- us %>%
  filter(census_chapter == 2, census_table == 3, short_desc == 'EXPENSE TOTALS, OPERATING - EXPENSE, MEASURED IN $') %>% 
  dplyr::select(county_name, total_expense = value)

#6. extracting fuel operating expense in $
us_fuel_expense <- us %>%
  filter(census_chapter == 2, census_table == 3, short_desc == 'FUELS, INCL LUBRICANTS - EXPENSE, MEASURED IN $') %>% 
  dplyr::select(county_name, fuel_expense = value)

#7. acres treated with chemical fertilizer (excl pastureland)
us_cf_excpasture_acre = us %>%
  filter(census_chapter == 2, census_table == 40, short_desc == 'AG LAND, CROPLAND, (EXCL PASTURED) - TREATED, MEASURED IN ACRES', domaincat_desc == 'FERTILIZER: (TOTAL)') %>% 
  dplyr::select(county_name, cf_excpasture_acre= value)

#8. acres treated with insecticides(excl nematicides)
us_insect_excnem_acre = us %>%
  filter(census_chapter == 2, census_table == 40, short_desc == 'AG LAND - TREATED, MEASURED IN ACRES', domaincat_desc == 'CHEMICAL, INSECTICIDE: (EXCL NEMATICIDES)') %>% 
  dplyr::select(county_name, insect_excnem_acre= value)

#9. acres treated with herbicide 
us_herbi_acre = us %>%
  filter(census_chapter == 2, census_table == 40, short_desc == 'AG LAND - TREATED, MEASURED IN ACRES', domaincat_desc == 'CHEMICAL, HERBICIDE: (TOTAL)') %>% 
  dplyr::select(county_name, herbi_acre= value)

#10 acres treated with fungicide
us_fungi_acre = us %>%
  filter(census_chapter == 2, census_table == 40, short_desc == 'AG LAND - TREATED, MEASURED IN ACRES', domaincat_desc == 'CHEMICAL, FUNGICIDE: (TOTAL)') %>%
  dplyr::select(county_name, fungi_acre= value)

#11. acres under conservation easement 
us_conserve_acre <- us %>%
  filter(census_chapter == 2, census_table == 41, short_desc == 'PRACTICES, LAND USE, CONSERVATION EASEMENT - ACRES') %>% 
  dplyr::select(county_name, conserve_acre= value)

#12. acres under no till 
us_notill_acre <- us %>%
  filter(census_chapter == 2, census_table == 41, short_desc == 'PRACTICES, LAND USE, CROPLAND, CONSERVATION TILLAGE, NO-TILL - ACRES') %>% 
  dplyr::select(county_name, notill_acre= value)

#13 number of farms practicing rotational or mgmt intensive grazing
us_rgraze_num <- us %>%
  filter(census_chapter == 2, census_table == 43, 
         short_desc == 'PRACTICES, ROTATIONAL OR MGMT INTENSIVE GRAZING - NUMBER OF OPERATIONS') %>% 
  dplyr::select(county_name, rgraze_num = value)

#14. number of farms practicing alley cropping and silvapasuture
us_alleysilva_num <- us %>%
  filter(census_chapter == 2, census_table == 43, short_desc == 'PRACTICES, ALLEY CROPPING & SILVAPASTURE - NUMBER OF OPERATIONS') %>% 
  dplyr::select(county_name, alleysilva_num = value)

#15.number of  organic farms (organic, exempt, and transitioning)
us_organic_num <- us %>%
  filter(census_chapter == 2, census_table == 42, short_desc == 'FARM OPERATIONS, ORGANIC - NUMBER OF OPERATIONS'|short_desc == 'AG LAND, ORGANIC, TRANSITIONING - NUMBER OF OPERATIONS' )%>%
  pivot_wider(id_cols = c(state_name, county_name), names_from = domaincat_desc, values_from = value) %>%
  clean_names() %>%
  mutate(
    organic_status_nop_usda_certified = ifelse(is.na(organic_status_nop_usda_certified), 0, organic_status_nop_usda_certified),
    organic_status_nop_usda_exempt = ifelse(is.na(organic_status_nop_usda_exempt), 0, organic_status_nop_usda_exempt),
    organic_status_transitioning =   ifelse(is.na(organic_status_transitioning), 0, organic_status_transitioning)
  )%>%
  mutate(
    organic_status_nop_usda_certified = as.numeric(gsub(",", "",organic_status_nop_usda_certified, fixed = TRUE)),
    organic_status_nop_usda_exempt = as.numeric(gsub(",", "",organic_status_nop_usda_exempt, fixed = TRUE)),
    organic_status_transitioning =   as.numeric(gsub(",", "",organic_status_transitioning, fixed = TRUE))
  )%>%
  mutate(
    organic_cet_num = organic_status_nop_usda_certified + organic_status_nop_usda_exempt + organic_status_transitioning
  ) %>% 
  dplyr::select(county_name, organic_num= organic_cet_num)

#16 extract acres of plant planted to cover crops 
us_cover_acre <- us %>% 
  filter(census_chapter == 2, census_table == 41,
         short_desc == 'PRACTICES, LAND USE, CROPLAND, COVER CROP PLANTED, (EXCL CRP) - ACRES') %>% 
  dplyr::select(county_name, cover_acre = value)


# merge the 15 selected variables into a new dataframe
us_df_list <- list(us_agland_num, us_agland_acres, us_agland_harv_acres, 
                   us_chs_num, us_total_expense, us_fuel_expense,
                   us_cf_excpasture_acre, us_insect_excnem_acre, us_herbi_acre,
                   us_fungi_acre, us_conserve_acre, us_notill_acre, us_rgraze_num,
                   us_alleysilva_num, us_organic_num, us_cover_acre)

us_selected_merged <- Reduce(function(x, y) left_join(x, y, by = "county_name"), us_df_list)

############### Row bind california, texas, and usa dataframe

ct_merged <- bind_rows(ca_selected_merged, tx_selected_merged, us_selected_merged)
write_xlsx(ct_merged, path = "D:/agecon_masters_uga/summer_sem/data_visualization_challenge/data_wrangling/ct_merged.xlsx")

ct <- ct_merged

#################### Replacing NA and "D" of numerator columns with 0 ###########################################
####################  and also CREATING AN INDICATOR ###########################################

numeric_col <- c("agland_num", "agland_acres", "agland_harv_acres", "chs_num", 
                 "total_expense",  "fuel_expense", "cf_excpasture_acre", "insect_excnem_acre",  
                 "herbi_acre", "fungi_acre", "conserve_acre", "notill_acre", "rgraze_num", 
                 "alleysilva_num", "organic_num", "cover_acre")

numerator_col <- c("fuel_expense", "cf_excpasture_acre", "insect_excnem_acre",  
                   "herbi_acre", "fungi_acre", "conserve_acre", "notill_acre", "rgraze_num", 
                   "alleysilva_num", "organic_num", "cover_acre")
ct <- ct %>% 
  mutate(across(everything(), ~ str_remove_all(., ","))) %>%  #remove "," in a character number. 
  mutate(across(all_of(numerator_col), ~replace(., is.na(.) | . == "(D)", 0))) %>%  #replace the Replacing NA and "D" of numerator columns with 0 
  mutate(across(all_of(numeric_col), as.numeric)) #make the selected columns numeric


ct <- ct %>% 
  mutate(p_cf_hacre = (cf_excpasture_acre/agland_harv_acres) * 100) %>% 
  mutate(p_insect_hacre = (insect_excnem_acre/agland_harv_acres) * 100) %>% 
  mutate(p_herbi_hacre = (herbi_acre/agland_harv_acres) * 100) %>% 
  mutate(p_fungi_hacre = (fungi_acre/agland_harv_acres) * 100) %>% 
  mutate(p_conserve_acre = (conserve_acre/agland_acres) * 100) %>% 
  mutate(p_notill_hacre = (notill_acre/agland_harv_acres) * 100) %>% 
  mutate(p_rgraze_num = (rgraze_num/chs_num) * 100) %>% 
  mutate(p_alleysilva_num = (alleysilva_num/agland_num) * 100) %>% 
  mutate(p_organic_num = (organic_num/agland_num)* 100) %>% 
  mutate(p_cover_acre = (cover_acre/agland_acres) * 100)


######################## adding usa observation as a separate column ############################

us_ind_col <- c( "p_cf_hacre", "p_insect_hacre", "p_herbi_hacre",
                 "p_fungi_hacre", "p_conserve_acre", "p_notill_hacre", 
                 "p_rgraze_num", "p_alleysilva_num", "p_organic_num", "p_cover_acre" )

us_ind_newcol <- c( "us_p_cf_hacre", "us_p_insect_hacre", "us_p_herbi_hacre",
                    "us_p_fungi_hacre", "us_p_conserve_acre", "us_p_notill_hacre", 
                    "us_p_rgraze_num", "us_p_alleysilva_num", "us_p_organic_num", "us_p_cover_acre" )

ctu <- ct %>% 
  slice_tail(n = 1) %>% 
  select(all_of(us_ind_col)) %>% 
  rename_with(~us_ind_newcol, all_of(us_ind_col)) %>% 
  bind_cols(ct, .)


  
#################### STANDARDIZING INDICATOR ###########################################
#################### STANDARDIZING INDICATOR ###########################################

#standardizing negative indicator 
ctu <- ctu %>% 
  mutate(si_cf_hacre = 100 * exp(log(0.5) * p_cf_hacre/us_p_cf_hacre)) %>% 
  mutate(si_insect_hacre = 100 * exp(log(0.5) * p_insect_hacre/us_p_insect_hacre)) %>% 
  mutate(si_herbi_hacre = 100 * exp(log(0.5) * p_herbi_hacre/us_p_herbi_hacre)) %>%
  mutate(si_fungi_hacre = 100 * exp(log(0.5) * p_fungi_hacre/us_p_fungi_hacre))
 
#standardizing positive indicator 
ctu <- ctu %>% 
  mutate(si_conserve_acre = 100 - (100 * exp(log(0.5) * p_conserve_acre/us_p_conserve_acre))) %>%
  mutate(si_notill_hacre = 100 - (100 * exp(log(0.5) * p_notill_hacre/us_p_notill_hacre))) %>%
  mutate(si_rgraze_num = 100 - (100 * exp(log(0.5) * p_rgraze_num/us_p_rgraze_num))) %>%
  mutate(si_alleysilva_num = 100 - (100 * exp(log(0.5) * p_alleysilva_num/us_p_alleysilva_num))) %>%
  mutate(si_organic_num = 100 - (100 * exp(log(0.5) * p_organic_num/us_p_organic_num))) %>%
  mutate(si_cover_acre = 100 - (100 * exp(log(0.5) * p_cover_acre/us_p_cover_acre)))
  
#taking unweighted average of 10 indicators to create one final index for environmental dimension

ctu <- ctu %>% 
  rowwise() %>% 
  mutate(si_env = mean(c(si_cf_hacre, si_insect_hacre, si_herbi_hacre, si_fungi_hacre, si_conserve_acre, si_notill_hacre, si_rgraze_num, si_alleysilva_num, si_organic_num, si_cover_acre), na.rm = TRUE ))
ctu <- ctu %>% 
  rowwise() %>% 
  mutate(si_neg = mean(c(si_cf_hacre, si_insect_hacre, si_herbi_hacre, si_fungi_hacre), na.rm = TRUE)) %>% 
  mutate(si_pos = mean(c(si_conserve_acre, si_notill_hacre, si_rgraze_num, si_alleysilva_num, si_organic_num, si_cover_acre), na.rm = TRUE)) %>% 
  mutate(si_wenv = mean(c(si_neg, si_pos), na.rm = TRUE))

ctu_env <- ctu %>% #select the state, county, and standard index column
  select(state_name, county_name, si_env) %>% 
  mutate(county_name = str_to_lower(county_name)) %>% 
  mutate(county_name = ifelse(county_name == "null", NA , county_name)) %>% 
  arrange(state_name, county_name) %>% 
  filter(state_name!= "US TOTAL")
  
#import the dataset containing health index 
health <- read_excel("../data_wrangling/health.xlsx") %>% 
  arrange(state, county) %>% 
  select(state_name = state, county_name = county, si_health = health_mean) %>% 
  mutate(county_name = str_replace_all(county_name, " County", ""),
         county_name = str_to_lower(county_name)) %>% 
  filter(state_name != "US_avg") %>% 
  select(county_name, si_health)


#write_xlsx(ctu_env, path = "D:/agecon_masters_uga/summer_sem/data_visualization_challenge/data_wrangling/ctu_env.xlsx")  

#import the dataset containing food accessibility index 
facc <- read_excel("../data_wrangling/food_access_afford_result.xlsx", sheet = 4) %>% 
  clean_names() %>% 
  select(state_name, county_name = geographicareaname, fdacc_n) %>% 
  mutate(county_name = str_replace_all(county_name, " County, California", "")) %>%
  mutate(county_name = str_replace_all(county_name, " County, Texas", "")) %>% 
  mutate(county_name = str_to_lower(county_name)) %>% 
  arrange(state_name, county_name) %>% 
  dplyr::select(county_name, si_facc = fdacc_n)

#import the dataset containing food affordability index 
faff <- read_excel("../data_wrangling/food_access_afford_result.xlsx", sheet = 2) %>% 
  clean_names() %>% 
  select(county_name = geographicareaname, state_name, fdaff_n) %>% 
  relocate(state_name, .before = "county_name") %>% 
  mutate(county_name = str_replace_all(county_name, " County, California", "")) %>% 
  mutate(county_name = str_replace_all(county_name, " County, Texas", "")) %>% 
  mutate(county_name = str_to_lower(county_name)) %>% 
  arrange(state_name, county_name) %>% 
  dplyr::select(county_name, si_faff = fdaff_n)

#bind the columns of ctu_env, facc, faff, and facc 

si_all <- bind_cols(ctu_env,health, facc, faff ) %>% 
  dplyr::select(-c(county_name...4, county_name...6, county_name...8)) %>% 
  rename(county_name = "county_name...2")

#rescale standardized index for si_env, si_facc, si_faff, si_health on a scale of (0-1)
si_all <- si_all %>% 
  mutate(si_env_spyder = si_env/100,
         si_health_spyder = si_health/100,
         si_facc_spyder = si_facc/100, 
         si_faff_spyder = si_faff/100)


#export standardized index as an excel file 
write_xlsx(si_all, path = "../data_wrangling/si_all.xlsx")

library(ggplot2)
df_c <- filter(ctu_env, state_name == 'CALIFORNIA')
df_t <- filter(ctu_env, state_name == 'TEXAS')


ggplot(df_c, aes(x = si_env)) + 
  geom_histogram(fill = 'blue', color = 'red')+
  labs(x = "standardized index", y = "Frequency") +
  ggtitle("Histogram of standardized index for California")

ggplot(df_t, aes(x = si_env)) + 
  geom_histogram(fill = 'blue', color = 'red')+
  labs(x = "standardized index", y = "Frequency") +
  ggtitle("Histogram of standardized index for Texas")

