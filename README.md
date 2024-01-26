# **AB ELECTRICITY GRID MODELING**

Code and functions to visually analyze large groups of forecasting energy data. Data is taken from SQL server databases, excel files, and .R files.

Also includes functions and code segments to visualize historical data from Alberta's electricity sector.

## CONTENTS

-   [CODE FILES](#code-files)
    -   [Database_Loading](#database_loading)
    -   [Scenario_Compare](#scenario_compare)
    -   [AESO_TradeAnalysis](#aeso_tradeanalysis)
    -   [AESO-Analysis](#aeso_analysis)
    -   [Referenced_Code](#referenced_code)
-   [FUNCTION FILES](#function-files)
    -   [Build_Retire_Functions](#build_retire_functions)
    -   [Daily_Output_Functions](#daily_output_functions)
    -   [Data_Filt_To_File](#data_filt_to_file)
    -   [Developing_Functions](#developing_functions)
    -   [Emission_Functions](#emission_functions)
    -   [Group_PlotSave](#group_plotsave)
    -   [Intertie_Functions](#intertie_functions)
    -   [other_functions](#other_functions)
    -   [Output_Gen_Functions](#output_gen_functions)
    -   [Price_Functions](#price_functions)
    -   [Res_Filter_Functions](#res_filter_functions)
    -   [Table_Functions](#table_functions)
    -   [aeso_eval_1](#aeso_eval_1)
    -   [aeso_sim_comp_1](#aeso_sim_comp_1)
    -   [Scenario_Compare_Functions](#scenario_compare_functions)

## CODE FILES

> Files to be run following each simulation. Database Loading is the main code, the rest are useful in specific situations only.

### <ins>Database_Loading

Contains the main code which is used to load data from Microsoft SQL server into the R environment. The script the imports chosen tables, filters required information, and formats dates. The code also contains format information for figures such as legend colors, text sizes, and names.

The sections referencing AESO data and filtering this data can be skipped if no comparison is needed.

##### *Database_Loading Special Notes:*

-   *Make sure the R project file (ab-net-zero-grid-model) is open first or "here" commands will not function.*
-   *Before running, create a folder called "Data Files"  in the project directory. Create two folders inside names "Result Files" and "Alberta Data", populate "Alberta Data" with nrgstream data. This just prevents said data from being uploaded to GitHub*
    -   *This folder is referenced in the .gitignore file*
-   *A folder called "Figures (Local)" should also be created in the project directory of R, this allows figures to be saved locally as opposed to on GitHub.*
        -   *This folder is referenced in the .gitignore file*
    
    ![image](https://github.com/jessvanos/ab-net-zero-grid-model/assets/105378838/86945249-4d38-4473-aa94-22c5ac95648e)
    
-   *Once this file is run through completion, can call any functions as long as the right tables have been loaded in!*

### <ins>Scenario_Compare

Contains packages and code to compare R files between scenarios. Requires same file set up as Database_Loading to save results. Includes options to combine files into single excel and .r format and compare files using plots.

##### *Scenario_Compare Special Notes:*

-   *Make sure the R project file (ab-net-zero-grid-model) is open first or "here" commands will not function.*
-   *The function "AnnualDataR" must be run before combining files. This function creates .r data that will be combined and referenced.*
-   *Before running, create a folder called "Data Files"  in the project directory and add two sub-folders called "Result Files" and "Scenario Compare".*
    -   *This folder is referenced in the .gitignore file*

### <ins>AESO_TradeAnalysis

Script imports data and analyses intertie behavior. Also looks at link capabilities over selected time periods. One-off code.

##### *AESO_TradeAnalysis Special Notes:*

-   *Make sure the project file is open first or "here" commands wont work right.*
-   *Before running, create folder called "Data Files" within project directory and populate it with AESO data.*
-   *Once this file is run through completion, can call any functions with environment that is loaded.*

### <ins>AESO-Analysis

Script for general analysis and plots related to historical generation using NRGstream data.

-   *Make sure the project file is open first or "here" commands wont work right.*
-   *Before running, create folder called "Data Files" within project directory and populate it with AESO data.*
-   *Once this file is run through completion, can call any functions with environment that is loaded.*

### <ins>Misc_Data_Visuals

Plots not directly related to simulation and modeling work. Inlcudes AESO planning area plots data and more.

##### *Misc_Data_Visuals Special Notes:*

-   *Make sure the project file is open first or "here" commands wont work right.*
-   *Before running, create folder called "Data Files/AESO Planning Locations" within project directory and populate it geographical data and project information.*

### <ins>Referenced_Code

Codes adapted from Dr. Andrew Leach and Taylor Pawlenchuk.Functions are used inside some other functions. 

## FUNCTION FILES

> Functions are found in the ["Functions"](https://github.com/jessvanos/ab-net-zero-grid-model/tree/main/Functions) folder and are organized into categories and stored in respective code files. For each function, there is a short description bellow, for more details (including inputs and required tables) see .R file.

### <ins>Build_Retire_Functions<ins>

Functions to evaluate the electricity grid capacity changes in alternative scenarios. These functions focus on resource additions and retirements.

#### Functions:

-   *Retirecol* : Plotting the resources retired as a bar chart.

-   *RetireMW* : Plotting the resource capacity retired as a bar chart.

-   *Builtcol* : Plotting the resources built as a bar chart. This is not the best visual when partial builds are allowed! Partial builds limits the capacity but not the actual number of units.

-   *Build_A\_MW* : Plotting the capacity of resources built by Aurora (does not include AESO que projects).

-   *BuildMW* : Plotting the built capacity for ALL new resources (resource table and new resource table).

-   *Eval_CapChange* : Shows net capacity changes each year. The first year of data does not have a prior capacity to compare to, so it is not used.

-   *TotalCapChange* : Shows capacity changes each year, includes additions and retirements.

-   *Units* : Unit specific bar chart showing builds by unit for certain resource type (ex: "WND").

-   *Slack* : Unit specific bar chart showing units not built (or available) for certain resource type.

-   *BuildUnits* : Show units built compared to available ones for a resource type.

-   *Build_CCSRet* : Show CCS retrofit options and what took place.

-   *Build_CCSRet2* : Show base units for CCS retrofit options and year retired to allow retrofit.

-   *CC_Fate_study* : Show if combined cycle gas plants continue to opperate, retire, or retrofit over entire study.

-   *CC_Fate_year* : Shows total combined cycle gas capacity in each year of study. Includes retirement and retrofit option.


### <ins>Daily_Output_Functions

Hourly outut for single days under alternative conditions. 

-   *CompDay_Season* : Compare two typical days in winter and summer.

-   *CompDay_Wind* : Plot max and min wind days for a given year.

-   *CompDay_Solar* : Plot max and min solar days for a given year.

-   *CompDay_Years* : Compare the saem day in two different years.

-   *CompDay_AESO* : Compare a specified day from the simulation with actual data.


### <ins>Data_Filt_To_File<ins>

Filters and organizes annual and hourly data, sends to an excel file which can be easily interpreted and exported to other programs. Also includes functions to filter and reformat key data for comparison between scenarios.

#### Functions:

-   *AnnaulDataExcel* : Writes all relevant annual data to an excel file on different sheets.

-   *HourlyDataExcel* : Writes all relevant hourly data to an excel file on different sheets.

-   *AnnualDataR* : Filters and organizes annual data, sends to R files for easy accessibility.

-   *CombineFilesR* : Reads filtered R data and combines two files into a single excel sheet.

##### *Data_Filt_To_Table Special Notes:*

-   *Excel sheets will be written to the "Data Files" folder stored locally within the R project. If the name is not changes it will be over-written when running again.*


### <ins>Developing_Functions

Functions that are not completed yet and are not yet categorized. Space to test functions out. When functions are de-bugged and working they will be moved to a different file.

### <ins>Emission_Functions

Functions to evaluate and show emissions.

#### Functions:

-   *AnnualEmStackCol* : Plot annual emissions by resource group as as stacked chart.

-   *AnnualEmLine* : Plot annual emissions by resource group as as line chart.

-   *Emissions_CER_Res* : Plot annual emissions for CER resources.

-   *Emissions_CER_group* : Plot annual emissions grouped by year CER is in effect.


### <ins>Group_PlotSave

Quickly save groups of plots to folders without creating each figure individually.

#### Functions:

-   *GGSave_Loc* : Saves all plots to a new folder named after case using ggplot.

-   *GGSave_Loc_Ex* : Saves all plots to a new folder called "Additional Analysis" inside image folder named after case using ggplot.

-   *GGSave_Loc_narrow* : Saves all plots to a new folder named after case using ggplot, narrow image.

-   *GGSave_Loc_wide* : Saves all plots to a new folder named after case using ggplot, wide image.

-   *Value_saveall* : Save all value plots including NPV, annual value, annual value per MWh generated.

-   *Slack_saveall* : Save plots to compare available units and built units in capacity expansion.

-   *Analysis_saveall* : Save general analysis plots including capacity, generation, emissions, and cost information.

-   *Detail_Gen_save* : Save detailed generation plots, shows hourly generation in different months and years.

-   *CER_saveall* : Save plot related to CER constraint implimentation.


### <ins>Intertie_Functions<ins>

Functions To use for plotting and evaluating intertie activities. Also shows information on trade and what is happening in BC/MT/SK.

#### Functions:

-   *Imp_Exp1 :* AB imports and exports plotted as yearly totals.

-   *Imp_Exp2 :* AB imports and exports plotted as annual chart, shows hourly patterns.

-   *Imp_ExpWk:* AB imports and exports for a single week.

-   *BC_SK_IE :* BC and SK imports and exports.

-   *MN_Trade_Price* : Get trade and price each month to compare.

-   *MN_TradeOnly* : AB imports and exports and pool price for specific month.

-   *T_month_all_Sim* : All trade for each month over one year.

Historical Fit Functions. Included for looking at historical trade patterns only.

-   *Capab_year* : Capability of intertie over entire year.

-   *Capab_mn* : Capability of intertie over selected month. Choose between min, max, or average.

-   *Capab_Allmn* : Capability of intertie over all months of year. Choose between min, max, or average.

-   *Capab_stats* : Gives mean, max, min, and standard deviation for capabilities over intertie lines.

-   *ZeroTrade* : Percentage of time where intertie capability is zero.


##### *Intertie_Functions Special Notes:*

-   *HR FIT Functions are also included in this file. These were used to build and analyze historical trade patterns.*

### <ins>other_functions

Additional functions to use, not related the data itself. Used within main functions.

#### Functions:

-   *packs_check :* Checks if packages are installed, installs them if not, and loads required functions.

-   *SaveRun_Loc* : Saves all plots to a new folder names after case.

-   *yhour* : Get the hour of year associated with a date in the form "%Y-%m-%d %H:%M:%S" (EX: Jan 1 at 1:00 = 0001, Dec 31 at 23:00 = 8760).

-   *round_any* : Use to round value to a certain accuracy.

-   *HrTime* : Convert the date and select a subset for one day from the data pulled in.

-   *WkTime* : Convert the date and select a subset for one week from the data pulled in.

-   *YrDay_Time* : Convert the date and select a subset for specific year and day.

-   *YrTime* : Convert the date and select a subset for one week from the data pulled in.

-   *Legend_PlotAll* : Plot legend for all things referenced in other plots.

-   *Legend_PlotMain* : Plot legend for main things referenced in other plots.

### <ins>Output_Gen_Functions

Functions to use for plotting and evaluating simulation data on resource outputs and generation. Also plots other miscalaneous things not covered elsewhere

#### Functions:

-   *Week1* : Plots output for a single week given the case study.

-   *day1* : Plots output for a single day given the case study.

-   *day2* : Plots output for a single day given the case study, formated for comparison.

-   *Stor1* : Weekly storage output.

-   *Stor2* : Weekly storage output with pool price overlaid.

-   *Evalyr* : Plotting year profiles of resource output.

-   *Evalcap* : Plotting month/year profiles of resource capacity.

-   *EvalPerc* : Year/month profiles as a percentage of the total.

-   *Output_Comp* : Plotting the capacity of resources individually for selected years as side by side bar charts.

-   *AnnualDemand* : Plot average demand in zone.

-   *CFcompare* : Compares capacity factor for two chosen years. Modified from Taylor Pawlenchuk.

-   *CF_Annual* : Compares capacity factor for two chosen years. Similar to plot seen on page 10 of AESOs net zero report dashboard.

-   *MaxCurtail* : Shows the maximum customer curtailment used in a  single hour for each year.

-   *CF_CER_Res* : Show capacity factor for individual resources included in CER.

-   *Hours_CER_Res* : Show hours opperated for individual units included in CER.

-   *CF_CER_groups* : Show capacity factor based on year CER applies.
    
-   *Wind_Dur* : Plot wind duration curve in chosen years as % Hours vs Fleet Output (MW).

-   *Wind_DurNorm* : Plot wind duration curve in chosen years as % hours vs output as percent of max.

-   *Week12* : Plots output for a single week given the case study. Supporting function To be used in year of weeks function.

-   *year_weeks* : Plots output for a single week given the case study.

-   *PrOt* : Plots pool price over one week of output data.

-   *PrOut* : Plots pool price over one week of output data with storage utilization.

-   *year_stor* : Plots storage output and pool price for one week of each month for selected year.

-   *FourMonthSummary* : Shows generation, storage, and price for one week of 4 months in a year.

-   *EachResWeek* : Show one week of generation and split wach resource type to its own plot.

-   *Num_Startups* : Show number of startups by tech and year.

-   *CER_EM_hour_Res* : Show hours run and total emissions for CER resources.

-   *CER_EM_hour_group* : Show capacity factor and emissions for CER resources based on year applied.


### <ins>Price_Functions

Functions related to technology capture prices, pool prices, and other cost related material.

#### Functions:

-   *week_price* : Electricity wholesale pool price for one week.

-   *Sim_dur* : Simulation duration curve ploted each year. The price duration curve represents the percentage of hours in which pool price equaled or exceeded a specified level.

-   *AvgMn_price* : Plots monthly average pool price with average internal load.

-   *poolprice_2year* : A function to plot the Monthly average pool price (Like in the AESO Market Report 2021 Figure 1).

-   *AvgYr_poolprice* : Plots monthly average pool price with average internal load.

-   *System_Cost* : Describes the average system costs incured by all resources in the system.

-   *ResValue_Line* : Shows the nominal annual value of resources as a line over entire study length. Based on plant type and year built. Define the Resource type based on number. 1 wind, 2- Solar, 3 - Storage, 4 - Unabated natural gas, 5- Abated natural gas, 6 - Hydrogen, 7 - Hydro, 8 - Other, 9 - Cogen.

-   *ResValue_Annual* : Shows the annual value of new resources based on plant type and year built. Define the Resource type based on number. 1 wind, 2- Solar, 3 - Storage, 4 - Unabated natural gas, 5- Abated natural gas, 6 - Hydrogen, 7 - Hydro, 8 - Other, 9 - Cogen.

-   *ResValue_Annual_MWh* : Shows the annual value per MWh for new resources based on plant type and year built. Define the Resource type based on number. 1 wind, 2- Solar, 3 - Storage, 4 - Unabated natural gas, 5- Abated natural gas, 6 - Hydrogen, 7 - Hydro, 8 - Other, 9 - Cogen.

-   *ResValue_NPV_MWh* : Shows the net present value per MWh generated. 1 wind, 2- Solar, 3 - Storage, 4 - Unabated natural gas, 5- Abated natural gas, 6 - Hydrogen, 7 - Hydro, 8 - Other, 9 - Cogen.

-   *ResValue_NPV* : Shows the net present value in 2023. 1 wind, 2- Solar, 3 - Storage, 4 - Unabated natural gas, 5- Abated natural gas, 6 - Hydrogen, 7 - Hydro, 8 - Other, 9 - Cogen.

-   *capture_p* : Shows capture prices by technology.

-   *Relcapture_p* : Shows capture prices by technology relative to pool price.

-   *ach_poolprem* : Shows achieved preium compared to pool price.

### <ins>Res_Filter_Functions

Functions to select and sort certain resource groups for other functions to use. A collection of filters.

#### Functions:

-   *sim_filt* : This function filters for the data that will be evaluated by ID.

-   *sim_filt1* : This function filters for the data that will be evaluated by ID, more detail (breaks up natural gas into combined and simple cycle).

-   *sim_filt2* : This function filters for the data that will be evaluated by primary fuel type.

-   *sim_filt3* : This function filters for the data that will be evaluated. Same as 2, with coal removed and storage into 3 components.

-   *sim_filt4* : This function filters for the data that will be evaluated. Same as 3, however only includes Aurora build options and is based on Fuel Type.

-   *sim_filt5* : This function filters for the data that will be evaluated by ID. Seperates in detail (inclusive of storage types).

-    *sim_filt6* : This function filters for EVERYTHING - based on Primary Fuel Type.

-   *sim_filtEm* : This function filters for emission releasing resources, filtered by ID.

-   *sim_filtFuel* : This function filters for the data that will be evaluated. Same as 3, however only includes Aurora build options and is based on Fuel Type.

### <ins>Table_Functions

Functions To use for summarizing data within R environment.

#### Functions:

-   *Report_P* : Report average zone prices in a table, organized by condition (average, peak, off-peak), year, and optionally zone).

-   *Build_Totals* : Report the capacity built for each fuel type in the study by year. Includes manually added additions.

-   *Build_A\_Totals* : Report the capacity built for each fuel type in the study by year. Aurora new builds only.


### <ins>aeso_eval_1

Functions used to used to plot and analyze other (AESO) data. Some functions based on code from Taylor Pawlenchuk (Retrieved June 3, 2022).

#### Functions:

-   *plnt_tr* : Identify specific plant traits.

-   *Week_act* : Plot actual AESO output for a single week.

-   *Day_AESO* : Plots actual AESO output for a single day.

-   *wkPrice* : Plot AESO pool price.

-   *cap_pf*

-   *hrc*

-   *cap_offer*

-   *cap_offermn*

-   *var_label*

-   *cdata*

-   *cap_type*

-   *table_type*

-   *table_data*

-   *graph_type*

-   *var_label*

-   *Cap3*

-   *Cap4*

-   *yearly_dmd*

-   *monthly_dmd_ave*

-   *AESO_PrOt* : Price and output side-by-side.

-   *Trade_Mn_AESO* : AB import and export with pool price for selected month.

-   *TradeOnly_Mn_AESO* : AB import and export for selected month.

-   *Trade_Yr_AESO* : AB import and export for a single year.

-   *Duration_AESO* : AB import and export along with pool price for a specific year.

-   *Wind_Dur_AESO* : Wind duration curve in chosen year as % Hours vs Fleet Output (MW).

-   *Wind_DurNorm_AESO* : Plot wind duration curve in chosen years as % Hours vs output as Percent of max

-   *T_month_all* : All trade for each month over one year.

### <ins>aeso_sim_comp_1

Functions used to compare simulation data with other/actual data. Some functions have been adapted from Taylor Pawlenchuk (Retrieved June 14, 2022).

#### Functions:

-   *AESO_SimOP* : Plot comparison between actual and simulated data.

-   *AESO_SimP* : Plot comparison between actual and simulated data price for 1 week.

-   *AESO_SimP2* : Plot comparison between actual and simulated data price for 1 week.

-   *AESO_SimO* : Plot comparison between actual and simulated data generation.

-   *rev_dur* : Plot difference between simulated and actual revenues.

-   *year_comp* : Plots the difference in Pool Price between AESO and Sim.

-   *year_dif* : Bar plot showing the difference between AESO and Sim.

-   *year_avg* : Bar chart comparing monthly average pool prices.

-   *year_pool* : A function to plot the Monthly average pool price; like in the AESO Market Report 2021 Figure 1.

-   *comp_dur* : Plots the Pool Price duration vs percentile for AESO and Sim; like AESO Market Report 2021 Figures 2 and 3.

-   *load_dur* : Plots the load duration vs percentile for AESO and Sim; like AESO Market Report 2021 Figures 7 and 8

-   *tech_cap* : Plots the capacity factor by technology for AESO and Sim; like AESO Market Report 2021 Figure 15.

-   *margin* : Plots the marginal price-setting technology for AESO and Sim; like AESO Market Report 2021 Figure 19.

-   *tot_cap* : Plots the year-end capacity by technology for AESO and Sim; like AESO Market Report 2021 Figure 11.

-   *AESOSim*

-   *AESO_Sim_WindDur* : Plot comparison between actual and simulated wind duration curves.

-   *AESO_Sim_WindDurNorm* : Plot comparison between actual and simulated wind duration curves.

### <ins>Scenario_Compare_Functions

Functions used to compare between different scenarios/simulations. To be run after files are combined in "Scenario_Compare" function

-   *AvgYr_price_COMPARE* : Plot average annual pool price.
