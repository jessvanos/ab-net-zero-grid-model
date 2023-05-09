# **NET ZERO GRID MODELING**

Code and functions to visually analyze large groups of forecasting energy data.

## CONTENTS

-   [CODE FILES](#code-files)
    -   [Database_Loading](#database-loading)
    -   [AESO_TradeAnalysis](#sec-insaeso_tradeanalysis)
    -   [Link_Shaping](#link-shaping)
    -   [DrLeach_Code](#drleach-code)
-   [FUNCTION FILES](#function-files)
    -   [aseo_eval_1](#sub-heading-1)
    -   [aseo_sim_comp_1](#sub-heading-2)
    -   [intertie_info](#sub-heading-3)
    -   [Net_Zero_eval](#sub-heading-4)
    -   [net_zero_tables](#sub-heading-5)
    -   [other_functions](#sub-heading-6)
    -   [sim_eval_1](#sub-heading-7)

## CODE FILES {#code-files}

Files to be run following each simulation. Database Loading is the main code, the rest are useful in specific situaitons only.

### <ins>Database_Loading

Contains the main code which is used to load data from Microsoft SQL server into the R environment. The script the imports chosen tables, filters required information, and formats dates. The code also contains format information for figures such as legend colors, text sizes, and names.

The sections referencing AESO data and filtering this data can be skipped if no comparison is needed.

###### *Database_Loading Special Notes:*

-   *Make sure the R project file (ab-net-zero-grid-model) is open first or "here" commands will not function.*
-   *Before running, create folder called "Data Files" in the project directory and populate it with other (AESO) data. This just prevents said data from being uploaded to GitHub*
    -   *This folder is referenced in the .gitignore file*
-   *Once this file is run through completion, can call any functions as long as the right tables have been loaded in!*

### <ins>AESO_TradeAnalysis {#sec-insaeso_tradeanalysis}

Script imports data and analyses intertie behavior.

###### *AESO_TradeAnalysis Special Notes:*

-   *Make sure the project file is open first or "here" commands wont work right.*
-   *Before running, create folder called "Data Files" withen project directory and populate it with AESO data.*
-   *Once this file is run through completion, can call any functions with environment that is loaded.*

### <ins>Link_Shaping

Code that was used to gather and format historical intertie data.

### <ins>DrLeach_Code

Helper codes addapted from Dr. Andrew Leach and Taylor Pawlenchuk

## FUNCTION FILES {#function-files}

Functions are organized into categories and stored in respective code files. For each function, there is a short description bellow, for more details (including inputs and required tables) see .R file.

### <ins>aseo_eval_1

Functions To used to plot and analyze other (AESO) data.

#### Functions:

### <ins>aseo_sim_comp_1

Functions To used to compare simulation data with other data.

#### Functions:

### <ins>Build_Retire_Functions

#### Functions:

### <ins>Data_Filt_To_Table

#### Functions:

### <ins>Developing Functions

#### Functions:

### <ins>Emission_Functions

#### Functions:

### <ins>Intertie_Functions

Functions To use for plotting and evaluating intertie activities.

#### Functions:

-   <ins>*Imp_Exp*</ins>

    -   AB imports and exports plotted as yearly totals.

-   <ins>*BC_SK_IE*</ins>

    -   BC and SK imports and exports.

### <ins><ins>other_functions

Additional functions to use, not related the data itself.

#### Functions:

-   <ins>*packs_check*</ins>

    -   Checks if packages are installed, installs them if not, and loads required functions.

-   <ins>*imsave_git*</ins>

    -   Save most recent plot to git folder with transparent background.

-   <ins>*imsave_loc*</ins>

    -   Save most recent plot to local folder with transparent background.

### <ins>Output_Gen_Functions

#### Functions:

### <ins>Price_Functions

#### Functions:

### <ins>Res_Filter_Functions

#### Functions:

### <ins>Table_Functions

#### Functions:
