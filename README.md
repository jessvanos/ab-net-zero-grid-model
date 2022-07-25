# **NET ZERO GRID MODELING**
Code and functions to visually analyze large groups of forecasting energy data.

- [CODE FILES](#heading)
  * [Database_Loading](#sub-heading)
- [FUNCTION FILES](#heading-1)
  * [aseo_eval_1](#sub-heading-1)
  * [aseo_sim_comp_1](#sub-heading-2)
  * [intertie_info](#sub-heading-3)
  * [Net_Zero_eval](#sub-heading-4)
  * [net_zero_tables](#sub-heading-5)
  * [other_functions](#sub-heading-6)
  * [sim_eval_1](#sub-heading-7)

<!-- toc-->

# CODE FILES
## Database_Loading
Contains the main code which is used to load data from Microsoft SQL server into the R environment. The script the imports chosen tables, filters required information, and formats dates. The code also contains format information for figures such as legend colors, text sizes, and names.

The sections referencing AESO data and filtering this data can be skipped if no comparison is needed.

#### *Special Notes:*
- *Make sure the R project file (ab-net-zero-grid-model) is open first or "here" commands will not function.*
- *Before running, create folder called "Data Files" in the project directory and populate it with other (AESO) data. This just prevents said data from being uploaded to GitHub*
  - *This folder is referenced in the .gitignore file*
- *Once this file is run through completion, can call any functions as long as the right tables have been loaded in!*

# FUNCTION FILES
For each function, there is a short description bellow, for more details (including inputs and required tables) see .R file.

## aseo_eval_1
Functions To used to plot and analyze other (AESO) data.

#### Functions:

## aseo_sim_comp_1
Functions To used to compare simulation data with other data.

#### Functions:

## intertie_info
Functions To use for plotting and evaluating intertie activities.

#### Functions:
- <ins>*Imp_Exp*</ins> - AB imports and exports plotted as yearly totals.
- <ins>*BC_SK_IE*</ins> - BC and SK imports and exports.

## Net_Zero_eval
Functions to evaluate the electricity grid and forecasting as it approaches possible net zero states.

#### Functions:

## net_zero_tables
Functions to use for summarizing data in tables, anything but plots.

#### Functions:
- <ins>*Report_P*</ins> - Report average zone prices in a table, organized by condition (average, peak, off-peak), year, and optionally zone

## other_functions
Additional functions to use, not related the data itself.

#### Functions:
- <ins>*packs_check*</ins> - Checks if packages are installed, installs them if not, and loads required functions.
- <ins>*imsave_git*</ins> - Save most recent plot to git folder with transparent background.
- <ins>*imsave_loc*</ins> - Save most recent plot to local folder with transparent background.

## sim_eval_1
Functions To use for plotting and evaluating simulation data.

#### Functions:
