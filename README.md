# **NET ZERO GRID MODELING**
Code and functions to visually analyze large groups of forecasting energy data.

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
## Net_Zero_eval
Functions to evaluate the electricity grid and forecasting as it approaches possible net zero states.

#### Functions:

## other_functions
Additional functions to use, not related the data itself.

#### Functions:

## aseo_eval_1
Functions To used to plot and analyze other (AESO) data.

#### Functions:

## aseo_sim_comp_1
Functions To used to compare simulation data with other data.

#### Functions:

## sim_eval_1
Functions To use for plotting and evaluating simulation data.

#### Functions:
