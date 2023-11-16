################################################################################
# TITLE: Extra_Analysis
# DESCRIPTION:  Run after Database_Loading for additional analysis.
#
#
# AUTHOR: Jessica Van Os
# CONTACT: jvanos@ualberta.ca
# CREATED: November 2023
#
################################################################################

################################################################################
# PLANT VALUES
# Shows net present value or value of resources
# Date is the build date to filter by
# 1 wind, 2- Solar, 3 - Storage, (4,4.1,4.2) - Unabated natural gas, 5- Abated natural gas, 6 - Hydrogen
# 7 - Hydro, 8 - Other, 9 - Cogen
################################################################################

windows(14,10,buffered=FALSE)

# WIND
  ResValue_NPV(1,1899,BC)
  SaveRun_Loc_Ex(CaseName,"NPV Wind")
  
  ResValue_NPV_MWh(1,1899,BC)
  SaveRun_Loc_Ex(CaseName,"NPV Wind MWh")
  
  ResValue_Line(1,1800,BC)
  SaveRun_Loc_Ex(CaseName,"Annual Value Line Wind")

  ResValue_Annual(1,1899,BC)
  SaveRun_Loc_Ex(CaseName,"Annual Value Dots Wind")

  ResValue_Annual_MWh(1,1899,BC)
  SaveRun_Loc_Ex(CaseName,"Annual Value Dots Wind MWh")
  
  
# SOLAR
  ResValue_NPV(2,1899,BC)
  SaveRun_Loc_Ex(CaseName,"NPV Solar")
  
  ResValue_NPV_MWh(2,1899,BC)
  SaveRun_Loc_Ex(CaseName,"NPV Solar MWh")
  
  ResValue_Line(2,1800,BC)
  SaveRun_Loc_Ex(CaseName,"Annual Value Line Solar")
  
  ResValue_Annual(2,1899,BC)
  SaveRun_Loc_Ex(CaseName,"Annual Value Dots Solar")
  
  ResValue_Annual_MWh(2,1899,BC)
  SaveRun_Loc_Ex(CaseName,"Annual Value Dots Solar MWh")

# SIMPLE
  ResValue_NPV(4.1,1899,BC)
  SaveRun_Loc_Ex(CaseName,"NPV Simple Cycle Gas")
  
  ResValue_NPV_MWh(4.1,1899,BC)
  SaveRun_Loc_Ex(CaseName,"NPV Simple Cycle Gas MWh")
  
  ResValue_Line(4.1,1800,BC)
  SaveRun_Loc_Ex(CaseName,"Annual Value Line Simple Cycle Gas")
  
  ResValue_Annual(4.1,1899,BC)
  SaveRun_Loc_Ex(CaseName,"Annual Value Dots Simple Cycle Gas")
  
  ResValue_Annual_MWh(4.1,1899,BC)
  SaveRun_Loc_Ex(CaseName,"Annual Value Dots Simple Cycle Gas MWh")

# COMBINED
  ResValue_NPV(4.2,1899,BC)
  SaveRun_Loc_Ex(CaseName,"NPV Combined Cycle Gas")
  
  ResValue_NPV_MWh(4.2,1899,BC)
  SaveRun_Loc_Ex(CaseName,"NPV Combined Cycle Gas MWh")
  
  ResValue_Line(4.2,1800,BC)
  SaveRun_Loc_Ex(CaseName,"Annual Value Line Combined Cycle Gas")

# CCS
  ResValue_NPV(5,1899,BC)
  SaveRun_Loc_Ex(CaseName,"NPV CCS Gas")
  
  ResValue_NPV_MWh(5,1899,BC)
  SaveRun_Loc_Ex(CaseName,"NPV CCS MWh")
  
  ResValue_Line(5,1800,BC)
  SaveRun_Loc_Ex(CaseName,"Annual Value Line CCS")

# H2
  ResValue_NPV(6,1899,BC)
  SaveRun_Loc_Ex(CaseName,"NPV Hydrogen")
  
  ResValue_NPV_MWh(6,1899,BC)
  SaveRun_Loc_Ex(CaseName,"NPV Hydrogen MWh")
  
  ResValue_Line(6,1800,BC)
  SaveRun_Loc_Ex_Ex(CaseName,"Annual Value Line Hydrogen")

# HYDRO
  ResValue_NPV(7,1899,BC)
  SaveRun_Loc_Ex_Ex(CaseName,"NPV Hydro")
  
  ResValue_NPV_MWh(7,1899,BC)
  SaveRun_Loc_Ex_Ex(CaseName,"NPV Hydro MWh")
  
  ResValue_Line(7,1800,BC)
  SaveRun_Loc_Ex_Ex(CaseName,"Annual Value Line Hydro")

# OTHER
  ResValue_NPV(8,1899,BC)
  SaveRun_Loc_Ex_Ex(CaseName,"NPV Other")
  
  ResValue_NPV_MWh(8,1899,BC)
  SaveRun_Loc_Ex_Ex(CaseName,"NPV Other MWh")
  
  ResValue_Line(8,1800,BC)
  SaveRun_Loc_Ex_Ex(CaseName,"Annual Value Line Other")

# COGEN
  ResValue_NPV(9,1899,BC)
  SaveRun_Loc_Ex_Ex(CaseName,"NPV Cogen Gas")
  
  ResValue_NPV_MWh(9,1899,BC)
  SaveRun_Loc_Ex_Ex(CaseName,"NPV Cogen MWh")
  
  ResValue_Line(9,1800,BC)
  SaveRun_Loc_Ex_Ex(CaseName,"Annual Value Line cogen")

################################################################################
# SLACK
#Compare available units and built units.
# ("WND", "SUN","GasCCS","BIO","Gas1","Gas2","H2","UR","PS")
################################################################################

  windows(14,10,buffered=FALSE)
  
# WIND
  BuildUnits(BC, "WND")
  SaveRun_Loc_Ex_Ex(CaseName,"Res Slack Wind")

# SUN
  BuildUnits(BC, "SUN")
  SaveRun_Loc_Ex_Ex(CaseName,"Res Slack Solar")

# CCS
  BuildUnits(BC, "GasCCS")
  SaveRun_Loc_Ex_Ex(CaseName,"Res Slack All CCS")

# CCS retrofits
  Build_CCSRet(BC)
  SaveRun_Loc_Ex_Ex(CaseName,"Res Slack CCS Retrofit")

# BIOMASS
  BuildUnits(BC, "OT")
  SaveRun_Loc_Ex_Ex(CaseName,"Res Slack Biomass")

# SIMPLE
  BuildUnits(BC, "Gas2")
  SaveRun_Loc_Ex_Ex(CaseName,"Res Slack Simple Cycle")

# COMBINED
  BuildUnits(BC, "Gas1")
  SaveRun_Loc_Ex_Ex(CaseName,"Res Slack Combined Cycle")

# H2
  BuildUnits(BC, "H2")
  SaveRun_Loc_Ex_Ex(CaseName,"Res Slack Hydrogen")

# Nuclear
  BuildUnits(BC, "UR")
  SaveRun_Loc_Ex_Ex(CaseName,"Res Slack Nuclear")

# Storage
  BuildUnits(BC, "PS")
  SaveRun_Loc_Ex_Ex(CaseName,"Res Slack Storage")
  