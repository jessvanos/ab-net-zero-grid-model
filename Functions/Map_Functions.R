################################################################################  
## FUNCTION: ProjectMap_bytype
## Plot a map of Alberta planning regions with projected new projects of specified plant type.
##
## INPUTS: 
##    PlanMapDataCombined - Input data including geographical data merged with new capacity info
##    ResType - Resource type to plot ("Solar", "wind", "Storage", or "Gas")
##    IncludedProjects - Criteria based on data used, plots all projects or projects under construction and with AUC approval only.
## TABLES REQUIRED: 
##    ResGroupEmYr -Yearly resource group emissions
################################################################################

ProjectMap_bytype<-function(PlanMapDataCombined,ResType,IncludedProjects) {
  
# Set up parameters based on resource selection
  if (ResType=="Solar") {
      ResFill<-PlanMapDataCombined$Solar
      ResColor<-"#7D5E02"
      AddText<-""
  } else if (ResType=="Wind") {
      ResFill<-PlanMapDataCombined$Wind
      ResColor<-"#277E3E"
      AddText<-""
  } else if (ResType=="Storage"){
      ResFill<-PlanMapDataCombined$Storage 
      ResColor<-"#C05200"
      AddText<-""
  } else if (ResType=="Gas"){
      ResFill<-PlanMapDataCombined$Gas
      ResColor<-"#595959"
      AddText<-"Includes projects classified as under construction, recieved AUC approval, and announced
                   Encompasses coal-to-gas transitions, cogeneration, and other gas generation"
  } else {
          }
  
# Set up subtitle based on selection
  if(IncludedProjects=="all"){
    captiontext<-"Includes projects classified as under construction, recieved AUC approval, and announced"
  } else if (IncludedProjects=="ExclAnnounced"){
    captiontext<-"Includes projects classified as under construction and recieved AUC approval"
  }else {
  }
    
  
  ggplot(data=PlanMapDataCombined) +
    geom_sf(mapping = aes(fill = ResFill),color="black") +
    theme_bw() +
    theme(axis.line = element_blank(),
          axis.ticks =  element_blank(),
          axis.text = element_blank(),
          axis.title=element_blank(),
          plot.caption =element_text(face = "italic",color="grey50",size=6,hjust = 1.35),
          plot.title=element_text(size=12,hjust=0.5),
          plot.subtitle=element_text(size=8,hjust=0.5),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.border = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.text=element_text(size=8,format(1000000, big.mark = ",", scientific = FALSE)),
          legend.title = element_text(size=10),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = 'transparent')) +
    
    scale_fill_gradient(low="white", high=ResColor)+
    
    labs(fill="Capacity (MW)",
         title=paste("Alberta", ResType, "Generation Projects at Various Stages of Development"),
         subtitle=paste(AddText,captiontext),
         caption ="Figure by Jessica Van Os, Data from AESO LTA report and connection project list (Aug 2023)") +
    
    geom_sf_text (aes(label = Area_ID),size=2,color="black")
  
}