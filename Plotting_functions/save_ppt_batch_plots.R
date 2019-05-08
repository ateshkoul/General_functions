save_ppt_batch_plots <- function(Data_file_name,save_tiff=FALSE,save_ppt=FALSE,description){
  
  
  Data <- read.csv(file.path("Sub_projects/Realtime_models/SVM_classification_models/Analysis/",paste0(Data_file_name,".csv")))
  
  
  if(save_tiff) {
    figure_path <- file.path("Sub_projects/Realtime_models/SVM_classification_models/Results/Figures/",paste0(Data_file_name,".tiff"))
    # full page size 174 mm, cell press
    tiff(filename = figure_path ,width = 174,height = 170,units = "mm",res=300)
  }
  
  
  
  # "X" is the row number
  p1 <- easy_plot(Data,y_col = "y_col",x_col = "time","plot_unprocessed",perform_reshape=TRUE,
                  gen_aes_list = list(group="type",color="type"),
                  reshaping_list = list(varying_list=4:13,
                                        idvar=c("subj","type","X"),
                                        new_times = as.factor(seq(10,100,10))),
                  plot_parameters = list(labs=labs(title=Data_file_name,x ="time", y = "Accuracy"),
                                         limits = coord_cartesian(ylim = c(0.4,1))))
  # if u dont use this, the plot isnt shown
  print(p1)
  if(save_tiff) dev.off()
  
  if(save_ppt){
    ppt_path <- file.path("Sub_projects/Realtime_models/SVM_classification_models/Results/results.pptx")
    
    # save ppt
    
    if(!file.exists(ppt_path)) {
      out <- read_pptx()
    } else {
      out <- read_pptx(ppt_path)
    }
    
    out %>%
      add_slide(layout = "Two Content", master = "Office Theme") %>%
      ph_with(value = description[[Data_file_name]], location = ph_location_type(type = "title")) %>%
      ph_with_vg(code = print(p1), type = "body") %>% 
      print(target = ppt_path)
    
  }
  
  
  return(p1)
}