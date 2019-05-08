easy_plot <- function(Data,y_col,x_col,dataformating,unprocessed_type='plot_unprocessed_line',
                      reshaping_list=NULL,gen_aes_list,perform_reshape=FALSE,plot_parameters=NULL,...){
  # the philosophy:
  # this function is destined to be a overall function for facilitating plotting
  # v.names_list is the list of dependent variables that 
  # idvars is the list of idvars including subject 
  # single_time_var is the one timevariable that u want to use for 
  
  

  
  library(ggplot2)

  if (!is.null(plot_parameters)){
    plot_parameters <- get_plot_parameters(plot_parameters)
  } else {
    plot_parameters <- get_plot_parameters()
    
  }
  
  
  
  
  if(perform_reshape){
    
    Data <- reshape_to_long(Data,reshaping_list)
    
  }
  
  
  
  switch(dataformating,
         processed_in_long = {
           
           
           p <- do.call("plot_processed",c(list(Data=Data,y_col=y_col,x_col=x_col,plot_parameters=plot_parameters,...)))
           p + plot_parameters[["theme_ci_piace"]]+plot_parameters[["limits"]]
           
           
         },
         plot_unprocessed = {
           
           
           
           p <- do.call(get(unprocessed_type),c(list(Data=Data,y_col=y_col,x_col=x_col,
                                                     gen_aes_list=gen_aes_list,plot_parameters=plot_parameters,...)))
           p <- p + 
             plot_parameters[["theme_ci_piace"]]+
             plot_parameters[["limits"]]+
             plot_parameters[["labs"]]
           
           
         })

  return(p)
  
}

plot_processed <- function(Data,y_col,x_col,gen_aes_list,plot_parameters,...){
  
  p <- gen_aes(Data,y_col,x_col,gen_aes_list,...)
  
  return(p)
  
  
  
}

plot_unprocessed_bar <- function(Data,y_col,x_col,gen_aes_list,plot_parameters,...){
  p <- gen_aes(Data,y_col,x_col,gen_aes_list,...)
  p <- p + stat_summary(fun.y="mean",geom = 'bar',size=3,position = "dodge")+
    stat_summary(fun.ymin = errorFunMin,fun.ymax =errorFunMax,geom = 'errorbar',
                 width = plot_parameters[["errorWidth"]],col=plot_parameters[["errorCol"]],size=plot_parameters[["errorSize"]],
                 position = position_dodge(width = 1))
  
  
  return(p)
  
}


plot_unprocessed_line <- function(Data,y_col,x_col,gen_aes_list,plot_parameters,...){
  
  # ggplot(Data,aes(time,y_col)) + 
  #   stat_summary(fun.y="mean",geom = 'line',size=1.5)+
  #   stat_summary(fun.ymin = errorFunMin,fun.ymax =errorFunMax,geom = 'errorbar',
  #                width = errorWidth,col=errorCol,size=errorSize,position = position_dodge(width = 1))
  # 
  p <- gen_aes(Data,y_col,x_col,gen_aes_list=gen_aes_list,...)
  p <- p + stat_summary(fun.y="mean",geom = 'line',size=1.3)+
    stat_summary(fun.ymin = errorFunMin,fun.ymax =errorFunMax,geom = 'errorbar',
                 width = plot_parameters[["errorWidth"]],col=plot_parameters[["errorCol"]],size=plot_parameters[["errorSize"]])
  
  
  return(p)
  
}

gen_aes <- function(Data,y_col,x_col,gen_aes_list,...){
  # for setting up fill, label etc
  # lapply(gen_aes_list,assign(names(gen_aes_list)))
  
  
  add_aes <- function(aestheticType,gen_aes_list,p){
    switch(aestheticType,
           group = {
             added_aes <- aes_(group=as.name(gen_aes_list[["group"]]))
             
           },
           label = {
             added_aes <- aes_(label=as.name(gen_aes_list[["label"]]))
             
           },
           fill = {
             added_aes <- aes_(fill=as.name(gen_aes_list[["fill"]]))
             
           },
           color = {
             added_aes <- aes_(color=as.name(gen_aes_list[["color"]]))
             
           }
           
           
           )
    
    return(added_aes)
    
  }
  
  
  
  
  
  
  p <- ggplot(Data,aes_(y=as.name(y_col),x=as.name(x_col)))
  
  added_aes_list <- lapply(names(gen_aes_list),add_aes,gen_aes_list=gen_aes_list)
  
  for(i in 1:length(added_aes_list)){
    p <- ggplot_add(added_aes_list[[i]],p)
  }
  
  
  
  # if(!is.null(gen_aes_list)){
  #   if("fill" %in% names(gen_aes_list)) {
  #     
  #     fill <- gen_aes_list[["fill"]]
  #     p <- ggplot(Data,aes_(y=as.name(y_col),x=as.name(x_col),fill=as.name(fill),...))
  #     
  #   }
  #   
  #   # not checked code
  #   
  #   
  #   if("label" %in% names(gen_aes_list)) {
  #     
  #     
  #     label <- gen_aes_list[["label"]]
  #     p <- ggplot(Data,aes_(y=as.name(y_col),x=as.name(x_col),label=as.name(label),...))
  #   }
  #   
  #   if("group" %in% names(gen_aes_list)) {
  #     
  #     
  #     group <- gen_aes_list[["group"]]
  #     p <- ggplot(Data,aes_(y=as.name(y_col),x=as.name(x_col),group=as.name(group),...))
  #   }
  #   
  #   
  #   
  #   
  #   
  # }else {
  #   
  #   p <- ggplot(Data,aes_(y=as.name(y_col),x=as.name(x_col),...))
  #   
  # }
  
  
  
  
  return(p)
  
  
}


reshape_to_long <- function(Data,reshaping_list){
  
  if("varying_list" %in% names(reshaping_list)){
    varying_list <- reshaping_list[["varying_list"]]
    
  }
  
  
  if("idvar" %in% names(reshaping_list)){
    idvar <- reshaping_list[["idvar"]]
    
  }
  
  
  
  if(is.character(varying_list)){
    varying_list <- which(names(Data) %in% varying_list)
    
  }
  
  
  if("new_times" %in% names(reshaping_list)) {
    new_times <- reshaping_list[["new_times"]]
    
  }else {
    new_times <- length(reshaping_list[["varying_list"]])
    
  }
  
  
  
  
  Data_reshaped <- reshape(Data,direction = "long",
                           varying = list(varying_list),idvar = idvar,v.names = "y_col",times = new_times)
  return(Data_reshaped)
  
}

