add_aes <- function(aestheticType,gen_aes_list){
  switch(aestheticType,
         group = {
           added_aes <- aes_(group=as.name(gen_aes_list[["group"]]))
           
         },
         label = {
           added_aes <- aes_(label=as.name(gen_aes_list[["label"]]))
           
         },
         fill = {
           added_aes <- aes_(fill=as.name(gen_aes_list[["fill"]]))
           
         }
         
         
  )
  return(added_aes)
  
}