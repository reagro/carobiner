## Cedric Ngakou & Effie 

check_duplicate <- function(path,group){
  
  data<-utils::read.csv(file.path(path, "data","compiled", paste0("carob","_",as.character(group),".","csv")))
  d <-unique(data["dataset_id"])
  list_of_id<- as.list(d$dataset_id)
  lst <- data.frame()
  for (i in 1:length(list_of_id)){
    j<-list_of_id[i] # extract unique dataset_id 
    data_id<-data[data$dataset_id==j,] # dataset you may want to check if it's duplicate
    k<- data_id[,c("country","yield","crop")] # 
    k1<-data[data$dataset_id!=j,]
    k11<- k1[,c("country","yield","crop")]
    l<- k$country %in% k11$country  & k$crop %in% k11$crop & k$yield %in% k11$yield 
    k<- k[l,]
    common <- match(k$yield, k1$yield) # get common rows 
    dcom<-k1[common,] 
    if (nrow(dcom)==0){ 
      message("No duplicate data")
      }
    else {
      dcom$duplicate_id <- j 
      dcom <- stats::na.omit(dcom[,c("dataset_id","duplicate_id")])
      dcom1<-unique(dcom[,c("dataset_id","duplicate_id")])
      for (N  in 1:nrow(dcom1)) {
        i1<- dcom1$dataset_id[N]
        i2<-dcom[dcom$dataset_id==i1,]
        n<- nrow(i2)
        Rate<- (n/nrow(k))*100 ### max(Rate)= 100%
        if (Rate >=97){    
          print(paste(j," is duplicate with",i1))
        }
        
      }
      
    }
    
  }
  
}
