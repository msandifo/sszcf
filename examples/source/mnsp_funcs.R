
data.directory<- "~/data/regional/australia/energy/nemweb/mnsp/sapowernetworks"

get_mnsp_files <- function(remote, local= paste0(data.directory,"/01.zip"), zip="01", remove=T){
  
  if (!(file.exists(local))) {
    download.file(remote, local)
    unzip(local, exdir= paste0(dirname(local),"/",zip), overwrite=F)
  }
  if (remove) file.remove(local)
}

get_mnsp_substation_name <-function( mnsp.file, 
                                     substation= "Morphett Vale East"){
  mnsp.names <-names(readr::read_csv(mnsp.file, skip=1, n_max=0 ))
  
  sub.index <- which(stringr::str_detect(  mnsp.names , substation), arr.ind = T)
  mnsp.names[sub.index]
}

get_mnsp_substation<-function( mnsp.file, 
                               substation= "Morphett Vale East"){
  
  mnsp.names <-names(readr::read_csv(mnsp.file, skip=1, n_max=0 ))
  
  sub.index <- which(stringr::str_detect(  mnsp.names , substation), arr.ind = T)
  
  data.table::fread(mnsp.file, skip=3, select= c(1,2,  (sub.index-1):(sub.index+1))) |> 
    dplyr::mutate( Date = lubridate::dmy_hm(stringr::str_c(Date, Time, sep= " "))) |>
    dplyr::select(-Time)
  
}

