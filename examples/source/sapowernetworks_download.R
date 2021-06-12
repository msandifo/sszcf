#-----------------------------
# download sapowernetworks substation data to local data.directory
# as set in mnsp_funcs.R which we start by sourcing
#-----------------------------

source("~/Dropbox/msandifo/documents/programming/r/sunshot-zcf/sszcf/examples/source/mnsp_funcs.R")


#-----------------------------
# to effect downloads set to 1
#-----------------------------


if (0){
r.files <-c(
  "https://www.sapowernetworks.com.au/public/download.jsp?id=316207", 
    "https://www.sapowernetworks.com.au/public/download.jsp?id=312926", 
    "https://www.sapowernetworks.com.au/public/download.jsp?id=310425",
    "https://www.sapowernetworks.com.au/public/download.jsp?id=10264",
    "https://www.sapowernetworks.com.au/public/download.jsp?id=10262",
    "https://www.sapowernetworks.com.au/public/download.jsp?id=10260",
    "https://www.sapowernetworks.com.au/public/download.jsp?id=10258",
    "https://www.sapowernetworks.com.au/public/download.jsp?id=10256",
    "https://www.sapowernetworks.com.au/public/download.jsp?id=10252",
    "https://www.sapowernetworks.com.au/public/download.jsp?id=10247"
)

 for (i in 1:length(r.files)) get_mnsp_files(r.files[i], zip=paste0(0,i))
}

#-----------------------------
# prepare morphet vale substation
#-----------------------------

region.pattern= "Metro South"  # as a pattern to select files below 
substation.pattern= "Morphett Vale East" #

mnsp.files <-list.files(data.dir, pattern=region.pattern, full.names = T, recursive = T)

ss.name <-get_mnsp_substation_name(mnsp.files[1], substation= substation.pattern)

mv.ss.df <-purrr::map_df(mnsp.files, get_substation) |> 
  dplyr::mutate(year= lubridate::year(Date)  , 
                month= lubridate::month(Date),
                hour = lubridate::hour(Date)+
                 lubridate::minute(Date)/60,
                substation=ss.name) |>
  dplyr::arrange(Date)

arrow::write_feather(mv.ss.df, "examples/data/morphetvale.feather")


                  