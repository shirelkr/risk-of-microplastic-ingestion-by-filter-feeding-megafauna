#---- Set Up ----
# data information
alldata_path <- "C:/Users/Gold Field/Documents/Shirel/Chapter 3- Plastic/Plastic Risk Assessment/alldata"

# ---- Utility Functions ----
#function takes path to alldata and name of file within that folder (lunge files)
findlungemat <- function(lunge_filename, lunge_dir) { #defining placeholders lunge_dir represents what ever I tell it 
  
  if (is.na(lunge_filename)){return(NA)}
  dir(lunge_dir, pattern = lunge_filename, full.name= TRUE)
  
}

#function takes path to alldata and name of file within that folder (prh files)
findprhmat <- function(prh_filename, prh_dir) {  #defining placeholders lunge_dir represents what ever I tell it 
  
  if (is.na(prh_filename)){return(NA)}
  result <- dir(prh_dir, pattern = prh_filename, full.name= TRUE)
  
  if (length(result) == 0) {return(NA)}
  result
}

lungehasp <- function(checklungefile) {   #where the mat files is and to be able to read the mat file
  lunge_mat<- readMat(checklungefile)
  'LungeDepth' %in% names(lunge_mat)  #is the character string LungeP in the list of names in lunge_mat
 } 

#finds lungei and depth
extractlungedata<- function(lungepath, prhpath) { 
  find_lunge_mat <- readMat(lungepath)
  LungeI <- find_lunge_mat$LungeI
  LungeDepth <- find_lunge_mat$LungeDepth
  LungeDN <- find_lunge_mat$LungeDN

  # If lunge mat doesn't have LungeDepth *or* LungeDN, have to open the PRH
  if(is.null(LungeDepth) || is.null(LungeDN)) { 
    prh <- readMat(prhpath)
    stopifnot(max(LungeI) <= length(prh$p))
    lunge_depth <- prh$p[LungeI]
    lunge_time <- matlab_to_posix_CA(prh$DN[LungeI])
  } else {
    lunge_depth <- as.vector(LungeDepth)
    lunge_time <- matlab_to_posix_CA(as.vector(LungeDN))
  }
  
  # Return list of both depth and time
  list(lunge_depth = lunge_depth, lunge_time = lunge_time)
  
}


# converts matlab date number into date time that is readable 
matlab_to_posix = function(x, timez = "UTC") { 
  as.POSIXct((x - 719529) * 86400, origin = "1970-01-01", tz = "UTC") %>% 
    force_tz(timez)
}
matlab_to_posix_CA <- function(x) { 
  matlab_to_posix(x, timez = "US/Pacific")
}

# get tag on and tag off times from prh (go into one column that is split into two)
get_tagon_tagoff <- function(prhpath) { 
  prh_mat <- readMat(prhpath)
  #stopifnot(!is.null(prh_mat$tagon))
  tag_on <- matlab_to_posix_CA(prh_mat$DN[first(which(prh_mat$tagon == 1))])
  tag_off <- matlab_to_posix_CA(prh_mat$DN[last(which(prh_mat$tagon == 1))])
  list(tag_on = tag_on, tag_off = tag_off)
}

# hardcoded with buckets based on Choy et al 2019
#export to an rds file 
cut_depth <- function(lunge_depth) { 
  depth_bucket = cut(lunge_depth,  
                     breaks = c(-Inf, 0.5, 5, 50, 150, Inf),
                     labels = c("Surface (0-0.5m)", 
                                "Sub-Surface (0.5-5m)",
                                "Shallow (5-50m)",
                                "Moderate (50-150m)",
                                "Deep (>150m)"))
}

# ---- Process Data ----
deployments <- read_csv("data/raw/alldata_CAwhales.csv") %>% 
  # find prh and lunge .mat
  #slice(162:164) %>% 
  mutate(lungepath = map_chr(lunge_Name, findlungemat, lunge_dir = alldata_path),
         prhpath = map_chr(prh_Name, findprhmat, prh_dir = alldata_path)) %>%
  drop_na(lungepath, prhpath) %>% 
  # true false of whether the prh or lunge file has the lunges 
  mutate(haslungeDepth = map_lgl(lungepath, lungehasp)) %>% 
  # finding tag_on_off times from prh
  mutate(tag_on_off = map(prhpath, get_tagon_tagoff)) %>% 
  unnest_wider(tag_on_off) %>% 
  drop_na(tag_on, tag_off) %>% 
  #adding species names - fix to always have the same code 
  mutate(spp_short = substr(deployID, start = 1, stop = 2),
         species_code = case_when(
           spp_short == "Bm" ~ "bw",
           spp_short == "Bp" ~ "bp",
           spp_short == "Mn" ~ "mn",
           spp_short == "mn" ~ "mn",
           spp_short == "bw" ~ "bw",
           spp_short == "bp" ~ "bp")) %>% 
  select(-spp_short)

lunges <- deployments %>% 
  # unnest lunges (make each lunge and time it's own line)
  mutate(lunge_data = map2(lungepath, prhpath, extractlungedata)) %>% 
  unnest_wider(lunge_data) %>% 
  unchop(cols = c(lunge_depth, lunge_time)) %>% 
  mutate(depth_bucket = cut_depth(lunge_depth))

#----#Sanity Check ----
#check to see if there are nas
# check species codes
stopifnot(
  all.equal(sort(unique(deployments$species_code)), c("bp", "bw", "mn")),
  !is.na(lunges$depth_bucket)
  )
  
  
# ---- Export Data ----
saveRDS(deployments, "data/output/deployments.RDS")
saveRDS(lunges, "data/output/lunges.RDS")



#lunges %>% 
 # rowid_to_column() %>% 
  # filter(is.na(depth_bucket))

