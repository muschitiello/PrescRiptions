library(PrescRiptions)

# read settings

addSettings()

settings = readSettings("PrescRiptions.yml")

########################################################
# #### Test of download functions
# #### !!!! attention: functions that download plpd data require a lot of time because files are very big
# 
# # Download all 2019 plpd files and save them as csv and feather data
# for(m in as.character(stringr::str_pad(1:12,2,"left","0"))){
#   downloadPLPDzip(yyyy = 2019, mm=m,basedir = settings$basedir,outFormat = "both")
# }
# 
# # Download all 2018 plpd files and save them as csv and feather data
# for(m in as.character(stringr::str_pad(1:12,2,"left","0"))){
#   downloadPLPDzip(yyyy=2018, mm=m,basedir = settings$basedir,outFormat = "both")
# }
# 
# # test githubDataDownload function
# 
# githubDataDownload(data = "bnf",yyyy=2019, mm=12,basedir = settings$basedir)
# githubDataDownload(data = "qof",yyyy=2019, mm=11,basedir = settings$basedir)
# githubDataDownload(data = "demog",yyyy=2019, mm=10,basedir = settings$basedir)
# 
# # download all data from github
# for(m in as.character(stringr::str_pad(1:12,2,"left","0"))){
#   githubDataDownload(data = "all", yyyy=2018, mm=m,basedir = settings$basedir)
# }
# 
# # Download all 2018 demog files and save them as feather data
# for(m in as.character(stringr::str_pad(1:12,2,"left","0"))){
#   downloadDemog(yyyy=2018, mm=m,basedir = settings$basedir,outFormat = "feather")
# }
# 
# # Download all 2018 demog files and save them as csv data
# for(m in as.character(stringr::str_pad(1:12,2,"left","0"))){
#   downloadDemog(yyyy=2018, mm=m,basedir = settings$basedir,outFormat = "csv")
# }
# 
# # Download all 2019 demog files and save them as feather and csv data
# for(m in as.character(stringr::str_pad(1:12,2,"left","0"))){
#   downloadDemog(yyyy=2019, mm=m,basedir = settings$basedir,outFormat = "both")
# }
########################################################

dataAll = importMonthlyData(mm=12)
dataAll
str(dataAll)

for (i in 1:length(dataAll)){
  assign(names(dataAll)[i],dataAll[[i]])
}

