library(stringr)
library(data.table)

# Download all 2019 plpd files and save them as fether data
for(m in as.character(str_pad(1:12,2,"left","0"))){
  downloadPLPDzip(mm=m,basedir = basedir)
}

# Work on single month at a time

month = 12
year = 2019
basedir = "D:/Progetti/2020_erum2020/"

# Upload PLPD data

pdpi = read_feather(str_params)


# Download BNF code information
# latest version 01/01/2020
# from (https://apps.nhsbsa.nhs.uk/infosystems/data/showDataSelector.do?reportId=126)
# Download of csv file has to be done manually

bnf = data.table(read.csv(
  paste0(basedir,"dataInput/02_bnf/csv","/",
         list.files(paste0(basedir,"dataInput/02_bnf/csv")))))

