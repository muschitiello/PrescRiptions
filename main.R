library(PrescRiptions)

# Download all 2019 plpd files and save them as fether data
for(m in as.character(str_pad(1:12,2,"left","0"))){
  downloadPLPDzip(mm=m,basedir = basedir)
}

# read settings

settings = readSettings("PrescRiptions.yml")

