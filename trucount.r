library(readr)
library(dplyr)
library(readxl)
library(tidyr)
recode <- read_csv(file="P://Outside DUSON RMT Projects//IRB 82649 Immune Development//Stewart Folder//Raw data//recode_trucount.csv")
filepref<-"P://Outside DUSON RMT Projects//IRB 82649 Immune Development//Stewart Folder//Raw data//CTOT-C02 Phenotypic flow data "
excel <- c('2009 07-08','2009 09-10','2009 11-12','2010 01-02V2','2010 03-04','2010 05-06','2010 07-08','2010 09-10','2010 11-12','2011 01-02','2011 03-04','2011 05-06','2011 07-08','2011 09-10','2011 11-12','2012 01-02','2012 03-04','2012 05-06','2012 07-08','2012 09-10','2012 11-12','2013 01-02')

#Define a function for calculating freq
freq_calc <- function(cell, lymph = Lymphocyte){
  x = 100*(cell/lymph)
  return(x)
}

#Cycle through each of the 22 spreadsheets
for(h in 1:22){
  fn <- paste('file',h,sep='') #Name for each spreadsheets
  
    #Read in sheet by sheet removing the extra column, filtering off the extra rows, and removing the R rows
    #Then perform the transpose using gather, convert the value to numeric, add additional variables, and filter of null value
    assign(fn,
           read_excel(paste(filepref,excel[h],'.xlsx',sep=''),sheet = 'Trucount') %>%
              filter(`A/R`!="R" & `A/R`!="R/?") %>%
              mutate(`B cells_FREQ` = 100*(as.numeric(`B cells`)/Lymphocyte),
                     `T cells_FREQ` = 100*(as.numeric(`T cells`)/Lymphocyte),
                     `CD4+CD8+_FREQ` = 100*(as.numeric(`CD4+CD8+`)/Lymphocyte),
                     `CD4+ T cells_FREQ` = 100*(as.numeric(`CD4+ T cells`)/Lymphocyte),
                     `CD8+ T cells_FREQ` = 100*(as.numeric(`CD8+ T cells`)/Lymphocyte),
                     `CD3+CD4-CD8-_FREQ` = 100*(as.numeric(`CD3+CD4-CD8-`)/Lymphocyte),
                     `Lymph / NOT CD3 or CD20_FREQ` = 100*(as.numeric(`Lymph / NOT CD3 or CD20`)/Lymphocyte),
                     `lymph /CD3- CD20-/ CD16+ CD56-_FREQ` = 100*(as.numeric(`lymph /CD3- CD20-/ CD16+ CD56-`)/Lymphocyte),
                     `lymph /CD3- CD20-/ CD16+ CD56+_FREQ` = 100*(as.numeric(`lymph /CD3- CD20-/ CD16+ CD56+`)/Lymphocyte),
                     `lymph /CD3- CD20-/ CD16- CD56+_FREQ` = 100*(as.numeric(`lymph /CD3- CD20-/ CD16- CD56+`)/Lymphocyte),
                     `lymph / CD3- CD20- / CD16- CD56-_FREQ` = 100*(as.numeric(`lymph / CD3- CD20- / CD16- CD56-`)/Lymphocyte)) %>%
              select(`Trucount sample`:`lymph / CD3- CD20- / CD16- CD56-`,`B cells_FREQ`:`lymph / CD3- CD20- / CD16- CD56-_FREQ`,Comments:`A/R`) %>%
              gather(key="item",value="value",Lymphocyte:`lymph / CD3- CD20- / CD16- CD56-_FREQ`) %>%
              mutate(value=as.numeric(value),`Entered by` = as.character(`Entered by`),Comments = as.character(Comments),`Visit #` = as.character(`Visit #`),`Collection Time`=as.character(`Collection Time`),panel_type='3',source_table='Trucount',source_file=excel[h],item=chartr('ï','i',item)) %>%
              filter(!is.na(value)))
  }
  
complete <- bind_rows(file1,file2,file3,file4,file5,file6,file7,file8,file9,file10,file11,file12,file13,file14,file15,file16,file17,file18,file19,file20,file21,file22) %>%
  mutate(calc_type = ifelse(grepl('_FREQ',item),'FREQ','ABS')) %>%
  mutate(item = gsub('_FREQ','',item)) %>%
  merge(recode,by='item') %>%
  arrange(source_file,`Trucount sample`,itemnum,calc_type) %>%
  select(`Trucount sample`:`A/R`,panel_type,source_table,source_file,item,itemnum,calc_type,value)

write.csv(complete,file="P://Outside DUSON RMT Projects//IRB 82649 Immune Development//Stewart Folder//Complete//CTOTO2_TRUCOUNT_DATA_COMPLETE.csv",na='',row.names=FALSE)