library(readr)
library(dplyr)
library(readxl)
library(tidyr)
recode <- read_csv(file="P://Outside DUSON RMT Projects//IRB 82649 Immune Development//Stewart Folder//Raw data//recode.csv")
filepref<-"P://Outside DUSON RMT Projects//IRB 82649 Immune Development//Stewart Folder//Raw data//CTOT-C02 Phenotypic flow data "
excel <- c('2009 07-08','2009 09-10','2009 11-12','2010 01-02V2','2010 03-04','2010 05-06','2010 07-08','2010 09-10','2010 11-12','2011 01-02','2011 03-04','2011 05-06','2011 07-08','2011 09-10','2011 11-12','2012 01-02','2012 03-04','2012 05-06','2012 07-08','2012 09-10','2012 11-12','2013 01-02')
freq <- c('Panel 1 freq','Panel 2 freq','Panel 3  freq','Panel 4 freq','Panel 5 freq','Panel 6 freq','Panel 7 freq','Panel 8 freq','Panel 9 freq','Panel 10 freq','Panel 11 freq','Panel 12 freq','Panel 13 freq')
abs <- c('Panel 1 abs','Panel 2 abs','Panel 3 abs','Panel 4 abs','Panel 5 abs','Panel 6 abs','Panel 7 abs','Panel 8 abs','Panel 9 abs','Panel 10 abs','Panel 11 abs','Panel 12 abs','Panel 13 abs')

#Cycle through each of the 22 spreadsheets
for(h in 1:22){
  fn <- paste('file',h,sep='') #Name for each spreadsheets
  
  #Cycle through each sheet within the spreadsheet
  for (i in 1:13) {
    dn <- paste('freq_raw',i,sep='') #Name for each of sheets
    an <- paste('abs_raw',i,sep='')
    gn <- if_else(i==11,'gamma delta','CD') #Account for different variable in panel 11 for transpose
    
    #Read in sheet by sheet removing the extra column, filtering off the extra rows, and removing the R rows
    #Then perform the transpose using gather, convert the value to numeric, add additional variables, and filter of null value
    assign(dn,
           read_excel(paste(filepref,excel[h],'.xlsx',sep=''),sheet = freq[i],skip=2) %>%
             select(-`Trucount sample`,-`Date Entered`,-`Entered by`) %>%
              filter(`Patient-Timepoint`!=0 & !(`Patient-Timepoint` %in% c('C02012028V02','C02300037V07','C01003044V02')) & `A/R/?/delete`!="R" & `A/R/?/delete`!="R/?") %>%
                gather(key="item",value="value",contains(gn)) %>%
                  mutate(value=as.numeric(value),`Collection Time`=as.character(`Collection Time`),panel_type='2',source_table=freq[i],source_file=excel[h],item=chartr('ï','i',item),phenotype_a_r_delete='',trucount_a_r_delete='') %>%
                    filter(!is.na(value)) %>%
                      select(patient_timepoint=`Patient-Timepoint`,source_file,source_table,panel_type,item,value,date_drawn=`Date Drawn`,collection_time=`Collection Time`,date_reviewed=`Date Reviewed`,reviewed_by=`Reviewed by`,comments=Comments,a_r_delete=`A/R/?/delete`,trucount_a_r_delete)
           )
    
    assign(an,
           read_excel(paste(filepref,excel[h],'.xlsx',sep=''),sheet = abs[i],skip=2) %>%
             select(-`Trucount File`) %>%
             filter(`Patient-Timepoint`!=0 & !(`Patient-Timepoint` %in% c('C02012028V02','C02300037V07','C01003044V02')) & `Phenotype A/R/?/delete`!="R" & `Phenotype A/R/?/delete`!="R/?") %>%
             gather(key="item",value="value",contains(gn)) %>%
             mutate(value=as.numeric(value),panel_type='1',source_table=abs[i],source_file=excel[h],item=chartr('ï','i',item),comments='',date_drawn=NaN,collection_time='',date_reviewed=NaN,reviewed_by='') %>%
             filter(!is.na(value)) %>%
             select(patient_timepoint=`Patient-Timepoint`,source_file,source_table,panel_type,item,value,date_drawn,collection_time,date_reviewed,reviewed_by,comments,a_r_delete=`Phenotype A/R/?/delete`,trucount_a_r_delete=`Trucount A/R/?/delete`)
          )
    
  }
  
  #Concatenate each of the sheets into 1 table
  assign(fn,bind_rows(freq_raw1,freq_raw2,freq_raw3,freq_raw4,freq_raw5,freq_raw6,freq_raw7,freq_raw8,freq_raw9,freq_raw10,freq_raw11,freq_raw12,freq_raw13,abs_raw1,abs_raw2,abs_raw3,abs_raw4,abs_raw5,abs_raw6,abs_raw7,abs_raw8,abs_raw9,abs_raw10,abs_raw11,abs_raw12,abs_raw13))
}
#Concatenate each of the spreadsheets into 1 csv and add item_num
complete <-bind_rows(file1,file2,file3,file4,file5,file6,file7,file8,file9,file10,file11,file12,file13,file14,file15,file16,file17,file18,file19,file20,file21,file22) %>%
  merge(recode,by='item') %>%
  select(patient_timepoint,source_file,source_table,panel_type,item,itemnum,value,date_drawn,collection_time,date_reviewed,reviewed_by,comments,a_r_delete) %>%
  arrange(source_file,patient_timepoint,source_table,itemnum)

write.csv(complete,file="P://Outside DUSON RMT Projects//IRB 82649 Immune Development//Stewart Folder//Complete//CTOTO2_ALL_DATA_COMPLETE.csv",na='',row.names=FALSE)