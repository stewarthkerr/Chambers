#Purpose: To clean and merge the FREQ panels in raw excel data for the abstract due 12/1/17
library(readr)
library(dplyr)
library(readxl)
library(tidyr)
recode <- read_csv(file="P://Outside DUSON RMT Projects//IRB 82649 Immune Development//Stewart Folder//Raw data//recode.csv")
filepref<-"P://Outside DUSON RMT Projects//IRB 82649 Immune Development//Stewart Folder//Raw data//CTOT-C02 Phenotypic flow data "
excel <- c('2009 07-08','2009 09-10','2009 11-12','2010 01-02V2','2010 03-04','2010 05-06','2010 07-08','2010 09-10','2010 11-12','2011 01-02','2011 03-04','2011 05-06','2011 07-08','2011 09-10','2011 11-12','2012 01-02','2012 03-04','2012 05-06','2012 07-08','2012 09-10','2012 11-12','2013 01-02')
sheet <- c('Panel 1 freq','Panel 2 freq','Panel 3  freq','Panel 4 freq','Panel 5 freq','Panel 6 freq','Panel 7 freq','Panel 8 freq','Panel 9 freq','Panel 10 freq','Panel 11 freq','Panel 12 freq','Panel 13 freq')

#Cycle through each of the 22 spreadsheets
for(h in 1:22){
  fn <- paste('file',h,sep='') #Name for each spreadsheets
  
  #Cycle through each sheet within the spreadsheet
  for (i in 1:13) {
    dn <- paste('raw',i,sep='') #Name for each of sheets
    gn <- if_else(i==11,'gamma delta','CD') #Account for different variable in panel 11 for transpose
    
    #Read in sheet by sheet removing the extra column, filtering off the extra rows, and removing the R rows
    #Then perform the transpose using gather, convert the value to numeric, add additional variables, and filter of null value
    assign(dn,
           read_excel(paste(filepref,excel[h],'.xlsx',sep=''),sheet = sheet[i],skip=2) %>%
             select(-`Trucount sample`,-`Date Entered`,-`Entered by`) %>%
              filter(`Patient-Timepoint`!=0 & !(`Patient-Timepoint` %in% c('C02012028V02','C02300037V07','C01003044V02')) & `A/R/?/delete`!="R" & `A/R/?/delete`!="R/?") %>%
                gather(key="item",value="value",contains(gn)) %>%
                  mutate(value=as.numeric(value),panel_type='2',source_table=sheet[i],source_file=excel[h],item=chartr('ï','i',item)) %>%
                    filter(!is.na(value)) %>%
                      rename(patient_timepoint=`Patient-Timepoint`,comments=Comments,date_drawn=`Date Drawn`,collection_time=`Collection Time`,date_reviewed=`Date Reviewed`,reviewed_by=`Reviewed by`,a_r_delete=`A/R/?/delete`)
           )
  }
  #Concatenate each of the sheets into 1 table
  assign(fn,rbind(raw1,raw2,raw3,raw4,raw5,raw6,raw7,raw8,raw9,raw10,raw11,raw12,raw13))
}
#Concatenate each of the spreadsheets into 1 csv and add item_num
complete <-rbind(file1,file2,file3,file4,file5,file6,file7,file8,file9,file10,file11,file12,file13,file14,file15,file16,file17,file18,file19,file20,file21,file22) %>%
    merge(recode,by='item')

write.csv(complete,file="P://Outside DUSON RMT Projects//IRB 82649 Immune Development//Stewart Folder//Complete//CTOTO2 FREQ DATA_COMPLETE.csv",na='',row.names=FALSE)