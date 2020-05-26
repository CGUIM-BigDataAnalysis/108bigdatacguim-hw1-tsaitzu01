### Q1
library(readr)
library(dplyr)
library(knitr)

Data104 <- read_csv("104_SalaryByEdu.csv")
Data104$大職業別 <- gsub("部門", "", Data104$大職業別)
Data104$大職業別 <- gsub("醫療保健服務業", "醫療保健業", Data104$大職業別)
Data104$大職業別 <- gsub("教育服務業", "教育業", Data104$大職業別)
Data104$大職業別 <- gsub("營造業", "營建工程", Data104$大職業別)
Data104$大職業別 <- gsub("資訊及通訊傳播業", "出版影音製作傳播及資通訊服務業", Data104$大職業別)
Data104$大職業別 <- gsub("_|、| ", "", Data104$大職業別)

Data107 <- read_csv("107_SalaryByEdu.csv")
Data107$大職業別 <- gsub("_|、| ", "", Data107$大職業別)

## Q1.1
SalaryData <- inner_join(Data104, Data107, by="大職業別")

SalaryData$`大學-薪資.x` <- gsub("—", " ", SalaryData$`大學-薪資.x`)
SalaryData$`大學-薪資.x` <- as.numeric(SalaryData$`大學-薪資.x`)
SalaryData$`大學-薪資.y` <- gsub("—|…", " ", SalaryData$`大學-薪資.y`)
SalaryData$`大學-薪資.y` <- as.numeric(SalaryData$`大學-薪資.y`)
SalaryData$薪資提高比例 <- SalaryData$`大學-薪資.y`/SalaryData$`大學-薪資.x`
SalaryData %>% 
  filter(`大學-薪資.y` > `大學-薪資.x`) %>%
  arrange(desc(薪資提高比例)) %>%
  head(10) %>%
  kable()

## Q1.2
Over5 <- SalaryData %>% 
  filter(薪資提高比例 > 1.05) %>%
  select(大職業別)
kable(Over5)

## Q1.3
strsplit(Over5$大職業別, "-") %>% 
  lapply("[", 1) %>%
  unlist() %>%
  table() %>%
  kable()

### Q2
Data104$`大學-女/男` <- gsub("—|…", " ", Data104$`大學-女/男`)
Data104$`大學-女/男` <- as.numeric(Data104$`大學-女/男`)
Data107$`大學-女/男` <- gsub("—|…", " ", Data107$`大學-女/男`)
Data107$`大學-女/男` <- as.numeric(Data107$`大學-女/男`)

## Q2.1
Data104 %>%
  arrange(`大學-女/男`) %>%
  head(10) %>%
  kable()
Data107 %>%
  arrange(`大學-女/男`) %>%
  head(10) %>%
  kable()

## Q2.2
Data104 %>%
  arrange(desc(`大學-女/男`)) %>%
  head(10) %>%
  kable()
Data107 %>%
  arrange(desc(`大學-女/男`)) %>%
  head(10) %>%
  kable()

### Q3
Data107$`大學-薪資` <- gsub("—|…", " ", Data107$`大學-薪資`)
Data107$`大學-薪資` <- as.numeric(Data107$`大學-薪資`)
Data107$`研究所-薪資` <- gsub("—|…", " ", Data107$`研究所-薪資`)
Data107$`研究所-薪資` <- as.numeric(Data107$`研究所-薪資`)

Data107$研究所及大學差異比 <- Data107$`研究所-薪資`/Data107$`大學-薪資`
Data107 %>%
  arrange(desc(研究所及大學差異比)) %>%
  head(10) %>%
  kable()

### Q4
## Q4.1
Data107 %>%
  filter(SalaryData$大職業別%in%
           c("專業科學及技術服務業",
             "專業科學及技術服務業-專業人員",
             "出版影音製作傳播及資通訊服務業",
             "出版影音製作傳播及資通訊服務業-專業人員",
             "教育業-專業人員")) %>%
  select(大職業別, `大學-薪資`, `研究所-薪資`) %>%
  kable()

## Q4.2
Data107$研究所及大學薪資差 <- (Data107$`研究所-薪資`-Data107$`大學-薪資`)
Data107 %>%
  filter(SalaryData$大職業別%in%
           c("專業科學及技術服務業",
             "專業科學及技術服務業-專業人員",
             "出版影音製作傳播及資通訊服務業",
             "出版影音製作傳播及資通訊服務業-專業人員",
             "教育業-專業人員")) %>%
  select(大職業別, `大學-薪資`, `研究所-薪資`,研究所及大學薪資差) %>%
  kable()