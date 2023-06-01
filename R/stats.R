
library(dplyr)
library(purrr)
library(ggthemes)

setwd("C:/Users/ftw712/Desktop/gbif-cit-sci/")

cs_keys = jsonlite::fromJSON("http://api.gbif.org/v1/dataset?machineTagNamespace=citizenScience.mgrosjean.gbif.org&limit=1000") %>% 
pluck("results") %>%
pull(key) %>%
glimpse()

ds_files = httr::GET("https://github.com/jhnwllr/gbif-datasetkey-counts/tree/main/exports/tsv?recursive=1") %>% 
httr::content() %>%
as.character() %>%
stringr::str_extract_all('[0-9]{4}-[0-9]{2}-[0-9]{2}') %>% 
pluck(1) %>%
unique()

d_gbif = readr::read_csv("https://analytics-files.gbif.org/global/csv/occ.csv") %>% 
mutate(type = "all GBIF") %>% 
select(created = snapshot, n_occ = occurrenceCount,type) %>%  
glimpse() 

d_cs = ds_files %>% 
map(~ 
readr::read_tsv(paste0("https://raw.githubusercontent.com/jhnwllr/gbif-datasetkey-counts/main/exports/tsv/",.x,".tsv")) %>%
filter(datasetkey %in% cs_keys)) %>%
bind_rows() %>%
group_by(created) %>%
summarise(n_occ = sum(n_occ)) %>% 
mutate(type = "citizen science") %>%
glimpse() 

d = rbind(d_cs,d_gbif) %>% 
glimpse()

d %>% readr::write_tsv("exports/table.tsv")

library(ggplot2)

breaks = seq(0,3000e6,100e6)
label = c(0,paste(seq(100,900,100),"M"),paste(seq(1,3,0.1),"B"))

p = ggplot(d,aes(created, n_occ,colour=type)) + 
scale_y_continuous(breaks = breaks,label = label) + 
geom_line() + 
geom_point() +
theme_hc() + 
scale_colour_manual(values = c("#231F20", "#509E2F")) + 
ylab("number of occurrences") + 
xlab("") + 
guides(colour=guide_legend(title="")) +  
theme(legend.position = c(.925,.1)) + 
theme(legend.background = element_rect(fill="white")) + 
scale_x_date(date_breaks = "1 year",date_labels = "%Y") + 
theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) 

ggsave("plots/plot.pdf", plot = p,width=10,height=7)
ggsave("plots/plot.svg", plot = p,width=10,height=7)
ggsave("plots/plot.png", plot = p,width=10,height=7,dpi=600)

