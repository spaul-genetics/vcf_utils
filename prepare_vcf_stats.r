#' ---
#' title: "VCF summary"
#' author: "Subrata Paul"
#' date: '2022-06-02'
#' output:
#'   html_document:
#'   toc: false # table of content true
#'   toc_depth: 2  # upto three depths of headings (specified by #, ## and ###)
#'   number_sections: false  ## if you want number sections at each table header
#'   theme: united  # many options for theme, this one is my favorite.
#'   highlight: tango  # specifies the syntax highlighting style
#'   toc_float: true
#'   code_folding: hide
#'   embed-resources: true
#'   standalone: true
#' ---

#' <style type="text/css">
#'   .main-container {
#'     max-width: 100% !important;
#'     margin: auto;
#'   }
#' .fixed-height150 .tab-content {
#'   height: 150em;
#' }
#' .fixed-height100 .tab-content {
#'   height: 100em;
#' }
#' .fixed-height50 .tab-content {
#'   height: 50em;
#' }
#' .fixed-height70 .tab-content {
#'   height: 70em;
#' }

#' .section h1 {
#'   color: #0F9434;
#' }

#' .section h2 {
#'   color: #1072A5;
#' }

#' .section h3 {
#'   color: #8D10A5
#' }

#' .title {
#'   margin-top: 2em !important;
#'   margin-bottom: 0.75em;
#'   text-align: center;
#'   color: #D63A18;
#' }

#' .author {
#'   text-align: center;
#'   font-size: 25px;
#' }

#' .date {
#'   text-align: center;
#' }
#' </style>   

#+ warning=F, message=F, echo=F

knitr::opts_chunk$set(echo = F, warning = F, message = F)
library(kableExtra, quietly = T)
library(DT, quietly = T)
library(ggpubr, quietly = T)
library(tidyverse, quietly = T)
create_dt <- function(x){
  DT::datatable(x,
                extensions = 'Buttons',
                options = list(dom = 'Blfrti',
                               buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                               lengthMenu = list(c(10,25,50,-1),
                                                 c(10,25,50,"All")),
                               pageLength = 10000, scrollY = "400px"))
}

data = readLines(vcf_stats)
header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}
overview_summary = grep('SN\\t', data, value = T)%>%stringr::str_split('\t', simplify = T)%>%gsub('# |\\[[1-9]]','',.)%>%data.frame()%>%header.true()

singleton_summary = grep('SiS\\t', data, value = T)%>%stringr::str_split('\t', simplify = T)%>%gsub('# |\\[[1-9]]','',.)%>%data.frame()%>%header.true()

af_summary = grep('AF\\t', data, value = T)%>%stringr::str_split('\t', simplify = T)%>%gsub('# |\\[[1-9]]','',.)%>%data.frame()%>%header.true()

qual_summary = grep('QUAL\\t', data, value = T)%>%stringr::str_split('\t', simplify = T)%>%gsub('# |\\[[1-9]]','',.)%>%data.frame()%>%header.true()

indel_summary = grep('IDD\\t', data, value = T)%>%stringr::str_split('\t', simplify = T)%>%gsub('# |\\[[1-9]]','',.)%>%data.frame()%>%header.true()

subs_summary = grep('ST\\t', data, value = T)%>%stringr::str_split('\t', simplify = T)%>%gsub('# |\\[[1-9]]','',.)%>%data.frame()%>%header.true()



#' ## {.tabset}

#' ### Overview 


overview_summary[,-c(1,2)]%>%kbl()%>%
  kable_styling(bootstrap_options = c("striped", "hover"))


#' ### Singletons


singleton_summary%>%gather()%>%dplyr::filter(!key%in%c('SiS','id'))%>%kbl()%>%
  kable_styling(bootstrap_options = c("striped", "hover"))


#' ### AF {.tabset}

#' #### Figure

#+ fig.width=15

plot_data = af_summary%>%mutate(MAF = as.numeric(`allele frequency`))%>%mutate(MAF = ifelse(MAF>0.5, 1 - MAF, MAF))%>%group_by(MAF)%>%summarise_at(vars(contains('number')), ~sum(as.numeric(.x)))%>%gather(key = "key", value = 'Numbers', -MAF)
#plot_data = af_summary%>%gather(key = "key", value = 'Numbers', -AF, -id,-`allele frequency`)%>%mutate(Numbers = as.numeric(Numbers))
ggplot(plot_data, aes(x = as.character(round(MAF,3)), y = Numbers, fill = key))+
  geom_bar(stat = 'identity')+
  theme_pubclean()+
  theme(axis.text.x = element_text(angle = 45, size = 7, hjust = 1, vjust = 1))


#' #### Data


create_dt(af_summary%>%select(-AF,-id))


#'### QUAL {.tabset}

#' #### Figure

#+ fig.width=10

plot_data = qual_summary%>%gather(key = "key", value = 'Numbers', -QUAL, -id,-Quality)%>%mutate(Numbers = as.numeric(Numbers), Quality = as.numeric(Quality))
ggplot(plot_data, aes(x = Quality, y = Numbers, fill = key))+
  geom_bar(stat = 'identity')+
  theme_pubclean()


#' #### Data


create_dt(qual_summary%>%select(-QUAL,-id))


#' ### INDEL {.tabset}

#' #### Figure

#+ fig.width=10
plot_data = indel_summary%>%rename(length = `length (deletions negative)`)%>%mutate(count = as.numeric(count), length = as.numeric(length))
ggplot(plot_data, aes(x = length, y = count, fill = abs(count)))+
  geom_bar(stat = 'identity')+
  theme_pubclean()


#' #### Data


create_dt(indel_summary%>%select(-IDD,-id))


#' ### Substitution types {.tabset}

#' #### Figure

#+ fig.width=10
plot_data = subs_summary%>%mutate(count = as.numeric(count))
ggplot(plot_data, aes(x = type, y = count))+
  geom_bar(stat = 'identity')+
  theme_pubclean()


#' #### Data


create_dt(subs_summary%>%select(-ST,-id))

