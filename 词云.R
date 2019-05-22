rm(list = ls())
library(jiebaRD)
library(jiebaR)
library(wordcloud2)
f <- readLines('E:/数统研究生/多元数据分析/总集/第一章/数据/云南白药点评.txt')
               seg <- qseg[f]
               seg <- seg[nchar(seg)>1]
               seg <- table(seg)
               seg <- seg[!grepl('[0-9]+',names(seg))]
               length(seg)
               seg <- sort(seg, decreasing = TRUE)[1:100]
               
               wordcloud2(seg)
               
