library(tidyverse)

# Google scholar
gs1 = read.csv('google_scholar_balanced inventory of desirable responding_2022-04-13.csv')
gs2 = read.csv('google_scholar_impression management_2022-04-13.csv')
gs3 = read.csv('google_scholar_marlow-crown_2022-04-13.csv')
gs4 = read.csv('google_scholar_marlow-crowne_2022-04-13.csv')
gs5 = read.csv('google_scholar_marlowe-crown_2022-04-13.csv')
gs6 = read.csv('google_scholar_marlowe-crowne_2022-04-13.csv')
gs7 = read.csv('google_scholar_self-deceptive enhancement_2022-04-13.csv')
gs8 = read.csv('google_scholar_social desirability_2022-04-13.csv')
gs9 = read.csv('google_scholar_socially desirable responding_2022-04-13.csv')

gs = rbind(gs1, gs2, gs3, gs4, gs5, gs6, gs7, gs8, gs9)

# PsychInfo, PsyArticles, ERIC
PPE1 = read.csv('PPE_bidr.csv', fileEncoding="UTF-8-BOM")
PPE1$search_string = 'balanced inventory of desirable responding'

PPE2 = read.csv('PPE_impression management.csv', fileEncoding="UTF-8-BOM")
PPE2$search_string = 'impression management'

PPE3 = read.csv('PPE_marlow-crowne.csv', fileEncoding="UTF-8-BOM")
PPE3$search_string = 'marlow-crowne'

PPE4 = read.csv('PPE_marlowe-crowne.csv', fileEncoding="UTF-8-BOM")
PPE4$search_string = 'marlowe-crowne'

PPE5 = read.csv('PPE_self-deceptive enhancement.csv', fileEncoding="UTF-8-BOM")
PPE5$search_string = 'self-deceptive enhancement'

PPE6 = read.csv('PPE_social desirability.csv', fileEncoding="UTF-8-BOM")
PPE6$search_string = 'social desirability'

PPE7 = read.csv('PPE_socially desirable responding.csv', fileEncoding="UTF-8-BOM")
PPE7$search_string = 'socially desirable responding'

PPE = rbind(PPE1, PPE2, PPE3, PPE4, PPE5, PPE6, PPE7)

# Psychinfo Measures
psyinf1 = read.csv('psychinfo_measures_bidr.csv', fileEncoding="UTF-8-BOM")
psyinf1$search_string = 'balanced inventory'

psyinf2 = read.csv('psychinfo_measures_bidr2.csv', fileEncoding="UTF-8-BOM")
psyinf2$search_string = 'desirable responding'

psyinf3 = read.csv('psychinfo_measures_impression management.csv', fileEncoding="UTF-8-BOM")
psyinf3$search_string = 'impression management'

psyinf4 = read.csv('psychinfo_measures_marlow-crowne.csv', fileEncoding="UTF-8-BOM")
psyinf4$search_string = 'marlow-crowne'

psyinf5 = read.csv('psychinfo_measures_marlowe-crown.csv', fileEncoding="UTF-8-BOM")
psyinf5$search_string = 'marlowe-crown'

psyinf6 = read.csv('psychinfo_measures_marlowe-crowne.csv', fileEncoding="UTF-8-BOM")
psyinf6$search_string = 'marlowe-crowne'

psyinf7 = read.csv('psychinfo_measures_social desirability.csv', fileEncoding="UTF-8-BOM")
psyinf7$search_string = 'social desirability'

psyinf8 = read.csv('psychinfo_measures_socially desirable responding.csv', fileEncoding="UTF-8-BOM")
psyinf8$search_string = 'socially desirable responding'

psyinf = rbind(psyinf1,psyinf2,psyinf3,psyinf4,psyinf5,psyinf6,psyinf7,psyinf8)

# PubMed
pubmed1 = read.csv('pubmed_impression_management.csv', fileEncoding="UTF-8-BOM")
pubmed1$search_string = 'impression management'

pubmed2 = read.csv('pubmed_social_desirability.csv', fileEncoding="UTF-8-BOM")
pubmed2$search_string = 'social desirability'

pubmed3 = read.csv('pubmed_socially_desirable_responding.csv', fileEncoding="UTF-8-BOM")
pubmed3$search_string = 'socially desirable responding'

pubmed = rbind(pubmed1, pubmed2, pubmed3)

# ScienceDirect
scidir1 = read.csv('ScienceDirect_bidr.csv', fileEncoding="UTF-8-BOM")
scidir1$search_string = 'balanced inventory of desirable responding'

scidir2 = read.csv('ScienceDirect_impression management.csv', fileEncoding="UTF-8-BOM")
scidir2$search_string = 'impression management'
scidir2 = scidir2[,colnames(scidir1)]

scidir3 = read.csv('ScienceDirect_marlow-crowne.csv', fileEncoding="UTF-8-BOM")
scidir3$search_string = 'marlow-crowne'
scidir3 = scidir3[,colnames(scidir1)]

scidir4 = read.csv('ScienceDirect_marlowe-crowne.csv', fileEncoding="UTF-8-BOM")
scidir4$search_string = 'marlowe-crowne'
scidir4 = scidir4[,colnames(scidir1)]

scidir5 = read.csv('ScienceDirect_self-deceptive enhancement.csv', fileEncoding="UTF-8-BOM")
scidir5$search_string = 'self-deceptive enhancement'
scidir5 = scidir5[,colnames(scidir1)]

scidir6 = read.csv('ScienceDirect_social_desirability.csv', fileEncoding="UTF-8-BOM")
scidir6$search_string = 'social desirability'
scidir6 = scidir6[,colnames(scidir1)]

scidir7 = read.csv('ScienceDirect_socially_desirable_responding.csv', fileEncoding="UTF-8-BOM")
scidir7$search_string = 'socially desirable responding'
scidir7 = scidir7[,colnames(scidir1)]

scidir = rbind(scidir1,scidir2,scidir3,scidir4,scidir5,scidir6,scidir7)

# Scopus
scopus1 = read.csv('scopus_bidr.csv', fileEncoding="UTF-8-BOM")
scopus1$search_string = 'balanced inventory of desirable responding'

scopus2 = read.csv('scopus_impression management.csv', fileEncoding="UTF-8-BOM")
scopus2$search_string = 'impression management'

scopus3 = read.csv('scopus_marlow-crowne.csv', fileEncoding="UTF-8-BOM")
scopus3$search_string = 'marlow-crowne'

scopus4 = read.csv('scopus_marlowe-crowne.csv', fileEncoding="UTF-8-BOM")
scopus4$search_string = 'marlowe-crowne'

scopus5 = read.csv('scopus_self-deceptive enhancement.csv', fileEncoding="UTF-8-BOM")
scopus5$search_string = 'self-deceptive enhancement'

scopus6 = read.csv('scopus_social_desirability.csv', fileEncoding="UTF-8-BOM")
scopus6$search_string = 'social desirability'

scopus7 = read.csv('scopus_socially desirable responding.csv', fileEncoding="UTF-8-BOM")
scopus7$search_string = 'socially desirable responding'

scopus = rbind(scopus1,scopus2,scopus3,scopus4,scopus5,scopus6,scopus7)

# Web of Science
wos1 = readxl::read_excel("WOS_bidr.xls")
wos1$search_string = 'balanced inventory of desirable responding'

wos2 = readxl::read_excel("WOS_impression management.xls")
wos2$search_string = 'impression management'

wos3 = readxl::read_excel("WOS_marlowe-crowne.xls")
wos3$search_string = 'marlowe-crowne'

wos4 = readxl::read_excel("WOS_self-deceptive enhancement.xls")
wos4$search_string = 'self-deceptive enhancement'

wos5 = readxl::read_excel("WOS_social desirability.xls")
wos5$search_string = 'social desirability'

wos6 = readxl::read_excel("WOS_socially desirable responding.xls")
wos6$search_string = 'socially desirable responding'

wos = rbind(wos1,wos2,wos3,wos4,wos5,wos6)

# ProQuest
proquest1 = readxl::read_excel("ProQuest_bidr.xls")
proquest1$search_string = 'balanced inventory of desirable responding'

proquest2 = readxl::read_excel("ProQuest_impression management.xls")
proquest2$search_string = 'impression management'
proquest2 = proquest2[, colnames(proquest1)]
  
proquest3 = readxl::read_excel("ProQuest_marlow-crown.xls")
proquest3$search_string = 'marlow-crown'
proquest3 = proquest3[, colnames(proquest1)]

proquest4 = readxl::read_excel("ProQuest_marlow-crowne.xls")
proquest4$search_string = 'marlow-crowne'
proquest4 = proquest4[, colnames(proquest1)]

proquest5 = readxl::read_excel("ProQuest_marlowe-crown.xls")
proquest5$search_string = 'marlowe-crown'
proquest5 = proquest5[, colnames(proquest1)]

proquest6 = readxl::read_excel("ProQuest_marlowe-crowne.xls")
proquest6$search_string = 'marlowe-crowne'
proquest6 = proquest6[, colnames(proquest1)]

proquest7 = readxl::read_excel("ProQuest_self-deceptive enhancement.xls")
proquest7$search_string = 'self-deceptive enhancement'
proquest7 = proquest7[, colnames(proquest1)]

proquest8 = readxl::read_excel("ProQuest_social desirability.xls")
proquest8$search_string = 'social desirability'
proquest8 = proquest8[, colnames(proquest1)]

proquest9 = readxl::read_excel("ProQuest_socially desirable responding.xls")
proquest9$search_string = 'socially desirable responding'
proquest9 = proquest9[, colnames(proquest1)]

proquest = rbind(proquest1,proquest2,proquest3,proquest4,proquest5,proquest6,
                 proquest7,proquest8,proquest9)



# tidying up datasets
common_cols = c('source','Doctype','Article.Title','Authors','Journal.Book','Year', 'search_string', 'DOI', 'URL')

# Google scholar
gs = rename(gs, Article.Title = title, Journal.Book = publication_info, search_string = keyword, URL = 'link')
gs$source = 'Google Scholar'
gs$Doctype = NA
gs$DOI = NA
gs = gs[, common_cols]

# PPE
colnames(PPE)[1] <- "Article.Title"
PPE = rename(PPE, Journal.Book = Journal.Title, Authors = Author, Year=Publication.Date, URL = PLink)
PPE$Year = substr(PPE$Year,2,5) # select year
PPE$source = 'Psycinfo_Psyarticles_ERIC'
PPE = PPE[, common_cols]

# PsycInfo Measures
psyinf = rename(psyinf, Journal.Book = Journal.Title, Authors = Author, Year=Publication.Date, URL = PLink)
psyinf$Year = substr(psyinf$Year,2,5) # select year
psyinf$source = 'Psycinfo_Measures'
psyinf = psyinf[, common_cols]

# pubmed
pubmed = rename(pubmed, Article.Title = Title, Year=Publication.Year)
pubmed$Doctype = 'Journal Article'
pubmed$source = 'pubmed'
pubmed$URL = NA
pubmed = pubmed[,common_cols]

# ScienceDirect
scidir = rename(scidir, Doctype=Item.type, Article.Title = Title, Journal.Book=Journal, Year=Publication.year, URL = URLs)
scidir$source = 'ScienceDirect'
scidir = scidir[,common_cols]

# Scopus
scopus = rename(scopus, Doctype=Document.Type, Article.Title = Title, Journal.Book=Source.title, URL = Link)
scopus$source = 'Scopus'
scopus = scopus[,common_cols]

# WebofScience
wos = rename(wos, Doctype=`Publication Type`, Article.Title = `Article Title`, Journal.Book=`Source Title`, Year = `Publication Year`)
wos$source = 'WebofScience'
wos$URL = NA
wos = wos[,common_cols]

# Proquest
proquest = rename(proquest, Article.Title=Title, Doctype=ArticleType ,Year=year,Journal.Book=pubtitle, URL = DocumentURL)
proquest$source = 'Proquest'
proquest$DOI = NA
proquest = proquest[,common_cols]

#####################
# combine data
df = rbind(gs, psyinf, PPE, scidir, proquest, wos, pubmed, scopus)
df$Doctype = recode(df$Doctype, Article= 'Journal Article', Review='Journal Article', `Article in Press`='Journal Article',`Scholarly Journals`='Journal Article',J='Journal Article')

# clean title
regex <- paste0("^[", paste(letters, collapse=""), "]+$")
df$Title_cleaned = gsub("[^A-Za-z]", "", df$Article.Title) %>% tolower()

# drop non-english titles
nrow(filter(df, Title_cleaned==''))
df = filter(df, Title_cleaned!='')

# detect exact duplicates
dup_titles = duplicated(df$Title_cleaned) 
dup_dois = duplicated(df$DOI) & !is.na(df$DOI)

dup_titles %>% sum()
dup_dois %>% sum()

df_1 = df[! (dup_titles | dup_dois), ]

# fuzzy duplicate detection
library(tidystringdist)

d = df_1 %>% 
  tidy_comb_all(Title_cleaned) %>% 
  tidy_stringdist(method='lv') %>%
  mutate(n1 = str_length(V1), 
         n2 = str_length(V2), 
         nmax = ifelse(n1 > n2, n1, n2),
         normdist = lv/nmax,
         similarity = 1 - normdist)

hist(d$similarity)

match <- d %>% 
  filter(similarity > .8) %>% # Set a threshold
  gather(x, match, starts_with("V")) %>% 
  filter(x=='V2') %>%
  .$match

df_2 = df_1 %>% 
  filter(!Title_cleaned %in% match)

# save data
write.csv(df_2, 'df_cleaned.csv', row.names = F)

df_2 %>% group_by(source) %>% count()
