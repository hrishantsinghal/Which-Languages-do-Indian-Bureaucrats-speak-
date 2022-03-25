library(tidyverse)
library(data.table) 
library(stringr)
library(tidytext)
library(lubridate)
library(ggplot2)
setwd("C://Users//hrish//Desktop//IAS Project")
dat<-read.csv("ias-profile.txt")

# Cleaning the data of states.
dat1<-dat %>% mutate(v=1) %>% 
  mutate(cadre= str_replace_all(Cadre,"Chhasttisgarh","Chhattisgarh") %>%
           str_replace_all("Maharastra","Maharashtra") %>%
           str_replace_all("A G M U T","AGMUT"),
         Place_of_Domicile=str_replace_all(Place_of_Domicile,"^Assam$","Assam Meghalya") %>% 
           str_replace_all("^Meghalaya$","Assam Meghalya")) %>%
  mutate_all((~na_if(., ''))) 

abc<-dat1 %>% group_by(Cadre) %>% summarise(n())
  
dat1[dat1=="N.A."]=NA


# Leaving out rows of languages which do not have the desired data
dat1<-dat1 %>% drop_na(Languages_Known)
dat1<-dat1 %>% filter(Languages_Known!=""|Languages_Known!="-")

#Using the mother tongue data to find unique elements and then adding 
#foregin languages to the set. 

lang<-dput(unique(dat$Mother_Tongue))
cde<-as.factor(c("Gujarati", "English", "Tamil", "Marathi", "Assamese", "Bengali", 
            "Oriya", "Khasi", "Hindi", "Malayalam", "Kannada", "Punjabi", 
            "Urdu", "Bodo", "Telugu", "Nepalese", "Maithili", "Garo", "Manipuri", 
            "Kashmiri", "Arabic", "Mizo", "Spanish", "Bhojpuri", "Nagamese", 
            "Sindhi", "Sikkimese", "Konkani", "Marwari", "Dogri", "Pahari", 
            "Lepcha", "Tulu", "Rajasthani", "Tenyidie(Angami)", "Nishing", 
            "Tibetan", "Bhutia", "Lambadi", "Adi","French","Sanskrit",
            "Spanish","Persian","Mizo","Japanese","Russian","German",
            "Neapalese","Portugese","Konkani","Persian","Konkani"))

#sorting the languages for comfort in visibility
dat1$Languages_Known<-sort(dat1$Languages_Known)

#combining mother tongue and languages known and finding unique elements
dat1<-dat1 %>% mutate(lang=paste(Languages_Known,Mother_Tongue))
dat2<-dat1 %>% 
  mutate(split = str_split(lang, " ")) %>% # split
  mutate(split = map(.$split, ~ unique(.x))) %>% # drop duplicates
  mutate(split = map_chr(.$split, ~paste(.x, collapse = " ")))

dat1$Languages_Known=dat2$lang

#function for proportion of people who speak the respective language
occ<-function(str1,data_to){
  random<-round(sum(grepl(str1,data_to$Languages_Known))*100/nrow(data_to),1)
  return(as.character(random))
}

def<-as.data.frame(sapply(lang,occ,dat1))

#arranging the langauge data for visibility and doing some other required cleaning
final_dat<-def %>% arrange(lang_known=desc(sapply(lang, occ,dat1)),ascending=FALSE)
final_dat$languages=rownames(final_dat)
names(final_dat)[names(final_dat)=='sapply(lang, occ, dat1)']<-"Percentage"

final_dat$Percentage<-as.numeric(final_dat$Percentage)

#Taking the desired columns and filtering languages spoken by at least 1% people
final_dat <- final_dat %>% select(Percentage,languages)

#Making the desired plot
final_dat %>% filter(languages!="-") %>% 
  ggplot(aes(y=reorder(languages,Percentage),x=Percentage,fill=languages)) +
  geom_bar(stat="identity") + 
  xlab("Percentage of Officers who speak the Language") +
  ylab("Languages") +
  ggtitle("Which Languages do Indian IAS Officers know?") +
  geom_text(aes(label=Percentage),hjust=-0.1) +
  scale_x_continuous(limits = c(0, 101),expand=c(0,0))+
  theme(legend.position = "none")

#English and Hindi spoken by Active officers in 2011
ias_2011<-dat1 %>% filter(Last_End_Date>="2011-01-01")
occ("English",ias_2011)
occ("Hindi",ias_2011)


#function for proportion of home officers who speak the popular language in the selected state
state_local_language<-function(state,language){
  dat123<-dat1 %>%
    filter(cadre==state,Place_of_Domicile==state)
  as.numeric(print(occ(language,dat123)))
  nrow(dat123)
}

state_local_language("West Bengal","Bengali")
state_local_language("Maharashtra","Marathi")
state_local_language("Karnataka","Kannada")
state_local_language("Punjab","Punjabi")
state_local_language("Kerala","Malayalam")
state_local_language("Gujarat","Gujarati")
state_local_language("Tamil Nadu","Tamil")
state_local_language("Andhra Pradesh","Telugu")
state_local_language("Odisha","Oriya")

#function for proportion of outside officers who speak the popular language in the selected state
outside_state_local_language<-function(state,language){
  dat123<-dat1 %>%
    filter(cadre==state,Place_of_Domicile!=state)
  as.numeric(print(occ(language,dat123)))
  nrow(dat123)
}

outside_state_local_language("West Bengal","Bengali")
outside_state_local_language("Maharashtra","Marathi")
outside_state_local_language("Karnataka","Kannada")
outside_state_local_language("Punjab","Punjabi")
outside_state_local_language("Kerala","Malayalam")
outside_state_local_language("Gujarat","Gujarati")
outside_state_local_language("Tamil Nadu","Tamil")
outside_state_local_language("Andhra Pradesh","Telugu")
outside_state_local_language("Odisha","Oriya")




