library(tidyverse)
library(data.table) 
library(stringr)
setwd("C://Users//hrish//Desktop//IAS Project")
dat<-read.csv("ias-profile.txt")

# Cleaning the data of states.
dat1<-dat %>% mutate(v=1) %>% 
  mutate(cadre= str_replace_all(Cadre,"Chhasttisgarh","Chhattisgarh") %>%
           str_replace_all("Manipur","Manipur Tripura") %>% 
           str_replace_all("Maharastra","Maharashtra") %>%
           str_replace_all("A G M U T","AGMUT")) %>%
  mutate_all((~na_if(., ''))) 
dat1[dat1=="N.A."]=NA

# abcd<-dat %>% mutate(mt=ifelse(dat$Mother_Tongue=="Hindi","Hindi","not"))
# abcd %>% group_by(mt) %>% 
#   summarise(sum1=sum(v)) %>% 
#   ggplot(aes(y=mt,x=sum1)) +
#   geom_bar(stat="identity") +
#   ylab("Languages") +
#   xlab("Number of People") +
#   ggtitle("What is the mother tongue of IAS Officers?") +
#   geom_text(aes(label=sum1),hjust=-0.1)

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
occ<-function(str1){
  random<-round(sum(grepl(str1,dat1$Languages_Known))*100/nrow(dat1),1)
  return(as.character(random))
}

def<-as.data.frame(sapply(lang,occ))

#arranging the langauge data for visibility and doing some other required cleaning
final_dat<-def %>% arrange(lang_known=desc(sapply(lang, occ)),ascending=FALSE)
final_dat$languages=rownames(final_dat)
names(final_dat)[names(final_dat)=='sapply(lang, occ)']<-"Percentage"

final_dat$Percentage<-as.numeric(final_dat$Percentage)

#Taking the desired columns and filtering languages spoken by at least 1% people
final_dat <- final_dat %>% filter(Percentage>=1) %>% 
  select(Percentage,languages)

#Making the desired plot
final_dat %>%
  ggplot(aes(y=reorder(languages,Percentage),x=Percentage,fill=languages)) +
  geom_bar(stat="identity") + 
  xlab("Percentage of Officers who speak the Language") +
  ylab("Languages") +
  ggtitle("Which Languages do Indian IAS Officers know?") +
  geom_text(aes(label=Percentage),hjust=-0.1) +
  scale_x_continuous(limits = c(0, 101),expand=c(0,0))+
  theme(legend.position = "none")















