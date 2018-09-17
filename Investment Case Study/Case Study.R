##############Checkpoint 1: Data Cleaning 1############

url = "https://cdn.upgrad.com/UpGrad/temp/d934844e-5182-4b58-b896-4ba2a499aa57/companies.txt"
download.file(url, destfile = "companies.txt")

companies <- read.delim("companies.txt", stringsAsFactors = FALSE)
rounds2 <- read.csv("rounds2.csv", stringsAsFactors = FALSE)
str(companies)
str(rounds2)
##########################

###########Table 1.1:Understand the Data Set############################

#1.#
#Changing to lower case to compare 
library(tidyr)
library(dplyr)
library(sqldf)
companies <- mutate_all(companies,funs(tolower))
str(companies)
rounds2 <- mutate_all(rounds2,funs(tolower))
str(rounds2)

unique_companies_in_rounds2 <- length(unique(rounds2$company_permalink))
sqldf("select count(distinct(company_permalink)) from rounds2;")


#2.

unique_companies_in_companies <- length(unique(companies$permalink))
sqldf("select count(distinct(permalink)) from companies;")

#3.permalink #

#4.

master_frame <- merge(companies,rounds2, by.x ='permalink', by.y='company_permalink')
companies$permalink[! companies$permalink %in% rounds2$company_permalink]
setdiff(companies$permalink,rounds2$company_permalink)

#5. 114949 #

###############Checkpoint 2: Funding Type Analysis#####################

##############Table 2.1##########################
#Transforming raised_amount_usd column to numeric
master_frame$raised_amount_usd<-as.numeric(master_frame$raised_amount_usd)


#Method1:
avg_venture <- mean(master_frame$raised_amount_usd[master_frame$funding_round_type=="venture"], na.rm=TRUE)
avg_angel <- mean(master_frame$raised_amount_usd[master_frame$funding_round_type=="angel"], na.rm=TRUE)
avg_seed <- mean(master_frame$raised_amount_usd[master_frame$funding_round_type=="seed"], na.rm=TRUE)
avg_priv_equity <-mean(master_frame$raised_amount_usd[master_frame$funding_round_type=="private_equity"], na.rm=TRUE)

#Method2:

sqldf("select funding_round_type, avg(raised_amount_usd) from master_frame where funding_round_type='venture' group by funding_round_type;")
sqldf("select funding_round_type, avg(raised_amount_usd) from master_frame where funding_round_type='angel' group by funding_round_type;")
sqldf("select funding_round_type, avg(raised_amount_usd) from master_frame where funding_round_type='seed' group by funding_round_type;")
sqldf("select funding_round_type, avg(raised_amount_usd) from master_frame where funding_round_type='private_equity' group by funding_round_type;")

##############Checkpoint 3: Country Analysis##################

##############Table 3.1##########################


#Method1:

venture_subset<-subset(master_frame,master_frame$funding_round_type=="venture")
top_list <- aggregate(venture_subset$raised_amount_usd, by=list(venture_subset$country_code), FUN=sum, na.rm=TRUE,na.omit=TRUE)
colnames(top_list)<-c("country_code","raised_amount_usd")
top_list_arranged<-arrange(top_list,desc(raised_amount_usd))
top_list_final<-filter(top_list_arranged,country_code!="")

TOP9 <-head(top_list_final,9)

#Method2:
venture_1<-filter(master_frame,funding_round_type == "venture")
top_country_list <- group_by(venture_1, country_code)
top_country_list1<-summarise(top_country_list,sum(raised_amount_usd, na.rm=TRUE,na.omit=TRUE))
names(top_country_list1)[2] <-"total_sum"
t3<-arrange(top_country_list1,desc(total_sum))
t3_final<-filter(t3,t3$country_code != "")

TOP_9_METHOD_2<-head(t3_final,9)

#Method3:

top_countries <- sqldf("select country_code, sum(raised_amount_usd) r from master_frame where funding_round_type='venture' group by country_code order by r desc;")
top_countries_final<-filter(top_countries,country_code != "")
TOP_9_METHOD_3 <-head(top_countries_final,9)

#####################Checkpoint 4: Sector Analysis 1#######################
#1.

master_frame <- separate(master_frame, category_list, into=c("Primary_Sector", "Sub_sector1","sub_sector2"), sep ="[|]"
, remove=FALSE, extra="merge")

#2.
library(stringr)
mapping <- read.csv("mapping.csv", stringsAsFactors = FALSE)
mapping <- mutate_all(mapping,funs(tolower))
mapping$Blanks<-NULL
mapping<-mapping[-1,]
mapping_new<- mapping %>% mutate(category_list=str_replace(category_list,"0","na"))
mapping_new <- gather(mapping_new, "Main_Sector", my_val, 2:9, na.rm=TRUE)
mapping_new <- mapping_new[!(mapping_new$my_val == 0),]
mapping_new <- mapping_new[, -3]


#####################Checkpoint 5: Sector Analysis 2#######################

merged_master_df <- merge(master_frame,mapping_new, by.x="Primary_Sector", by.y="category_list",all.x=TRUE)


D1 <- filter(merged_master_df,funding_round_type=="venture" & country_code=="usa" & raised_amount_usd>=5000000 & raised_amount_usd<=15000000) 
D2 <- filter(merged_master_df,funding_round_type=="venture" & country_code=="gbr" & raised_amount_usd>=5000000 & raised_amount_usd<=15000000) 
D3 <- filter(merged_master_df,funding_round_type=="venture" & country_code=="ind" & raised_amount_usd>=5000000 & raised_amount_usd<=15000000) 

D1_count <- D1 %>%  group_by(Main_Sector) %>%  summarise(total.count=n(), total_amount=sum(raised_amount_usd))
D2_count <- D2 %>%  group_by(Main_Sector) %>%  summarise(total.count=n(), total_amount=sum(raised_amount_usd))
D3_count <- D3 %>%  group_by(Main_Sector) %>%  summarise(total.count=n(), total_amount=sum(raised_amount_usd))
            
D1 <- merge(D1,D1_count,by="Main_Sector", all.x=TRUE)
D2 <- merge(D2,D2_count,by="Main_Sector", all.x=TRUE)
D3 <- merge(D3,D3_count,by="Main_Sector", all.x=TRUE)
