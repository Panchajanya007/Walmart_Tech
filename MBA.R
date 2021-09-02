########## Loading all necessary packages 


install.packages("arules")
library(arules)
#install and load arulesViz
install.packages("arulesViz")
library(arulesViz)
#install and load tidyverse
install.packages("tidyverse")
library(tidyverse)
#install and load readxml
install.packages("readxml")
library(readxl)
#install and load knitr
#install.packages("knitr")
library(knitr)
#load ggplot2 as it comes in tidyverse
library(ggplot2)
#install and load lubridate
#install.packages("lubridate")
library(lubridate)
#install and load plyr
#install.packages("plyr")
library(plyr)
library(RColorBrewer)
list.packages <- c('foreach','glue','parallel','doParallel','mlutils','MatchIt','dplyr','shinycssloaders')
if(length(list.packages) > 0){
  for(i in 1:length(list.packages))
  {
    print(i)
    package <- list.packages[i]
    if(!(package %in% rownames(installed.packages())))
    {install.packages(package, dependencies = T)}
    library(package = package, character.only = T)
  }
  rm(i, package)
}

packages <- c("dplyr", "dbplyr", "rio", "jsonlite", "DBI", 
              "RMySQL", "stringr", "doParallel", "futile.logger", "odbc","lubridate")
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
}
)
rm(package.check, packages,list.packages)
options(scipen = 999)


Query = paste0("select * FROM ecomm_ops_tables. Item_Details_1  ")
Txns_Items = dataset.load(name="Presto", query= Query)
Txns_Items = Txns_Items %>% arrange(desc(sales_order_grp_num))
# Replace  Comma with extra space
Txns_Items$prod_nm = gsub(","," ",Txns_Items$prod_nm)
Txns_Items$prod_nm = gsub("[^[:alnum:][:blank:]?&/\\-]", "", Txns_Items$prod_nm)

# Remove extra spaces
Txns_Items$prod_nm = gsub("\\s+", " ", str_trim(Txns_Items$prod_nm))

Txn_Item_Table <- ddply(Txns_Items,c("sales_order_grp_num"),
                        function(df1)paste(df1$prod_nm,
                                           collapse = ","))


Txn_Item_Table$sales_order_grp_num = NULL 

colnames(Txn_Item_Table) <- c("Item_Combo")
# index = sample(1:nrow(Txn_Item_Table), 250000)
# Txn_Item_Table_sample = as.data.frame(Txn_Item_Table[index,])
# head(Txn_Item_Table_sample)
## Store it into csv
# rm(Txn_Item_Table_sample)
# rm(index)
rm(Txns_Items)
write.csv(Txn_Item_Table,"Market_Basket_Transactions.csv", quote = FALSE, row.names = FALSE)

# Reading the excel as an object of Trans

tr <- read.transactions('Market_Basket_Transactions.csv', format = 'basket', sep=',')

tr
summary(tr)

# itemFrequencyPlot(tr,topN=5,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")
association_rules <- apriori(tr, parameter = list(supp=0.00001, conf=0.7,maxlen=3))
inspect(association_rules)

subset_rules <- which(colSums(is.subset(association_rules, association_rules)) > 1)
length(subset_rules)
List = seq(1, length(subset_rules), 2)
subset_rules = subset_rules[List]

subset_association_rules <- association_rules[-subset_rules]
inspect(subset_association_rules)

quality(subset_association_rules)<-round(quality(subset_association_rules) ,digits=3)
subset_association_rules_sorted <- sort(subset_association_rules, by="count")
subset_association_rules_sorted_head =   head(subset_association_rules_sorted, n = 100, by = "support")
plot(subset_association_rules_sorted_head)
inspect(subset_association_rules_sorted_head)

plot(subset_association_rules_sorted_head,method="grouped")


# 2 Dimensional Publication
plot(subset_association_rules_sorted_head, method = "graph",  engine = "htmlwidget")
plot(subset_association_rules_sorted_head, method="paracoord")
