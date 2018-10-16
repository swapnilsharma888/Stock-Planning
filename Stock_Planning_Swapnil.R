library(readxl)
library(dplyr)
library(lubridate)
loc_e<- read_excel("C:/Users/SWAPNIL/Desktop/Stock Planning/E.xlsx")
loc_e<-as.data.frame(loc_e)
unique(loc_e$Code)
class(loc_e)
head(loc_e)
table(loc_e$Code)
class(loc_e$Month)


total_qty<-aggregate(loc_e$Qty,by=list(loc_e$Code),sum)
names(total_qty)<-c("Code","total_qty")
total_spend<-aggregate(loc_e$`Spend in USD`,by=list(loc_e$Code),sum)
names(total_spend)<-c("Code","total_spend")
total_count<-table(loc_e$Code)
total_count<-as.data.frame(total_count)
names(total_count)<-c("Code","total_count")
last_order<-aggregate(loc_e$Month,by=list(loc_e$Code),max)
last_order<-as.data.frame(last_order)
last_order$x<-as_date(last_order$x)
names(last_order)<-c("Code","last_order")


total_E<-cbind(total_qty,total_count,total_spend,last_order)
total_E<-total_E[,-c(3,5,7)]
summary(total_E)
unused_spareE<-total_E[total_E$total_qty<=0,]      # spare part which are not at all used ( 221 )
total_E<-total_E[total_E$total_qty>0,]   # Removed all the -ve qty
total_E<-total_E[total_E$total_spend>0,] # Remove with -ve spend
total_E$price_unit<-total_E$total_spend/total_E$total_qty
total_E$price_unit<-round(total_E$price_unit,0)
total_E$spare_onetimeorder<-total_E$total_qty/total_E$total_count
total_E$spare_onetimeorder<-round(total_E$spare_onetimeorder,0)

total_E$count_per_month<-total_E$total_count/57
total_E$order_after_months<-1/total_E$count_per_month
total_E$order_after_months<-round(total_E$order_after_months,0)


total_E1<-total_E
total_E1$execution_time<-1
total_E1$execution_time[1:5932]<-0.5     # 15 days
total_E1$execution_time[5933:11865]<-1   # 30 days
table(total_E1$execution_time)


quantile(total_E1$price_unit,p=(1:10)/10)

# 40 % spare have less than $12. So in order to reduce risk we can order double the stock.
total_E1$spare_onetimeorder_new<-ifelse(total_E1$price_unit<=12,(2*total_E1$spare_onetimeorder),total_E1$spare_onetimeorder)


# We assume that "LOC_E Plant" is nearby to "LOC_H Plant". So they can excange some costly common spare parts
# among themselves



loc_h<- read_excel("C:/Users/SWAPNIL/Desktop/Stock Planning/H.xlsx")
loc_h<-as.data.frame(loc_h)
length(unique(loc_h$Code))
class(loc_h)
head(loc_h)



total_qty<-aggregate(loc_h$Qty,by=list(loc_h$Code),sum)
names(total_qty)<-c("Code","total_qty")
total_spend<-aggregate(loc_h$`Spend in USD`,by=list(loc_h$Code),sum)
names(total_spend)<-c("Code","total_spend")
total_count<-table(loc_h$Code)
total_count<-as.data.frame(total_count)
names(total_count)<-c("Code","total_count")
last_order<-aggregate(loc_h$Month,by=list(loc_h$Code),max)
last_order<-as.data.frame(last_order)
last_order$x<-as_date(last_order$x)
names(last_order)<-c("Code","last_order")



total_H<-cbind(total_qty,total_count,total_spend,last_order)
total_H<-total_H[,-c(3,5,7)]
summary(total_H)
unused_spareH<-total_H[total_H$total_qty<=0,]      # spare part which are not at all used ( 609 NOS )
total_H<-total_H[total_H$total_qty>0,]   # Removed all the -ve qty
total_H<-total_H[total_H$total_spend>0,] # Remove with -ve spend
total_H$price_unit<-total_H$total_spend/total_H$total_qty
total_H$price_unit<-round(total_H$price_unit,0)
total_H$spare_onetimeorder<-total_H$total_qty/total_H$total_count
total_H$spare_onetimeorder<-round(total_H$spare_onetimeorder,0)

total_H$count_per_month<-total_H$total_count/57
total_H$order_after_months<-1/total_H$count_per_month
total_H$order_after_months<-round(total_H$order_after_months,0)


total_H1<-total_H
total_H1$execution_time<-1
total_H1$execution_time[1:5942]<-0.5      # 15 days
total_H1$execution_time[5943:11885]<-1    # 30 days
table(total_H1$execution_time)


quantile(total_H1$price_unit,p=(1:10)/10)

# 40 % spare have less than $12. So in order to reduce risk we can order double the stock.
total_H1$spare_onetimeorder_new<-ifelse(total_H1$price_unit<=12,(2*total_H1$spare_onetimeorder),total_H1$spare_onetimeorder)

# Plant LOC_E & Plant LOC_H are nearby so they can share their spare parts in emergency. 

names(total_E1)<-c("Code","total_qty_E","total_count_E","total_spend_E","last_order_E","price_unit_E","spare_onetimeorder_E",
                   "count_per_month_E","order_after_months_E","execution_month_E","spare_onetimeorder_new_E")
names(total_H1)<-c("Code","total_qty_H","total_count_H","total_spend_H","last_order_H","price_unit_H","spare_onetimeorder_H",
                   "count_per_month_H","order_after_months_H","execution_month_H","spare_onetimeorder_new_H")

common_sparepart<-merge(total_E1,total_H1,by="Code")

quantile(common_sparepart$price_unit_E,p=(1:10)/10)

common_sparepart$max_qty_consump<-ifelse(common_sparepart$total_qty_E>=common_sparepart$total_qty_H,
                                         common_sparepart$total_qty_E,common_sparepart$total_qty_H)
  
quantile(common_sparepart$max_qty_consump,p=(1:10)/10)


# if price per unit > $243 and no of units <20 then we can share Spare parts between Plant E and Plant H

names(common_sparepart)
common_SparePart<-common_sparepart[common_sparepart$price_unit_E>243 & common_sparepart$max_qty_consump<20,] # 182 spare part


write.csv(total_E1,"total_E1.csv")
write.csv(total_H1,"total_H1.csv")
write.csv(common_SparePart,"common_SparePart.csv")
