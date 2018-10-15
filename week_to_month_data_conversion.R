
# Importing the required libraries
library(xts)
library(dplyr)
library(fpp2)
library(tseries)

# Function to convert weekly to monthly for a particular column from a .csv file
w2m <- function(file.name, col.name){
  x <- read.csv(paste0(file.name,".csv"))
  
  # Create an index to convert weeks into calendar dates, and create a week index
  CW <- seq(as.POSIXct("1979-9-3", "GMT"), as.POSIXct("2011-12-19", "GMT"), by = "week")
  cal.ix <- CW[x[,"WEEK"]]
  
  y <- xts(x[,col.name], order.by=cal.ix)
  st.day <- first(cal.ix)
  end.day <- as.POSIXct(as.Date(last(cal.ix))+6)
  yd <- xts(,seq(st.day,end.day,"day"))
  
  yd <- merge(y,yd)
  yd<- na.locf(yd)/7
  
  # Accumulate demand by month
  ym <- apply.monthly(yd,sum)
  ym <- ts(ym, start=2001, frequency=12)
  colnames(ym) <- col.name
  
  return(ym)  
}

# The UPC information was used to obtain the vendor codes corresponding to Unilever
upc = read.csv("UPC information.csv")
upc_unilever = data.frame(filter(upc, L3 == "UNILEVER"))
upc_unilever$VEND = as.factor(upc_unilever$VEND)
Unilever_Codes = levels(upc_unilever$VEND)

# Reading the data by filtering only unilever vendor codes
# Grouping and aggregating the data weekly

dCHD <- read.csv("PB-Drug-CHICAGO-patched.csv")[,-1] %>%  # Read file
  filter(VEND %in% Unilever_Codes) %>% # Filter only the Unilever codes
  mutate(VOL = VOL_EQ * UNITS) %>%  # Calculate volume in pounds for each row
  group_by(WEEK) %>% 
  summarize(DEMAND = sum(VOL),TOT.DOLLARS= sum(DOLLARS)) %>% # Obtaining weekly aggregates
  mutate(PRICE = TOT.DOLLARS/DEMAND) %>%
  select(WEEK, PRICE, DEMAND)

dCHG <- read.csv("PB-Groc-CHICAGO-patched.csv")[,-1] %>%
  filter(VEND %in% Unilever_Codes) %>%
  mutate(VOL = VOL_EQ * UNITS) %>%
  group_by(WEEK) %>%
  summarize(DEMAND = sum(VOL),TOT.DOLLARS= sum(DOLLARS)) %>%
  mutate(PRICE = TOT.DOLLARS/DEMAND) %>%
  select(WEEK, PRICE, DEMAND)

dLAD <- read.csv("PB-Drug-LA-patched.csv")[,-1] %>%
  filter(VEND %in% Unilever_Codes) %>%
  mutate(VOL = VOL_EQ * UNITS) %>%
  group_by(WEEK) %>%
  summarize(DEMAND = sum(VOL),TOT.DOLLARS= sum(DOLLARS)) %>%
  mutate(PRICE = TOT.DOLLARS/DEMAND) %>%
  select(WEEK, PRICE, DEMAND)

dLAG <- read.csv("PB-Groc-LA-patched.csv")[,-1] %>%
  filter(VEND %in% Unilever_Codes) %>%
  mutate(VOL = VOL_EQ * UNITS) %>%
  group_by(WEEK) %>%
  summarize(DEMAND = sum(VOL),TOT.DOLLARS= sum(DOLLARS)) %>%
  mutate(PRICE = TOT.DOLLARS/DEMAND) %>%
  select(WEEK, PRICE, DEMAND)

# Obtaining the aggregated data from the 4 dataframes
dTotal = data.frame("WEEK"=dCHD$WEEK,"Dtotal"=dCHD$DEMAND+dCHG$DEMAND+dLAD$DEMAND+dLAG$DEMAND, "AvgPrice" = (dCHD$PRICE+dCHG$PRICE+dLAD$PRICE+dLAG$PRICE)/4)
write.csv(dTotal,"Demand_Week_Total_Unilever.csv")

# Obtaining the weekly data for LA and CH markets separately
dLA = data.frame("WEEK"=dLAD$WEEK, "Dtotal"=dLAD$DEMAND+dLAG$DEMAND, "AvgPrice" = (dLAD$PRICE+dLAG$PRICE)/2)
dCH = data.frame("WEEK"=dCHD$WEEK, "Dtotal"=dCHD$DEMAND+dCHG$DEMAND, "AvgPrice" = (dCHD$PRICE+dCHG$PRICE)/2)
write.csv(dLA,"Demand_Week_LA_Unilever.csv")
write.csv(dCH,"Demand_Week_CH_Unilever.csv")

# Convert the demand and price from weekly data to monthly data using the function "w2m"
dMonthly <- data.frame(w2m("Demand_Week_Total_Unilever","Dtotal"))
pMonthly <- data.frame(w2m("Demand_Week_Total_Unilever","AvgPrice"))

month_data = data.frame(dMonthly, pMonthly)
month_data = ts(month_data, start=2001, frequency = 12)
write.csv(month_data,"Demand_Month_Total_Unilever.csv")   # Write the variable created as a CSV file









d1 <- read.csv("PB-Drug-CHICAGO-patched.csv")[,-1] %>%  # Read file
  filter(VEND %in% Unilever_Codes) %>%
  mutate(VOL = VOL_EQ * UNITS)

d2 <- read.csv("PB-Groc-CHICAGO-patched.csv")[,-1] %>%  # Read file
  filter(VEND %in% Unilever_Codes) %>%
  mutate(VOL = VOL_EQ * UNITS)

d3 <- read.csv("PB-Drug-LA-patched.csv")[,-1] %>%  # Read file
  filter(VEND %in% Unilever_Codes) %>%
  mutate(VOL = VOL_EQ * UNITS)

d4 <- read.csv("PB-Groc-LA-patched.csv")[,-1] %>%  # Read file
  filter(VEND %in% Unilever_Codes) %>%
  mutate(VOL = VOL_EQ * UNITS)

dTotal = full_join(d1,d2)
dTotal = full_join(dTotal,d3)
dTotal = full_join(dTotal,d4)

#View(dTotal)
library(dummies)

dummy_F = dummy(dTotal$F)
dTotal = cbind(dTotal, dummy_F)
dTotal$F = NULL
#dTotal

dummy_D = dummy(dTotal$D)
dTotal = cbind(dTotal, dummy_D)
dTotal$D = NULL
#dTotal

dummy_PR = dummy(dTotal$PR)
dTotal = cbind(dTotal, dummy_PR)
dTotal$PR = NULL
#dTotal

dTotal[,"FAplus"] = dTotal$`FA+`
dTotal$`FA+` = NULL

dTotal <- dTotal %>%  
  mutate(VOL = VOL_EQ * UNITS)%>%
  mutate(FA = VOL*FA, FAplus = VOL*FAplus, FB = VOL*FB, D1 = VOL*D1, D2 = VOL*D2, PR1 = VOL*PR1) %>%
  group_by(WEEK) %>%
  summarize(DEMAND = sum(VOL),TOT.DOLLARS= sum(DOLLARS), FA = sum(FA), FAplus = sum(FAplus), FB = sum(FB), D1 = sum(D1), D2 = sum(D2), PR1 = sum(PR1))  %>%
  mutate(PRICE = TOT.DOLLARS/DEMAND) %>%
  select(WEEK, DEMAND, PRICE, FA, FAplus, FB, D1, D2, PR1)
dTotal

      #write.csv(dTotal,"Demand-CH-Uni-Week.csv")   # Write the variable created as a CSV file

write.csv(dTotal,"Demand-Total-Uni-Combined.csv")   # Write the variable created as a CSV file

demand <- data.frame(w2m_d("Demand-Total-Uni-Combined","DEMAND")) # Call the function with the file name 
demand
vars <- data.frame(w2m("Demand-Total-Uni-Combined"))
vars

dTotal = data.frame(demand, vars)
dTotal[,c(-1,-2)] = dTotal[,c(-1,-2)]/dTotal$DEMAND
dTotal

write.csv(dTotal,"Demand-Total-Month-Uni-Combined.csv")# Write the variable created as a CSV file




