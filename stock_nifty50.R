#read closing price for 50 stocks
stock<-read.csv(file.choose(),header = TRUE)
#remove index column
Nifty<-stock[,-1]
Nifty
# basic stats for all 50 closing price
summary(Nifty)
#installing package to calculate skewness and kurtosis
install.packages("moments")
library(moments)
var(Nifty)
skewness(Nifty,na.rm = TRUE)
kurtosis(Nifty,na.rm = TRUE)

#plotting density function for  closing prices of all 50 companies
title_vec<-colnames(return_df)
par(mfrow=c(4,13))
par(mar=c(3,3,1.5,1.5))    

{plot(density(Nifty$ACC))
plot(density(Nifty$ADANIPORTS))
plot(density(Nifty$AMBUJACEM))
plot(density(Nifty$ASIANPAINT))
plot(density(Nifty$AUROPHARMA))
plot(density(Nifty$AXISBANK))
plot(density(Nifty$BAJAJ.AUTO))
plot(density(Nifty$BANKBARODA))
plot(density(Nifty$BPCL))
plot(density(Nifty$BHARTIARTL))
plot(density(Nifty$INFRATEL))
plot(density(Nifty$BOSCHLTD))
plot(density(Nifty$CIPLA))
plot(density(Nifty$COALINDIA))
plot(density(Nifty$DRREDDY))
plot(density(Nifty$EICHERMOT))
plot(density(Nifty$GAIL))
plot(density(Nifty$HCLTECH))
plot(density(Nifty$HDFCBANK))
plot(density(Nifty$HEROMOTOCO))
plot(density(Nifty$HINDALCO))
plot(density(Nifty$HINDUNILVR))
plot(density(Nifty$HDFC))
plot(density(Nifty$ITC))
plot(density(Nifty$ICICIBANK))
plot(density(Nifty$IBULHSGFIN,na.rm = TRUE))
plot(density(Nifty$IOC))
plot(density(Nifty$INDUSINDBK))
plot(density(Nifty$INFY))
plot(density(Nifty$KOTAKBANK))
plot(density(Nifty$LT))
plot(density(Nifty$LUPIN))
plot(density(Nifty$M.M))
plot(density(Nifty$MARUTI))
plot(density(Nifty$NTPC))
plot(density(Nifty$ONGC))
plot(density(Nifty$POWERGRID))
plot(density(Nifty$RELIANCE))
plot(density(Nifty$SBIN))
plot(density(Nifty$SUNPHARMA))
plot(density(Nifty$TCS))
plot(density(Nifty$TATAMTRDVR))
plot(density(Nifty$TATAMOTORS))
plot(density(Nifty$TATAPOWER))
plot(density(Nifty$TATASTEEL))
plot(density(Nifty$TECHM))
plot(density(Nifty$ULTRACEMCO))
plot(density(Nifty$VEDL))
plot(density(Nifty$WIPRO))
plot(density(Nifty$YESBANK))
plot(density(Nifty$ZEEL))}


#creating data frame for all closing prices  
Nifty_final<-data.frame(Nifty$ACC,Nifty$ADANIPORTS,Nifty$AMBUJACEM,Nifty$ASIANPAINT,Nifty$AUROPHARMA,Nifty$AXISBANK,Nifty$BAJAJ.AUTO,Nifty$BANKBARODA,Nifty$BPCL,Nifty$BHARTIARTL,Nifty$INFRATEL,Nifty$BOSCHLTD,Nifty$CIPLA,Nifty$COALINDIA,Nifty$DRREDDY,Nifty$EICHERMOT,Nifty$GAIL,Nifty$HCLTECH,Nifty$HDFCBANK,Nifty$HEROMOTOCO,Nifty$HINDALCO,Nifty$HINDUNILVR,Nifty$HDFC,Nifty$ITC,Nifty$ICICIBANK,Nifty$IBULHSGFIN,Nifty$IOC,Nifty$INDUSINDBK,Nifty$INFY,Nifty$KOTAKBANK,Nifty$LT,Nifty$LUPIN,Nifty$M.M,Nifty$MARUTI,Nifty$NTPC,Nifty$ONGC,Nifty$POWERGRID,Nifty$RELIANCE,Nifty$SBIN,Nifty$SUNPHARMA,Nifty$TCS,Nifty$TATAMTRDVR,Nifty$TATAMOTORS,Nifty$TATASTEEL,Nifty$TECHM,Nifty$ULTRACEMCO,Nifty$VEDL,Nifty$WIPRO,Nifty$YESBANK,Nifty$ZEEL)

#check log values for all closing prices
log_values<-log(Nifty_final)


# plotting log prices
plot(density(log_values$Nifty.ACC))

#calculating returns for all closing prices

return_df<-matrix(nrow=nrow(Nifty_final)-1, ncol=ncol(Nifty_final))
for(i in 1:ncol(Nifty_final)){
  for(j in 2:nrow(Nifty_final)){
    return_df[j-1,i]<-round((Nifty_final[j,i]-Nifty_final[(j-1),i])/Nifty_final[j-1,i]*100,2)
  }
}
head(return_df)
View(return_df)
colnames(return_df)<-colnames(Nifty_final)
return_df<-as.data.frame(return_df)
View(return_df)



#calculating log returns for all closing prices

log_returns<-matrix(nrow=nrow(log_values)-1, ncol=ncol(log_values))
for(i in 1:ncol(log_values)){
  for(j in 2:nrow(log_values)){
    log_returns[j-1,i]<-log_values[j,i]-log_values[j-1,i]
  }
}
colnames(log_returns)<-colnames(log_values)
log_returns<-as.data.frame(log_returns)
log_returns

###plotting  returns

title_vec<-colnames(return_df)
par(mfrow=c(4,13))
par(mar=c(3,3,1.5,1.5))    

{plot(density(return_df$Nifty.ACC))
  plot(density(return_df$Nifty.ADANIPORTS))
  plot(density(return_df$Nifty.AMBUJACEM))
  plot(density(return_df$Nifty.ASIANPAINT))
  plot(density(return_df$Nifty.AUROPHARMA))
  plot(density(return_df$Nifty.AXISBANK))
  plot(density(return_df$Nifty.BAJAJ.AUTO))
  plot(density(return_df$Nifty.BANKBARODA))
  plot(density(return_df$Nifty.BPCL))
  plot(density(return_df$Nifty.BHARTIARTL))
  plot(density(return_df$Nifty.INFRATEL))
  plot(density(return_df$Nifty.BOSCHLTD))
  plot(density(return_df$Nifty.CIPLA))
  plot(density(return_df$Nifty.COALINDIA))
  plot(density(return_df$Nifty.DRREDDY))
  plot(density(return_df$Nifty.EICHERMOT))
  plot(density(return_df$Nifty.GAIL))
  plot(density(return_df$Nifty.HCLTECH))
  plot(density(return_df$Nifty.HDFCBANK))
  plot(density(return_df$Nifty.HEROMOTOCO))
  plot(density(return_df$Nifty.HINDALCO))
  plot(density(return_df$Nifty.HINDUNILVR))
  plot(density(return_df$Nifty.HDFC))
  plot(density(return_df$Nifty.ITC))
  plot(density(return_df$Nifty.ICICIBANK))
  #plot(density(return_df$Nifty.IBULHSGFIN))
  plot(density(return_df$Nifty.IOC))
  plot(density(return_df$Nifty.INDUSINDBK))
  plot(density(return_df$Nifty.INFY))
  plot(density(return_df$Nifty.KOTAKBANK))
  plot(density(return_df$Nifty.LT))
  plot(density(return_df$Nifty.LUPIN))
  plot(density(return_df$Nifty.M.M))
  plot(density(return_df$Nifty.MARUTI))
  plot(density(return_df$Nifty.NTPC))
  plot(density(return_df$Nifty.ONGC))
  plot(density(return_df$Nifty.POWERGRID))
  plot(density(return_df$Nifty.RELIANCE))
  plot(density(return_df$Nifty.SBIN))
  plot(density(return_df$Nifty.SUNPHARMA))
  plot(density(return_df$Nifty.TCS))
  plot(density(return_df$Nifty.TATAMTRDVR))
  plot(density(return_df$Nifty.TATAMOTORS))
  plot(density(return_df$Nifty.TATASTEEL))
  plot(density(return_df$Nifty.TECHM))
  plot(density(return_df$Nifty.ULTRACEMCO))
  plot(density(return_df$Nifty.VEDL))
  plot(density(return_df$Nifty.WIPRO))
  plot(density(return_df$Nifty.YESBANK))
  plot(density(return_df$Nifty.ZEEL))
  }




#installing package to calculate mean and std dev
install.packages("fitdistrplus")
library(fitdistrplus)

f <- fitdist(return_df$Nifty.ADANIPORTS, "lnorm")
summary(f)



##########mean returns of returns###########
mean_returns<-round(colMeans(return_df),2)
mean_returns



#calculating correlation for all stocks closing prices
par(mfrow=c(1,1))
install.packages("corrplot")
library(corrplot)
cor<- cor(Nifty_final)
corrplot(cor,method = 'circle',type = 'upper')
corrplot(cor,method = 'circle')


