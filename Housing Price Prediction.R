
#"Price of a property is one of the most important decision criterion when people buy homes.
#Real state firms need to be consistent in their pricing in order to attract buyers .
#Having a predictive model for the same will be great tool to have ,
#which in turn can also be used to tweak development of properties ,
#putting more emphasis on qualities which increase the value of the property."


#...............................................................................................................

#Two datasets housing_train.csv and housing_test.csv are given.
#Need to use data housing_train to build predictive model for response variable "Price".
#Housing_test data contains all other factors except "Price", 



#..................................................CODE.............................................................

h_train=read.csv('housing_train.csv', stringsAsFactors = F)

h_test=read.csv('housing_test.csv', stringsAsFactors = F)




library('dplyr')


setdiff(names(h_train),names(h_test))
#Price

#Creating an extra column in h_test(with values=NA) just combine both train and test file for preprocessing.
h_test$Price=NA

#Also creating an identity column for train & test to serperate them out later on after preprocessing.
h_train$data='train'
h_test$data='test'

#lets combine to datasets for preproccessing
h_t=rbind(h_train,h_test)





glimpse(h_t)

#get the column names and storing it, to find na's in each column
cols <- colnames(h_t)
sapply(h_t[,cols],function(x)sum(is.na(x)))



colsubs <- unique(h_t$Suburb)

#This the function to create dummies.
CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var]) ## getting the table for the variable(any categorical variable)
  t=t[t>freq_cutoff] ## cutoff is the frequency of occurance of a variable default is 0 , but ideally it should be atleast 15-20% of actual data,
  ## so here whatever categories which are less than that cut off frequencies are dropped(no dummes are created for them)
  t=sort(t) ## sort the data
  categories=names(t)[-1] ## pick everything but exclude the first as it has lowest frequency: REmember its n-1
  
  for( cat in categories){
    name=paste(var,cat,sep="_") ## Inside the for loop create a name separated by name of variable and category separeted by "_" underscore
    name=gsub(" ","",name) ## replace any spaces if there is found in categoreis of variables
    name=gsub("-","_",name) ## replace any dash if found in categories to underscropes: e.g. 'Cat-1', 'Cat-2' will be 'Cat_1', 'Cat_2'
    name=gsub("\\?","Q",name) ## any question mark is converted to 'Q'
    name=gsub("<","LT_",name) ## Less than sign is converted to LT_
    name=gsub("\\+","",name) ## + sign is removed
    
    data[,name]=as.numeric(data[,var]==cat) ## finally converting everyting to numeric
  }
  
  data[,var]=NULL
  return(data)
}




subnames <- table(h_train$Suburb)

#Creating dummies of column Suburb.
h_t=CreateDummies(h_t,'Suburb',20)



#cor.test(h_t$Rooms,h_t$Bedroom2,na.rm=T)
#round(prop.table(sort(table(h_t$Distance)))*100)

#h_t=CreateDummies(h_t,'Distance',50)



glimpse(h_t)



table(h_t$Method)
h_t=CreateDummies(h_t,'Method',10)



#table(h_t$Type)


h_t=CreateDummies(h_t,'Type',10)
table(h_t$Address)



glimpse(h_t)



table((h_t$SellerG))
h_t=CreateDummies(h_t,'SellerG',50)


table(h_t$Rooms)
h_t=CreateDummies(h_t,'Rooms',30)



glimpse(h_t)



table(h_t$CouncilArea)
#Placing NA's where there is empty str in CounsilArea column.
h_t$CouncilArea[h_t$CouncilArea==""] <- 'NA'

sort(table(h_t[h_t$data=='train','CouncilArea']))



#Replacing NA's with most frequently Occuring Value.
h_t$CouncilArea[h_t$CouncilArea=="NA"] <- 'Boroondara'
table(h_t$CouncilArea)
h_t=CreateDummies(h_t,'CouncilArea',200)




#removing list important columns.
h_t=h_t %>% select(-Distance,-YearBuilt,-Landsize,
                   -Bathroom,-Bedroom2,-BuildingArea)



glimpse(h_t)


table(h_t[h_t$data=='train','Car'])
sum(is.na(h_t[,'Car']))
#Replacing NA's with 1 in Car column.
h_t[is.na(h_t[,'Car']),'Car'] <- 1
glimpse(h_t)
table(h_t$Car)
h_t=CreateDummies(h_t,'Car',50)



glimpse(h_t)

#cols <- colnames(h_t)
#h_t[any(sapply(h_t[,cols],function(x)sum(is.na(x))))]


#Seperating combined data to build model on train data.

ht_train=h_t %>% filter(data=='train') %>% select(-data)
ht_test=h_t %>% filter(data=='test') %>% select(-data,-Price)


set.seed(2)
s=sample(1:nrow(ht_train),0.7*nrow(ht_train))
ht_train1=ht_train[s,]
ht_train2=ht_train[-s,]#30% data is to validate the model is not overfiting the trainig data.



#Function for formula(where the independent variables are use to predict dependent varible)
formula_function <- function(x, non_required_ivs, dv) {
  vector_ivs <- setdiff(names(x), c(non_required_ivs, dv))
  ## finding the vecotor for which we want to make formula(only containing independent variables)
  ## as.formula to convert the string to a formula containing plus and tilde sign
  ## first paste0 will concatenate the 'dv' to independent variables separated by plus sign
  ## the last paste0 will concatenate all the independent variables separated by plus
  as.formula(paste0(dv , ' ~ ', paste0(vector_ivs, collapse = ' + ')))
  
}





formula <- formula_function(ht_train1,c('Address'),'Price')

#applying linear REGRESSION as the prediction is continuous value.

fit=lm(formula,data=ht_train1)



#Checking variation influence factor and sorting it in decreasing order to remove high VIF predictors.

library(car)
sort(vif(fit),decreasing = T)




x <-  1000
object_floating <- 'Price'


while(x > 2){
  fit=lm(formula,data=ht_train1) ## Every loop will create a new fit
  sorted <- sort(vif(fit),decreasing = T) ## sort the vif value of fit , pick the first one
  x <- sorted[1] ## pick the first one
  print(names(x)) ## print on console 
  object_floating <-c(names(x), object_floating) ## update the object_floating whenever there is any variable having vif greater than 2
  formula <- formula_function(ht_train1,c('Address', 'Price', object_floating),'Price') 
  ## update the formula by removing the variables which has vif greater than 2
}




fit <- lm(formula, data= ht_train1)


summary(fit)


fit=step(fit)


#to save the trained model till here 
saveRDS(file='fitproj1.RDS', fit)


summary(fit)


formula(fit)
fit=lm(Price ~ Suburb_Gowanbrae + 
         Suburb_Yallambie + Suburb_Albion + Suburb_CaulfieldNorth + 
         Suburb_Parkville + Suburb_Alphington + Suburb_Hughesdale + 
         Suburb_CarltonNorth + Suburb_Viewbank + 
         Suburb_MontAlbert + Suburb_Watsonia + Suburb_Fairfield + 
         Suburb_CaulfieldSouth + Suburb_Fitzroy + Suburb_Braybrook + 
         Suburb_Carlton + Suburb_Canterbury + Suburb_CliftonHill + 
          Suburb_KewEast + Suburb_AlbertPark + 
         Suburb_BoxHill + Suburb_Windsor + Suburb_Elsternwick + Suburb_Collingwood + 
         Suburb_OakPark +Suburb_Hadfield + Suburb_Abbotsford + 
         Suburb_HeidelbergWest + Suburb_NorthMelbourne + Suburb_CoburgNorth + 
         Suburb_Murrumbeena + Suburb_HeidelbergHeights + Suburb_Malvern + 
         Suburb_SouthMelbourne + Suburb_Ashburton + Suburb_BrunswickEast + 
         Suburb_Maidstone + Suburb_AirportWest + Suburb_FitzroyNorth + 
         Suburb_Ormond + Suburb_SunshineNorth + Suburb_WestFootscray + 
         Suburb_AvondaleHeights + Suburb_Fawkner + Suburb_AltonaNorth + 
         Suburb_Armadale +Suburb_Williamstown + 
         Suburb_Melbourne + Suburb_SunshineWest + Suburb_Ivanhoe + 
         Suburb_TemplestoweLower + Suburb_KeilorEast + Suburb_HawthornEast + 
         Suburb_Prahran + Suburb_SurreyHills + Suburb_Kensington + 
         Suburb_Sunshine + Suburb_Toorak + Suburb_Elwood + Suburb_Maribyrnong + 
         Suburb_AscotVale + Suburb_MooneePonds + 
         Suburb_Thornbury + Suburb_Hampton + Suburb_Balwyn + Suburb_MalvernEast + 
         Suburb_Camberwell + Suburb_Carnegie + Suburb_PortMelbourne + 
         Suburb_Bentleigh + Suburb_PascoeVale + Suburb_BrightonEast + 
         Suburb_Hawthorn + Suburb_BalwynNorth + Suburb_Northcote + 
         Suburb_Kew + Suburb_Brighton + Suburb_Glenroy + Suburb_GlenIris + 
         Suburb_Essendon + Suburb_Brunswick + Suburb_SouthYarra + 
         Suburb_StKilda + Suburb_Preston + Suburb_Richmond + Suburb_BentleighEast + 
         Method_VB + Method_SP + Method_PI + Type_h + SellerG_Kay + 
          SellerG_Miles + SellerG_Greg + SellerG_RT + 
         SellerG_Ray + SellerG_Marshall + SellerG_hockingstuart + 
         SellerG_Jellis + Rooms_5 + Rooms_1 + Rooms_4 + Rooms_2 + 
         CouncilArea_Boroondara + Car_3 + Car_2,data = ht_train1)



summary(fit)


#to validate on data.
val.pred=predict(fit,newdata=ht_train2)

#Calculating error on validation data &  the aim to reduce RMSE .
errors=ht_train2$Price-round(val.pred)



errors**2 %>% mean() %>% sqrt()
# 212467/411632.2
# 0.5153484rmse




fit.final=fit=lm(Price ~ Suburb_Gowanbrae + 
                   Suburb_Yallambie + Suburb_Albion + Suburb_CaulfieldNorth + 
                   Suburb_Parkville + Suburb_Alphington + Suburb_Hughesdale + 
                   Suburb_CarltonNorth + Suburb_Viewbank + 
                   Suburb_MontAlbert + Suburb_Watsonia + Suburb_Fairfield + 
                   Suburb_CaulfieldSouth + Suburb_Fitzroy + Suburb_Braybrook + 
                   Suburb_Carlton + Suburb_Canterbury + Suburb_CliftonHill + 
                   Suburb_KewEast + Suburb_AlbertPark + 
                   Suburb_BoxHill + Suburb_Windsor + Suburb_Elsternwick + Suburb_Collingwood + 
                   Suburb_OakPark +Suburb_Hadfield + Suburb_Abbotsford + 
                   Suburb_HeidelbergWest + Suburb_NorthMelbourne + Suburb_CoburgNorth + 
                   Suburb_Murrumbeena + Suburb_HeidelbergHeights + Suburb_Malvern + 
                   Suburb_SouthMelbourne + Suburb_Ashburton + Suburb_BrunswickEast + 
                   Suburb_Maidstone + Suburb_AirportWest + Suburb_FitzroyNorth + 
                   Suburb_Ormond + Suburb_SunshineNorth + Suburb_WestFootscray + 
                   Suburb_AvondaleHeights + Suburb_Fawkner + Suburb_AltonaNorth + 
                   Suburb_Armadale +Suburb_Williamstown + 
                   Suburb_Melbourne + Suburb_SunshineWest + Suburb_Ivanhoe + 
                   Suburb_TemplestoweLower + Suburb_KeilorEast + Suburb_HawthornEast + 
                   Suburb_Prahran + Suburb_SurreyHills + Suburb_Kensington + 
                   Suburb_Sunshine + Suburb_Toorak + Suburb_Elwood + Suburb_Maribyrnong + 
                   Suburb_AscotVale + Suburb_MooneePonds + 
                   Suburb_Thornbury + Suburb_Hampton + Suburb_Balwyn + Suburb_MalvernEast + 
                   Suburb_Camberwell + Suburb_Carnegie + Suburb_PortMelbourne + 
                   Suburb_Bentleigh + Suburb_PascoeVale + Suburb_BrightonEast + 
                   Suburb_Hawthorn + Suburb_BalwynNorth + Suburb_Northcote + 
                   Suburb_Kew + Suburb_Brighton + Suburb_Glenroy + Suburb_GlenIris + 
                   Suburb_Essendon + Suburb_Brunswick + Suburb_SouthYarra + 
                   Suburb_StKilda + Suburb_Preston + Suburb_Richmond + Suburb_BentleighEast + 
                   Method_VB + Method_SP + Method_PI + Type_h + SellerG_Kay + 
                   SellerG_Miles + SellerG_Greg + SellerG_RT + 
                   SellerG_Ray + SellerG_Marshall + SellerG_hockingstuart + 
                   SellerG_Jellis + Rooms_5 + Rooms_1 + Rooms_4 + Rooms_2 + 
                   CouncilArea_Boroondara + Car_3 + Car_2,data = ht_train)


#Using stepwise to get best model........


#The algorithm adds predictors to
#the stepwise model based on the entering values and excludes predictor from the stepwise model 
#if it does not satisfy the excluding threshold.


fit.final=step(fit.final)


summary(fit.final)


test.pred=predict(fit.final,newdata=ht_test)

write.csv(test.pred,"submisionproj1.csv",row.names = F)
