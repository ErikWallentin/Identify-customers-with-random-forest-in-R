#####################################################################################################
# Ett givet f�retag beh�ver hj�lp med att identifiera vilka kunder de ska v�nda sig till ang�ende ett 
# erbjudande om att k�pa en mjukvara (software) de skapat. F�retaget �nskar d�rf�r att en 
# random forest-modell som p� ett bra s�tt predikterar potentiella k�pare skapas f�r att hj�lpa dem 
# att inte sl�sa tid och pengar p� fel kunder. Kunder som predikteras som icke k�pare h�r f�retaget 
# sig inte av till, medan de h�r av sig till kunder som predikteras som k�pare.

# F�r att skapa random forest-modell finns det tillg�ng till 14 variabler som nedan f�rklaras n�rmare.

#   Variabel		      F�rklaring

#   LineDiscount 		  Customer Discount Agreement
#   CustomerUnit		  Customer Size Segmentation
#   Municipality		  Municipality
#   County		        County
#   Country		        Country
#   NoOfEmployees	    Number of Employees
#   Potential		      Customer IT Spend Potential
#   AccessoryRatio	  Share of Accessories Purchased
#   NSB		            Net Sales (SEK)
#   CPLow		          Customer Profit (%)
#   Days		          Days since last purchase
#   TargetDays		    Days since software purchase
#   NrOfOrder		      Number of Orders
#   Software		      Customer bought software (Yes/No)
#####################################################################################################

# Importera data.
Data <- read.csv(file.choose(), header=TRUE, sep=",")

# Ta bort icke fullst�ndig data.
ofullst�ndig <- c("N/A", "NULL", "Not Defined")
Data <- Data[!Data$County %in% ofullst�ndig,]
Data <- Data[complete.cases(Data),]

# Vissa kolumner passar b�ttre som typen "numeric". Konvertera dessa faktor-variabler till numeric.
Data$CPLow <- as.numeric(Data$CPLow)
Data$NSB <- as.numeric(Data$NSB)
Data$AccessoryRatio <- as.numeric(Data$AccessoryRatio)
Data$Potential <- as.numeric(Data$Potential)
Data$NoOfEmployees <- as.numeric(Data$NoOfEmployees)

# Unders�k om vi med hj�lp av summary-funktionen direkt kan uppt�cka outliers i datamaterialet.
summary(Data)

# Max-v�rdet f�r variabeln "NrOfOrder" ser suspekt stort ut. Unders�k denna variabel vidare. 
table(Data$NrOfOrder)
plot(Data$NrOfOrder)

# Ta bort outliern fr�n v�r dataframe
Data <- Data[-which(Data$NrOfOrder==max(Data$NrOfOrder)),]

# Skapa en dataframe inneh�llande information om hur m�nga observationer som �r fr�n ett givet county 
# sorterat fr�n mest observationer till l�gst. De 49 county'n med flest observationer f�rblir kvar, 
# medan de �vriga sl�s ihop till ett county under namn "Others".
# Detta eftersom random forest-metoden i R endast kan inkludera en variabel inneh�llandes h�gst 
# 53 levels.
County_Vektor <- as.data.frame(table(Data$County))  
County_Vektor <- County_Vektor[order(County_Vektor[,2], decreasing = T),]
�vriga_County <- County_Vektor[c(50:nrow(County_Vektor)),1]
�vriga_County <- as.character(�vriga_County)
Data$County <- as.character(Data$County)
Data$County[Data$County %in% �vriga_County] <- "Others"
Data$County <- as.factor(Data$County)

# Skapa ett tr�nings data-set samt ett test data-set. 90 % av observationerna kommer hamna i
# tr�nings data-setet och resterande 10 % i test data-setet.
attach(Data)
set.seed(2)
train <- sample(1:nrow(Data), round(nrow(Data)*0.90, 0))

# Skapa en stor random forest-modell som skattar om en person kommer k�pa en mjukvara eller ej.
install.packages("randomForest")
library(randomForest)
rf <- randomForest(Software ~ LineDiscount +  CustomerUnit + County
                   + Country + NoOfEmployees + Potential + AccessoryRatio + NSB
                   + CPLow + Days + TargetDays + NrOfOrder, 
                   data = Data, subset = train,
                   mtry = 3, importance = T, ntree = 1000)

# Skapa test data-setet inneh�llandes de variabler som inkluderas i randomforest-modellen.
install.packages("dplyr")
library(dplyr)
test_Dataset <- select(Data[-train,], LineDiscount, CustomerUnit, County,
                       Country, NoOfEmployees, Potential, AccessoryRatio, NSB,
                       CPLow, Days, TargetDays, NrOfOrder)

# Ta fram skattade v�rden p� y, om en person k�per en mjukvara eller ej.
rf.Yhatt <- predict(rf, newdata = test_Dataset)

# Ta fram de sanna klassificeringar som sedan j�mf�rs med de skattade.
Software.test <- Software[-train]

# Skapa en "confusion matrix" samt r�kna ut andelen korrekta/icke korrekta klassificeringar.
table(Skattat = rf.Yhatt, Sant = Software.test)
mean(rf.Yhatt == Software.test)
mean(rf.Yhatt != Software.test)

# Finjustera parametern "mtry" genom att testa ett v�rde p� parametern mellan 1-12.
# En plott redovisar sedan vilket v�rde p� "mtry" som ger l�gst andel test-missklassifikation.
rf.MissClass <- rep(0,12)
for(d in 1:12){
  rf.Check <- randomForest(Software ~ LineDiscount +  CustomerUnit + County
                           + Country + NoOfEmployees + Potential + AccessoryRatio + NSB
                           + CPLow + Days + TargetDays + NrOfOrder, 
                           data = Data, subset = train,
                           mtry = d, importance = T, ntree = 1000)
  rf.Yhatt <- predict(rf.Check, newdata = test_Dataset)
  rf.MissClass[d] <- mean(rf.Yhatt != Software.test)
}

MTRY <- (1:12)
plot(MTRY, rf.MissClass, type = "b")

# Ta reda p� vilka f�rklarande variabler som �r viktigast i v�r modell.
# (Vid klassificering anv�nds "MeanDecreaseGini"-plotten)
varImpPlot(rf)

# Visa att alla observationer som antar ett v�rde �ver 1100 i kolumen "TargetDays" �r icke k�pare.
# Detta �r allts� en variabel som enkelt kan klassificera om en personer k�per mjukvaran eller ej.
DataTargetDaysBig <- subset(Data, TargetDays > 1100)
summary(DataTargetDaysBig$Software)

# Skapa en mindre random forest-modell som inte inkluderar den viktigaste  
# f�rklarande varibeln "TargetDays". Andelen korrekta/icke korrekta klassificeringar r�knas sedan ut.
test_Dataset <- select(Data[-train,], NrOfOrder, Days, County, CPLow)

rf.MissClass <- rep(0,4)
for(d in 1:4){
  rf.Check <- randomForest(Software ~ NrOfOrder + Days + County + CPLow, 
                           data = Data, subset = train,
                           mtry = d, importance = T, ntree = 1000)
  rf.Yhatt <- predict(rf.Check, newdata = test_Dataset)
  rf.MissClass[d] <- mean(rf.Yhatt != Software.test)
}

MTRY <- (1:4)
plot(MTRY, rf.MissClass, type = "b")

rf.Small <- randomForest(Software ~ NrOfOrder + Days + County + CPLow, 
                         data = Data, subset = train,
                         mtry = 1, importance = T, ntree = 1000)

rf.Small.Yhatt <- predict(rf.Small, newdata = test_Dataset)

table(Skattat = rf.Small.Yhatt, Sant = Software.test)
mean(rf.Small.Yhatt == Software.test)
mean(rf.Small.Yhatt != Software.test)

#####################################################################################################
# Ovanst�ende modell klassificerar en potentiell kund r�tt ungef�r 83% av fallen i v�rt test-dataset.
# (Notera att detta v�rde kan variera n�got d� random forest skapar 1000 slumpm�ssiga tr�d).
# Majoriteten av de som klassificeras r�tt �r dock personer som inte kommer k�pa mjukvaran, medan
# personer som verkligen k�pte mjukvaran klassificeras som att de inte k�per i ca 72% av fallen.
# Ur ett Business-perspektiv �r denna modell inte att f�redra, d� vi som ovan n�mt misslyckas 
# att n� ut till ca 72% av k�parna. 

# En term som anv�nds f�r att f�rklara andelen personer som korrekt klassificeras som k�pare av 
# mjukvaran kallas f�r specificitet, och andelen personer som korrekt klassificeras som icke k�pare 
# kallas f�r sensitivitet. Vi �r i detta fall mer intresserade av modellens specificitet, som i
# ovanst�ende modell antar ett v�rde p� ungef�r 100 - 72 = 28%. Vi �nskar �ka denna andel, och g�r
# det genom att justera v�rdet f�r hur en observation klassificeras som "No" eller "Yes".

# En markant �kning av specificiteten leder ofta till en  minsking av sensitiviteten, vilket i sin tur
# kan leda till att andelen personer som �verlag klassificeras r�tt av v�r modell minskar. I v�rt
# fall �r det dock viktigare med en h�g specificitet, �n en h�g allm�n klassifications pricks�kerhet.
# Avv�gningen mellan specificitet och sensitivitet kan f�rdelaktigen visualiseras genom en ROC-kurva.
#####################################################################################################

# Skapa en ROC-kurva
install.packages("pROC")
library(pROC)

rf.roc<-roc(Software[train],rf.Small$votes[,2])
par(pty = "s")
plot(rf.roc, main = "ROC-curve")

# Nedan klassificeras en kund som "No" om sannolikheten enligt modellen att personen
# inte k�per produkten �verstiger 95%, och som "Yes" annars. 
# I de tv� tidigare modellerna anv�nds gr�nsen > 50% f�r "No" och < 50% f�r "Yes".
# �nskas h�gre specificitet, ange ett v�rde �nnu n�rmare 1 i argumentet "cutoff".
rf.Small.Spec <- randomForest(Software ~ NrOfOrder + Days + County + CPLow, 
                              data = Data, subset = train, cutoff = c(0.95, 1 - 0.95),
                              mtry = 1, importance = T, ntree = 1000)

rf.Small.Spec.Yhatt <- predict(rf.Small.Spec, newdata = test_Dataset)

# Skapa en "confusion matrix" samt r�kna ut andelen korrekta/icke korrekta klassificeringar.
table <- table(Skattat = rf.Small.Spec.Yhatt, Sant = Software.test)
table
mean(rf.Small.Spec.Yhatt == Software.test)
mean(rf.Small.Spec.Yhatt != Software.test)

# ROC-kurvan som tidigare skapades uppdaterats nedan med v�rden f�r specificiteten samt 
# sensitiviteten. Arean under ROC-kurva redovisas ocks�. Ju h�gre v�rde p� arean, desto b�ttre.
# (Arean under kurvan kan justeras genom att inkludera alternativt exkludera variabler i v�r modell).
plot(rf.roc, main = "ROC-curve",
     xlab = paste("Specificity = ", round(sum(table[2,2]) / sum(table[,2]),3)),
     ylab = paste("Sensitivity = ", round(sum(table[1,1]) / sum(table[,1]),3))) 
text(0.40, 0.15, paste("The area under the curve is", round(auc(rf.roc),3)))

#####################################################################################################
# I modellen ovan, d�r v�rdet f�r hur en observation klassificeras som "No" eller "Yes" justerats
# med avseende att �ka specificiteten, n�r vi ut till ungef�r 85% av alla k�pare i test data-setet.
# De totala antalet personer som vi h�r av oss till har �kat med ungef�r 1000 personer, men i  
# kontrast till detta har f�rs�ljningen av mjukvaran �kat ca tre g�nger s� mycket om man j�mf�r med  
# modellen som inte justerat f�r hur en observation klassificeras.
#####################################################################################################