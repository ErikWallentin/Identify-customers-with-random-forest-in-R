#####################################################################################################
# Ett givet företag behöver hjälp med att identifiera vilka kunder de ska vända sig till angående ett 
# erbjudande om att köpa en mjukvara (software) de skapat. Företaget önskar därför att en 
# random forest-modell som på ett bra sätt predikterar potentiella köpare skapas för att hjälpa dem 
# att inte slösa tid och pengar på fel kunder. Kunder som predikteras som icke köpare hör företaget 
# sig inte av till, medan de hör av sig till kunder som predikteras som köpare.

# För att skapa random forest-modell finns det tillgång till 14 variabler som nedan förklaras närmare.

#   Variabel		      Förklaring

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

# Ta bort icke fullständig data.
ofullständig <- c("N/A", "NULL", "Not Defined")
Data <- Data[!Data$County %in% ofullständig,]
Data <- Data[complete.cases(Data),]

# Vissa kolumner passar bättre som typen "numeric". Konvertera dessa faktor-variabler till numeric.
Data$CPLow <- as.numeric(Data$CPLow)
Data$NSB <- as.numeric(Data$NSB)
Data$AccessoryRatio <- as.numeric(Data$AccessoryRatio)
Data$Potential <- as.numeric(Data$Potential)
Data$NoOfEmployees <- as.numeric(Data$NoOfEmployees)

# Undersök om vi med hjälp av summary-funktionen direkt kan upptäcka outliers i datamaterialet.
summary(Data)

# Max-värdet för variabeln "NrOfOrder" ser suspekt stort ut. Undersök denna variabel vidare. 
table(Data$NrOfOrder)
plot(Data$NrOfOrder)

# Ta bort outliern från vår dataframe
Data <- Data[-which(Data$NrOfOrder==max(Data$NrOfOrder)),]

# Skapa en dataframe innehållande information om hur många observationer som är från ett givet county 
# sorterat från mest observationer till lägst. De 49 county'n med flest observationer förblir kvar, 
# medan de övriga slås ihop till ett county under namn "Others".
# Detta eftersom random forest-metoden i R endast kan inkludera en variabel innehållandes högst 
# 53 levels.
County_Vektor <- as.data.frame(table(Data$County))  
County_Vektor <- County_Vektor[order(County_Vektor[,2], decreasing = T),]
Övriga_County <- County_Vektor[c(50:nrow(County_Vektor)),1]
Övriga_County <- as.character(Övriga_County)
Data$County <- as.character(Data$County)
Data$County[Data$County %in% Övriga_County] <- "Others"
Data$County <- as.factor(Data$County)

# Skapa ett tränings data-set samt ett test data-set. 90 % av observationerna kommer hamna i
# tränings data-setet och resterande 10 % i test data-setet.
attach(Data)
set.seed(2)
train <- sample(1:nrow(Data), round(nrow(Data)*0.90, 0))

# Skapa en stor random forest-modell som skattar om en person kommer köpa en mjukvara eller ej.
install.packages("randomForest")
library(randomForest)
rf <- randomForest(Software ~ LineDiscount +  CustomerUnit + County
                   + Country + NoOfEmployees + Potential + AccessoryRatio + NSB
                   + CPLow + Days + TargetDays + NrOfOrder, 
                   data = Data, subset = train,
                   mtry = 3, importance = T, ntree = 1000)

# Skapa test data-setet innehållandes de variabler som inkluderas i randomforest-modellen.
install.packages("dplyr")
library(dplyr)
test_Dataset <- select(Data[-train,], LineDiscount, CustomerUnit, County,
                       Country, NoOfEmployees, Potential, AccessoryRatio, NSB,
                       CPLow, Days, TargetDays, NrOfOrder)

# Ta fram skattade värden på y, om en person köper en mjukvara eller ej.
rf.Yhatt <- predict(rf, newdata = test_Dataset)

# Ta fram de sanna klassificeringar som sedan jämförs med de skattade.
Software.test <- Software[-train]

# Skapa en "confusion matrix" samt räkna ut andelen korrekta/icke korrekta klassificeringar.
table(Skattat = rf.Yhatt, Sant = Software.test)
mean(rf.Yhatt == Software.test)
mean(rf.Yhatt != Software.test)

# Finjustera parametern "mtry" genom att testa ett värde på parametern mellan 1-12.
# En plott redovisar sedan vilket värde på "mtry" som ger lägst andel test-missklassifikation.
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

# Ta reda på vilka förklarande variabler som är viktigast i vår modell.
# (Vid klassificering används "MeanDecreaseGini"-plotten)
varImpPlot(rf)

# Visa att alla observationer som antar ett värde över 1100 i kolumen "TargetDays" är icke köpare.
# Detta är alltså en variabel som enkelt kan klassificera om en personer köper mjukvaran eller ej.
DataTargetDaysBig <- subset(Data, TargetDays > 1100)
summary(DataTargetDaysBig$Software)

# Skapa en mindre random forest-modell som inte inkluderar den viktigaste  
# förklarande varibeln "TargetDays". Andelen korrekta/icke korrekta klassificeringar räknas sedan ut.
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
# Ovanstående modell klassificerar en potentiell kund rätt ungefär 83% av fallen i vårt test-dataset.
# (Notera att detta värde kan variera något då random forest skapar 1000 slumpmässiga träd).
# Majoriteten av de som klassificeras rätt är dock personer som inte kommer köpa mjukvaran, medan
# personer som verkligen köpte mjukvaran klassificeras som att de inte köper i ca 72% av fallen.
# Ur ett Business-perspektiv är denna modell inte att föredra, då vi som ovan nämt misslyckas 
# att nå ut till ca 72% av köparna. 

# En term som används för att förklara andelen personer som korrekt klassificeras som köpare av 
# mjukvaran kallas för specificitet, och andelen personer som korrekt klassificeras som icke köpare 
# kallas för sensitivitet. Vi är i detta fall mer intresserade av modellens specificitet, som i
# ovanstående modell antar ett värde på ungefär 100 - 72 = 28%. Vi önskar öka denna andel, och gör
# det genom att justera värdet för hur en observation klassificeras som "No" eller "Yes".

# En markant ökning av specificiteten leder ofta till en  minsking av sensitiviteten, vilket i sin tur
# kan leda till att andelen personer som överlag klassificeras rätt av vår modell minskar. I vårt
# fall är det dock viktigare med en hög specificitet, än en hög allmän klassifications pricksäkerhet.
# Avvägningen mellan specificitet och sensitivitet kan fördelaktigen visualiseras genom en ROC-kurva.
#####################################################################################################

# Skapa en ROC-kurva
install.packages("pROC")
library(pROC)

rf.roc<-roc(Software[train],rf.Small$votes[,2])
par(pty = "s")
plot(rf.roc, main = "ROC-curve")

# Nedan klassificeras en kund som "No" om sannolikheten enligt modellen att personen
# inte köper produkten överstiger 95%, och som "Yes" annars. 
# I de två tidigare modellerna används gränsen > 50% för "No" och < 50% för "Yes".
# Önskas högre specificitet, ange ett värde ännu närmare 1 i argumentet "cutoff".
rf.Small.Spec <- randomForest(Software ~ NrOfOrder + Days + County + CPLow, 
                              data = Data, subset = train, cutoff = c(0.95, 1 - 0.95),
                              mtry = 1, importance = T, ntree = 1000)

rf.Small.Spec.Yhatt <- predict(rf.Small.Spec, newdata = test_Dataset)

# Skapa en "confusion matrix" samt räkna ut andelen korrekta/icke korrekta klassificeringar.
table <- table(Skattat = rf.Small.Spec.Yhatt, Sant = Software.test)
table
mean(rf.Small.Spec.Yhatt == Software.test)
mean(rf.Small.Spec.Yhatt != Software.test)

# ROC-kurvan som tidigare skapades uppdaterats nedan med värden för specificiteten samt 
# sensitiviteten. Arean under ROC-kurva redovisas också. Ju högre värde på arean, desto bättre.
# (Arean under kurvan kan justeras genom att inkludera alternativt exkludera variabler i vår modell).
plot(rf.roc, main = "ROC-curve",
     xlab = paste("Specificity = ", round(sum(table[2,2]) / sum(table[,2]),3)),
     ylab = paste("Sensitivity = ", round(sum(table[1,1]) / sum(table[,1]),3))) 
text(0.40, 0.15, paste("The area under the curve is", round(auc(rf.roc),3)))

#####################################################################################################
# I modellen ovan, där värdet för hur en observation klassificeras som "No" eller "Yes" justerats
# med avseende att öka specificiteten, når vi ut till ungefär 85% av alla köpare i test data-setet.
# De totala antalet personer som vi hör av oss till har ökat med ungefär 1000 personer, men i  
# kontrast till detta har försäljningen av mjukvaran ökat ca tre gånger så mycket om man jämför med  
# modellen som inte justerat för hur en observation klassificeras.
#####################################################################################################