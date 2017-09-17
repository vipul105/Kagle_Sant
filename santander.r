
sample <- read.csv("D:/Documents/santander/Final/san_data.csv")


#View(sample)
#install.packages("dummies")
library(dummies)

### START: Country ####

sample <- dummy.data.frame(sample, names = c("pais_residencia"))

sample$pais_residencia <- NULL 

### END: Country ####

### START: Income SD ####

mean_inc <- mean(sample$renta,na.rm=TRUE)

sd_inc <- sd(sample$renta,na.rm=TRUE)

sample$Zval_inc <- (sample$renta - mean_inc)/sd_inc
sample$inc_fac <- cut(sample$Zval_inc, breaks = c(0,5,10,15,20,35,50,65,80)) 
sample <- dummy.data.frame(sample, names = c("inc_fac"))

sample$Zval_inc <- NULL
sample$renta<- NULL
### END: Income SD ####

### START: Segmentation ####

sample <- dummy.data.frame(sample, names = c("segmento"))

sample$segmento <- NULL
### END: Segmentation ####

### START: Age ####

sample$age_fac <- cut(as.integer(sample$age), breaks = c(1,10,15,20,35,45,60,96)) 

sample <- dummy.data.frame(sample, names = c("age_fac"))

sample$age <- NULL

### END: Age ####

### START: Customer Type ####

sample <- dummy.data.frame(sample, names = c("indrel_1mes"))

sample$indrel_1mes <- NULL

### END:  Customer Type ####

### START: Customer relation type ####

sample <- dummy.data.frame(sample, names = c("tiprel_1mes"))
sample$tiprel_1mes <- NULL
### END:  Customer relation type ####

### START: Residence index ####

sample <- dummy.data.frame(sample, names = c("indresi"))
sample$indresi <- NULL
### END:  Residence index ####

### START: Employee index ####

sample <- dummy.data.frame(sample, names = c("ind_empleado"))
sample$ind_empleado <- NULL
### END:  Employee index ####

### START: sex ####

sample <- dummy.data.frame(sample, names = c("sexo"))
sample$sexo <- NULL
### END: sex ####


### START: Activity index ####

sample <- dummy.data.frame(sample, names = c("ind_actividad_cliente"))
sample$ind_actividad_cliente <- NULL

### END: Activity index ####

### START: Removing COls ####
sample$X1 <- NULL
sample$fecha_dato <- NULL

sample$ind_empleado <- NULL

sample$fecha_alta <- NULL
sample$ind_nuevo <- NULL
sample$antiguedad <- NULL

sample$indrel <- NULL
sample$ult_fec_cli_1t <- NULL
sample$indext<- NULL

sample$conyuemp <- NULL
sample$canal_entrada <- NULL
sample$indfall <- NULL

sample$tipodom <- NULL
sample$cod_prov <- NULL

sample$nomprov <- NULL

### END: Removing COls ####

# calculate cosine similarity between two vectors
cosine_sim <- function(x,y) 
{
  this.cosine <- sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
  return(this.cosine)
}

# Making a item vs. item matrix
sample.similarity  <- matrix(NA, nrow=ncol(sample),ncol=ncol(sample),dimnames=list(colnames(sample),colnames(sample)))

# filling cosine similarity 
for(i in 1:ncol(sample)) {
  
  for(j in 1:ncol(sample)) {
    sample.similarity[i,j] <- cosine_sim(as.matrix(sample[i]),as.matrix(sample[j]))
  }
}

sample.similarity <- as.data.frame(sample.similarity)

#calculate score
getScore <- function(history, similarities)
{
  x <- sum(history*similarities)/sum(similarities)
  x
}
# holder matrix 
holder <- matrix(NA, nrow=nrow(sample),ncol=ncol(sample)-1,dimnames=list((sample$ncodpers),colnames(sample[-1])))

for(i in 1:nrow(holder)) 
{
  
  for(j in 1:ncol(holder)) 
  {
    
    user <- rownames(holder)[i]
    product <- colnames(holder)[j]
    
    # already consumed it, we store an empty
    if(as.integer(sample[sample$ncodpers==user,product]) == 1)
    { 
      holder[i,j]<-""
    } else {
      
      # top 10 neighbours sorted by similarity
      topN<-((head(n=11,(sample.similarity[order(sample.similarity[,product],decreasing=TRUE),][product]))))
      topN.names <- as.character(rownames(topN))
      topN.similarities <- as.numeric(topN[,1])
      
      # Drop the first one because it will always be the same product
      topN.similarities<-topN.similarities[-1]
      topN.names<-topN.names[-1]
      
      # We then get the user's purchase history for those 10 items
      topN.purchases<- sample[,c("ncodpers",topN.names)]
      topN.userPurchases<-topN.purchases[topN.purchases$ncodpers==user,]
      topN.userPurchases <- as.numeric(topN.userPurchases[!(names(topN.userPurchases) %in% c("ncodpers"))])
      
      # We then calculate the score for that product and that user
      holder[i,j]<-getScore(similarities=topN.similarities,history=topN.userPurchases)
      
    } 
  } 
} 

sample.scores <- holder

# removing non products cols 
sample.scores <- sample.scores[,!colnames(sample.scores) %in% "ind_empleadoN"]
sample.scores <- sample.scores[,!colnames(sample.scores) %in% "pais_residenciaES"]
sample.scores <- sample.scores[,!colnames(sample.scores) %in% "sexoH"]
sample.scores <- sample.scores[,!colnames(sample.scores) %in% "indrel_1mesNA"]
sample.scores <- sample.scores[,!colnames(sample.scores) %in% "sexoV"]
sample.scores <- sample.scores[,!colnames(sample.scores) %in% "indrel_1mes1"]
sample.scores <- sample.scores[,!colnames(sample.scores) %in% "tiprel_1mesI"]
sample.scores <- sample.scores[,!colnames(sample.scores) %in% "indresiS"]

sample.scores <- sample.scores[,!colnames(sample.scores) %in% "tiprel_1mesA"]
sample.scores <- sample.scores[,!colnames(sample.scores) %in% "ind_actividad_cliente0"]
sample.scores <- sample.scores[,!colnames(sample.scores) %in% "ind_actividad_cliente1"]
sample.scores <- sample.scores[,!colnames(sample.scores) %in% "ind_actividad_clienteNA"]
sample.scores <- sample.scores[,!colnames(sample.scores) %in% "segmento02 - PARTICULARES"]
sample.scores <- sample.scores[,!colnames(sample.scores) %in% "segmento03 - UNIVERSITARIO"]
sample.scores <- sample.scores[,!colnames(sample.scores) %in% "inc_fac(0,5]"]

sample.scores <- sample.scores[,!colnames(sample.scores) %in% "inc_fac(5,10]"]
sample.scores <- sample.scores[,!colnames(sample.scores) %in% "inc_facNA"]
sample.scores <- sample.scores[,!colnames(sample.scores) %in% "age_fac(1,10]"]
sample.scores <- sample.scores[,!colnames(sample.scores) %in% "age_fac(10,15]"]
sample.scores <- sample.scores[,!colnames(sample.scores) %in% "age_fac(15,20]"]
sample.scores <- sample.scores[,!colnames(sample.scores) %in% "age_fac(20,35]"]
sample.scores <- sample.scores[,!colnames(sample.scores) %in% "age_facNA"]

#modifying matrix to look clean
sample.scores.holder <- matrix(NA, nrow=nrow(sample.scores),ncol=1,dimnames=list(rownames(sample.scores)))
#View(sample.scores.holder)
for(i in 1:nrow(sample.scores)) 
{
  sample.scores.holder[i,] <- names(head(n=1,(sample.scores[,order(sample.scores[i,],decreasing=TRUE)])[i,]))
}

#View(sample.scores.holder)
colnames(sample.scores.holder)[1] <- c("added_products")
write.csv(sample.scores.holder,"D:/Documents/santander/Final/submission.csv")