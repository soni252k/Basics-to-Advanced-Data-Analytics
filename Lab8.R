#installing and loading the required package- hashFunction (not present in R version 4.0.3 and 
                                                            #is removed from the CRAN repository)

#1. download hashFunction from https://cran.r-project.org/src/contrib/Archive/hashFunction/

#2. download rtools from https://cran.r-project.org/bin/windows/Rtools/ and run it. 

#3. open rstudio

#4. follow the following two steps on r studio:
    #writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")

    #restart r studio and run the following command:
    #Sys.which("make")

#5. install the package using the following command-
#install.packages("C:/Users/sonis/Downloads/hashFunction_1.0.tar.gz", repos = NULL, type = "source")

#6. load using library function
library(hashFunction)

#---------------------------------
#now that we are done with installation of the package, 
#let's perform bloom filter

k<- 5 #total no. of hash function
m<- 100 #size of bloom filter
bf<- matrix(0,1,m)

s<- c(1:100)

for(i in s){
  h<- cityhash.64(as.character(i))[1]%%m
  g<- cityhash.64(as.character(n))[2]%%m
  hash_indices <- c()
  for(j in 1:k){
    hash_indices[j]<- (h+j*g)%%m
    if(hash_indices[j]==0)
      hash_indices[j]<- 1
  }
  bf[,hash_indices]<- 1
}

bf

#-----------------
cityhash.64('soni')

#-----------------
#implementation of bloom filter

query <- function(bitarray, str) {
  s <- c(1:100)
  m<- 100
  for(i in s){
    
    hash_indices <- c()
    for(j in 1:length(str)){
      if(bitArray[hash_indices[j]])
        return (1)
      else
        return (0)
    }
  }
}

insert <- function(bitArray,str) {
#  if(query(bitarray, size, str) == 1) {
#    print(paste(str, "already present."))
#  }
#  else {
    hash_indices <- c()
    
    for(iter in 1:length(str)) {
      hash_indices[iter] <- 1
      print(paste(str, "inserted"))
      return (bitArray)
    }
  }
#}

bitArray <- matrix(0,1, 26)
bitArray
s <- c("abound", "arouse", "adapt")
s
s[1,1]

bitArray<- insert(bitArray, s)
bitArray

query(bitArray, size,  "uri")
query(bitArray, size,  "zuhb1dsakfj")
query(bitArray, size,  "abound")
