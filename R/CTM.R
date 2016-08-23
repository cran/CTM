#' @title Term Document Matrix
#' @description Constructs Term-Document Matrix from Chinese Text Documents.
#'
#' @param doc The Chinese text document.
#' @param weighting Available weighting function with matrix are tf, tfidf.
#' @param shortTermDeleted Deltected short word when nchar <2.
#' @author Jim Liu
#' @details
#' This function run a Chinese word segmentation by jiebeR and build
#' term-document matrix, and there is two weighting function with matrix,
#' term frequency and term frequency inverse document frequency.
#'
#'
#' @export
#' @import jiebaR
#' @examples
#' library(CTM)
#' a <- "hello taiwan"
#' b <- "world of tank"
#' c <- "taiwan weather"
#' d <- "local weather"
#' text <- t(data.frame(a,b,c,d))
#' tdm <- CTDM(doc = text, weighting = "tfidf", shortTermDeleted = FALSE)



CTDM <- function(doc,weighting,shortTermDeleted){
  ###jiebaR
  cutter <- worker()
  cutfunc <- function(s){
    return(cutter <= s)
  }
  dataText <- doc
  segMatrix <- matrix(nrow = 1,ncol = 2)
  colnames(segMatrix) <- c("id","term")
  for(i in 1:length(dataText)){
    uniSeg <- data.frame(i,as.matrix(cutter[dataText[i]]))
    colnames(uniSeg) <- c("id","term")
    segMatrix <- rbind(segMatrix,uniSeg)
  }
  segMatrix <- segMatrix[-1,]
  ###
  term <- unique(segMatrix[,2])
  if(shortTermDeleted){
    term <- term[-which(nchar(term)<2)]
  }
  dtm <- matrix(nrow = length(dataText),ncol = length(term),0)
  if(weighting=="tf"){
    for(i in 1:length(term)){
      find1 <- table(segMatrix[which(segMatrix[,2]==term[i]),1])
      find2 <- table(segMatrix[which(segMatrix[,1] %in% as.numeric(names(find1))),1])
      dtm[as.numeric(names(find1)),i] <- find1/find2
      cat("\r","Calculating for",i,"of",length(term),"term")
    }
  }

  if(weighting=="tfidf"){
    for(i in 1:length(term)){
      find1 <- table(segMatrix[which(segMatrix[,2]==term[i]),1])
      find2 <- table(segMatrix[which(segMatrix[,1] %in% as.numeric(names(find1))),1])
      dtm[as.numeric(names(find1)),i] <- find1/find2
      dtm[,i] <- dtm[,i]*log(nrow(dtm)/(1+length(which(dtm[,i]>0))))
      cat("\r","Calculating for",i,"of",length(term),"term")
    }
  }
  colnames(dtm) <- term
  rownames(dtm) <- 1:length(dataText)
  dtm <- t(dtm)
}




#' @title Document Term Matrix
#' @description Constructs Document-Term Matrix from Chinese Text Documents.
#'
#' @param doc The Chinese text document.
#' @param weighting Available weighting function with matrix are tf, tfidf.
#' @param shortTermDeleted Deltected short word when nchar <2.
#' @author Jim Liu
#' @details
#' This function run a Chinese word segmentation by jiebeR and build document-term matrix
#' and there is two weighting function with matrix,
#' term frequency and term frequency inverse document frequency.
#'
#'
#' @export
#' @import jiebaR
#' @examples
#' library(CTM)
#' a <- "hello taiwan"
#' b <- "world of tank"
#' c <- "taiwan weather"
#' d <- "local weather"
#' text <- t(data.frame(a,b,c,d))
#' dtm <- CDTM(doc = text, weighting = "tfidf", shortTermDeleted = FALSE)



CDTM <- function(doc,weighting,shortTermDeleted){
  ###jiebaR
  cutter <- worker()
  cutfunc <- function(s){
    return(cutter <= s)
  }
  dataText <- doc
  segMatrix <- matrix(nrow = 1,ncol = 2)
  colnames(segMatrix) <- c("id","term")
  for(i in 1:length(dataText)){
    uniSeg <- data.frame(i,as.matrix(cutter[dataText[i]]))
    colnames(uniSeg) <- c("id","term")
    segMatrix <- rbind(segMatrix,uniSeg)
  }
  segMatrix <- segMatrix[-1,]
  ###
  term <- unique(segMatrix[,2])
  if(shortTermDeleted){
    term <- term[-which(nchar(term)<2)]
  }
  dtm <- matrix(nrow = length(dataText),ncol = length(term),0)
  if(weighting=="tf"){
    for(i in 1:length(term)){
      find1 <- table(segMatrix[which(segMatrix[,2]==term[i]),1])
      find2 <- table(segMatrix[which(segMatrix[,1] %in% as.numeric(names(find1))),1])
      dtm[as.numeric(names(find1)),i] <- find1/find2
      cat("\r","Calculating for",i,"of",length(term),"term")
    }
  }

  if(weighting=="tfidf"){
    for(i in 1:length(term)){
      find1 <- table(segMatrix[which(segMatrix[,2]==term[i]),1])
      find2 <- table(segMatrix[which(segMatrix[,1] %in% as.numeric(names(find1))),1])
      dtm[as.numeric(names(find1)),i] <- find1/find2
      dtm[,i] <- dtm[,i]*log(nrow(dtm)/(1+length(which(dtm[,i]>0))))
      cat("\r","Calculating for",i,"of",length(term),"term")
    }
  }
  colnames(dtm) <- term
  rownames(dtm) <- 1:length(dataText)
  dtm <- dtm
}



#' @title Term Count
#' @description Computing term count from text documents
#'
#' @param doc The Chinese text document.
#' @param shortTermDeleted Deltected short word when nchar <2.
#' @author Jim Liu
#' @details
#' This function run a Chinese word segmentation by jiebeR and
#' compute term count from these text document.
#'
#'
#' @export
#' @import jiebaR
#' @examples
#' library(CTM)
#' a <- "hello taiwan"
#' b <- "world of tank"
#' c <- "taiwan weather"
#' d <- "local weather"
#' text <- t(data.frame(a,b,c,d))
#' count <- termCount(doc = text, shortTermDeleted = FALSE)



termCount <- function(doc,shortTermDeleted){
  ###jiebaR
  cutter <- worker()
  cutfunc <- function(s){
    return(cutter <= s)
  }
  dataText <- doc
  res <- data.frame(table(unlist(lapply(dataText, cutfunc))))
  res[,1] <- as.character(res[,1])
  if(shortTermDeleted){
    res <- res[which(nchar(res[,1])>1),]
  }
  res <- res[order(-res$Freq),]
}


