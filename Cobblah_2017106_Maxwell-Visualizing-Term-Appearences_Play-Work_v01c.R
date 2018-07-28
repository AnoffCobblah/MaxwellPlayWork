rm(list=ls())

#TO DO: WRITE OUT THE PURPOSE OF THE SCRIPT BELOW EACH TIME
#PURPOSE: The purpose of this script is to take Campbell and Garnett's Life of James Clerk Maxwell and to visualize all appearances of work and play terms within the text.
#PURPOSE: This script can be improved by including a better list of play and work terms (editing searchedtermlist), and by better cleaning the .txt file being used.

#To Do: Change output location and datalocation each time.
outputlocation <- "D:/Box Sync/Computational-Criticism/Cobblah_20171106_Maxwell-Play-Work-Visualization"
datalocation <- "D:/Box Sync/Computational-Criticism/Cobblah_20171106_Maxwell-Play-Work-Visualization"

#################################### PART ONE: COMPUTATIONAL RECOVERY OF DATA #########################
files <- list.files(path = datalocation, 
                    pattern = "txt", full.names = TRUE) #creates vector of txt file names.

tok1 <- "coreNLP"
tok2 <- "tokenizers"
toggletoken <- tok2
searchedtermlist <- c("play","playful","playing","amuse","amusement","amusing","recreate","recreation","leisure","entertain","entertaining","diversion","pastime","game","spectacle","pleasure","sport","sporting","toy","fun","competition","compete","work","working","toil","labour","laborious","serious","earnest")
if (toggletoken == tok1) {
  library(coreNLP)
  initCoreNLP()
}
if (toggletoken == tok2) {
  #install.packages("tokenizers") #Only once
  library(tokenizers)
  #install.packages("readr")  #only once
  library(readr)
}
sentences <- list()
lemma <- list()
sentences_perc <- list()
lemma_perc <- list()
data <- matrix(,ncol=12,nrow=1)
tempdata <- matrix(,ncol=12,nrow=1)
colnames(data) <- c("Text","Text_ID", "searchedterm","Lemma", "Sentence","Lemma_Perc","Sentence_Perc","searchedterm_ID","category","date","author","Lemma_Length")
outputGraphics <- list()
for (p in 1:length(searchedtermlist)) {
  iter <- 1
  iterp <- 1
  iterpw <- 1
  searchedterm <- searchedtermlist[p]
  if (p <= 22) {tempcategory <- "play"} #TO DO: change this line with changing searchedtermlist
  if (p >= 23) {tempcategory <- "work"} #TO DO: change this line with changing searchedtermlist
  for (i in 1:length(files)) {
    tempdate <- "1882"
    tempauthor <- "Campbell"
    if (toggletoken == tok1) {
      anno <- annotateFile(files[i]) 
      token <- getToken(anno)
      for (j in 1:nrow(token)) {
        #NOTE: I chose to search for the appearences of the exact word ([j,3]).  A more
        #general method would be to search for appearances of the lemma ([j,4]).
        if(token[j,3] == searchedterm) {
          sentences[[iterp]] <- token[j,1]
          lemma[[iterp]] <- j
          iterp <- iterp+1
        }
      }
      for (k in 1:length(lemma)) {
        sentences_perc[[k]] <- (sentences[[k]] / token[nrow(token),1]) *100
        lemma_perc[[k]] <- (lemma[[k]] / nrow(token)) *100
      }
    }
    if (toggletoken == tok2) {
      fileName <- read_file(files[i])
      #since tokenize_sentences function requires things to be encoded in UTF-8, need to remove some data.
      Encoding(fileName) <- "UTF-8"
      fileName <- iconv(fileName, "UTF-8", "UTF-8",sub='')
      ltoken <- tokenize_words(fileName, lowercase = TRUE, stopwords = NULL, simplify = FALSE)
      stoken <- tokenize_sentences(fileName, lowercase = FALSE, strip_punctuation = FALSE, simplify = FALSE)
      ltoken <- unlist(ltoken)
      stoken <- unlist(stoken)
      for (w in 1:length(ltoken)) {
        #NOTE: I chose to search for the appearences of the exact word ([j,3]).  A more
        #general method would be to search for appearances of the lemma ([j,4]).
        if(ltoken[w] == searchedterm) {
          lemma[[iterp]] <- w
          iterp <- iterp+1
        }
      }
      for (z in 1:length(stoken)) {
        if(grepl(searchedterm, stoken[z], ignore.case = TRUE)) {
          sentences[[iterpw]] <- z
          iterpw <- iterpw+1
        }
      }
      if (length(lemma) != 0){
        for (k in 1:length(lemma)) {
          lemma_perc[[k]] <- (lemma[[k]] / length(ltoken)) *100
        }
      }
      if (length(sentences) != 0) {
        for (g in 1:length(sentences)) {
          sentences_perc[[g]] <- (sentences[[g]] / length(stoken)) *100
        }
      }
    }
    lemma <- unlist(lemma)
    if (length(lemma) == 0) {
      lemma <- NA
    }
    sentences <- unlist(sentences)
    if (length(sentences) == 0) {
      sentences <- NA
    }
    lemma_perc <- unlist(lemma_perc)
    if (length(lemma_perc) == 0) {
      lemma_perc <- NA
    }
    sentences_perc <- unlist(sentences_perc)
    if (length(sentences_perc) == 0) {
      sentences_perc <- NA
    }
    if (length(lemma) > length(sentences)) {
      mat <- matrix(,ncol=12,nrow=length(lemma))
      mat[,1] <- files[i]
      mat[,2] <- i
      mat[,3] <- searchedterm
      mat[,4] <- lemma
      mat[1:length(sentences),5] <- sentences
      mat[(length(sentences)+1):length(lemma),5] <- NA
      mat[,6] <- lemma_perc
      mat[1:length(sentences),7] <- sentences_perc
      mat[(length(sentences)+1):length(lemma),7] <- NA
      mat[,8] <- p
      mat[,9] <- tempcategory
      mat[,10] <- tempdate
      mat[,11] <- tempauthor
      mat[,12] <- length(ltoken)
    }
    if (length(lemma) < length(sentences)) {
      mat <- matrix(,ncol=12,nrow=length(sentences))
      mat[,1] <- files[i]
      mat[,2] <- i
      mat[,3] <- searchedterm
      mat[1:length(lemma),4] <- lemma
      mat[(length(lemma)+1):length(sentences),4] <- NA
      mat[,5] <- sentences
      mat[1:length(lemma_perc),6] <- lemma_perc
      mat[(length(lemma_perc)+1):length(sentences),6] <- NA
      mat[,7] <- sentences_perc
      mat[,8] <- p
      mat[,9] <- tempcategory
      mat[,10] <- tempdate
      mat[,11] <- tempauthor
      mat[,12] <- length(ltoken)
    }
    if (length(lemma) == length(sentences)) {
      mat <- matrix(,ncol=12,nrow=length(sentences))
      mat[,1] <- files[i]
      mat[,2] <- i
      mat[,3] <- searchedterm
      mat[,4] <- lemma
      mat[,5] <- sentences
      mat[,6] <- lemma_perc
      mat[,7] <- sentences_perc
      mat[,8] <- p
      mat[,9] <- tempcategory
      mat[,10] <- tempdate
      mat[,11] <- tempauthor
      mat[,12] <- length(ltoken)
    }
    tempdata <- rbind(tempdata,mat)
    sentences <- list()
    lemma <- list()
    sentences_perc <- list()
    lemma_perc <- list()
  }
  tempdata <- tempdata[-1,]
  data <- rbind(data,tempdata)
}
data <- data[-1,]
datadf <- as.data.frame(data)
dup <- matrix(,ncol=12,nrow=1)
for (o in 1:length(files)) {
  dupdf <- datadf[grep(files[o],datadf$Text),]
  dupdf <- dupdf[complete.cases(dupdf$Lemma),] #my data method creates a lot of duplicates and empty rows.  These steps erase those.
  dupdf <- dupdf[!duplicated(dupdf$Lemma),]
  dupmat <- as.matrix(dupdf)
  dup <- rbind(dup,dupmat)
}
dup <- dup[-1,]
datadf <- as.data.frame(dup)

#Plotting and data output begin below
write.table(datadf, file.path(outputlocation,"Maxwell-Appearance-play-work-datadf"))

############# Visualizations for each play term  ##############
#Scatterplot
maxwellindex <- grep("1882",datadf$date)
maxwelldf <- datadf[maxwellindex,]
maxwelldf$searchedterm <- factor(maxwelldf$searchedterm, levels = maxwelldf$searchedterm[order(as.numeric(as.character(maxwelldf$searchedterm_ID)))])
dev.off()
pdf(file = file.path(outputlocation,"Maxwell_1882_Lemma_Perc-play-workoutput.pdf"))
library(ggplot2)
colScale <- scale_colour_manual(name = "category",values = myColors)
p <- ggplot(maxwelldf, aes(y = as.factor(searchedterm), x = as.numeric(as.character(Lemma_Perc)), color = as.factor(category))) + geom_point(size=1,pch = 3)
pl <- p + labs(x = "% of Text (by lemma)", y = "Searched Term", title = "Appearances of Play and Work in Campbell, Life of Maxwell")
pl
dev.off()

#Tile Plot
histmaxwellmat <- matrix(,nrow=1,ncol=10)
for (u in 1:length(searchedtermlist)) {
  blankmat <- matrix(,nrow=20,ncol=10)
  colnames(blankmat) <- c("searchedterm","searchedterm_ID", "bin", "bin_avg", "frequency","Text","Text_ID","category","date","author")
  tempindex <- grep(searchedtermlist[u],maxwelldf$searchedterm)
  tempdf <- maxwelldf[tempindex,]
  bins <- cut(as.numeric(as.character(tempdf$Lemma_Perc)),breaks=seq(0,100,by=5),labels=FALSE, include.lowest = TRUE)
  blankmat[,1] <- searchedtermlist[u]
  blankmat[,2] <- u
  blankmat[,3] <- c("(0-5)","(5-10)", "(10-15)", "(15-20)", "(20-25)", "(25-30)", "(30-35)", "(35-40)", "(40-45)", "(45-50)", "(50-55)", "(55-60)", "(60-65)", "(65-70)", "(70-75)", "(75-80)", "(80-85)", "(85-90)", "(90-95)", "(95-100)")
  blankmat[,4] <- c(2.5,7.5,12.5,17.5,22.5,27.5,32.5,37.5,42.5,47.5,52.5,57.5,62.5,67.5,72.5,77.5,82.5,87.5,92.5,97.5)
  blankmat[,5] <- 0 #ran into some trouble here.  Cut doesn't always return the # of bins if no data, so I needed to add it
  for (v in 1:20) {
    mat3 <- as.matrix(table(bins))
    vec3 <- as.numeric(rownames(mat3))
    blankmat[vec3[v],5] <- mat3[v]
  }
  blankmat[,6] <- maxwelldf[1,1]
  blankmat[,7] <- maxwelldf[1,2]
  if (u <= 22) {tempcategory <- "play"}
  if (u >= 23) {tempcategory <- "work"}
  blankmat[,8] <- tempcategory
  blankmat[,9] <- as.character(maxwelldf[1,10])
  blankmat[,10] <- as.character(maxwelldf[1,11])
  histmaxwellmat <- rbind(histmaxwellmat,blankmat)
}
histmaxwellmat <- histmaxwellmat[-1,]
histmaxwelldf <- as.data.frame(histmaxwellmat)
histmaxwelldf$searchedterm <- factor(histmaxwelldf$searchedterm, levels = histmaxwelldf$searchedterm[order(histmaxwelldf$category)])
histmaxwelldf$bin <- factor(histmaxwelldf$bin, levels=histmaxwelldf$bin[order(as.numeric(as.character(histmaxwelldf$bin_avg)))])
dev.off()
pdf(file = file.path(outputlocation,"Maxwell_1882_Lemma_Perc-play-workoutput_tile.pdf"))
library(ggplot2)
p <- ggplot(histmaxwelldf, aes(y = as.factor(searchedterm), x = as.factor(bin))) + geom_tile(aes(fill=as.numeric(as.character(frequency)))) +scale_fill_gradient(low="blue",high="red")
pl <- p +labs(x = "% of Text (by lemma)", y = "Searched Term", title = "Appearances of Play and Work in Campbell, Life of Maxwell") + theme(text = element_text(size=8), legend.position = "bottom", legend.title = element_blank())
pl
dev.off()
#Binned-Dotplot
dev.off()
pdf(file = file.path(outputlocation,"Maxwell_1882_Lemma_Perc-play-workoutput_point-binned.pdf"))
library(ggplot2)
library(RColorBrewer)
p <- ggplot(histmaxwelldf) + geom_point(aes(y = as.factor(searchedterm), x = as.factor(bin), size = (as.numeric(as.character(frequency))), color = as.factor(category)))
pl <- p + labs(x = "% of Text (by lemma)", y = "Searched Term", title = "Appearances of Play and Work in Campbell, Life of Maxwell") + theme(text = element_text(size=8),legend.position = "bottom", legend.title = element_blank())
pl
dev.off()

##############Trying to get bar plot##########
playdf <- datadf[grep("play",datadf$category),]
workdf <- datadf[grep("work",datadf$category),]
tempplaydf <- cbind(playdf,paste(playdf$author,"play-terms", sep = "-"))
tempworkdf <- cbind(workdf,paste(workdf$author,"work-terms", sep = "-"))
colnames(tempplaydf)[13] <- "corpuslabel"
colnames(tempworkdf)[13] <- "corpuslabel"
corpusdatadf <- rbind(tempplaydf,tempworkdf)

#1. Tile Plot
histcorpusmat <- matrix(,nrow=1,ncol=6)
corpuslabels <- c("Campbell-play-terms", "Campbell-work-terms")
for (u in 1:length(corpuslabels)) {
  blankmat <- matrix(,nrow=20,ncol=6)
  colnames(blankmat) <- c("corpuslabel", "bin", "bin_avg", "frequency","category","freq_per_lemma")
  tempindex <- grep(corpuslabels[u],corpusdatadf$corpuslabel)
  tempdf <- corpusdatadf[tempindex,]
  bins <- cut(as.numeric(as.character(tempdf$Lemma_Perc)),breaks=seq(0,100,by=5),labels=FALSE, include.lowest = TRUE)
  blankmat[,1] <- corpuslabels[u]
  blankmat[,2] <- c("(0-5)","(5-10)", "(10-15)", "(15-20)", "(20-25)", "(25-30)", "(30-35)", "(35-40)", "(40-45)", "(45-50)", "(50-55)", "(55-60)", "(60-65)", "(65-70)", "(70-75)", "(75-80)", "(80-85)", "(85-90)", "(90-95)", "(95-100)")
  blankmat[,3] <- c(2.5,7.5,12.5,17.5,22.5,27.5,32.5,37.5,42.5,47.5,52.5,57.5,62.5,67.5,72.5,77.5,82.5,87.5,92.5,97.5)
  blankmat[,4] <- 0
  for (v in 1:20) {
    mat3 <- as.matrix(table(bins))
    vec3 <- as.numeric(rownames(mat3))
    blankmat[vec3[v],4] <- mat3[v]
    if (u==1) {blankmat[,5] <- "play"}
    if (u==2) {blankmat[,5] <- "work"}
    blankmat[,6] <- (as.numeric(as.character(blankmat[,4]))/as.numeric(as.character(tempdf$Lemma_Length[2])))*100
  }
  histcorpusmat <- rbind(histcorpusmat,blankmat)
}
histcorpusmat <- histcorpusmat[-1,]
histcorpusdf <- as.data.frame(histcorpusmat)
histcorpusdf$corpuslabel <- factor(histcorpusdf$corpuslabel, levels = histcorpusdf$corpuslabel[order(as.character(histcorpusdf$corpuslabel))])
histcorpusdf$bin <- factor(histcorpusdf$bin, levels=histcorpusdf$bin[order(as.numeric(as.character(histcorpusdf$bin_avg)))])
dev.off()
pdf(file = file.path(outputlocation,"Lemma_play-workoutput_tile-freq.pdf"))
library(ggplot2)
p <- ggplot(histcorpusdf, aes(y = as.factor(corpuslabel), x = as.factor(bin))) + geom_tile(aes(fill=as.numeric(as.character(frequency)))) +scale_fill_gradient(low="blue",high="red")
pl <- p +labs(x = "% of Text (by lemma)", y = "Text Terms", title = "Appearances of Play and Work in Campbell, Life of Maxwell") + theme(text = element_text(size=8), legend.position = "bottom", legend.title = element_blank())
pl
dev.off()
dev.off()
pdf(file = file.path(outputlocation,"Lemma_play-workoutput_tile-freqperlemma.pdf"))
library(ggplot2)
p <- ggplot(histcorpusdf, aes(y = as.factor(corpuslabel), x = as.factor(bin))) + geom_tile(aes(fill=as.numeric(as.character(freq_per_lemma)))) +scale_fill_gradient(low="blue",high="red")
pl <- p +labs(x = "% of Text (by lemma)", y = "Text Terms", title = "Appearances of Play and Work Campbell, Life of Maxwell, Normalized by the Number of Lemma") + theme(text = element_text(size=8), legend.position = "bottom", legend.title = element_blank())
pl
dev.off()
#3. Binned-Dotplot
dev.off()
pdf(file = file.path(outputlocation,"Lemma-play-workoutput_point-freq.pdf"))
library(ggplot2)
p <- ggplot(histcorpusdf) + geom_point(aes(y = as.factor(corpuslabel), x = as.factor(bin), size = (as.numeric(as.character(frequency))), color = as.factor(category)))
pl <- p + labs(x = "% of Text (by lemma)", y = "Searched Term", title = "Appearances of Play and Work in Campbell, Life of Maxwell") + theme(text = element_text(size=8),legend.position = "bottom", legend.title = element_blank())
pl
dev.off()
dev.off()
pdf(file = file.path(outputlocation,"Lemma-play-workoutput_point-freqperlemma.pdf"))
library(ggplot2)
p <- ggplot(histcorpusdf) + geom_point(aes(y = as.factor(corpuslabel), x = as.factor(bin), size = (as.numeric(as.character(freq_per_lemma))), color = as.factor(category)))
pl <- p + labs(x = "% of Text (by lemma)", y = "Searched Term", title = "Appearances of Play and Work in Campbell, Life of Maxwell, Normalized by the Number of Lemma") + theme(text = element_text(size=8),legend.position = "bottom", legend.title = element_blank())
pl
dev.off()


maxwellplaydf <- playdf[grep("Campbell",playdf$author),]
maxwellworkdf <- workdf[grep("Campbell",workdf$author),]
dev.off()
pdf(file = file.path(outputlocation,"Corpus_Prob-Dense-Histograms.pdf"))
par(mfrow=c(1,2))
hist(as.numeric(as.character(maxwellplaydf$Lemma_Perc)), breaks=c(0:100),freq=TRUE, main = "Histogram of Play in Campbell,\n Life of Maxwell", xlab = "% of Text", ylim=c(0,15))
hist(as.numeric(as.character(maxwellworkdf$Lemma_Perc)), breaks=c(0:100),freq=TRUE, main = "Histogram of Work in Campbell,\n Life of Maxwell", xlab = "% of Text",ylim=c(0,15))
dev.off()

