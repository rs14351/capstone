library(shiny)
library(stringr)
library(qdap)
library(tm)

unigram<-read.csv(paste(getwd(), "/data/unigram.csv", sep=""))
bigram<-read.csv(paste(getwd(), "/data/bigram.csv", sep=""))
trigram<-read.csv(paste(getwd(), "/data/trigram.csv", sep=""))
quadgram<-read.csv(paste(getwd(), "/data/quadgram.csv", sep=""))


CleanInputString <- function(input)
{
  # Test sentence
  
  # First remove the non-alphabatical characters
  input <- iconv(input, "latin1", "ASCII", sub=" ");
  input<-str_replace_all(input, pattern="[[:punct:]]","") 
  input<-gsub('[[:digit:]]+', '', input)
  input <- rm_stopwords(input, tm::stopwords("english"), unlist = FALSE, separate = FALSE, strip = FALSE, unique = FALSE, char.keep = NULL, names = FALSE, ignore.case = TRUE, apostrophe.remove = FALSE)
  input<-rm_white(input)
  input<-tolower(input)
  
  # If the resulting string is empty return empty and string.
  if (nchar(input) > 0) {
    return(input); 
  } else {
    return("");
  }
}


PredNextTerm <- function(inStr)
{
  assign("mesg", "in PredNextTerm", envir = .GlobalEnv)
  
  # Clean up the input string and extract only the words with no leading and trailing white spaces
  inStr <- CleanInputString(inStr);
  
  # Split the input string across white spaces and then extract the length
  inStr <- unlist(strsplit(inStr, split=" "));
  inStrLen <- length(inStr);
  
  nxtTermFound <- FALSE;
  predNxtTerm <- as.character(NULL);
  #mesg <<- as.character(NULL);
  # 1. First test the Four Gram using the four gram data frame
  if (inStrLen >= 3 & !nxtTermFound)
  {
    # Assemble the terms of the input string separated by one white space each
    inStr1 <- paste(inStr[(inStrLen-2):inStrLen], collapse=" ");
    
    # Subset the Four Gram data frame 
    searchStr <- paste("^",inStr1, sep = "");
    quadgram_temp <- quadgram[grep (searchStr, quadgram$word), ];
    
    # Check to see if any matching record returned
    if (length(quadgram_temp[, 2]) > 0 )
    {
      predNxtTerm <- quadgram_temp[1,2];
      nxtTermFound <- TRUE;
      mesg <<- "Next word is predicted using quadgram."
    }
    quadgram_temp <- NULL;
  }
  
  # 2. Next test the Triram using the trigram data frame
  if (inStrLen >= 2 & !nxtTermFound)
  {
    # Assemble the terms of the input string separated by one white space each
    inStr1 <- paste(inStr[(inStrLen-1):inStrLen], collapse=" ");
    
    # Subset the Three Gram data frame 
    searchStr <- paste("^",inStr1, sep = "");
    trigram_temp <- trigram[grep (searchStr, trigram$word), ];
    
    # Check to see if any matching record returned
    if (length(trigram_temp[, 2]) > 0 )
    {
      predNxtTerm <- trigram_temp[1,2];
      nxtTermFound <- TRUE;
      mesg <<- "Next word is predicted using trigram."
    }
    trigram_temp <- NULL;
  }
  
  # 3. Next test the Two Gram using the three gram data frame
  if (inStrLen >= 1 & !nxtTermFound)
  {
    # Assemble the terms of the input string separated by one white space each
    inStr1 <- inStr[inStrLen];
    
    # Subset the Two Gram data frame 
    searchStr <- paste("^",inStr1, sep = "");
    bigram_temp <- bigram[grep (searchStr, bigram$word), ];
    
    # Check to see if any matching record returned
    if (length(bigram_temp[, 2]) > 0 )
    {
      predNxtTerm <- bigram_temp[1,2];
      nxtTermFound <- TRUE;
      mesg <<- "Next word is predicted using bigram.";
    }
    bigram_temp <- NULL;
  }
  
  # 4. If no next term found in Four, Three and Two Grams return the most
  #    frequently used term from the One Gram using the one gram data frame
  if (!nxtTermFound & inStrLen > 0)
  {
    predNxtTerm <- unigram$terms[1];
    mesg <- "No next word found. The most frequent word is selected as next word."
  }
  
  nextTerm <- word(predNxtTerm, -1);
  
  if (inStrLen > 0){
    unigram_temp <- data.frame(nextTerm, mesg);
    return(unigram_temp);
  } else {
    nextTerm <- "";
    mesg <-"";
    unigram_temp <- data.frame(nextTerm, mesg);
    return(unigram_temp);
  }
}

msg <- ""
shinyServer(function(input, output) {
  output$prediction <- renderPrint({
    str2 <- CleanInputString(input$inputString);
    strDF <- PredNextTerm(str2);
    input$action;
    msg <<- as.character(strDF[1,2]);
    cat("", as.character(strDF[1,1]))
    cat("\n\t");
    cat("\n\t");
    cat("Note: ", as.character(strDF[1,2]));
  })
  
  output$text1 <- renderText({
    paste("Input Sentence: ", input$inputString)});
  
  output$text2 <- renderText({
    input$action;
    #paste("Note: ", msg);
  })
}
)
