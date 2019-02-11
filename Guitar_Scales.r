######################################################################
## Shows the fingering for given scales on the guitar fretboard. Root
## note as in: Ab, A, Bb, C etc..  A scale among MAJ, MIN, PMAJ, PMIN,
## DIM, DIM2, PHRDOM, BLUES, ION, DOR, PHR LYD, MYX, AEO, LOC. The
## user can, however define additional scales.
## Run the whole script, to load the functions, then at the R prompt,
## type 'Fingering()', this will show you the fingering for the default
## Key=C and Scale=MAJ. The Fingering() function takes two arguments:
## Key and Scale, (i.e. for Key=A, scale=MIN): Fingering(A, MIN).
## The output is a graphical table showing the positions on the guitar
## neck 'X', corresponding to the notes of the specific key and scale.
## Script by M@Z (2018): use freely.
## Please acknowledge author.
######################################################################

##Clean up the workspace
rm(list=ls())

## These libraries for displaying graphical tables
library(ggplot2)
library(grid)
library(gridExtra)
library(gtable)

##DEFINE THE CHROMATIC SCALE AS A REFERENCE
Notes <- c('Ab','A','Bb','B','C','Db','D','Eb','E','F','Gb','G')

##DEFINE VARIABLES WITH THE SAME NAMES AS THE DEGREES OF THE
##CHROMATIC SCALE, SO YOU DON'T NEED TO TYPE THE QUOTATION MARKS
##EVERY TIME
Ab <- 'Ab'; A <- 'A'; Bb <- 'Bb'; B <- 'B'; C <- 'C'; Db <- 'Db'
D <- 'D'; Eb <- 'Eb'; E <- 'E'; F <- 'F'; Gb <- 'Gb'; G <- 'G'

##VOCABULARY
##DEFINE THE INTERVALS THAT CHARACTERISE THE SCALES
MAJ <- c(2, 2, 1, 2, 2, 2, 1)
MIN <- c(2, 1, 2, 2, 1, 2, 2)
PMAJ <- c(2, 2, 3, 2, 3) #Pentatonic Major
PMIN <- c(3, 2, 2, 3, 2) #Pentatonic Minor
DIM <- c(2, 1, 2, 1, 2, 1, 2, 1) #Whole-half
DIM2 <- c(1, 2, 1, 2, 1, 2, 1, 2) #Half-Whole
WHT <- c(2, 2, 2, 2, 2, 2, 2) #The Whole Tone scale
PHRDOM <- c(1, 3, 1, 2, 1, 2, 2) #Phrygian Dominant or Spanish Phrygian
BLUES <- c(3, 2, 1, 1, 3, 2) #Based on PMIN with extra altered notes

##Intervals for MODAL scales
ION <- c(2, 2, 1, 2, 2, 2, 1)
DOR <- c(2, 1, 2, 2, 2, 1, 2)
PHR <- c(1, 2, 2, 2, 1, 2, 2)
LYD <- c(2, 2, 2, 1, 2, 2, 1)
MYX <- c(2, 2, 1, 2, 2, 1, 2)
AEO <- c(2, 1, 2, 2, 1, 2, 2)
LOC <- c(1, 2, 2, 1, 2, 2, 2)

##THIS FUNCTION BUILDS THE SCALE ON THE GUITAR NECK
MakeScale <- function(Root='C', Intervals=MAJ){
Tones <- rep(which(Notes==Root),13)
IntVect <- length(Intervals)
for(i in 2:13){
  j <- i-1
  ##Modulo IntVect to fit Intervals vector
  if((i-1) > IntVect & (i-1)%%IntVect == 0) j <- IntVect
  else if((i-1) > IntVect & (i-1)%%IntVect != 0) j <- (i-1)%%IntVect
  Tones[i] <- Tones[i-1] + Intervals[j]
  if (Tones[i] > 12) Tones[i] <- Tones[i]%%12  #Modulo 12
  }
Tones
}

##Bind frets by rows to form the first twelve frets of the guitar neck
Guit <- rbind(c(9:12, 1:9), c(4:12, 1:4), c(12, 1:12), c(7:12, 1:7), c(2:12,1:2), c(9:12, 1:9))

Fingering <- function(Key='C', Scale=MAJ){
  Label <- deparse(substitute(Scale))
  Tones <- MakeScale(Key, Scale)
  Guit2 <- Guit %in% Tones #Find them in the matrix of frets
  dim(Guit2) <- c(6,13)
  ##NEED TO CONSTRUCT THE FRET NUMBERS SO THEY GET EQUAL COLUMN SPACE
  ##IN THE FINAL TABLE
  xxx <- paste(c(0:9), ' ')
  yyy <- paste(c(10:12))
  colnames(Guit2) <- c(xxx, yyy)
  rownames(Guit2) <- c('e','B','G','D','A','E')
  ##Here we need to convert to a dataframe in order to substitute
  ##numbers in the matrix with characters, in order to print as a
  ##table
  Guit2 <- as.data.frame(Guit2)

  Guit2[Guit2==0] <- ''
  Guit2[Guit2=='TRUE'] <- 'x'

  ##Nicer way of showing the fretboard positions
  tt <- gridExtra::ttheme_default(base_size = 20)
  t1 <- gridExtra::tableGrob(Guit2, theme = tt)
  title <- grid::textGrob(paste("Key =", Key,", ", "Scale =", Label, sep=' '),
                          gp=grid::gpar(fontsize=16))
  padding <- grid::unit(2,"mm")

  table <- gtable::gtable_add_rows(t1,
                                   heights = grid::grobHeight(title) + padding,
                                   pos = 0)
  table <- gtable::gtable_add_grob(table, title, 1, 1, 1, ncol(table))

  grid::grid.newpage()
  grid::grid.draw(table)
}

