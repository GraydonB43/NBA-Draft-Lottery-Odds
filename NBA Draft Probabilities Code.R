library(tidyverse)

probtable <- as_tibble(data.frame(
  Seed = 1:16,
  Chances = c(114,113,112,111,99,89,79,69,59,49,39,29,19,9,6,4)
))

# first five picks - draw five balls...
# first pick is straightforward
probtable$Pick1 <- probtable$Chances/sum(probtable$Chances)
probtable$Pick2 <- 0
probtable$Pick3 <- 0
probtable$Pick4 <- 0
probtable$Pick5 <- 0
remainingProbs <- matrix(0,ncol=11,nrow=16)
findFixedTeam <- function(probs,remainingProbs,restProbs) {
  for(z in 1:11) {
    nonzeros <- which(probs!=0)
    team <- min(nonzeros)
    remainingProbs[team,z] <- remainingProbs[team,z] + restProbs
    probs[team] <- 0
  }
  return(remainingProbs)
}

# assume seed i have been picked
for(i in probtable$Seed) {
  chances <- probtable$Chances
  chances[i] <- 0
  conditionalprobs <- probtable$Pick1[i] * chances/sum(chances)
  probtable$Pick2 <- probtable$Pick2 + conditionalprobs
  
  for(j in probtable$Seed) {
    if(i!=j) {
      chances2 <- chances
      chances2[j] <- 0
      conditionalprobs2 <- conditionalprobs[j] * chances2/sum(chances2)
      probtable$Pick3 <- probtable$Pick3 + conditionalprobs2
      
      for(k in probtable$Seed) {
        if(i!=k & j!=k) {
          chances3 <- chances2
          chances3[k] <- 0
          conditionalprobs3 <- conditionalprobs2[k] * chances3/sum(chances3)
          probtable$Pick4 <- probtable$Pick4 + conditionalprobs3
          
          for(l in probtable$Seed) {
            if(i!=l & j!=l & k!=l) {
              chances4 <- chances3
              chances4[l] <- 0
              conditionalprobs4 <- conditionalprobs3[l] * chances4/sum(chances4)
              probtable$Pick5 <- probtable$Pick5 + conditionalprobs4
              
              for(m in probtable$Seed) {
                if(i!=m & j!=m & k!=m & l!= m) {
                  restProbs <- conditionalprobs4[m]
                  chances5 <- chances4
                  chances5[m] <- 0
                  remainingProbs <- findFixedTeam(chances5,remainingProbs,restProbs)
                }
              }
            }
          }
        }
      }
    }
  }
}

probtable
apply(probtable,2,sum)
remainingProbs
apply(remainingProbs,2,sum)
