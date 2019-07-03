# Chapter 1.3
{
  F1.3 = new.env()
  
  # Outfits possibilities with numWays1 tops and numWays2 bottoms
  F1.3$NumWays = function(numWays1, numWays2) {
    return(numWays1 * numWays2)
  }
  
  # Contents of bowl of frozen yogurt with numBinChoices possible flavors
  # This includes not choosing any option; e.g. bowl of nothing
  F1.3$NumBinOptions = function(numBinChoices) {
    return(2**numBinChoices)
  }
  
  # 3! = 3 * 2 * 1
  F1.3$Factorial = function(number) {
    if (number == 0) {
      return(1)
    }
    else if (number < 0) {
      return(0)
    }
    bar = 1
    for (i in 0:(number-1))
    {
      bar = bar * (number - i)
    }
    return(bar)
  }
  
  # Number of possible seating arrangements with numSelect seats and numPool people
  # numPool >= numSelect
  F1.3$Permutation = function(numPool, numSelect) {
    if (numSelect == numPool || numSelect == 0) {
      return(1)
    }
    else if (numSelect < 0 || numSelect > numPool) {
      return(0)
    }
    barNumerator = F1.3$Factorial(numPool)
    barDenominator = F1.3$Factorial(numPool - numSelect)
    return (barNumerator / barDenominator)
  }
  
  # Card hand of numSelect cards from a deck of numPool cards
  F1.3$Combination = function(numPool, numSelect){
    if (numSelect == numPool || numSelect == 0) {
      return(1)
    }
    else if (numSelect < 0 || numSelect > numPool) {
      return(0)
    }
    barNumerator = F1.3$Permutation(numPool, numSelect)
    barDenominator = F1.3$Factorial(numSelect)
    return(barNumerator / barDenominator)
  }
  
  # Rolling numSelect dice that each have numPool sides
  F1.3$Permutation.Replace = function(numPool, numSelect) {
    if (numPool == 0 || numSelect == 0) {
      return(1)
    }
    else if (numSelect < 0 || numPool < 0) {
      return(0)
    }
    return(numPool**numSelect)
  }
  
  # Recording how many of numSelect people go to see numPool movies
  # This will not be tested on, despite being the concept of 1.3 Problem 1
  F1.3$Combination.Replace = function(numPool, numSelect) {
    return(F1.3$Combination(numPool + numSelect - 1, numSelect))
  }
  
}