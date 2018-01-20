table = data.frame(
  A= c(0,1,0,1,1,0,1,0,0,1),
  B= c(0,0,1,0,0,0,1,0,1,1),
  C= c(1,1,0,0,1,1,0,0,0,1),
  class = c(FALSE,TRUE,FALSE,FALSE,TRUE,TRUE,FALSE,FALSE,TRUE,TRUE)
)
table$class

pA1gP = sum(table$A==1 & table$class)/sum(table$class)


pB0gP = mean( (table$B == 0)[table$class] )


pC1gP = mean( table$C[ table$class ] == 1 )



pA1gN = mean( table$A[!table$class] == 1 )
pB0gN = mean( table$B[!table$class] == 0 )
pC1gN = mean( table$C[!table$class] == 1 )




# "full" Bayesian formula:
#
#                       P(A=1,B=0,C=1 | +) P(+)
# P(+ | A=1,B=0,C=1) == -----------------------
#                       P(A=1,B=0,C=1)
#
# eg mean( table$label[ table$A==1 & table$B==0 & table$C==1 ] )
#
# However usually sum( table$A==1 & table$B==0 & table$C==1 ) is small.
#
#
#
# Instead, compare P(A=1,B=0,C=1 | +)P(+) vs P(A=1,B=0,C=1 | -)P(-)
# and also treat the condition as independent


prPos = mean( table$class == TRUE )
prNeg = mean( table$class == FALSE )

# Comparison Value 1:
# P(A=1,B=0,C=1 | +)P(+) -> P(A=1|+)P(B=0|+)P(C=1|+) P(+)
propPos = pA1gP*pB0gP*pC1gP * prPos

# Comparison Value 2:
# P(A=1,B=0,C=1 | -)P(-) -> P(A=1|-)P(B=0|-)P(C=1|-) P(-)
propNeg = pA1gN*pB0gN*pC1gN * prNeg



predictionPositive = propPos >= propNeg
predictionStrength = propPos/(propPos+propNeg)


sprintf("We predict the label as %s (%f)",
        if(predictionPositive) "Positive" else "Negative",
        predictionStrength
        )

