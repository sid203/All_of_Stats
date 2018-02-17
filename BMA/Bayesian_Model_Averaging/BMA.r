

# Install the Necessary Packages ------------------------------------------
#source("https://bioconductor.org/biocLite.R")
#biocLite("iterativeBMAsurv")
# First Phase -------------------------------------------------------------
library(BMA)
library(iterativeBMAsurv)
data(trainData)          #dataframe of medical covarites/Genes
data(trainSurv)          #Survival time for a patient cooresponding to its medical covariatesGenes
data(trainCens)         # Logical Value, indicates whether the information concerning a patient was censored or not
ret.list <- iterateBMAsurv.train.wrapper (x=trainData, 
                                          surv.time=trainSurv, cens.vec=trainCens)
                        #function to train the genes Data so that it can be used later on to predict the survival time of a patient

ret.bic.surv <- ret.list$obj
gene.names <- ret.list$curr.names
top.gene.names <- gene.names[ret.bic.surv$probne0 > 0]

top.gene.names   #top Genes to have a lot of effect on survival time of a patient 
ret.bic.surv$postprob #posterior prob of the models selected

ret.bic.surv$namesx 

ret.bic.surv$which #indicates which variable in in which model

ret.bic.surv$mle #MLE for each coefficient of each model



ret.bic.surv$probne0 

# Second Phase ------------------------------------------------------------
data(testData)
data(testSurv)
data(testCens)
curr.test.dat <- testData[, top.gene.names]
y.pred.test <- apply (curr.test.dat, 1, 
                      predictBicSurv, postprob.vec=ret.bic.surv$postprob,
                      mle.mat=ret.bic.surv$mle)

               #•predict the risk for test samples
             
y.pred.train <- apply (trainData[, top.gene.names], 1, 
                      predictBicSurv, postprob.vec=ret.bic.surv$postprob,
                      mle.mat=ret.bic.surv$mle)
               
               # predict the risk for train samples

ret.table <- predictiveAssessCategory (y.pred.test, y.pred.train, testCens, cutPoint=50)

risk.vector <- ret.table$groups

risk.table <- ret.table$assign.risk

# Third Phase -------------------------------------------------------------

mySurv.obj <- Surv(testSurv, testCens) #create a survival object from the test set




stats <- survdiff(mySurv.obj ~ unlist(risk.vector))

stats

complete_survival_analysis=iterateBMAsurv.train.predict.assess(train.dat =trainData ,
                                    test.dat = testData,
                                    surv.time.train = trainSurv,
                                    surv.time.test = testSurv,
                                    cens.vec.train = trainCens,
                                    cens.vec.test = testCens,
                                    p=10,
                                    nbest = 5)
number.genes = complete_survival_analysis$nvar
number.models = complete_survival_analysis$nmodel
evaluate.success = complete_survival_analysis$success
evaluate.success


cv =crossVal (exset=trainData, survTime=trainSurv, censor=trainCens, 
              diseaseType="DLBCL", noRuns = 3)



imageplot.iterate.bma.surv(ret.bic.surv)
#Best Models selected, each model is like a column, and it includes the 
# genes /variables/medical covariates which perform best according to BMA

