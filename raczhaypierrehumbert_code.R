###############################################################################################################
# setting up
###############################################################################################################

# "condition" is the across-subject condition
# "correct" is the outcome variable. in exp4 test it isn't quite "correct"
# "gender" and "age" are participant variables
# "kill" and "kill2" are two outlier removal thresholds, at 5% and at 2.5%. use the latter.


# packages
library(languageR)
library(effects)
library(beanplot)
library(lme4)
library(plyr)
library(arm)
# install.packages("xtable")
library(xtable)
library(Hmisc)
# library(stargazer)
set.seed(1987)

# data

dat <- read.delim('frontierssipaperdata.csv', sep=',')

# set outlier threshold per condition

# training <- count(dat[dat$phase=="training",], c("condition","subject"))
# names(training)[3] <- "trialCount"
# upper <- data.frame(tapply(training$trialCount, training$condition, function(x) quantile(x, 0.975)))
# names(upper) <- "upper"
# upper$condition <- rownames(upper)
# training <- merge(training,upper)
# training$kill2 <- ifelse(training$trialCount > training$upper, T, F)
# # training <- subset(training, kill==F)
# # dat <- subset(dat, subject %in% training$subject)
# killList2 <- unique(training[,c("subject","kill2")])
# dat <- merge(dat,killList2)
# 
# people <- unique(dat$subject)
# peopleInventory <- dat[!duplicated(dat$subject), ]
# with(peopleInventory, table(kill2,condition))
# dat <- droplevels(subset(dat, kill2==F))
# people2 <- unique(dat$subject)
# pplRatio <- length(people2)/length(people)

# getting age right -- some on AMT give the current year as their year of birth

# dat$age <- ifelse(dat$yearOfBirth > 2000 | dat$yearOfBirth < 1900, NA, 2014-dat$yearOfBirth)
# dat$c.age <- mean(na.omit(dat$age))-dat$age

# # fixing coding for correct
# 
# dat$correct2 <- NA
# dat$correct2 <- ifelse (as.character(dat$correct) == "1" | as.character(dat$correct) == "TRUE",1,dat$correct2)
# dat$correct2 <- ifelse (as.character(dat$correct) == "0" | as.character(dat$correct) == "FALSE",0,dat$correct2)
# # dat$correct2 <- as.factor(dat$correct2)
# dat$correct <- dat$correct2
# dat$correct2 <- NULL

aggregate(age ~ experiment, dat ,mean)
aggregate(age ~ experiment, dat ,sd)

# how many people have missing age data
ages = aggregate(age ~ subject, dat, mean)
ages2 = aggregate(correct ~ subject, dat, mean)

###############################################################################################################
# subsetting, training length
###############################################################################################################
exp1 <- subset(dat, condition=="exp1")

exp1Test <- subset(exp1, phase=="test")
exp1TestNAomit <- subset(exp1, phase=="test" & gender %in% c('Male','Female') & !is.na(age))
exp1Training <- count(exp1[exp1$phase=="training",], "subject")
exp1Testav <- aggregate(correct ~ subject + inTraining + age, exp1Test, mean)

exp2 <- subset(dat, condition %in% c("exp2acc","exp2rel"))

exp2Test <- subset(exp2, phase=="test")
# exp2Test <- merge(paper1exp2perceptualdistance,exp2Test)
exp2TestNAomit <- subset(exp2, phase=="test" & gender %in% c('Male','Female') & !is.na(age))
exp2Training <- count(exp2[exp2$phase=="training",], c("cueSalient","subject"))
exp2Testav <- aggregate(correct ~ subject + inTraining + age + cueSalient + gender, exp2Test, mean)

exp12 <- subset(dat, condition %in% c("exp2acc","exp2rel","exp1"))
exp12Test <- subset(exp12, phase=="test")
exp12TestNAomit <- subset(exp12, phase=="test" & gender %in% c('Male','Female') & !is.na(age))
exp12Test$cueType <- as.character(exp12Test$cueSalient)
exp12Test$cueType <- ifelse(exp12Test$condition=="exp1","linguistic",exp12Test$cueType)
summary(as.factor(exp12Test$cueType))

exp3 <- subset(dat, condition %in% c("exp3acc","exp3rel"))

exp3Test <- subset(exp3, phase=="test")
exp3TestNAomit <- subset(exp3, phase=="test" & gender %in% c('Male','Female') & !is.na(age))
exp3Training <- count(exp3[exp3$phase=="training",], c("cueSalient","subject"))
exp3Testav <- aggregate(correct ~ subject + inTraining + age + cueSalient + gender, exp3Test, mean)

exp4 <- subset(dat, condition %in% c("exp4acc","exp4rel"))

exp4Test <- subset(exp4, phase=="test")
exp4TestNAomit <- subset(exp4, phase=="test" & gender %in% c('Male','Female') & !is.na(age))
exp4Training <- count(exp4[exp4$phase=="training",], c("cueSalient","subject"))
exp4Testav <- aggregate(rememberedName ~ subject + age + cueSalient + gender, exp4Test, mean)

exp53 <- subset(dat, condition %in% c("exp3acc","exp3rel","exp5acc","exp5rel"))
exp5 <- subset(dat, condition %in% c("exp5acc","exp5rel"))
exp5$bristol <- grepl('2016-', as.character(exp5$timestamp))
exp5Training <- count(exp5[exp5$phase=="training",], c("cueSalient","subject"))
exp5Test <- droplevels(subset(exp5, phase=='test'))
exp53Test <- subset(exp53, phase=="test")
exp5Testav <- aggregate(correct ~ subject + inTraining + age + cueSalient + gender, exp5Test, mean)
exp53TestNAomit <- subset(exp53, phase=="test" & gender %in% c('Male','Female') & !is.na(age))

exp65 <- subset(dat, condition %in% c("exp6acc","exp6rel","exp5acc","exp5rel"))
exp6 <- subset(dat, condition %in% c("exp6acc","exp6rel"))
exp6Training <- count(exp6[exp6$phase=="training",], c("cueSalient","subject"))
exp6Test <- subset(exp6, phase=="test")
exp65Test <- subset(exp65, phase=="test")
exp65TestNAomit <- subset(exp65, phase=="test" & gender %in% c('Male','Female') & !is.na(age))
exp6Testav <- aggregate(correct ~ subject + inTraining + age + cueSalient + gender, exp6Test, mean)

exp62 <- subset(dat, condition %in% c("exp6acc","exp6rel","exp5acc","exp5rel"))
exp62Test <- droplevels(subset(exp62, phase=='test'))


exp356 <- subset(dat, condition %in% c("exp3acc","exp3rel","exp5acc","exp5rel","exp6acc","exp6rel"))
exp356Test <- droplevels(subset(exp356, phase=='test'))

# expAge includes the long training experiment and that behaves differently
expAge2 <- subset(dat, condition %in% c("exp2acc","exp2rel","exp3acc","exp3rel","exp5acc","exp5rel"))
expAge2TestNAomit <- subset(expAge2, phase=="test" & !is.na(age))

## here come the individual experiments ##

###############################################################################################################
# exp1
###############################################################################################################

mean(exp1Training$freq)
sd(exp1Training$freq)

plot(density(exp1Training$freq))


paper1exp1fit <- glmer(correct ~ inTraining + age + (1 + inTraining | subject), data=exp1Test, family="binomial")
summary(paper1exp1fit)
xtable(summary(paper1exp1fit)$coef)

pdf("exp1test_intraining.pdf", height=6, width=6)
beanplot(exp1Testav$correct ~ exp1Testav$inTraining, ll = 0.01,
         main = "", side = "both", xlab="type of item", ylab="mean rate of subject accuracy",
         col = list("blue", c("red","darkblue")),
         axes=F, cutmin = 0, cutmax = 1)
axis(1, at=c(0.75,1.25),  labels=c("unseen items", "seen items"))
axis(2, at=c(0,0.25,0.5,0.75,1))
dev.off()

learner <- aggregate(correct ~ subject, exp1Test, mean)
learner$subjectLearner <- ifelse(learner$correct > mean(learner$correct), T, F)
learner <- learner[,c("subject","subjectLearner")]
summary(learner$subjectLearner)
exp1 <- merge(exp1,learner)
aggregate(correct ~ subjectLearner, exp1[exp1$phase=='test',], mean)

hist(aggregate(correct ~ subject, exp1Test, mean)$correct)

att.count1 <- count(exp1[exp1$phase=="training" & exp1$subjectLearner==T,], "subject")
inatt.count1 <- count(exp1[exp1$phase=="training" & exp1$subjectLearner==F,], "subject")
wilcox.test(jitter(att.count1$freq),jitter(inatt.count1$freq),alternative="two.sided") # W = xxx p < 0.0001

mean(att.count1$freq)
mean(inatt.count1$freq)

summary(learner$subjectLearner) 
20/nrow(learner)*100 # 46%

exp1Test <- subset(exp1, phase=="test")
exp1Testav <- aggregate(correct ~ subject + inTraining + subjectLearner + age, exp1Test, mean)
exp1Testav$subjectLearner2 <- ifelse(exp1Testav$subjectLearner == T, "learner", "non-learner")

pdf("exp1test_intraining_learners.pdf", height=6, width=6)
beanplot(exp1Testav$correct ~ exp1Testav$inTraining*exp1Testav$subjectLearner2, ll = 0.01,
         main = "good/poor learners, exp1", side = "both", xlab="type of subject", ylab="mean rate of subject correctness",
         col = list("blue", c("red","darkblue")),
         axes=T, cutmin = 0, cutmax = 1)
legend("bottomleft", c("seen items", "unseen items"), fill=c("red","darkblue"))
dev.off()


###############################################################################################################
# exp2
###############################################################################################################

mean(exp2Training$freq)
sd(exp2Training$freq)

wilcox.test(exp2Training$freq,exp1Training$freq,alternative="two.sided")
wilcox.test(exp2Training[exp2Training$cueSalient==F,]$freq,exp2Training[exp2Training$cueSalient==T,]$freq,alternative="two.sided")

wilcox.test(exp1Training$freq,exp2Training[exp2Training$cueSalient==T,]$freq,alternative="two.sided")

aggregate(freq ~ cueSalient, exp2Training, mean)
aggregate(freq ~ cueSalient, exp2Training, sd)

pdf("exp2traininglength.pdf", height=6, width=6)
#par(mfrow=c(2,1))
plot(density(exp2Training[exp2Training$cueSalient==T,]$freq, bw = 3.5), main="training length", ylim=c(0,0.05), col="red", xlab="")
abline(v=mean(exp2Training[exp2Training$cueSalient==T,]$freq), lty=2, col="red")
lines(density(exp2Training[exp2Training$cueSalient==F,]$freq, bw = 3.5),col="blue")
abline(v=mean(exp2Training[exp2Training$cueSalient==F,]$freq), col="blue", lty=2)
legend("topright", c("gender","view"), lty = 1, col=c("red","blue"), cex=0.8)
dev.off()

pdf("exp2test_personCue.pdf", height=6, width=6)
beanplot(exp2Testav$correct ~ exp2Testav$cueSalient, ll = 0.01,
         main = "", side = "both", xlab="type of item", ylab="mean rate of participant accuracy",
         col = list("blue", c("red","darkblue")),
         axes=F, cutmin = 0, cutmax = 1, log = "")
axis(1, at=c(0.75,1.25),  labels=c("view cue", "gender cue"))
axis(2, at=c(0,0.25,0.5,0.75,1))
dev.off()


paper1exp2fit <- glmer(correct ~ age + cueSalient + (1|subject), data=exp2Test, family="binomial")
summary(paper1exp2fit)
xtable(summary(paper1exp2fit)$coef)

exp2Test2 <- merge(exp2Test,exp2Training)
with(exp2Test2, somers2(freq, correct)["C"])

learner2 <- aggregate(correct ~ subject + cueSalient, exp2Test, mean)
learner2$subjectLearner2 <- ifelse(learner2$correct > mean(learner2$correct), T, F)
learner2 <- learner2[,c("subject","subjectLearner2","cueSalient")]
with(learner2, table(subjectLearner2,cueSalient))
exp2 <- merge(exp2,learner2)

hist(aggregate(correct ~ subject, exp2Test, mean)$correct)

att.count2 <- count(exp2[exp2$phase=="training" & exp2$subjectLearner2==T,], "subject")
inatt.count2 <- count(exp2[exp2$phase=="training" & exp2$subjectLearner2==F,], "subject")
wilcox.test(jitter(att.count2$freq),jitter(inatt.count2$freq),alternative="two.sided")

with(learner2, table(subjectLearner2,cueSalient))

mean(exp2Test[exp2Test$cueSalient==T,]$correct)
mean(exp1Test$correct)



exp12 <- subset(dat, condition %in% c("exp2acc","exp2rel","exp1"))
exp12Test <- subset(exp12, phase=="test")
exp12TestNAomit <- subset(exp12, phase=="test" & gender %in% c('Male','Female') & !is.na(age))
exp12Test$cueType <- as.character(exp12Test$cueSalient)
summary(as.factor(exp12Test$cueType))
exp12Test$cueType <- ifelse(exp12Test$condition=="exp1","linguistic",exp12Test$cueType)
exp12Test$ItemSeenInTraining = as.factor(exp12Test$inTraining)
exp12Test$ItemSeenInTraining = revalue(exp12Test$ItemSeenInTraining, c("TRUE"="in_training", "FALSE"="not_in_training"))
exp12Test$cueType = as.factor(exp12Test$cueType)
exp12Test$cueType <- revalue(exp12Test$cueType, c("FALSE"="view_cue", "linguistic"="linguistic_cue", "TRUE"="gender_cue"))
exp12Test$cueType = factor(exp12Test$cueType, levels=c("view_cue","gender_cue","linguistic_cue"))

exp12regressionb <- glmer(correct ~ cueType * ItemSeenInTraining + (1|subject), data=exp12Test, family="binomial")
summary(exp12regressionb)

plot(effect("cueType:ItemSeenInTraining", exp12regressionb))
pdf("exp12cuetype_intraining.pdf",height=5,width=5)
plotLMER.fnc(exp12regressionb, pred="cueType", intr=list("ItemSeenInTraining", c("in_training", "not_in_training"), "end"), addlines=T, cex=1, ylim=c(0.5,1))
dev.off()

idok = as.numeric(NULL)
emberek = unique(exp2$subject)
j=1
for ( i in unique(exp2$subject)){
ember = droplevels(subset(exp2, subject==i))
ember = subset(ember, phase=='test')
ido = max(as.numeric(ember$timestamp))-min(as.numeric(ember$timestamp))
idok[j] = ido
j=j+1
}

elet = data.frame(cbind(as.character(emberek),idok))
names(elet) = c('subject','testTime')
elet2 = merge(elet,learner2)
elet2$testTime = as.numeric(elet2$testTime)
plot(density(elet2[elet2$subjectLearner2==T,]$testTime),col='red')
lines(density(elet2[elet2$subjectLearner2==F,]$testTime),col='blue')
wilcox.test(jitter(elet2[elet2$subjectLearner2==T,]$testTime),jitter(elet2[elet2$subjectLearner2==F,]$testTime))

idok = as.numeric(NULL)
emberek = unique(exp2$subject)
j=1
for ( i in unique(exp2$subject)){
ember = droplevels(subset(exp2, subject==i))
ember = subset(ember, phase=='training')
ido = max(as.numeric(ember$timestamp))-min(as.numeric(ember$timestamp))
idok[j] = ido
j=j+1
}

elet = data.frame(cbind(as.character(emberek),idok))
names(elet) = c('subject','testTime')
elet2 = merge(elet,learner2)
elet2$testTime = as.numeric(elet2$testTime)
plot(density(elet2[elet2$subjectLearner2==T,]$testTime),col='red')
lines(density(elet2[elet2$subjectLearner2==F,]$testTime),col='blue')
wilcox.test(jitter(elet2[elet2$subjectLearner2==T,]$testTime),jitter(elet2[elet2$subjectLearner2==F,]$testTime))
aggregate(testTime ~ subjectLearner2, elet2, mean)
###############################################################################################################
# exp3
###############################################################################################################

mean(exp3Training$freq)
sd(exp3Training$freq)

wilcox.test(exp3Training$freq,exp2Training$freq,alternative="two.sided")
wilcox.test(jitter(exp3Training[exp3Training$cueSalient==F,]$freq),jitter(exp3Training[exp3Training$cueSalient==T,]$freq),alternative="two.sided")

pdf("exp3traininglength.pdf", height=6, width=6)
#par(mfrow=c(2,1))
plot(density(exp3Training[exp3Training$cueSalient==T,]$freq, bw = 3.5), main="training length", ylim=c(0,0.05), col="red", xlab="")
abline(v=mean(exp3Training[exp3Training$cueSalient==T,]$freq), lty=2, col="red")
lines(density(exp3Training[exp3Training$cueSalient==F,]$freq, bw = 3.5),col="blue")
abline(v=mean(exp3Training[exp3Training$cueSalient==F,]$freq), col="blue", lty=2)
legend("topright", c("gender","view"), lty = 1, col=c("red","blue"), cex=0.8)
dev.off()

pdf("exp3test_genderCue.pdf", height=6, width=6)
beanplot(exp3Testav$correct ~ exp3Testav$cueSalient, ll = 0.01,
         main = "", side = "both", xlab="type of item", ylab="mean rate of participant accuracy",
         col = list("blue", c("red","darkblue")),
         axes=F, cutmin = 0, cutmax = 1, log = "")
axis(1, at=c(0.75,1.25),  labels=c("view cue", "gender cue"))
axis(2, at=c(0,0.25,0.5,0.75,1))
dev.off()

paper1exp3fit <- glmer(correct ~ cueSalient + (1|subject), data=exp3Test, family="binomial", na.action=na.omit)

summary(paper1exp3fit)
xtable(summary(paper1exp3fit)$coef)

learner3 <- aggregate(correct ~ subject + cueSalient, exp3Test, mean)
learner3$subjectLearner <- ifelse(learner3$correct > mean(learner3$correct), T, F)
learner3 <- learner3[,c("subject","subjectLearner","cueSalient")]
with(learner3, table(subjectLearner,cueSalient))

learner3b = merge(exp3Training,learner3)
aggregate(freq ~ subjectLearner, learner3b, mean)
wilcox.test(learner3b[learner3b$subjectLearner==T,]$freq,learner3b[learner3b$subjectLearner==F,]$freq)
###############################################################################################################
# exp4
###############################################################################################################

mean(exp4Training$freq)
sd(exp4Training$freq)

wilcox.test(exp4Training$freq,exp2Training$freq,alternative="two.sided")
wilcox.test(exp4Training$freq,exp3Training$freq,alternative="two.sided")
wilcox.test(jitter(exp4Training[exp4Training$cueSalient==F,]$freq),jitter(exp4Training[exp4Training$cueSalient==T,]$freq),alternative="two.sided")

pdf("exp4traininglength.pdf", height=6, width=6)
#par(mfrow=c(2,1))
plot(density(exp4Training[exp4Training$cueSalient==T,]$freq, bw = 3.5), main="training length", ylim=c(0,0.05), col="red", xlab="")
abline(v=mean(exp4Training[exp4Training$cueSalient==T,]$freq), lty=2, col="red")
lines(density(exp4Training[exp4Training$cueSalient==F,]$freq, bw = 3.5),col="blue")
abline(v=mean(exp4Training[exp4Training$cueSalient==F,]$freq), col="blue", lty=2)
legend("topright", c("person","gender"), lty = 1, col=c("red","blue"), cex=0.8)
dev.off()

aggregate(rememberedName ~ cueSalient, exp4Test, mean)
summary(exp4Test$rememberedName)

# .57
# binom.test(771,(771+1053),0.5)

pdf("exp4test_personCue.pdf", height=6, width=6)
beanplot(exp4Testav$rememberedName ~ exp4Testav$cueSalient, ll = 0.01,
         main = "", side = "both", xlab="type of item", ylab="mean rate of participant choosing base",
         col = list("blue", c("red","darkblue")),
         axes=F, cutmin = 0, cutmax = 1, log = "")
axis(1, at=c(0.75,1.25),  labels=c("view cue", "gender cue"))
axis(2, at=c(0,0.25,0.5,0.75,1))
dev.off()


paper1exp4fit = glmer(rememberedName ~ cueSalient + (1|subject), data=exp4Test, family=binomial)

summary(paper1exp4fit)
xtable(summary(paper1exp4fit)$coef)

learner4 <- aggregate(rememberedName ~ subject + cueSalient, exp4Test, mean)
learner4$subjectLearner <- ifelse(learner4$rememberedName > mean(learner4$rememberedName), T, F)
learner4 <- learner4[,c("subject","subjectLearner","cueSalient")]
with(learner4, table(subjectLearner,cueSalient)) # but this means sg slightly different

# object people and people people

stem <- aggregate(rememberedName ~ subject + cueSalient, exp4Test, mean)
stem$subject.stem <- ifelse(stem$rememberedName > mean(stem$rememberedName), T, F)
stem <- stem[,c("subject","subject.stem","cueSalient")]
exp4 <- merge(exp4,stem)

stem.count2 <- count(exp4[exp4$phase=="training" & exp4$subject.stem==T & exp4$cueSalient==T,], "subject")
suffix.count2 <- count(exp4[exp4$phase=="training" & exp4$subject.stem==F & exp4$cueSalient==T,], "subject")

pdf("exp4test_stem2.pdf", height=6, width=6)
#par(mfrow=c(2,1))
plot(density(stem.count2$freq, bw = 3.5), main="training length (person cue)", xlab="", ylim=c(0,0.05), xlim=c(0,100), col="red")
abline(v=mean(stem.count2$freq), col="red", lty=2)
lines(density(suffix.count2$freq, bw = 3.5), col="blue")
abline(v=mean(suffix.count2$freq), col="blue", lty=2)
legend("topright", c("picks stem in test","picks suffix in test"), col = c("red","blue"), lty=1, cex=0.8)
dev.off()


###############################################################################################################
# exp5 (alongside 3)
###############################################################################################################

wilcox.test(exp3Training$freq,exp5Training$freq,alternative="two.sided")

wilcox.test(exp3Training[exp3Training$cueSalient==T,]$freq,exp5Training[exp5Training$cueSalient==T,]$freq,alternative="two.sided")
wilcox.test(exp3Training[exp3Training$cueSalient==F,]$freq,exp5Training[exp5Training$cueSalient==F,]$freq,alternative="two.sided")

wilcox.test(exp5Training[exp5Training$cueSalient==F,]$freq,exp5Training[exp5Training$cueSalient==T,]$freq,alternative="two.sided")




paper1exp5testfit <- glmer(correct ~ cueSalient + (1|subject), data=exp5Test, family="binomial")
xtable(summary(paper1exp5testfit)$coef)

paper1exp35fit <- glmer(correct ~ cueSalient + (1|subject), data=exp53Test, family="binomial")
summary(paper1exp35fit)
xtable(summary(paper1exp35fit)$coef)

exp5Testav <- aggregate(correct ~ subject + cueSalient, exp5Test, mean)

pdf("exp5test_personCue.pdf", height=6, width=6)
beanplot(exp5Testav$correct ~ exp5Testav$cueSalient, ll = 0.01,
         main = "", side = "both", xlab="type of item", ylab="mean rate of participant accuracy",
         col = list("blue", c("red","darkblue")),
         axes=F, cutmin = 0, cutmax = 1, log = "")
axis(1, at=c(0.75,1.25),  labels=c("view cue", "gender cue"))
axis(2, at=c(0,0.25,0.5,0.75,1))
dev.off()

###############################################################################################################
# exp6 (alongside 5)
###############################################################################################################

wilcox.test(exp6Training[exp6Training$cueSalient==T,]$freq,exp6Training[exp6Training$cueSalient==F,]$freq,alternative="two.sided")

paper1exp6fit <- glmer(correct ~ cueSalient + (1|subject), data=exp6Test, family='binomial')
xtable(summary(paper1exp6fit)$coef)


exp62Test$trainingLength = factor(exp62Test$trainingLength, levels=c("6items",'18items'))
exp62Test$training.length = exp62Test$trainingLength
exp62Test$cue.type = ifelse(exp62Test$cueSalient==T, 'gender', 'view')
exp62Test$cue.type <- as.factor(exp62Test$cue.type)
# paper1exp62fit <- glmer(correct ~ trainingLength * cueSalient + (1|subject), data=exp62Test, family="binomial")
paper1exp62fit <- glmer(correct ~ training.length * cue.type + (1|subject), data=exp62Test, family="binomial")
xtable(summary(paper1exp62fit)$coef)

pdf("exp6test_personCue.pdf", height=6, width=6)
beanplot(exp6Testav$correct ~ exp6Testav$cueSalient, ll = 0.01,
         main = "", side = "both", xlab="type of item", ylab="mean rate of participant accuracy",
         col = list("blue", c("red","darkblue")),
         axes=F, cutmin = 0, cutmax = 1, log = "")
axis(1, at=c(0.75,1.25),  labels=c("view cue", "gender cue"))
axis(2, at=c(0,0.25,0.5,0.75,1))
dev.off()

learner6 <- aggregate(correct ~ subject + cueSalient, exp6Test, mean)
learner6$subjectLearner <- ifelse(learner6$correct > mean(learner6$correct), T, F)
learner6 <- learner6[,c("subject","subjectLearner","cueSalient")]

with(learner2, table(subjectLearner2,cueSalient)) # of course
with(learner6, table(subjectLearner,cueSalient))

pdf('exp6interaction.pdf', height=4, width=4)
plot(effect('training.length:cue.type', paper1exp62fit))
dev.off()



######################################################################################
# discussion

# for exp 2-3-5


paper1ageeffects <- glmer(correct ~ cueSalient + age + (1|subject), data=expAge2TestNAomit, family=binomial)
xtable(summary(paper1ageeffects)$coef)

plot(effect("c.age",fit6))
plot(effect("c.age:gender",paper1exp2fit))


