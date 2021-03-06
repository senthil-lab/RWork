
set.seed(1, sample.kind = "Rounding")

disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))


test <- rep(NA, 1e6)

test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))

test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

length(disease)
mean(disease)

mean(test==1)

sum(disease[which(test==0)]==1)

mean(disease[which(test==0)]==1)

mean(disease[test==0]==1)
mean(disease[test==0])

mean(disease[which(test==1)]==1)

mean(disease==1)

sum(disease[which(test==0)]==1)

(sum(disease[which(test==1)]==1)/length(disease[which(test==1)]==1))/mean(disease==1)
