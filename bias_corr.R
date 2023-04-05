data(cccma)
set.seed(1)

# Univariate quantile mapping
qdm.c <- cccma$gcm.c*0
qdm.p <- cccma$gcm.p*0
for(i in seq(ncol(cccma$gcm.c))){
  fit.qdm <- QDM(o.c=cccma$rcm.c[,i], m.c=cccma$gcm.c[,i],
                 m.p=cccma$gcm.p[,i], ratio=cccma$ratio.seq[i],
                 trace=cccma$trace[i])
  qdm.c[,i] <- fit.qdm$mhat.c
  qdm.p[,i] <- fit.qdm$mhat.p
}
