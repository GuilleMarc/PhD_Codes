##############################################################
## Snippets adapted from the original code in my first PhD project
## This project was a meta-analysis of 60 US studies to analyze the
## impact of cover crops on corn yields.
## Yield effect = mean + error_paper + X(management_cover crop) + error_between_papers + error
## Yield effect= log-(Yield_cover - Yield_no_cover)/(yield_no_cover)
## Required: metawcc.csv (This file is included in the master)
################################################################

require(metafor)                # Should load up every library required

# Computing effect sizes and variances
# ROM refers to the log transformed ratio of means (Hedges 1999), Eq (1), Eq(2),Eq(3) in FEM
# This can be done from scratch, no need of the escalc function in metafor.

metawcc<- escalc(n1i = N.exp., n2i = N.control.,
                 m1i = yieldcover, m2i = yieldcontrol,
                 sd1i=SD.EXP., sd2i=SD.CON.,
                 data=metawcc, measure= "ROM",
                 append=TRUE, na.rm=TRUE)

metawcc$yi<- as.numeric(metawcc$yi)      #make size effects numeric,
metawcc$vi<-as.numeric(metawcc$vi)

# ------------------1) Whole dataset: Homogenenity tests-------------------
# If significant, need to explain why is not homogeneous (~include covariates)
# It's just a weighted mean for each period
# B,Q,tau2,I2,K are measures to account variability
hom<-function(data){
  ma.H<-rma(yi,vi,data=data,method="DL")
  res.H<-t(as.vector(ma.H[c(exp(as.numeric(ma.H[[1]])),18,7,8,22,11)])) #B,Q,tau2,I2,K,
  no.studies<-length(unique(data[,2]))
  rh<-data.frame(res.H,no.studies)
  return(rh)
}

ma.F<-hom(subset(metawcc,metaA=="F"))                 #papers (1965-2004)
ma.G<-hom(subset(metawcc,metaA=="G"))                 #papers (2005-2015)
ma.FG<-hom(metawcc)                                   #Whole thing

w.coefs<-as.numeric(c(ma.F$b, ma.G$b, ma.FG$b))
w.vars<-as.numeric(c(ma.F$vb, ma.G$vb, ma.FG$vb))
labels<-c('1965-2004','2005-2015','1965-2015')        #labels

# Forest plots are great to visualize a distribution of mean-effects in a meta-analysis:

png(filename="MeanYResponseByYear",res=120, width=700, height=600, type="cairo")
forest(w.coefs, w.vars, slab=labels,atransf = exp,    #back-tranform log-ratios
       main='Mean corn yield response to winter cover crops',
       xlab='Effect sizes, (Ycc/Ync)',
       alim=c(-0.10,0.25),psize=2, cex=1.3, cex.axis=1.2,
       pch=16,cex.lab=1.3, ylim=c(-5,20),rows=c(-2,7,15),
       annotate=TRUE)                                 ##change order
dev.off()
# ----------------2) Whole dataset + controls ----------------------------------------
# Data not homogeneous
# Example showing just 2 controls (can add as many as needed...respect stat principles though!)
# at least one is/should be significant

## try the covariate "Tillage"
het <-function(data){
  ma.till<- rma(yi~Tillage, data=data,vi, method="DL")
  bg.till<-data.frame( Moderator="Tillage",
                       t(anova.rma(ma.till)[c(1,2,4,6)]))

## try the covariate "Cover crop species"
  ma.type<- rma(yi~Type, data = data,vi,method = "DL")
  bg.type<- data.frame( Moderator='WCC species',
                        t(anova.rma(ma.type)[c(1,2,4,6)]))

  return(rbind(bg.till,bg.type)
}

het.all<-het(metawcc)

# ----------------2) Re-run metaA on significant controls -------------------------------
# basically do a new metaA at each level of the significant control
# example shown for species (found significant in step 2)

# is subset legumes/grass/biculture homogeneous?---see lines 19:22
ma.legume<-hom(subset(metawcc,Type=="legume"))
ma.grass<-hom(subset(metawcc,Type=="grass"))
ma.mixture<-hom(subset(metawcc,Type=="biculture"))

type.coefs<-as.numeric(c(ma.legume$b, ma.mixture$b, ma.grass$b))
type.vars<-as.numeric(c(ma.legume$vb, ma.mixture$vb, ma.grass$vb))
type.labels<-c('Legume','Mixture', 'Grass')

png(filename="MeanYResponseByType",res=120, width=700, height=600, type="cairo")
forest(type.coefs, type.vars, slab=type.labels, atransf = exp,
       xlab='RR, (Ycc/Ync)',
       alim=c(-0.3,0.70),psize=1.6, cex=1.3, cex.axis=1.2,
       pch=16,cex.lab=1.3, ylim=c(-5,20),rows=c(0,7,15),
       annotate=TRUE)
dev.off()

# What control affect each level in turn?-----see lines 47:50
het.leg<-het(subset(metawcc,Type=="legume"))          #(till*,region,NFR*)
het.gr<-het(subset(metawcc,Type=="grass"))            #(corn yield*)
het.mix<-het(subset(metawcc,Type=="biculture"))       #(KDate*,biomass sig*)

# ---------------3) Meta-regressions ---------------------------------------------------
# Methods above work nice for factors/categoricals (anova based methods)
# for continuous variables, regression based models are better
# technically, this is much like ancova
# depending on your assumptions, you may change models as needed---it's fun!!
# if variance is structured: consider mixed models
# if suspect of non-linearity in covariates: glm, gam, segmented regression,etc
# to control for different study variance effects: weighted regressions
# below is basically a weighted linear mixed effects model

nitro<-metawcc[,c("yi",'vi','Nitrogen','study','Type')]
nitro<- nitro[complete.cases(nitro),]                 #seems like a bug. Check, I'm lazy now!
nitspec<-groupedData(yi~Nitrogen|Type, data=nitro)

meta.lmer.N11<-lmer(exp(yi)~Type*Nitrogen+(1|Nitrogen/study), data=nitspec)  #anova
meta.lmer.N12<-lmer(exp(yi)~Type+Nitrogen:Type-1+(1|Nitrogen/study), data=nitspec) #for int & slopes

coefs.meta.N12<-summary(meta.lmer.N12)    #list with parameters
ci.meta.N12<-confint(meta.lmer.N12)

# my code for the regression plots is long and repetitive. To be updated.
