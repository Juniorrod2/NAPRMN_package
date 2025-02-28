library(PepsNMR); library(NAPRMN)


zebrafish_fid <- ReadFids("C:/Users/igorj/Documents/Tássia/Zeca/Zebrafish - reteno", subdirs = TRUE, dirs.names = TRUE)


fid <- zebrafish_fid$Fid_data
info <- zebrafish_fid$Fid_info

fid_proc <- GroupDelayCorrection(fid,info)
fid_proc <- SolventSuppression(fid_proc,lambda.ss = 1e6)


spec <- FourierTransform(fid_proc,info)

#--------------------------------------------------------------

library(NMRphasing)

autophase <- function(spectra,phasingMethod="SPC_AAM",absorptionOnly = F,withBC = F){
  
  for(i in 1:dim(spectra)[1]){
    spec <- spectra[i,]
    phased_spec <-NMRphasing(specDatIn = spec,method = phasingMethod,absorptionOnly = absorptionOnly,withBC = withBC)
    spectra[i,] <- phased_spec
  }
  
  return(spectra)
}

phased_spec <- autophase(spec,withBC = T)

phased_spec <- InternalReferencing(phased_spec,info)
plot_interactive_Spectra(phased_spec)


phased_spec <- BaselineCorrection(phased_spec)
plot_interactive_Spectra(phased_spec,plot_only = 1:17)


plot_interactive_Spectra(spec)




compare_phasing_methods <- function(spectra,fid_info,ref=1,absorptionOnly = F,withBC = F,
                                    phasingMethods=c("NLS","MPC_DAOM","MPC_EMP", "MPC_AAM","MPC_DSM", 
                                                     "MPC_ADSM", "SPC_DAOM", "SPC_EMP", "SPC_AAM", 
                                                     "SPC_DSM", "SPC_ADSM")){
  spec <- spectra[ref,]
  phasingMatrix <- ZeroOrderPhaseCorrection(spec)

  
  for(i in phasingMethods){
    assign(i,NMRphasing(specDatIn = spec,method = i,absorptionOnly = absorptionOnly,withBC = withBC))
    phasingMatrix <- rbind(phasingMatrix,eval(parse(text = i)))
  }

  rownames(phasingMatrix) <- c("PepsNMR",phasingMethods)
  phasingMatrix <- PepsNMR::InternalReferencing(phasingMatrix,Fid_info = fid_info)
  return(phasingMatrix)
}


phased <- compare_phasing_methods(spec,info)

plot_interactive_Spectra(phased,plot_only = 1:12)
#---------------------------------------------------------------

spec <- ZeroOrderPhaseCorrection(spec)
spec <- InternalReferencing(spec,info)

