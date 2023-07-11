#' Govardovskii nomogram
#'
#' Calculate Govardovskii nomogram as per Govardovskii et al 2000 (Vis Neurosci. 2000 Jul-Aug;17(4):509-28. doi: 10.1017/s0952523800174036.)
#' @param lmax lambda max of the opsin's photon sensitivity in the absence of preceptoral filtering (nm)
#' @param wavelen a vector containing the wavelengths over which the nomogram is to be defined
#' @return raw a-opic action spectra (relative photon sensitivity as a function of wavelength); arbitrary units, no correction for prereceptoral filtering
#' @examples 
#' aspecp1 <- govardovskii(450, 300:800);
#' @export
govardovskii <- function(lmax,wavelen) {
  bband = 189 + (0.315*(lmax));
  fmaxa = 300000000000000000/lmax;
  fmaxb = 300000000000000000/bband;
  Aa = 69.7;
  Ab = 0.26;
  a = 0.8795+0.0459*(exp((-1*(lmax-300)^2)/11940));
  B = 28;
  b = 0.922;
  C = -14.9; 
  c = 1.104; 
  D = 0.674; 
  d = -40.5+(0.195*lmax);
  freq = 300000000000000000/wavelen;
  ffmax = freq/fmaxa;
  Sa = (exp(Aa*(a-ffmax))+exp(B*(b-ffmax))+exp(C*(c-ffmax))+D)^-1;
  Sb = Ab*exp(-1*((wavelen-bband)/d)^2);
  aspecp = Sa+Sb;
  return(aspecp);
}



#' Photon energy
#'
#' Calculates the energy of a photon at a given wavelengths (nm)
#' @param wavelen a vector containing the wavelengths
#' @return photon energy (W.s/photon.nm)
#' @examples 
#' e <- photonenergy(300:800);
#' @export
photonenergy <- function(wavelen) {
  wavelen = wavelen/1000000000; #wavelength in meter
  h = 6.62607E-34;
  c = 299800000;
  e = (h*c)/wavelen; #joules/photon
  return(e);
}




#' Prereceptoral filtering wavelength harmonizer
#'
#' Extrapolate/adjust species specific prereceptoral filtering data to match the wavelength range for action spectra definition
#' @param Pfilter existing transmission measurement data matrix with transmissions (%) and wavelengths (nm)
#' @param wavelen the wavelength range to extrapolate/restrict to
#' @return adjusted transmission data
#' @examples 
#' trans <- harmonisetrans(data.frame(wavelen = 350:750, trans = seq(0,100,length.out=401)), 300:800);
#' @export
harmonisetrans <- function(Pfilter,wavelen) {
  #extract existing transmission data
  wlp=Pfilter$wavelen;
  trans=Pfilter$trans;
  #adjust short wavelength end if required
  wdiffs=wlp[1]-wavelen[1];
  if (wdiffs>0) {
    #extrapolate to shorter wavelengths
    delta=((trans[3]-trans[2])+(trans[2]-trans[1]))/2; #average decrease first 3 measurements
    addata=(seq(wdiffs,1,-1)*-delta)+trans[1]; #extrapolated decrease across missing range
    addata[addata<0]=0; #minimum transmission is 0
    trans=c(addata,trans); #shift transmission data add extrapolation
    wlp=(wavelen[1]:wlp[length(wlp)]); #update wavelength range for transmission data
  } else if (wdiffs<0) { #remove some initial values
    trans=trans[(-wdiffs+1):length(trans)];
    wlp=wlp[(-wdiffs+1):length(wlp)];
  }
  #adjust long wavelength end if required
  wdiffl=wlp[length(wlp)]-wavelen[length(wavelen)];
  if (wdiffl<0) {
    #extrapolate to longer wavelengths
    trans[(length(trans)+1):(length(trans)-wdiffl)]=trans[length(trans)]; #assume no further change in transmission
    wlp=(wlp[1]:wavelen[length(wavelen)]); #update wavelength range for transmission data
  } else if (wdiffl>0) { #remove some data for longer wavelengths
    trans=trans[1:(length(trans)-wdiffl)];
    wlp=wlp[1:(length(wlp)-wdiffl)];
  }
  return(trans);
}



#' Opsin sensitivity wavelength harmonizer
#'
#' Extrapolate/adjust curve to match the wavelength range for species specific action spectra definition
#' @param Sens existing sensitivity measurement data matrix with photon sensitivities and wavelengths (nm)
#' @param wavelen the wavelength range to extrapolate/restrict to
#' @return adjusted sensitivtity curve
#' @examples
#' sens <- harmonisesens(data.frame(wavelen = 350:750, aspecp = govardovskii(450, 350:750)), 300:800);
#' @export
harmonisesens <- function(Sens,wavelen) {
  #extract existing transmission data
  w2=Sens$wavelen;
  ph=Sens$aspecp;
  #adjust short wavelength end if required
  wdiffs=w2[1]-wavelen[1];
  if (wdiffs>0) {
    #extrapolate to shorter wavelengths
    ph=c(rep(NA,wdiffs),ph); #shift ph data 
    w2=(wavelen[1]:w2[length(w2)]); #update wavelength range for ph data
  } else if (wdiffs<0) { #remove some initial values
    ph=ph[(-wdiffs+1):length(ph)];
    w2=w2[(-wdiffs+1):length(w2)];
  }
  #adjust long wavelength end if required
  wdiffl=w2[length(w2)]-wavelen[length(wavelen)];
  if (wdiffl<0) {
    #extrapolate to longer wavelengths
    ph[(length(ph)+1):(length(ph)-wdiffl)]=NA;
    w2=(w2[1]:wavelen[length(wavelen)]);
  } else if (wdiffl>0) { #remove some data for longer wavelengths
    ph=ph[1:(length(ph)-wdiffl)];
    w2=w2[1:(length(w2)-wdiffl)];
  }
  return(ph);
}




#' Alpha-opic action spectrum generator
#'
#' Generate a govardovski nomogram with corrections prereceptoral filtering
#' @param opsin target opsin name (One of the following options: 'Mel','Rod','Scone','Mcone','Lcone') or 'Photopic' human lux
#' @param lmax the lambda max of the opsin's photon sensitivity in the absence of preceptoral filtering. A string specifying a species specific work space in the subdirectory '/data' e.g. 'Mouse', or a numerical wavelength value specifying the lambda max to allow modelling
#' @param pfilter information about function for prereceptoral filtering. A string specifying a species specific work space in the subdirectory '/data' e.g. 'Mouse', zero for no prereceptoral filtering, or new transmission measurement data matrix with transmissions and wavelengths
#' @return list of opsin, harmonized wavelength range, prereceptoral transmission, action spectrum, photon action spectrum, a-opic efficacy of luminous radiation for daylight. aspec = normalized a-opic action spectra for relative energy sensitivity as a function of wavelength. aspecp = normalized a-opic action spectra for relative photon sensitivity as a function of wavelength. kavD65 = a-opic efficacy of luminous radiation for daylight (D65; (W/lm)) for conversion to a-opic EDI. wavelen = the wavelength range over which aspec is defined (be default 300 to 780nm). trans = the prereceptoral filtering correction used to adjust the curves.
#' @examples
#' as1 <- generateaopicactionspec('Photopic', 'Human', 'Human');
#' as2 <- generateaopicactionspec('Mel', 'Mouse', 'Mouse');
#' as3 <- generateaopicactionspec('Mel', 480, 'Mouse');
#' as4 <- generateaopicactionspec('Rod', 'Dog', data.frame(wavelen = 350:750, trans = seq(0,100,length.out=401)));
#' @export
generateaopicactionspec <- function(opsin,lmax,pfilter) {
  wavelen=(300:780);  #default wavelength range of 300 to 780nm
  #define spectral sensitivity as appropriate
  if (lmax>0 & length(lmax)==1 & is.numeric(lmax)) { #Lambda max model
    aspecp = govardovskii(lmax, wavelen); #relative photon sensitivity as a function of wavelength prior to correction for prereceptoral filtering
  } else if (is.character(lmax)) { #Use a predefined sensitivity
    data(SensRefData);
    Sens=SensRefData[[lmax]][[opsin]];
    #harmonise wavelength range for sensitivity measurements and action spectra
    aspecp=harmonisesens(Sens,wavelen);
  }
  #define correction for prereceptoral filtering as appropriate (%transmission across wavelengths)
  if (length(pfilter)==1 & is.numeric(pfilter)) {
    if (pfilter==0) { #No filter
      trans=rep(1,length(wavelen))*100;
    }
  } else if (is.character(pfilter)) { #Use a predefined filter
    data(TransRefData);
    Pfilter=TransRefData[[pfilter]];
    #harmonise wavelength range for transmission measurements and action spectra
    trans=harmonisetrans(Pfilter,wavelen);
  } else if (dim(pfilter)[2]==2) { #Use new input matrix (should be in 1nm resolution; first column is wavelegth and second column is transmission %)
    Pfilter=data.frame(wavelen=pfilter[,1], trans=pfilter[,2]);
    #harmonise wavelength range for transmission measurements and action spectra
    trans=harmonisetrans(Pfilter,wavelen);
  }
  #apply corrections to obtain corrected action spectra (e.g. Reference Human data aspecp already lens filtered)
  if (is.character(lmax)) {
    if (SensRefData[[lmax]][[opsin]]$noprf[1]=='No') {
      aspecp=aspecp;
      e = photonenergy(wavelen);
      aspec=aspecp/e;
    } else if (SensRefData[[lmax]][[opsin]]$noprf[1]=='Yes') {
      aspecp=aspecp*(trans/100); #correct a-opic action spectra for lens transmission
      e = photonenergy(wavelen); #extract photon energy across specified wavelength range
      aspec=aspecp/e; #convert nomogram from photon to energy sensitivity
    }
  } else if (lmax>0 & length(lmax)==1 & is.numeric(lmax)) {
    aspecp=aspecp*(trans/100); #correct a-opic action spectra for lens transmission
    e = photonenergy(wavelen); #extract photon energy across specified wavelength range
    aspec=aspecp/e; #convert nomogram from photon to energy sensitivity
  }
  #normalize final action spectra
  if (max(aspec, na.rm = T)>0) {
    aspec=aspec/max(aspec, na.rm = T);
    aspecp=aspecp/max(aspecp, na.rm = T);
  } 
  #calculate a-opic efficacy of luminous radiation for daylight (D65: (W/lm))
  #NOTE - photopic is restricted to wavelengths from 380 to 780, animal specific a-opic spectra may go to lower wavelengths
  data(VisualStandards);
  #extract D65 data and harmonise to wavelength range of the action spectra
  wl65=VisualStandards$D65$wavelen;
  sp65=VisualStandards$D65$spectra;
  sp65=sp65[wl65>=wavelen[1] & wl65<=wavelen[length(wavelen)]];
  #calculate kavD65 = alphaopic weighted irradiance of D65 divided by photopic illuminance
  kavD65=sum(sp65*aspec, na.rm = T)/VisualStandards$D65$lux;
  return(list(opsin=opsin, wavelen=wavelen, trans=trans, aspec=aspec, aspecp=aspecp, kavD65=kavD65));
}




#' Species specific a-opic calculator
#'
#' Calculate species specific a-opic EDIs (+ photopic illuminance), irradiances and effective photons from predefined species specific parameters
#' @param spd vector containing spectral power distribution in W/m2.nm
#' @param wl corresponding wavelength range over which measurements are acquired; assumes equally spaced spectra and uses spline extrapolation if not have 1nm spacing
#' @param species string containing the name of a species e.g. 'Mouse'
#' @param opsin target opsin name (One of the following options: 'Mel','Rod','Scone','Mcone','Lcone') or 'Photopic' human lux
#' @param lmax the lambda max of the opsin's photon sensitivity in the absence of preceptoral filtering. A string specifying a species specific work space in the subdirectory '/data' e.g. 'Mouse', or a numerical wavelength value specifying the lambda max to allow modelling
#' @param pfilter information about function for prereceptoral filtering. A string specifying a species specific work space in the subdirectory '/data' e.g. 'Mouse', zero for no prereceptoral filtering, or new transmission measurement data matrix with transmissions and wavelengths
#' @return list of species name, target opsin, EDI = a-opic equivalent daylight illuminance (in lux), IRR = a-opic irradiance (W/m2), PHO = Effective photon [log10(photons/cm2.s)]
#' @examples
#' #Wavelength range
#' wl = 300:780
#' #Spectral power distribution in W/m2.nm
#' spd = dnorm(300:780, mean = 480, sd = 30)
#' 
#' #If target species is available in the package data
#' exposure1 <- alphaopic(spd, wl, 'Mouse', 'Scone', 'Mouse', 'Mouse');
#' 
#' #If lambda max value of opsin's photon sensitivity will be used
#' exposure2 <- alphaopic(spd, wl, 'Cat', 'Mel', 480, 'Cat');
#' 
#' #If new lens transmission observations will be used
#' exposure3 <- alphaopic(spd, wl, 'Sheep', 'Rod', 500, data.frame(wavelen = 350:750, trans = seq(0,100,length.out=401)));
#' @export
alphaopic <- function(spd,wl,species,opsin,lmax,pfilter) {
  if (mean(diff(wl))!=1) { #not 1nm spacing, use spline extrapolation; note assumes equally spaced spectra values will be incorrect if this assumption is not met
    wlo=wl;
    wl=round(wlo[1]):round(wlo[length(wlo)]);
    spd=spline(wlo, spd, n = length(wl), method = 'natural')[['y']];
    spd=spd/mean(diff(wlo));
  }
  #Data availability check
  #Check if transmission already available
  data(TransRefData);
  data(SensRefData);
  #Check if opsin sensitivity already available
  if (is.character(lmax)) {
    print(paste0('Searching for ',lmax,' opsin photon sensitivity: '));
    if (is.character(lmax) & (lmax %in% names(SensRefData))) {
      if (opsin %in% names(SensRefData[[lmax]])) {
        print('Available opsin photon sensitivity data for this species');
      } else {
        print('No opsin photon sensitivity data for this species');
      }
    } else {
      print('No opsin photon sensitivity data for this species');
    }
  }
  #Check if lens transmission already available
  if (is.character(pfilter)) {
    print(paste0('Searching for ',pfilter,' lens transmission: '));
    if (is.character(pfilter) & (pfilter[1] %in% names(TransRefData))) {
      print('Available lens transmission data for this species');
    } else {
      print('No opsin photon sensitivity data for this species');
    }
  }
  #If no error in the file names continue
  #get action spectra and constants
  aopics <- generateaopicactionspec(opsin,lmax,pfilter);
  crv=aopics$aspec; #a-opic action spec
  crvp=aopics$aspecp; #a-opic action spec
  const=aopics$kavD65; #kavD65
  opsin=aopics$opsin; #opsin name
  wavelen=aopics$wavelen;
  #constrain spectra/measurements to a common range
  if (wl[1]<wavelen[1]) {
    i=which(wl==wavelen[1]);
    wl=wl[i:length(wl)];
    spd=spd[i:length(spd)];
  } else if (wl[1]>wavelen[1]) {
    i=which(wavelen==wl[1]);
    wavelen=wavelen[i:length(wavelen)];
    crv=crv[i:length(crv)];
    crvp=crvp[i:length(crvp)];
  }
  if (wl[length(wl)]>wavelen[length(wavelen)]) {
    i=which(wl==wavelen[length(wavelen)]);
    wl=wl[1:i];
    spd=spd[1:i];
  } else if (wl[length(wl)]<wavelen[length(wavelen)]) {
    i=which(wavelen==wl[length(wl)]);
    wavelen=wavelen[1:i];
    crv=crv[1:i];
    crvp=crvp[1:i];
  }
  #calculate aopic EDI, irradiance and effective photons
  e = photonenergy(wavelen); #W.s/photon for calculation of photon flux
  EDI=sum(spd*crv, na.rm = T)/const; #EDI lux
  IRR=sum(spd*crv, na.rm = T); #a-opic irradiance (W/m2)
  PHO=log10(sum(spd*crvp/(e*10000), na.rm = T)); #effective log10(photons/cm2/s)
  return(list(Animal=species,Opsin=opsin,
              EDI=as.numeric(EDI),IRR=as.numeric(IRR),PHO=as.numeric(PHO)));
}



#' Species list
#'
#' Show the list of species with available data of lens transmission and opsin sensitivity
#' @return species names
#' @examples 
#' animals <- aopicSpecies();
#' @export
aopicSpecies <- function() {
  data(SensRefData);
  data(TransRefData);
  data(SpeciesListData);
  spe = data.frame(matrix(nrow=length(names(SensRefData)), ncol=2));
  colnames(spe) = c('species','photopigment')
  for (i in 1:length(names(SensRefData))) {
    spe[i,1] = names(SensRefData)[i];
    spe[i,2] = paste(names(SensRefData[[i]]),collapse =' - ');
  }
  tra = data.frame(matrix(nrow=length(names(TransRefData)), ncol=2));
  colnames(tra) = c('species','lensTransmission')
  for (i in 1:length(names(TransRefData))) {
    tra[i,1] = names(TransRefData)[i];
    tra[i,2] = 'Yes';
  }
  spe = merge(spe, tra, by = 'species', all = T)
  spe = merge(spe, SpeciesListData, by = 'species', all = T)
  spe = spe[order(spe$species),]
  return(spe);
}


