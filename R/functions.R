#' Illuminance/Luminance Calculator
#'
#' Calculates photopic illuminance (lux) or luminance (cd/m²) based on the 
#' V(λ) photopic sensitivity function.
#'
#' @param power Numeric vector. Spectral irradiance in W/m²·nm or radiance in 
#' W/m²·sr·nm
#' @param wavelength Numeric vector. Wavelengths in nanometers (nm) 
#' corresponding to the power values.
#' @details \code{wavelength} must be defined in 1 nm increments.
#' @return Numeric. Photopic illuminance (lux) or luminance (cd/m²).
#' @examples
#' # Define wavelength range
#' wl <- 300:780
#' # Example spectral power distribution in W/m²·nm
#' irradiance <- dnorm(wl, mean = 480, sd = 30)
#' # Calculate photopic illuminance
#' lux <- vlambda(irradiance, wl)
#' @export
vlambda <- function(power, wavelength) {
  
  # Load required datasets
  temp_env <- new.env()
  data(VisualStandards, envir = temp_env)
  VisualStandards <- temp_env$VisualStandards
  
  # Load V(λ) reference data
  vlam <- data.frame(
    wavelen = 380:780,
    vlambda = VisualStandards$vlambda$photopic
  )
  
  # Build spectral power distribution dataframe
  spd <- data.frame(
    wavelen = wavelength,
    power = power
  )
  
  # Merge SPD with V(λ) by wavelength
  df_merge <- merge(spd, vlam, by = "wavelen", all = FALSE)
  
  # Compute photopic illuminance (lux) or luminance (cd/m²)
  photopic <- 683 * sum(df_merge$power * df_merge$vlambda, na.rm = TRUE)
  
  # Return
  return(photopic)
}





#' Biological Light Detection for Light Pollution
#'
#' Calculates luminous, radiant, or photon quantities 
#' using the \eqn{B_a(\lambda)}, \eqn{B_p(\lambda)}, and \eqn{B_e(\lambda)}
#' biological sensitivity functions. These functions represent a-opic biological
#' light detection for:
#' \itemize{
#'    \item all animals, and
#'    \item all plants and microbes, and
#'    \item all organisms, respectively.
#' }
#' 
#' @param power Numeric vector. Spectral irradiance in W/m²·nm or radiance 
#' in W/m²·sr·nm
#' @param wavelength Numeric vector. Wavelengths in nanometers (nm) 
#' corresponding to the power values.
#' @param quantity Character. Specifies the basis of the output system. Must 
#' be one of: "luminous", "radiant", or "photon". Default is "luminous".
#' @details \code{wavelength} must be defined in 1 nm increments.
#' @importFrom utils data
#' @return A data frame with the following columns: ("luminous" quantity = 
#' equivalent daylight illuminance in lux or luminance in cd/m²; "radiant" 
#' quantity = irradiance in W/m² or radiance in W/m²·sr; "photon" quantity = 
#' photon irradiance in log₁₀-scaled photons/cm²·s or photon radiance in 
#' log₁₀-scaled photons/cm²·s·sr).
#' \itemize{
#'    \item \code{Ba} — Animals  
#'    \item \code{Bp} — Plants and microorganisms  
#'    \item \code{Be} — All organisms  
#' }
#' @examples
#' # Define wavelength range
#' wl <- 300:780
#' # Example spectral power distribution (W/m²·nm)
#' irradiance <- dnorm(wl, mean = 480, sd = 30)
#' # Biological light detection EDI values
#' exposure_animals  <- blambda(irradiance, wl)$Ba
#' exposure_plants   <- blambda(irradiance, wl)$Bp
#' exposure_general  <- blambda(irradiance, wl)$Be
#'
#' @export
blambda <- function(power, wavelength, quantity = "luminous") {
  
  # Load required datasets
  temp_env <- new.env()
  data(VisualStandards, envir = temp_env)
  VisualStandards <- temp_env$VisualStandards
  
  # Build spectral power distribution data frame
  spd <- data.frame(
    wavelen = wavelength,
    power = power
  )
  
  # Ba(λ): all animals
  balam <- data.frame(
    wavelen  = VisualStandards$balambda$wavelen,
    balambda = VisualStandards$balambda$balambda
  )
  df_merge_balam <- merge(spd, balam, by = "wavelen", all = FALSE)
  
  # Bp(λ): all plants and microbes
  bplam <- data.frame(
    wavelen  = VisualStandards$bplambda$wavelen,
    bplambda = VisualStandards$bplambda$bplambda
  )
  df_merge_bplam <- merge(spd, bplam, by = "wavelen", all = FALSE)
  
  # Be(λ): all organisms
  belam <- data.frame(
    wavelen  = VisualStandards$belambda$wavelen,
    belambda = VisualStandards$belambda$belambda
  )
  df_merge_belam <- merge(spd, belam, by = "wavelen", all = FALSE)
  
  # Different outputs according to output measurement system
  if (quantity == "luminous") {
    
    # Luminous
    ba <- sum(df_merge_balam$power * df_merge_balam$balambda, na.rm = TRUE) /
      VisualStandards$balambda$kavD65
    bp <- sum(df_merge_bplam$power * df_merge_bplam$bplambda, na.rm = TRUE) /
      VisualStandards$bplambda$kavD65
    be <- sum(df_merge_belam$power * df_merge_belam$belambda, na.rm = TRUE) /
      VisualStandards$belambda$kavD65
    # Return biological EDI or EDL values
    return(data.frame(
      Ba = as.numeric(ba),
      Bp = as.numeric(bp),
      Be = as.numeric(be)
    ))
    
  } else if (quantity == "radiant") {
    
    # Radiant
    ba <- sum(df_merge_balam$power * df_merge_balam$balambda, na.rm = TRUE)
    bp <- sum(df_merge_bplam$power * df_merge_bplam$bplambda, na.rm = TRUE)
    be <- sum(df_merge_belam$power * df_merge_belam$belambda, na.rm = TRUE)
    # Return biological irradiance or radiance values
    return(data.frame(
      Ba = as.numeric(ba),
      Bp = as.numeric(bp),
      Be = as.numeric(be)
    ))
    
  } else if (quantity == "photon") {
    
    # Photon
    ba <- log10(sum(df_merge_balam$power * df_merge_balam$balambda / 
                      (photonenergy(df_merge_balam$wavelen) * 10000), na.rm = TRUE))
    bp <- log10(sum(df_merge_bplam$power * df_merge_bplam$bplambda / 
                      (photonenergy(df_merge_bplam$wavelen) * 10000), na.rm = TRUE))
    be <- log10(sum(df_merge_belam$power * df_merge_belam$belambda / 
                      (photonenergy(df_merge_belam$wavelen) * 10000), na.rm = TRUE))
    # Return biological photon irradiance or photon radiance values
    return(data.frame(
      Ba = as.numeric(ba),
      Bp = as.numeric(bp),
      Be = as.numeric(be)
    ))
    
  } else {
    stop("Quantity parameter must be 'luminous', 'radiant', or 'photon'")
  }
}





#' Photon Energy
#'
#' Calculates the energy of a photon for given wavelengths in nanometers (nm),
#' based on Planck's equation: \eqn{E = hc / \lambda}.
#'
#' @param wavelength Numeric vector. Wavelengths in nanometers (nm).
#' @return Numeric vector. Photon energy in Joules per photon.
#' @examples
#' # Calculate photon energy for visible spectrum
#' wavelength <- 300:800
#' e <- photonenergy(wavelength)
#'
#' @export
photonenergy <- function(wavelength) {
  
  # Convert wavelength from nm to meters
  wavelen_m <- wavelength * 1e-9
  
  # Constants
  h <- 6.62607e-34  # Planck's constant (J·s)
  c <- 2.998e8      # Speed of light (m/s)
  
  # Photon energy
  e <- h * c / wavelen_m
  
  # Return
  return(e)
}





#' Govardovskii Visual Pigment Template
#'
#' Computes the Govardovskii et al. (2000) universal visual pigment template,
#' returning the unfiltered photon action spectrum for a photopigment with a 
#' given peak wavelength (\eqn{\lambda_{\max}}). The function implements the 
#' Govardovskii et al. (2000) nomogram exactly as described in: Govardovskii 
#' et al. (2000). In search of the visual pigment template. Vis Neurosci. 
#' 17(4), 509–528. doi: 10.1017/s0952523800174036.
#'
#' @param lmax Numeric. Peak (\eqn{\lambda_{\max}}) of the photopigment in nm, 
#' measured without prereceptoral filtering.
#' @param wavelength Numeric vector. Wavelengths (nm) at which the action 
#' spectrum should be calculated.
#' @return A numeric vector containing the relative photon sensitivity at each 
#' wavelength in \code{wavelength} (arbitrary units). No prereceptoral filtering 
#' is applied.
#' @examples
#' # Compute raw sensitivity of an opsin with λmax = 450 nm
#' wavelengths <- 300:800
#' aspecp <- govardovskii(450, wavelengths)
#'
#' @export
govardovskii <- function(lmax, wavelength) {
  
  # Long-wave component parameters
  bband <- 189 + 0.315 * lmax
  fmaxa <- 3e17 / lmax
  fmaxb <- 3e17 / bband
  Aa <- 69.7
  Ab <- 0.26
  a  <- 0.8795 + 0.0459 * exp(-((lmax - 300)^2) / 11940)
  B <- 28
  b <- 0.922
  C <- -14.9
  c <- 1.104
  D <- 0.674
  d <- -40.5 + 0.195 * lmax
  
  # Convert wavelengths to frequency
  freq <- 3e17 / wavelength
  ffmax <- freq / fmaxa
  
  # A-band (alpha band)
  Sa <- 1 / (exp(Aa * (a - ffmax)) +
               exp(B  * (b - ffmax)) +
               exp(C  * (c - ffmax)) +
               D)
  
  # B-band (beta band)
  Sb <- Ab * exp(-((wavelength - bband) / d)^2)
  
  # Total unfiltered sensitivity
  aspecp <- Sa + Sb
  
  # Return
  return(aspecp)
}





#' Species List
#'
#' Returns a table of species with information about lens transmission data 
#' and available opsin sensitivities.
#'
#' @note Use the species common name (CommonName column) to specify data for 
#' that species when calling other functions in the library.
#' @importFrom utils data
#' @return Data frame with columns:
#' \itemize{
#'    \item \code{CommonName} — Common species name
#'    \item \code{Order} — Taxonomic order
#'    \item \code{Family} — Taxonomic family
#'    \item \code{LatinName} — Species Latin name
#'    \item \code{LensTransmissionInfoAvailable} — "Yes" if lens transmission data is available
#'    \item \code{OpsinSensitivityInfoAvailable} — "Yes" if any opsin data is available
#' }
#' @examples
#' aopicspecies()
#' 
#' @export
aopicspecies <- function() {
  
  # Load required datasets
  temp_env <- new.env()
  data(SensRefData, envir = temp_env)
  SensRefData <- temp_env$SensRefData
  data(TransRefData, envir = temp_env)
  TransRefData <- temp_env$TransRefData
  data(SpeciesListData, envir = temp_env)
  SpeciesListData <- temp_env$SpeciesListData
  
  # Photon sensitivity info
  spe <- data.frame(
    species = names(SensRefData),
    photopigment = "Yes",
    stringsAsFactors = FALSE
  )
  
  # Lens transmission info
  tra <- data.frame(
    species = names(TransRefData),
    lensTransmission = "Yes",
    stringsAsFactors = FALSE
  )
  
  # Merge datasets
  spe <- merge(spe, tra, by = "species", all = TRUE)
  spe <- merge(spe, SpeciesListData, by = "species", all = TRUE)
  
  # Reorder and rename columns
  spe <- spe[order(spe$species), ]
  spe <- spe[, c("species", "order", "family", "species_latin_name", "lensTransmission", "photopigment")]
  colnames(spe) <- c(
    "CommonName", 
    "Order", 
    "Family", 
    "LatinName", 
    "LensTransmissionInfoAvailable", 
    "OpsinSensitivityInfoAvailable"
  )
  
  # Return
  return(spe)
}





#' Prereceptoral Filtering Wavelength Harmonizer
#'
#' Adjust species-specific prereceptoral transmission data to match a 
#' target wavelength range for action spectrum definition by trimming or 
#' interpolating where required.
#'
#' @param pfilter Data frame with columns:
#'   - \code{wavelen}: measured wavelengths (nm)  
#'   - \code{trans}: transmission values (%)  
#' @param range Numeric vector. Target wavelength range (nm) to match.
#' @details Both `wavelen` and `range` must be defined in 1 nm increments.
#' @return Numeric vector. Transmission values (%) extrapolated or trimmed to 
#' match the target wavelength range (same length as \code{range}).
#' @examples
#' df <- data.frame(wavelen = 350:750, trans = seq(0, 100, length.out = 401))
#' trans <- harmonisetrans(df, 300:800)
#' 
#' @noRd
harmonisetrans <- function(pfilter, range) {
  
  # Extract existing data
  wlp <- pfilter$wavelen
  trans <- pfilter$trans
  
  # Short wavelength adjustment
  delta_short <- wlp[1] - range[1]
  if (delta_short > 0) {
    # Extrapolate shorter wavelengths
    avg_delta <- mean(diff(trans[1:3]))
    extra <- trans[1] - avg_delta * seq(delta_short, 1)
    extra[extra < 0] <- 0
    trans <- c(extra, trans)
    wlp <- seq(range[1], wlp[length(wlp)])
  } else if (delta_short < 0) {
    # Trim initial wavelengths
    trans <- trans[(-delta_short + 1):length(trans)]
    wlp <- wlp[(-delta_short + 1):length(wlp)]
  }
  
  # Long wavelength adjustment
  delta_long <- range[length(range)] - wlp[length(wlp)]
  if (delta_long > 0) {
    # Extrapolate longer wavelengths (assume constant transmission)
    trans <- c(trans, rep(trans[length(trans)], delta_long))
    wlp <- seq(wlp[1], range[length(range)])
  } else if (delta_long < 0) {
    # Trim excess wavelengths
    trans <- trans[1:(length(trans) + delta_long)]
    wlp <- wlp[1:(length(wlp) + delta_long)]
  }
  
  # Return
  return(trans)
}





#' Opsin Sensitivity Wavelength Harmonizer
#'
#' Adjusts a measured opsin photon sensitivity curve to match a target wavelength 
#' range for species-specific action spectra definition by trimming or 
#' interpolating as required.
#' 
#' @param sens Data frame with columns:
#'   - \code{wavelen}: measured wavelengths (nm)  
#'   - \code{aspecp}: photon sensitivity values  
#' @param range Numeric vector. Target wavelength range (nm) to match.
#' @details Both `wavelen` and `range` must be defined in 1 nm increments.
#' @return Numeric vector. Photon sensitivity values aligned with the target 
#' wavelength range (same length as \code{range}).
#' @examples
#' df <- data.frame(wavelen = 350:750, aspecp = govardovskii(450, 350:750))
#' sens <- harmonisesens(df, 300:800)
#'
#' @noRd
harmonisesens <- function(sens, range) {
  
  # Extract existing data
  wls <- sens$wavelen
  ph <- sens$aspecp
  
  # Short wavelength adjustment
  delta_short <- wls[1] - range[1]
  if (delta_short > 0) {
    # Extrapolate (NA) for missing short wavelengths
    ph <- c(rep(NA, delta_short), ph)
    wls <- seq(range[1], wls[length(wls)])
  } else if (delta_short < 0) {
    # Trim initial values
    ph <- ph[(-delta_short + 1):length(ph)]
    wls <- wls[(-delta_short + 1):length(wls)]
  }
  
  # Long wavelength adjustment
  delta_long <- wls[length(wls)] - range[length(range)]
  if (delta_long < 0) {
    # Extrapolate (NA) for missing long wavelengths
    ph <- c(ph, rep(NA, -delta_long))
    wls <- seq(wls[1], range[length(range)])
  } else if (delta_long > 0) {
    # Trim excess values
    ph <- ph[1:(length(ph) - delta_long)]
    wls <- wls[1:(length(wls) - delta_long)]
  }
  
  # Return
  return(ph)
}





#' Alpha-Opic Action Spectrum Generator
#'
#' Generates a Govardovskii nomogram for a specified opsin with optional 
#' corrections for prereceptoral filtering.
#'
#' @param opsin Character. Specifies the target opsin name. Must be one of: 
#' "Mel", "Rod", "Scone", "Mcone", "Lcone". Default is "Mel".
#' @param lmax Numeric or character. Lambda max of the opsin's photon sensitivity 
#' without prereceptoral filtering. Can be:
#' \itemize{
#'     \item A numeric wavelength (nm) for modelling a custom opsin, or
#'     \item A character string specifying a species-specific workspace in the 
#'     \code{/data} directory (e.g., "Mouse") to load a pre-measured lambda max.
#' }
#' @param pfilter Numeric, character, or data frame. Information for prereceptoral 
#' filtering correction. Can be:
#' \itemize{
#'    \item A numeric zero for no filtering, or
#'    \item A character string specifying a species-specific workspace in \code{/data}, or  
#'    \item A data frame with columns:
#'    \itemize{
#'        \item \code{wavelen}: measured wavelengths (nm). The wavelengths must be 
#'        defined in 1 nm steps.
#'        \item \code{trans}: transmission values (\%).
#'    }
#' }
#' @param range Numeric vector. Wavelength range (nm) over which to generate the 
#' action spectrum (default: 300:800). Must be in 1 nm steps.
#' @importFrom utils data
#' @return A list containing:
#' \itemize{
#'    \item \code{opsin} — opsin name  
#'    \item \code{wavelen} — harmonized wavelength range  
#'    \item \code{trans} — prereceptoral lens transmission used  
#'    \item \code{aspec} — normalized energy-based action spectrum corrected for lens transmission
#'    \item \code{aspecp} — normalized photon-based action spectrum corrected for lens transmission
#'    \item \code{kavD65} — a-opic efficacy of luminous radiation for daylight (D65; W/lm)
#' }
#' @examples
#' # Generate a-opic action spectra for different opsins and species
#' actionspec1 <- generateaopicactionspec(
#'   opsin = 'Mel',
#'   lmax = 'Human',
#'   pfilter = 'Human',
#'   range = 380:780
#' )
#' actionspec2 <- generateaopicactionspec(opsin = 'Mel', lmax = 'Mouse', pfilter = 'Mouse')
#' actionspec3 <- generateaopicactionspec(opsin = 'Mel', lmax = 480, pfilter = 'Mouse')
#' actionspec4 <- generateaopicactionspec(
#'   opsin = 'Rod',
#'   lmax = 'Dog',
#'   pfilter = data.frame(
#'     wavelen = 350:750,
#'     trans = seq(0, 100, length.out = 401)
#'   )
#' )
#' 
#' @export
generateaopicactionspec <- function(opsin = "Mel", lmax, pfilter, range = 300:800) {
  
  # Load required datasets
  temp_env <- new.env()
  data(VisualStandards, envir = temp_env)
  VisualStandards <- temp_env$VisualStandards
  data(SensRefData, envir = temp_env)
  SensRefData <- temp_env$SensRefData
  data(TransRefData, envir = temp_env)
  TransRefData <- temp_env$TransRefData
  
  # Define photon-based spectral sensitivity
  if (is.numeric(lmax) && length(lmax) == 1 && lmax > 0) {
    # Lambda max model (Govardovskii nomogram) for photon sensıtıvıty
    aspecp <- govardovskii(lmax, range) 
  } else if (is.character(lmax)) {
    # Predefined sensitivity
    Sens <- SensRefData[[lmax]][[opsin]]
    aspecp <- harmonisesens(Sens, range)
  }
  
  # Define prereceptoral transmission
  if (is.numeric(pfilter) && length(pfilter) == 1) {
    trans <- if (pfilter == 0) rep(100, length(range)) 
  } else if (is.character(pfilter)) {
    # Predefined trasnmission
    Pfilter <- TransRefData[[pfilter]]
    trans <- harmonisetrans(Pfilter, range)
  } else if (is.data.frame(pfilter) && ncol(pfilter) == 2) {
    # New data of trasnmission
    Pfilter <- data.frame(wavelen = pfilter[,1], trans = pfilter[,2])
    trans <- harmonisetrans(Pfilter, range)
  }
  
  # Apply prereceptoral correction and convert to energy sensitivity
  e <- photonenergy(range)  # photon energy in Joules
  if (is.character(lmax)) {
    # Predefined sensitivity with lens filtering
    lens_filter <- SensRefData[[lmax]][[opsin]]$lens_filter
    if (lens_filter == "yes") {
      aspec <- aspecp / e  # convert photon sensitivity to energy sensitivity
    } else if (lens_filter == "no") {
      aspecp <- aspecp * (trans / 100)
      aspec <- aspecp / e
    }
  } else {
    # Modeled Govardovskii sensitivity
    aspecp <- aspecp * (trans / 100)
    aspec <- aspecp / e
  }

  # Normalize both photon and energy spectra
  max_aspec <- max(aspec, na.rm = TRUE)
  max_aspecp <- max(aspecp, na.rm = TRUE)
  if (max_aspec <= 0) {
    stop("Error: Maximum of energy-based action spectrum is zero or negative. Cannot normalize.")
  }
  if (max_aspecp <= 0) {
    stop("Error: Maximum of photon-based action spectrum is zero or negative. Cannot normalize.")
  }
  aspec <- aspec / max_aspec
  aspecp <- aspecp / max_aspecp

  # Calculate a-opic efficacy of D65 daylight
  wl65 <- VisualStandards$D65$wavelen
  sp65 <- VisualStandards$D65$spectra
  sp65 <- sp65[wl65 >= range[1] & wl65 <= range[length(range)]]
  kavD65 <- sum(sp65 * aspec, na.rm = TRUE) / VisualStandards$D65$lux
  
  # Return results
  return(list(
    opsin = opsin,
    wavelen = range,
    trans = trans,
    aspec = aspec,
    aspecp = aspecp,
    kavD65 = kavD65
  ))
}





#' Species-Specific a-Opic Calculator
#'
#' Calculates luminous, radiant, or photon quantities using species-specific 
#' a-opic functions.
#'
#' @param power Numeric vector. Spectral irradiance in W/m²·nm or radiance in 
#' W/m²·sr.nm
#' @param wavelength Numeric vector. Wavelengths in nanometers (nm) 
#' corresponding to the power values. Assumes equally spaced spectra; 
#' spline interpolation is applied if spacing is not 1 nm.
#' @param opsin Character. Specifies the target opsin name. Must be one of: 
#' "Mel", "Rod", "Scone", "Mcone", "Lcone".
#' Default is "Mel".
#' @param lmax Numeric or character. Lambda max of the opsin's photon sensitivity 
#' without prereceptoral filtering. Can be:
#' \itemize{
#'     \item A numeric wavelength (nm) for modelling a custom opsin, or
#'     \item A character string specifying a species-specific workspace in the 
#'     \code{/data} directory (e.g., "Mouse") to load a pre-measured lambda max.
#' }
#' @param pfilter Numeric, character, or data frame. Information for prereceptoral 
#' filtering correction. Can be:
#' \itemize{
#'    \item A numeric zero for no filtering, or
#'    \item A character string specifying a species-specific workspace in \code{/data}, or  
#'    \item A data frame with columns:
#'    \itemize{
#'        \item \code{wavelen}: measured wavelengths (nm). The wavelengths must be 
#'        defined in 1 nm steps.
#'        \item \code{trans}: transmission values (\%).
#'    }
#' }
#' @importFrom utils data
#' @importFrom stats spline
#' @return A data frame with the following columns:
#' \itemize{
#'    \item \code{Luminous}
#'    a-opic equivalent daylight quantity expressed in:
#'    \itemize{
#'        \item lux (for illuminance), or
#'        \item cd/m² (for luminance)
#'    }
#'    \item \code{Radiant}
#'    a-opic radiant quantity expressed in:
#'    \itemize{
#'        \item W/m² (irradiance), or
#'        \item W/m²·sr (radiance)
#'    }
#'    \item \code{Photon}
#'    a-opic photon-based quantity expressed in:
#'    \itemize{
#'        \item log₁₀ photons/cm²·s* (photon irradiance), or
#'        \item log₁₀ photons/cm²·s·sr (photon radiance)
#'    }
#' }
#' @note
#' \itemize{
#'    \item Use the species common name (e.g., "Mouse") to specify data for that 
#'    species when using predefined sensitivities or prereceptoral filters.  
#'    \item Use the \code{aopicspecies()} function to see the list of species available in 
#'    the package data.  
#'    \item Ensure that \code{power} and \code{wavelength} vectors have the same length and that 
#'    wavelength values are equally spaced. Spline interpolation will be applied 
#'    if spacing is not 1 nm.  
#'    \item The measurement wavelength range (\code{wavelength}) should cover the target 
#'    opsin sensitivity range for accurate calculation.
#' }
#' @examples
#' # Define wavelength range
#' wl <- 300:780
#' # Example spectral irradiance distribution in W/m²·nm
#' irradiance <- dnorm(wl, mean = 480, sd = 30)
#' # Using predefined species data
#' exposure1 <- alphaopic(irradiance, wl, opsin = 'Scone', lmax = 'Human', pfilter = 'Human')
#' # Using numeric lambda max for opsin modeling
#' exposure2 <- alphaopic(irradiance, wl, opsin = 'Mel', lmax = 480, pfilter = 'Cat')
#' # Using new lens transmission observations
#' lens_data <- data.frame(wavelen = 350:750, trans = seq(0, 100, length.out = 401))
#' exposure3 <- alphaopic(irradiance, wl, opsin = 'Rod', lmax = 500, pfilter = lens_data)
#' # Apply no lens filtering
#' exposure4 <- alphaopic(irradiance, wl, opsin = 'Mcone', lmax = 'Horse', pfilter = 0)
#' 
#' @export
alphaopic <- function(power, wavelength, opsin = "Mel", lmax, pfilter) {
  
  # Load required datasets
  temp_env <- new.env()
  data(SensRefData, envir = temp_env)
  SensRefData <- temp_env$SensRefData
  data(TransRefData, envir = temp_env)
  TransRefData <- temp_env$TransRefData
  
  # Ensure wl and power are consistent
  wl <- wavelength
  if (mean(diff(wl)) != 1) { 
    # Not 1 nm spacing, use spline interpolation
    wlo <- wl
    wl <- round(wlo[1]):round(wlo[length(wlo)])
    power <- spline(wlo, power, n = length(wl), method = "natural")$y
    power <- power / mean(diff(wlo))  # preserve scale
  }
  
  # Check opsin photon sensitivity
  if (is.character(lmax) && 
      (lmax %in% names(SensRefData))) {
    if (opsin %in% names(SensRefData[[lmax]]) &&
        !is.na(SensRefData[[lmax]][[opsin]]$opsin_not_available) &&
        SensRefData[[lmax]][[opsin]]$opsin_not_available == "no") {
      message("Available opsin photon sensitivity info for this species")
    } else {
      message("No opsin photon sensitivity info for this species")
    }
  }
  # Check lens transmission
  if (is.character(pfilter) && 
      (pfilter[1] %in% names(TransRefData))) {
    message("Available lens transmission info for this species")
  } else {
    message("No lens transmission info for this species")
  }
  
  # Get action spectra and constants
  aopics <- generateaopicactionspec(opsin, lmax, pfilter, wl)
  crv <- aopics$aspec   # energy-based action spectrum
  crvp <- aopics$aspecp # photon-based action spectrum
  const <- aopics$kavD65
  wavelen <- aopics$wavelen
  
  # Calculate a-opic luminous, radiant, or photon quantities
  e <- photonenergy(wavelen) # energy per photon (J)
  # EDI or EDL in lux or cd/m²
  Luminous <- sum(power * crv, na.rm = TRUE) / const 
  # a-opic irradiance (W/m²) or radiance (W/sr/m²)
  Radiant <- sum(power * crv, na.rm = TRUE) 
  # photon irradiance log10(photons/cm²/s) or radiance log10(photons/cm²/s)/sr
  Photon <- log10(sum(power * crvp / (e * 10000), na.rm = TRUE)) 
  
  # Return results
  return(data.frame(
    Luminous = as.numeric(Luminous),
    Radiant = as.numeric(Radiant),
    Photon = as.numeric(Photon)
  ))
}




