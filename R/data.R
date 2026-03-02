#' Opsin Sensitivity Reference Data
#'
#' This dataset contains species-specific opsin sensitivity curves. 
#' Each element of the list corresponds to a species (e.g., "Human", "Mouse") 
#' and provides the reference visual pigment sensitivity curves for that species.
#' The structure is nested lists: each species has sublists for 
#' Rod, Mel, Scone, Mcone, and Lcone opsins.
#' 
#' @format A list of species, each of which is a list with the following elements:
#' \itemize{
#'   \item \code{order} Character. Taxonomic order of the species (e.g., "Primates").
#'   \item \code{family} Character. Taxonomic family of the species (e.g., "Hominidae").
#'   \item \code{species} Character. Common name of the species (e.g., "Human").
#'   \item \code{species_latin_name} Character. Scientific name of the species (e.g., "Homo sapiens").
#'   \item \code{Rod, Mel, Scone, Mcone, Lcone} Each is a list of 12 elements describing opsin properties:
#'   \itemize{
#'     \item \code{opsin} Character. Type of opsin (Rod, Mel, Scone, Mcone, Lcone).
#'     \item \code{reference} Character or NA. Reference source for the data.
#'     \item \code{opsin_not_available} Character ("yes"/"no") or NA. "yes" if the 
#'     species does not have the opsin.
#'     \item \code{full_data_available} Character ("yes"/"no") or NA. "yes" if full
#'      spectral data is available instead of modelling from the lambda max.
#'     \item \code{method} Character or NA. Measurement method (e.g., Absorbance, 
#'     MSP, Predicted, ERG).
#'     \item \code{reported_peak} Numeric or NA. Reported peak wavelength of the 
#'     opsin in the reference.
#'     \item \code{measured_without_filter} Character ("yes"/"no") or NA. "yes" if 
#'     measurement was without filter.
#'     \item \code{notes} Character or NA. Additional notes.
#'     \item \code{lmax} Numeric or NA. Opsin peak wavelength to use in 
#'     Govargoskii function.
#'     \item \code{lens_filter} Character ("yes"/"no"). Whether lens filter was 
#'     present in the final opsin sensitivity function.
#'     \item \code{wavelen} Integer vector. Wavelength range (e.g., 300:800).
#'     \item \code{aspecp} Numeric vector. Opsin photon sensitivity function.
#'   }
#' }
#' @source Internal compiled dataset
#' @usage data(SensRefData)
#' @examples
#' data(SensRefData)
#' # View species names
#' names(SensRefData)
#' 
"SensRefData"





#' Prereceptoral Lens Transmission Data
#'
#' This dataset contains species-specific prereceptoral lens transmission curves.
#' Each element of the list corresponds to a species (e.g., "Human", "Mouse") 
#' and provides the reference transmission spectrum for that species.
#' 
#' @format A named list of species, each of which is a list with the following elements:
#' \itemize{
#'   \item \code{order} Character. Taxonomic order of the species (e.g., "Primates").
#'   \item \code{family} Character. Taxonomic family of the species (e.g., "Hominidae").
#'   \item \code{species} Character. Common name of the species (e.g., "Human").
#'   \item \code{species_latin_name} Character. Scientific name of the species (e.g., "Homo sapiens").
#'   \item \code{reference} Character. Source reference for the transmission data.
#'   \item \code{wavelen} Integer vector. Wavelength range (e.g., 300:800 nm).
#'   \item \code{trans} Numeric vector. Transmission values (\%) corresponding to each wavelength.
#' }
#' @source Internal compiled dataset
#' @usage data(TransRefData)
#' @examples
#' data(TransRefData)
#' # View species names
#' names(TransRefData)
#' # Plot human lens transmission
#' human_data <- TransRefData$Human
#' plot(human_data$wavelen, human_data$trans, type = "l",
#'      xlab = "Wavelength (nm)", ylab = "Transmission")
#'      
"TransRefData"





#' VisualStandards
#'
#' Standard visual spectra used for photopic and biological light detection calculations.
#' 
#' @format A list of datasets including D65, V(λ), Ba(λ), Bp(λ), Be(λ)
#' @source Internal compiled dataset
#' @usage data(VisualStandards)
#' @examples
#' data(VisualStandards)
#' 
"VisualStandards"





#' Species List Data
#'
#' This dataset provides taxonomic information for species included in the study.
#' It includes the taxonomic order and family, as well as the common and 
#' scientific names.
#' 
#' @format A data frame with 4 variables:
#' \itemize{
#'   \item \code{order} Character. Taxonomic order of the species (e.g., "Primates").
#'   \item \code{family} Character. Taxonomic family of the species (e.g., "Hominidae").
#'   \item \code{species} Character. Common name of the species (e.g., "Human").
#'   \item \code{species_latin_name} Character. Scientific name of the species (e.g., "Homo sapiens").
#' }
#' @source Internal compiled dataset
#' @usage data(SpeciesListData)
#' @examples
#' data(SpeciesListData)
#' # List all species
#' unique(SpeciesListData$species)
"SpeciesListData"




