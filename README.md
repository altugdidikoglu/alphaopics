![](logo.png)

# Species and Photoreceptor-Specific Light Exposure Measurement in Mammals

**Light serves as one of the primary environmental cues for regulating various physiological and behavioral processes. Disruptions in circadian rhythms resulting from mistimed or inadequate light exposure have been linked to a range of health problems across mammalian species. As a consequence, ensuring appropriate lighting is crucial for indoor housed mammals. Lux is the most commonly used metric for measuring lighting, yet it is a weighted spectral function based on human perceived brightness and is unsuitable for capturing non-image forming effects of light or for measuring light exposure across different species. Photoreceptor-specific (α-opic) measurements of lighting have been proposed as a more suitable units of quantifying light in humans. Since the number of photoreceptor types, photopigment spectral sensitivities, and eye anatomy differ across mammalian species, we have developed a method for measuring light exposure in a species-specific manner based on photoreceptor activation.**

------------------------------------------------------------------------

*The package was prepared by Time Vision Behaviour Lab in the University of Manchester. This project has received funding from the Wellcome Trust Investigator Award (210684/Z/18/Z) to [Robert Lucas](https://research.manchester.ac.uk/en/persons/robert.lucas). Problems or requests can be emailed to package maintainer [Altug Didikoglu](https://orcid.org/0000-0002-5582-6956). This package includes R functions to calculate animal light exposure and required reference data. The packege is also available as an online app: [Animal α-opic light exposure calculator](%5Bhttps://altugdidikoglu.shinyapps.io/alphaopics/%5D(https://alphaopics.shinyapps.io/animal_light_toolbox/)).*

------------------------------------------------------------------------

*This package also implements biological light detection spectral weighting functions for animals, plants and microorganisms, and for all organisms, based on the known wavelength dependence of photoreceptive mechanisms. Light metrics employing B(λ) functions provide a means to quantify anthropogenic light at night (ALAN) in terms relevant to its likely biological detection, thereby offering a tool for assessing and reducing its presence in the environment.*

------------------------------------------------------------------------

# alphaopics

R package to provide tools to calculate species-specific a-opic light exposure metrics for ecological and photobiological research. The package includes datasets for species-specific photopigment sensitivities and prereceptoral filtering. Additional tools compute photopic illuminance and biological light detection indices to support light pollution assessments.

## Installation

**Install with GitHub**

To install this package from GitHub, make sure that the `devtools` library is installed.

```         
install.packages("devtools")
library(devtools)
```

Use `install_github` to install using `devtools`.

```         
install_github("altugdidikoglu/alphaopics")
```

## a-opic light exposure calculation

To calculate species and photopigment-specific ligh exposure, use the `alphaopic()` function. This function calculates species-specific a-opic radiance/irradiance, photon radiance/irradiance, and equivalent daylight illuminance/luminance from predefined species specific parameters.

```         
exposure <- alphaopic(power, wavelength, opsin, lmax, pfilter)
```

The arguments taken by `alphaopic()` are used to specify light stimuli and calculation parameters.

<sub> **power** Vector containing spectral irradiance in W/m²·nm or radiance in W/m²·sr.nm </sub>

<sub> **wavelength** Corresponding wavelength range over which measurements are acquired; assumes equally spaced spectra and uses spline extrapolation if not have 1nm spacing </sub>

<sub> **opsin** Target photopigment name (One of the following options: 'Mel','Rod','Scone','Mcone','Lcone') </sub>

<sub> **lmax** The lambda max of the opsin's photon sensitivity in the absence of preceptoral filtering. One of the following options: a string specifying a species specific work space in the subdirectory '/data' e.g. 'Mouse', or a numerical wavelength value specifying the lambda max to allow modelling. Use the `aopicspecies()` function to see the list of species available in the package data </sub>

<sub> **pfilter** Information about function for prereceptoral filtering. One of the following options: string specifying a species-specific work space in the subdirectory '/data' e.g. 'Mouse', zero for no prereceptoral filtering, or new transmission measurement data matrix with transmissions and wavelengths </sub>

This function returns A data frame with the following columns: (1) Luminous; a-opic equivalent daylight quantity expressed in lux (for illuminance) or cd/m² (for luminance), (2) Radiant; a-opic radiant quantity expressed in W/m² (irradiance) or W/m²·sr (radiance), (3) Photon; a-opic photon-based quantity expressed in log₁₀ photons/cm²·s (photon irradiance) or log₁₀ photons/cm²·s·sr (photon radiance)

```         
# Example calculations

# Define wavelength range
wl <- 300:780

# Example spectral irradiance distribution in W/m²·nm
irradiance <- dnorm(wl, mean = 480, sd = 30)

# Using predefined species data
exposure1 <- alphaopic(irradiance, wl, opsin = 'Scone', lmax = 'Human', pfilter = 'Human')

# Using numeric lambda max for opsin modeling
exposure2 <- alphaopic(irradiance, wl, opsin = 'Mel', lmax = 480, pfilter = 'Cat')

# Using new lens transmission observations
lens_data <- data.frame(wavelen = 350:750, trans = seq(0, 100, length.out = 401))
exposure3 <- alphaopic(irradiance, wl, opsin = 'Rod', lmax = 500, pfilter = lens_data)

# Apply no lens filtering
exposure4 <- alphaopic(irradiance, wl, opsin = 'Mcone', lmax = 'Horse', pfilter = 0)
```

## Photopic illuminance/luminance calculation

Calculates photopic illuminance (lux) or luminance (cd/m²) based on the V(λ) photopic sensitivity function.

```         
exposure <- vlambda(power, wavelength)
```

## Biological light detection for light pollution calculation

Calculates luminous, radiant, or photon quantities using the Ba(λ), Bp(λ), and Be(λ) biological sensitivity functions. These functions represent a-opic biological light detection for: all animals, and all plants and microbes, and all organisms, respectively.

```         
exposure <- blambda(power, wavelength, quantity)
```

The arguments taken by `blambda()` are used to specify light stimuli and calculation parameters.

<sub> **power** Vector containing spectral irradiance in W/m²·nm or radiance in W/m²·sr.nm </sub>

<sub> **wavelength** Corresponding wavelength range over which measurements are acquired; assumes equally spaced spectra and uses spline extrapolation if not have 1nm spacing </sub>

<sub> **quantity** Specifies the basis of the output system. Must be one of: "luminous", "radiant", or "photon" </sub>

## Other functions in the package

-   *aopicspecies*

<sub>Show the list of species with available data of lens transmission and opsin sensitivity<sub>

-   *govardovskii*

<sub>Calculate Govardovskii nomogram as Govardovskii et al. 2000. (Vis Neurosci. 2000 Jul-Aug;17(4):509-28. doi: 10.1017/s0952523800174036)</sub>

-   *photonenergy*

<sub>Calculates the energy of a photon at a given wavelengths(nm)</sub>

-   *harmonisetrans*

<sub>Extrapolate/adjust curve to match the wavelength range for species-specific prereceptoral filtering data</sub>

-   *harmonisesens*

<sub>Extrapolate/adjust curve to match the wavelength range for species specific action spectra</sub>

-   *generateaopicactionspec*

<sub>Generates a Govardovskii nomogram for a specified opsin with optional corrections for prereceptoral filtering</sub>

## Alphaopics References

Lucas, R. J. et al. Measuring and using light in the melanopsin age. Trends Neurosci 37, 1-9 (2014). [https://doi.org:10.1016/j.tins.2013.10.004](https://doi.org:10.1016/j.tins.2013.10.004)

CIE S026/E:2018: CIE System for Metrology of Optical Radiation for ipRGC-Influenced Responses to Light. (2018). [https://doi.org:10.25039/S026.2018](https://doi.org:10.25039/S026.2018)

Lucas, R. J. et al. Recommendations for measuring and standardizing light for laboratory mammals to improve welfare and reproducibility in animal research. PLoS Biol 22, e3002535 (2024). [https://doi.org/10.1371/journal.pbio.3002535](https://doi.org/10.1371/journal.pbio.3002535)

Douglas, R. H. & Jeffery, G. The spectral transmission of ocular media suggests ultraviolet sensitivity is widespread among mammals. Proc Biol Sci 281, 20132995 (2014). [https://doi.org:10.1098/rspb.2013.2995](https://doi.org:10.1098/rspb.2013.2995)

Govardovskii, V. I., Fyhrquist, N., Reuter, T., Kuzmin, D. G. & Donner, K. In search of the visual pigment template. Vis Neurosci 17, 509-528 (2000). [https://doi.org:10.1017/s0952523800174036](https://doi.org:10.1017/s0952523800174036)

Didikoglu, A. Optimal spectral weighting functions for biological and ecological effects of environmental light pollution. CIE (2026). [https://cie.co.at/reporter/dr-6-50](https://cie.co.at/reporter/dr-6-50)

