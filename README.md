![Screenshot](logo.png)

# Species and Photoreceptor-Specific Light Exposure Measurement in Mammals
--------------------------------------------------------------------------------

**Light serves as one of the primary environmental cues for regulating various physiological and behavioral processes. Disruptions in circadian rhythms resulting from mistimed or inadequate light exposure have been linked to a range of health problems across mammalian species. As a consequence, ensuring appropriate lighting is crucial for indoor housed mammals. Lux is the most commonly used metric for measuring lighting, yet it is a weighted spectral function based on human perceived brightness and is unsuitable for capturing non-image forming effects of light or for measuring light exposure across different species. Photoreceptor-specific (α-opic) measurements of lighting have been proposed as a more suitable units of quantifying light in humans. Since the number of photoreceptor types, photopigment spectral sensitivities, and eye anatomy differ across mammalian species, we have developed a method for measuring light exposure in a species-specific manner based on photoreceptor activation.**

--------------------------------------------------------------------------------

*The package was prepared by Time Vision Behaviour Lab in the University of Manchester. This project has received funding from the Wellcome Trust Investigator Award (210684/Z/18/Z) to [Robert Lucas](https://research.manchester.ac.uk/en/persons/robert.lucas). Problems or requests can be emailed to package maintainer [Altug Didikoglu](https://research.manchester.ac.uk/en/persons/altug.didikoglu). This package includes R functions to calculate animal light exposure and required reference data. The packege is also available as an online app: [Animal α-opic light exposure calculator](https://altugdidikoglu.shinyapps.io/alphaopics/).*

--------------------------------------------------------------------------------

# alphaopics

R package to calculate mammal species and photopigment specific light exposures

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

## Light exposure calculation

To calculate species and photopigment-specific ligh exposure, use the `alphaopic()` function. This function calculates species-specific a-opic EDIs (+ photopic illuminance), irradiances and effective photons from predefined species specific parameters.

```
exposure <- alphaopic(spd, wl, species, opsin, lmax, pfilter);
```

The arguments taken by alphaopic() are used to specify light stimuli and calculation parameters.

<sub> **spd** Vector containing spectral power distribution in W/m2.nm </sub>

<sub> **wl** Corresponding wavelength range over which measurements are acquired; assumes equally spaced spectra and uses spline extrapolation if not have 1nm spacing </sub>

<sub> **species** String containing the name of a species e.g. 'Mouse' </sub>

<sub> **opsin** Target photopigment name (One of the following options: 'Mel','Rod','Scone','Mcone','Lcone') or 'Photopic' for human lux </sub>

<sub> **lmax** The lambda max of the opsin's photon sensitivity in the absence of preceptoral filtering. One of the following options: a string specifying a species specific work space in the subdirectory '/data' e.g. 'Mouse', or a numerical wavelength value specifying the lambda max to allow modelling </sub>

<sub> **pfilter** Information about function for prereceptoral filtering. One of the following options: string specifying a species-specific work space in the subdirectory '/data' e.g. 'Mouse', zero for no prereceptoral filtering, or new transmission measurement data matrix with transmissions and wavelengths </sub>

This function generates a list of species name, target opsin, *(EDI)* a-opic equivalent daylight illuminance [lux], *(IRR)* a-opic irradiance [W/m2], *(PHO)* Effective photon [log10(photons/cm2.s)]

```
# Example calculations

# Wavelength range
wl = 300:780

# Spectral power distribution in W/m2.nm
spd = dnorm(300:780, mean = 480, sd = 30)

# If target species is available in the package data
exposure1 <- alphaopic(spd, wl, 'Mouse', 'Scone', 'Mouse', 'Mouse');

# If lambda max value of opsin's photon sensitivity will be used
exposure2 <- alphaopic(spd, wl, 'Cat', 'Mel', 480, 'Cat');

# If new lens transmission observations will be used
exposure3 <- alphaopic(spd, wl, 'Sheep', 'Rod', 500, data.frame(wavelen = 350:750, trans = seq(0,100,length.out=401)));
```

## Other functions in the package

* *aopicSpecies*

<sub>Show the list of species with available data of lens transmission and opsin sensitivity<sub>

* *govardovskii*

<sub>Calculate Govardovskii nomogram as Govardovskii et al. 2000. (Vis Neurosci. 2000 Jul-Aug;17(4):509-28. doi: 10.1017/s0952523800174036)</sub>

* *photonenergy*

<sub>Calculates the energy of a photon at a given wavelengths(nm)</sub>

* *harmonisetrans*

<sub>Extrapolate/adjust species specific prereceptoral filtering data</sub>

* *harmonisesens*

<sub>Extrapolate/adjust curve to match the wavelength range for species specific action spectra</sub>

* *generateaopicactionspec*

<sub>Generate a govardovski nomogram with corrections prereceptoral filtering</sub>

## Alphaopics References

Duthie, A. B., Cusack, J. J., Jones, I. L., Nilsen, E. B., Pozo, R. A., Rakotonarivo, O. S., Moorter, B. Van, & Bunnefeld, N. (2018). GMSE: an R package for generalised management strategy evaluation. *Methods in Ecology and Evolution*, 9, 2396-2401. https://doi.org/10.1101/221432

Duthie, A. B., A. Bach, & J. Minderman (2021). GMSE: Generalised Management Strategy Evaluation Simulator. R package version 0.7.0.0. https://confoobio.github.io/gmse/








