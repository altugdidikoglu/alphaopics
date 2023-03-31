# alphaopics
R package to calculate mammal species and photopigment specific light exposures

![Screenshot](screenshot.png)


# Species and Photoreceptor-Specific Light Exposure Measurement in Mammals


This repository includes R scripts to calculate animal light exposure and required reference data
1. **functions.R** includes functions:
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

These functions are called by *calculateEDI* function which calculates EDI, total irradiance, and effective photon according to given spectral power distribution, wavelength range, species name, opsin type, sensitivity curve (from data or given lambda max), prereceptoral filtering (from data or given normalised transmission data)

2. **data** includes required spectral sensitivity and transmission data and standards

3. **addspecieswizard.R** includes code to add new sensitivity or transmission to data

4. **App.R** includes Shiny package to convert functions to a web page

Online app: [Animal α-opic light exposure calculator](https://altugdidikoglu.shinyapps.io/alphaopics/)
