Hi! 👋
Here is the python graphical interface for the DSM Kernel. It is a interactive map that enables to select seismic stations with events from the catalog IRIS and directly download the data 
(xml file, and sac/mseed for the seismic trace). The first goal is to be able to compute faster and easier the synthetic seismograms for any teleseismic event thanks to the DSM Kernel algorithm.

First of all, to download the file :
```
git clone https://github.com/LorrN13/DSMKernel.git
```
Then you have to download the conda environment to use the application:
```
conda env create -f /path/to/DSMKernel/utilpyqt/environment.yml
conda activate DSM_GUI
```
The application is launched with:
```
python main.py
```

_(more information of the utilisation are coming...)_
