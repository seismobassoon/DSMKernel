Hi! 👋
Here is the Python graphical interface for the DSM Kernel. It is an interactive map that enables to select seismic stations with events from the catalogue IRIS and directly download the data 
(XML file, and sac/mseed for the seismic trace). The first goal is to be able to compute faster and easier the synthetic seismograms for any teleseismic event thanks to the DSM Kernel algorithm.

First of all, to download the file :
```
git clone https://github.com/LorrN13/DSMKernel.git
```
Then you have to download the conda environment to use the application. If you are using the Windows operating system:
```
conda env create -f /path/to/DSMKernel/utilpyqt/environment-windows.yml
```
If you are using the Linux operating system:
```
conda env create -f /path/to/DSMKernel/utilpyqt/environment-linux.yml
```
Then, activate the conda environment:
```
conda activate DSM_GUI
```
The application is launched with:
```
python main.py
```

_(more information of the utilisation are coming...)_
