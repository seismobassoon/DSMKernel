'''
Pre-/post- processing GUI facilities for DSM kernel

2022 Nobuaki Fuji

with contribution from Hugo Jaegler (2013)
inspired by Stephanie Durand (2017)

'''

#from babel.support import Translations
# Of course this software should be multilingual soon!!

import numpy as np
import matplotlib.pyplot as plt
from pylab  import vlines
from obspy.core import read
import subprocess
import tkinter as Tk
from PIL import ImageTk, Image
from tkinter import filedialog
import os
import sys

sys.path.append("./modules")
sys.path.append("./images")
import DSMTk


if __name__ == "__main__":
    
    root0 = DSMTk.SampleApp()
    root0.geometry("1024x1400")
    #fenetrePath = DSMTk.Welcome(root0)

    root0.mainloop()



