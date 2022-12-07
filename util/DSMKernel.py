'''
Pre-/post- processing GUI facilities for DSM kernel

2022 Nobuaki Fuji

with contributions from Hugo Jaegler (2013)
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
import DSMTk

global root


root = Tk.Tk()
fenetrePath = DSMTk.Welcome(root)
root.geometry("1024x1400")
root.mainloop()



root.mainloop()




#img = ImageTk.PhotoImage(Image.open("MuseSeLFiE.png"))
#panel = Tk.Label(root, image = img)
#
#panel.pack(side="bottom", fill="both", expand="yes")

#menuButton.grid(row=0, column=0)
root.mainloop()


