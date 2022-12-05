'''
Pre-/post- processing GUI facilities for DSM kernel

2022 Nobuaki Fuji

with contributions from Hugo Jaegler (2013)
inspired by Stephanie Durand (2017)

'''

#from babel.support import Translations
# Of course this software should be multilingual soon!!

import tkinter as Tk
from PIL import ImageTk, Image
from tkinter import filedialog
import os

def openfn():
    filename = filedialog.askopenfilename(title='open')
    return filename
def open_img():
    x = openfn()
    img = Image.open(x)
    img = img.resize((100, 250), Image.ANTIALIAS)
    img = ImageTk.PhotoImage(img)
    panel = Label(root, image=img)
    panel.image = img
    panel.pack()

root = Tk.Tk()
root.geometry("550x300+300+150")
root.resizable(width=True,height=True)
root.title('DSM Kernel')
img = ImageTk.PhotoImage(Image.open("./MuseSeLFiE.png"))
panel=Tk.Label(root,image=img)
panel.pack(side="bottom",fill="both",expand="yes")
menuButton = Tk.Button(root, text='Menu')
btn = Tk.Button(root, text='open image', command=open_img).pack()

root.mainloop()




#img = ImageTk.PhotoImage(Image.open("MuseSeLFiE.png"))
#panel = Tk.Label(root, image = img)
#
#panel.pack(side="bottom", fill="both", expand="yes")

#menuButton.grid(row=0, column=0)
root.mainloop()


