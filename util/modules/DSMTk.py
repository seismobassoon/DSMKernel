import tkinter as Tk
from PIL import ImageTk, Image
from tkinter import filedialog
import os

global parentDir

parentDir = os.getcwd()

class SampleApp(Tk.Tk):
    def __init__(self):
        Tk.Tk.__init__(self)
        self._frame = None
        self.switch_frame(Welcome)

    def switch_frame(self, frame_class):
        """Destroys current frame and replaces it with a new one."""
        new_frame = frame_class(self)
        if self._frame is not None:
            self._frame.destroy()
            self._frame = None
        self._frame = new_frame
        self._frame.pack()

def openfn():
    filename = filedialog.askopenfilename(title='open')
    return filename
def open_img():
    x = openfn()
    img = Image.open(x)
    img = img.resize((193.5, 40), Image.ANTIALIAS)
    img = ImageTk.PhotoImage(img)
    panel = Label(master, image=img)
    panel.image = img
    panel.pack()


class Welcome(Tk.Frame):
    def __init__(self, master):
        Tk.Frame.__init__(self, master)
        file=parentDir+'/images/MuseSeLFiE.png'
        #print(file)
        #master.resizable(width=True,height=True)
        #Tk.title('DSM Kernel')
        self.img = ImageTk.PhotoImage(Image.open(file).resize((750,384)),Image.ANTIALIAS)
        self.panel=Tk.Label(master,image=self.img)
        self.panel.img=self.img
        #panel.pack(side="top",fill="both",expand="yes")
        # create a dummy button
        self.button = Tk.Button(master,image=self.img,borderwidth=0,
                           command=lambda: master.switch_frame(Path))
        self.button.pack()
        self.canvas=Tk.Canvas(master,width=800,height=300)
        self.canvas.create_text(412,30,text="Welcome to DSM Kernel Suite 2022!", fill="black", font=('Helvetica 15 bold'))
        self.canvas.pack()
        self.canvas.create_text(312,70,text="This software is the fruit long-lasting works! Please cite: Fuji et al. 2012; XXX", fill="black", font=('Helvetica 12'))
        self.canvas.pack()
        #text = Tk.Label(root0,text="")
        #text.pack()

class PageOne(Tk.Frame):
    def __init__(self, master):
        Tk.Frame.__init__(self, master)
        Tk.Label(self, text="This is page one").pack(side="top", fill="x", pady=10)
        Tk.Button(self, text="Return to start page",
                  command=lambda: master.switch_frame(Welcome)).pack()


class Path(Tk.Frame):
    def __init__(self, root):
        Tk.Frame.__init__(self, root)        
        self.canvas = Tk.Canvas(root, borderwidth=1, background="#ffffff")
        self.frame = Tk.Frame(self.canvas, background="#ffffff")
        self.vsb = Tk.Scrollbar(root, orient="vertical", command=self.canvas.yview)
        self.canvas.configure(yscrollcommand=self.vsb.set)
        self.vsb.pack(side="right", fill="y")
        self.canvas.pack(side="left", fill="both", expand=True)
        
        self.canvas.create_window((4,4), window=self.frame, anchor="nw", tags="self.frame")

        self.frame.bind("<Configure>", self.OnFrameConfigure)

        self.data()

        Tk.Button(self, text="Return to start page",
                  command=lambda: master.switch_frame(Welcome)).pack()
        
    def data(self): 
        
        global textPath
        textPath = StringVar()
        global text0a
        text0a = StringVar()
        global text0b 
        text0b = StringVar()
        global text2a 
        text2a = StringVar()
        global text3 
        text3 = StringVar()
        global alphaVar
        alphaVar = IntVar()
        global betaVar 
        betaVar = IntVar()
        global allVar
        allVar = IntVar()
        global text6a
        text6a = "0"
        global filterVar
        filterVar = IntVar()
        global text6b
        text6b = StringVar()
        global t1x
        t1x = ""
        global t2x
        t2x = ""
        global t3x
        t3x = ""
        global t4x
        t4x = ""
        global text8_0
        text8_0 = StringVar()
        global text8_1
        text8_1 = StringVar()
        
        Label(self.frame,text="Path ? ").grid(row=0, column=0)
        Entry(self.frame,textvariable=textPath).grid(row=1, column=0)
        Button(self.frame, text="Valider et afficher", command = affiche_recap).grid(row=1, column=1)
    
        Label(self.frame, text="Green function database information file\n (for a certain depth only for the instance) ?").grid(row=3)
        Entry(self.frame, textvariable=text0a).grid(row=4)
        
        Label(self.frame, text="Output directory (parentdir) ?").grid(row=5)
        Entry(self.frame, textvariable=text0b).grid(row=6)
            
        Label(self.frame, text="Phase name ?").grid(row=9)
        Entry(self.frame, textvariable=text3).grid(row=10)
        
        def afficheAlpha():
            seismicPara["text"]="alpha"
            betaVar.set(0)
            allVar.set(0)
        def afficheBeta():
            seismicPara["text"]="beta"
            alphaVar.set(0)
            allVar.set(0)
        def afficheAll():
            seismicPara["text"]="all"
            alphaVar.set(0)
            betaVar.set(0)
        
        seismicPara = Menubutton(self.frame, text="Seismic Parameter", relief=RAISED)
        seismicPara.grid(row=0)
        seismicPara.menu = Menu(seismicPara, tearoff = 0)
        seismicPara["menu"] = seismicPara.menu

        
        seismicPara.menu.add_checkbutton(label="alpha", variable = alphaVar, command = afficheAlpha)
        seismicPara.menu.add_checkbutton(label="beta", variable = betaVar, command = afficheBeta)
        seismicPara.menu.add_checkbutton(label="all", variable = allVar, command = afficheAll)
        seismicPara.grid(row=11)
        
        
        
        Label(self.frame, text="Filter name ?").grid(row=12)
        Entry(self.frame, textvariable=text6b).grid(row=13)
        
        
        
        Label(self.frame, text="time window t1 ?").grid(row=14)
        Labelt1 = Label(self.frame, text="-->").grid(row=15)
        Button(self.frame, text="time 1", command=self.time1).grid(row=15, column=1)
        
        Label(self.frame, text="time window t2 ?").grid(row=16)
        Labelt1 = Label(self.frame, text="-->").grid(row=17)
        Button(self.frame, text="time 2", command=self.time2).grid(row=17, column=1)
        '''
        Label(self.frame, text="time window t3 ?").grid(row=18)
        Labelt1 = Label(self.frame, text="-->").grid(row=19)        
        Button(self.frame, text="time 3", command=self.time3).grid(row=19, column=1)
        
        Label(self.frame, text="time window t4 ?").grid(row=20)
        Labelt1 = Label(self.frame, text="-->").grid(row=21)
        Button(self.frame, text="time 4", command=self.time4).grid(row=21, column=1)
        '''
        def affiche0():
            convertPara["text"]="No conversion"
            text8_1.set(0)
            
        def affiche1():
            convertPara["text"]="Conversion"
            text8_0.set(0)
    
        convertPara = Menubutton(self.frame, text="Geodetic latitude to geocentric latitude conversion", relief=RAISED)
        convertPara.grid(row=0)
        convertPara.menu = Menu(convertPara, tearoff = 0)
        convertPara["menu"] = convertPara.menu

        convertPara.menu.add_checkbutton(label="No conversion", variable = text8_0, command = affiche0)
        convertPara.menu.add_checkbutton(label="Conversion", variable = text8_1, command = affiche1)
        
        convertPara.grid(row=22)
        b = Checkbutton(self.frame, text = "apply filter", variable = filterVar)
        b.grid(row=23, column = 0)
        Button(self.frame, text="continue", command=self.quitter).grid(row=23, column=1)
        
    def time1(self):
        global t1x
        global t1y
        t1x, t1y = Pointage()
        print (type(t1x))
        print (t1y)

    def time2(self):
        global t2x
        global t2y
        t2x, t2y = Pointage()
        print (t2x)
        print (t2y)
            
    def time3(self):
        t3x, t3y = Pointage()
        print (t3x)
        print (t3y)
            
    def time4(self):
            t4x, t4y = Pointage()
            print (t4x)
            print (t4y)
            
            
    def quitter(self):
        root.destroy()
    
    
    def OnFrameConfigure(self, event):
        '''Reset the scroll region to encompass the inner frame'''
        self.canvas.configure(scrollregion=self.canvas.bbox("all"))
        
class RecapCalculs:
    '''
    Interface graphique recapitulant les caracteristique du sismogramme
    presentant les options de filtrage et de calculs du noyau de sensibilite
    '''
    def __init__(self,root):
        self.canvas = Canvas(root, borderwidth=1, background="#ffffff")
        self.frame = Frame(self.canvas, background="#ffffff")
        self.vsb = Scrollbar(root, orient="vertical", command=self.canvas.yview)
        self.canvas.configure(yscrollcommand=self.vsb.set)
        self.vsb.pack(side="right", fill="y")
        self.canvas.pack(side="left", fill="both", expand=True)
        self.canvas.create_window((4,4), window=self.frame, anchor="nw", tags="self.frame")

        self.frame.bind("<Configure>", self.OnFrameConfigure)

        self.data()
    def data(self):
        
        self.message = Label(self.frame, text="Recapitulatif du sismogramme").grid(row=0)
       
        self.recap = Listbox(self.frame, height = 15, width = 50)
           
        self.recap.insert(1, "network: {}\n".format(X[0].stats.network))
        self.recap.insert(2, "station: {}\n".format(X[0].stats.station))
        self.recap.insert(3, "location: {}\n".format(X[0].stats.location))
        self.recap.insert(4, "channel: {}\n".format(X[0].stats.channel))
        self.recap.insert(5, "start time: {}\n".format(X[0].stats.starttime))
        self.recap.insert(6, "end time: {}\n".format(X[0].stats.endtime))
        self.recap.insert(7, "sampling rate: {}\n".format(X[0].stats.sampling_rate))
        self.recap.insert(8, "delta: {}\n".format(X[0].stats.delta))
        self.recap.insert(9, "number points: {}\n".format(X[0].stats.npts))
        self.recap.insert(10, "calibration: {}\n".format(X[0].stats.calib))
        self.recap.insert(11, "event latitude: {}\n".format(X[0].stats.sac.evla))
        self.recap.insert(12, "event longitude: {}\n".format(X[0].stats.sac.evlo))
        self.recap.insert(13, "event depth: {}\n".format(X[0].stats.sac.evdp))
        self.recap.insert(14, "station latitude: {}\n".format(X[0].stats.sac.stla))
        self.recap.insert(15, "station longitude: {}\n".format(X[0].stats.sac.stlo))
        self.recap.grid(row=0)       
        
    def OnFrameConfigure(self, event):
        '''Reset the scroll region to encompass the inner frame'''
        self.canvas.configure(scrollregion=self.canvas.bbox("all"))
  
        
def affiche_recap():
    global X
    X=read(textPath.get())
    plt.figure(1)
    t = np.arange(0, X[0].stats.npts / X[0].stats.sampling_rate, X[0].stats.delta)
    plt.subplot(111)
    plt.plot(t, X[0].data, 'k')
    plt.ylabel('Raw Data')
    plt.xlabel('Time [s]')
    plt.suptitle(textPath.get())
    plt.show()

    root = Tk()   
    fenetre = RecapCalculs(root)
    root.geometry("500x300+200+0")
    root.mainloop()
    
    
class filterswindow:
    '''
    Interface graphique recapitulant les caracteristique du sismogramme
    presentant les options de filtrage et de calculs du noyau de sensibilite
    '''
    def __init__(self,racine):
        self.canvas = Canvas(racine, borderwidth=1, background="#ffffff")
        self.frame = Frame(self.canvas, background="#ffffff")
        self.vsb = Scrollbar(racine, orient="vertical", command=self.canvas.yview)
        self.canvas.configure(yscrollcommand=self.vsb.set)
        self.vsb.pack(side="right", fill="y")
        self.canvas.pack(side="left", fill="both", expand=True)
        self.canvas.create_window((4,4), window=self.frame, anchor="nw", tags="self.frame")

        self.frame.bind("<Configure>", self.OnFrameConfigure)

        self.data()
            
    def data(self):
        global filterVar
        filterVar = 1
        global text6a
        text6a = "1"
        global text6c1
        text6c1 = StringVar()
        global text6c2
        text6c2 = StringVar()
        global text6c3
        text6c3 = StringVar()
           
        Label(self.frame, text="Option Filter").grid(row=0)
        Label(self.frame, text="\n").grid(row=1)
        
        Label(self.frame, text="lowest frequency ?").grid(row=4)
        e1 = Entry(self.frame, textvariable=text6c1)
        e1.grid(row=5)
        
        Label(self.frame, text="highest frequency ?").grid(row=20)
        e2 = Entry(self.frame, textvariable=text6c2)
        e2.grid(row=21)
        
        Label(self.frame, text="number of poles ?").grid(row=22)
        e3 = Entry(self.frame, textvariable=text6c3)
        e3.grid(row=23)
                    
        Button(self.frame, text="continue", command=self.quitter).grid(row=24)
                  
    def quitter(self):
        global racine
        racine.destroy()
        afficheSismoFiltre(textPath.get(), float(text6c1.get()), float(text6c2.get()), float(text6c3.get()))
            
    def OnFrameConfigure(self, event):
        '''Reset the scroll region to encompass the inner frame'''
        self.canvas.configure(scrollregion=self.canvas.bbox("all"))
        
def optionFilter():
    global racine
    
    racine = Tk()   
    fenetre = filterswindow(racine)
    racine.geometry("500x300+200+0")
    racine.mainloop()    
    
    
def afficheSismoFiltre(Path, frequenceMin, frequenceMax, nbPoles):
    
    # Read the seismogram
    st = read(Path)

    # There is only one trace in the Stream object, let's work on that trace...
    tr = st[0]

    # Filtering with a lowpass on a copy of the original Trace
    tr_filt = tr.copy()
    tr_filt.filter('bandpass', freqmin = frequenceMin, freqmax = frequenceMax, corners = nbPoles, zerophase=True)

    # Now let's plot the raw and filtered data...
    t = np.arange(0, tr.stats.npts / tr.stats.sampling_rate, tr.stats.delta)
    plt.subplot(211)
    plt.plot(t, tr.data, 'k')
    plt.ylabel('Raw Data')
    plt.subplot(212)
    plt.plot(t, tr_filt.data, 'k')
    plt.ylabel('filtered Data')
    plt.xlabel('Time [s]')
    plt.suptitle(tr.stats.starttime)
    plt.show()



class LineBuilder :
    '''
    Fonction permettant de creer des lignes verticales sur un sismo et
    d'enregistrer les coordonees des endroits pointes sur la courbe
    ''' 
    def __init__(self, line):
        self.line = line
        self.xs = list(line.get_xdata())
        self.ys = list(line.get_ydata())
        line.figure.canvas.mpl_connect('button_press_event', self)
        
    def __call__(self, event):
        print ('click', event)
        if event.inaxes!=self.line.axes: return
                
        self.xs = [event.xdata, event.xdata]
        self.ys = [ymin, ymax]
        self.line.set_data(self.xs, self.ys)
        self.line.figure.canvas.draw()
        return True
        
def onclick(event): 
    if enregx == []:
        enregx.append(event.xdata)
        enregy.append(X[0].data[event.xdata])
        vlines(event.xdata, ymin, ymax, color='k', linestyles='dashed')
      
def Pointage():
    global enregx
    global enregy
    enregx=[]
    enregy=[]
    
    
        
    global X
    X=read(textPath.get())   
    fig = plt.figure()
    ax = fig.add_subplot(111)
    ax.set_title('click on graphic to store data')
    line, = ax.plot(np.arange(0, len(X[0])) , X[0].data, 'k')
    plt.xlim(0, len(X[0]))
    global ymin
    global ymax
    ymin, ymax = plt.ylim(X[0].data.min()*1.1, X[0].data.max()*1.1)
    
    fig.canvas.mpl_connect('button_press_event', onclick)
        
    line, = ax.plot([0], [0])
    linebuilder = LineBuilder(line)
    plt.show()
    print (enregx)    # Abscisse des points selectionnes 
    print (enregy)    # Ordonnees lues sur le sismo
    
    return enregx[0], enregy[0]


