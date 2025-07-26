#!/bin/usr/python3
######################################################
# Import libraries                                   #
######################################################
import sys
import platform
if (platform.system() == 'Linux') :
    import distro
import subprocess
from subprocess import Popen, PIPE
import contextlib
import os
import datetime
import pandas as pd
import numpy as np
from numpy import exp, pi, sqrt
from lmfit import Model
from os.path import exists

import types
import pyqtgraph as pg
from pyqtgraph.graphicsItems.LegendItem import ItemSample
from pyqtgraph.graphicsItems.ScatterPlotItem import drawSymbol

from PyQt5 import QtWidgets, QtCore, QtGui, QtPrintSupport
from PyQt5.QtCore    import QSize, Qt, QRect, QPoint, QLocale
from PyQt5.QtGui     import (QColor, QTextCursor, QTextBlock, QRegion,
        QIcon, QKeySequence, QPen, QImage, QScreen, 
        QImageWriter, QPalette, QFont)
from PyQt5.QtGui import QPainter
        
from PyQt5.QtWidgets import (QApplication, QCheckBox, QGroupBox,
        QMenu, QPushButton, QRadioButton, QHBoxLayout, QWidget,
        QLabel, QSlider, QGraphicsView, QFileDialog, QMessageBox,
        QAction, qApp, QShortcut, QToolBar, QMainWindow, QDialog,
        QVBoxLayout, QGridLayout, QInputDialog, QSpinBox, 
        QComboBox, QGraphicsScene)
from matplotlib import cm
# next lines in view of backward compatibility with older matplotlib versions
try: #2024-01-18 this to allow new colormaps call; if except, older cm.get_colormap will be used instead
    from matplotlib import colormaps 
except Exception: 
    pass
import matplotlib.pyplot as plt
import colorcet as cc

from scipy import interpolate
from scipy import stats
from scipy.stats import moment
#from functools import partial
#from sys import argv
import argparse
import math

from PyPDF2 import PdfReader
# next is used for icon display on taskbar in Windows
import ctypes


#import tkinter as tk
#from tkinter import ttk

######################################################
# set the DGUI version and its date                  #
######################################################
dguiv = "DGUI V2R7"
dguid = "28-Dec-2024"
dgui_v = dguiv + " " + dguid

######################################################
# set the default dynac version                      #
######################################################
global dynacv, systembin
dynacv = "dynacv7"

######################################################
# Initialize some parameters                         #
######################################################
global inter_selected, KDE_selected, acr_selected, evi_selected, xrd_selected, n_of_KDE_bins, pro_raw, pro_fit
global rangex, rangexp, rangey, rangeyp, rangez, rangezp
global xvals, xpvals, yvals, ypvals, zvals, zpvals, GRS, fit_amp, plot_ellipse
global emivals_selected, emivals_bottom, ABS_selected, COG_selected, NRMS
global lut, colormap_name, last_dfpath, last_ifpath
global prdir, prnumber, was_in_spbox
global gr_file_ext, sw0, sw1, sw2, sw3, versiontxt, mac_os_names

gr_file_ext="png"
        
prdir = ""
was_in_spbox = False
project_name="project"
# Unix, Windows and old Macintosh end-of-line
newlinechars = ['\n', '\r\n', '\r']
ifpath=""
dgpath=""
ifname=""
dfname=""
prnumber = 1
# MAC OS names (because programatically, there is no easy way to get this out of a MAC)
mac_os_names = ['Cheetah','Puma','Jaguar','Panther','Tiger','Leopard','Snow Leopard',
                'Lion','Mountain Lion','Mavericks','Yosemite','El Capitan','Sierra',
                'High Sierra','Mojave','Catalina','Big Sur','Monterey','Ventura',
                'Sonoma','Sequoia']

# dfpath is distribution file path, ifpath is input file path
default_dfpath=""
last_dfpath=""
last_ifpath=""
# next line sets default color map to be used by dynac gui for density plots
colormap_name = "default"
SV1=0
inter_selected = True
KDE_selected = False
# initialize PDF viewer selection
acr_selected = False
evi_selected = False
xrd_selected = False
if (platform.system() == 'Linux') :
    xrd_selected = True
#    print("linux PDF by xrd")
if (platform.system() == 'Windows') :
    acr_selected = True
    #below to allow icon to show on w10 taskbar
    myappid = 'mycompany.myproduct.subproduct.version'
    # use next line instead of previous line for w11
    #myappid = 'mycompany.myproduct.subproduct.version'
    ctypes.windll.shell32.SetCurrentProcessExplicitAppUserModelID(myappid)

#    print("Windows PDF by acr")
if (platform.system() == 'Darwin') :
    acr_selected = True
#    print("MAC PDF by acr")
#
emivals_selected = False
emivals_bottom = False
n_of_KDE_bins = 50
fit_amp = 16./3.
GRS = "Auto"
ABS_selected = True
COG_selected = False
pro_raw = True
pro_fit = False
rangex = False
rangexp = False
rangey = False
rangeyp = False
rangez = False
rangezp = False
plot_ellipse = False
# mass units (in MeV)
xmat_p    = 938.27231
xmat_hmin = 939.301404
xmat_hi   = 931.4940954
xmat_e    =   0.510998928
amu = 1
ener = 0.
xmat = xmat_p 
NRMS = 6.

######################################################
# Define command line options                        #
######################################################
dgparser = argparse.ArgumentParser(description=dgui_v)
dgparser.add_argument('-v', help="show DGUI version number and exit", action='store_const', const=dgui_v)
dgparser.add_argument('--version', help="show DGUI version number and exit", action='version', version=dguiv)
dgparser.add_argument('-p',help='here P is the path to the location of dgui.ini; no space between -p and the path! -pP is mandatory if none of the other optional arguments are used', action='store', nargs=1)

######################################################
# Get command line options and check dgui.ini        #
######################################################
args = dgparser.parse_args()
if (args.v != None):
    print(args.v)
    dgparser.exit(status=0, message=None)
if (args.p == None):
    print('No -p argument given')
    print("DGUI requires an argument; for help type:")
    if (platform.system() == 'Windows') :
        print("python dgui.py -h")
    else:
        print("python3 dgui.py -h")
    dgparser.exit(status=0, message=None)
dgpath=args.p[0]
xvals=np.zeros(2)
xpvals=np.zeros(2)
yvals=np.zeros(2)
ypvals=np.zeros(2)
zvals=np.zeros(2)
zpvals=np.zeros(2)
rangex  = True
rangexp = True
rangey  = True
rangeyp = True
rangez  = True
rangezp = True
xvals[0]=-2.
xvals[1]=2.
xpvals[0]=-200.
xpvals[1]=200.
yvals[0]=-2.
yvals[1]=2.
ypvals[0]=-200.
ypvals[1]=200.
zvals[0]=-180.
zvals[1]=180.
zpvals[0]=0.
zpvals[1]=20.
#***************
#stuff related to reading Type Code texts from DYNAC UG
numtct = 10  # maximum number of TypeCode types
maxntc = 20  # maximum number of TypeCodes for a given TypeCode type
TypeCodeTypes  = np.empty(numtct, dtype=object)
NumOfTypeCodes = np.empty(numtct, dtype=object)
# TypeCodes first  index corresponds to TypeCode type, e.g. Input Beam (there are 10 of them)
# TypeCodes second index will contain the TypeCode name, e.g. CAVNUM
TypeCodes = np.empty(shape=(numtct,maxntc), dtype=object)
TypeCodeText = np.empty(shape=(numtct,maxntc), dtype=object)
TC_HelpAction = np.empty(shape=(numtct,maxntc), dtype=object)
#TC_HelpAction = {}{}
chapter6 = np.empty(shape=(4000), dtype=object)
newtext = np.empty(shape=(400), dtype=object)

try:
    with open(dgpath + os.sep + "dgui.ini") as fp:  
        line = fp.readline()
        dynpathff=line
        dynpath = dynpathff

        cnt = 1
        while line:
            sline=line.strip()
            if "COLORMAP" in sline:
                colormap_name=sline[9:len(sline)]
            if "DYNACVERSION" in sline:
                dynacv=sline[13:len(sline)]
#                print('DEBUG ',dynacv)
            if "PDFVIEWER" in sline:
                if "evince" in sline:
                    acr_selected = False
                    evi_selected = True
                    xrd_selected = False
#                    print("evince selected in .ini as PDF viewer")
                elif "acrobat" in sline: 
                    acr_selected = True   
                    evi_selected = False
                    xrd_selected = False
#                    print("Acrobat Reader selected in .ini as PDF viewer")
                elif "xreader" in sline: 
                    acr_selected = False   
                    evi_selected = False
                    xrd_selected = True   
#            print("Line {}: {}".format(cnt, sline))
            if "PROFILES" in sline:
                if "fit" in sline:
                    pro_fit = True
                    pro_raw = False
            if "PROJECTDIR" in sline:
                prdir = sline[11:]
#                print("PROJECTDIR=",prdir)
            if "RANGES" in sline:
                if " X " in sline:
                    fline=""
                    fline = sline[3+sline.find(" X "):]
                    xvals = [float(x) for x in fline.split()]
#                    print("X  limits",xvals[0],xvals[1])
                    rangex = True
                if " XP " in sline:
                    fline=""
                    fline = sline[3+sline.find(" XP "):]
                    xpvals = [float(x) for x in fline.split()]
#                    print("XP limits",xpvals[0],xpvals[1])
                    rangexp = True
                if " Y " in sline:
                    fline=""
                    fline = sline[3+sline.find(" Y "):]
                    yvals = [float(x) for x in fline.split()]
#                    print("Y  limits",yvals[0],yvals[1])
                    rangey = True
                if " YP " in sline:
                    fline=""
                    fline = sline[3+sline.find(" YP "):]
                    ypvals = [float(x) for x in fline.split()]
#                    print("YP limits",ypvals[0],ypvals[1])
                    rangeyp = True
                if " Z " in sline:
                    fline=""
                    fline = sline[3+sline.find(" Z "):]
                    zvals = [float(x) for x in fline.split()]
#                    print("Z  limits",zvals[0],zvals[1])
                    rangez = True
                if " ZP " in sline:
                    fline=""
                    fline = sline[3+sline.find(" ZP "):]
                    zpvals = [float(x) for x in fline.split()]
#                    print("ZP limits",zpvals[0],zpvals[1])
                    rangezp = True
            if "INPUTFILEPATH" in sline:
                last_ifpath = sline[14:]
                #print("INPUTFILEPATH=",last_ifpath)
            if "DISTRIBUTIONFILEPATH" in sline:
                last_dfpath = sline[21:]
                #print("DISTRIBUTIONFILEPATH=",last_dfpath)
            line = fp.readline()
            cnt += 1
except OSError as e:
    erno = e.errno 
    if(erno == 2):
#        emtxt = dgpath + os.sep + "dgui.ini"
#        emsg1 = QMessageBox()
#        emsg1.setIcon(self,QMessageBox.critical)
#        emsg1.setText("File not found:\n'%s'" % emtxt)
#        emsg1.setWindowTitle("Error Message")
#        emsg1.exec()                                 
        print("ERROR: " + dgpath + os.sep + "dgui.ini : file not found")
    else:
        print('ERROR ', erno,' on opening dgui.ini')
        
    
if (platform.system() == 'Windows') :
    dynpath=dynpath[:-4] + "bin"
    default_dfpath=dynpath[:-3] + "datafiles"
    if (last_dfpath == ""):
        last_dfpath=default_dfpath
    if (last_ifpath == ""):
        last_ifpath=default_dfpath
    default_ugpath=dynpath[:-3] + "help"
    dynpath=dynpathff    
else:
    dynpath=dynpath[:-4] + "bin"
    default_dfpath=dynpath[:-3] + "datafiles"
    if (last_dfpath == ""):
        last_dfpath=default_dfpath
    if (last_ifpath == ""):
        last_ifpath=default_dfpath
    default_ugpath=dynpath[:-3] + "help"

myplatform=platform.system()
systembin="DYNAC GUI running on " + platform.system() + " with DYNAC binary in " + dynpath

######################################################
# Remove page header and footer, if present in text  #  
######################################################
def remove_haf(mytext):
    """ remove header and footer: remove_haf(mytext)"""
    nolit = len(mytext.split('\n'))
    mytext_list = mytext.split('\n')
    longstring = ''
    klm = 0
    nlc = 0
    jj = 0    
    while klm < nolit :
        if('DYNAC V' in mytext_list[klm]):
            nlc = nlc -1
        else:
            newtext[nlc] = mytext_list[klm]
            nlc = nlc + 1
        klm = klm + 1
    while jj < nlc :
        longstring = longstring + '\n' + newtext[jj]
        jj = jj + 1 
    return longstring

######################################################
# Get Type Code descriptions from DYNAC UG pdf       #  
######################################################
def get_tcd(pdf_document):
    """ Get Type Code descriptions: get_tcd(pdf_document)"""
    c6i = 0
    with open(pdf_document, "rb") as filehandle:
        store_lines = False
        store_tclines = False
        pdf = PdfReader(filehandle)
        num_of_pages = len(pdf.pages)
#    num_of_pages = 28
        tcnum=0
        tct=0
        done_with_index = False 
        found_tc_start = False 
        not_done_with_tc = True
    
        for page_number in range(num_of_pages):
            page = pdf.pages[page_number]
            pagetext = page.extract_text()
            pagesplit = pagetext.splitlines(True)
            nolines = len(pagetext.splitlines())
            linenum = 0
# Get number of Type Codes types and Type Codes from the Index pages        
            while (linenum < nolines and not done_with_index) :
                if 'DESCRIPTION OF THE GRAPHICS POST PROCESSOR PLOTIT' in pagesplit[linenum].rstrip():
                     done_with_index = True
                     store_lines = False
                if (store_lines):
                    # now we can get and store text
                    newline = pagesplit[linenum].rstrip()
                    if(len(newline) > 2 and newline[1] == '.'): 
                        if(newline[3] != '.' and newline[4] != '.'): #title is a type code type
                            TypeCodeTypes[tct] = newline[8:]
                            tct = tct + 1
                            tcnum = 0
                        elif "TYPE CODE:" in newline: #title is of a type code, extract its name   
                            newline = newline[(2+newline.index(':')):]
                            TypeCodes[tct-1][tcnum] = newline
                            tcnum = tcnum + 1
                            NumOfTypeCodes[tct-1] = tcnum
                if (pagesplit[linenum].rstrip() == '6     DESCRIPTION OF AVAILABLE TYPE CODE ENTRIES'):
                    store_lines = True
                linenum = linenum + 1
############################            
# find where Type Codes start (chapter 6) 
############################            
            linenum = 0        
            while (linenum < nolines and done_with_index and not found_tc_start) :
                newline = pagesplit[linenum].rstrip()
                if (newline == '6.1 INPUT BEAM'):
                    found_tc_start = True
                    tcnum = 0
                linenum = linenum + 1            
############################            
# Get text until end of chapter (i.e. start of chapter 7) encountered
############################            
            done_with_tc_in_tct = False
            while (linenum < nolines and done_with_index and found_tc_start and not_done_with_tc ) :
                newline = pagesplit[linenum].rstrip()
                if 'DESCRIPTION OF THE GRAPHICS POST PROCESSOR PLOTIT' in newline:
                    not_done_with_tc = False                    
                else:
                    chapter6[c6i] = newline
                    c6i = c6i + 1
                linenum = linenum + 1           
############################            
        j = 0
        while j < NumOfTypeCodes[0] :
            TypeCodeText[0][j] = ''
            j = j + 1
        linenum = 0
        i = 0
        typect = 0
        tcnum = -1
        j = 0
        while linenum < c6i :
            newline = chapter6[linenum]
            ijk = 0
            found_tct = False
            while ijk < tct :
                if(TypeCodeTypes[ijk] in newline): found_tct = True   
                ijk = ijk + 1         
            if(found_tct): #title is a type code type
                if("SPACE CHARGE COMPUTATION" in newline): # read lines between this tct and the first type code
                    while (not "TYPE CODE:" in newline):
                        linenum = linenum + 1
                        newline = chapter6[linenum]
                    tcnum = 0
                else:
                    tcnum = -1            
                typect = typect + 1 
                j = 0
                while j < NumOfTypeCodes[typect] :
                    TypeCodeText[typect][j] = ''
                    j = j + 1
            elif("TYPE CODE:" in newline): #title is of a type code, extract its name   
                tcnum = tcnum + 1
            else:
                TypeCodeText[typect][tcnum] = TypeCodeText[typect][tcnum] + newline + '\r\n'
            linenum = linenum + 1
####
        i = 0
        while i < tct :
            j = 0
            while j < NumOfTypeCodes[i] :
                TypeCodeText[i][j] = remove_haf(TypeCodeText[i][j]) # remove header and footer           
                j = j + 1                    
            i = i + 1        

######################################################
# Function for isometric plots (replacement for      #
# interp2d)                                          #
######################################################
#def createInterpolation(x,y,param,param_err):
def createInterpolation(x,y,param):
    x = x
    y = y
    param = param
#    param_weights=1/param_err
#    spl = interpolate.SmoothBivariateSpline(x,y,param,w=param_weights)
    spl = interpolate.RectBivariateSpline(x, y, param, kx=1, ky=1)
    #spl = NearestNDInterpolator(list(zip(x, y)),param)
    return spl


######################################################
# Gaussian for fitting purposes                      #
######################################################
def gaussian(x, amp, cen, wid, lof):
    """1-d gaussian: gaussian(x, amp, cen, wid, lof)"""
    return lof + amp * exp(-(x-cen)**2 / (2*wid**2))

######################################################
# myLegendPaint colors the legend backround          #
######################################################
def myLegendPaint(self, p, *args):
    p.setPen(pg.mkPen(0,0,0)) # outline
    p.setBrush(pg.mkBrush(255,255,255))   # background
    p.drawRect(self.boundingRect())

######################################################
# myLegendSample class                               #
######################################################
class myLegendSample(ItemSample):
    def __init__(self, item):
        super().__init__(item)

    def paint(self, p, *args):
        opts = self.item.opts

        if opts.get('fillLevel',None) is not None and opts.get('fillBrush',None) is not None:
            p.setBrush(pg.mkBrush(opts['fillBrush']))
            p.setPen(pg.mkPen(None))
            p.drawPolygon(QtGui.QPolygonF([QtCore.QPointF(2,10), QtCore.QPointF(18,10), QtCore.QPointF(18,18), QtCore.QPointF(2,18)]))

        if not isinstance(self.item, pg.ScatterPlotItem):
            p.setPen(pg.mkPen(opts['pen']))
            p.drawLine(2, 10, 18, 10)

        symbol = opts.get('symbol', None)
        if symbol is not None:
            if isinstance(self.item, pg.PlotDataItem):
                opts = self.item.scatter.opts


            pen = pg.mkPen(opts['pen'])
            brush = pg.mkBrush(opts['brush'])
            size = opts['size']

            p.translate(10,10)
            path = drawSymbol(p, symbol, size, pen, brush)
               
######################################################
# Define LUT for selected colormap                   #
######################################################
def cm_lut(cmn,show_thr):
    """lut for color map: cm_lut(cmn,show_thr)"""
# cmn is the color map name    
    global lut
    # Get the colormap
    if(cmn == "default"): 
    # Get bottom part of colormap
# 2024-01-17 get_cmap will be deprecated; replace with new call
        try:
            cmapbot = colormaps["RdPu"]  
        except:
            cmapbot = cm.get_cmap("RdPu")
        cmapbot._init()
        lutbot = (cmapbot._lut * 255).view(np.ndarray)  # Convert matplotlib colormap from 0-1 to 0 -255 for Qt
        # Get central part of colormap
        try:
            colormap = colormaps["nipy_spectral"]  
        except:
            colormap = cm.get_cmap("nipy_spectral")
        colormap._init()
        lut = (colormap._lut * 255).view(np.ndarray)  # Convert matplotlib colormap from 0-1 to 0 -255 for Qt
        # Get top part of colormap
        try:
            cmaptst = colormaps["Reds"]  
        except:
            cmaptst = cm.get_cmap("Reds")
        cmaptst._init()
        luttop = (cmaptst._lut * 255).view(np.ndarray)  # Convert matplotlib colormap from 0-1 to 0 -255 for Qt
        #overwrite bottom part of lut
        indx=1
        lim=15
        while indx < lim+1:
            lut[lim-indx,0]=lutbot[228-10*indx,0]
            lut[lim-indx,1]=lutbot[228-10*indx,1]
            lut[lim-indx,2]=lutbot[228-10*indx,2]
            lut[lim-indx,3]=lutbot[228-10*indx,3]
            indx = indx + 1
        #overwrite top part of lut
        indx=240
        delta=50
        lim=255
        step=0
        while indx < lim+1:
            lut[indx,0]=luttop[indx-delta+step*3,0]
            lut[indx,1]=luttop[indx-delta+step*3,1]
            lut[indx,2]=luttop[indx-delta+step*3,2]
            lut[indx,3]=luttop[indx-delta+step*3,3]
            indx = indx + 1
            step=step+1
        #make sure level zero = white
        lut[0,0]=255.
        lut[0,1]=255.
        lut[0,2]=255.
        lut[0,3]=255.
    else: 
        cmtype=0
        if((cmn == "nipy_spectral") or (cmn == "viridis") or (cmn == "gist_stern") or 
           (cmn == "jet") or (cmn == "jet_white")):
            cmtype=1
            if(cmn=="jet_white"):
                try:
                    colormap = colormaps["jet"]   
                except:
                    colormap = cm.get_cmap("jet")
            else:
                try:
                    colormap = colormaps[cmn]   
                except:
                    colormap = cm.get_cmap(cmn)
            colormap._init()
            lut =  (colormap._lut * 255).view(np.ndarray)  # Convert matplotlib colormap from 0-1 to 0-255 for Qt
            olut = (colormap._lut * 255).view(np.ndarray)  # Convert matplotlib colormap from 0-1 to 0-255 for Qt
            indx=1
            lim=255
            while indx < lim+1:
                lut[indx,0]=olut[lim+1-indx,0]
                lut[indx,1]=olut[lim+1-indx,1]
                lut[indx,2]=olut[lim+1-indx,2]
                lut[indx,3]=olut[lim+1-indx,3]
                indx = indx + 1
        if((cmn == "gnuplot2_r") or (cmn == "gist_earth_r")): 
            cmtype=1
            try:
                colormap = colormaps[cmn]   
            except:
                colormap = cm.get_cmap(cmn)
            colormap._init()
            lut =  (colormap._lut * 255).view(np.ndarray)  # Convert matplotlib colormap from 0-1 to 0-255 for Qt
        if (cmtype == 0): 
            if((cmn == "linear_worb_100_25_c53") or (cmn == "linear_wcmr_100_45_c42")): 
               colormap = cc.cm[cmn]
               colormap._init()
               lut =  (colormap._lut * 255).view(np.ndarray)  # Convert matplotlib colormap from 0-1 to 0-255 for Qt
            else: 
               colormap = cc.cm[cmn]
               colormap._init()
               lut =  (colormap._lut * 255).view(np.ndarray)  # Convert matplotlib colormap from 0-1 to 0-255 for Qt
               olut = (colormap._lut * 255).view(np.ndarray)  # Convert matplotlib colormap from 0-1 to 0-255 for Qt
               indx=1
               lim=255
               while indx < lim+1:
                   lut[indx,0]=olut[lim+1-indx,0]
                   lut[indx,1]=olut[lim+1-indx,1]
                   lut[indx,2]=olut[lim+1-indx,2]
                   lut[indx,3]=olut[lim+1-indx,3]
                   indx = indx + 1
    if(cmn == "jet"): 
        colors_i = np.concatenate((np.linspace(0, 1., 255), (0., 0., 0., 0.)))
        cmap = cm.jet
        colors_rgba = cmap(colors_i)
        lut = colors_rgba * 255
    if(cmn == "jet_white"): 
        colors_i = np.concatenate((np.linspace(0, 1., 255), (0., 0., 0., 0.)))
        cmap = cm.jet
        colors_rgba = cmap(colors_i)
        lut = colors_rgba * 255
    if(show_thr == 1):                                            
#   zero out below threshold (but not for colormap "jet")
#   do this only for the color bar in the main graph window
# do not do this for the color bar in the options box                
        indx=0
        thresh=int(258*SV1/100)
        if(cmn != "jet"): 
            if(thresh == 0):
                thresh=1
        while indx < thresh:
            lut[indx,0]=0.
            lut[indx,1]=0.
            lut[indx,2]=0.
            lut[indx,3]=0.
            indx=indx + 1
#        indx=0
#        if(cmn == "jet"): 
#            lut[indx,0]=100.
#            lut[indx,1]=100.
#            lut[indx,2]=100.
#            lut[indx,3]=100.
#   make sure no white spots show up at peak (peak should be black)
    indx=255
    while indx < 259:
        lut[indx,0]=0.
        lut[indx,1]=0.
        lut[indx,2]=0.
        lut[indx,3]=255.
        indx=indx + 1

######################################################
# CHECK BOX DIALOG class                             #
######################################################
class Type_Codes_Texts:
    def __init__(self, name):
        self.name = name
        self.tct = []    # creates a new empty list for each TypeCode
    def add_tct(self, tct):
        self.tct.append(tct)
        
######################################################
# CHECK BOX DIALOG class                             #
######################################################        
class checkdialog(QWidget):
    def __init__(self, MainWindow, parent = None):
        super(checkdialog, self).__init__(parent)
#        self.setWindowModality(Qt.ApplicationModal)
        self.setFixedWidth(800)
        self.setFixedHeight(150)
        self.gr2layout = QGridLayout()
        self.gr2layout.setSpacing(10)
        self.setLayout(self.gr2layout)

        mytext = "Files will be copied to: \n"+ os.path.abspath(prdir)
        self.cbl1 = QLabel(mytext)
#        self.cbl1.setAlignment(Qt.AlignmentFlag.AlignCenter)
#       addWidget (self, QWidget, row, column, rowSpan, columnSpan, Qt.Alignment alignment = 0)        
        self.gr2layout.addWidget(self.cbl1,0,0,2,3)
        
               
        self.bif1 = QCheckBox("Input files")
        self.bif1.setChecked(True)
#        self.b1.stateChanged.connect(lambda:self.btnstate(self.b1))
        self.gr2layout.addWidget(self.bif1,2,0,1,3)
 		
        self.bopf1 = QCheckBox("Plotit files")
        self.bopf1.setChecked(True)
#        self.b2.toggled.connect(lambda:self.btnstate(self.b2)) 
        self.gr2layout.addWidget(self.bopf1,2,1,1,3)

        self.bopf2 = QCheckBox("Plot files")
        self.bopf2.setChecked(True)
#        self.b2.toggled.connect(lambda:self.btnstate(self.b2)) 
        self.gr2layout.addWidget(self.bopf2,3,0,1,3)
        
        self.bof1 = QCheckBox("Output files")
        self.bof1.setChecked(True)
#        self.b2.toggled.connect(lambda:self.btnstate(self.b2)) 
        self.gr2layout.addWidget(self.bof1,3,1,1,3)

        self.cbtn_savef = QtWidgets.QPushButton("Cancel", self)
        self.gr2layout.addWidget(self.cbtn_savef,4,0,1,1)

        self.qbtn_savef = QtWidgets.QPushButton("Save && Close", self)
        self.gr2layout.addWidget(self.qbtn_savef,4,1,1,1)
        
        self.setWindowTitle("Select project file types to be saved")

######################################################
# Type Code Info class                              #
######################################################        
class TypeCodeInfo(QWidget):
    def __init__(self, MainWindow, TCname, TCtext, parent = None):
        super(TypeCodeInfo, self).__init__(parent)
        self.name = TCname.split(' ',1)[0]
        self.expl = (TCname.split(' ',1)[1]).split('(',1)[1]
        self.text = TCtext[1:]
        self.setFixedWidth(790)
        self.setFixedHeight(330)       
        TClabel = "Type Code " + f'{self.name}' + ": " + f'{self.expl[:len(self.expl)-1]}'                       
        TCtext = f'{self.text}'
        self.text_editor = QtWidgets.QTextEdit(self)
        self.text_editor.acceptRichText()
        self.text_editor.append(TCtext)
        self.text_editor.moveCursor(QTextCursor.Start)        
        self.grlayout = QVBoxLayout()
        self.grlayout.addWidget(self.text_editor)
        self.setLayout(self.grlayout)
        self.setWindowTitle(TClabel)                
        
######################################################
# SPIN BOX DIALOG class                              #
######################################################        
class spindialog(QWidget):
    def __init__(self, MainWindow, parent = None):
        super(spindialog, self).__init__(parent)
        self.setFixedWidth(390)
        self.setFixedHeight(130)      
#        self.setWindowModality(Qt.ApplicationModal)
        # enable custom window hint
#        self.setWindowFlags(self.windowFlags() | QtCore.Qt.CustomizeWindowHint)  
        # disable (but not hide) close button
#        self.setWindowFlags(self.windowFlags() & ~QtCore.Qt.WindowCloseButtonHint) 
     
        self.grlayout = QGridLayout()
        self.grlayout.setSpacing(10)
        self.setLayout(self.grlayout)
        
        self.l1 = QLabel("Select a number for this project:")
        self.l1.setAlignment(Qt.AlignmentFlag.AlignCenter)
#       addWidget (self, QWidget, row, column, rowSpan, columnSpan, Qt.Alignment alignment = 0)        
        self.grlayout.addWidget(self.l1,0,0,1,3)

        self.sp = QSpinBox()	
        self.sp.setRange(0, 100000)
        self.sp.setValue(prnumber)
        self.grlayout.addWidget(self.sp,1,0,1,3)
    
        self.cbtn = QtWidgets.QPushButton("Cancel", self)
        self.grlayout.addWidget(self.cbtn,2,0,1,1)

        self.qbtn = QtWidgets.QPushButton("Save && Close", self)
        self.grlayout.addWidget(self.qbtn,2,2,1,1)
          
        self.setWindowTitle("Project number selection")
                
######################################################
# OPTIONS LAYOUT class                               #
######################################################
class OptionsLayout(QWidget):
    def __init__(self, MainWindow, parent=None):
        global colormap_name, topvpos, vdel, acr_selected, evi_selected, xrd_selected 
        super().__init__(parent)
#        self.setWindowModality(Qt.WindowModal)
        self.mymain = MainWindow
## Create a ParameterTree widget
        self.top_line = QtWidgets.QLabel(self)
        self.top_line.setText("DGUI PREFERENCES AND OPTIONS SELECTION")
        self.top_line.resize(375,25)
        topvpos=10
        vdel=30
        boxesvoff = 0
        self.top_line.move(20, topvpos)        

# Display name of dynac binary, read from dgui.ini
        self.label_binary = QtWidgets.QLabel(self)
        self.label_binary.setText("Name of DYNAC binary:")
        self.label_binary.resize(375,25)
        self.label_binary.move(20, topvpos+vdel)        
        self.text_binary = QtWidgets.QTextEdit(self)
        self.text_binary.resize(140,29)
        self.text_binary.move(200, topvpos+vdel-1)        
        self.text_binary.setText(dynacv)
        self.BinBtn = QtWidgets.QPushButton('Change', self)
        self.BinBtn.resize(80,32)
        self.BinBtn.move(350, topvpos+vdel-2)        
        # Add BinBtn call back
        self.BinBtn.clicked.connect(self.change_bin)
        self.BinBtn.setToolTip('Change the dynac executable to be used')  

#       create "Density plot method" options box (box 1)
        self.OptBox1 = QGroupBox(self)
        self.OptBox1.setGeometry(QtCore.QRect(10, topvpos+2*vdel, 220, 50))
        self.OptBox1.setTitle("Density plot method")
        # Add L radio button (coordinates within groupBox)
        self.Optradio1 = QRadioButton(self.OptBox1)
        self.Optradio1.setGeometry(QtCore.QRect(10, 20, 120, 25))
        self.Optradio1.setText("Interpolation")
        self.Optradio1.setChecked(True)
        # Add R radio button (coordinates within groupBox)
        self.Optradio2 = QRadioButton(self.OptBox1)
        self.Optradio2.setGeometry(QtCore.QRect(150, 20, 60, 25))
        self.Optradio2.setText("KDE")
        # Add radio button 1 call back
        self.Optradio1.clicked.connect(self.get_or1)
        self.Optradio1.setToolTip('If selected, use interpolation between histogrammed data points')  
        # Add radio button 2 call back
        self.Optradio2.clicked.connect(self.get_or2)
        self.Optradio2.setToolTip('If selected, use KDE method')  

#       create "Number of bins for KDE plots" options box (box 3)
        self.OptBox3 = QGroupBox(self)
        self.OptBox3.setGeometry(QtCore.QRect(235, topvpos+2*vdel, 210, 50))
        self.OptBox3.setTitle("# of bins for KDE plots")
        # Add L radio button (coordinates within groupBox)
        self.OptradKDE1 = QRadioButton(self.OptBox3)
        self.OptradKDE1.setGeometry(QtCore.QRect(10, 20, 100, 25))
        self.OptradKDE1.setText("50")
        self.OptradKDE1.setChecked(True)
        # Add M radio button (coordinates within groupBox)
        self.OptradKDE2 = QRadioButton(self.OptBox3)
        self.OptradKDE2.setGeometry(QtCore.QRect(65, 20, 50, 25))
        self.OptradKDE2.setText("75")
        # Add R radio button (coordinates within groupBox)
        self.OptradKDE3 = QRadioButton(self.OptBox3)
        self.OptradKDE3.setGeometry(QtCore.QRect(120, 20, 60, 25))
        self.OptradKDE3.setText("100")
        # Add KDE radio button 1 call back
        self.OptradKDE1.clicked.connect(self.get_orKDE1)
        self.OptradKDE1.setToolTip('If selected, use 50 bins (fast)')  
        # Add KDE radio button 2 call back
        self.OptradKDE2.clicked.connect(self.get_orKDE2)
        self.OptradKDE2.setToolTip('If selected, use 75 bins (slow)')  
        # Add KDE radio button 3 call back
        self.OptradKDE3.clicked.connect(self.get_orKDE3)
        self.OptradKDE3.setToolTip('If selected, use 100 bins (very slow)')  

#       create "colormap" options box 
        self.label_cmn = QtWidgets.QLabel(self)
        self.label_cmn.setText("Color map:")
        self.label_cmn.resize(275,25)
        self.comboBox = QtWidgets.QComboBox(self)
        self.comboItems = {1: "default", 2: "gnuplot2_r", 3: "gist_earth_r", 4: "gist_stern",
            5: "viridis", 6: "nipy_spectral", 7: "jet", 8: "jet_white", 9: "diverging_rainbow_bgymr_45_85_c67",
            10: "rainbow_bgyr_35_85_c72", 11: "linear_tritanopic_krjcw_5_98_c46",
            12: "linear_worb_100_25_c53", 13: "linear_wcmr_100_45_c42",
            14: "linear_kryw_5_100_c67", 15:"linear_kryw_0_100_c71"}
#        self.comboBox.addItems(self.comboItems)
#       set preferred colormap as first (this way it will show at the top of the selection box)
#        self.comboBox.addItem(colormap_name)
        indx=1
        while indx < 16:
#            if(colormap_name != self.comboItems.get(indx)):
            self.comboBox.addItem(self.comboItems.get(indx))
            indx=indx + 1
        if (platform.system() == 'Windows') :
            self.label_cmn.move(125, topvpos+4*vdel-2)        
            self.comboBox.move(198, topvpos+4*vdel)        
        if (platform.system() == 'Linux') :
            self.label_cmn.move(125, topvpos+4*vdel-6)        
            self.comboBox.move(198, topvpos+4*vdel-4)        
        if (platform.system() == 'Darwin') :  
            self.label_cmn.move(115, topvpos+4*vdel-2)        
            self.comboBox.move(180, topvpos+4*vdel-4)
#        if(sys.version_info[0] == 3) :
#            if(sys.version_info[1] < 9 ) :
#                self.comboBox.activated[str].connect(self.cm_choice)  <-- pyqt5 (no longer needed; [int] for pyqt6) 
        self.comboBox.activated.connect(self.cm_choice)          
        self.comboBox.setToolTip('Select the colormap to be used for density plots')  
                
#       create "Threshold for density plot" options box (box 2)
        self.OptBox2 = QGroupBox(self)
        self.OptBox2.setGeometry(QtCore.QRect(10, topvpos+5*vdel, 220, 50))
        self.OptBox2.setTitle("Threshold for density plot")
        # Add slider 
        self.Optsld1 = QSlider(Qt.Orientation.Horizontal, self.OptBox2)
        self.Optsld1.setFocusPolicy(Qt.FocusPolicy.NoFocus)
        self.Optsld1.setGeometry(20, 22, 100, 25)
        self.Optsld1.setValue(SV1)
        self.Optsld1.valueChanged[int].connect(self.changeSV1)
        self.Optsld1.setToolTip('Change the threshold for the density plots')  
        # Add slider labels        
        self.OptSL1a = QLabel(self.OptBox2)
        self.OptSL1a.setText("0")
        self.OptSL1a.setGeometry( 10, 22, 20, 20)
        self.OptSL1b = QLabel(self.OptBox2)
        self.OptSL1b.setText("99")
        self.OptSL1b.setGeometry(125, 22, 20, 20)
        self.OptSL1c = QLabel(self.OptBox2)
        self.OptSL1c.setText(str(SV1))
        self.OptSL1c.setGeometry( 150, 22, 20, 20)

#       create colorbar based on selected colormap 
# Get the LUT corresponding to the selected colormap
        cm_lut(colormap_name,0)
        #Static plot widget is used here to create a colorbar
        self.staticPlt = pg.PlotWidget(self)
        #Set background of colorbar to white:
        self.staticPlt.setBackground((252,252,245, 255))
        xmin=0.
        xmax=99.
        ymin=0.
        ymax=0.2
        x = np.empty((256, 2)) 
        y = np.empty((256, 2)) 
        indx=0
        lut[indx,0]=255.
        lut[indx,1]=255.
        lut[indx,2]=255.
        while indx < 100:
            indice = int(indx * 255. / 99.)
            x[indx,0]= indx
            x[indx,1]= indx
            y[indx,0]=ymin
            y[indx,1]=ymax
            self.staticPlt.plot(x[indx,],y[indx,], pen=QPen(QColor(int(lut[indice,0]), int(lut[indice,1]), int(lut[indice,2]))))
            indx=indx + 1
#        staticPlt.setTitle(title='Colorbar')
        self.staticPlt.showAxis('left', show=False)
#        staticPlt.move(215,94)
        self.staticPlt.move(235,topvpos+4*vdel)
#        staticPlt.resize(175,55)
        self.staticPlt.resize(210,55)
        if (platform.system() == 'Windows') :
            self.staticPlt.move(235,topvpos+4*vdel+24)
            self.staticPlt.resize(210,55)
        if (platform.system() == 'Linux') :
            self.staticPlt.move(235,topvpos+4*vdel+24)
            self.staticPlt.resize(200,55)
        if (platform.system() == 'Darwin') :  
            self.staticPlt.move(235,topvpos+4*vdel+24)
            self.staticPlt.resize(200,55)
        self.staticPlt.setToolTip('This is the colormap used for density plots')  
        
#       create "Data in profiles are" options box (box 5)
        self.OptBox5 = QGroupBox(self)
        self.OptBox5.setGeometry(QtCore.QRect(10, topvpos+7*vdel, 220, 50))
        self.OptBox5.setTitle("Data in profiles are to be")
        # Add profiles check box (coordinates within groupBox)        
        self.checkBox6 = QCheckBox(self.OptBox5)
        self.checkBox6.setGeometry(QtCore.QRect(10, 22, 60, 25))
        self.checkBox6.setText("Raw")
        if(pro_raw != 0):
            self.checkBox6.setChecked(True)
        self.checkBox6.clicked.connect(self.get_cb6)
        # Add R check box (coordinates within groupBox)        
        self.checkBox7 = QCheckBox(self.OptBox5)
        self.checkBox7.setGeometry(QtCore.QRect(100, 22, 70, 25))
        self.checkBox7.setText("Fitted")
        if(pro_fit != 0):
            self.checkBox7.setChecked(True)
        self.checkBox7.clicked.connect(self.get_cb7)
        # Add Raw check tool tip
        self.checkBox6.setToolTip('If selected, profiles will be based on raw data')  
        # Add Fit check tool tip
        self.checkBox7.setToolTip('If selected, profiles will be based on a Gaussian fit')  

#       create "Amplitude of fits (a.u.)" options box (box 8)
        self.OptBox8 = QGroupBox(self)
        self.OptBox8.setGeometry(QtCore.QRect(235, topvpos+7*vdel, 210, 50))
        self.OptBox8.setTitle("Amplitude of profiles (a.u.)")
        # Add L radio button (coordinates within groupBox)
        self.OptradAF1 = QRadioButton(self.OptBox8)
        self.OptradAF1.setGeometry(QtCore.QRect(10, 20, 100, 25))
        self.OptradAF1.setText("1")
        # Add ML radio button (coordinates within groupBox)
        self.OptradAF2 = QRadioButton(self.OptBox8)
        self.OptradAF2.setGeometry(QtCore.QRect(60, 20, 50, 25))
        self.OptradAF2.setText("2")
        # Add MR radio button (coordinates within groupBox)
        self.OptradAF3 = QRadioButton(self.OptBox8)
        self.OptradAF3.setGeometry(QtCore.QRect(110, 20, 50, 25))
        self.OptradAF3.setChecked(True)
        self.OptradAF3.setText("3")
        # Add R radio button (coordinates within groupBox)
        self.OptradAF4 = QRadioButton(self.OptBox8)
        self.OptradAF4.setGeometry(QtCore.QRect(160, 20, 50, 25))
        self.OptradAF4.setText("4")
        # Add AF radio button 1 call back
        self.OptradAF1.clicked.connect(self.get_orAF1)
        self.OptradAF1.setToolTip('Small amplitude for fits and raw data')  
        # Add AF radio button 2 call back
        self.OptradAF2.clicked.connect(self.get_orAF2)
        self.OptradAF2.setToolTip('Small to medium amplitude for fits and raw data')  
        # Add AF radio button 3 call back
        self.OptradAF3.clicked.connect(self.get_orAF3)
        self.OptradAF3.setToolTip('Medium to large amplitude for fits and raw data')  
        # Add AF radio button 4 call back
        self.OptradAF4.clicked.connect(self.get_orAF4)
        self.OptradAF4.setToolTip('Large amplitude for fits and raw data')  

#       create "Graph limits based on" options box (box 7)
        self.OptBox7 = QGroupBox(self)
        self.OptBox7.setGeometry(QtCore.QRect(10, topvpos+9*vdel, 220, 50))
        self.OptBox7.setTitle("Graph limits based on")
        # Add L radio button (coordinates within groupBox)
        self.OptradGL1 = QRadioButton(self.OptBox7)
        self.OptradGL1.setGeometry(QtCore.QRect(10, 20, 100, 25))
        self.OptradGL1.setText("Auto")
        self.OptradGL1.setChecked(True)
        # Add R radio button (coordinates within groupBox)
        self.OptradGL2 = QRadioButton(self.OptBox7)
        self.OptradGL2.setGeometry(QtCore.QRect(100, 20, 60, 25))
        self.OptradGL2.setText("User")
        # Add GL radio button 1 call back
        self.OptradGL1.clicked.connect(self.get_orGL1)
        self.OptradGL1.setToolTip('If selected, auto range will be used')  
        # Add GL radio button 2 call back
        self.OptradGL2.clicked.connect(self.get_orGL2)
        self.OptradGL2.setToolTip('If selected, user defined settings will be used')  

# Create top right options box, plot centering (box 14)
        self.OptBox14 = QGroupBox(self)
        self.OptBox14.setGeometry(QtCore.QRect(235, topvpos+9*vdel, 210, 50))
        self.OptBox14.setTitle("Plot center options")
#       create top upper options box buttons (within box 14)        
        # Add L radio button (coordinates within OptBox14
        self.radOptABS = QRadioButton(self.OptBox14)
        self.radOptABS.setGeometry(QtCore.QRect(10, 20, 90, 25))
        self.radOptABS.setText("Absolute")        
        self.radOptABS.setChecked(True)
        # Add L radio button call back
        self.radOptABS.clicked.connect(self.get_rOptABS)
        self.radOptABS.setToolTip("Place distributions with respect to abolute reference axis")  
        # Add R radio button (coordinates within OptBox14)
        self.radOptCOG = QRadioButton(self.OptBox14)
        self.radOptCOG.setGeometry(QtCore.QRect(110, 20, 70, 25))
        self.radOptCOG.setText("COG")
        # Add R radio button call back
        self.radOptCOG.clicked.connect(self.get_rOptCOG)
        self.radOptCOG.setToolTip("Place distributions with respect to COG")  

#       create "Ellipse options" options box (box 12)
        self.OptBox12 = QGroupBox(self)
        self.OptBox12.setGeometry(QtCore.QRect(10, topvpos+11*vdel, 150, 50))
        self.OptBox12.setTitle("Ellipse options")
        # Add profiles check box (coordinates within groupBox)        
        self.checkBox12 = QCheckBox(self.OptBox12)
        self.checkBox12.setGeometry(QtCore.QRect(10, 22, 120, 25))
        self.checkBox12.setText("Plot ellipses")
#        self.checkBox12.setChecked(True)
        self.checkBox12.clicked.connect(self.get_cb12)
        # Add "Plot ellipses" check tool tip
        self.checkBox12.setToolTip('If selected, ellipses will be plotted based on NRMS times the RMS emittance size')  
# Display NRMS, read from dpu.ini  options box (box 11)
        self.label_nrms = QtWidgets.QLabel(self)
        self.label_nrms.setText("NRMS:")
        self.label_nrms.resize(275,25)
        self.label_nrms.move(170, topvpos+11*vdel-4)        
        self.text_nrms = QtWidgets.QTextEdit(self)
        self.text_nrms.resize(50,29)
        self.text_nrms.move(170, topvpos+int(11.5*vdel)+4)
        txtnrms = str(NRMS)      
        self.text_nrms.setText(txtnrms)
        self.NRMSBtn = QtWidgets.QPushButton('Change', self)
        self.NRMSBtn.resize(75,32)
        self.NRMSBtn.move(230, topvpos+int(11.5*vdel)+3)        
        # Add NRMSBtn call back
        self.NRMSBtn.clicked.connect(self.change_nrms)
        self.NRMSBtn.setToolTip("Change the number of RMS multiples for ellipses")  

#       create "Open User Guides (pdf) with" options box (box 4)
        self.OptBox4 = QGroupBox(self)
        self.OptBox4.setGeometry(QtCore.QRect(10, topvpos+13*vdel, 220, 50))
        self.OptBox4.setTitle("Open User Guides (pdf) with")
        # Add L radio button (coordinates within groupBox)
        self.Optradio3 = QRadioButton(self.OptBox4)
        self.Optradio3.setGeometry(QtCore.QRect(10, 20, 80, 25))
        self.Optradio3.setText("Adobe")
        # Add R radio button (coordinates within groupBox)
        self.Optradio4 = QRadioButton(self.OptBox4)
        self.Optradio4.setGeometry(QtCore.QRect(80, 20, 90, 25))
        self.Optradio4.setText("Evince")
         # Add R radio button (coordinates within groupBox)
        self.Optradio5 = QRadioButton(self.OptBox4)
        self.Optradio5.setGeometry(QtCore.QRect(150, 20, 90, 25))
        self.Optradio5.setText("xrd")
#        print("button acr, evi, xrd ",acr_selected,evi_selected,xrd_selected)
        if acr_selected == True :
            self.Optradio3.setChecked(True)
        if evi_selected == True :
            self.Optradio4.setChecked(True)
        if xrd_selected == True :
            self.Optradio5.setChecked(True)
        # Add radio button 1 call back
        self.Optradio3.clicked.connect(self.get_or3)
        self.Optradio3.setToolTip('If selected, Acrobat reader will be used for opening User Guides')  
        # Add radio button 2 call back
        self.Optradio4.clicked.connect(self.get_or4)
        self.Optradio4.setToolTip('If selected, Evince will be used for opening User Guides. Only works on linux')
        # Add radio button 3 call back
        self.Optradio5.clicked.connect(self.get_or5)
        self.Optradio5.setToolTip('If selected, xreader will be used for opening User Guides. Only works on linux')
          
# Display graph limits, read from dpu.ini  options box
        self.label_xmin = QtWidgets.QLabel(self)
        self.label_xmin.setText("Xmin:")
        self.label_xmin.resize(275,25)
        self.label_xmin.move(10, topvpos+15*vdel-4)        
        self.text_xmin = QtWidgets.QTextEdit(self)
        self.text_xmin.resize(50,29)
        self.text_xmin.move(10, topvpos+int(15.5*vdel)+4)
        txtminmax = str(xvals[0])      
        self.text_xmin.setText(txtminmax)
        self.label_xmax = QtWidgets.QLabel(self)
        self.label_xmax.setText("Xmax:")
        self.label_xmax.resize(275,25)
        self.label_xmax.move(10, topvpos+17*vdel-4)        
        self.text_xmax = QtWidgets.QTextEdit(self)
        self.text_xmax.resize(50,29)
        self.text_xmax.move(10, topvpos+int(17.5*vdel)+4)
        txtminmax = str(xvals[1])
        self.text_xmax.setText(txtminmax)
        #              
        self.label_xpmin = QtWidgets.QLabel(self)
        self.label_xpmin.setText("XPmin:")
        self.label_xpmin.resize(275,25)
        self.label_xpmin.move(85, topvpos+15*vdel-4)        
        self.text_xpmin = QtWidgets.QTextEdit(self)
        self.text_xpmin.resize(50,29)
        self.text_xpmin.move(85, topvpos+int(15.5*vdel)+4)
        txtminmax = str(xpvals[0])      
        self.text_xpmin.setText(txtminmax)
        self.label_xpmax = QtWidgets.QLabel(self)
        self.label_xpmax.setText("XPmax:")
        self.label_xpmax.resize(275,25)
        self.label_xpmax.move(85, topvpos+17*vdel-4)        
        self.text_xpmax = QtWidgets.QTextEdit(self)
        self.text_xpmax.resize(50,29)
        self.text_xpmax.move(85, topvpos+int(17.5*vdel)+4)
        txtminmax = str(xpvals[1])
        self.text_xpmax.setText(txtminmax)
        #              
        self.label_ymin = QtWidgets.QLabel(self)
        self.label_ymin.setText("Ymin:")
        self.label_ymin.resize(275,25)
        self.label_ymin.move(160, topvpos+15*vdel-4)        
        self.text_ymin = QtWidgets.QTextEdit(self)
        self.text_ymin.resize(50,29)
        self.text_ymin.move(160, topvpos+int(15.5*vdel)+4)
        txtminmax = str(yvals[0])      
        self.text_ymin.setText(txtminmax)
        self.label_ymax = QtWidgets.QLabel(self)
        self.label_ymax.setText("Ymax:")
        self.label_ymax.resize(275,25)
        self.label_ymax.move(160, topvpos+17*vdel-4)        
        self.text_ymax = QtWidgets.QTextEdit(self)
        self.text_ymax.resize(50,29)
        self.text_ymax.move(160, topvpos+int(17.5*vdel)+4)
        txtminmax = str(yvals[1])      
        self.text_ymax.setText(txtminmax)
        #              
        self.label_ypmin = QtWidgets.QLabel(self)
        self.label_ypmin.setText("YPmin:")
        self.label_ypmin.resize(275,25)
        self.label_ypmin.move(235, topvpos+15*vdel-4)        
        self.text_ypmin = QtWidgets.QTextEdit(self)
        self.text_ypmin.resize(50,29)
        self.text_ypmin.move(235, topvpos+int(15.5*vdel)+4)
        txtminmax = str(ypvals[0])      
        self.text_ypmin.setText(txtminmax)
        self.label_ypmax = QtWidgets.QLabel(self)
        self.label_ypmax.setText("YPmax:")
        self.label_ypmax.resize(275,25)
        self.label_ypmax.move(235, topvpos+17*vdel-4)        
        self.text_ypmax = QtWidgets.QTextEdit(self)
        self.text_ypmax.resize(50,29)
        self.text_ypmax.move(235, topvpos+int(17.5*vdel)+4)
        txtminmax = str(ypvals[1])      
        self.text_ypmax.setText(txtminmax)
        #
        self.label_zmin = QtWidgets.QLabel(self)
        self.label_zmin.setText("Zmin:")
        self.label_zmin.resize(275,25)
        self.label_zmin.move(310, topvpos+15*vdel-4)        
        self.text_zmin = QtWidgets.QTextEdit(self)
        self.text_zmin.resize(50,29)
        self.text_zmin.move(310, topvpos+int(15.5*vdel)+4)
        txtminmax = str(zvals[0])      
        self.text_zmin.setText(txtminmax)
        self.label_zmax = QtWidgets.QLabel(self)
        self.label_zmax.setText("Zmax:")
        self.label_zmax.resize(275,25)
        self.label_zmax.move(310, topvpos+17*vdel-4)        
        self.text_zmax = QtWidgets.QTextEdit(self)
        self.text_zmax.resize(50,29)
        self.text_zmax.move(310, topvpos+int(17.5*vdel)+4)
        txtminmax = str(zvals[1])      
        self.text_zmax.setText(txtminmax)
        #
        self.label_zpmin = QtWidgets.QLabel(self)
        self.label_zpmin.setText("ZPmin:")
        self.label_zpmin.resize(275,25)
        self.label_zpmin.move(385, topvpos+15*vdel-4)        
        self.text_zpmin = QtWidgets.QTextEdit(self)
        self.text_zpmin.resize(50,29)
        self.text_zpmin.move(385, topvpos+int(15.5*vdel)+4)
        txtminmax = str(zpvals[0])      
        self.text_zpmin.setText(txtminmax)
        self.label_zpmax = QtWidgets.QLabel(self)
        self.label_zpmax.setText("ZPmax:")
        self.label_zpmax.resize(275,25)
        self.label_zpmax.move(385, topvpos+17*vdel-4)        
        self.text_zpmax = QtWidgets.QTextEdit(self)
        self.text_zpmax.resize(50,29)
        self.text_zpmax.move(385, topvpos+int(17.5*vdel)+4)
        txtminmax = str(zpvals[1])      
        self.text_zpmax.setText(txtminmax)
        
        self.ChangeLimitsBtn = QtWidgets.QPushButton('Update graph limits', self)
        self.ChangeLimitsBtn.resize(425,32)
        self.ChangeLimitsBtn.move(10, topvpos+int(18.9*vdel))        
        # Add ChangeLimitsBtn call back
        self.ChangeLimitsBtn.clicked.connect(self.change_limits)
        self.ChangeLimitsBtn.setToolTip("Change the graph limits")  

#       create "Emittance values" option box 
        self.OptBox20 = QGroupBox(self)
        self.OptBox20.setGeometry(QtCore.QRect(235, topvpos+13*vdel, 220, 50))
        self.OptBox20.setTitle("Emittance values")
        # Add Display check box (coordinates within groupBox)        
        self.checkBox20 = QCheckBox(self.OptBox20)
        self.checkBox20.setGeometry(QtCore.QRect(10, 22, 70, 25))
        self.checkBox20.setText("Display") 
        self.checkBox20.clicked.connect(self.set_dev)
        self.checkBox20.setToolTip('If selected, displays RMS emittances on top 3 distribution plots')  
        # Add "at the bottom" check box (location of emittance values in plot)        
        self.checkBox21 = QCheckBox(self.OptBox20)
        self.checkBox21.setGeometry(QtCore.QRect(85, 22, 110, 25))
        self.checkBox21.setText("at the bottom") 
        self.checkBox21.clicked.connect(self.set_emi_pos)
        self.checkBox21.setToolTip('If selected, changes location of the display values to the bottom of the plots')  
        
# Display amu, optionally read from dgui.ini 
        boxesvoff = -15
        self.label1_amu = QtWidgets.QLabel(self)
        self.label1_amu.setText("Particle mass (AMU):") 
        self.label1_amu.resize(285,30)
        self.label1_amu.move(10, topvpos+int(20.*vdel))        
        self.text_amu = QtWidgets.QTextEdit(self)
        self.text_amu.resize(50,26)
        self.text_amu.move(10, topvpos+int(21.5*vdel) + boxesvoff)
        txtmass = str(amu)      
        self.text_amu.setText(txtmass)
        self.AMUBtn = QtWidgets.QPushButton('Change', self)
        self.AMUBtn.resize(75,32)
        self.AMUBtn.move(65, topvpos+int(21.5*vdel) + boxesvoff - 4)        
        # Add AMUBtn call back
        self.AMUBtn.clicked.connect(self.change_amu)
        self.AMUBtn.setToolTip("Change the particle mass") 

#       create "Save graphs as:" options box (box 23)
        self.OptBox23 = QGroupBox(self)
        self.OptBox23.setGeometry(QtCore.QRect(235, topvpos+20*vdel+6, 200, 50))
        self.OptBox23.setTitle("Save graphs as:")
        # Add L radio button (coordinates within groupBox)
        self.OptPlotit1 = QRadioButton(self.OptBox23)
        self.OptPlotit1.setGeometry(QtCore.QRect(10, 20, 100, 25))
        self.OptPlotit1.setText("png")
        self.OptPlotit1.setChecked(True)
        # Add M radio button (coordinates within groupBox)
        self.OptPlotit2 = QRadioButton(self.OptBox23)
        self.OptPlotit2.setGeometry(QtCore.QRect(65, 20, 50, 25))
        self.OptPlotit2.setText("jpeg")
#        # Add R radio button (coordinates within groupBox) GIF not generally supported on all platforms by QT
#        bmp and tiff supported by QT, but not out of the box with gnuplot (unless initially requested 
#        at installation time (?))
#        self.OptPlotit3 = QRadioButton(self.OptBox23)
#        self.OptPlotit3.setGeometry(QtCore.QRect(120, 20, 60, 25))
#        self.OptPlotit3.setText("gif")
        # Add Plotit radio button 1 call back
        self.OptPlotit1.clicked.connect(self.get_Plot1)
        self.OptPlotit1.setToolTip('Store graphs as png files')  
        # Add Plotit radio button 2 call back
        self.OptPlotit2.clicked.connect(self.get_Plot2)
        self.OptPlotit2.setToolTip('Store graphs as jpeg files')  
#        # Add Plotit radio button 3 call back GIF not generally supported on all platforms by QT
#        self.OptPlotit3.clicked.connect(self.get_Plot3)
#        self.OptPlotit3.setToolTip('Store plotit graphs as gif files')  
        
        
# Display Energy, optionally read from dgui.ini
#        self.label1_energy = QtWidgets.QLabel(self)
#        self.label1_energy.setText("Beam energy (MeV/u):")
#        self.label1_energy.resize(285,25)
#        self.label1_energy.move(300, topvpos+int(20.*vdel))        
#        self.text_energy = QtWidgets.QTextEdit(self)
#        self.text_energy.resize(50,26)
#        self.text_energy.move(300, topvpos+int(21.5*vdel) + boxesvoff)
#        txtener = str(ener)      
#        self.text_energy.setText(txtener)
#        self.EnergyBtn = QtWidgets.QPushButton('Change', self)
#        self.EnergyBtn.resize(75,32)
#        self.EnergyBtn.move(355, int(topvpos+21.5*vdel) + boxesvoff - 4)        
#        # Add EnergyBtn call back
#        self.EnergyBtn.clicked.connect(self.change_energy)
#        self.EnergyBtn.setToolTip("Change the beam energy")  

        
    def set_dev(self):
        global emivals_selected
        '''Display emittance values'''               
        if(self.checkBox20.isChecked() == True ):
            emivals_selected = True 
        else:      
            emivals_selected = False         

    def set_emi_pos(self):
        global emivals_bottom
        '''Position of emittance values'''               
        if(self.checkBox21.isChecked() == True ):
            emivals_bottom = True 
        else:      
            emivals_bottom = False         

    def change_amu(self):
        global amu
        '''Change the mass (AMU)'''               
        amutxt = self.text_amu.toPlainText()
        amu = float(amutxt)
        mytext = ""
        mytext = "Particle mass changed to " + amutxt + "\n"
        mycursor=self.mymain.inpLog.textCursor()
        self.mymain.inpLog.setTextCursor(mycursor)
        mycursor.insertText(mytext) 
        
#    def change_energy(self):
#        global ener
#        '''Change the beam energy (MeV/u)'''               
#        enertxt = self.text_energy.toPlainText()
#        ener = float(enertxt)
#        print("Beam energy changed to ",ener," MeV/u")                        

    def cm_choice(self,textnum):
        global colormap_name, topvpos, vdel, lut 
        self.staticPlt.close()
        # color maps numbered from 1..15, but standard button starts from 0
        textnum = textnum + 1
        
        text = self.comboItems[textnum]
        if(text == "jet_white"):
            colormap_name = "jet"
        else:
            colormap_name = text                
# Get the LUT corresponding to the selected colormap
        cm_lut(colormap_name,0)
        if(text == "jet_white"):
            colormap_name = "jet_white"    
        self.staticPlt = pg.PlotWidget(self)
        self.staticPlt.setBackground((252,252,245, 255))
        xmin=0.
        xmax=99.
        ymin=0.
        ymax=0.2
        x = np.empty((256, 2)) 
        y = np.empty((256, 2)) 
        indx=0
        lut[indx,0]=255.
        lut[indx,1]=255.
        lut[indx,2]=255.
        while indx < 100:
            indice = int(indx * 255. / 99.)
            x[indx,0]= indx
            x[indx,1]= indx
            y[indx,0]=ymin
            y[indx,1]=ymax
            self.staticPlt.plot(x[indx,],y[indx,], pen=QPen(QColor(int(lut[indice,0]), int(lut[indice,1]), int(lut[indice,2]))))
            indx=indx + 1
        self.staticPlt.showAxis('left', show=False)
        self.staticPlt.move(235,topvpos+4*vdel)
        self.staticPlt.resize(210,55)
        if (platform.system() == 'Windows') :
            self.staticPlt.move(235,topvpos+4*vdel+24)
            self.staticPlt.resize(210,55)
        if (platform.system() == 'Linux') :
            self.staticPlt.move(235,topvpos+4*vdel+24)
            self.staticPlt.resize(200,55)
        if (platform.system() == 'Darwin') :  
            self.staticPlt.move(235,topvpos+4*vdel+24)
            self.staticPlt.resize(200,55)
        self.staticPlt.setToolTip('This is the colormap used for density plots')  
        self.staticPlt.show()
        mytext=""        
        mytext="Colormap "  + colormap_name + " selected\n"      
        mycursor=self.mymain.inpLog.textCursor()
        self.mymain.inpLog.setTextCursor(mycursor)
        mycursor.insertText(mytext) 
                      
    def get_or1(self):
        global inter_selected, KDE_selected
        inter_selected = True
        KDE_selected = False

    def get_or2(self):
        global inter_selected, KDE_selected
        inter_selected = False
        KDE_selected = True

    def get_or3(self):
        global acr_selected, evi_selected, xrd_selected
        acr_selected = True
        evi_selected = False
        xrd_selected = False

    def get_or4(self):
        global acr_selected, evi_selected, xrd_selected
        acr_selected = False
        evi_selected = True
        xrd_selected = False

    def get_or5(self):
        global acr_selected, evi_selected, xrd_selected
        acr_selected = False
        evi_selected = False
        xrd_selected = True

    def get_rOptABS(self):
        global ABS_selected, COG_selected
        ABS_selected = True
        COG_selected = False

    def get_rOptCOG(self):
        global ABS_selected, COG_selected
        COG_selected = True
        ABS_selected = False
        
    def get_orKDE1(self):
        global n_of_KDE_bins
        n_of_KDE_bins=50

    def get_orKDE2(self):
        global n_of_KDE_bins
        n_of_KDE_bins=75

    def get_orKDE3(self):
        global n_of_KDE_bins
        n_of_KDE_bins=100

    def get_Plot1(self):
        global gr_file_ext
        gr_file_ext="png"

    def get_Plot2(self):
        global gr_file_ext
        gr_file_ext="jpeg"

    def get_Plot3(self):
        global gr_file_ext
        gr_file_ext="gif"
        
        
    def get_orAF1(self):
        global fit_amp
        fit_amp=16./1.        

    def get_orAF2(self):
        global fit_amp
        fit_amp=16./2.        

    def get_orAF3(self):
        global fit_amp
        fit_amp=16./3.       

    def get_orAF4(self):
        global fit_amp
        fit_amp=16./4.      

    def changeSV1(self, value):
        global SV1
        self.OptSL1c.setText(str(value))
        SV1=value        

    def change_nrms(self):
        global NRMS
        '''Change the number of RMS multiples'''               
        nrmstxt = self.text_nrms.toPlainText()
        NRMS = float(nrmstxt)
        mytxt = ""
        mytext = "Number of RMS multiples changed to " + nrmstxt + "\n"
        mycursor=self.mymain.inpLog.textCursor()
        self.mymain.inpLog.setTextCursor(mycursor)
        mycursor.insertText(mytext) 

    def change_limits(self):
        global xvals,xpvals,yvals,ypvals,zvals,zpvals
        '''Change the graph limits'''               
        limtxt = self.text_xmin.toPlainText()
        xvals[0] = float(limtxt)
        limtxt = self.text_xmax.toPlainText()
        xvals[1] = float(limtxt)
        limtxt = self.text_xpmin.toPlainText()
        xpvals[0] = float(limtxt)
        limtxt = self.text_xpmax.toPlainText()
        xpvals[1] = float(limtxt)
        #
        limtxt = self.text_ymin.toPlainText()
        yvals[0] = float(limtxt)
        limtxt = self.text_ymax.toPlainText()
        yvals[1] = float(limtxt)
        limtxt = self.text_ypmin.toPlainText()
        ypvals[0] = float(limtxt)
        limtxt = self.text_ypmax.toPlainText()
        ypvals[1] = float(limtxt)
        #
        limtxt = self.text_zmin.toPlainText()
        zvals[0] = float(limtxt)
        limtxt = self.text_zmax.toPlainText()
        zvals[1] = float(limtxt)
        limtxt = self.text_zpmin.toPlainText()
        zpvals[0] = float(limtxt)
        limtxt = self.text_zpmax.toPlainText()
        zpvals[1] = float(limtxt)
        mytext = ""
        mytext = "Graph limits updated\n"
        mycursor=self.mymain.inpLog.textCursor()
        self.mymain.inpLog.setTextCursor(mycursor)
        mycursor.insertText(mytext) 
                
    def get_cb6(self):
        global pro_raw
        if (self.checkBox6.isChecked() != 0 ):   
            pro_raw = True
#            print('CB6 Selected')
        else:    
            pro_raw = False
#            print('CB6 Not Selected')

    def get_cb7(self):
        global pro_fit
        if (self.checkBox7.isChecked() != 0 ):   
            pro_fit = True
#            print('CB7 Selected')
        else:    
            pro_fit = False
#            print('CB7 Not Selected')

    def get_cb12(self):
        global plot_ellipse
        if(self.checkBox12.isChecked() != 0 ):   
            plot_ellipse = True
        else:    
            plot_ellipse = False

    def get_orGL1(self):
        global GRS
        GRS="Auto"

    def get_orGL2(self):
        global GRS
        GRS="File"

    def change_bin(self):                            #
        global dynacv
        '''Change the dynac executable'''            #
        dynacv = self.text_binary.toPlainText()
        mytext = ""
        mytext = "DYNAC executable changed to " + dynacv + "\n"
        mycursor=self.mymain.inpLog.textCursor()
        self.mymain.inpLog.setTextCursor(mycursor)
        mycursor.insertText(mytext) 


######################################################
# OPTIONS class                                      #
######################################################
class Options(QMainWindow):
#    def __init__(self, parent=None):
    def __init__(self, MainWindow, parent=None):
        super().__init__(parent)
#        self.mymain = MainWindow
## Create a ParameterTree like widget
        self.setMinimumSize(QSize(455, 700))    
        self.setMaximumSize(QSize(455, 700))    
#        self.setMaximumSize(QSize(625, 875))    
        self.setWindowTitle("DYNAC GUI OPTIONS") 

        # Set window background color
        self.setAutoFillBackground(True)
        p = self.palette()
#        p.setColor(self.backgroundRole(), QColor(154, 0, 154))
        p.setColor(self.backgroundRole(), QColor(176,224,230))
        self.setPalette(p)

        self.main_widget = OptionsLayout(MainWindow, parent=self)
        self.setCentralWidget(self.main_widget)
        # filling up a menu bar
        bar = self.menuBar()
        # File menu
        file_menu = bar.addMenu('File')
        # adding actions to file menu
        open_action = QtWidgets.QAction('Open', self)
        close_action = QtWidgets.QAction('Close', self)
        file_menu.addAction(open_action)
        file_menu.addAction(close_action)
        # Edit menu
        edit_menu = bar.addMenu('Edit')
        # adding actions to edit menu
        undo_action = QtWidgets.QAction('Undo', self)
        redo_action = QtWidgets.QAction('Redo', self)
        edit_menu.addAction(undo_action)
        edit_menu.addAction(redo_action)
        # use `connect` method to bind signals to desired behavior
        close_action.triggered.connect(self.close)


######################################################
# MAINWINDOW class                                   #
######################################################
class MainWindow(QMainWindow):
    def __init__(self, parent=None):
        super().__init__(parent)
        # creating widget and setting it as central
        self.setMinimumSize(QSize(1250, 700))    
#        self.setMaximumSize(QSize(1500, 840))    
        self.setMaximumSize(QSize(1250, 700))    
        self.setWindowTitle(dgui_v) 

        self.dynugpdf = None
        self.dgugpdf  = None

        # Set window background color
        self.setAutoFillBackground(True)
        p = self.palette()
        p.setColor(self.backgroundRole(), QColor(0, 204, 204))
        self.setPalette(p)
 
        self.createActions()

        self.main_widget = MainLayout(parent=self)
        self.setCentralWidget(self.main_widget)
        # filling up a menu bar
        bar = self.menuBar()
        bar.setNativeMenuBar(False) # Disables the global menu bar on MacOS
        
        # File menu
        file_menu = bar.addMenu('File')
        # adding actions to file menu
        openif_action = QtWidgets.QAction('Open input file', self, triggered=self.main_widget.get_inp)
        opendf_action = QtWidgets.QAction('Open dist. file', self, triggered=self.main_widget.get_dst)
        setpfd_action = QtWidgets.QAction('Set project files directory', self, triggered=self.main_widget.set_pfd)
        savef_action  = QtWidgets.QAction('Save project file(s)', self, triggered=self.main_widget.save_files)
        close_action  = QtWidgets.QAction('Close DGUI', self)
        close_all     = QtWidgets.QAction('Close all windows',    self)
        file_menu.addAction(openif_action)
        file_menu.addAction(opendf_action)
        file_menu.addAction(setpfd_action)
        file_menu.addAction(savef_action)
        file_menu.addAction(close_action)
        file_menu.addAction(close_all)
        # Edit menu
#        edit_menu = bar.addMenu('Edit')
#        # adding actions to edit menu
#        undo_action = QtWidgets.QAction('Undo', self)
#        redo_action = QtWidgets.QAction('Redo', self)
#        edit_menu.addAction(undo_action)
#        edit_menu.addAction(redo_action)
        # Options menu
        file_menu = bar.addMenu('Options')
        # adding actions to file menu
        prscr_action = QtWidgets.QAction('Print Screen', self, shortcut="Ctrl+P",
                statusTip="Print the full screen", triggered=self.printscr_)
        file_menu.addAction(prscr_action)
 
        # Help menu
        file_menu = bar.addMenu('Help')
        # adding actions to file menu
        helpdy_action = QtWidgets.QAction('DYNAC help', self, 
                statusTip="Help on DYNAC", triggered=self.help_dynac)
        helpdy_action.setShortcut( QKeySequence("Ctrl+M") )
        file_menu.addAction(helpdy_action)
        
        helpdg_action = QtWidgets.QAction('DGUI help', self, 
                statusTip="Help on DGUI", triggered=self.help_dgui)
        helpdg_action.setShortcut( QKeySequence("Ctrl+G") )
        file_menu.addAction(helpdg_action)

# get text for pull-down menu help on TCs
        pdf_document =  default_ugpath + os.sep + 'dynac_UG.pdf'
        get_tcd(pdf_document)        
        
#        helptc1_action  = QtWidgets.QAction('Input Beam Type Codes', self) 
#        helptc2_action  = QtWidgets.QAction('Optical Lenses Type Codes', self) 
        helptc3_action  = QtWidgets.QAction('Accelerator, Buncher, E-gun, Stripper Type Codes', self) 
        helptc4_action  = QtWidgets.QAction('Functioning Modes Type Codes', self)
        helptc5_action  = QtWidgets.QAction('Beam Redefinition Type Codes', self)
        helptc6_action  = QtWidgets.QAction('Tolerances and Errors Type Codes', self)
        helptc7_action  = QtWidgets.QAction('Space Charge Type Codes', self)
        helptc8_action  = QtWidgets.QAction('Output Print Type Codes', self)
        helptc9_action  = QtWidgets.QAction('Output Plot Type Codes', self)
#        helptc10_action = QtWidgets.QAction('Other Type Codes', self)

        i = 0
        tct = 10
        file_submenu = file_menu.addMenu("Type Codes")
        file_tc1_submenu = file_submenu.addMenu("Input Beam Type Codes")

        file_tc1_submenu.addAction(QtWidgets.QAction(TypeCodes[0][0].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[0][0],TypeCodeText[0][0])))) 
        file_tc1_submenu.addAction(QtWidgets.QAction(TypeCodes[0][1].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[0][1],TypeCodeText[0][1])))) 
        file_tc1_submenu.addAction(QtWidgets.QAction(TypeCodes[0][2].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[0][2],TypeCodeText[0][2])))) 
        file_tc1_submenu.addAction(QtWidgets.QAction(TypeCodes[0][3].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[0][3],TypeCodeText[0][3]))))            

        file_tc2_submenu = file_submenu.addMenu("Optical Lenses Type Codes")
        file_tc2_submenu.addAction(QtWidgets.QAction(TypeCodes[1][0].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[1][0],TypeCodeText[1][0])))) 
        file_tc2_submenu.addAction(QtWidgets.QAction(TypeCodes[1][1].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[1][1],TypeCodeText[1][1])))) 
        file_tc2_submenu.addAction(QtWidgets.QAction(TypeCodes[1][2].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[1][2],TypeCodeText[1][2])))) 
        file_tc2_submenu.addAction(QtWidgets.QAction(TypeCodes[1][3].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[1][3],TypeCodeText[1][3]))))         
        file_tc2_submenu.addAction(QtWidgets.QAction(TypeCodes[1][4].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[1][4],TypeCodeText[1][4])))) 
        file_tc2_submenu.addAction(QtWidgets.QAction(TypeCodes[1][5].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[1][5],TypeCodeText[1][5])))) 
        file_tc2_submenu.addAction(QtWidgets.QAction(TypeCodes[1][6].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[1][6],TypeCodeText[1][6])))) 
        file_tc2_submenu.addAction(QtWidgets.QAction(TypeCodes[1][7].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[1][7],TypeCodeText[1][7]))))         
        file_tc2_submenu.addAction(QtWidgets.QAction(TypeCodes[1][8].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[1][8],TypeCodeText[1][8])))) 
        file_tc2_submenu.addAction(QtWidgets.QAction(TypeCodes[1][9].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[1][9],TypeCodeText[1][9])))) 
        file_tc2_submenu.addAction(QtWidgets.QAction(TypeCodes[1][10].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[1][10],TypeCodeText[1][10])))) 
        file_tc2_submenu.addAction(QtWidgets.QAction(TypeCodes[1][11].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[1][11],TypeCodeText[1][11]))))         
        file_tc2_submenu.addAction(QtWidgets.QAction(TypeCodes[1][12].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[1][12],TypeCodeText[1][12])))) 
        file_tc2_submenu.addAction(QtWidgets.QAction(TypeCodes[1][13].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[1][13],TypeCodeText[1][13])))) 
        file_tc2_submenu.addAction(QtWidgets.QAction(TypeCodes[1][14].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[1][14],TypeCodeText[1][14])))) 
        file_tc2_submenu.addAction(QtWidgets.QAction(TypeCodes[1][15].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[1][15],TypeCodeText[1][15]))))         
        file_tc2_submenu.addAction(QtWidgets.QAction(TypeCodes[1][16].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[1][16],TypeCodeText[1][16])))) 
        file_tc2_submenu.addAction(QtWidgets.QAction(TypeCodes[1][17].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[1][17],TypeCodeText[1][17])))) 
                   
        file_tc3_submenu = file_submenu.addMenu("Accelerator, Buncher, E-gun, Stripper Type Codes")
        file_tc3_submenu.addAction(QtWidgets.QAction(TypeCodes[2][0].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[2][0],TypeCodeText[2][0])))) 
        file_tc3_submenu.addAction(QtWidgets.QAction(TypeCodes[2][1].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[2][1],TypeCodeText[2][1])))) 
        file_tc3_submenu.addAction(QtWidgets.QAction(TypeCodes[2][2].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[2][2],TypeCodeText[2][2])))) 
        file_tc3_submenu.addAction(QtWidgets.QAction(TypeCodes[2][3].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[2][3],TypeCodeText[2][3]))))         
        file_tc3_submenu.addAction(QtWidgets.QAction(TypeCodes[2][4].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[2][4],TypeCodeText[2][4])))) 
        file_tc3_submenu.addAction(QtWidgets.QAction(TypeCodes[2][5].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[2][5],TypeCodeText[2][5])))) 
        file_tc3_submenu.addAction(QtWidgets.QAction(TypeCodes[2][6].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[2][6],TypeCodeText[2][6])))) 
        file_tc3_submenu.addAction(QtWidgets.QAction(TypeCodes[2][7].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[2][7],TypeCodeText[2][7]))))         
        file_tc3_submenu.addAction(QtWidgets.QAction(TypeCodes[2][8].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[2][8],TypeCodeText[2][8])))) 
        file_tc3_submenu.addAction(QtWidgets.QAction(TypeCodes[2][9].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[2][9],TypeCodeText[2][9])))) 
        file_tc3_submenu.addAction(QtWidgets.QAction(TypeCodes[2][10].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[2][10],TypeCodeText[2][10]))))        

        file_tc4_submenu = file_submenu.addMenu("Functioning Modes Type Codes")
        file_tc4_submenu.addAction(QtWidgets.QAction(TypeCodes[3][0].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[3][0],TypeCodeText[3][0])))) 
        file_tc4_submenu.addAction(QtWidgets.QAction(TypeCodes[3][1].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[3][1],TypeCodeText[3][1])))) 
        file_tc4_submenu.addAction(QtWidgets.QAction(TypeCodes[3][2].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[3][2],TypeCodeText[3][2])))) 
        file_tc4_submenu.addAction(QtWidgets.QAction(TypeCodes[3][3].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[3][3],TypeCodeText[3][3])))) 
                  
        file_tc5_submenu = file_submenu.addMenu("Beam Redefinition Type Codes")
        file_tc5_submenu.addAction(QtWidgets.QAction(TypeCodes[4][0].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[4][0],TypeCodeText[4][0])))) 
        file_tc5_submenu.addAction(QtWidgets.QAction(TypeCodes[4][1].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[4][1],TypeCodeText[4][1])))) 
        file_tc5_submenu.addAction(QtWidgets.QAction(TypeCodes[4][2].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[4][2],TypeCodeText[4][2])))) 
        file_tc5_submenu.addAction(QtWidgets.QAction(TypeCodes[4][3].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[4][3],TypeCodeText[4][3]))))         
        file_tc5_submenu.addAction(QtWidgets.QAction(TypeCodes[4][4].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[4][4],TypeCodeText[4][4])))) 
        file_tc5_submenu.addAction(QtWidgets.QAction(TypeCodes[4][5].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[4][5],TypeCodeText[4][5])))) 
        
        file_tc6_submenu = file_submenu.addMenu("Tolerances and Errors Type Codes")
        file_tc6_submenu.addAction(QtWidgets.QAction(TypeCodes[5][0].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[5][0],TypeCodeText[5][0])))) 
        file_tc6_submenu.addAction(QtWidgets.QAction(TypeCodes[5][1].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[5][1],TypeCodeText[5][1])))) 
        file_tc6_submenu.addAction(QtWidgets.QAction(TypeCodes[5][2].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[5][2],TypeCodeText[5][2])))) 
        file_tc6_submenu.addAction(QtWidgets.QAction(TypeCodes[5][3].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[5][3],TypeCodeText[5][3]))))         
        file_tc6_submenu.addAction(QtWidgets.QAction(TypeCodes[5][4].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[5][4],TypeCodeText[5][4])))) 
        file_tc6_submenu.addAction(QtWidgets.QAction(TypeCodes[5][5].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[5][5],TypeCodeText[5][5])))) 
        
        file_tc7_submenu = file_submenu.addMenu("Space Charge Type Codes")
        file_tc7_submenu.addAction(QtWidgets.QAction(TypeCodes[6][0].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[6][0],TypeCodeText[6][0])))) 
        file_tc7_submenu.addAction(QtWidgets.QAction(TypeCodes[6][1].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[6][1],TypeCodeText[6][1])))) 
        
        file_tc8_submenu = file_submenu.addMenu("Output Print Type Codes")
        file_tc8_submenu.addAction(QtWidgets.QAction(TypeCodes[7][0].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[7][0],TypeCodeText[7][0])))) 
        file_tc8_submenu.addAction(QtWidgets.QAction(TypeCodes[7][1].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[7][1],TypeCodeText[7][1])))) 
        file_tc8_submenu.addAction(QtWidgets.QAction(TypeCodes[7][2].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[7][2],TypeCodeText[7][2])))) 
        file_tc8_submenu.addAction(QtWidgets.QAction(TypeCodes[7][3].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[7][3],TypeCodeText[7][3]))))         
        
        file_tc9_submenu = file_submenu.addMenu("Output Plot Type Codes")
        file_tc9_submenu.addAction(QtWidgets.QAction(TypeCodes[8][0].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[8][0],TypeCodeText[8][0])))) 
        file_tc9_submenu.addAction(QtWidgets.QAction(TypeCodes[8][1].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[8][1],TypeCodeText[8][1])))) 
        file_tc9_submenu.addAction(QtWidgets.QAction(TypeCodes[8][2].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[8][2],TypeCodeText[8][2])))) 
        file_tc9_submenu.addAction(QtWidgets.QAction(TypeCodes[8][3].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[8][3],TypeCodeText[8][3])))) 
        file_tc9_submenu.addAction(QtWidgets.QAction(TypeCodes[8][4].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[8][4],TypeCodeText[8][4])))) 

        
        file_tc10_submenu = file_submenu.addMenu("Other Type Codes")
        file_tc10_submenu.addAction(QtWidgets.QAction(TypeCodes[9][0].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[9][0],TypeCodeText[9][0])))) 
        file_tc10_submenu.addAction(QtWidgets.QAction(TypeCodes[9][1].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[9][1],TypeCodeText[9][1])))) 
        file_tc10_submenu.addAction(QtWidgets.QAction(TypeCodes[9][2].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[9][2],TypeCodeText[9][2])))) 
        file_tc10_submenu.addAction(QtWidgets.QAction(TypeCodes[9][3].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[9][3],TypeCodeText[9][3]))))         
        file_tc10_submenu.addAction(QtWidgets.QAction(TypeCodes[9][4].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[9][4],TypeCodeText[9][4])))) 
        file_tc10_submenu.addAction(QtWidgets.QAction(TypeCodes[9][5].split(' ',1)[0], self, 
            triggered=(lambda: self.help_TypeCodes(TypeCodes[9][5],TypeCodeText[9][5]))))
        
        # About menu
        file_menu = bar.addMenu('About')
        # adding actions to the about menu
        aboutdg_action = QtWidgets.QAction('About DGUI', self, 
                statusTip="About DGUI", triggered=self.about_dgui)
        file_menu.addAction(aboutdg_action)
        versions_list = QtWidgets.QAction('List software versions', self,
                statusTip="List versions of installed software", triggered=self.list_versions)
        file_menu.addAction(versions_list)

        
## allow user to interrupt if DYNAC is running
#        self.interrupt_shortcut = QShortcut(QKeySequence("Ctrl+I"), self)
#        self.interrupt_shortcut.activated.connect(self.sigint_handler)
#
        # use `connect` method to bind signals to desired behavior
#        close_action.triggered.connect(self.close)
        self.force_close = True
        close_action.triggered.connect(lambda: self.close_dgui("dgui"))
        close_all.triggered.connect(lambda: self.close_dgui("all"))

        
    def closeEvent(self, event):            
        if self.force_close is True:
        # Here is where you add code to perform some operation due to the user clicking the X        
            self.close_dgui("all")
#        else:    
#            print('Closed DGUI from the close DGUI option') 


    def create_lambda(x=None):
       return lambda: x


        
###################################################
# define what to do when closing DGUI             #    
###################################################
    def close_dgui(self,scope):
        '''Close DGUI'''
        global last_ifpath, prdir
        ifp = 0
        pdp = 0
        dfp = 0
        with open(dgpath + os.sep + "dgui.ini") as fp:  
            line = fp.readline()
            dynpathff=line.strip()
            cnt = 1
            while line:
                sline=line.strip()
                if "PROJECTDIR" in sline:
                    pdp = 1
                    pdcnt = cnt-1
                if "INPUTFILEPATH" in sline:
                    ifp = 1
                    ifcnt = cnt-1
                if "DISTRIBUTIONFILEPATH" in sline:
                    dfp = 1
                    dfcnt = cnt-1
                line = fp.readline()
                cnt += 1 
        
        cnt = cnt - 1         
        if (ifp == 0):
        # there is no INPUTFILEPATH in the .ini file, so append it 
            with open(dgpath + os.sep + "dgui.ini","r+") as fp:  
                inilines = fp.read().splitlines()
                inilines.append('INPUTFILEPATH '+ last_ifpath + '\n')
                final_string = '\n'.join(inilines)
            with open(dgpath + os.sep + "dgui.ini","w") as fp:  
                fp.writelines(final_string)
            cnt = cnt + 1 
        else:        
        # there is an INPUTFILEPATH in the .ini file, overwrite it             
            with open(dgpath + os.sep + "dgui.ini") as fp:  
                inilines = fp.read().splitlines()
            with open(dgpath + os.sep + "dgui.ini","w") as fp:
                indx=0
                while indx < cnt:
                    if(indx == ifcnt):
                        fp.write('INPUTFILEPATH ' + last_ifpath + '\n')
                    else:    
                        fp.write(inilines[indx]+ '\n')              
                    indx = indx + 1
        if (dfp == 0):
        # there is no DISTRIBUTIONFILEPATH in the .ini file, so append it 
            with open(dgpath + os.sep + "dgui.ini","r+") as fp:  
                inilines = fp.read().splitlines()
                inilines.append('DISTRIBUTIONFILEPATH '+ last_dfpath + '\n')
                final_string = '\n'.join(inilines)
            with open(dgpath + os.sep + "dgui.ini","w") as fp:  
                fp.writelines(final_string) 
            cnt = cnt + 1              
        else:        
        # there is a DISTRIBUTIONFILEPATH in the .ini file, overwrite it             
            with open(dgpath + os.sep + "dgui.ini") as fp:  
                inilines = fp.read().splitlines()
            with open(dgpath + os.sep + "dgui.ini","w") as fp:
                indx=0
                while indx < cnt:
                    if(indx == dfcnt):
                        fp.write('DISTRIBUTIONFILEPATH ' + last_dfpath + '\n')
                    else:    
                        fp.write(inilines[indx]+ '\n')              
                    indx = indx + 1
        if (pdp == 0):
        # there is no PROJECTDIR in the .ini file, so append it    
            with open(dgpath + os.sep + "dgui.ini","r+") as fp:  
                inilines = fp.read().splitlines()
                inilines.append('PROJECTDIR '+ prdir + '\n')
                final_string = '\n'.join(inilines)
            with open(dgpath + os.sep + "dgui.ini","w") as fp:  
                fp.writelines(final_string)
            cnt = cnt + 1
        else:        
        # there is a PROJECTDIR in the .ini file, overwrite it             
            with open(dgpath + os.sep + "dgui.ini") as fp:  
                inilines = fp.read().splitlines()
            with open(dgpath + os.sep + "dgui.ini","w") as fp:
                indx=0
                while indx < cnt:
                    if(indx == pdcnt):
                        fp.write('PROJECTDIR ' + prdir + '\n')
                    else:    
                        fp.write(inilines[indx]+ '\n')              
                    indx = indx + 1
        self.force_close = False
        if hasattr(self.main_widget, 'selectopt'): 
            # close the options window (if it exists)
            self.main_widget.selectopt.close()
        if(scope == "all"):
            self.main_widget.select_closewins()
            self.plotitclose = QtCore.QProcess()
            if (platform.system() == 'Windows') :
                cmd = "Taskkill /IM gnuplot_qt.exe /F"
            if (platform.system() == 'Linux') :
                self.main_widget.getuser()
                cmd = "killall -q -u " + cuser + " gnuplot_x11"
            if (platform.system() == 'Darwin') :
#                self.getuser()
                self.main_widget.getuser()
                cmd = "killall -u " + cuser + " gnuplot_x11"
            self.plotitclose.start(cmd)
            self.plotitclose.waitForFinished()
            self.plotitclose.close()
        self.close()
        
#    def sigint_handler(*args):                    
#        sys.stderr.write('\r')
#        if QMessageBox.question(None, '', "Do you want to stop DYNAC execution?", QMessageBox.Yes | QMessageBox.No, QMessageBox.No) == QMessageBox.Yes:
#             kill_dynac= self.ProcServer4(parent=self)
##            QApplication.quit()
##            QApplication.quit() <-- would stop DGUI
        
    def contextMenuEvent(self, event):
        menu = QMenu(self)
        menu.addAction(self.printAct)
#        menu.addAction(self.cutAct)
#        menu.addAction(self.copyAct)
#        menu.addAction(self.pasteAct)
        menu.exec_(event.globalPos())
        
    def createActions(self):
        self.printAct = QAction("&Print window to file...", self, shortcut="Ctrl+P",
                statusTip="Print the document", triggered=self.print_)
        self.exitAct = QAction("E&xit", self, shortcut="Ctrl+Q",
                statusTip="Exit DGUI", triggered=self.close)

    def print_(self):
# grab the window        
        if (platform.system() == 'Linux') :
            self.linuxpw = QtCore.QProcess()
            self.linuxpw.start("gnome-screenshot -i --delay=1 -w")
        else:
            self.pxmap = self.grab()
            pxmapfn, _ = QFileDialog.getSaveFileName(self, 'Save', '', filter="PNG(*.png);; JPEG(*.jpg)")
            if pxmapfn[-3:] == "png":
                self.pxmap.save(pxmapfn, "png")
            elif pxmapfn[-3:] == "jpg":
                self.pxmap.save(pxmapfn, "jpg")
            
    def printscr_(self):
# grab the whole screen
        if (platform.system() == 'Linux') :
            self.linuxps = QtCore.QProcess()
            self.linuxps.start("gnome-screenshot")
        else:
            self.pxmap = QApplication.primaryScreen().grabWindow(0)
            pxmapfn, _ = QFileDialog.getSaveFileName(self, 'Save', '', filter="PNG(*.png);; JPEG(*.jpg)")
            if pxmapfn[-3:] == "png":
                self.pxmap.save(pxmapfn, "png")
            elif pxmapfn[-3:] == "jpg":
                self.pxmap.save(pxmapfn, "jpg")

    def list_versions(self):
        global sw0, sw1, sw2, sw3, psw1, dsw1, versiontxt
        sw3 = ''
# List versions of DYNAC related software
        cmd = "gnuplot -V"
        sw_version = os.popen(cmd)
        sw1 = sw_version.read().rstrip("\n")
        #print("Checked gnuplot  version:",sw1)
        sw_version.close()

        if (platform.system() == 'Windows') :
            sw2 = "python " + sys.version
        else:    
# On linux and mac, also get gcc version when asking python version. get rid of gcc version
            sw2 = "python " + sys.version
            splitsw2 = sw2.split('\n')
            sw2 = splitsw2[0]
        #print("Checked python   version:",sw2)
                                       
        size = len(dynpath)
        if (platform.system() == 'Linux') :
            plotit_exe = dynpath[:-3] + "plot" + os.sep + "dynplt"
            dynac_exe = dynpath + os.sep + dynacv
# .linux_distribution will be deprecated, replace by new calls
#            sw0_prefix = distro.linux_distribution()[0] + ' ' + distro.linux_distribution()[1] + ' ' + distro.linux_distribution()[2] +  ' ' 
            sw0_prefix = distro.name() + ' ' + distro.version() + ' ' + distro.codename() +  ' ' 
        elif (platform.system() == 'Darwin') : 
            plotit_exe = dynpath[:-3] + "plot" + os.sep + "dynplt"
            dynac_exe = dynpath + os.sep + dynacv
            mac_version = platform.mac_ver()[0]
            vindex = -1
            if(mac_version[0:2] == '10'):
                vindex = int(mac_version[3:5])
            elif(mac_version[0:2] == '11'):
                vindex = 16
            elif(mac_version[0:2] == '12'):
                vindex = 17                    
            elif(mac_version[0:2] == '13'):
                vindex = 18                    
            elif(mac_version[0:2] == '14'):
                vindex = 19                    
            elif(mac_version[0:2] == '15'):
                vindex = 20                                                    
            if(vindex == -1):                               
                sw0_prefix = ''
            else:    
                sw0_prefix = sys.platform + ' ' + mac_os_names[vindex] + ' '                                       
        else:        
            plotit_exe = dynpath[:-4] + "plot" + os.sep + "dynplt"
            dynac_exe = dynpath[:-1] + os.sep + dynacv
            sw0_prefix = ''            
#        print("plotit binary: ",plotit_exe)                          
        cmd = plotit_exe + " -v"
        sw_version = os.popen(cmd)
        psw1 = sw_version.read().rstrip("\n")
        #print("Checked dyndat   version:",psw1)
        sw_version.close() 

#        print("dynac binary: ",dynac_exe)                          
        cmd = dynac_exe + " -v"
        sw_version = os.popen(cmd)
        dsw1 = sw_version.read().rstrip("\n")
        dsw1 = dsw1.strip()
        dsw1 = dsw1[7:-7]
        #print("Checked dynac    version:",dsw1)
        sw_version.close()
            
        #print("Checked dgui     version:",dgui_v)                                   
                           
        cmd = "gfortran"
        args = '-v'
        self.check_version = QtCore.QProcess() 
        self.check_version.readyReadStandardError.connect(self.handle_stderr)            
        self.check_version.finished.connect(self.check_version_finished)  # Clean up once complete.
#        print("start cmd",dir(self.check_version))
        self.check_version.start(cmd,[args])                                              
                        
        # list the OS version
        sw0 = sw0_prefix + platform.platform()
           
    def check_version_finished(self):
        global sw0, sw1, sw2, sw3, dsw1, psw1, versiontxt  
        versiontxt = ""
        versiontxt = ""
        versionstxt = sw0 + "\n" + dsw1 + "\n" + dgui_v + "\n" + psw1 + "\n" + sw1 + "\n" + sw2 + "\n" + sw3 + "\n"
        svtxt = versionstxt.split('\n')
        sw_versions_box = QMessageBox(self)
        sw_versions_box.setSizeGripEnabled(True)
        sw_versions_box.setWindowTitle('Software Versions')
        sw_versions_box.setStyleSheet("QLabel{min-width:700 px; font-size: 16px;} QPushButton{ width:100px; font-size: 16px; }")       
        sw_versions_box.setText(versionstxt)
#        sw_versions_box.setDetailedText(versionstxt)
        sw_versions_box.show()                
        self.check_version = None
                                                
    def handle_stderr(self):
        global sw3
        data = self.check_version.readAllStandardError().data().decode("utf8")
        sw3list = data.split('\n')
        nofels = len(sw3list)
        sw3 = sw3list[nofels-2]
        sw3 = 'gfortran ' + sw3           
#        print("handle_stderr:", sw3)                                   

    def help_dynac(self):
# Open the DYNAC User Guide (assumes it is in ..../dynac/help directory)
        if (platform.system() == 'Windows') :
            path_to_acro = r"C:\Program Files\Adobe\Acrobat DC\Acrobat\Acrobat.exe"
            file_exists = exists(path_to_acro)  
            if (file_exists) :
#                cmd='"C:\Program Files\Adobe\Acrobat DC\Acrobat\Acrobat.exe"' + ' /n ' +  default_ugpath + os.sep + 'dynac_UG.pdf'
                cmd = r'"C:\Program Files\Adobe\Acrobat DC\Acrobat\Acrobat.exe"'
                argmnt1 =  ' /n '  
                argmnt2 =  default_ugpath + os.sep + 'dynac_UG.pdf'                
            else:   
                print("Can not find ",path_to_acro)
                print("Trying AcroRd32.exe") 
#                cmd='"AcroRd32.exe" "' + default_ugpath + os.sep + "dynac_UG.pdf" + '"'
                cmd='"AcroRd32.exe"'
                argmnt1 =  ' /n '  
                argmnt2 =  default_ugpath + os.sep + 'dgui_UG.pdf'               
        if (platform.system() == 'Linux') :
            #print("linux dynac PDF with acr, evi, xrd ",acr_selected,evi_selected,xrd_selected)
            if (acr_selected == True) :
                cmd="acroread"
            elif (evi_selected == True):   
                cmd="evince"
            else:    
                cmd="xreader" 
            argmnts = default_ugpath + os.sep + "dynac_UG.pdf"
        if (platform.system() == 'Darwin') :  
#            cmd="open " + default_ugpath + os.sep + "dynac_UG.pdf"
            cmd = "open"
            argmnts = default_ugpath + os.sep + "dynac_UG.pdf"

        if self.dynugpdf is None: 
            self.dynugpdf = QtCore.QProcess()
            self.dynugpdf.finished.connect(self.dynugpdf_finished)  # Clean up once complete.
            if (platform.system() == 'Windows') :
#                print("Opening DYNAC UG with: ",cmd,argmnt1,argmnt2)
                self.dynugpdf.start(cmd,[argmnt1,argmnt2])
            else:
#                print("Opening DYNAC UG with: ",cmd,argmnts)
                self.dynugpdf.start(cmd,[argmnts])
#        else:
#            print("DYNAC UG already open")        

    def dynugpdf_finished(self):
        self.dynugpdf = None
        
    def help_dgui(self):
# Open the DGUI User Guide (assumes it is in ..../dynac/help directory)       
        if (platform.system() == 'Windows') :
            path_to_acro = r"C:\Program Files\Adobe\Acrobat DC\Acrobat\Acrobat.exe"
            file_exists = exists(path_to_acro)  
            if (file_exists) :
#                cmd='"C:\Program Files\Adobe\Acrobat DC\Acrobat\Acrobat.exe" "' + '" /n "' + default_ugpath + os.sep + "dgui_UG.pdf" + '"'
#                cmd='"C:\Program Files\Adobe\Acrobat DC\Acrobat\Acrobat.exe"' + ' /n ' + default_ugpath + os.sep + 'dgui_UG.pdf' 
                cmd = r'"C:\Program Files\Adobe\Acrobat DC\Acrobat\Acrobat.exe"'
                argmnt1 =  ' /n '  
                argmnt2 =  default_ugpath + os.sep + 'dgui_UG.pdf'
            else:   
                print("Can not find ",path_to_acro)
                print("Trying AcroRd32.exe") 
#                cmd='"AcroRd32.exe" "' + default_ugpath + os.sep + "dgui_UG.pdf" + '"'
                cmd='"AcroRd32.exe"' 
                argmnt1 =  ' /n '  
                argmnt2 =  default_ugpath + os.sep + 'dgui_UG.pdf'               
        if (platform.system() == 'Linux') :
            #print("linux dynac PDF with acr, evi, xrd",acr_selected,evi_selected,xrd_selected)
            if (acr_selected == True) :
                cmd="acroread"
            elif (evi_selected == True): 
                cmd="evince"
            else:    
                cmd="xreader"
            argmnts = default_ugpath + os.sep + "dgui_UG.pdf"
        if (platform.system() == 'Darwin') :
#            cmd="open " + default_ugpath + os.sep + "dgui_UG.pdf"
            cmd = "open"
            argmnts = default_ugpath + os.sep + "dgui_UG.pdf"
#        print("Opening DGUI UG with: ",cmd)
#        self.dgugpdf = QtCore.QProcess()
#        self.dgugpdf.start(cmd)
        if self.dgugpdf is None: 
            self.dgugpdf = QtCore.QProcess()
            self.dgugpdf.finished.connect(self.dgugpdf_finished)  # Clean up once complete.
            if (platform.system() == 'Windows') :
#                print("Opening DGUI UG with: ",cmd,argmnt1,argmnt2)
                self.dgugpdf.start(cmd,[argmnt1,argmnt2])
            else:
#                print("Opening DYNAC UG with: ",cmd,argmnts)
                self.dgugpdf.start(cmd,[argmnts])          
#        else:
#            print("DGUI UG already open")        

    def dgugpdf_finished(self):
        self.dgugpdf = None

    def about_dgui(self):
## DYNAC GUI User Guide is assumed to be in ..../dynac/help directory
        txt1= "DGUI is a graphical user interface to the beam dynamics code DYNAC. "
        txt2= dguiv + " works best with DYNAC V7R4 and with python3.10 or newer."
        abouttxt = txt1 + txt2
        QMessageBox.about(self, "About DGUI", abouttxt)

######################################################
# Help related to Type Codes listed as text in sub-sub-menu box
######################################################
    def help_TypeCodes(self,tcname,tctext):
#        print(tcname,tctext)
        self.TCW = TypeCodeInfo(self,tcname,tctext)
        self.TCW.show()
        self.TCW.activateWindow()
        self.TCW.raise_() 

######################################################
# MAINLAYOUT class                                   #
######################################################
class MainLayout(QWidget):
    def __init__(self, parent=None):
        super().__init__(parent)

        # create layout and place widgets

        self.top_line1 = QtWidgets.QLabel(self)
#        self.top_line.setText(self.get_dynpath())
        self.top_line1.setText(systembin)
        self.top_line1.resize(750,25)
        self.top_line1.move(125, 4) 

        self.top_line2 = QtWidgets.QLabel(self)
        if not prdir == "" :
            self.top_line2.setText("Project: " + os.path.normpath(prdir))
            self.top_line2.resize(750,25)
            self.top_line2.move(125, 18) 
             
#        self.top_DGUIV = QtWidgets.QLabel(self)
#        self.top_DGUIV.setText(dgui_v)
#        self.top_DGUIV.resize(200,25)
#        self.top_DGUIV.move(1030, 6)        

        # Add OPTIONS button
        self.PlotBtn7 = QtWidgets.QPushButton('Options', self)
        self.PlotBtn7.resize(100,32)
        self.PlotBtn7.move(5, 5)                
        self.LeftBtn1 = QtWidgets.QPushButton('Get dist. file', self)
        self.LeftBtn1.resize(100,32)
        self.LeftBtn1.move(5, 50)        
        self.LeftBtn2 = QtWidgets.QPushButton('Get input file', self)
        self.LeftBtn2.resize(100,32)
        self.LeftBtn2.move(5, 100)        

        self.text_box1 = QtWidgets.QTextEdit(self)
        self.text_box1.resize(750,44)
        self.text_box1.move(120, 44)        
        self.text_box2 = QtWidgets.QTextEdit(self)
        self.text_box2.resize(750,44)
        self.text_box2.move(120, 94)

        self.RightBtn0 = QtWidgets.QPushButton('Save project', self)
        self.RightBtn0.resize(100,32)
        self.RightBtn0.move(880, 5)        
        self.RightBtn1 = QtWidgets.QPushButton('Plot dist. file', self)
        self.RightBtn1.resize(100,32)
        self.RightBtn1.move(880, 50)        
        self.RightBtn2 = QtWidgets.QPushButton('Run DYNAC', self)
        self.RightBtn2.resize(100,32)
        self.RightBtn2.move(880, 100)        
                
        self.save_button = QtWidgets.QPushButton('Save')
        self.clear_button = QtWidgets.QPushButton('Clear')
        
#       create top upper options box
        self.groupBox1 = QGroupBox(self)
        self.groupBox1.setGeometry(QtCore.QRect(990, 24, 254, 70))
        self.groupBox1.setTitle("Plot dist. options")
#       create top lower options box
        self.groupBox2 = QGroupBox(self)
        self.groupBox2.setGeometry(QtCore.QRect(990, 92, 254, 50))
        self.groupBox2.setTitle("Run time options")       
#       create top lower options box
        self.groupBox3 = QGroupBox(self)
        self.groupBox3.setGeometry(QtCore.QRect(1104, 280, 140, 70))
        self.groupBox3.setTitle("Envelope options")

#       create top upper options box buttons        
        # Add L radio button (coordinates within groupBox)
        self.radio1 = QRadioButton(self.groupBox1)
        self.radio1.setGeometry(QtCore.QRect(10, 20, 110, 25))
        self.radio1.setText("Distribution")
        self.radio1.setChecked(True)
        # Add R radio button (coordinates within groupBox)
        self.radio2 = QRadioButton(self.groupBox1)
        self.radio2.setGeometry(QtCore.QRect(140, 20, 90, 25))
        self.radio2.setText("Density")
        # Add L check box (coordinates within groupBox)        
        self.checkBox1 = QCheckBox(self.groupBox1)
        self.checkBox1.setGeometry(QtCore.QRect(10, 45, 90, 25))
        self.checkBox1.setText("Dst")
        self.checkBox1.setChecked(True)
        # Add R check box (coordinates within groupBox)        
        self.checkBox2 = QCheckBox(self.groupBox1)
        self.checkBox2.setGeometry(QtCore.QRect(140, 45, 90, 25))
        self.checkBox2.setText("Profiles")
#       create top lower options box buttons
        # Add L check box (coordinates within groupBox2)        
        self.checkBox3 = QCheckBox(self.groupBox2)
        self.checkBox3.setGeometry(QtCore.QRect(10, 20,110, 25))
        self.checkBox3.setText("Clear page")
        self.checkBox3.setChecked(True)
#       create bottom plot options box        
        # Add plotit button 
        self.PlotBtn1 = QtWidgets.QPushButton('plotit', self)
        self.PlotBtn1.resize(75,32)
        self.PlotBtn1.move(975, 150)        
        # Add close plotit windows button 
        self.PlotBtn1s = QtWidgets.QPushButton('Save', self)
        self.PlotBtn1s.resize(75,32)
        self.PlotBtn1s.move(1052, 150)        
        # Add close plotit windows button 
        self.PlotBtn1c = QtWidgets.QPushButton('Close gnuplots', self)
        self.PlotBtn1c.resize(116,32)
        self.PlotBtn1c.move(1129, 150)        
        # Add plot Erms button 
        self.PlotBtn2 = QtWidgets.QPushButton('Plot Erms', self)
        self.PlotBtn2.resize(127,32)
        self.PlotBtn2.move(975, 200)        
        # Add plot W button 
        b3txt="Plot W," + "\N{GREEK SMALL LETTER PHI}"
        self.PlotBtn3 = QtWidgets.QPushButton(b3txt, self)
        self.PlotBtn3.resize(127,32)
        self.PlotBtn3.move(975, 250)        
        # Add plot envelopes button 
        self.PlotBtn4 = QtWidgets.QPushButton('Plot X,Y env.', self)
        self.PlotBtn4.resize(127,32)
        self.PlotBtn4.move(975, 300)        
        # Add plot dW button 
        b5txt="Plot dW,d" + "\N{GREEK SMALL LETTER PHI}" + " env."
        self.PlotBtn5 = QtWidgets.QPushButton(b5txt, self)
        self.PlotBtn5.resize(127,32)
        self.PlotBtn5.move(975, 350)        
        # Add plot dispersion button 
        self.PlotBtn6 = QtWidgets.QPushButton('Plot dispersion', self)
        self.PlotBtn6.resize(127,32)
        self.PlotBtn6.move(975, 400)
        # Add Plot all 7 plots button
        self.PlotBtn28 = QtWidgets.QPushButton('Plot all 7 plots', self)
        self.PlotBtn28.resize(127,32)
        self.PlotBtn28.move(975, 550)               
        # Add Save plots button
        self.PlotBtn18 = QtWidgets.QPushButton('Save visible plots', self)
        self.PlotBtn18.resize(127,32)
        self.PlotBtn18.move(975, 600)               
        # Add Close plots button
        self.PlotBtn8 = QtWidgets.QPushButton('Close plots', self)
        self.PlotBtn8.resize(127,32)
        self.PlotBtn8.move(1105, 600)               
        # Add plot transmission button 
        b9txt ="Plot x\u0304 , y\u0304 , TX"
#        b9txt = 'Plot <font><span style="text-decoration: overline">X</span></font>, <font><span style="text-decoration: overline">Y</span></font>, TX'
#        b9txt = '<font><span style="text-decoration: overline">X</span></font>'
        self.PlotBtn9 = QtWidgets.QPushButton(b9txt, self)
        self.PlotBtn9.resize(127,32)
        self.PlotBtn9.move(975, 450)
        # Add plot losses button 
        self.PlotBtn12 = QtWidgets.QPushButton('Plot losses', self)
        self.PlotBtn12.resize(127,32)
        self.PlotBtn12.move(975,500)         
                       
        # Add plot Env check box (coordinates within groupBox)        
        self.checkBox4 = QCheckBox(self.groupBox3)
        self.checkBox4.setGeometry(QtCore.QRect(10, 22, 60, 25))
        self.checkBox4.setText("Env")
        self.checkBox4.setChecked(True)
        # Add plot Ext check box (coordinates within groupBox)        
        self.checkBox5 = QCheckBox(self.groupBox3)
        self.checkBox5.setGeometry(QtCore.QRect(75, 22, 60, 25))
        self.checkBox5.setText("Ext")
        # Add show elements check box (coordinates within groupBox)        
        self.checkBox8 = QCheckBox(self.groupBox3)
        self.checkBox8.setGeometry(QtCore.QRect(10, 45, 60, 25))
        self.checkBox8.setText("ELE")
       # Add show aperture check box (coordinates within groupBox)        
        self.checkBox9 = QCheckBox(self.groupBox3)
        self.checkBox9.setGeometry(QtCore.QRect(75, 45, 60, 25))
        self.checkBox9.setText("Aper")
                

        # Add L button 1 call back
        self.LeftBtn1.clicked.connect(self.get_dst)
        self.LeftBtn1.setToolTip('Select a particle distribution file to be plotted')  
        # Add L button 2 call back
        self.LeftBtn2.clicked.connect(self.get_inp)
        self.LeftBtn2.setToolTip('Select a DYNAC input file')  
        
        # Add R button 0 call back
#        self.RightBtn0.clicked.connect(self.main_widget.save_files)
        self.RightBtn0.clicked.connect(self.save_files)
        self.RightBtn0.setToolTip('Save files to the selected project directory')  
        # Add R button 1 call back
        self.RightBtn1.clicked.connect(self.plot_dst)
        self.RightBtn1.setToolTip('Plot a particle distribution file')  
#        self.RightBtn1.setStyleSheet("color : #0000ff; ")    
                # Add R button 2 call back
        self.RightBtn2.clicked.connect(self.run_dyn)
        self.RightBtn2.setToolTip('Run DYNAC using the input file') 
        self.RightBtn2.setStyleSheet("color : #0000ff; ")        
        # Add radio button 1 call back
        self.radio1.clicked.connect(self.get_r1)
        self.radio1.setToolTip('If selected and Dst selected, macro particles will be plotted')  
        # Add radio button 2 call back
        self.radio2.clicked.connect(self.get_r2)
        self.radio2.setToolTip('If selected, density plots will be displayed')  
        # Add L check 1 call back
        self.checkBox1.clicked.connect(self.get_cb1)
        self.checkBox1.setToolTip('If selected and Distribution selected, macro particles will be plotted')  
        # Add R check 2 call back
        self.checkBox2.clicked.connect(self.get_cb2)
        self.checkBox2.setToolTip('If selected, profiles will be plotted')  
        # Add L check 3 call back
        self.checkBox3.clicked.connect(self.get_cb3)
        self.checkBox3.setToolTip('If selected, the text window will be cleared before new output is displayed')  
        # Add Env check call back
        self.checkBox4.clicked.connect(self.get_cb4)
        self.checkBox4.setToolTip('If selected, envelopes will be plotted')  
        # Add Ext check  call back
#        self.checkBox5.clicked.connect(self.get_cb3)
        self.checkBox5.setToolTip('If selected, beam extents will be plotted')  
        # Add ELE check  call back
#        self.checkBox8.clicked.connect(self.get_cb3)
        self.checkBox8.setToolTip('If selected, beam line elements will be plotted')  
        # Add plotit button call back
        self.PlotBtn1.clicked.connect(self.plotit)
        self.PlotBtn1.setToolTip('Display the plots requested in the DYNAC input file')  
        self.PlotBtn1.setStyleSheet("color : #0000ff; ") 
        # Add save gnuplots button call back
        self.PlotBtn1s.clicked.connect(self.splotit)
        self.PlotBtn1s.setToolTip('Save the plots requested in the DYNAC input file')  
        self.PlotBtn1s.setStyleSheet("color : #0000ff; ") 
        # Add close plotit windows button call back
        self.PlotBtn1c.clicked.connect(self.close_gnuplots)
        self.PlotBtn1c.setToolTip('Close the gnuplots created by plotit')  
        # Add plot Erms button call back
        self.PlotBtn2.clicked.connect(self.plot_erms)
        self.PlotBtn2.setToolTip('Plot the Erms for the 3 phase planes along the beam axis') 
        # Add plot W,phase button call back
        self.PlotBtn3.clicked.connect(self.plot_energy)
        self.PlotBtn3.setToolTip('Plot energy and synchronous phase along the beam axis')
        # Add plot transverse envelopes button call back         
        self.PlotBtn4.clicked.connect(self.plot_t_envelopes)
        self.PlotBtn4.setToolTip('Plot the horizontal and vertical envelopes along the beam axis') 
        # Add plot longitudinal envelopes button call back                 
        self.PlotBtn5.clicked.connect(self.plot_l_envelopes)
        self.PlotBtn5.setToolTip('Plot the longitudinal (energy and phase) envelopes along the beam axis') 
        # Add plot dispersion button call back                 
        self.PlotBtn6.clicked.connect(self.plot_dispersion)
        self.PlotBtn6.setToolTip('Plot the horizontal and vertical dispersion along the beam axis') 
        # Add OPTIONS button call back                 
        self.PlotBtn7.clicked.connect(self.select_opt)
        self.PlotBtn7.setToolTip('Select options and preferences') 
        # Add plot transmission button call back                 
        self.PlotBtn9.clicked.connect(self.plot_transmission)
        self.PlotBtn9.setToolTip('Plot the beam centroid and transmisssion along the beam axis') 
        # Add plot losses button call back
        self.PlotBtn12.clicked.connect(self.plot_losses)
        self.PlotBtn12.setToolTip('Plot the beam losses along the beam axis')         
        
        # Add Save visible graph windows button call back                 
        self.PlotBtn18.clicked.connect(self.select_savewins)
        self.PlotBtn18.setToolTip('Save visible graphics window(s), including the distribution plot (if visible)') 
        # Add Plot all 7 graph windows button call back                 
        self.PlotBtn28.clicked.connect(self.select_plotallwins)
        self.PlotBtn28.setToolTip('Plot all 7 graphics windows, but distribution plot not included') 
        
        # Add close graph windows button call back                 
        self.PlotBtn8.clicked.connect(self.select_closewins)
        self.PlotBtn8.setToolTip('Close all graphics windows') 


        # big text box at bottomgraphicsView where info will be logged
        self.inpLog = QtWidgets.QTextEdit(self)
#        self.inpLog = QtWidgets.QPlainTextEdit(self)
        self.inpLog.resize(960, 520)
        self.inpLog.move(10, 145)
# Change font, colour of big text box
#        self.inpLog.setStyleSheet(
#        """QPlainTextEdit {background-color: #333;
#                           color: #00FF00;
#                           text-decoration: underline;
#                           font-family: Courier;}""")
        self.cursor = self.inpLog.textCursor()
        self.inpLog.setTextCursor(self.cursor)

# allow user to interrupt if DYNAC is running
        self.interrupt_shortcut = QShortcut(QKeySequence("Ctrl+I"), self)
        self.interrupt_shortcut.activated.connect(self.sigint_handler)

######################################################
    def sigint_handler(self):                    
######################################################
        runbutcol=self.RightBtn2.styleSheet()        
        if runbutcol[8:15] == "#ff0000" :
            sys.stderr.write('\r')
            if QMessageBox.question(None, '', "Do you want to stop DYNAC execution?", QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No, QMessageBox.StandardButton.No) == QMessageBox.StandardButton.Yes:
                 mytext = "\n\nDYNAC execution interrupted by user\n"
                 self.log_it(mytext)        
                 kill_resp = self.dynraw.kill()
#                QApplication.quit() <-- would stop DGUI

######################################################
    def log_it(self,mytext):
        '''Log the text in the log text box'''               
######################################################
        self.inpLog.insertPlainText(mytext)
        self.inpLog.verticalScrollBar().setValue(self.inpLog.verticalScrollBar().maximum())
        self.inpLog.repaint()

######################################################
# define calls related to the buttons                #    
######################################################
    def select_opt(self):
        '''Select options and preferences'''
        self.selectopt = Options(self)
        self.selectopt.move(1270, 20)
        self.selectopt.show()

######################################################
    def select_closewins(self):                      #
        '''Close all graphics windows'''             #
######################################################
        if hasattr(self, 'win'): 
            self.win.close()
        if hasattr(self, 'win1'): 
            self.win1.close()
        if hasattr(self, 'win2'): 
            self.win2.close()
        if hasattr(self, 'win3'): 
            self.win3.close()
        if hasattr(self, 'win4'): 
            self.win4.close()
        if hasattr(self, 'win5'): 
            self.win5.close()
        if hasattr(self, 'win6'): 
            self.win6.close()
        if hasattr(self, 'win7'): 
            self.win7.close()

######################################################
    def select_savewins(self):                      #
        '''Save graphics windows'''                  #
######################################################
        global gr_file_ext
        # delete the old files before creating the new ones            
        nof_gfiles = 8
        savedfiles = 0
        gfnum = 1
        sfnum = str(gfnum).zfill(3)
        cp_file=os.path.dirname(ifname) + os.sep + "dyngraf" + sfnum + "." + gr_file_ext
        while gfnum < nof_gfiles+1 :
            if (os.path.exists(cp_file)) :
                if (platform.system() == 'Windows') :
                    cmd = 'del /Q "' + cp_file + '"'
                else:
                    cmd = 'rm ' + cp_file
                os.system(cmd)
            gfnum += 1
            sfnum = str(gfnum).zfill(3)
            cp_file=os.path.dirname(ifname) + os.sep + "dyngraf" + sfnum + "." + gr_file_ext
        # creating the new ones            
        if hasattr(self, 'win'): 
#            if not self.win.closed causes AttributeError: 'QDialog' object has no attribute 'closed'
            if self.win.isVisible() : 
                cp_file=os.path.dirname(ifname) + os.sep + "dyngraf001." + gr_file_ext
                self.win.grab().save(cp_file)
                savedfiles += 1
        if hasattr(self, 'win1'): 
            if not self.win1.closed : 
                cp_file=os.path.dirname(ifname) + os.sep + "dyngraf002." + gr_file_ext
                self.win1.grab().save(cp_file)
# gif not supported; need to use convertors, but not all platforms have these standard                
#                if (platform.system() == 'Darwin') and (gr_file_ext == "gif") :  
#                    cp_nfile=os.path.dirname(ifname) + os.sep + "dyngraf002.gif"
#                    cmd = 'sips -s format gif ' + cp_file + ' --out ' + cp_nfile
#                    os.system(cmd)
#                    cmd = 'rm ' + cp_file
#                    os.system(cmd)
                savedfiles += 1
        if hasattr(self, 'win2'): 
            if not self.win2.closed : 
                cp_file=os.path.dirname(ifname) + os.sep + "dyngraf003." + gr_file_ext
                self.win2.grab().save(cp_file)
                savedfiles += 1
        if hasattr(self, 'win3'): 
            if not self.win3.closed : 
                cp_file=os.path.dirname(ifname) + os.sep + "dyngraf004." + gr_file_ext
                self.win3.grab().save(cp_file)
                savedfiles += 1
        if hasattr(self, 'win4'): 
            if not self.win4.closed : 
                cp_file=os.path.dirname(ifname) + os.sep + "dyngraf005." + gr_file_ext
                self.win4.grab().save(cp_file)
                savedfiles += 1
        if hasattr(self, 'win5'): 
            if not self.win5.closed : 
                cp_file=os.path.dirname(ifname) + os.sep + "dyngraf006." + gr_file_ext
                self.win5.grab().save(cp_file)
                savedfiles += 1
        if hasattr(self, 'win6'): 
            if not self.win6.closed : 
                cp_file=os.path.dirname(ifname) + os.sep + "dyngraf007." + gr_file_ext
                self.win6.grab().save(cp_file)
                savedfiles += 1
        if hasattr(self, 'win7'): 
            if not self.win7.closed : 
                cp_file=os.path.dirname(ifname) + os.sep + "dyngraf008." + gr_file_ext
                self.win7.grab().save(cp_file)
                savedfiles += 1
        mytext = "Saved " + str(savedfiles) + " plot(s) with extension " + gr_file_ext + "\n"
        self.log_it(mytext)

######################################################
    def select_plotallwins(self):                    #
        '''Plot all graphics windows'''              #
######################################################
        global gr_file_ext
        # delete the old files before creating the new ones            
        nof_gfiles = 8
        savedfiles = 0
        gfnum = 1
        sfnum = str(gfnum).zfill(3)
        cp_file=os.path.dirname(ifname) + os.sep + "dyngraf" + sfnum + "." + gr_file_ext
        while gfnum < nof_gfiles+1 :
            if (os.path.exists(cp_file)) :
                if (platform.system() == 'Windows') :
                    cmd = 'del /Q "' + cp_file + '"'
                else:
                    cmd = 'rm ' + cp_file
                os.system(cmd)
            gfnum += 1
            sfnum = str(gfnum).zfill(3)
            cp_file=os.path.dirname(ifname) + os.sep + "dyngraf" + sfnum + "." + gr_file_ext
        # open all 7 windows and plot them 
        self.plot_erms()
        self.plot_energy()
        self.plot_t_envelopes()
        self.plot_l_envelopes()
        self.plot_dispersion()
        self.plot_transmission()
        self.plot_losses()
        #      
#        cp_file=os.path.dirname(ifname) + os.sep + "dyngraf002." + gr_file_ext
#        self.win1.grab().save(cp_file)
        #
#        cp_file=os.path.dirname(ifname) + os.sep + "dyngraf003." + gr_file_ext
#        self.win2.grab().save(cp_file)
        #
#        cp_file=os.path.dirname(ifname) + os.sep + "dyngraf004." + gr_file_ext
#        self.win3.grab().save(cp_file)
        #
#        cp_file=os.path.dirname(ifname) + os.sep + "dyngraf005." + gr_file_ext
#        self.win4.grab().save(cp_file)
        #
#        cp_file=os.path.dirname(ifname) + os.sep + "dyngraf006." + gr_file_ext
#        self.win5.grab().save(cp_file)
        #
#        cp_file=os.path.dirname(ifname) + os.sep + "dyngraf007." + gr_file_ext
#        self.win6.grab().save(cp_file)
        #
#        cp_file=os.path.dirname(ifname) + os.sep + "dyngraf008." + gr_file_ext
#        self.win7.grab().save(cp_file)
        
#        mytext = "Saved 7 plots with extension " + gr_file_ext + "\n"
#        self.log_it(mytext)


######################################################
    def dstfileSelected(self, fname1):               #
######################################################
        global dfname, last_dfpath
        if fname1:
            dfname=fname1
            try:
                string_length=len(dfname)
                if string_length != 0 :
#                distLog.delete('1.0',fd.END)
#                distLog.insert(0.0, dfname)
                    # fix dfname
                    dfname = os.path.abspath(dfname)
#                print("dist. file name: ",dfname)
                    last_dfpath=""
                    last_sep=dfname.rfind(os.sep)
                    last_dfpath=dfname[0:last_sep]
#                print("new dist. path=",last_dfpath)
                    self.text_box1.setText(dfname)
            except:
                msg1 = QMessageBox(self)
                msg1.setWindowTitle("Error Message")                                 
                msg1.setText("Failed to read distribution file\n'%s'" % dfname)
                msg1.setIcon(QMessageBox.Icon.Critical)
#                msg1.setInformativeText(e)
                msg1.exec()                                                                                                   
#                print("Open Source File", "Failed to read distribution file\n'%s'" % dfname)
            return
                                        
######################################################
    def get_dst(self):                               #
        '''get a DYNAC particle distribution file''' #
######################################################
        global dfname, last_dfpath
#        fname1 = QFileDialog.getOpenFileName(self, "Open file",last_dfpath, 
#         "Distribution files (*.dst);;Text files (*.txt);;All files (*.*)")
        filedialog = QFileDialog(self,"Open file",last_dfpath,"Distribution files (*.dst);;Text files (*.txt);;All files (*.*)")
        filedialog.setFixedSize(900,440)
        filedialog.fileSelected.connect(self.dstfileSelected)
        filedialog.show()     

######################################################
    def inpfileSelected(self, fname2):               #
######################################################
        global ifname, ifpath, last_ifpath
        if fname2:
            ifname=fname2
            try:
                string_length=len(ifname)
                if string_length != 0 :
                    # fix ifname
                    ifname = os.path.abspath(ifname)
                    plen= string_length - len(os.path.basename(ifname))
                    ifpath = os.path.abspath(ifname[0:plen])
                    ifpath = ifpath.rstrip()
                    ifpath = ifpath + os.sep
                    last_ifpath=ifpath
                    self.text_box2.setText(ifname)
            except:           
                msg2 = QMessageBox(self)
                msg2.setWindowTitle("Error Message")                                 
                msg2.setText("Failed to read input file\n'%s'" % ifname)
                msg2.setIcon(QMessageBox.Icon.Critical)
                msg2.setInformativeText("This is a warning")
                msg2.exec()                                                                                             
#                print("Open Source File", "Failed to read input file\n'%s'" % ifname)
            return
            
######################################################
    def get_inp(self):                               #
        '''get a DYNAC input file'''                 #
######################################################
        global ifname, ifpath, last_ifpath
#        fname2 = QFileDialog.getOpenFileName(self, "Open file",last_ifpath, 
#         "Input files (*.in);;Data files (*.dat);;Text files (*.txt);;All files (*.*)")
        filedialog = QFileDialog(self,"Open file",last_ifpath,"Input files (*.in);;Data files (*.dat);;Text files (*.txt);;All files (*.*)")
        filedialog.setFixedSize(900,440)
        filedialog.fileSelected.connect(self.inpfileSelected)
        filedialog.show()     

######################################################
    def save_close_spin(self):                       #
        '''set project files directory'''            #
######################################################
        global prdir, project_name, prnumber, pdname, was_in_spbox
        prnumber = self.ex2.sp.value()        
        mytext = "Project number set to " + str(prnumber) + "\n"
        self.log_it(mytext)
        prdir = pdname + "/" + project_name + str(prnumber)
        try:
            os.mkdir(prdir)
        except OSError:
            mytext = "Creation of the project directory " + os.path.abspath(prdir) + " failed\n"
            redColor = QColor(255, 0, 0)
            blackColor = QColor(0, 0, 0)
            self.inpLog.setTextColor(redColor)
            self.inpLog.insertPlainText(mytext)
            self.inpLog.setTextColor(blackColor)
            self.inpLog.verticalScrollBar().setValue(self.inpLog.verticalScrollBar().maximum())
            self.inpLog.repaint()
        else:
            mytext = "Creation of the project directory " + os.path.abspath(prdir) + " successful\n"
            self.log_it(mytext)
            was_in_spbox = True
            self.top_line2.setText("Project: " + os.path.normpath(prdir))
            self.top_line2.resize(750,25)
            self.top_line2.move(125, 18) 
            
#       close the spinbox window
#        self.ex2.setWindowModality(Qt.NonModal)
        self.ex2.close()
#        self.mymain = MainWindow
#        self.mymain.setWindowModality(self, Qt.WindowModal)

######################################################
    def close_spin(self):                            #
        '''project files directory cancelled'''      #
######################################################
        global prdir, project_name, prnumber, pdname
        mytext = "Project number NOT changed, still set to " + str(prnumber) + "\n"
        self.log_it(mytext)
        if(prdir == ""):
            mytext = "Currently NO project directory set"
            self.log_it(mytext)
        else:    
            mytext = "Current project directory: " + os.path.abspath(prdir)
            self.log_it(mytext)
        self.ex2.close()

######################################################
    def save_close_check(self):                      #
        '''save project files'''                     #
######################################################
        global prdir, project_name, prnumber, pdname, was_in_spbox
        global ifname, prdir, project_name
        global gr_file_ext
        list_of_ifiles = list()
        list_of_ofiles = list()
        list_of_pfiles = list()
        list_of_gfiles = list()
        nof_ifiles = 0
        nof_ofiles = 6
        nof_pfiles = 0
        nof_gfiles = 0
        cp_file=""
        if "project" in prdir:
            mytext = "Use directory1 "+ os.path.abspath(prdir) + "\n"
            self.log_it(mytext)
        else:    
            if(was_in_spbox):   
                prnumber = self.ex2.sp.value()
            else:
                mytext = "No project number set (?)\n"
                self.log_it(mytext)
            prdir = prdir + "/" + project_name + str(prnumber)
            mytext = "Use directory2 "+ os.path.abspath(prdir) + "\n"
            self.log_it(mytext)
    
        mytext = "Save project files\n"
        self.log_it(mytext)
# first the input files (if selected)          
        if(self.ex.bif1.isChecked() != 0 ):   
            if(ifname):
                with open(ifname) as fp:  
                    line = fp.readline()
                    while line:
                        sline=line.strip()
                        if (sline[0:6] == "RDBEAM"):
                            line = fp.readline()
                            cp_file=os.path.dirname(ifname) + os.sep + line.strip()
                            list_of_ifiles.append(cp_file)
                            nof_ifiles += 1
                        if (sline[0:6] == "RFQPTQ"):
                            line = fp.readline()
                            cp_file=os.path.dirname(ifname) + os.sep + line.strip()
                            list_of_ifiles.append(cp_file)
                            nof_ifiles += 1
                            # rfq descriptor file may contain RMS and/or FF files
                            with open(cp_file) as rfqp:  
                                rfqline = rfqp.readline()
                                while rfqline:
                                    rfqnumbers = []
                                    rfqsline=rfqline.strip()
                                    for word in rfqsline.split():
                                       if word.isdigit():
                                          rfqnumbers.append(int(word))
                                    rfqline = rfqp.readline()
                                    # make sure we're not reading an empty line
                                    if rfqnumbers: 
                                        if (rfqnumbers[1] == 6): 
#                                            print("RMS of type 6 found")
                                            cp_file=os.path.dirname(ifname) + os.sep + rfqline.strip()
                                            list_of_ifiles.append(cp_file)
                                            nof_ifiles += 1
                                            rfqline = rfqp.readline()
                                        if (rfqnumbers[1] == 7): 
#                                            print("FF  of type 7 found")
                                            cp_file=os.path.dirname(ifname) + os.sep + rfqline.strip()
                                            list_of_ifiles.append(cp_file)
                                            nof_ifiles += 1
                                            rfqline = rfqp.readline()
                        if (sline[0:5] == "FIELD"):
                            line = fp.readline()
                            cp_file=line.strip()
                            cp_file=os.path.dirname(ifname) + os.sep + line.strip()
                            list_of_ifiles.append(cp_file)
                            nof_ifiles += 1
                        if (sline[0:5] == "FSOLE"):
                            line = fp.readline()
                            cp_file=line.strip()
                            cp_file=os.path.dirname(ifname) + os.sep + line.strip()
                            list_of_ifiles.append(cp_file)
                            nof_ifiles += 1
                        if (sline[0:4] == "EGUN"):
                            line = fp.readline()
                            cp_file=line.strip()
                            cp_file=os.path.dirname(ifname) + os.sep + line.strip()
                            list_of_ifiles.append(cp_file)
                            nof_ifiles += 1
                        if (sline[0:4] == "ETAC"):
                            line = fp.readline()
                            ncstats=int(line.strip())
                            if(ncstats == 0):
                                line = fp.readline()
                                cp_file=line.strip()
                                cp_file=os.path.dirname(ifname) + os.sep + line.strip()
                                list_of_ifiles.append(cp_file)
                                nof_ifiles += 1
                        line = fp.readline()
                    
                # now copy the files         
                fcnt = 0
                if (platform.system() == 'Windows') :
                    docopy = 'copy ' + ifname + " " + os.path.abspath(prdir)  + " > nul"
                    #print("I File ",fcnt," =",docopy)
                    os.system(docopy)
                    while fcnt < nof_ifiles :
                        docopy = 'copy ' + list_of_ifiles[fcnt] + " " + os.path.abspath(prdir) + " > nul" 
                        #print("I File ",fcnt+1," =",docopy)
                        os.system(docopy)
                        fcnt += 1
                else:
                    docopy = 'cp ' + ifname + " " + os.path.abspath(prdir) 
                    os.system(docopy)
                    while fcnt < nof_ifiles :
                        docopy = 'cp ' + list_of_ifiles[fcnt] + " " + os.path.abspath(prdir) 
                        os.system(docopy)
                        fcnt += 1
                mytext = str(fcnt+1) + " input file(s) copied to "+ os.path.abspath(prdir) + "\n"
                self.log_it(mytext)
            else:
                mytext = "No input file selected, therefore no input file(s) copied\n"
                redColor = QColor(255, 0, 0)
                blackColor = QColor(0, 0, 0)
                self.inpLog.setTextColor(redColor)
                self.inpLog.insertPlainText(mytext)
                self.inpLog.setTextColor(blackColor)
                self.inpLog.verticalScrollBar().setValue(self.inpLog.verticalScrollBar().maximum())
                self.inpLog.repaint()
        else:
            mytext = "Skipping input file(s)\n"
            self.log_it(mytext)
            
# now copy the plotit (dynplot) files (if selected)             
        if(self.ex.bopf1.isChecked() != 0 ):   
            if(ifname):
                ifnum = 1
                sfnum = str(ifnum).zfill(3)
                cp_file=os.path.dirname(ifname) + os.sep + "dynplot" + sfnum + "." + gr_file_ext
                while (os.path.exists(cp_file)) :
                    nof_pfiles += 1
                    list_of_pfiles.append(cp_file)
                    ifnum += 1
                    sfnum = str(ifnum).zfill(3)
                    cp_file=os.path.dirname(ifname) + os.sep + "dynplot" + sfnum + "." + gr_file_ext
                # now copy the files         
                fcnt = 0
                if (platform.system() == 'Windows') :
                    while fcnt < nof_pfiles :
                        docopy = 'copy ' + list_of_pfiles[fcnt] + " " + os.path.abspath(prdir) + " > nul" 
                        #print("P File ",fcnt," =",docopy)
                        os.system(docopy)
                        fcnt += 1
                else:
                    while fcnt < nof_pfiles :
                        docopy = 'cp ' + list_of_pfiles[fcnt] + " " + os.path.abspath(prdir) 
                        os.system(docopy)
                        fcnt += 1
                mytext = str(fcnt) + " plotit file(s) copied to "+ os.path.abspath(prdir) + "\n"
                self.log_it(mytext)
        else:
            mytext = "Skipping plotit file(s)\n"
            self.log_it(mytext)
            
# now copy the plot (dyngraf) files (if selected)             
        if(self.ex.bopf2.isChecked() != 0 ):   
            if(ifname):
                # find the existing graf files            
                max_gfiles = 8
                gfnum = 1
                sfnum = str(gfnum).zfill(3)
                cp_file=os.path.dirname(ifname) + os.sep + "dyngraf" + sfnum + "." + gr_file_ext
                while gfnum < max_gfiles+1 :
                    if (os.path.exists(cp_file)) :
                        nof_gfiles += 1
                        list_of_gfiles.append(cp_file)
                    gfnum += 1
                    sfnum = str(gfnum).zfill(3)
                    cp_file=os.path.dirname(ifname) + os.sep + "dyngraf" + sfnum + "." + gr_file_ext
                # now copy the files         
                fcnt = 0
                if (platform.system() == 'Windows') :
                    while fcnt < nof_gfiles :
                        docopy = 'copy ' + list_of_gfiles[fcnt] + " " + os.path.abspath(prdir) + " > nul"
                        #print("G File ",fcnt," =",docopy)
                        os.system(docopy)
                        fcnt += 1
                else:
                    while fcnt < nof_gfiles :
                        docopy = 'cp ' + list_of_gfiles[fcnt] + " " + os.path.abspath(prdir) 
                        os.system(docopy)
                        fcnt += 1
                mytext = str(fcnt) + " plot file(s) copied to "+ os.path.abspath(prdir) + "\n"
                self.log_it(mytext)
        else:
            mytext = "Skipping plot file(s)\n"
            self.log_it(mytext)
            
# now copy the output files (if selected)             
        if(self.ex.bof1.isChecked() != 0 ):   
            if(ifname):
                cp_file=os.path.dirname(ifname) + os.sep + "dynac.short"
                list_of_ofiles.append(cp_file)
                cp_file=os.path.dirname(ifname) + os.sep + "dynac.long"
                list_of_ofiles.append(cp_file)
                cp_file=os.path.dirname(ifname) + os.sep + "dynac.dmp"
                list_of_ofiles.append(cp_file)
                cp_file=os.path.dirname(ifname) + os.sep + "dynac.print"
                list_of_ofiles.append(cp_file)
                cp_file=os.path.dirname(ifname) + os.sep + "emit.plot"
                list_of_ofiles.append(cp_file)
                cp_file=os.path.dirname(ifname) + os.sep + "cavdat.out"
                list_of_ofiles.append(cp_file)
                cp_file=os.path.dirname(ifname) + os.sep + "rfq_coef.data"
                if (os.path.exists(cp_file)):
                    list_of_ofiles.append(cp_file)
                    nof_ofiles += 1
                cp_file=os.path.dirname(ifname) + os.sep + "rfq_list.data"
                if (os.path.exists(cp_file)):
                    list_of_ofiles.append(cp_file)
                    nof_ofiles += 1

                # now copy the files         
                fcnt = 0
                if (platform.system() == 'Windows') :
                    while fcnt < nof_ofiles :
                        docopy = 'copy ' + list_of_ofiles[fcnt] + " " + os.path.abspath(prdir)  + " > nul"
                        #print("O File ",fcnt," =",docopy)
                        os.system(docopy)
                        fcnt += 1
                else:
                    while fcnt < nof_ofiles :
                        docopy = 'cp ' + list_of_ofiles[fcnt] + " " + os.path.abspath(prdir) 
                        os.system(docopy)
                        fcnt += 1
                mytext = str(fcnt) + " output files copied to "+ os.path.abspath(prdir) + "\n"
                self.log_it(mytext)
            else:
                mytext = "No input file selected, therefore no output files copied\n"
                redColor = QColor(255, 0, 0)
                blackColor = QColor(0, 0, 0)
                self.inpLog.setTextColor(redColor)
                self.inpLog.insertPlainText(mytext)
                self.inpLog.setTextColor(blackColor)
                self.inpLog.verticalScrollBar().setValue(self.inpLog.verticalScrollBar().maximum())
                self.inpLog.repaint()
        else:
            mytext = "Skipping output file(s)\n"
            self.log_it(mytext)
            
#       close the spinbox window
        self.ex.close()

######################################################
    def close_check(self):                           #
        '''save project files cancelled'''           #
######################################################
        global prdir, project_name, prnumber, pdname
        mytext = "Project files will not be saved\n"
        self.log_it(mytext)
#       close the checkbox window
        self.ex.close()
        
                            
######################################################
    def set_pfd(self):                               #
        '''select project files directory'''         #
######################################################
        global prdir, project_name, prnumber, pdname
        dname3 = ""
        pdname = ""
        if(prdir == ""):
            prdir = ""
            dialog = QtWidgets.QFileDialog(self)
            dialog.setAcceptMode(QtWidgets.QFileDialog.AcceptMode.AcceptOpen)
            dialog.setDirectory(last_ifpath)
            dialog.setFileMode(QtWidgets.QFileDialog.FileMode.Directory)
            dialog.setOption(QtWidgets.QFileDialog.Option.ShowDirsOnly, True)
            dialog.setViewMode(QtWidgets.QFileDialog.ViewMode.Detail)
            dialog.setWindowTitle("Select where to save project files in automatically generated sub-directories")
            if dialog.exec() :
                dname3 = dialog.selectedFiles()
                pdname = str(dname3[0])
            mytext = "Project files will be saved in sub-directories of " + os.path.abspath(pdname) + "\n"
            self.log_it(mytext)
#       get the project number
            self.ex2 = spindialog(self)
            self.ex2.show()
            self.ex2.activateWindow()
            self.ex2.raise_()
            self.ex2.qbtn.clicked.connect(lambda: self.save_close_spin())
            self.ex2.cbtn.clicked.connect(lambda: self.close_spin())
        else:
            mytext = "Project files directory set to " + os.path.abspath(prdir) + "\n"
            self.log_it(mytext)
            choice = QtWidgets.QMessageBox.question(self, 'Project Directory',"Change the project directory?",QtWidgets.QMessageBox.StandardButton.Yes | QtWidgets.QMessageBox.StandardButton.No)
            if choice == QtWidgets.QMessageBox.StandardButton.Yes:
                choice2 = QtWidgets.QMessageBox.question(self, 'Project Directory',"Create a new project directory?",QtWidgets.QMessageBox.StandardButton.Yes | QtWidgets.QMessageBox.StandardButton.No)
                if choice2 == QtWidgets.QMessageBox.StandardButton.Yes:
                    prdir = ""
                    dialog = QtWidgets.QFileDialog(self)
                    dialog.setAcceptMode(QtWidgets.QFileDialog.AcceptMode.AcceptOpen)
                    dialog.setDirectory(last_ifpath)
                    dialog.setFileMode(QtWidgets.QFileDialog.FileMode.Directory)
                    dialog.setOption(QtWidgets.QFileDialog.Option.ShowDirsOnly, True)
                    dialog.setViewMode(QtWidgets.QFileDialog.ViewMode.Detail)
                    dialog.setWindowTitle("Select where to save project files in automatically generated sub-directories")
                    if dialog.exec() :
                        dname3 = dialog.selectedFiles()
                        pdname = str(dname3[0])
                    mytext = "Project files will be saved in sub-directories of " + os.path.abspath(pdname) + "\n"
                    self.log_it(mytext)
#       get the project number
                    self.ex2 = spindialog(self)
                    self.ex2.show()
                    self.ex2.activateWindow()
                    self.ex2.raise_() 
                    self.ex2.qbtn.clicked.connect(lambda: self.save_close_spin())
                    self.ex2.cbtn.clicked.connect(lambda: self.close_spin())
                else:
                    prdir = ""
                    dialog = QtWidgets.QFileDialog(self)
                    dialog.setAcceptMode(QtWidgets.QFileDialog.AcceptMode.AcceptOpen)
                    dialog.setDirectory(last_ifpath)
                    dialog.setFileMode(QtWidgets.QFileDialog.FileMode.Directory)
                    dialog.setOption(QtWidgets.QFileDialog.Option.ShowDirsOnly, True)
                    dialog.setViewMode(QtWidgets.QFileDialog.ViewMode.Detail)
                    dialog.setWindowTitle("Select the project files directory")
                    if dialog.exec() :
                        dname3 = dialog.selectedFiles()
                        prdir = str(dname3[0])
                    mytext = "Project files will be saved in " + os.path.abspath(prdir) + "\n"
                    self.log_it(mytext)
            else:
                mytext = "Project files directory not changed\n"
                self.log_it(mytext)
            
######################################################
    def save_files(self):                            #
        '''save relevant project file(s)'''          #
######################################################
        global ifname, prdir, project_name
        if(prdir == ""):
            mytext = "No project directory set!\nSet by selecting 'Set project files directory' under 'File'\n"
            redColor = QColor(255, 0, 0)
            blackColor = QColor(0, 0, 0)
            self.inpLog.setTextColor(redColor)
            self.inpLog.insertPlainText(mytext)
            self.inpLog.setTextColor(blackColor)
            self.inpLog.verticalScrollBar().setValue(self.inpLog.verticalScrollBar().maximum())
            self.inpLog.repaint()
        else:    
            docopy=""
#           select what to save
            self.ex = checkdialog(self)
            self.ex.show()            
            self.ex.qbtn_savef.clicked.connect(lambda: self.save_close_check())
            self.ex.cbtn_savef.clicked.connect(lambda: self.close_check())            
            
            
######################################################
    def gen_ellips(self,TWISS):                      #
        '''Generate npoints on ellipse'''            #
######################################################
# Generate points on an ellipse based on Twiss parameters
# input:  TWISS (Array containing Twiss parameters,emittance, NRMS and centroid coordinates
#         NRMS is the ellips size in RMS multiples
# output: XPoints,YPoints are the coordinates of points on the ellips
        npoints=200
        Theta = np.linspace(0,2*pi,npoints)
        XPoints = np.zeros((npoints),float)
        YPoints = np.zeros((npoints),float)
        m11=math.sqrt(math.fabs(TWISS[1]))
        m21=-TWISS[0]/math.sqrt(math.fabs(TWISS[1]))
        m22=1/math.sqrt(math.fabs(TWISS[1]))
        Radius=math.sqrt(math.fabs(TWISS[3]))
        rmsmul = math.sqrt(TWISS[4])
        m12=0.
        PHI = math.atan(2.0*TWISS[0]/(TWISS[2]-TWISS[1]))/2.0
        for i in range(npoints):
            XPoints[i] = TWISS[5] + Radius*(m11*math.cos(Theta[i]) + m12*math.sin(Theta[i]))*rmsmul
            YPoints[i] = TWISS[6] + Radius*(m21*math.cos(Theta[i]) + m22*math.sin(Theta[i]))*rmsmul
        return XPoints,YPoints            

######################################################
    def plot_dst(self):                              #
        '''draw particle distributions'''            #
######################################################
        global noprtcls,iflag,freq,wref,wcog, dfname, xmat, lut, ener, emivals_selected
#        self.RightBtn1.setStyleSheet("color : #ff0000; ") 
#        print("Test: switching to red")       
        grafcols = ['blue', 'red', 'black', 'magenta', 'green', 'gold', 'darkorange', 'gray',
        'sienna', 'darkgreen', 'turquoise', 'cyan', 'purple', 'pink', 'olive', 
        'saddlebrown', 'steelblue', 'indigo', 'crimson', 'yellow']
        dfname=self.text_box1.toPlainText()
#        print("Using data (??) from: ",dfname)
        if(dfname == ""):
            self.get_dst()
#        print("Using data from: ",dfname)
#        print('n_of_KDE_bins in print dist=',n_of_KDE_bins)

        if(dfname):
            try:
#                print("Plotting " + dfname)
                mytime = '{0:%Y-%m-%d %H:%M:%S}'.format(datetime.datetime.now())
                mytitle = "DYNAC " + mytime + "      " + dfname
                # read the first line
                with open(dfname,'r') as myf:
                    first_row = myf.readline()       
                my_args = first_row.split()
                num_args = len(first_row.split()) 
                noprtcls = int(my_args[0])
                flag = float(my_args[1])
                iflag = int(flag)
                freq = float(my_args[2])
                if(self.checkBox3.isChecked() != 0 ):   
                    self.inpLog.clear()            # Clear text in the log frame before running again
                mytext = ""
                if(num_args > 3 ):
                    wref = float(my_args[3])
                    wcog = float(my_args[4])
                    if(num_args == 5 ):
                        mytext = "Distribution file has " + str(num_args) + " arguments:  " + str(noprtcls) + " " + str(iflag) + " " + str(freq) + " " + str(wref) + " " + str(wcog) +  "\n"
                    else:
                        mytext = "Distribution file contains the following arguments:  " + str(noprtcls) + " " + str(iflag) + " " + str(freq) + " " + str(wref) + " " + str(wcog) + "\n"       
                else:
                    mytext = "Distribution file has " + str(num_args) + " arguments:  " + str(noprtcls) + " " + str(iflag) + " " + str(freq) + "\n"
                self.log_it(mytext)
        
                if(iflag in [0, 10, 100, 110]):
                    myDataFrame = pd.read_csv(dfname, skiprows=1, sep=r'\s+', header=None, names=["x", "xp", "y", "yp", "z", "zp"])
                if(iflag in [1, 2, 11, 12, 101, 102, 111, 112]):
                    myDataFrame = pd.read_csv(dfname, skiprows=1, sep=r'\s+', header=None, names=["x", "xp", "y", "yp", "z", "zp", "value"])
                if(iflag in [3, 13, 103, 113]):
                    myDataFrame = pd.read_csv(dfname, skiprows=1, sep=r'\s+', header=None, names=["x", "xp", "y", "yp", "z", "zp", "value", "pn"])
                   
                # change from rads to mrads
                myDataFrame["xp"] = 1000 * myDataFrame["xp"]  
                myDataFrame["yp"] = 1000 * myDataFrame["yp"]
                # change from rads or ns to degs
                if(iflag in [0, 1, 2, 3, 100, 101, 102, 103]):
                    rad2deg = 180/np.pi
                    myDataFrame["z"] = rad2deg * myDataFrame["z"]           
                else:
                    if(freq != 0.):
#                        print("RF Frequency ",freq," MHz")
                        mytext="RF Frequency " + str(freq) + " MHz" + "\n"
                    else:
#                        freq=tkinter.simpledialog.askfloat("Input Data Required", "Enter frequency [MHz]")
                        myfdialog = QtWidgets.QInputDialog()
                        myfdialog.setLocale(QLocale(QLocale.English,QLocale.UnitedStates))
                        myfdialog.setInputMode(QInputDialog.DoubleInput)
                        myfdialog.setLabelText("Enter frequency [MHz]:")
                        myfdialog.setDoubleMaximum(10000)
                        myfdialog.setDoubleDecimals(5)
                        myfdialog.setWindowTitle("Input Data Required")
                        okPressed = myfdialog.exec_()
                        if okPressed:
                            freq = myfdialog.doubleValue()
                        else:
                            return
                        mytext="RF Frequency " + str(freq) + " MHz" + "\n"
                        self.log_it(mytext)                 
                    ns2deg  = 0.36 * freq                  
                    myDataFrame["z"] = ns2deg * myDataFrame["z"] 
                if(iflag in [2, 12, 102, 112]):
                # file with charge states
                    grouped = myDataFrame.groupby("value")
                    qstates = {}
                    qcount = 0
                    for name, group in grouped:
                        qstates[qcount] = name
                        qcount = qcount + 1
                    names = grouped.groups.keys()
                    mytext = "There are " + str(qcount) + " charge states listed in " + dfname + "\n"
                    self.log_it(mytext)
                # set up white background
                pg.setConfigOption('background', 'w')
                pg.setConfigOption('foreground', 'k')
#############################################################
#               SET UP MAIN GRAPHICS WINDOW                 #                
#############################################################                
                self.win = QDialog()
                self.win.setWindowTitle(mytitle)
                self.win.setStyleSheet('background-color: white;')
#               setGeometry(pos left, pos top, width, height)
                self.win.setGeometry(100, 100, 1180, 800)
#                stitle = '<div style="text-align: center"><span style="color: #000000; font-size: 14pt;"><b>Particle distribution plots for ' + str(noprtcls) + ' macro-particles</b></span></div>'
                stitle = "Particle distribution plots for " + str(noprtcls) + " macro-particles"
               
                self.win.horizontalGroupBox = QGroupBox(stitle)
                self.win.horizontalGroupBox.setAlignment(Qt.AlignmentFlag.AlignCenter)
#red font:      self.win.horizontalGroupBox.setStyleSheet('color: #FF2222; font-weight: bold; font-size: 18px')
# "background-color: yellow;"
#                self.win.horizontalGroupBox.setStyleSheet('background-color: white; font-weight: bold; font-size: 22px; border: 20px white;')
                self.win.horizontalGroupBox.setStyleSheet('background-color: white; font-weight: bold; font-size: 22px;')
                layout = QGridLayout()
                layout.setHorizontalSpacing(0) 
                layout.setVerticalSpacing(0) 
                if (platform.system() == 'Darwin') :
                    layout.setContentsMargins(0,10,0,0) 
                else:    
                    layout.setContentsMargins(10,0,20,10)
                tax1=pg.AxisItem(orientation='top',   pen=None, linkView=None, parent=None, maxTickLength=-5, showValues=False)
                rax1=pg.AxisItem(orientation='right', pen=None, linkView=None, parent=None, maxTickLength=-5, showValues=False)                
                tax2=pg.AxisItem(orientation='top',   pen=None, linkView=None, parent=None, maxTickLength=-5, showValues=False)
                rax2=pg.AxisItem(orientation='right', pen=None, linkView=None, parent=None, maxTickLength=-5, showValues=False)                
                tax3=pg.AxisItem(orientation='top',   pen=None, linkView=None, parent=None, maxTickLength=-5, showValues=False)
                rax3=pg.AxisItem(orientation='right', pen=None, linkView=None, parent=None, maxTickLength=-5, showValues=False)
                tax4=pg.AxisItem(orientation='top',   pen=None, linkView=None, parent=None, maxTickLength=-5, showValues=False)
                rax4=pg.AxisItem(orientation='right', pen=None, linkView=None, parent=None, maxTickLength=-5, showValues=False)                
                tax5=pg.AxisItem(orientation='top',   pen=None, linkView=None, parent=None, maxTickLength=-5, showValues=False)
                rax5=pg.AxisItem(orientation='right', pen=None, linkView=None, parent=None, maxTickLength=-5, showValues=False)                
                tax6=pg.AxisItem(orientation='top',   pen=None, linkView=None, parent=None, maxTickLength=-5, showValues=False)
                rax6=pg.AxisItem(orientation='right', pen=None, linkView=None, parent=None, maxTickLength=-5, showValues=False)                
                p1 = pg.PlotWidget(axisItems = {'top': tax1, 'right':rax1})
                p2 = pg.PlotWidget(axisItems = {'top': tax2, 'right':rax2})
                p3 = pg.PlotWidget(axisItems = {'top': tax3, 'right':rax3})
                p3a = pg.PlotWidget()
                p4 = pg.PlotWidget(axisItems = {'top': tax4, 'right':rax4})
                p5 = pg.PlotWidget(axisItems = {'top': tax5, 'right':rax5})
                p6 = pg.PlotWidget(axisItems = {'top': tax6, 'right':rax6})
                #prevent tick label from being cut off on the right side of the bottom axis
                p1.getAxis('right').setWidth(4)
                p2.getAxis('right').setWidth(4)
                p3.getAxis('right').setWidth(4)
                p4.getAxis('right').setWidth(4)
                p5.getAxis('right').setWidth(4)
                p6.getAxis('right').setWidth(4)
                p6a = pg.PlotWidget()
                if(self.radio2.isChecked() != 0 ): 
# Density plots with 6 main graphs with 2 colorbars total on the right                
#                   layout.addWidget(p1, row pos, column pos, row span, column span)
# somehow, column span works the opposite way of what one would expect!
# as last argument in the following calls one would want to give 3 (where it says 1) and vice versa
                    layout.addWidget(p1, 2,0,1,1)
                    layout.addWidget(p2, 2,3,1,1)
                    layout.addWidget(p3, 2,6,1,1)
                    layout.addWidget(p3a,2,9,1,3)
                    layout.addWidget(p4, 3,0,1,1)
                    layout.addWidget(p5, 3,3,1,1)
                    layout.addWidget(p6, 3,6,1,1)
                    layout.addWidget(p6a,3,9,1,3)
                else:
# Scatter plots with 6 main graphs               
                    layout.addWidget(p1, 2,0,1,1)
                    layout.addWidget(p2, 2,1,1,1)
                    layout.addWidget(p3, 2,2,1,1)
                    layout.addWidget(p4, 3,0,1,1)
                    layout.addWidget(p5, 3,1,1,1)
                    layout.addWidget(p6, 3,2,1,1)
                self.win.horizontalGroupBox.setLayout(layout)
                windowLayout = QVBoxLayout()
                windowLayout.addWidget(self.win.horizontalGroupBox)
                self.win.setLayout(windowLayout)
#############################################################                
#############################################################                
                nbins=20
                if(noprtcls > 499):
                    nbins=50
                if(noprtcls > 9999):
                    nbins=100
                if(noprtcls > 99999):
                    nbins=200
                binctrx={}
                binctry={}
                binvalx={}
                binvaly={}
                xmax=np.nanmax(myDataFrame["x"].values)
                xmin=np.nanmin(myDataFrame["x"].values)
                xpmax=np.nanmax(myDataFrame["xp"].values)
                xpmin=np.nanmin(myDataFrame["xp"].values)
                ymax=np.nanmax(myDataFrame["y"].values)
                ymin=np.nanmin(myDataFrame["y"].values)
                ypmax=np.nanmax(myDataFrame["yp"].values)
                ypmin=np.nanmin(myDataFrame["yp"].values)
                zmax=np.nanmax(myDataFrame["z"].values)
                zmin=np.nanmin(myDataFrame["z"].values)
                zpmax=np.nanmax(myDataFrame["zp"].values)
                zpmin=np.nanmin(myDataFrame["zp"].values)
#                xmean = np.nanmean(myDataFrame["x"].values)
                xmean = myDataFrame["x"].mean()
                xpmean = myDataFrame["xp"].mean()
                ymean  = myDataFrame["y"].mean()
                ypmean = myDataFrame["yp"].mean()
                zmean  = myDataFrame["z"].mean()
                zpmean = myDataFrame["zp"].mean()
                ener=zpmean
                txtener = str(ener)
#                print(zmean,"ener=",txtener)      
#                self.text_energy.setText(txtener)
                if(zpmean < 0.001):
                    if(wcog < 0.001):
                        rel_gamma=1.
                        rel_beta=0.
                    else:  
                        rel_gamma = 1. + wcog / (xmat*amu)
                        rel_beta = math.sqrt(1.-1./(rel_gamma*rel_gamma)) 
                else:                   
                    rel_gamma = 1. + zpmean / (xmat*amu)
                    rel_beta = math.sqrt(1.-1./(rel_gamma*rel_gamma)) 

                x2mom  = moment(myDataFrame["x"].values,  moment=2)
                xp2mom = moment(myDataFrame["xp"].values, moment=2)
                y2mom  = moment(myDataFrame["y"].values,  moment=2)
                yp2mom = moment(myDataFrame["yp"].values, moment=2)
                z2mom  = moment(myDataFrame["z"].values,  moment=2)
                zp2mom = moment(myDataFrame["zp"].values, moment=2)

                new12 =  (myDataFrame["x"].values - xmean) * (myDataFrame["xp"].values - xpmean)
                xxpmom = new12.sum() / float(noprtcls)
                new34 =  (myDataFrame["y"].values - ymean) * (myDataFrame["yp"].values - ypmean)
                yypmom = new34.sum() / float(noprtcls)
                new56 =  (myDataFrame["z"].values - zmean) * (myDataFrame["zp"].values - zpmean)
                zzpmom = new56.sum() / float(noprtcls)
                emitx = x2mom * xp2mom - xxpmom * xxpmom
                emity = y2mom * yp2mom - yypmom * yypmom
                emitz = z2mom * zp2mom - zzpmom * zzpmom
                xext = math.sqrt(x2mom)
                xpext= math.sqrt(xp2mom)
                yext = math.sqrt(y2mom)
                ypext= math.sqrt(yp2mom)
                zext = math.sqrt(z2mom)
                zpext= math.sqrt(zp2mom)
                if(xp2mom < 1.0E-20):                
                    xcor = 0.0
                else:    
                    xcor = xxpmom / math.sqrt(x2mom * xp2mom)
                if(yp2mom < 1.0E-20):                    
                    ycor = 0.0
                else:    
                    ycor = yypmom / math.sqrt(y2mom * yp2mom)
                if(zp2mom < 1.0E-20):
                    zcor = 0.0
                else:    
                    zcor = zzpmom / math.sqrt(z2mom * zp2mom)

                if(emitx > 0.):
                    xbet = 10. * xext * xext / math.sqrt(emitx)
                    xgam = 0.1 * xpext * xpext / math.sqrt(emitx)                    
                    xalp = math.sqrt(xbet*xgam-1.)                    
                else:
                    xbet = 0.
                    xgam = 0.
                    xalp = 0.
                if(emity > 0.):
                    ybet = 10. * yext * yext / math.sqrt(emity)
                    ygam = 0.1 * ypext * ypext / math.sqrt(emity)                    
                    yalp = math.sqrt(ybet*ygam-1.)                    
                else:
                    ybet = 0.                  
                    ygam = 0. 
                    yalp = 0.
                if(xcor > 0. ): xalp = -xalp
                if(ycor > 0. ): yalp = -yalp
                                
                if(emitz < 0.000001):
                    zbet = 0.
                    zgam = 0.
                    zalp = 0.
                else:    
                    zbet =  zext *  zext / math.sqrt(emitz)
                    zgam = zpext * zpext / math.sqrt(emitz)
                    zalp = math.sqrt(zbet*zgam-1.)

                if(zcor > 0. ): zalp = -zalp

                cogx  = 0.  
                cogxp = 0.   
                cogy  = 0.   
                cogyp = 0.   
                cogz  = 0.   
                cogzp = 0.   
                if(COG_selected == True): 
                    cogx  = -xmean * 10.   
                    cogxp = -xpmean   
                    cogy  = -ymean * 10.   
                    cogyp = -ypmean   
                    cogz  = -zmean    
                    cogzp = -zpmean                
                if(GRS=="File"):
# check that if user defined limits are to be used, that they are reasonable
                    if(xvals[0]  > xmax+cogx):
                        msg = QMessageBox(self)
                        msg.setWindowTitle("DGUI error in user defined graph limits")
                        msg.setText('User defined Xmin  > Xmax  in data set')
                        msg.setIcon(QMessageBox.Icon.Critical)
                        msg.exec_()                                                                  
                    if(xvals[1]  < xmin+cogx):
                        msg = QMessageBox(self)
                        msg.setWindowTitle("DGUI error in user defined graph limits")
                        msg.setText('User defined Xmax  < Xmin  in data set')
                        msg.setIcon(QMessageBox.Icon.Critical)
                        msg.exec_()                                                                  
                    if(xpvals[0] > xpmax+cogx):
                        msg = QMessageBox(self)
                        msg.setWindowTitle("DGUI error in user defined graph limits")
                        msg.setText('User defined XPmin > XPmax in data set')
                        msg.setIcon(QMessageBox.Icon.Critical)
                        msg.exec_()                                                                  
                    if(xpvals[1] < xpmin+cogxp):
                        msg = QMessageBox(self)
                        msg.setWindowTitle("DGUI error in user defined graph limits")
                        msg.setText('User defined XPmax < XPmin in data set')
                        msg.setIcon(QMessageBox.Icon.Critical)
                        msg.exec_()                                                                  
                    if(yvals[0]  > ymax+cogy):
                        msg = QMessageBox(self)
                        msg.setWindowTitle("DGUI error in user defined graph limits")
                        msg.setText('User defined Ymin  > Ymax  in data set')
                        msg.setIcon(QMessageBox.Icon.Critical)
                        msg.exec_()                                                                  
                    if(yvals[1]  < ymin+cogy):
                        msg = QMessageBox(self)
                        msg.setWindowTitle("DGUI error in user defined graph limits")
                        msg.setText('User defined Ymax  < Ymin  in data set')
                        msg.setIcon(QMessageBox.Icon.Critical)
                        msg.exec_()                                                                  
                    if(ypvals[0] > ypmax+cogyp):
                        msg = QMessageBox(self)
                        msg.setWindowTitle("DGUI error in user defined graph limits")
                        msg.setText('User defined YPmin > YPmax in data set')
                        msg.setIcon(QMessageBox.Icon.Critical)
                        msg.exec_()                                                                  
                    if(ypvals[1] < ypmin+cogyp):
                        msg = QMessageBox(self)
                        msg.setWindowTitle("DGUI error in user defined graph limits")
                        msg.setText('User defined YPmax < YPmin in data set')
                        msg.setIcon(QMessageBox.Icon.Critical)
                        msg.exec_()                                                                  
                    if(zvals[0]  > zmax+cogz):
                        msg = QMessageBox(self)
                        msg.setWindowTitle("DGUI error in user defined graph limits")
                        msg.setText('User defined Zmin  > Zmax  in data set')
                        msg.setIcon(QMessageBox.Icon.Critical)
                        msg.exec_()                                                                  
                    if(zvals[1]  < zmin+cogz):
                        msg = QMessageBox(self)
                        msg.setWindowTitle("DGUI error in user defined graph limits")
                        msg.setText('User defined Zmax  < Zmin  in data set')
                        msg.setIcon(QMessageBox.Icon.Critical)
                        msg.exec_()                                                                  
                    if(zpvals[0] > zpmax+cogzp):
                        msg = QMessageBox(self)
                        msg.setWindowTitle("DGUI error in user defined graph limits")
                        msg.setText('User defined ZPmin > ZPmax in data set')
                        msg.setIcon(QMessageBox.Icon.Critical)
                        msg.exec_()                                                                  
                    if(zpvals[1] < zpmin+cogzp): 
                        msg = QMessageBox(self)
                        msg.setWindowTitle("DGUI error in user defined graph limits")
                        msg.setText('User defined ZPmin > ZPmax in data set')
                        msg.setIcon(QMessageBox.Icon.Critical)
                        msg.exec_()                                                                  
                  
                if(plot_ellipse == True ): 
                    TwissXXP = [xalp, xbet, xgam, 10. * math.sqrt(emitx), NRMS, 10. * xmean, xpmean]
                    ELLX,ELLXP = self.gen_ellips(TwissXXP)
                    ELLX = 0.1 * ELLX
                    TwissYYP = [yalp, ybet, ygam, 10. * math.sqrt(emity), NRMS, 10. * ymean, ypmean]
                    ELLY,ELLYP = self.gen_ellips(TwissYYP)
                    ELLY = 0.1 * ELLY
                    TwissZZP = [zalp, zbet, zgam, math.sqrt(emitz), NRMS, zmean, zpmean]
                    ELLZ,ELLZP = self.gen_ellips(TwissZZP)
                if(zpmax == zpmin):
                    zpmax = 1.001 * zpmax
                    zpmin = 0.999 * zpmin
                # Get the LUT corresponding to the selected colormap
                cm_lut(colormap_name,1)
                rown=0
#                self.win.setWindowTitle('plotWidget title') <- use this to update the window title
                rown=rown+1
                if(iflag in [2, 12, 102, 112]):
                    ## Add charge states list, use HTML style tags to specify color/size
                    qs = 0
                    ntitle="<span style='color: #000000; font-weight: bold; font-size:10pt'>Charge states: </span>"
                    while qs < qcount:
                        newone=""
                        colors=QColor(grafcols[int(qs)])
                        if(self.radio2.isChecked() != 0 ):   
                            newone="<span style='color: " + "#000000" + "; font-weight: bold; font-size:10pt'>" + str(qstates[qs]) + " " + "</span>"
                        else:
                            newone="<span style='color: " + colors.name() + "; font-weight: bold; font-size:10pt'>" + str(qstates[qs]) + " " + "</span>"
                        ntitle = ntitle + newone
                        qs = qs + 1 
                    addLabel = QLabel(ntitle)
                    addLabel.setAlignment(Qt.AlignmentFlag.AlignCenter)
                    layout.addWidget(addLabel, 1,0,1,9)
                    rown=rown+1
                pg.setConfigOptions(antialias=True)
                myttltxt = ''
                myttltxt = '<div style="text-align: center"><span style="color: #000000; font-size: 12pt;"><b>Horizontal Phase Plane</b></span></div>'
                p1.setTitle(myttltxt)
                p1.showAxis('right')
                p1.showAxis('top')
                mylbltxt = ''
                mylbltxt = '<div style="text-align: center"><span style="color: #000000; font-size: 10pt;"><b>'
                mylbltxt = mylbltxt + "X' [mrad]" + '</b></span></div>'
                p1.setLabel('left', mylbltxt)
                mylbltxt = ''
                mylbltxt = '<div style="text-align: center"><span style="color: #000000; font-size: 10pt;"><b>X [cm]</b></span></div>'
#                p1.setStyle(tickTextOffset = 1)
                p1.setLabel('bottom', mylbltxt)
                myttltxt = ''
                myttltxt = '<div style="text-align: center"><span style="color: #000000; font-size: 12pt;"><b>Vertical Phase Plane</b></span></div>'
                p2.setTitle(myttltxt)
                p2.showAxis('right')
                p2.showAxis('top')
                mylbltxt = ''
                mylbltxt = '<div style="text-align: center"><span style="color: #000000; font-size: 10pt;"><b>'
                mylbltxt = mylbltxt + "Y' [mrad]" + '</b></span></div>'
                p2.setLabel('left', mylbltxt)
                mylbltxt = ''
                mylbltxt = '<div style="text-align: center"><span style="color: #000000; font-size: 10pt;"><b>Y [cm]</b></span></div>'
                p2.setLabel('bottom', mylbltxt)
                myttltxt = ''
                myttltxt = '<div style="text-align: center"><span style="color: #000000; font-size: 12pt;"><b>Longitudinal Phase Plane</b></span></div>'                
                p3.setTitle(myttltxt)
                p3.showAxis('right')
                p3.showAxis('top')
                mylbltxt = ''
                if(COG_selected == True): 
                    mylbltxt = '<div style="text-align: center"><span style="color: #000000; font-size: 10pt;"><b>dW [MeV]</b></span></div>'
                    p3.setLabel('left', mylbltxt)
                    mylbltxt = ''
                    mylbltxt = '<div style="text-align: center"><span style="color: #000000; font-size: 11pt;"><b>d&#x03D5; [deg]</b></span></div>'
                    p3.setLabel('bottom', mylbltxt)
                else:
                    mylbltxt = '<div style="text-align: center"><span style="color: #000000; font-size: 10pt;"><b>W [MeV]</b></span></div>'
                    p3.setLabel('left', mylbltxt)
                    mylbltxt = ''
                    mylbltxt = '<div style="text-align: center"><span style="color: #000000; font-size: 11pt;"><b>&#x03D5; [deg]</b></span></div>'
                    p3.setLabel('bottom', mylbltxt)
                                    
                rown=rown+1
                myttltxt = ''
                myttltxt = '<div style="text-align: center"><span style="color: #000000; font-size: 12pt;"><b>Transverse Plane</b></span></div>'
                p4.setTitle(myttltxt)
                p4.showAxis('right')
                p4.showAxis('top')
                mylbltxt = ''
                mylbltxt = '<div style="text-align: center"><span style="color: #000000; font-size: 10pt;"><b>Y [cm]</b></span></div>'
                p4.setLabel('left', mylbltxt)
                mylbltxt = ''
                mylbltxt = '<div style="text-align: center"><span style="color: #000000; font-size: 10pt;"><b>X [cm]</b></span></div>'
                p4.setLabel('bottom', mylbltxt)
                myttltxt = ''
                myttltxt = '<div style="text-align: center"><span style="color: #000000; font-size: 12pt;"><b>Top</b></span></div>'
                p5.setTitle(myttltxt)
                p5.showAxis('right')
                p5.showAxis('top')
                mylbltxt = ''
                mylbltxt = '<div style="text-align: center"><span style="color: #000000; font-size: 10pt;"><b>X [cm]</b></span></div>'
                p5.setLabel('left', mylbltxt)
                mylbltxt = ''
                if(COG_selected == True): 
                    mylbltxt = '<div style="text-align: center"><span style="color: #000000; font-size: 11pt;"><b>d&#x03D5; [deg]</b></span></div>'
                    p5.setLabel('bottom', mylbltxt)
                else:
                    mylbltxt = '<div style="text-align: center"><span style="color: #000000; font-size: 11pt;"><b>&#x03D5; [deg]</b></span></div>'
                    p5.setLabel('bottom', mylbltxt)
                myttltxt = ''
                myttltxt = '<div style="text-align: center"><span style="color: #000000; font-size: 12pt;"><b>Side</b></span></div>'
                p6.setTitle(myttltxt)
                p6.showAxis('right')
                p6.showAxis('top')
                mylbltxt = ''
                mylbltxt = '<div style="text-align: center"><span style="color: #000000; font-size: 10pt;"><b>Y [cm]</b></span></div>'
                p6.setLabel('left', mylbltxt)
                mylbltxt = ''
                pad_size=0.01
                if(COG_selected == True): 
                    mylbltxt = '<div style="text-align: center"><span style="color: #000000; font-size: 11pt;"><b>d&#x03D5; [deg]</b></span></div>'
                    p6.setLabel('bottom', mylbltxt)
                else:
                    mylbltxt = '<div style="text-align: center"><span style="color: #000000; font-size: 11pt;"><b>&#x03D5; [deg]</b></span></div>'
                    p6.setLabel('bottom', mylbltxt)
                if(rangex and GRS=="File"):
                    p1.setRange(xRange=[xvals[0],xvals[1]],padding=pad_size)
                    p4.setRange(xRange=[xvals[0],xvals[1]],padding=pad_size)
                    p5.setRange(yRange=[xvals[0],xvals[1]],padding=pad_size)
                    exlab_hrange=xvals[1]-xvals[0]
                    exlab_hmin=xvals[0]
                else:
                    if(COG_selected == True): 
                        p1.setRange(xRange=[xmin+cogx,xmax+cogx],padding=pad_size)
                        p4.setRange(xRange=[xmin+cogx,xmax+cogx],padding=pad_size)
                        p5.setRange(yRange=[xmin+cogx,xmax+cogx],padding=pad_size)
                        exlab_hmin=xmin+cogx
                    else:
                        p1.setRange(xRange=[xmin,xmax],padding=pad_size)
                        p4.setRange(xRange=[xmin,xmax],padding=pad_size)
                        p5.setRange(yRange=[xmin,xmax],padding=pad_size)
                        exlab_hmin=xmin
                    exlab_hrange=xmax-xmin
                if(rangexp and GRS=="File"):
                    p1.setRange(yRange=[xpvals[0],xpvals[1]],padding=pad_size)
                    exlab_vrange=xpvals[1]-xpvals[0]
                    exlab_vmax=xpvals[1]
                else:
                    if(COG_selected == True): 
                        p1.setRange(yRange=[xpmin+cogxp,xpmax+cogxp],padding=pad_size)
                        exlab_vmax=xpmax+cogxp
                    else:
                        p1.setRange(yRange=[xpmin,xpmax],padding=pad_size)
                        exlab_vmax=xpmax
                    exlab_vrange=xpmax-xpmin
                if(rangey and GRS=="File"):
                    p2.setRange(xRange=[yvals[0],yvals[1]],padding=pad_size)
                    p4.setRange(yRange=[yvals[0],yvals[1]],padding=pad_size)
                    p6.setRange(yRange=[yvals[0],yvals[1]],padding=pad_size)
                    eylab_hrange=yvals[1]-yvals[0]
                    eylab_hmin=yvals[0]
                else:
                    if(COG_selected == True): 
                        p2.setRange(xRange=[ymin+cogy,ymax+cogy],padding=pad_size)
                        p4.setRange(yRange=[ymin+cogy,ymax+cogy],padding=pad_size)
                        p6.setRange(yRange=[ymin+cogy,ymax+cogy],padding=pad_size)
                        eylab_hmin=ymin+cogy
                    else:
                        p2.setRange(xRange=[ymin,ymax],padding=pad_size)
                        p4.setRange(yRange=[ymin,ymax],padding=pad_size)
                        p6.setRange(yRange=[ymin,ymax],padding=pad_size)
                        eylab_hmin=ymin
                    eylab_hrange=ymax-ymin
                if(rangeyp and GRS=="File"):
                    p2.setRange(yRange=[ypvals[0],ypvals[1]],padding=pad_size)
                    eylab_vrange=ypvals[1]-ypvals[0]
                    eylab_vmax=ypvals[1]
                else:
                    if(COG_selected == True): 
                        p2.setRange(yRange=[ypmin+cogyp,ypmax+cogyp],padding=pad_size)
                        eylab_vmax=ypmax+cogyp
                    else:
                        p2.setRange(yRange=[ypmin,ypmax],padding=pad_size)
                        eylab_vmax=ypmax
                    eylab_vrange=ypmax-ypmin
                if(rangez and GRS=="File"):
                    p3.setRange(xRange=[zvals[0],zvals[1]],padding=pad_size)
                    p5.setRange(xRange=[zvals[0],zvals[1]],padding=pad_size)
                    p6.setRange(xRange=[zvals[0],zvals[1]],padding=pad_size)
                    ezlab_hrange=zvals[1]-zvals[0]
                    ezlab_hmin=zvals[0]
                else:
                    if(COG_selected == True): 
                        p3.setRange(xRange=[zmin+cogz,zmax+cogz],padding=pad_size)
                        p5.setRange(xRange=[zmin+cogz,zmax+cogz],padding=pad_size)
                        p6.setRange(xRange=[zmin+cogz,zmax+cogz],padding=pad_size)
                        ezlab_hmin=zmin+cogz
                    else:
                        p3.setRange(xRange=[zmin,zmax],padding=pad_size)
                        p5.setRange(xRange=[zmin,zmax],padding=pad_size)
                        p6.setRange(xRange=[zmin,zmax],padding=pad_size)
                        ezlab_hmin=zmin
                    ezlab_hrange=zmax-zmin
                if(rangezp and GRS=="File"):
                    p3.setRange(yRange=[zpvals[0],zpvals[1]],padding=pad_size)
                    ezlab_vrange=zpvals[1]-zpvals[0]
                    ezlab_vmax=zpvals[1]
                else:
                    if(COG_selected == True): 
                        p3.setRange(yRange=[zpmin+cogzp,zpmax+cogzp],padding=pad_size)
                        ezlab_vmax=zpmax+cogzp
                    else:
                        p3.setRange(yRange=[zpmin,zpmax],padding=pad_size)
                        ezlab_vmax=zpmax
                    ezlab_vrange=zpmax-zpmin
                
                if(iflag in [2, 12, 102, 112]):
                    qs = 0
                    if (self.radio1.isChecked() != 0 ) and (self.checkBox1.isChecked() != 0 ):   
#                   X-XP plot of particles (multiple charge states)
                        cstep=int(255/(qcount+1))
                        while qs < qcount:
                            ByGroup = grouped.get_group(qstates[qs])                
#                            c1=int(255-qs*cstep)
#                            c2=int(15+qs*cstep)
#                            c3=int(10+qs*cstep)
#                            colors = pg.mkBrush(color=(c1, c2, c3))
                            colors=QColor(grafcols[int(qs)])
                            scatterplot = pg.ScatterPlotItem(cogx + ByGroup["x"], cogxp + ByGroup["xp"], symbol='o', size=1.2, pen=pg.mkPen(None), brush = colors)
                            p1.addItem(scatterplot)
                            scatterplot = pg.ScatterPlotItem(cogy + ByGroup["y"], cogyp + ByGroup["yp"], symbol='o', size=1.2, pen=pg.mkPen(None), brush = colors)
                            p2.addItem(scatterplot)
                            scatterplot = pg.ScatterPlotItem(cogz + ByGroup["z"], cogzp + ByGroup["zp"], symbol='o', size=1.2, pen=pg.mkPen(None), brush = colors)
                            p3.addItem(scatterplot)
                            scatterplot = pg.ScatterPlotItem(cogx + ByGroup["x"], cogy + ByGroup["y"],  symbol='o', size=1.2, pen=pg.mkPen(None), brush = colors)
                            p4.addItem(scatterplot)
                            scatterplot = pg.ScatterPlotItem(cogz + ByGroup["z"], cogx + ByGroup["x"],  symbol='o', size=1.2, pen=pg.mkPen(None), brush = colors)
                            p5.addItem(scatterplot)
                            scatterplot = pg.ScatterPlotItem(cogz + ByGroup["z"], cogy + ByGroup["y"],  symbol='o', size=1.2, pen=pg.mkPen(None), brush = colors)
                            p6.addItem(scatterplot)
                            qs = qs + 1 
                        if (emivals_selected == True ):   
                        # add emittance values to the top 3 plots
                            exlabel=pg.TextItem(anchor=(0.5,0.5))
                            eylabel=pg.TextItem(anchor=(0.5,0.5))
                            ezlabel=pg.TextItem(anchor=(0.5,0.5))
                            if(emivals_bottom == True):
                                exlabel.setPos(exlab_hmin+0.5*exlab_hrange,exlab_vmax-0.97*exlab_vrange)
                                eylabel.setPos(eylab_hmin+0.5*eylab_hrange,eylab_vmax-0.97*eylab_vrange)
                                ezlabel.setPos(ezlab_hmin+0.5*ezlab_hrange,ezlab_vmax-0.97*ezlab_vrange)
                            else:
                                exlabel.setPos(exlab_hmin+0.5*exlab_hrange,exlab_vmax-0.03*exlab_vrange)
                                eylabel.setPos(eylab_hmin+0.5*eylab_hrange,eylab_vmax-0.03*eylab_vrange)
                                ezlabel.setPos(ezlab_hmin+0.5*ezlab_hrange,ezlab_vmax-0.03*ezlab_vrange)
                            exlabel.setHtml('<div style="text-align: center"><span style="color: #ff3336;">Ex,n,rms=%1.5f mm.mrad </span></div>'%(10.*rel_beta*rel_gamma*math.sqrt(emitx)))
                            p1.addItem(exlabel)    
                            eylabel.setHtml('<div style="text-align: center"><span style="color: #ff3336;">Ey,n,rms=%1.5f mm.mrad </span></div>'%(10.*rel_beta*rel_gamma*math.sqrt(emity)))
                            p2.addItem(eylabel)    
                            ezlabel.setHtml('<div style="text-align: center"><span style="color: #ff3336;">Ez,rms=%6.3f MeV.deg </span></div>'%(math.sqrt(emitz)))
                            p3.addItem(ezlabel)
                        if(plot_ellipse == True ): 
                            p1.plot(ELLX + cogx, ELLXP + cogxp, pen=pg.mkPen('g', width=2))
                            p2.plot(ELLY + cogy, ELLYP + cogyp, pen=pg.mkPen('g', width=2))
                            p3.plot(ELLZ + cogz, ELLZP + cogzp, pen=pg.mkPen('g', width=2))
                        self.win.show()
                else:
                #single charge state beam
                    if (self.radio1.isChecked() != 0 ) and (self.checkBox1.isChecked() != 0 ):   
                        # scatter plots
                        # top left plot
                        scatterplot = pg.ScatterPlotItem(cogx + myDataFrame["x"], cogxp + myDataFrame["xp"], symbol='o', size=1.2, pen=pg.mkPen(None), brush = 'b')
                        p1.addItem(scatterplot)
                        if(plot_ellipse == True ): 
                            p1.plot(ELLX + cogx, ELLXP + cogxp, pen=pg.mkPen('g', width=2))
                        # top middle plot
                        scatterplot = pg.ScatterPlotItem(cogy + myDataFrame["y"], cogyp + myDataFrame["yp"], symbol='o', size=1.2, pen=pg.mkPen(None), brush = 'b')
                        p2.addItem(scatterplot)
                        if(plot_ellipse == True ): 
                            p2.plot(ELLY + cogy, ELLYP + cogyp, pen=pg.mkPen('g', width=2))
                        # top right plot
                        scatterplot = pg.ScatterPlotItem(cogz + myDataFrame["z"], cogzp + myDataFrame["zp"], symbol='o', size=1.2, pen=pg.mkPen(None), brush = 'b')
                        p3.addItem(scatterplot)
                        if(plot_ellipse == True ): 
                            p3.plot(ELLZ + cogz, ELLZP + cogzp, pen=pg.mkPen('g', width=2))
                            
                        # bottom left plot
                        scatterplot = pg.ScatterPlotItem(cogx + myDataFrame["x"], cogy + myDataFrame["y"],  symbol='o', size=1.2, pen=pg.mkPen(None), brush = 'b')
                        p4.addItem(scatterplot)
                        # bottom middle plot
                        scatterplot = pg.ScatterPlotItem(cogz + myDataFrame["z"], cogx + myDataFrame["x"],  symbol='o', size=1.2, pen=pg.mkPen(None), brush = 'b')
                        p5.addItem(scatterplot)
                        # bottom right plot
                        scatterplot = pg.ScatterPlotItem(cogz + myDataFrame["z"], cogy + myDataFrame["y"],  symbol='o', size=1.2, pen=pg.mkPen(None), brush = 'b')
                        p6.addItem(scatterplot)
                        if (emivals_selected == True ):   
                        # add emittance values to the top 3 plots
                            if(rel_beta == 0):
                                msg = QMessageBox()
                                msg.setIcon(self,QMessageBox.critical)
                                msg.setText('Average energy of particle distribution is quasi 0 keV, normalized emittances will be zero')
                                msg.setWindowTitle("DGUI warning")
                                msg.exec_()                          
                            exlabel=pg.TextItem(anchor=(0.5,0.5))
                            eylabel=pg.TextItem(anchor=(0.5,0.5))
                            ezlabel=pg.TextItem(anchor=(0.5,0.5))
                            if(emivals_bottom == True):
                                exlabel.setPos(exlab_hmin+0.5*exlab_hrange,exlab_vmax-0.97*exlab_vrange)
                                eylabel.setPos(eylab_hmin+0.5*eylab_hrange,eylab_vmax-0.97*eylab_vrange)
                                ezlabel.setPos(ezlab_hmin+0.5*ezlab_hrange,ezlab_vmax-0.97*ezlab_vrange)
                            else:
                                exlabel.setPos(exlab_hmin+0.5*exlab_hrange,exlab_vmax-0.03*exlab_vrange)
                                eylabel.setPos(eylab_hmin+0.5*eylab_hrange,eylab_vmax-0.03*eylab_vrange)
                                ezlabel.setPos(ezlab_hmin+0.5*ezlab_hrange,ezlab_vmax-0.03*ezlab_vrange)
                            exlabel.setHtml('<div style="text-align: center"><span style="color: #ff3336;">Ex,n,rms=%1.5f mm.mrad </span></div>'%(10.*rel_beta*rel_gamma*math.sqrt(emitx)))
                            p1.addItem(exlabel)    
                            eylabel.setHtml('<div style="text-align: center"><span style="color: #ff3336;">Ey,n,rms=%1.5f mm.mrad </span></div>'%(10.*rel_beta*rel_gamma*math.sqrt(emity)))
                            p2.addItem(eylabel)    
                            ezlabel.setHtml('<div style="text-align: center"><span style="color: #ff3336;">Ez,rms=%6.3f MeV.deg </span></div>'%(math.sqrt(emitz)))
                            p3.addItem(ezlabel)
                hticksz1 = 0.
                hticksz2 = 0.
                hticksz3 = 0.
                vticksz1 = 0.
                vticksz2 = 0.
                vticksz3 = 0.
                if(self.radio2.isChecked() != 0 ): 
                    # Density plots
#        # Setup scaling of colorbar (next, implement it as an ImageItem)
                    mytr = QtGui.QTransform()  # prepare ImageItem transformation:
                    mytr.scale(0.01, 0.0122)       # scale horizontal and vertical axes
                    #tr.translate(-1.5, -1.5) # move 3x3 image to locate center at axis origin
#        # Setup colorbar, implemented as an ImageItem
                    bar1=pg.ImageItem()
                    bar1.setTransform(mytr) # assign transform (i.e. scale)
                    bar2=pg.ImageItem()
                    bar2.setTransform(mytr) # assign transform (i.e. scale)
#        # The color bar ImageItem levels run from 0 to 1
                    bar1.setImage(np.linspace(0,1,8192)[None,:])
                    bar2.setImage(np.linspace(0,1,8192)[None,:])
# Add a color bar to the first row, linked to the image. 
                    mylbltxt = ''
                    mylbltxt = '<div style="text-align: center"><span style="color: #000000; font-size: 10pt;"><b>'
                    mylbltxt = mylbltxt + "Intensity [%]" + '</b></span></div>'
                    p3a.setLabel('left', mylbltxt)
                    p3a.hideAxis('bottom')
                    bar1.setLookupTable(lut)
# 2021                bar1.scale(0.01, 0.0122) (replaced by bar1.setTransform(mytr) above)
                    bar1.setPos(0, 0)
                    p3a.addItem(bar1)
# Add a color bar to the second row, linked to the image.                                              
                    mylbltxt = ''
                    mylbltxt = '<div style="text-align: center"><span style="color: #000000; font-size: 10pt;"><b>'
                    mylbltxt = mylbltxt + "Intensity [%]" + '</b></span></div>'
                    p6a.setLabel('left', mylbltxt)                         
                    p6a.hideAxis('bottom')
                    bar2.setLookupTable(lut)
# 2021                bar2.scale(0.01, 0.0122) (replaced by bar2.setTransform(mytr) above) 
                    bar2.setPos(0, 0)
                    p6a.addItem(bar2)
                    if(inter_selected != 0 ):                         
                    # Perform interpolation
                        newbins=4*nbins
                        # top left plot
                        plotDataFrame = myDataFrame 
                        if(GRS=="File"):
                            if(COG_selected == True):
                                plotDataFrame = plotDataFrame[(plotDataFrame["x"]+cogx > xvals[0]) & (plotDataFrame["x"]+cogx < xvals[1]) & (plotDataFrame["xp"]+cogxp > xpvals[0]) & (plotDataFrame["xp"] + cogxp < xpvals[1])]    
                            else: 
                                plotDataFrame = plotDataFrame[(myDataFrame["x"]>xvals[0]) & (myDataFrame["x"]<xvals[1]) & (myDataFrame["xp"]>xpvals[0]) & (myDataFrame["xp"]<xpvals[1])]    
                            xmin=np.nanmin(plotDataFrame["x"].values)
                            xmax=np.nanmax(plotDataFrame["x"].values)
                            xpmin=np.nanmin(plotDataFrame["xp"].values)
                            xpmax=np.nanmax(plotDataFrame["xp"].values)  
                            if(colormap_name == "jet"): 
                                p1.setRange(xRange=[xmin+cogx,xmax+cogx],padding=pad_size)
                                p1.setRange(yRange=[xpmin+cogxp,xpmax+cogxp],padding=pad_size)
                                exlab_hmin=xmin+cogx
                                exlab_hrange=xmax-xmin
                                exlab_vmax=xpmax+cogxp
                                exlab_vrange=xpmax-xpmin
                        try:
# compatible with numpy V21 and older
                            histxxp, bin_edgex, bin_edgexp = np.histogram2d(plotDataFrame["x"], plotDataFrame["xp"], nbins, normed = True)           
                        except:
# compatble with numpy > v21
                            histxxp, bin_edgex, bin_edgexp = np.histogram2d(plotDataFrame["x"], plotDataFrame["xp"], nbins)
                        xcords = [ ]
                        ycords = [ ]
                        for indx in range(1, len(bin_edgex)):
                            xcords.append(0.5*(bin_edgex[indx-1]+bin_edgex[indx]) + cogx)
                        for indx in range(1, len(bin_edgexp)):
                            ycords.append(0.5*(bin_edgexp[indx-1]+bin_edgexp[indx]) + cogxp)
# 2024-01-17 interp2d will be deprecated, replaced with new function
#                        f = interpolate.interp2d(xcords, ycords, histxxp, kind='linear')
                        f = createInterpolation(xcords, ycords, histxxp)
                        hticksz1 = (xpmax-xpmin)*0.01 
                        vticksz1 = (xmax-xmin)*0.01 
                        xstep=(xmax-xmin)/newbins
                        ystep=(xpmax-xpmin)/newbins
                        xnew = np.arange(xmin + cogx, xmax + cogx, xstep)
                        ynew = np.arange(xpmin + cogxp, xpmax + cogxp, ystep)
                        znew = f(xnew, ynew)
                        mytr = QtGui.QTransform()  # prepare ImageItem transformation:
                        mytr.scale(xstep, ystep)       # scale horizontal and vertical axes
                        twodplot = pg.ImageItem()
# 2021                     twodplot.scale(xstep, ystep)
                        twodplot.setTransform(mytr) # assign transform (i.e. scale)
                        twodplot.setPos(xmin + cogx, xpmin + cogxp)
                        # Apply the colormap
                        twodplot.setLookupTable(lut)
                        twodplot.setImage(znew)
                        p1.addItem(twodplot)
                        if(plot_ellipse == True ): 
                            p1.plot(ELLX + cogx, ELLXP + cogxp, pen=pg.mkPen('g', width=2))
                        # top middle plot
                        plotDataFrame = myDataFrame 
                        if(GRS=="File"):
                            if(COG_selected == True):
                                plotDataFrame = plotDataFrame[(plotDataFrame["y"]+cogy > yvals[0]) & (plotDataFrame["y"]+cogy < yvals[1]) & (plotDataFrame["yp"]+cogyp > ypvals[0]) & (plotDataFrame["yp"] + cogyp < ypvals[1])]    
                            else: 
                                plotDataFrame = plotDataFrame[(myDataFrame["y"]>yvals[0]) & (myDataFrame["y"]<yvals[1]) & (myDataFrame["yp"]>ypvals[0]) & (myDataFrame["yp"]<ypvals[1])]    
                            ymin=np.nanmin(plotDataFrame["y"].values)
                            ymax=np.nanmax(plotDataFrame["y"].values)
                            ypmin=np.nanmin(plotDataFrame["yp"].values)
                            ypmax=np.nanmax(plotDataFrame["yp"].values)  
                            if(colormap_name == "jet"): 
                                p2.setRange(xRange=[ymin+cogy,ymax+cogy],padding=pad_size)
                                p2.setRange(yRange=[ypmin+cogyp,ypmax+cogyp],padding=pad_size)
                                eylab_hmin=ymin+cogy
                                eylab_hrange=ymax-ymin
                                eylab_vmax=ypmax+cogyp
                                eylab_vrange=ypmax-ypmin
                        try:
# compatible with numpy V21 and older
                            histyyp, bin_edgey, bin_edgeyp = np.histogram2d(plotDataFrame["y"], plotDataFrame["yp"], nbins, normed = True)
                        except:
# compatble with numpy > v21
                            histyyp, bin_edgey, bin_edgeyp = np.histogram2d(plotDataFrame["y"], plotDataFrame["yp"], nbins)
                        xcords = [ ]
                        ycords = [ ]
                        for indx in range(1, len(bin_edgey)):
                            xcords.append(0.5*(bin_edgey[indx-1]+bin_edgey[indx]) + cogy)
                        for indx in range(1, len(bin_edgeyp)):
                            ycords.append(0.5*(bin_edgeyp[indx-1]+bin_edgeyp[indx]) + cogyp)
# 2024-01-17 interp2d will be deprecated, replaced with new function
#                        f = interpolate.interp2d(xcords, ycords, histyyp, kind='linear')
                        f = createInterpolation(xcords, ycords, histyyp)
                        hticksz2 = (ypmax-ypmin)*0.01 
                        vticksz2 = (ymax-ymin)*0.01 
                        xstep=(ymax-ymin)/newbins
                        ystep=(ypmax-ypmin)/newbins
                        xnew = np.arange(ymin + cogy, ymax + cogy, xstep)
                        ynew = np.arange(ypmin + cogyp, ypmax + cogyp, ystep)
                        znew = f(xnew, ynew)
                        mytr = QtGui.QTransform()  # prepare ImageItem transformation:
                        mytr.scale(xstep, ystep)       # scale horizontal and vertical axes
                        twodplot = pg.ImageItem()
# 2021                        twodplot.scale(xstep, ystep)
                        twodplot.setTransform(mytr) # assign transform (i.e. scale)
                        twodplot.setPos(ymin + cogy, ypmin + cogyp)
                        # Apply the colormap
                        twodplot.setLookupTable(lut)
                        twodplot.setImage(znew)
                        p2.addItem(twodplot)
                        if(plot_ellipse == True ): 
                            p2.plot(ELLY + cogy, ELLYP + cogyp, pen=pg.mkPen('g', width=2))
                        # top right plot
                        plotDataFrame = myDataFrame
                        if(GRS=="File"):
                            if(COG_selected == True):
                                plotDataFrame = plotDataFrame[(plotDataFrame["z"]+cogz > zvals[0]) & (plotDataFrame["z"]+cogz < zvals[1]) & (plotDataFrame["zp"]+cogzp > zpvals[0]) & (plotDataFrame["zp"] + cogzp < zpvals[1])]    
                            else: 
                                plotDataFrame = plotDataFrame[(plotDataFrame["z"]>zvals[0]) & (plotDataFrame["z"]<zvals[1]) & (plotDataFrame["zp"]>zpvals[0]) & (plotDataFrame["zp"]<zpvals[1])]    
                            zmin=np.nanmin(plotDataFrame["z"].values)
                            zmax=np.nanmax(plotDataFrame["z"].values)
                            zpmin=np.nanmin(plotDataFrame["zp"].values)
                            zpmax=np.nanmax(plotDataFrame["zp"].values)
                            if(colormap_name == "jet"): 
                                p3.setRange(xRange=[zmin+cogz,zmax+cogz],padding=pad_size)
                                p3.setRange(yRange=[zpmin+cogzp,zpmax+cogzp],padding=pad_size)  
                                ezlab_hmin=zmin+cogz
                                ezlab_hrange=zmax-zmin
                                ezlab_vmax=zpmax+cogzp
                                ezlab_vrange=zpmax-zpmin
                        try:
# compatible with numpy V21 and older
                            histzzp, bin_edgez, bin_edgezp = np.histogram2d(plotDataFrame["z"], plotDataFrame["zp"], nbins, normed = True)
                        except:
# compatble with numpy > v21
                            histzzp, bin_edgez, bin_edgezp = np.histogram2d(plotDataFrame["z"], plotDataFrame["zp"], nbins)
                        xcords = [ ]
                        ycords = [ ]
                        for indx in range(1, len(bin_edgez)):
                            xcords.append(0.5*(bin_edgez[indx-1]+bin_edgez[indx]) + cogz)
                        for indx in range(1, len(bin_edgezp)):
                            ycords.append(0.5*(bin_edgezp[indx-1]+bin_edgezp[indx]) + cogzp)
# 2024-01-17 interp2d will be deprecated, replaced with new function
#                        f = interpolate.interp2d(xcords, ycords, histzzp, kind='linear')
                        f = createInterpolation(xcords, ycords, histzzp)
                        hticksz3 = (zpmax-zpmin)*0.01 
                        vticksz3 = (zmax-zmin)*0.01
                        xstep = (zmax-zmin)/newbins
                        ystep = (zpmax-zpmin)/newbins
                        xnew = np.arange(zmin + cogz ,  zmax + cogz,  xstep)
                        ynew = np.arange(zpmin + cogzp, zpmax + cogzp, ystep)
                        znew = f(xnew, ynew)
                        mytr = QtGui.QTransform()  # prepare ImageItem transformation:
                        mytr.scale(xstep, ystep)       # scale horizontal and vertical axes
                        twodplot = pg.ImageItem()
# 2021                        twodplot.scale(xstep, ystep)
                        twodplot.setTransform(mytr) # assign transform (i.e. scale)
                        twodplot.setPos(zmin + cogz, zpmin + cogzp)
                        # Apply the colormap
                        twodplot.setLookupTable(lut)
                        twodplot.setImage(znew)
                        p3.addItem(twodplot)
                        if(plot_ellipse == True ): 
                            p3.plot(ELLZ + cogz, ELLZP + cogzp, pen=pg.mkPen('g', width=2))
                        if (emivals_selected == True ):   
                        # add emittance valuess to the top 3 plots
#                            exlabel=pg.TextItem(anchor=(0.5,0.5), border='r', fill=(255,255,255))
                            exlabel=pg.TextItem(anchor=(0.5,0.5))
                            eylabel=pg.TextItem(anchor=(0.5,0.5))
                            ezlabel=pg.TextItem(anchor=(0.5,0.5))
                            if(emivals_bottom == True):
                                exlabel.setPos(exlab_hmin+0.5*exlab_hrange,exlab_vmax-0.97*exlab_vrange)
                                eylabel.setPos(eylab_hmin+0.5*eylab_hrange,eylab_vmax-0.97*eylab_vrange)
                                ezlabel.setPos(ezlab_hmin+0.5*ezlab_hrange,ezlab_vmax-0.97*ezlab_vrange)
                            else:
                                exlabel.setPos(exlab_hmin+0.5*exlab_hrange,exlab_vmax-0.03*exlab_vrange)
                                eylabel.setPos(eylab_hmin+0.5*eylab_hrange,eylab_vmax-0.03*eylab_vrange)
                                ezlabel.setPos(ezlab_hmin+0.5*ezlab_hrange,ezlab_vmax-0.03*ezlab_vrange)
                            exlabel.setHtml('<div style="text-align: center"><span style="color: #ff3336;">Ex,n,rms=%1.5f mm.mrad </span></div>'%(10.*rel_beta*rel_gamma*math.sqrt(emitx)))
                            p1.addItem(exlabel)    
                            eylabel.setHtml('<div style="text-align: center"><span style="color: #ff3336;">Ey,n,rms=%1.5f mm.mrad </span></div>'%(10.*rel_beta*rel_gamma*math.sqrt(emity)))
                            p2.addItem(eylabel)    
                            ezlabel.setHtml('<div style="text-align: center"><span style="color: #ff3336;">Ez,rms=%6.3f MeV.deg </span></div>'%(math.sqrt(emitz)))
                            p3.addItem(ezlabel) 
                                                   
                        # bottom left plot
                        plotDataFrame = myDataFrame
                        if(GRS=="File"):
                            if(COG_selected == True):
                                plotDataFrame = plotDataFrame[(plotDataFrame["x"]+cogx > xvals[0]) & (plotDataFrame["x"]+cogx < xvals[1]) & (plotDataFrame["y"]+cogy > yvals[0]) & (plotDataFrame["y"]+cogy < yvals[1])]    
                            else: 
                                plotDataFrame = plotDataFrame[(plotDataFrame["x"]>xvals[0]) & (plotDataFrame["x"]<xvals[1]) & (plotDataFrame["y"]>yvals[0]) & (plotDataFrame["y"]<yvals[1])]    
                            xmin=np.nanmin(plotDataFrame["x"].values)
                            xmax=np.nanmax(plotDataFrame["x"].values)  
                            ymin=np.nanmin(plotDataFrame["y"].values)
                            ymax=np.nanmax(plotDataFrame["y"].values)  
                            if(colormap_name == "jet"): 
                                p4.setRange(xRange=[xmin+cogx,xmax+cogx],padding=pad_size)
                                p4.setRange(yRange=[ymin+cogy,ymax+cogy],padding=pad_size)
                        try:
# compatible with numpy V21 and older
                            histxy, bin_edgex, bin_edgey = np.histogram2d(plotDataFrame["x"], plotDataFrame["y"], nbins, normed = True)
                        except:
# compatble with numpy > v21
                            histxy, bin_edgex, bin_edgey = np.histogram2d(plotDataFrame["x"], plotDataFrame["y"], nbins)
                        xcords = [ ]
                        ycords = [ ]
                        for indx in range(1, len(bin_edgex)):
                            xcords.append(0.5*(bin_edgex[indx-1]+bin_edgex[indx]) + cogx)
                        for indx in range(1, len(bin_edgey)):
                            ycords.append(0.5*(bin_edgey[indx-1]+bin_edgey[indx]) + cogy)
# 2024-01-17 interp2d will be deprecated, replaced with new function
#                        f = interpolate.interp2d(xcords, ycords, histxy, kind='linear')
                        f = createInterpolation(xcords, ycords, histxy)
                        xstep = (xmax-xmin)/newbins
                        ystep = (ymax-ymin)/newbins
                        xnew = np.arange(xmin + cogx, xmax + cogx, xstep)
                        ynew = np.arange(ymin + cogy, ymax + cogy, ystep)
                        znew = f(xnew, ynew)
                        mytr = QtGui.QTransform()  # prepare ImageItem transformation:
                        mytr.scale(xstep, ystep)       # scale horizontal and vertical axes
                        twodplot = pg.ImageItem()
# 2021                        twodplot.scale(xstep, ystep)
                        twodplot.setTransform(mytr) # assign transform (i.e. scale)
                        twodplot.setPos(xmin + cogx, ymin + cogy)
                        # Apply the colormap
                        twodplot.setLookupTable(lut)
                        twodplot.setImage(znew)
                        p4.addItem(twodplot)
                        # bottom middle plot
                        plotDataFrame = myDataFrame
                        if(GRS=="File"):
                            if(COG_selected == True):
                                plotDataFrame = plotDataFrame[(plotDataFrame["z"]+cogz > zvals[0]) & (plotDataFrame["z"]+cogz < zvals[1]) & (plotDataFrame["x"]+cogx > xvals[0]) & (plotDataFrame["x"]+cogx < xvals[1])]    
                            else: 
                                plotDataFrame = plotDataFrame[(plotDataFrame["z"]>zvals[0]) & (plotDataFrame["z"]<zvals[1]) & (plotDataFrame["x"]>xvals[0]) & (plotDataFrame["x"]<xvals[1])]    
                            zmin=np.nanmin(plotDataFrame["z"].values)
                            zmax=np.nanmax(plotDataFrame["z"].values)
                            ymin=np.nanmin(plotDataFrame["x"].values)
                            ymax=np.nanmax(plotDataFrame["x"].values)  
                            if(colormap_name == "jet"): 
                                p5.setRange(xRange=[zmin+cogz,zmax+cogz],padding=pad_size)
                                p5.setRange(yRange=[xmin+cogx,xmax+cogx],padding=pad_size)
                        try:
# compatible with numpy V21 and older
                            histzx, bin_edgez, bin_edgex = np.histogram2d(plotDataFrame["z"], plotDataFrame["x"], nbins, normed = True)
                        except:
# compatble with numpy > v21
                            histzx, bin_edgez, bin_edgex = np.histogram2d(plotDataFrame["z"], plotDataFrame["x"], nbins)
                        xcords = [ ]
                        ycords = [ ]
                        for indx in range(1, len(bin_edgez)):
                            xcords.append(0.5*(bin_edgez[indx-1]+bin_edgez[indx]) + cogz)
                        for indx in range(1, len(bin_edgex)):
                            ycords.append(0.5*(bin_edgex[indx-1]+bin_edgex[indx]) + cogx)
# 2024-01-17 interp2d will be deprecated, replaced with new function
#                        f = interpolate.interp2d(xcords, ycords, histzx, kind='linear')
                        f = createInterpolation(xcords, ycords, histzx)
                        xstep = (zmax-zmin)/newbins
                        ystep = (xmax-xmin)/newbins
                        xnew = np.arange(zmin + cogz, zmax + cogz, xstep)
                        ynew = np.arange(xmin + cogx, xmax + cogx, ystep)
                        znew = f(xnew, ynew)
                        mytr = QtGui.QTransform()  # prepare ImageItem transformation:
                        mytr.scale(xstep, ystep)       # scale horizontal and vertical axes
                        twodplot = pg.ImageItem()
# 2021                        twodplot.scale(xstep, ystep)
                        twodplot.setTransform(mytr) # assign transform (i.e. scale)
                        twodplot.setPos(zmin + cogz, xmin + cogx)
                        # Apply the colormap
                        twodplot.setLookupTable(lut)
                        twodplot.setImage(znew)
                        p5.addItem(twodplot)
                        # bottom right plot
                        plotDataFrame = myDataFrame
                        if(GRS=="File"):
                            if(COG_selected == True):
                                plotDataFrame = plotDataFrame[(plotDataFrame["z"]+cogz > zvals[0]) & (plotDataFrame["z"]+cogz < zvals[1]) & (plotDataFrame["y"]>yvals[0]) & (plotDataFrame["y"]<yvals[1])]    
                            else: 
                                plotDataFrame = plotDataFrame[(plotDataFrame["z"]>zvals[0]) & (plotDataFrame["z"]<zvals[1]) & (plotDataFrame["y"]>yvals[0]) & (plotDataFrame["y"]<yvals[1])]    
                            zmin=np.nanmin(plotDataFrame["z"].values)
                            zmax=np.nanmax(plotDataFrame["z"].values)
                            ymin=np.nanmin(plotDataFrame["y"].values)
                            ymax=np.nanmax(plotDataFrame["y"].values)  
                            if(colormap_name == "jet"): 
                                p6.setRange(xRange=[zmin+cogz,zmax+cogz],padding=pad_size)
                                p6.setRange(yRange=[ymin+cogy,ymax+cogy],padding=pad_size)
                        try:
# compatible with numpy V21 and older
                            histzy, bin_edgez, bin_edgey = np.histogram2d(plotDataFrame["z"], plotDataFrame["y"], nbins, normed = True)
                        except:
# compatble with numpy > v21
                            histzy, bin_edgez, bin_edgey = np.histogram2d(plotDataFrame["z"], plotDataFrame["y"], nbins)
                        xcords = [ ]
                        ycords = [ ]
                        for indx in range(1, len(bin_edgez)):
                            xcords.append(0.5*(bin_edgez[indx-1]+bin_edgez[indx]) + cogz)
                        for indx in range(1, len(bin_edgey)):
                            ycords.append(0.5*(bin_edgey[indx-1]+bin_edgey[indx]) + cogy)
# 2024-01-17 interp2d will be deprecated, replaced with new function
#                        f = interpolate.interp2d(xcords, ycords, histzy, kind='linear')
                        f = createInterpolation(xcords, ycords, histzy)
                        xstep = (zmax-zmin)/newbins
                        ystep = (ymax-ymin)/newbins
                        xnew = np.arange(zmin + cogz, zmax + cogz, xstep)
                        ynew = np.arange(ymin + cogy, ymax + cogy, ystep)
                        znew = f(xnew, ynew)
                        mytr = QtGui.QTransform()  # prepare ImageItem transformation:
                        mytr.scale(xstep, ystep)       # scale horizontal and vertical axes
                        twodplot = pg.ImageItem()
# 2021                        twodplot.scale(xstep, ystep)
                        twodplot.setTransform(mytr) # assign transform (i.e. scale)
                        twodplot.setPos(zmin + cogz, ymin + cogy)
                        # Apply the colormap
                        twodplot.setLookupTable(lut)
                        twodplot.setImage(znew)
                        p6.addItem(twodplot)
                    if(KDE_selected != 0 ):                         
                    # Perform Kernel Density Estimate (KDE)
                        # top left plot
                        plotDataFrame = myDataFrame   
                        if(GRS=="File"):
                            if(COG_selected == True):
                                plotDataFrame = plotDataFrame[(plotDataFrame["x"]+cogx > xvals[0]) & (plotDataFrame["x"]+cogx < xvals[1]) & (plotDataFrame["xp"]+cogxp > xpvals[0]) & (plotDataFrame["xp"] + cogxp < xpvals[1])]    
                            else: 
                                plotDataFrame = plotDataFrame[(myDataFrame["x"]>xvals[0]) & (myDataFrame["x"]<xvals[1]) & (myDataFrame["xp"]>xpvals[0]) & (myDataFrame["xp"]<xpvals[1])]    
                            xmin=xvals[0]
                            xmax=xvals[1]                
                            xpmin=xpvals[0]
                            xpmax=xpvals[1]
                        if(GRS=="Auto"):
                            if(COG_selected == True):
                                xmin=np.nanmin(plotDataFrame["x"].values) + cogx
                                xmax=np.nanmax(plotDataFrame["x"].values) + cogx
                                xpmin=np.nanmin(plotDataFrame["xp"].values) + cogxp
                                xpmax=np.nanmax(plotDataFrame["xp"].values) + cogxp
                                plotDataFrame = plotDataFrame[(plotDataFrame["x"]+cogx > xmin) & (plotDataFrame["x"]+cogx < xmax) & (plotDataFrame["xp"]+cogxp > xpmin) & (plotDataFrame["xp"] + cogxp < xpmax)]    
                                if(colormap_name == "jet"): 
                                    p1.setRange(xRange=[xmin,xmax],padding=pad_size)
                                    p1.setRange(yRange=[xpmin,xpmax],padding=pad_size)  
                        xx, yy = np.mgrid[xmin:xmax:n_of_KDE_bins*1j, xpmin:xpmax:n_of_KDE_bins*1j]
#                        positions = np.vstack([cogx + xx.ravel(), cogxp + yy.ravel()])
                        positions = np.vstack([xx.ravel(), yy.ravel()])
                        values = np.vstack([cogx + plotDataFrame["x"].values, cogxp + plotDataFrame["xp"].values])
                        kernel = stats.gaussian_kde(values)
                        f = np.reshape(kernel(positions).T, xx.shape)
                        hticksz1 = (xpmax-xpmin)*0.01 
                        vticksz1 = (xmax-xmin)*0.01 
                        xstep=(xmax-xmin)/n_of_KDE_bins
                        ystep=(xpmax-xpmin)/n_of_KDE_bins
                        mytr = QtGui.QTransform()  # prepare ImageItem transformation:
                        mytr.scale(xstep, ystep)       # scale horizontal and vertical axes
                        twodplot = pg.ImageItem()
# 2021                        twodplot.scale(xstep, ystep)
                        twodplot.setTransform(mytr) # assign transform (i.e. scale)
#                        twodplot.setPos(xmin + cogx, xpmin + cogxp)
                        twodplot.setPos(xmin, xpmin)
                        # Apply the colormap
                        twodplot.setLookupTable(lut)
                        twodplot.setImage(f)
                        p1.addItem(twodplot)
                        if(plot_ellipse == True ): 
                            p1.plot(ELLX + cogx, ELLXP + cogxp, pen=pg.mkPen('g', width=2))
                        # top middle plot
                        # Peform the kernel density estimate
                        plotDataFrame = myDataFrame    
                        if(GRS=="File"):
                            if(COG_selected == True):
                                plotDataFrame = plotDataFrame[(plotDataFrame["y"]+cogy > yvals[0]) & (plotDataFrame["y"]+cogy < yvals[1]) & (plotDataFrame["yp"]+cogyp > ypvals[0]) & (plotDataFrame["yp"] + cogyp < ypvals[1])]    
                            else: 
                                plotDataFrame = plotDataFrame[(myDataFrame["y"]>yvals[0]) & (myDataFrame["y"]<yvals[1]) & (myDataFrame["yp"]>ypvals[0]) & (myDataFrame["yp"]<ypvals[1])]    
                            ymin=yvals[0]
                            ymax=yvals[1]                
                            ypmin=ypvals[0]
                            ypmax=ypvals[1]                
                        if(GRS=="Auto"):
                            if(COG_selected == True):
                                ymin=np.nanmin(plotDataFrame["y"].values) + cogy
                                ymax=np.nanmax(plotDataFrame["y"].values) + cogy
                                ypmin=np.nanmin(plotDataFrame["yp"].values) + cogyp
                                ypmax=np.nanmax(plotDataFrame["yp"].values) + cogyp
                                plotDataFrame = plotDataFrame[(plotDataFrame["y"]+cogy > ymin) & (plotDataFrame["y"]+cogy < ymax) & (plotDataFrame["yp"]+cogyp > ypmin) & (plotDataFrame["yp"] + cogyp < ypmax)]    
                                if(colormap_name == "jet"): 
                                    p2.setRange(xRange=[ymin,ymax],padding=pad_size)
                                    p2.setRange(yRange=[ypmin,ypmax],padding=pad_size)  
                        xx, yy = np.mgrid[ymin:ymax:n_of_KDE_bins*1j, ypmin:ypmax:n_of_KDE_bins*1j]
#                        positions = np.vstack([cogy + xx.ravel(), cogyp + yy.ravel()])
                        positions = np.vstack([xx.ravel(), yy.ravel()])
                        values = np.vstack([cogy + plotDataFrame["y"].values, cogyp + plotDataFrame["yp"].values])
                        kernel = stats.gaussian_kde(values)
                        f = np.reshape(kernel(positions).T, xx.shape)
                        hticksz2 = (ypmax-ypmin)*0.01
                        vticksz2 = (ymax-ymin)*0.01
                        xstep=(ymax-ymin)/n_of_KDE_bins
                        ystep=(ypmax-ypmin)/n_of_KDE_bins
                        mytr = QtGui.QTransform()  # prepare ImageItem transformation:
                        mytr.scale(xstep, ystep)       # scale horizontal and vertical axes
                        twodplot = pg.ImageItem()
# 2021                        twodplot.scale(xstep, ystep)
                        twodplot.setTransform(mytr) # assign transform (i.e. scale)
#                        twodplot.setPos(ymin + cogy, ypmin + cogyp)
                        twodplot.setPos(ymin, ypmin)
                        # Apply the colormap
                        twodplot.setLookupTable(lut)
                        twodplot.setImage(f)
                        p2.addItem(twodplot)
                        if(plot_ellipse == True ): 
                            p2.plot(ELLY + cogy, ELLYP + cogyp, pen=pg.mkPen('g', width=2))
                        # top right plot
                        # Peform the kernel density estimate                        
                        plotDataFrame = myDataFrame    
                        if(GRS=="File"):
                            if(COG_selected == True):
                                plotDataFrame = plotDataFrame[(plotDataFrame["z"]+cogz > zvals[0]) & (plotDataFrame["z"]+cogz < zvals[1]) & (plotDataFrame["zp"]+cogzp > zpvals[0]) & (plotDataFrame["zp"] + cogzp < zpvals[1])]    
                            else: 
                                plotDataFrame = plotDataFrame[(plotDataFrame["z"]>zvals[0]) & (plotDataFrame["z"]<zvals[1]) & (plotDataFrame["zp"]>zpvals[0]) & (plotDataFrame["zp"]<zpvals[1])]    
                            zmin=zvals[0]
                            zmax=zvals[1]                
                            zpmin=zpvals[0]
                            zpmax=zpvals[1]                
                        if(GRS=="Auto"):
                            if(COG_selected == True):
                                zmin=np.nanmin(plotDataFrame["z"].values) + cogz
                                zmax=np.nanmax(plotDataFrame["z"].values) + cogz
                                zpmin=np.nanmin(plotDataFrame["zp"].values) + cogzp
                                zpmax=np.nanmax(plotDataFrame["zp"].values) + cogzp
                                plotDataFrame = plotDataFrame[(plotDataFrame["z"]+cogz > zmin) & (plotDataFrame["z"]+cogz < zmax) & (plotDataFrame["zp"]+cogzp > zpmin) & (plotDataFrame["zp"] + cogzp < zpmax)]    
                                if(colormap_name == "jet"): 
                                    p3.setRange(xRange=[zmin,zmax],padding=pad_size)
                                    p3.setRange(yRange=[zpmin,zpmax],padding=pad_size)  
                                
                        xx, yy = np.mgrid[zmin:zmax:n_of_KDE_bins*1j, zpmin:zpmax:n_of_KDE_bins*1j]
#                        positions = np.vstack([cogz + xx.ravel(), cogzp + yy.ravel()])
                        positions = np.vstack([xx.ravel(), yy.ravel()])
                        values = np.vstack([cogz + plotDataFrame["z"].values, cogzp + plotDataFrame["zp"].values])
                        kernel = stats.gaussian_kde(values)
                        f = np.reshape(kernel(positions).T, xx.shape)
                        hticksz3 = (zpmax-zpmin)*0.01
                        vticksz3 = (zmax-zmin)*0.01 
                        xstep=(zmax-zmin)/n_of_KDE_bins
                        ystep=(zpmax-zpmin)/n_of_KDE_bins
                        mytr = QtGui.QTransform()  # prepare ImageItem transformation:
                        mytr.scale(xstep, ystep)       # scale horizontal and vertical axes
                        twodplot = pg.ImageItem()
# 2021 problem below with setPos?                       twodplot.scale(xstep, ystep)
                        twodplot.setTransform(mytr) # assign transform (i.e. scale)
                        twodplot.setPos(zmin + cogz, zpmin + cogzp)
                        twodplot.setPos(zmin, zpmin)
                        # Apply the colormap
                        twodplot.setLookupTable(lut)
                        twodplot.setImage(f)
                        p3.addItem(twodplot)
                        if(plot_ellipse == True ): 
                            p3.plot(ELLZ + cogz, ELLZP + cogzp, pen=pg.mkPen('g', width=2))
                        if (emivals_selected == True ):   
                        # add emittance values to the top 3 plots
                            exlabel=pg.TextItem(anchor=(0.5,0.5))
                            eylabel=pg.TextItem(anchor=(0.5,0.5))
                            ezlabel=pg.TextItem(anchor=(0.5,0.5))
                            if(emivals_bottom == True):
                                exlabel.setPos(exlab_hmin+0.5*exlab_hrange,exlab_vmax-0.97*exlab_vrange)
                                eylabel.setPos(eylab_hmin+0.5*eylab_hrange,eylab_vmax-0.97*eylab_vrange)
                                ezlabel.setPos(ezlab_hmin+0.5*ezlab_hrange,ezlab_vmax-0.97*ezlab_vrange)
                            else:
                                exlabel.setPos(exlab_hmin+0.5*exlab_hrange,exlab_vmax-0.03*exlab_vrange)
                                eylabel.setPos(eylab_hmin+0.5*eylab_hrange,eylab_vmax-0.03*eylab_vrange)
                                ezlabel.setPos(ezlab_hmin+0.5*ezlab_hrange,ezlab_vmax-0.03*ezlab_vrange)
                            exlabel.setHtml('<div style="text-align: center"><span style="color: #ff3336;">Ex,n,rms=%1.5f mm.mrad </span></div>'%(10.*rel_beta*rel_gamma*math.sqrt(emitx)))
                            p1.addItem(exlabel)    
                            eylabel.setHtml('<div style="text-align: center"><span style="color: #ff3336;">Ey,n,rms=%1.5f mm.mrad </span></div>'%(10.*rel_beta*rel_gamma*math.sqrt(emity)))
                            p2.addItem(eylabel)    
                            ezlabel.setHtml('<div style="text-align: center"><span style="color: #ff3336;">Ez,rms=%6.3f MeV.deg </span></div>'%(math.sqrt(emitz)))
                            p3.addItem(ezlabel)                        
                            
                        # bottom left plot
                        # Peform the kernel density estimate                        
#                            if(COG_selected == True):
#                                plotDataFrame = plotDataFrame[(plotDataFrame["x"]+cogx > xvals[0]) & (plotDataFrame["x"]+cogx < xvals[1]) & (plotDataFrame["y"]+cogy > yvals[0]) & (plotDataFrame["y"]+cogy < yvals[1])]    
#                            else: 
#                                plotDataFrame = plotDataFrame[(plotDataFrame["x"]>xvals[0]) & (plotDataFrame["x"]<xvals[1]) & (plotDataFrame["y"]>yvals[0]) & (plotDataFrame["y"]<yvals[1])]    
                        xx, yy = np.mgrid[xmin:xmax:n_of_KDE_bins*1j, ymin:ymax:n_of_KDE_bins*1j]
#                        positions = np.vstack([cogx + xx.ravel(), cogy + yy.ravel()])
                        positions = np.vstack([xx.ravel(), yy.ravel()])
                        values = np.vstack([cogx + plotDataFrame["x"].values, cogy + plotDataFrame["y"].values])
                        kernel = stats.gaussian_kde(values)
                        f = np.reshape(kernel(positions).T, xx.shape)
                        xstep=(xmax-xmin)/n_of_KDE_bins
                        ystep=(ymax-ymin)/n_of_KDE_bins
                        mytr = QtGui.QTransform()  # prepare ImageItem transformation:
                        mytr.scale(xstep, ystep)       # scale horizontal and vertical axes
                        twodplot = pg.ImageItem()
# 2021                        twodplot.scale(xstep, ystep)
                        twodplot.setTransform(mytr) # assign transform (i.e. scale)
#                        twodplot.setPos(xmin + cogx, ymin + cogy)
                        twodplot.setPos(xmin, ymin)
                        # Apply the colormap
                        twodplot.setLookupTable(lut)
                        twodplot.setImage(f)
                        p4.addItem(twodplot)
                        # bottom middle plot
                        # Peform the kernel density estimate
                        xx, yy = np.mgrid[zmin:zmax:n_of_KDE_bins*1j, xmin:xmax:n_of_KDE_bins*1j]
#                        positions = np.vstack([cogz + xx.ravel(), cogx + yy.ravel()])
                        positions = np.vstack([xx.ravel(), yy.ravel()])
                        values = np.vstack([cogz + plotDataFrame["z"].values, cogx + plotDataFrame["x"].values])
                        kernel = stats.gaussian_kde(values)
                        f = np.reshape(kernel(positions).T, xx.shape)
                        xstep=(zmax-zmin)/n_of_KDE_bins
                        ystep=(xmax-xmin)/n_of_KDE_bins
                        mytr = QtGui.QTransform()  # prepare ImageItem transformation:
                        mytr.scale(xstep, ystep)       # scale horizontal and vertical axes
                        twodplot = pg.ImageItem()
# 2021                        twodplot.scale(xstep, ystep)
                        twodplot.setTransform(mytr) # assign transform (i.e. scale)
#                        twodplot.setPos(zmin + cogz, xmin + cogx)
                        twodplot.setPos(zmin, xmin)
                        # Apply the colormap
                        twodplot.setLookupTable(lut)
                        twodplot.setImage(f)
                        p5.addItem(twodplot)
                        # bottom right plot
                        # Peform the kernel density estimate                        
                        xx, yy = np.mgrid[zmin:zmax:n_of_KDE_bins*1j, ymin:ymax:n_of_KDE_bins*1j]
#                        positions = np.vstack([cogz + xx.ravel(), cogy + yy.ravel()])
                        positions = np.vstack([xx.ravel(), yy.ravel()])
                        values = np.vstack([cogz + plotDataFrame["z"].values, cogy + plotDataFrame["y"].values])
                        kernel = stats.gaussian_kde(values)
                        f = np.reshape(kernel(positions).T, xx.shape)
                        xstep=(zmax-zmin)/n_of_KDE_bins
                        ystep=(ymax-ymin)/n_of_KDE_bins
                        mytr = QtGui.QTransform()  # prepare ImageItem transformation:
                        mytr.scale(xstep, ystep)       # scale horizontal and vertical axes
                        twodplot = pg.ImageItem()
# 2021                        twodplot.scale(xstep, ystep)
                        twodplot.setTransform(mytr) # assign transform (i.e. scale)
#                        twodplot.setPos(zmin + cogz, ymin + cogy)
                        twodplot.setPos(zmin, ymin)
                        # Apply the colormap
                        twodplot.setLookupTable(lut)
                        twodplot.setImage(f)
                        p6.addItem(twodplot)
                if(self.checkBox2.isChecked() != 0 ):
#               Plot of beam profiles
                    ##############################################################################
                    if(colormap_name != 'jet'):
                        hticksz1 = 0.
                        vticksz1 = 0. 
                        hticksz2 = 0.
                        vticksz2 = 0. 
                        hticksz3 = 0.
                        vticksz3 = 0.                        
#                   X-XP profiles
                    # reset arrays first (avoid using old, out of bound parts)
                    xranges = p1.viewRange()
                    try:
                        del profxraw
                        del profyraw
                        del profx
                        del profy
                        del binctrx
                        del binctry
                        del binvalx
                        del binvaly
                        binctrx={}
                        binctry={}
                        binvalx={}
                        binvaly={}
                    except:
                    # do nothing
                        i = 0
                    histx, bin_edgex  = np.histogram(myDataFrame["x"],  density=False, bins=nbins)
                    histy, bin_edgey  = np.histogram(myDataFrame["xp"], density=False, bins=nbins)
                    binmax=float(histx[0])
                    binmay=float(histy[0])
                    for i in range(0,nbins):
                        binctrx[i] = 0.5*(bin_edgex[i] + bin_edgex[i+1])
                        binctry[i] = 0.5*(bin_edgey[i] + bin_edgey[i+1])
                        binvalx[i] = float(histx[i])
                        binvaly[i] = float(histy[i])
                        if(binvalx[i] > binmax):
                            binmax = binvalx[i]
                        if(binvaly[i] > binmay):
                            binmay = binvaly[i]
                    for i in range(0,nbins):
#                        binvaly[i] = (binvaly[i]/binmay)*(xmax-xmin)/fit_amp
#                        binvalx[i] = (binvalx[i]/binmax)*(xpmax-xpmin)/fit_amp
                        binvaly[i] = (binvaly[i]/binmay)*(xranges[0][1]-xranges[0][0])/fit_amp
                        binvalx[i] = (binvalx[i]/binmax)*(xranges[1][1]-xranges[1][0])/fit_amp
                    profxraw = pd.Series(binctrx)
                    profyraw = pd.Series(binvalx) 
                    profx = pd.Series(binctrx)
                    profy = pd.Series(binvalx) 
                    # make estimate of where center of Gaussian is; need to correct for offset in y
                    if(pro_fit != 0 ):   
                        slop = (profy.values[len(profy.values)-1]-profy.values[0])/(profx.values[len(profx.values)-1]-profx.values[0])
                        intcpt=profy.values[0]
                        yac=profy.values-(profx.values*slop+intcpt)
                        xbar = np.sum(profx.values*yac)/np.sum(yac)
                        width = np.sqrt(np.abs(np.sum(yac*(profx.values-xbar)**2)/np.sum(yac)))
                        ampl = 0.5*(max(yac)-min(yac))
                        loff=0.
                        mod = Model(gaussian) 
                        pars = mod.make_params(amp=ampl, cen=xbar, wid=width, lof=loff)
                        result = mod.fit(profy.values, pars, x=profx.values)
#                        print(result.fit_report())
                        flof = result.best_values['lof']
                        famp = result.best_values['amp']
                        fcen = result.best_values['cen']
                        fwid = result.best_values['wid']
                        if(rangex and GRS=="File"):                    
                            step = bin_edgex[1]-bin_edgex[0]
                            nfbins = int((xvals[1]-xvals[0])/step)
                            step = (xmax-xmin)/nfbins
                            for i in range(0,nfbins):
                                binctrx[i] = xmin + step/2 + i * step
                                binvalx[i] = flof + famp * exp(-(binctrx[i]-fcen)**2 / (2*fwid**2))
                            profx = pd.Series(binctrx)
                            profy = pd.Series(binvalx)
                            p1.plot(cogx + profx.values, hticksz1 + xranges[1][0] + profy.values, pen='g')
                        else:    
                            p1.plot(cogx + profxraw.values, hticksz1 + xranges[1][0] + result.best_fit, pen='g')
                    if(pro_raw != 0 ):
                        if(self.radio1.isChecked() != 0) :
                        # scatter plots were chosen
                            p1.plot(cogx + profxraw.values, hticksz1 + xranges[1][0] + profyraw.values, pen='r')
                        else:
                            if(colormap_name=="jet"):
                                aquamarine = pg.mkPen(color=(127, 255, 212))
                                yellow = pg.mkPen(color=(255, 255, 0))
                                p1.plot(cogx + profxraw.values, hticksz1 + xranges[1][0] + profyraw.values, pen=yellow)
                            else:
                                p1.plot(cogx + profxraw.values, hticksz1 + xranges[1][0] + profyraw.values, pen='r')
                    del profxraw
                    del profyraw
                    del profx
                    del profy
                    profxraw = pd.Series(binctry)
                    profyraw = pd.Series(binvaly)
                    profx = pd.Series(binctry)
                    profy = pd.Series(binvaly)
                    # make estimate of where center of Gaussian is; need to correct for offset in y
                    if(pro_fit != 0 ):
                        slop = (profy.values[len(profy.values)-1]-profy.values[0])/(profx.values[len(profx.values)-1]-profx.values[0])
                        intcpt=profy.values[0]
                        yac=profy.values-(profx.values*slop+intcpt)
                        xbar = np.sum(profx.values*yac)/np.sum(yac)
                        width = np.sqrt(np.abs(np.sum(yac*(profx.values-xbar)**2)/np.sum(yac)))
                        ampl = 0.5*(max(yac)-min(yac))
                        loff = 0.
                        mod = Model(gaussian) 
                        pars = mod.make_params(amp=ampl, cen=xbar, wid=width, lof=loff)
                        result = mod.fit(profy.values, pars, x=profx.values)
#                        print(result.fit_report())
                        flof = result.best_values['lof']
                        famp = result.best_values['amp']
                        fcen = result.best_values['cen']
                        fwid = result.best_values['wid']
                        flof = result.best_values['lof']
                        if(rangexp and GRS=="File"):                    
                            del profx
                            del profy
                            del binctrx
                            del binvalx
                            binctrx={}
                            binvalx={}
                            step = bin_edgey[1]-bin_edgey[0]
                            nfbins = int((xpvals[1]-xpvals[0])/step)
                            step = (xpmax-xpmin)/nfbins
                            for i in range(0,nfbins):
                                binctrx[i] = xpmin + step/2 + i * step
                                binvalx[i] = flof + famp * exp(-(binctrx[i]-fcen)**2 / (2*fwid**2))
                            profx = pd.Series(binctrx)
                            profy = pd.Series(binvalx)
                            p1.plot(vticksz1 + xranges[0][0] + profy.values, cogxp + profx.values, pen='g')
                        else:    
                            p1.plot(vticksz1 + xranges[0][0] + result.best_fit, cogxp + profxraw.values, pen='g')
                    if(pro_raw != 0 ):
                        if(self.radio1.isChecked() != 0) :
                        # scatter plots were chosen
                            p1.plot(vticksz1 + xranges[0][0] + profyraw.values, cogxp + profxraw.values, pen='r')
                        else:
                            if(colormap_name=="jet"):
                                aquamarine = pg.mkPen(color=(127, 255, 212)) 
                                yellow = pg.mkPen(color=(255, 255, 0))
                                p1.plot(vticksz1 + xranges[0][0] + profyraw.values, cogxp + profxraw.values, pen=yellow)
                            else:
                                p1.plot(vticksz1 + xranges[0][0] + profyraw.values, cogxp + profxraw.values, pen='r')
                    ############################################################################## 
#                   Y-YP profiles                        
                    yranges = p2.viewRange()
                    del profxraw
                    del profyraw
                    del profx
                    del profy
                    del binctrx
                    del binctry
                    del binvalx
                    del binvaly
                    binctrx={}
                    binctry={}
                    binvalx={}
                    binvaly={}
                    histx, bin_edgex  = np.histogram(myDataFrame["y"],  density=False, bins=nbins)
                    histy, bin_edgey  = np.histogram(myDataFrame["yp"], density=False, bins=nbins)
                    binmax=float(histx[0])
                    binmay=float(histy[0])
                    for i in range(0,nbins):
                        binctrx[i] = 0.5*(bin_edgex[i] + bin_edgex[i+1])
                        binctry[i] = 0.5*(bin_edgey[i] + bin_edgey[i+1])
                        binvalx[i] = float(histx[i])
                        binvaly[i] = float(histy[i])
                        if(binvalx[i] > binmax):
                            binmax = binvalx[i]
                        if(binvaly[i] > binmay):
                            binmay = binvaly[i]
                    for i in range(0,nbins):
#                        binvaly[i] = (binvaly[i]/binmay)*(ymax-ymin)/fit_amp
#                        binvalx[i] = (binvalx[i]/binmax)*(ypmax-ypmin)/fit_amp
                        binvaly[i] = (binvaly[i]/binmay)*(yranges[0][1]-yranges[0][0])/fit_amp
                        binvalx[i] = (binvalx[i]/binmax)*(yranges[1][1]-yranges[1][0])/fit_amp
                    profxraw = pd.Series(binctrx)
                    profyraw = pd.Series(binvalx) 
                    profx = pd.Series(binctrx)
                    profy = pd.Series(binvalx)
                    # make estimate of where center of Gaussian is; need to correct for offset in y
                    if(pro_fit != 0 ):   
                        slop = (profy.values[len(profy.values)-1]-profy.values[0])/(profx.values[len(profx.values)-1]-profx.values[0])
                        intcpt=profy.values[0]
                        yac=profy.values-(profx.values*slop+intcpt)
                        xbar = np.sum(profx.values*yac)/np.sum(yac)
                        width = np.sqrt(np.abs(np.sum(yac*(profx.values-xbar)**2)/np.sum(yac)))
                        ampl = 0.5*(max(yac)-min(yac))
                        loff=0.
                        mod = Model(gaussian) 
                        pars = mod.make_params(amp=ampl, cen=xbar, wid=width, lof=loff)
                        result = mod.fit(profy.values, pars, x=profx.values)
#                        print(result.fit_report())
                        flof = result.best_values['lof']
                        famp = result.best_values['amp']
                        fcen = result.best_values['cen']
                        fwid = result.best_values['wid']
                        if(rangey and GRS=="File"):                    
                            step = bin_edgex[1]-bin_edgex[0]
                            nfbins = int((yvals[1]-yvals[0])/step)
                            step = (ymax-ymin)/nfbins
                            for i in range(0,nfbins):
                                binctrx[i] = ymin + step/2 + i * step
                                binvalx[i] = flof + famp * exp(-(binctrx[i]-fcen)**2 / (2*fwid**2))
                            profx = pd.Series(binctrx)
                            profy = pd.Series(binvalx)
                            p2.plot(cogy + profx.values, hticksz2 + yranges[1][0] + profy.values, pen='g')
                        else:    
                            p2.plot(cogy + profxraw.values, hticksz2 + yranges[1][0] + result.best_fit, pen='g')
                    if(pro_raw != 0 ):
                        if(self.radio1.isChecked() != 0) :
                        # scatter plots were chosen
                            p2.plot(cogy + profxraw.values, hticksz2 + yranges[1][0] + profyraw.values, pen='r')
                        else:
                            if(colormap_name=="jet"):
                                aquamarine = pg.mkPen(color=(127, 255, 212)) 
                                yellow = pg.mkPen(color=(255, 255, 0))
                                p2.plot(cogy + profxraw.values, hticksz2 + yranges[1][0] + profyraw.values, pen=yellow)
                            else:
                                p2.plot(cogy + profxraw.values, hticksz2 + yranges[1][0] + profyraw.values, pen='r')
                    del profxraw
                    del profyraw
                    del profx
                    del profy
                    profxraw = pd.Series(binctry)
                    profyraw = pd.Series(binvaly)
                    profx = pd.Series(binctry)
                    profy = pd.Series(binvaly) 
                    # make estimate of where center of Gaussian is; need to correct for offset in y
                    if(pro_fit != 0 ):   
                        slop = (profy.values[len(profy.values)-1]-profy.values[0])/(profx.values[len(profx.values)-1]-profx.values[0])
                        intcpt=profy.values[0]
                        yac=profy.values-(profx.values*slop+intcpt)
                        xbar = np.sum(profx.values*yac)/np.sum(yac)
                        width = np.sqrt(np.abs(np.sum(yac*(profx.values-xbar)**2)/np.sum(yac)))
                        ampl = 0.5*(max(yac)-min(yac))
                        loff=0.
                        mod = Model(gaussian) 
                        pars = mod.make_params(amp=ampl, cen=xbar, wid=width, lof=loff)
                        result = mod.fit(profy.values, pars, x=profx.values)
#                        print(result.fit_report())
                        flof = result.best_values['lof']
                        famp = result.best_values['amp']
                        fcen = result.best_values['cen']
                        fwid = result.best_values['wid']
                        if(rangeyp and GRS=="File"):                    
                            del profx
                            del profy
                            del binctrx
                            del binvalx
                            binctrx={}
                            binvalx={}
                            step = bin_edgey[1]-bin_edgey[0]
                            nfbins = int((ypvals[1]-ypvals[0])/step)
                            step = (ypmax-ypmin)/nfbins
                            fslope = 0.
                            for i in range(0,nfbins):
                                binctrx[i] = ypmin + step/2 + i * step
                                binvalx[i] = flof + famp * exp(-(binctrx[i]-fcen)**2 / (2*fwid**2))
                            profx = pd.Series(binctrx)
                            profy = pd.Series(binvalx)
                            p2.plot(vticksz2 + yranges[0][0] + profy.values, cogyp + profx.values, pen='g')
                        else:    
                            p2.plot(vticksz2 + yranges[0][0] + result.best_fit, cogyp + profxraw.values, pen='g')
                    if(pro_raw != 0 ): 
                        if(self.radio1.isChecked() != 0) :
                        # scatter plots were chosen
                            p2.plot(vticksz2 + yranges[0][0] + profyraw.values, cogyp + profxraw.values, pen='r')
                        else:
                            if(colormap_name=="jet"):
                                aquamarine = pg.mkPen(color=(127, 255, 212)) 
                                yellow = pg.mkPen(color=(255, 255, 0))
                                p2.plot(vticksz2 + yranges[0][0] + profyraw.values, cogyp + profxraw.values, pen=yellow)
                            else:
                                p2.plot(vticksz2 + yranges[0][0] + profyraw.values, cogyp + profxraw.values, pen='r')
                    ##############################################################################    
#                   Z-ZP profiles                        
                    zranges = p3.viewRange()
                    del profxraw
                    del profyraw
                    del profx
                    del profy
                    del binctrx
                    del binctry
                    del binvalx
                    del binvaly
                    binctrx={}
                    binctry={}
                    binvalx={}
                    binvaly={}
                    histx, bin_edgex  = np.histogram(myDataFrame["z"],  density=False, bins=nbins)
                    histy, bin_edgey  = np.histogram(myDataFrame["zp"], density=False, bins=nbins)
                    binmax=float(histx[0])
                    binmay=float(histy[0])
                    binmix=float(histx[0])
                    binmiy=float(histy[0])
                    for i in range(0,nbins):
                        binctrx[i] = 0.5*(bin_edgex[i] + bin_edgex[i+1])
                        binctry[i] = 0.5*(bin_edgey[i] + bin_edgey[i+1])
                        binvalx[i] = float(histx[i])
                        binvaly[i] = float(histy[i])
                        if(binvalx[i] > binmax):
                            binmax = binvalx[i]
#                            xmaxat = binctrx[i]
                        if(binvaly[i] > binmay):
                            binmay = binvaly[i]
                        if(binvalx[i] < binmix):
                            binmix = binvalx[i]
                        if(binvaly[i] < binmiy):
                            binmiy = binvaly[i]
                    for i in range(0,nbins):
#                        binvaly[i] = (binvaly[i]/binmay)*(zmax-zmin)/fit_amp
#                        binvalx[i] = (binvalx[i]/binmax)*(zpmax-zpmin)/fit_amp
                        binvaly[i] = (binvaly[i]/binmay)*(zranges[0][1]-zranges[0][0])/fit_amp
                        binvalx[i] = (binvalx[i]/binmax)*(zranges[1][1]-zranges[1][0])/fit_amp
                    profxraw = pd.Series(binctrx)
                    profyraw = pd.Series(binvalx)
                    profx = pd.Series(binctrx)
                    profy = pd.Series(binvalx) 
                    # make estimate of where center of Gaussian is; need to correct for offset in y
                    if(pro_fit != 0 ):   
                        slop = (profy.values[len(profy.values)-1]-profy.values[0])/(profx.values[len(profx.values)-1]-profx.values[0])
                        intcpt=profy.values[0]
                        yac=profy.values-(profx.values*slop+intcpt)
                        if(np.sum(yac) != 0.0):                        
                            xbar = np.sum(profx.values*yac)/np.sum(yac)
#                            xbar = xmaxat
                            width = np.sqrt(np.abs(np.sum(yac*(profx.values-xbar)**2)/np.sum(yac)))
                            ampl = 0.5*(max(yac)-min(yac))
                            loff=0.
                            mod = Model(gaussian) 
                            pars = mod.make_params(amp=ampl, cen=xbar, wid=width, lof=loff)
                            result = mod.fit(profy.values, pars, x=profx.values)
#                        print(result.fit_report())
                            flof = result.best_values['lof']
                            famp = result.best_values['amp']
                            fcen = result.best_values['cen']
                            fwid = result.best_values['wid']
                            if(rangez and GRS=="File"):                    
                                step = bin_edgex[1]-bin_edgex[0]
                                nfbins = int((zvals[1]-zvals[0])/step)
                                step = (zmax-zmin)/nfbins
                                for i in range(0,nfbins):
                                    binctrx[i] = zmin + step/2 + i * step
                                    binvalx[i] = flof + famp * exp(-(binctrx[i]-fcen)**2 / (2*fwid**2))
                                profx = pd.Series(binctrx)
                                profy = pd.Series(binvalx)
                                p3.plot(cogz + profx.values, hticksz3 + zranges[1][0] + profy.values, pen='g')
                            else:    
                                p3.plot(cogz + profxraw.values, hticksz3 + zranges[1][0] + result.best_fit, pen='g')
                    if(pro_raw != 0 ): 
                        if(self.radio1.isChecked() != 0) :
                        # scatter plots were chosen
                            p3.plot(cogz + profxraw.values, hticksz3 + zranges[1][0] + profyraw.values, pen='r')
                        else:
                            if(colormap_name=="jet"):
                                aquamarine = pg.mkPen(color=(127, 255, 212)) 
                                yellow = pg.mkPen(color=(255, 255, 0))
                                p3.plot(cogz + profxraw.values, hticksz3 + zranges[1][0] + profyraw.values, pen=yellow)
                            else:
                                p3.plot(cogz + profxraw.values, hticksz3 + zranges[1][0] + profyraw.values, pen='r')
                    del profxraw
                    del profyraw
                    del profx
                    del profy
                    profxraw = pd.Series(binctry)
                    profyraw = pd.Series(binvaly)
                    profx = pd.Series(binctry)
                    profy = pd.Series(binvaly) 
                    # make estimate of where center of Gaussian is; need to correct for offset in y
                    if(pro_fit != 0 ):   
                        slop = (profy.values[len(profy.values)-1]-profy.values[0])/(profx.values[len(profx.values)-1]-profx.values[0])
                        intcpt=profy.values[0]
                        yac=profy.values-(profx.values*slop+intcpt)
                        xbar = np.sum(profx.values*yac)/np.sum(yac)
                        width = np.sqrt(np.abs(np.sum(yac*(profx.values-xbar)**2)/np.sum(yac)))
                        ampl = 0.5*(max(yac)-min(yac))
                        if(width != 0.0):                        
                            loff=0.
                            mod = Model(gaussian) 
                            pars = mod.make_params(amp=ampl, cen=xbar, wid=width, lof=loff)
                            result = mod.fit(profy.values, pars, x=profx.values)
#                        print(result.fit_report())
                            flof = result.best_values['lof']
                            famp = result.best_values['amp']
                            fcen = result.best_values['cen']
                            fwid = result.best_values['wid']
                            if(rangezp and GRS=="File"):                    
                                del profx
                                del profy
                                del binctrx
                                del binvalx
                                binctrx={}
                                binvalx={}
                                step = bin_edgey[1]-bin_edgey[0]
                                nfbins = int((zpvals[1]-zpvals[0])/step)
                                step = (zpmax-zpmin)/nfbins
                                for i in range(0,nfbins):
                                    binctrx[i] = zpmin + step/2 + i * step
                                    if(COG_selected == True and self.radio2.isChecked() != 0 and KDE_selected != 0):
                                        binvalx[i] = flof + famp * exp(-(binctrx[i])**2 / (2*fwid**2))
                                    else:    
                                        binvalx[i] = flof + famp * exp(-(binctrx[i]-fcen)**2 / (2*fwid**2))
                                profx = pd.Series(binctrx)
                                profy = pd.Series(binvalx)
#                                if(COG_selected == True  and KDE_selected != 0):
#                                    p3.plot(vticksz3 + zranges[0][0] + profy.values, profx.values, pen='g')
#                                else:
#                                    p3.plot(vticksz3 + zranges[0][0] + profy.values, cogzp + profx.values, pen='g')
                                if(COG_selected == True  and self.radio2.isChecked() != 0 and KDE_selected != 0):
                                    p3.plot(vticksz3 + zranges[0][0] + profy.values, profx.values, pen='g')
                                else:
                                    p3.plot(vticksz3 + zranges[0][0] + profy.values, cogzp + profx.values, pen='g')
                            else:
                                p3.plot(vticksz3 + zranges[0][0] + result.best_fit, cogzp + profxraw.values, pen='g')
                    if(pro_raw != 0 ): 
                        if(self.radio1.isChecked() != 0) :
                        # scatter plots were chosen
                            p3.plot(vticksz3 + zranges[0][0] + profyraw.values, cogzp + profxraw.values, pen='r')
                        else:
                            if(colormap_name=="jet"):
                                aquamarine = pg.mkPen(color=(127, 255, 212)) 
                                yellow = pg.mkPen(color=(255, 255, 0))
                                p3.plot(vticksz3 + zranges[0][0] + profyraw.values, cogzp + profxraw.values, pen=yellow)
                            else:
                                p3.plot(vticksz3 + zranges[0][0] + profyraw.values, cogzp + profxraw.values, pen='r')
                self.win.show()
            except:
                msg3 = QMessageBox(self)
                msg3.setWindowTitle("Error Message")                                 
                msg3.setText("Failed to read file\n'%s'" % dfname)
                msg3.setIcon(QMessageBox.Icon.Critical)
                msg3.exec()                                                                  
#            print("Test: already done?")                                    
#            self.RightBtn1.setStyleSheet("color : #0000ff; ") 
            return



######################################################
    def plot_erms(self):                             #
        '''plot Erms'''                              #
######################################################
        if (ifpath == ""):
            self.get_inp()
        pfname = ifpath + "dynac.print"
#        print("Using data from: ",pfname)

        if pfname:
            try:
                # print("Plotting " + pfname)
                mytime = '{0:%Y-%m-%d %H:%M:%S}'.format(datetime.datetime.now())
                mytitle = "DYNAC " + mytime + "      " + pfname
                # read the first line
                with open(pfname,'r') as myf:
                    first_row = myf.readline()
                my_args = first_row.split()
                num_args = len(first_row.split())
                #print(num_args ,"arguments: ",my_args[0],my_args[1],my_args[2],my_args[3],my_args[4])
                #print(num_args ,"arguments read ")
                # read the remainder of the data
                myDataFrame = pd.read_csv(pfname, skiprows=1, sep=r'\s+', header=None, names=["ELEMENT", "l(m)", "x(mm)", "y(mm)", "z(deg)", "z(mm)", "Ex,n,RMS(mm.mrd)", "Ey,n,RMS(mm.mrd)", "Ez,RMS(keV.ns)", "Wcog(MeV)", "n_of_particles", "xmin(mm)", "xmax(mm)", "ymin(mm)", "ymax(mm)", "tmin(s)", "tmax(s)", "phmin(deg)", "phmax(deg)", "Wmin(MeV)", "Wmax(MeV)", "Dx(m)", "Dy(m)", "dW(MeV)", "Wref(MeV)", "Tref(s)", "Tcog(s)", "xbar(mm)", "ybar(mm)","ax","bx(mm/mrad)","ay","by(mm/mrad)","az","bz(ns/keV)","ElementName","Ax(mm)","Ay(mm)","Ar(mm)"])
                # set up white background
                pg.setConfigOption('background', 'w')
                pg.setConfigOption('foreground', 'k')
                if(sys.version_info[0] == 3) :
                    if(sys.version_info[1] > 8 ) :
                        self.win1 = pg.GraphicsLayoutWidget(title=mytitle, show=True)
                    else:                                        
#                        self.win1 = pg.GraphicsWindow(title=mytitle)
                        self.win1 = pg.GraphicsLayoutWidget(title=mytitle, show=True)                        
                else:
                    print("You seem to be running Python ",sys.version_info[0],".",sys.version_info[1])                   
                    print("You need to be running Python > 3.4") 

#2021                self.win1 = pg.GraphicsWindow(title=mytitle)
#                self.win1 = pg.GraphicsLayoutWidget(title=mytitle, show=True)

                self.win1.resize(1000,700)
                stitle = "Erms as a function of position"
#            self.win.setWindowTitle('plotWidget title') <- use this to update the window title
                self.win1.addLabel(stitle, row=0, col=0, size='12pt')
                pg.setConfigOptions(antialias=True)
                rax=pg.AxisItem('right', pen=None, linkView=None, parent=None, maxTickLength=-5, showValues=False)                
                tax=pg.AxisItem('top',   pen=None, linkView=None, parent=None, maxTickLength=-5, showValues=False)                
                p1 = self.win1.addPlot(title="Transverse emittances", row=1, col=0, axisItems=({'right': rax,'top': tax}))
                p1.showAxis('right')
                p1.showAxis('top')
#               legend1 = myLegend(offset=(42.,35.), size=(50,50)) pos. is from left top, negative is from right bottom
                legend1 = pg.LegendItem(offset=(62.,35.))
                legend1.setParentItem(p1.graphicsItem())
                legend1.paint = types.MethodType(myLegendPaint, legend1)                
                # Fix the spacing between legend symbol and text.
                legend1.layout.setHorizontalSpacing(20)
#                 p1.addLegend(offset=(10, 10))
                ezmax=math.ceil(np.nanmax(myDataFrame["l(m)"].values))
                p1.setXRange(0.,ezmax,padding=0)
                c1 = p1.plot(myDataFrame["l(m)"], myDataFrame["Ex,n,RMS(mm.mrd)"], pen='r')
                c2 = p1.plot(myDataFrame["l(m)"], myDataFrame["Ey,n,RMS(mm.mrd)"], pen='b')
                legend1.addItem(myLegendSample(c1), 'Ex')
                legend1.addItem(myLegendSample(c2), 'Ey')
                p1.setLabel('left', 'Ex, Ey (mm.mrad)')
                p1.setLabel('bottom', 'z (m)')
                # lower plot
                rax=pg.AxisItem('right', pen=None, linkView=None, parent=None, maxTickLength=-5, showValues=False)                
                tax=pg.AxisItem('top',   pen=None, linkView=None, parent=None, maxTickLength=-5, showValues=False)                
                p2 = self.win1.addPlot(title="Longitudinal emittance", row=2, col=0, axisItems=({'right': rax,'top': tax}))
                p2.showAxis('right')
                p2.showAxis('top')
#               legend2 = myLegend(offset=(42.,35.), size=(50,50)) pos. is from left top, negative is from right bottom
                legend2 = pg.LegendItem(offset=(62.,35.))
                legend2.setParentItem(p2.graphicsItem())
                legend2.paint = types.MethodType(myLegendPaint, legend2)                
                # Fix the spacing between legend symbol and text.
                legend2.layout.setHorizontalSpacing(20)
#                p2.addLegend(offset=(10, 10))
                p2.setXRange(0.,ezmax,padding=0)
                cc1 = p2.plot(myDataFrame["l(m)"], myDataFrame["Ez,RMS(keV.ns)"], pen="#000000")
                legend2.addItem(myLegendSample(cc1), 'Ez')
                p2.setLabel('left', 'Ez (keV.ns)')
                p2.setLabel('bottom', 'z (m)')
            except:
                msg3 = QMessageBox(self)
                msg3.setWindowTitle("Error Message")                                 
                msg3.setText("Failed to read file\n'%s'" % pfname)
                msg3.setIcon(QMessageBox.Icon.Critical)
                msg3.exec()                                                                  
            return

######################################################
    def plot_energy(self):                           #
        '''plot energy and synchronous phase'''      #
######################################################
        if (ifpath == ""):
            self.get_inp()
#    pfname = ifpath + os.sep + "dynac.print"
        pfname   = ifpath + "dynac.print"
        dmpfname = ifpath + "dynac.dmp"
#        print("Using data from 1: ",pfname)
#        print("Using data from 2: ",dmpfname)
        if pfname:
            try:
                # print("Plotting " + pfname)
                mytime = '{0:%Y-%m-%d %H:%M:%S}'.format(datetime.datetime.now())
                mytitle = "DYNAC " + mytime + "      " + pfname
                # read the data, filter out the lines that contain the # character, and convert to numbers
                dmpDataFrame = pd.read_csv(dmpfname, skiprows=3, sep=r'\s+', header=None, comment='#',names=["element", "Z", "trans", "PHIs", "TOF(COG)", "Beta(COG)", "Wcog", "TOF(REF)", "Beta(REF)", "Wref", "Ex,RMS,n", "Ey,RMS,n", "El,RMS", "dWref", "EffVolt","ElementName"])
#                dmpDataFrame=dmpDataFrame[~dmpDataFrame.element.str.contains("#")]
#                if(dmpDataFrame.dtypes.element == 'object'): dmpDataFrame=dmpDataFrame[~dmpDataFrame.element.str.contains("#")]
#                dmpDataFrame=dmpDataFrame.apply(pd.to_numeric, errors='coerce')
                printDataFrame = pd.read_csv(pfname, skiprows=1, sep=r'\s+', header=None, names=["ELEMENT", "l(m)", "x(mm)", "y(mm)", "z(deg)", "z(mm)", "Ex,n,RMS(mm.mrd)", "Ey,n,RMS(mm.mrd)", "Ez,RMS(keV.ns)", "Wcog(MeV)", "n_of_particles", "xmin(mm)", "xmax(mm)", "ymin(mm)", "ymax(mm)", "tmin(s)", "tmax(s)", "phmin(deg)", "phmax(deg)", "Wmin(MeV)", "Wmax(MeV)", "Dx(m)", "Dy(m)", "dW(MeV)", "Wref(MeV)", "Tref(s)", "Tcog(s)", "xbar(mm)", "ybar(mm)","ax","bx(mm/mrad)","ay","by(mm/mrad)","az","bz(ns/keV)","ElementName","Ax(mm)","Ay(mm)","Ar(mm)"])
#                printDataFrame=printDataFrame[~printDataFrame.element.str.contains("#")]
#                printDataFrame=printDataFrame.apply(pd.to_numeric, errors='coerce')
                # read the first line
                with open(pfname,'r') as myf:
                    first_row = myf.readline()
                my_args = first_row.split()
                num_args = len(first_row.split())
                #print(num_args ,"arguments: ",my_args[0],my_args[1],my_args[2],my_args[3],my_args[4])
                #print(num_args ,"arguments read ")
                # set up white background
                pg.setConfigOption('background', 'w')
                pg.setConfigOption('foreground', 'k')
                if(sys.version_info[0] == 3) :
                    if(sys.version_info[1] > 8 ) :
                        self.win2 = pg.GraphicsLayoutWidget(title=mytitle, show=True)
                    else:                                        
#                        self.win2 = pg.GraphicsWindow(title=mytitle)
                        self.win2 = pg.GraphicsLayoutWidget(title=mytitle, show=True)
                else:
                    print("You seem to be running Python ",sys.version_info[0],".",sys.version_info[1])                   
                    print("You need to be running Python > 3.4") 
                
#                self.win2 = pg.GraphicsLayoutWidget(title=mytitle, show=True)
                self.win2.resize(1000,700)
                stitle = "Energy and synchronous phase as a function of position"
#            self.win.setWindowTitle('plotWidget title') <- use this to update the window title
                self.win2.addLabel(stitle, row=0, col=0, size='12pt')
                # set up the 2 grafs                
                pg.setConfigOptions(antialias=True)
                rax=pg.AxisItem('right', pen=None, linkView=None, parent=None, maxTickLength=-5, showValues=False)                
                tax=pg.AxisItem('top',   pen=None, linkView=None, parent=None, maxTickLength=-5, showValues=False)                
                p1 = self.win2.addPlot(title="Energy", row=1, col=0, axisItems=({'right': rax,'top': tax}))
                p1.showAxis('right')
                p1.showAxis('top')
#               legend1 = myLegend(offset=(42.,35.), size=(50,50)) pos. is from left top, negative is from right bottom
                legend1 = pg.LegendItem(offset=(62.,35.))
                legend1.setParentItem(p1.graphicsItem())
                legend1.paint = types.MethodType(myLegendPaint, legend1)                
                # Fix the spacing between legend symbol and text.
                legend1.layout.setHorizontalSpacing(20)
                ezmax=math.ceil(np.nanmax(printDataFrame["l(m)"].values))
                p1.setXRange(0.,ezmax,padding=0)
                c1 = p1.plot(printDataFrame["l(m)"], printDataFrame["Wcog(MeV)"], pen='r')
                c2 = p1.plot(printDataFrame["l(m)"], printDataFrame["Wref(MeV)"], pen='b')
                legend1.addItem(myLegendSample(c1), 'Wcog')
                legend1.addItem(myLegendSample(c2), 'Wref')
                p1.setLabel('left', 'Wtotal (MeV)')
                p1.setLabel('bottom', 'z (m)')
                # lower plot
                rax=pg.AxisItem('right', pen=None, linkView=None, parent=None, maxTickLength=-5, showValues=False)                
                tax=pg.AxisItem('top',   pen=None, linkView=None, parent=None, maxTickLength=-5, showValues=False)                
                p2 = self.win2.addPlot(title="Synchronous Phase", row=2, col=0, axisItems=({'right': rax,'top': tax}))
                p2.showAxis('right')
                p2.showAxis('top')
#                p2.addLegend(offset=(10, 10))
                p2.setXRange(0.,ezmax,padding=0)
                phis="\N{GREEK SMALL LETTER PHI}" + "<SUB> s</SUB>"
#                phis="\N{LATIN SUBSCRIPT SMALL LETTER S}"
#                phis = 'PHIs'
#                p2.plot(dmpDataFrame["Z"], dmpDataFrame["PHIs"], pen="#000000", name=phis)
#  BUG               legend (name = 'phis') doesn't work
#                scatterplot = pg.ScatterPlotItem(dmpDataFrame["Z"], dmpDataFrame["PHIs"], name = 'phis', symbol='o', size=4., pen=pg.mkPen(None), brush = 'b')
                scatterplot = pg.ScatterPlotItem(dmpDataFrame["Z"], dmpDataFrame["PHIs"], symbol='o', size=4., pen=pg.mkPen(None), brush = 'b')
                p2.addItem(scatterplot)
                
                p2.setLabel('left', 'Phase (deg)')
                p2.setLabel('bottom', 'z (m)')
            except:
                msg3 = QMessageBox(self)
                msg3.setWindowTitle("Error Message")                                 
                msg3.setText("Failed to read file\n'%s'" % pfname)
                msg3.setIcon(QMessageBox.Icon.Critical)
                msg3.exec()                                                                  
            return

######################################################
    def plot_losses(self):                           #
        '''plot losses'''                            #
######################################################
        if (ifpath == ""):
            self.get_inp()
        pfname = ifpath + "dynac.print"

        if pfname:
            try:
                ellength=np.zeros(10000)
                mytime = '{0:%Y-%m-%d %H:%M:%S}'.format(datetime.datetime.now())
                mytitle = "DYNAC " + mytime + "      " + pfname
                # read the first line
                with open(pfname,'r') as myf:
                    first_row = myf.readline()
                my_args = first_row.split()
                num_args = len(first_row.split())
                # read the remainder of the data
                myDataFrame = pd.read_csv(pfname, skiprows=1, sep=r'\s+', header=None, names=["ELEMENT", "l(m)", "x(mm)", "y(mm)", "z(deg)", "z(mm)", "Ex,n,RMS(mm.mrd)", "Ey,n,RMS(mm.mrd)", "Ez,RMS(keV.ns)", "Wcog(MeV)", "#particles", "xmin(mm)", "xmax(mm)", "ymin(mm)", "ymax(mm)", "tmin(s)", "tmax(s)", "phmin(deg)", "phmax(deg)", "Wmin(MeV)", "Wmax(MeV)", "Dx(m)", "Dy(m)", "dW(MeV)", "Wref(MeV)", "Tref(s)", "Tcog(s)", "xbar(mm)", "ybar(mm)","ax","bx(mm/mrad)","ay","by(mm/mrad)","az","bz(ns/keV)","ElementName","Ax(mm)","Ay(mm)","Ar(mm)"])
                nelements=-1+myDataFrame.shape[0]
                npatinput = myDataFrame['#particles'].iloc[0]
                # set up white background
                pg.setConfigOption('background', 'w')
                pg.setConfigOption('foreground', 'k')
                if(sys.version_info[0] == 3) :
                    if(sys.version_info[1] > 8 ) :
                        self.win7 = pg.GraphicsLayoutWidget(title=mytitle, show=True)
                    else:                                        
#                        self.win7 = pg.GraphicsWindow(title=mytitle)
                        self.win7 = pg.GraphicsLayoutWidget(title=mytitle, show=True)
                else:
                    print("You seem to be running Python ",sys.version_info[0],".",sys.version_info[1])                   
                    print("You need to be running Python > 3.4") 
                
#                self.win7 = pg.GraphicsLayoutWidget(title=mytitle, show=True)
                self.win7.resize(1000,700)
                stitle = "Beam losses along beam line elements"
                self.win7.addLabel(stitle, row=0, col=0, size='12pt')
                pg.setConfigOptions(antialias=True)
                rax=pg.AxisItem('right', pen=None, linkView=None, parent=None, maxTickLength=-5, showValues=False)                
                tax=pg.AxisItem('top',   pen=None, linkView=None, parent=None, maxTickLength=-5, showValues=False)                
                p1 = self.win7.addPlot(title="Relative beam loss per unit length with respect to the input", row=1, col=0, axisItems=({'right': rax,'top': tax}))
                p1.showAxis('right')
                p1.showAxis('top')
###########################
## calculate the losses  ##
###########################
                indx = 1
                loss_max = 0.
                while indx < nelements+1:
                    if (myDataFrame["ELEMENT"][indx] != "START" ):
                        stepz = myDataFrame['l(m)'][indx] - myDataFrame['l(m)'][indx-1]
                        loss  = myDataFrame['#particles'][indx-1] - myDataFrame['#particles'][indx]
#                       relative loss            
                        rel_loss = 100. * loss / npatinput
                        if (rel_loss > loss_max):
                            loss_max = rel_loss 
                    indx = indx + 1
###########################
                ezmax=math.ceil(np.nanmax(myDataFrame["l(m)"].values))
                p1.setXRange(0.,ezmax,padding=0)
                if(loss_max > 0.):
                    exmax = 1.1 * loss_max
                    elheight = -0.04 * loss_max                                
                    eloffset = exmax
                else:
                    exmax = 0.05
                    elheight = -0.0023                                
                    eloffset = exmax                    
                eoffset = 0. 
                p1.setYRange(0.,exmax,padding=0)
                p1.setLabel('left', 'Relative loss per unit length [%/m]')
                p1.setLabel('bottom', 'z (m)')
           
                mytext = "Loss analysis based on " + pfname + "\n"
                self.log_it(mytext)
                mytext = "Number of beam line elements         =" + str(nelements) + "\n"
                self.log_it(mytext)
                mytext = "Number of particles at the input       =" + str(npatinput) + "\n"
                self.log_it(mytext)
                mytext = "Number of particles at the output     =" + str(myDataFrame['#particles'].iloc[-1]) + "\n"
                self.log_it(mytext)
                mytext = "Maximum loss percentage per meter=" + str(loss_max) + "\n"
                self.log_it(mytext)

                indx=1
                newindx=0
# {'b', 'g', 'r', 'c', 'm', 'y', 'k', 'w'},  
# blue, green, red, cyan, magenta, yellow, black, and white
                while indx < nelements+1:
                    if (myDataFrame["ELEMENT"][indx] != "START" ):
                        #set default colour for the element border to black             
                        elcolor='w'
                        elbcolor='k'
                        newindx = newindx +1 
                        stepz = myDataFrame['l(m)'][indx] - myDataFrame['l(m)'][indx-1]
                        loss  = myDataFrame['#particles'][indx-1] - myDataFrame['#particles'][indx]
#                       relative loss            
                        rel_loss = 100. * loss / npatinput
                        eheight = rel_loss
                        # bar for losses
                        myrect = np.array([  [myDataFrame["l(m)"][indx-1],eoffset],
                            [myDataFrame["l(m)"][indx-1],eoffset+eheight],
                            [myDataFrame["l(m)"][indx],eoffset+eheight],
                            [myDataFrame["l(m)"][indx],eoffset],
                            [myDataFrame["l(m)"][indx-1],eoffset]  ])
                        # box for element
                        elrect = np.array([  [myDataFrame["l(m)"][indx-1],eloffset],
                            [myDataFrame["l(m)"][indx-1],eloffset+elheight],
                            [myDataFrame["l(m)"][indx],eloffset+elheight],
                            [myDataFrame["l(m)"][indx],eloffset],
                            [myDataFrame["l(m)"][indx-1],eloffset]  ])                            

#                       drifts do not get a label, nor a box. Some elements get their label placed
#                       relative to the end of the previous element.
                        if (myDataFrame["ELEMENT"][indx] == "RFQPTQ" ):
                            elcolor='g'
                            if (myDataFrame["ELEMENT"][indx-1] != "RFQPTQ" ):
                                ellabel = pg.TextItem(anchor=(0.,0.8))
                                ellabel.setHtml('<div style="text-align: left"><span style="color: #000000;">RFQ</span></div>')
                                ellabel.setPos(myDataFrame["l(m)"][indx-1], eloffset + 1.7 * elheight)
                                p1.addItem(ellabel)
                        elif (myDataFrame["ELEMENT"][indx] == "CAVSC" ):
                            elcolor='g'
                            ellabel = pg.TextItem(anchor=(0.9,0.8))
                            ellabel.setHtml('<div style="text-align: left"><span style="color: #000000;">C</span></div>')
                            ellabel.setPos(myDataFrame["l(m)"][indx], eloffset + 1.7 * elheight)
                            p1.addItem(ellabel)
                        elif (myDataFrame["ELEMENT"][indx] == "CAVMC" ):
                            elcolor='g'
                            ellabel = pg.TextItem(anchor=(0.9,0.8))
                            ellabel.setHtml('<div style="text-align: left"><span style="color: #000000;">CA</span></div>')
                            ellabel.setPos(myDataFrame["l(m)"][indx], eloffset + 1.7 * elheight)
                            p1.addItem(ellabel)
                        elif (myDataFrame["ELEMENT"][indx] == "CAVNUM" ):
                            elcolor='g'
                            ellabel = pg.TextItem(anchor=(0.0,0.8))
                            ellabel.setHtml('<div style="text-align: left"><span style="color: #000000;">CA</span></div>')
                            ellabel.setPos(myDataFrame["l(m)"][indx-1], eloffset + 1.7 * elheight)
                            p1.addItem(ellabel)
                        elif (myDataFrame["ELEMENT"][indx] == "QUADRUPO" ):
                            elcolor='r'
                            if (myDataFrame["ELEMENT"][indx-1] != "QUADRUPO" ):
                                ellabel = pg.TextItem(anchor=(0.4,0.8))
                                ellabel.setHtml('<div style="text-align: left"><span style="color: #000000;">Q</span></div>')
                                ellabel.setPos(myDataFrame["l(m)"][indx-1], eloffset + 1.7 * elheight)
                                p1.addItem(ellabel)
                        elif (myDataFrame["ELEMENT"][indx] == "QUAFK" ):
                            elcolor='r'
                            if (myDataFrame["ELEMENT"][indx-1] != "QUAFK" ):
                                ellabel = pg.TextItem(anchor=(0.4,0.8))
                                ellabel.setHtml('<div style="text-align: left"><span style="color: #000000;">Q</span></div>')
                                ellabel.setPos(myDataFrame["l(m)"][indx-1], eloffset + 1.7 * elheight)
                                p1.addItem(ellabel)
                        elif (myDataFrame["ELEMENT"][indx] == "BUNCHER" ):
                            elcolor='g'
                            elbcolor='g'
                            ellabel = pg.TextItem(anchor=(0.5,0.8))
                            ellabel.setHtml('<div style="text-align: left"><span style="color: #000000;">B</span></div>')
                            ellabel.setPos(myDataFrame["l(m)"][indx], eloffset + 1.7 * elheight)
                            p1.addItem(ellabel)
                        elif (myDataFrame["ELEMENT"][indx] == "STEER" ):
                            elcolor='b'
                            elbcolor='b'
                            ellabel = pg.TextItem(anchor=(0.5,0.8))
                            ellabel.setHtml('<div style="text-align: left"><span style="color: #000000;">C</span></div>')
                            ellabel.setPos(myDataFrame["l(m)"][indx], eloffset + 1.7 * elheight)
                            p1.addItem(ellabel)
                        elif (myDataFrame["ELEMENT"][indx] == "MHB" ):
                            elcolor='g'
                            elbcolor='g'
                            ellabel = pg.TextItem(anchor=(0.9,0.8))
                            ellabel.setHtml('<div style="text-align: left"><span style="color: #000000;">MHB</span></div>')
                            ellabel.setPos(myDataFrame["l(m)"][indx], eloffset + 1.7 * elheight)
                            p1.addItem(ellabel)
                        elif (myDataFrame["ELEMENT"][indx] == "BMAGNET" ):
                            elcolor='y'
                            elbcolor='k'
                            if (myDataFrame["ELEMENT"][indx-1] != "BMAGNET" ):
                                ellabel = pg.TextItem(anchor=(0.2,0.8))
                                ellabel.setHtml('<div style="text-align: left"><span style="color: #000000;">DI</span></div>')
                                ellabel.setPos(myDataFrame["l(m)"][indx-1], eloffset + 1.7 * elheight)
                                p1.addItem(ellabel)
                        elif (myDataFrame["ELEMENT"][indx] == "QUAELEC" ):
                            elcolor='r'
                            elbcolor='g'
                            if (myDataFrame["ELEMENT"][indx-1] != "QUAELEC" ):
                                ellabel = pg.TextItem(anchor=(0.,0.8))
                                ellabel.setHtml('<div style="text-align: left"><span style="color: #000000;">Q</span></div>')
                                ellabel.setPos(myDataFrame["l(m)"][indx-1], eloffset + 1.7 * elheight)
                                p1.addItem(ellabel)
                        elif (myDataFrame["ELEMENT"][indx] == "EDFLEC" ):
                            elcolor='y'
                            elbcolor='g'
                            if (myDataFrame["ELEMENT"][indx-1] != "EDFLEC" ):
                                ellabel = pg.TextItem(anchor=(0.2,0.8))
                                ellabel.setHtml('<div style="text-align: left"><span style="color: #000000;">DI</span></div>')
                                ellabel.setPos(myDataFrame["l(m)"][indx-1], eloffset + 1.7 * elheight)
                                p1.addItem(ellabel)
                        elif (myDataFrame["ELEMENT"][indx] == "SOLENO" ):
                            elcolor='c'
                            if (myDataFrame["ELEMENT"][indx-1] != "SOLENO" ):
                                ellabel = pg.TextItem(anchor=(0.,0.8))
                                ellabel.setHtml('<div style="text-align: left"><span style="color: #000000;">S</span></div>')
                                ellabel.setPos(myDataFrame["l(m)"][indx-1], eloffset + 1.7 * elheight)
                                p1.addItem(ellabel)
                        elif (myDataFrame["ELEMENT"][indx] == "SEXTUPO" ):
                            elcolor='m'
                            if (myDataFrame["ELEMENT"][indx-1] != "SEXTUPO" ):
                                ellabel = pg.TextItem(anchor=(0.2,0.8))
                                ellabel.setHtml('<div style="text-align: left"><span style="color: #000000;">SX</span></div>')
                                ellabel.setPos(myDataFrame["l(m)"][indx-1], eloffset + 1.7 * elheight)
                                p1.addItem(ellabel)
                        elif (myDataFrame["ELEMENT"][indx] == "QUADSXT" ):
                            elcolor='r'
                            if (myDataFrame["ELEMENT"][indx-1] != "QUADSXT" ):
                                ellabel = pg.TextItem(anchor=(0.3,0.8))
                                ellabel.setHtml('<div style="text-align: left"><span style="color: #000000;">QS</span></div>')
                                ellabel.setPos(myDataFrame["l(m)"][indx-1], eloffset + 1.7 * elheight)
                                p1.addItem(ellabel)
                        elif (myDataFrame["ELEMENT"][indx] == "FSOLE" ):
                            elcolor='c'
                            ellabel = pg.TextItem(anchor=(0.,0.8))
                            ellabel.setHtml('<div style="text-align: left"><span style="color: #000000;">S</span></div>')
                            ellabel.setPos(myDataFrame["l(m)"][indx], eloffset + 1.7 * elheight)
                            p1.addItem(ellabel)
                        else:
                            elcolor='w'
                            elbcolor='k'
                        if (eheight > 0. ):
                            qitem  = pg.PlotDataItem(myrect, fillLevel=True, pen=pg.mkPen(elbcolor, width=1), brush = elcolor)
                            p1.addItem(qitem)
                        if (myDataFrame["ELEMENT"][indx] != "DRIFT" ):
                            elitem = pg.PlotDataItem(elrect, fillLevel=True, pen=pg.mkPen(elbcolor, width=1), brush = elcolor)
                            p1.addItem(elitem)
                        indx = indx + 1
            except:
                msg3 = QMessageBox(self)
                msg3.setWindowTitle("Error Message")                                 
                msg3.setText("Failed to read file\n'%s'" % pfname)
                msg3.setIcon(QMessageBox.Icon.Critical)
                msg3.exec()                                                                  
            return
            
######################################################
    def plot_t_envelopes(self):                      #
        '''plot transverse envelopes'''              #
######################################################
        if (ifpath == ""):
            self.get_inp()
        pfname = ifpath + "dynac.print"
#        print("Using data from: ",pfname)

        if pfname:
            try:
                ellength=np.zeros(10000)
                # print("Plotting " + pfname)
                mytime = '{0:%Y-%m-%d %H:%M:%S}'.format(datetime.datetime.now())
                mytitle = "DYNAC " + mytime + "      " + pfname
                # read the first line
                with open(pfname,'r') as myf:
                    first_row = myf.readline()
                my_args = first_row.split()
                num_args = len(first_row.split())
                #print(num_args ,"arguments: ",my_args[0],my_args[1],my_args[2],my_args[3],my_args[4])
                #print(num_args ,"arguments read ")
                # read the remainder of the data
                myDataFrame = pd.read_csv(pfname, skiprows=1, sep=r'\s+', header=None, names=["ELEMENT", "l(m)", "x(mm)", "y(mm)", "z(deg)", "z(mm)", "Ex,n,RMS(mm.mrd)", "Ey,n,RMS(mm.mrd)", "Ez,RMS(keV.ns)", "Wcog(MeV)", "n_of_particles", "xmin(mm)", "xmax(mm)", "ymin(mm)", "ymax(mm)", "tmin(s)", "tmax(s)", "phmin(deg)", "phmax(deg)", "Wmin(MeV)", "Wmax(MeV)", "Dx(m)", "Dy(m)", "dW(MeV)", "Wref(MeV)", "Tref(s)", "Tcog(s)", "xbar(mm)", "ybar(mm)","ax","bx(mm/mrad)","ay","by(mm/mrad)","az","bz(ns/keV)","ElementName","Ax(mm)","Ay(mm)","Ar(mm)"])
                # set up white background
                pg.setConfigOption('background', 'w')
                pg.setConfigOption('foreground', 'k')
                if(sys.version_info[0] == 3) :
                    if(sys.version_info[1] > 8 ) :
                        self.win3 = pg.GraphicsLayoutWidget(title=mytitle, show=True)
                    else:                                        
#                        self.win3 = pg.GraphicsWindow(title=mytitle)
                        self.win3 = pg.GraphicsLayoutWidget(title=mytitle, show=True)
                else:
                    print("You seem to be running Python ",sys.version_info[0],".",sys.version_info[1])                   
                    print("You need to be running Python > 3.4") 
               
#                self.win3 = pg.GraphicsLayoutWidget(title=mytitle, show=True)
                self.win3.resize(1000,700)
                stitle = "No plots selected"
                if (self.checkBox4.isChecked() != 0 ):   
                    stitle = "RMS beam size"
                if (self.checkBox5.isChecked() != 0 ):   
                    stitle = "Beam extent"
                if (self.checkBox4.isChecked() != 0 ) and (self.checkBox5.isChecked() != 0 ):            
                    stitle = "Beam extent and RMS beam size"
                if (self.checkBox4.isChecked() == 0 ) and (self.checkBox5.isChecked() == 0 ) and (self.checkBox8.isChecked() != 0):            
                    stitle = "Beam line elements"
#            self.win.setWindowTitle('plotWidget title') <- use this to update the window title
                self.win3.addLabel(stitle, row=0, col=0, size='12pt')
                pg.setConfigOptions(antialias=True)
                rax=pg.AxisItem('right', pen=None, linkView=None, parent=None, maxTickLength=-5, showValues=False)                
                tax=pg.AxisItem('top',   pen=None, linkView=None, parent=None, maxTickLength=-5, showValues=False)                
                p1 = self.win3.addPlot(title="Horizontal beam size", row=1, col=0, axisItems=({'right': rax,'top': tax}))
                p1.showAxis('right')
                p1.showAxis('top')
#               legend1 = myLegend(offset=(42.,35.), size=(50,50)) pos. is from left top, negative is from right bottom
                legend1 = pg.LegendItem(offset=(892.,35.))
                legend1.setParentItem(p1.graphicsItem())
                legend1.paint = types.MethodType(myLegendPaint, legend1)                
                # Fix the spacing between legend symbol and text.
                legend1.layout.setHorizontalSpacing(20)
########                if (self.checkBox4.isChecked() != 0 ) or (self.checkBox5.isChecked() != 0 ):            
########                    p1.addLegend(offset=(10, 10))
                ezmax=math.ceil(np.nanmax(myDataFrame["l(m)"].values))
                p1.setXRange(0.,ezmax,padding=0)
                if (self.checkBox4.isChecked() != 0 ): 
                    exmax=math.ceil(np.nanmax(myDataFrame["x(mm)"].values))
                    eheight = 0.05 * exmax
                    eoffset = 0. 
                    p1.setYRange(0.,exmax,padding=0)
                    c1 = p1.plot(myDataFrame["l(m)"], myDataFrame["x(mm)"], pen='r')
                    legend1.addItem(myLegendSample(c1), 'Xrms')
                if (self.checkBox5.isChecked() != 0 ):   
                    xext="X" + "<SUB> ext</SUB>"   
                    exmax=math.ceil(np.nanmax(myDataFrame["xmax(mm)"].values))
                    exmin=math.ceil(np.nanmin(myDataFrame["xmin(mm)"].values))
                    if (abs(exmin) > exmax ):
                        exmax = abs(exmin)
                    eheight = 0.1 * exmax
                    eoffset = -0.5 * eheight 
                    p1.setYRange(-exmax*1.1,exmax*1.1,padding=0)
                    c3 = p1.plot(myDataFrame["l(m)"], myDataFrame["xmax(mm)"], pen="#000000")
                    p1.plot(myDataFrame["l(m)"], myDataFrame["xmin(mm)"], pen="#000000")
                    legend1.addItem(myLegendSample(c3), 'Xext')
                if (self.checkBox4.isChecked() == 0 ) and (self.checkBox5.isChecked() == 0 ) and (self.checkBox8.isChecked() != 0):            
                    exmax=math.ceil(np.nanmax(myDataFrame["x(mm)"].values))
                    eheight = 0.05 * exmax
                    eoffset = 0. 
                    p1.setYRange(0.,exmax,padding=0)

                if (self.checkBox9.isChecked() != 0 ):
# plot available aperture  
                    if (math.ceil(np.nanmax(myDataFrame["Ax(mm)"].values)) > exmax):
                        exmax=math.ceil(np.nanmax(myDataFrame["Ax(mm)"].values))
                        eheight = 0.05 * exmax
                        eoffset = 0. 
                    p1.setYRange(0.,exmax,padding=0)
                    ac1 = p1.plot(myDataFrame["l(m)"], myDataFrame["Ax(mm)"], pen='g')
                    legend1.addItem(myLegendSample(ac1), 'Ax')
                    
                    
                p1.setLabel('left', 'X (mm)')
                p1.setLabel('bottom', 'z (m)')
#                myrect = np.array([  [3,2],[3,3],[4,3],[4,2],[3,2]  ])
#                qitem = pg.PlotDataItem(myrect, fillLevel=True, pen=pg.mkPen('b', width=2), brush = 'g')
#                p1.addItem(qitem)
#               number of rows: myDataFrame.shape[0],  number of columns: myDataFrame.shape[1]
                if (self.checkBox8.isChecked() != 0 ):   
                    nelements=-1+myDataFrame.shape[0]
#                    print("Number of beam line elements=",nelements)
                    indx=1
                    newindx=0
# {'b', 'g', 'r', 'c', 'm', 'y', 'k', 'w'},  
# blue, green, red, cyan, magenta, yellow, black, and white
                    while indx < nelements+1:
                        if (myDataFrame["ELEMENT"][indx] != "DRIFT" ):
                            #set default colour for the element border to black             
                            elcolor='r'
                            elbcolor='k'
                            newindx = newindx +1 
                            myrect = np.array([  [myDataFrame["l(m)"][indx-1],eoffset],
                                [myDataFrame["l(m)"][indx-1],eoffset+eheight],
                                [myDataFrame["l(m)"][indx],eoffset+eheight],
                                [myDataFrame["l(m)"][indx],eoffset],
                                [myDataFrame["l(m)"][indx-1],eoffset]  ])
                            if (myDataFrame["ELEMENT"][indx] == "RFQPTQ" ):
                                elcolor='g'
                                if (myDataFrame["ELEMENT"][indx-1] != "RFQPTQ" ):
                                    ellabel = pg.TextItem(anchor=(0.,0.8))
                                    ellabel.setHtml('<div style="text-align: left"><span style="color: #000000;">RFQ</span></div>')
                                    ellabel.setPos(myDataFrame["l(m)"][indx-1], eoffset+eheight)
                                    p1.addItem(ellabel)
                            elif (myDataFrame["ELEMENT"][indx] == "CAVSC" ):
                                elcolor='g'
                                ellabel = pg.TextItem(anchor=(0.9,0.8))
                                ellabel.setHtml('<div style="text-align: left"><span style="color: #000000;">C</span></div>')
                                ellabel.setPos(myDataFrame["l(m)"][indx], eoffset+eheight)
                                p1.addItem(ellabel)
                            elif (myDataFrame["ELEMENT"][indx] == "CAVMC" ):
                                elcolor='g'
                                ellabel = pg.TextItem(anchor=(0.9,0.8))
                                ellabel.setHtml('<div style="text-align: left"><span style="color: #000000;">CA</span></div>')
                                ellabel.setPos(myDataFrame["l(m)"][indx], eoffset+eheight)
                                p1.addItem(ellabel)
                            elif (myDataFrame["ELEMENT"][indx] == "CAVNUM" ):
                                elcolor='g'
                                ellabel = pg.TextItem(anchor=(0.9,0.8))
                                ellabel.setHtml('<div style="text-align: left"><span style="color: #000000;">CA</span></div>')
                                ellabel.setPos(myDataFrame["l(m)"][indx], eoffset+eheight)
                                p1.addItem(ellabel)
                            elif (myDataFrame["ELEMENT"][indx] == "QUADRUPO" ):
                                elcolor='r'
                                if (myDataFrame["ELEMENT"][indx-1] != "QUADRUPO" ):
                                    ellabel = pg.TextItem(anchor=(0.4,0.8))
                                    ellabel.setHtml('<div style="text-align: left"><span style="color: #000000;">Q</span></div>')
                                    ellabel.setPos(myDataFrame["l(m)"][indx-1], eoffset+eheight)
                                    p1.addItem(ellabel)
                            elif (myDataFrame["ELEMENT"][indx] == "QUAFK" ):
                                elcolor='r'
                                if (myDataFrame["ELEMENT"][indx-1] != "QUAFK" ):
                                    ellabel = pg.TextItem(anchor=(0.4,0.8))
                                    ellabel.setHtml('<div style="text-align: left"><span style="color: #000000;">Q</span></div>')
                                    ellabel.setPos(myDataFrame["l(m)"][indx-1], eoffset+eheight)
                                    p1.addItem(ellabel)
                            elif (myDataFrame["ELEMENT"][indx] == "BUNCHER" ):
                                elcolor='g'
                                elbcolor='g'
#                                ellabel = pg.TextItem(anchor=(0.,0.), border='w', fill=(0,0,255))
                                ellabel = pg.TextItem(anchor=(0.5,0.8))
#                                ellabel.setHtml('<div style="text-align: center"><span style="color: #FFF;">Vmax=%0.1f mm/s @ %0.1f s</span></div>'%(np.abs(signemax),tmax))
                                ellabel.setHtml('<div style="text-align: left"><span style="color: #000000;">B</span></div>')
                                ellabel.setPos(myDataFrame["l(m)"][indx], eoffset+eheight)
                                p1.addItem(ellabel)
                            elif (myDataFrame["ELEMENT"][indx] == "STEER" ):
                                elcolor='b'
                                elbcolor='b'
                                ellabel = pg.TextItem(anchor=(0.5,0.8))
                                ellabel.setHtml('<div style="text-align: left"><span style="color: #000000;">C</span></div>')
                                ellabel.setPos(myDataFrame["l(m)"][indx], eoffset + eheight)
                                p1.addItem(ellabel)
                            elif (myDataFrame["ELEMENT"][indx] == "MHB" ):
                                elcolor='g'
                                elbcolor='g'
                                ellabel = pg.TextItem(anchor=(0.9,0.8))
                                ellabel.setHtml('<div style="text-align: left"><span style="color: #000000;">MHB</span></div>')
                                ellabel.setPos(myDataFrame["l(m)"][indx], eoffset+eheight)
                                p1.addItem(ellabel)
                            elif (myDataFrame["ELEMENT"][indx] == "BMAGNET" ):
                                elcolor='y'
                                elbcolor='k'
                                if (myDataFrame["ELEMENT"][indx-1] != "BMAGNET" ):
                                    ellabel = pg.TextItem(anchor=(0.2,0.8))
                                    ellabel.setHtml('<div style="text-align: left"><span style="color: #000000;">DI</span></div>')
                                    ellabel.setPos(myDataFrame["l(m)"][indx-1], eoffset+eheight)
                                    p1.addItem(ellabel)
                            elif (myDataFrame["ELEMENT"][indx] == "QUAELEC" ):
                                elcolor='r'
                                elbcolor='g'
                                if (myDataFrame["ELEMENT"][indx-1] != "QUAELEC" ):
                                    ellabel = pg.TextItem(anchor=(0.,0.8))
                                    ellabel.setHtml('<div style="text-align: left"><span style="color: #000000;">Q</span></div>')
                                    ellabel.setPos(myDataFrame["l(m)"][indx-1], eoffset+eheight)
                                    p1.addItem(ellabel)
                            elif (myDataFrame["ELEMENT"][indx] == "EDFLEC" ):
                                elcolor='y'
                                elbcolor='g'
                                if (myDataFrame["ELEMENT"][indx-1] != "EDFLEC" ):
                                    ellabel = pg.TextItem(anchor=(0.2,0.8))
                                    ellabel.setHtml('<div style="text-align: left"><span style="color: #000000;">DI</span></div>')
                                    ellabel.setPos(myDataFrame["l(m)"][indx-1], eoffset+eheight)
                                    p1.addItem(ellabel)
                            elif (myDataFrame["ELEMENT"][indx] == "SOLENO" ):
                                elcolor='c'
                                if (myDataFrame["ELEMENT"][indx-1] != "SOLENO" ):
                                    ellabel = pg.TextItem(anchor=(0.,0.8))
                                    ellabel.setHtml('<div style="text-align: left"><span style="color: #000000;">S</span></div>')
                                    ellabel.setPos(myDataFrame["l(m)"][indx-1], eoffset+eheight)
                                    p1.addItem(ellabel)
                            elif (myDataFrame["ELEMENT"][indx] == "SEXTUPO" ):
                                elcolor='m'
                                if (myDataFrame["ELEMENT"][indx-1] != "SEXTUPO" ):
                                    ellabel = pg.TextItem(anchor=(0.2,0.8))
                                    ellabel.setHtml('<div style="text-align: left"><span style="color: #000000;">SX</span></div>')
                                    ellabel.setPos(myDataFrame["l(m)"][indx-1], eoffset+eheight)
                                    p1.addItem(ellabel)
                            elif (myDataFrame["ELEMENT"][indx] == "QUADSXT" ):
                                elcolor='r'
#                                elbcolor='m'
                                if (myDataFrame["ELEMENT"][indx-1] != "QUADSXT" ):
                                    ellabel = pg.TextItem(anchor=(0.3,0.8))
                                    ellabel.setHtml('<div style="text-align: left"><span style="color: #000000;">QS</span></div>')
                                    ellabel.setPos(myDataFrame["l(m)"][indx-1], eoffset+eheight)
                                    p1.addItem(ellabel)
                            elif (myDataFrame["ELEMENT"][indx] == "FSOLE" ):
                                elcolor='c'
                                ellabel = pg.TextItem(anchor=(0.,0.8))
                                ellabel.setHtml('<div style="text-align: left"><span style="color: #000000;">S</span></div>')
                                ellabel.setPos(myDataFrame["l(m)"][indx], eoffset+eheight)
                                p1.addItem(ellabel)
                            else:
                                elcolor='w'
                                elbcolor='k'
                            qitem = pg.PlotDataItem(myrect, fillLevel=True, pen=pg.mkPen(elbcolor, width=1), brush = elcolor)
                            p1.addItem(qitem)
                        indx = indx + 1
                # lower plot
                rax=pg.AxisItem('right', pen=None, linkView=None, parent=None, maxTickLength=-5, showValues=False)                
                tax=pg.AxisItem('top',   pen=None, linkView=None, parent=None, maxTickLength=-5, showValues=False)                
                p2 = self.win3.addPlot(title="Vertical beam size", row=2, col=0, axisItems=({'right': rax,'top': tax}))
                p2.showAxis('right')
                p2.showAxis('top')
#               legend2 = myLegend(offset=(42.,35.), size=(50,50)) pos. is from left top, negative is from right bottom
                legend2 = pg.LegendItem(offset=(894.,35.))
                legend2.setParentItem(p2.graphicsItem())
                legend2.paint = types.MethodType(myLegendPaint, legend2)                
                # Fix the spacing between legend symbol and text.
                legend2.layout.setHorizontalSpacing(20)
########                if (self.checkBox4.isChecked() != 0 ) or (self.checkBox5.isChecked() != 0 ):            
########                    p2.addLegend(offset=(10, 10))
                p2.setXRange(0.,ezmax,padding=0)
                if (self.checkBox4.isChecked() != 0 ):   
                    eymax=math.ceil(np.nanmax(myDataFrame["y(mm)"].values))
                    eheight = 0.05 * eymax
                    eoffset = 0. 
                    p2.setYRange(0.,eymax,padding=0)
                    cc1 = p2.plot(myDataFrame["l(m)"], myDataFrame["y(mm)"], pen='b')
                    legend2.addItem(myLegendSample(cc1), 'Yrms')
                if (self.checkBox5.isChecked() != 0 ):
                    yext="Y" + "<SUB> ext</SUB>"   
                    eymax=math.ceil(np.nanmax(myDataFrame["ymax(mm)"].values))
                    eymin=math.ceil(np.nanmin(myDataFrame["ymin(mm)"].values))
                    if (abs(eymin) > eymax ):
                        eymax = abs(eymin)
                    eheight = 0.1 * eymax
                    eoffset = -0.5 * eheight 
                    p2.setYRange(-eymax*1.1,eymax*1.1,padding=0)
                    cc3 = p2.plot(myDataFrame["l(m)"], myDataFrame["ymax(mm)"], pen="#000000")
                    p2.plot(myDataFrame["l(m)"], myDataFrame["ymin(mm)"], pen="#000000")
                    legend2.addItem(myLegendSample(cc3), 'Yext')
                if (self.checkBox4.isChecked() == 0 ) and (self.checkBox5.isChecked() == 0 ) and (self.checkBox8.isChecked() != 0):            
                    eymax=math.ceil(np.nanmax(myDataFrame["y(mm)"].values))
                    eheight = 0.05 * eymax
                    eoffset = 0. 
                    p2.setYRange(0.,eymax,padding=0)
                    
                if (self.checkBox9.isChecked() != 0 ):
# plot available aperture  
                    if (math.ceil(np.nanmax(myDataFrame["Ay(mm)"].values)) > eymax):
                        eymax=math.ceil(np.nanmax(myDataFrame["Ay(mm)"].values))
                        eheight = 0.05 * eymax
                        eoffset = 0. 
                    p2.setYRange(0.,eymax,padding=0)
                    ac2 = p2.plot(myDataFrame["l(m)"], myDataFrame["Ay(mm)"], pen='g')
                    legend2.addItem(myLegendSample(ac1), 'Ay')
                    
                    
                p2.setLabel('left', 'Y (mm)')
                p2.setLabel('bottom', 'z (m)')
                if (self.checkBox8.isChecked() != 0 ):   
                    indx=1
                    newindx=0
                    while indx < nelements+1:
                        if (myDataFrame["ELEMENT"][indx] != "DRIFT" ):
                            #set default colour for the element border to black             
                            elcolor='r'
                            elbcolor='k'
                            newindx = newindx +1 
                            myrect = np.array([  [myDataFrame["l(m)"][indx-1],eoffset],
                                [myDataFrame["l(m)"][indx-1],eoffset+eheight],
                                [myDataFrame["l(m)"][indx],eoffset+eheight],
                                [myDataFrame["l(m)"][indx],eoffset],
                                [myDataFrame["l(m)"][indx-1],eoffset]  ])
                            if (myDataFrame["ELEMENT"][indx] == "RFQPTQ" ):
                                elcolor='g'
                                if (myDataFrame["ELEMENT"][indx-1] != "RFQPTQ" ):
                                    ellabel = pg.TextItem(anchor=(0.,0.8))
                                    ellabel.setHtml('<div style="text-align: left"><span style="color: #000000;">RFQ</span></div>')
                                    ellabel.setPos(myDataFrame["l(m)"][indx-1], eoffset+eheight)
                                    p2.addItem(ellabel)
                            elif (myDataFrame["ELEMENT"][indx] == "CAVSC" ):
                                elcolor='g'
                                ellabel = pg.TextItem(anchor=(0.9,0.8))
                                ellabel.setHtml('<div style="text-align: left"><span style="color: #000000;">C</span></div>')
                                ellabel.setPos(myDataFrame["l(m)"][indx], eoffset+eheight)
                                p2.addItem(ellabel)
                            elif (myDataFrame["ELEMENT"][indx] == "CAVMC" ):
                                elcolor='g'
                                ellabel = pg.TextItem(anchor=(0.9,0.8))
                                ellabel.setHtml('<div style="text-align: left"><span style="color: #000000;">CA</span></div>')
                                ellabel.setPos(myDataFrame["l(m)"][indx], eoffset+eheight)
                                p2.addItem(ellabel)
                            elif (myDataFrame["ELEMENT"][indx] == "CAVNUM" ):
                                elcolor='g'
                                ellabel = pg.TextItem(anchor=(0.9,0.8))
                                ellabel.setHtml('<div style="text-align: left"><span style="color: #000000;">CA</span></div>')
                                ellabel.setPos(myDataFrame["l(m)"][indx], eoffset+eheight)
                                p2.addItem(ellabel)
                            elif (myDataFrame["ELEMENT"][indx] == "QUADRUPO" ):
                                elcolor='r'
                                if (myDataFrame["ELEMENT"][indx-1] != "QUADRUPO" ):
                                    ellabel = pg.TextItem(anchor=(0.4,0.8))
                                    ellabel.setHtml('<div style="text-align: left"><span style="color: #000000;">Q</span></div>')
                                    ellabel.setPos(myDataFrame["l(m)"][indx-1], eoffset+eheight)
                                    p2.addItem(ellabel)
                            elif (myDataFrame["ELEMENT"][indx] == "QUAFK" ):
                                elcolor='r'
                                if (myDataFrame["ELEMENT"][indx-1] != "QUAFK" ):
                                    ellabel = pg.TextItem(anchor=(0.4,0.8))
                                    ellabel.setHtml('<div style="text-align: left"><span style="color: #000000;">Q</span></div>')
                                    ellabel.setPos(myDataFrame["l(m)"][indx-1], eoffset+eheight)
                                    p2.addItem(ellabel)
                            elif (myDataFrame["ELEMENT"][indx] == "BUNCHER" ):
                                elcolor='g'
                                elbcolor='g'
#                                ellabel = pg.TextItem(anchor=(0.,0.), border='w', fill=(0,0,255))
                                ellabel = pg.TextItem(anchor=(0.5,0.8))
#                                ellabel.setHtml('<div style="text-align: center"><span style="color: #FFF;">Vmax=%0.1f mm/s @ %0.1f s</span></div>'%(np.abs(signemax),tmax))
                                ellabel.setHtml('<div style="text-align: left"><span style="color: #000000;">B</span></div>')
                                ellabel.setPos(myDataFrame["l(m)"][indx], eoffset+eheight)
                                p2.addItem(ellabel)
                            elif (myDataFrame["ELEMENT"][indx] == "STEER" ):
                                elcolor='b'
                                elbcolor='b'
                                ellabel = pg.TextItem(anchor=(0.5,0.8))
                                ellabel.setHtml('<div style="text-align: left"><span style="color: #000000;">C</span></div>')
                                ellabel.setPos(myDataFrame["l(m)"][indx], eoffset + eheight)
                                p2.addItem(ellabel)
                            elif (myDataFrame["ELEMENT"][indx] == "MHB" ):
                                elcolor='g'
                                elbcolor='g'
                                ellabel = pg.TextItem(anchor=(0.9,0.8))
                                ellabel.setHtml('<div style="text-align: left"><span style="color: #000000;">MHB</span></div>')
                                ellabel.setPos(myDataFrame["l(m)"][indx], eoffset+eheight)
                                p2.addItem(ellabel)
                            elif (myDataFrame["ELEMENT"][indx] == "BMAGNET" ):
                                elcolor='y'
                                elbcolor='k'
                                if (myDataFrame["ELEMENT"][indx-1] != "BMAGNET" ):
                                    ellabel = pg.TextItem(anchor=(0.2,0.8))
                                    ellabel.setHtml('<div style="text-align: left"><span style="color: #000000;">DI</span></div>')
                                    ellabel.setPos(myDataFrame["l(m)"][indx-1], eoffset+eheight)
                                    p2.addItem(ellabel)
                            elif (myDataFrame["ELEMENT"][indx] == "QUAELEC" ):
                                elcolor='r'
                                elbcolor='g'
                                if (myDataFrame["ELEMENT"][indx-1] != "QUAELEC" ):
                                    ellabel = pg.TextItem(anchor=(0.,0.8))
                                    ellabel.setHtml('<div style="text-align: left"><span style="color: #000000;">Q</span></div>')
                                    ellabel.setPos(myDataFrame["l(m)"][indx-1], eoffset+eheight)
                                    p2.addItem(ellabel)
                            elif (myDataFrame["ELEMENT"][indx] == "EDFLEC" ):
                                elcolor='y'
                                elbcolor='g'
                                if (myDataFrame["ELEMENT"][indx-1] != "EDFLEC" ):
                                    ellabel = pg.TextItem(anchor=(0.2,0.8))
                                    ellabel.setHtml('<div style="text-align: left"><span style="color: #000000;">DI</span></div>')
                                    ellabel.setPos(myDataFrame["l(m)"][indx-1], eoffset+eheight)
                                    p2.addItem(ellabel)
                            elif (myDataFrame["ELEMENT"][indx] == "SOLENO" ):
                                elcolor='c'
                                if (myDataFrame["ELEMENT"][indx-1] != "SOLENO" ):
                                    ellabel = pg.TextItem(anchor=(0.,0.8))
                                    ellabel.setHtml('<div style="text-align: left"><span style="color: #000000;">S</span></div>')
                                    ellabel.setPos(myDataFrame["l(m)"][indx-1], eoffset+eheight)
                                    p2.addItem(ellabel)
                            elif (myDataFrame["ELEMENT"][indx] == "SEXTUPO" ):
                                elcolor='m'
                                if (myDataFrame["ELEMENT"][indx-1] != "SEXTUPO" ):
                                    ellabel = pg.TextItem(anchor=(0.2,0.8))
                                    ellabel.setHtml('<div style="text-align: left"><span style="color: #000000;">SX</span></div>')
                                    ellabel.setPos(myDataFrame["l(m)"][indx-1], eoffset+eheight)
                                    p2.addItem(ellabel)
                            elif (myDataFrame["ELEMENT"][indx] == "QUADSXT" ):
                                elcolor='r'
#                                elbcolor='m'
                                if (myDataFrame["ELEMENT"][indx-1] != "QUADSXT" ):
                                    ellabel = pg.TextItem(anchor=(0.3,0.8))
                                    ellabel.setHtml('<div style="text-align: left"><span style="color: #000000;">QS</span></div>')
                                    ellabel.setPos(myDataFrame["l(m)"][indx-1], eoffset+eheight)
                                    p2.addItem(ellabel)
                            elif (myDataFrame["ELEMENT"][indx] == "FSOLE" ):
                                elcolor='c'
                                ellabel = pg.TextItem(anchor=(0.,0.8))
                                ellabel.setHtml('<div style="text-align: left"><span style="color: #000000;">S</span></div>')
                                ellabel.setPos(myDataFrame["l(m)"][indx], eoffset+eheight)
                                p2.addItem(ellabel)
                            else:
                                elcolor='w'
                                elbcolor='k'
                            qitem = pg.PlotDataItem(myrect, fillLevel=True, pen=pg.mkPen(elbcolor, width=1), brush = elcolor)
                            p2.addItem(qitem)
                        indx = indx + 1
            except:
                msg3 = QMessageBox(self)
                msg3.setWindowTitle("Error Message")                                 
                msg3.setText("Failed to read file\n'%s'" % pfname)
                msg3.setIcon(QMessageBox.Icon.Critical)
                msg3.exec()                                                                  
            return

######################################################
    def plot_l_envelopes(self):                      #
        '''plot longitudinal envelopes'''            #
######################################################
        if (ifpath == ""):
            self.get_inp()
        pfname = ifpath + "dynac.print"
#        print("Using data from: ",pfname)

        if pfname:
            try:
                # print("Plotting " + pfname)
                mytime = '{0:%Y-%m-%d %H:%M:%S}'.format(datetime.datetime.now())
                mytitle = "DYNAC " + mytime + "      " + pfname
                # read the first line
                with open(pfname,'r') as myf:
                    first_row = myf.readline()
                my_args = first_row.split()
                num_args = len(first_row.split())
                #print(num_args ,"arguments: ",my_args[0],my_args[1],my_args[2],my_args[3],my_args[4])
                #print(num_args ,"arguments read ")
                # read the remainder of the data
                myDataFrame = pd.read_csv(pfname, skiprows=1, sep=r'\s+', header=None, names=["ELEMENT", "l(m)", "x(mm)", "y(mm)", "z(deg)", "z(mm)", "Ex,n,RMS(mm.mrd)", "Ey,n,RMS(mm.mrd)", "Ez,RMS(keV.ns)", "Wcog(MeV)", "n_of_particles", "xmin(mm)", "xmax(mm)", "ymin(mm)", "ymax(mm)", "tmin(s)", "tmax(s)", "phmin(deg)", "phmax(deg)", "Wmin(MeV)", "Wmax(MeV)", "Dx(m)", "Dy(m)", "dW(MeV)", "Wref(MeV)", "Tref(s)", "Tcog(s)", "xbar(mm)", "ybar(mm)","ax","bx(mm/mrad)","ay","by(mm/mrad)","az","bz(ns/keV)","ElementName","Ax(mm)","Ay(mm)","Ar(mm)"])
                # set up white background
                pg.setConfigOption('background', 'w')
                pg.setConfigOption('foreground', 'k')
                if(sys.version_info[0] == 3) :
                    if(sys.version_info[1] > 8 ) :
                        self.win4 = pg.GraphicsLayoutWidget(title=mytitle, show=True)
                    else:                                        
#                        self.win4 = pg.GraphicsWindow(title=mytitle)
                        self.win4 = pg.GraphicsLayoutWidget(title=mytitle, show=True)
                else:
                    print("You seem to be running Python ",sys.version_info[0],".",sys.version_info[1])                   
                    print("You need to be running Python > 3.4") 
                
#                self.win4 = pg.GraphicsLayoutWidget(title=mytitle, show=True)
                self.win4.resize(1000,700)
                stitle = "No plots selected"
                if (self.checkBox4.isChecked() != 0 ):   
                    stitle = "RMS beam size"
                if (self.checkBox5.isChecked() != 0 ):   
                    stitle = "Beam extent"
                if (self.checkBox4.isChecked() != 0 ) and (self.checkBox5.isChecked() != 0 ):            
                    stitle = "RMS beam size and beam extent"
#            self.win.setWindowTitle('plotWidget title') <- use this to update the window title
                self.win4.addLabel(stitle, row=0, col=0, size='12pt')
                pg.setConfigOptions(antialias=True)
                rax=pg.AxisItem('right', pen=None, linkView=None, parent=None, maxTickLength=-5, showValues=False)                
                tax=pg.AxisItem('top',   pen=None, linkView=None, parent=None, maxTickLength=-5, showValues=False)                
                p1 = self.win4.addPlot(title="Energy spread", row=1, col=0, axisItems=({'right': rax,'top': tax}))
                p1.showAxis('right')
                p1.showAxis('top')
#               legend1 = myLegend(offset=(42.,35.), size=(50,50)) pos. is from left top, negative is from right bottom
                legend1 = pg.LegendItem(offset=(62.,35.))
                legend1.setParentItem(p1.graphicsItem())
                legend1.paint = types.MethodType(myLegendPaint, legend1)                
                # Fix the spacing between legend symbol and text.
                legend1.layout.setHorizontalSpacing(20)
                ezmax=math.ceil(np.nanmax(myDataFrame["l(m)"].values))
                p1.setXRange(0.,ezmax,padding=0)
                if (self.checkBox4.isChecked() != 0 ):   
                    c1 = p1.plot(myDataFrame["l(m)"], myDataFrame["dW(MeV)"], pen='r')
                    legend1.addItem(myLegendSample(c1), "<font>dW<SUB>rms</SUB></font>")
                if (self.checkBox5.isChecked() != 0 ):   
                    c3 = p1.plot(myDataFrame["l(m)"], myDataFrame["Wmax(MeV)"], pen="#000000")
                    p1.plot(myDataFrame["l(m)"], myDataFrame["Wmin(MeV)"], pen="#000000")
                    legend1.addItem(myLegendSample(c3), "<font>dW<SUB>ext</SUB></font>")
                p1.setLabel('left', 'dW (MeV)')
                p1.setLabel('bottom', 'z (m)')
                # lower plot
                rax=pg.AxisItem('right', pen=None, linkView=None, parent=None, maxTickLength=-5, showValues=False)                
                tax=pg.AxisItem('top',   pen=None, linkView=None, parent=None, maxTickLength=-5, showValues=False)                
                p2 = self.win4.addPlot(title="Phase spread", row=2, col=0, axisItems=({'right': rax,'top': tax}))
                p2.showAxis('right')
                p2.showAxis('top')
#               legend2 = myLegend(offset=(42.,35.), size=(50,50)) pos. is from left top, negative is from right bottom
                legend2 = pg.LegendItem(offset=(62.,35.))
                legend2.setParentItem(p2.graphicsItem())
                legend2.paint = types.MethodType(myLegendPaint, legend2)                
                # Fix the spacing between legend symbol and text.
                legend2.layout.setHorizontalSpacing(20)
                p2.setXRange(0.,ezmax,padding=0)
                if (self.checkBox4.isChecked() != 0 ): 
                    mylab =  'd\N{GREEK SMALL LETTER PHI}' + "<font><SUB>rms</SUB></font>" 
                    cc1 = p2.plot(myDataFrame["l(m)"], myDataFrame["z(deg)"], pen='b', name=mylab)
                    legend2.addItem(myLegendSample(cc1), mylab)
                if (self.checkBox5.isChecked() != 0 ):   
                    mylab =  'd\N{GREEK SMALL LETTER PHI}' + "<font><SUB>ext</SUB></font>" 
                    cc3 = p2.plot(myDataFrame["l(m)"], myDataFrame["phmax(deg)"], pen="#000000")
                    p2.plot(myDataFrame["l(m)"], myDataFrame["phmin(deg)"], pen="#000000")
                    legend2.addItem(myLegendSample(cc3), mylab)
                p2.setLabel('left', 'd\N{GREEK SMALL LETTER PHI} (deg)')
                p2.setLabel('bottom', 'z (m)')
            except:
                msg3 = QMessageBox(self)
                msg3.setWindowTitle("Error Message")                                 
                msg3.setText("Failed to read file\n'%s'" % pfname)
                msg3.setIcon(QMessageBox.Icon.Critical)
                msg3.exec()                                                                  
            return
            
######################################################
    def plot_dispersion(self):                       #
        '''plot dispersion'''                        #
######################################################
        if (ifpath == ""):
            self.get_inp()
        pfname = ifpath + "dynac.print"
#        print("Using data from: ",pfname)

        if pfname:
            try:
                # print("Plotting " + pfname)
                mytime = '{0:%Y-%m-%d %H:%M:%S}'.format(datetime.datetime.now())
                mytitle = "DYNAC " + mytime + "      " + pfname
                # read the first line
                with open(pfname,'r') as myf:
                    first_row = myf.readline()
                my_args = first_row.split()
                num_args = len(first_row.split())
                #print(num_args ,"arguments: ",my_args[0],my_args[1],my_args[2],my_args[3],my_args[4])
                #print(num_args ,"arguments read ")
                # read the remainder of the data
                myDataFrame = pd.read_csv(pfname, skiprows=1, sep=r'\s+', header=None, names=["ELEMENT", "l(m)", "x(mm)", "y(mm)", "z(deg)", "z(mm)", "Ex,n,RMS(mm.mrd)", "Ey,n,RMS(mm.mrd)", "Ez,RMS(keV.ns)", "Wcog(MeV)", "n_of_particles", "xmin(mm)", "xmax(mm)", "ymin(mm)", "ymax(mm)", "tmin(s)", "tmax(s)", "phmin(deg)", "phmax(deg)", "Wmin(MeV)", "Wmax(MeV)", "Dx(m)", "Dy(m)", "dW(MeV)", "Wref(MeV)", "Tref(s)", "Tcog(s)", "xbar(mm)", "ybar(mm)","ax","bx(mm/mrad)","ay","by(mm/mrad)","az","bz(ns/keV)","ElementName","Ax(mm)","Ay(mm)","Ar(mm)"])
                # set up white background
                pg.setConfigOption('background', 'w')
                pg.setConfigOption('foreground', 'k')
                if(sys.version_info[0] == 3) :
                    if(sys.version_info[1] > 8 ) :
                        self.win5 = pg.GraphicsLayoutWidget(title=mytitle, show=True)
                    else:                                        
#                        self.win5 = pg.GraphicsWindow(title=mytitle)
                        self.win5 = pg.GraphicsLayoutWidget(title=mytitle, show=True)
                else:
                    print("You seem to be running Python ",sys.version_info[0],".",sys.version_info[1])                   
                    print("You need to be running Python > 3.4") 
                
#                self.win5 = pg.GraphicsLayoutWidget(title=mytitle, show=True)
                self.win5.resize(1000,700)
                stitle = "Dispersion as a function of position"
#            self.win.setWindowTitle('plotWidget title') <- use this to update the window title
                self.win5.addLabel(stitle, row=0, col=0, size='12pt')
                pg.setConfigOptions(antialias=True)
                rax=pg.AxisItem('right', pen=None, linkView=None, parent=None, maxTickLength=-5, showValues=False)                
                tax=pg.AxisItem('top',   pen=None, linkView=None, parent=None, maxTickLength=-5, showValues=False)                
                p1 = self.win5.addPlot(title="H dispersion", row=1, col=0, axisItems=({'right': rax,'top': tax}))
                p1.showAxis('right')
                p1.showAxis('top')
#               legend1 = myLegend(offset=(42.,35.), size=(50,50)) pos. is from left top, negative is from right bottom
                legend1 = pg.LegendItem(offset=(71.,35.))
                legend1.setParentItem(p1.graphicsItem())
                legend1.paint = types.MethodType(myLegendPaint, legend1)                
                # Fix the spacing between legend symbol and text.
                legend1.layout.setHorizontalSpacing(20)
                ezmax=math.ceil(np.nanmax(myDataFrame["l(m)"].values))
                p1.setXRange(0.,ezmax,padding=0)
                c1 = p1.plot(myDataFrame["l(m)"], myDataFrame["Dx(m)"], pen='r')
                legend1.addItem(myLegendSample(c1), 'Dx')
                p1.setLabel('left', 'Dx (m)')
                p1.setLabel('bottom', 'z (m)')
                # lower plot
                rax=pg.AxisItem('right', pen=None, linkView=None, parent=None, maxTickLength=-5, showValues=False)                
                tax=pg.AxisItem('top',   pen=None, linkView=None, parent=None, maxTickLength=-5, showValues=False)                
                p2 = self.win5.addPlot(title="V dispersion", row=2, col=0, axisItems=({'right': rax,'top': tax}))
                p2.showAxis('right')
                p2.showAxis('top')
#               legend2 = myLegend(offset=(42.,35.), size=(50,50)) pos. is from left top, negative is from right bottom
                legend2 = pg.LegendItem(offset=(71.,35.))
                legend2.setParentItem(p2.graphicsItem())
                legend2.paint = types.MethodType(myLegendPaint, legend2)                
                # Fix the spacing between legend symbol and text.
                legend2.layout.setHorizontalSpacing(20)
                p2.setXRange(0.,ezmax,padding=0)
                cc1 = p2.plot(myDataFrame["l(m)"], myDataFrame["Dy(m)"], pen='b')
                legend2.addItem(myLegendSample(cc1), 'Dy')
                p2.setLabel('left', 'Dy (m)')
                p2.setLabel('bottom', 'z (m)')
            except:
                msg3 = QMessageBox(self)
                msg3.setWindowTitle("Error Message")                                 
                msg3.setText("Failed to read file\n'%s'" % pfname)
                msg3.setIcon(QMessageBox.Icon.Critical)
                msg3.exec()                                                                  
            return

######################################################
    def plot_transmission(self):                     #
        '''plot transmission'''                      #
######################################################
        if (ifpath == ""):
            self.get_inp()
        pfname = ifpath + "dynac.print"
#        print("Using data from: ",pfname)

        if pfname:
            try:
                # print("Plotting " + pfname)
                mytime = '{0:%Y-%m-%d %H:%M:%S}'.format(datetime.datetime.now())
                mytitle = "DYNAC " + mytime + "      " + pfname
                # read the first line
                with open(pfname,'r') as myf:
                    first_row = myf.readline()
                    second_row = myf.readline()
                    second_row_args = second_row.split()
                    nopatinput = int(second_row_args[10])
                my_args = first_row.split()
                num_args = len(first_row.split())
                #print(num_args ,"arguments: ",my_args[0],my_args[1],my_args[2],my_args[3],my_args[4])
                #print(num_args ,"arguments read ")
                # read the remainder of the data
                myDataFrame = pd.read_csv(pfname, skiprows=1, sep=r'\s+', header=None, names=["ELEMENT", "l(m)", "x(mm)", "y(mm)", "z(deg)", "z(mm)", "Ex,n,RMS(mm.mrd)", "Ey,n,RMS(mm.mrd)", "Ez,RMS(keV.ns)", "Wcog(MeV)", "n_of_particles", "xmin(mm)", "xmax(mm)", "ymin(mm)", "ymax(mm)", "tmin(s)", "tmax(s)", "phmin(deg)", "phmax(deg)", "Wmin(MeV)", "Wmax(MeV)", "Dx(m)", "Dy(m)", "dW(MeV)", "Wref(MeV)", "Tref(s)", "Tcog(s)", "xbar(mm)", "ybar(mm)","ax","bx(mm/mrad)","ay","by(mm/mrad)","az","bz(ns/keV)","ElementName","Ax(mm)","Ay(mm)","Ar(mm)"])
                # set up white background
                pg.setConfigOption('background', 'w')
                pg.setConfigOption('foreground', 'k')
                if(sys.version_info[0] == 3) :
                    if(sys.version_info[1] > 8 ) :
                        self.win6 = pg.GraphicsLayoutWidget(title=mytitle, show=True)
                    else:                                        
#                        self.win6 = pg.GraphicsWindow(title=mytitle)
                        self.win6 = pg.GraphicsLayoutWidget(title=mytitle, show=True)
                else:
                    print("You seem to be running Python ",sys.version_info[0],".",sys.version_info[1])                   
                    print("You need to be running Python > 3.7") 
                
#                self.win6 = pg.GraphicsLayoutWidget(title=mytitle, show=True)
                self.win6.resize(1000,700)
                stitle = "Centroids and transmission as a function of position"
#            self.win.setWindowTitle('plotWidget title') <- use this to update the window title
                self.win6.addLabel(stitle, row=0, col=0, size='12pt')
                pg.setConfigOptions(antialias=True)
                rax=pg.AxisItem('right', pen=None, linkView=None, parent=None, maxTickLength=-5, showValues=False)                
                tax=pg.AxisItem('top',   pen=None, linkView=None, parent=None, maxTickLength=-5, showValues=False)                
                p1 = self.win6.addPlot(title="Transverse centroids", row=1, col=0, axisItems=({'right': rax,'top': tax}))
                p1.showAxis('right')
                p1.showAxis('top')
#               legend1 = myLegend(offset=(42.,35.), size=(50,50)) pos. is from left top, negative is from right bottom
                legend1 = pg.LegendItem(offset=(67.,35.))
                legend1.setParentItem(p1.graphicsItem())
                legend1.paint = types.MethodType(myLegendPaint, legend1)                
                # Fix the spacing between legend symbol and text.
                legend1.layout.setHorizontalSpacing(20)
                ezmax=math.ceil(np.nanmax(myDataFrame["l(m)"].values))
                p1.setXRange(0.,ezmax,padding=0)
                mylab = '<font><span style="text-decoration: overline">X</span></font>'
#                p1.plot(myDataFrame["l(m)"], myDataFrame["xbar(mm)"], pen='r', name='x\u0304')
                c1 = p1.plot(myDataFrame["l(m)"], myDataFrame["xbar(mm)"], pen='r')
                legend1.addItem(myLegendSample(c1), mylab)
                mylab = '<font><span style="text-decoration: overline">Y</span></font>'
                c2 = p1.plot(myDataFrame["l(m)"], myDataFrame["ybar(mm)"], pen='b')
                legend1.addItem(myLegendSample(c2), mylab)
                p1.setLabel('left', 'Position (mm)')
                p1.setLabel('bottom', 'z (m)')
                # lower plot
                rax=pg.AxisItem('right', pen=None, linkView=None, parent=None, maxTickLength=-5, showValues=False)                
                tax=pg.AxisItem('top',   pen=None, linkView=None, parent=None, maxTickLength=-5, showValues=False)                
                p2 = self.win6.addPlot(title="Transmission", row=2, col=0, axisItems=({'right': rax,'top': tax}))
                p2.showAxis('right')
                p2.showAxis('top')
#                p2.addLegend(offset=(10, 10))
                p2.setXRange(0.,ezmax,padding=0)
                p2.plot(myDataFrame["l(m)"], 100*myDataFrame["n_of_particles"]/nopatinput, pen='r', name='')
                p2.setLabel('left', 'Transmission (%)')
                p2.setLabel('bottom', 'z (m)')
            except:
                msg3 = QMessageBox(self)
                msg3.setWindowTitle("Error Message")                                 
                msg3.setText("Failed to read file\n'%s'" % pfname)
                msg3.setIcon(QMessageBox.Icon.Critical)
                msg3.exec()                                                                  
            return

######################################################
    def plotit(self):                                #
        '''plotit'''                                 #
######################################################
        self.PlotBtn1.setStyleSheet("color : #ff0000; ")        
        #print("System=",platform.system())
        if (platform.system() == 'Windows') :
            ndynpath= dynpath[:-5] + os.sep
            doplotit = '"' + ndynpath + "plot" + os.sep + "dynplt" + '"'
            plotitopt = "w"
#            doplotit = os.path.abspath(doplotit)
        if (platform.system() == 'Linux') :
            doplotit = dynpath[0:-4] + os.sep + "plot" + os.sep + "dynplt"
            plotitopt = "l"
#            doplotit = '"' + os.path.abspath(doplotit) + '"'                 
            doplotit = os.path.abspath(doplotit) 
        if (platform.system() == 'Darwin') :
            doplotit = dynpath[0:-4] + os.sep + "plot" + os.sep + "dynplt"
            plotitopt = "m"
#            doplotit = '"' + os.path.abspath(doplotit) + '"'                 
            doplotit = os.path.abspath(doplotit)
        if (ifpath == ""):
            self.get_inp()
        pathop= "-p" 
#        pathop= pathop + '"' + ifpath + '"'
        pathop= pathop + ifpath 
        dgo = "-dgui"        
        cmd = doplotit + " " + plotitopt + " " + dgo + " " + pathop
        ipathname = pathop
#        print("Run plotit using ",doplotit," ",plotitopt," ",dgo," ",ipathname)
        mytext="Run plotit using " + cmd + "\n"
        if (self.checkBox3.isChecked() != 0 ):   
            self.inpLog.clear()            # Clear text in the log frame before running again
        self.log_it(mytext)
        self.plotitraw = QtCore.QProcess()
        self.plotitraw.readyReadStandardOutput.connect(self.procServer1)
        self.plotitraw.finished.connect(self.plotitraw_done)
#        self.plotitraw.start(cmd)
        self.plotitraw.start(doplotit,[plotitopt,dgo,ipathname])

######################################################
    def splotit(self):                                #
        '''splotit'''                                 #
######################################################
        global gr_file_ext
        self.PlotBtn1s.setStyleSheet("color : #ff0000; ")        
        #print("System in SPLOTIT=",platform.system())
        if (platform.system() == 'Windows') :
            ndynpath= dynpath[:-5] + os.sep
            doplotit = '"' + ndynpath + "plot" + os.sep + "dynplt" + '"'
            plotitopt = "w"
#            doplotit = os.path.abspath(doplotit)
        if (platform.system() == 'Linux') :
            doplotit = dynpath[0:-4] + os.sep + "plot" + os.sep + "dynplt"
            plotitopt = "l"
#            doplotit = '"' + os.path.abspath(doplotit) + '"' 
            doplotit = os.path.abspath(doplotit)  
        if (platform.system() == 'Darwin') :
            doplotit = dynpath[0:-4] + os.sep + "plot" + os.sep + "dynplt"
            plotitopt = "m"
            doplotit = os.path.abspath(doplotit)                    
#            doplotit = '"' + os.path.abspath(doplotit) + '"'                 
        if (ifpath == ""):
            self.get_inp()
        # delete the old files before creating the new ones            
        ifnum = 1
        sfnum = str(ifnum).zfill(3)
        cp_file=os.path.dirname(ifname) + os.sep + "dynplot" + sfnum + "." + gr_file_ext
        while (os.path.exists(cp_file)) :
            if (platform.system() == 'Windows') :
                cmd = 'del /Q "' + cp_file + '"'
            else:
                cmd = 'rm ' + cp_file 
                #print('P ',cmd) 
            os.system(cmd)
            ifnum += 1
            sfnum = str(ifnum).zfill(3)
            cp_file=os.path.dirname(ifname) + os.sep + "dynplot" + sfnum + "." + gr_file_ext
        # creating the new ones            
        pathop= "-p" 
#        pathop= pathop + '"' + ifpath + '"'
        pathop= pathop + ifpath 
        dgo = "-tt" + gr_file_ext + " "        
        cmd = doplotit + " " + plotitopt + " " + dgo + " " + pathop
        ipathname = pathop
#        print("Run plotit (save plots) using ",doplotit," ",plotitopt," ",dgo," ",ipathname)
        mytext="Run plotit (save plots) using " + cmd + "\n"
        if (self.checkBox3.isChecked() != 0 ):   
            self.inpLog.clear()            # Clear text in the log frame before running again
        self.log_it(mytext)
        self.plotitraw = QtCore.QProcess()
        self.plotitraw.readyReadStandardOutput.connect(self.procServer1)
        self.plotitraw.finished.connect(self.splotitraw_done)
#        self.plotitraw.start(cmd)
        self.plotitraw.start(doplotit,[plotitopt,dgo,ipathname])
        
######################################################
    def close_gnuplots(self):                        #
        '''Close gnuplot windows opened by plotit''' #
######################################################
        self.plotitclose = QtCore.QProcess()
        if (platform.system() == 'Windows') :
#            cmd = "Taskkill /IM gnuplot_qt.exe /F"
            cmd = "Taskkill"
            cmdopt1 = '/IM'  
            cmdopt2 = 'gnuplot_qt.exe'           
            cmdopt3 = '/F'
        if (platform.system() == 'Linux') :
            self.getuser()
#            cmd = "killall -u " + cuser + " gnuplot_x11"
            cmd = "killall" 
            cmdopt1 = '-u' 
            cmdopt2 = cuser 
            cmdopt3 = 'gnuplot_x11'
        if (platform.system() == 'Darwin') :
            self.getuser()
#            cmd = "killall -u " + cuser + " gnuplot_x11"
            cmd = "killall" 
            cmdopt1 = '-u' 
            cmdopt2 = cuser 
            cmdopt3 = 'gnuplot_x11'
#        print("Run close gnuplots using ",cmd," ",cmdopt1," ",cmdopt2," ",cmdopt3)
#        self.plotitclose.start(cmd)
        self.plotitclose.start(cmd,[cmdopt1,cmdopt2,cmdopt3])

######################################################
    def run_dyn(self):                               #
        '''Run DYNAC'''                              #
######################################################
        global dorun,dynopt,ifname
        ButtonColor = self.RightBtn2.styleSheet()
        self.inpLog.verticalScrollBar().setValue(self.inpLog.verticalScrollBar().maximum())
        if(ButtonColor == "color : #ff0000; "):
            self.dynraw.kill()
            mytext = ""
            mytext = '\n' + 'DYNAC execution stopped by user'
            self.cursor = self.inpLog.textCursor()
            self.inpLog.setTextCursor(self.cursor)
            self.cursor.insertText(mytext) 
        else:
            self.RightBtn2.setStyleSheet("color : #ff0000; ")       
            if (platform.system() == 'Windows') :
                dorun=dynpath[:-1] + os.sep + dynacv
#                dorun='"' + dorun + '"'
                newifpath='"' + ifpath + '"'
                dynopt="-p" + ifpath
            else:    
                dorun=dynpath + os.sep + dynacv
#                dorun='"' + dorun + '"'
                newifpath='"' + ifpath + '"'
#                dynopt="-p" + newifpath
                dynopt="-p" + ifpath
            ifname=self.text_box2.toPlainText() 
            if(ifname == ""):
                self.get_inp()
                if (platform.system() == 'Windows') :
                    lastsep=ifname.rfind(os.sep)
#                    newifpath='"' + ifname[0:lastsep+1] + '"'
                    newifpath=ifname[0:lastsep+1]
                    dynopt="-p" + newifpath
                    cmd=dorun + " " + dynopt + " "
                    cmd=cmd + '"' + ifname + '"'
                else:    
#                    newifpath='"' + ifpath + '"'
                    newifpath=ifpath
                    dynopt="-p" + newifpath
                    cmd=dorun + " " + dynopt + " " 
                    cmd=cmd + '"' + ifname + '"'
            else:
                if (platform.system() == 'Windows') :
                    cmd=dorun + " " + dynopt + " "
                    cmd=cmd + '"' + ifname + '"'
                else:    
                    cmd=dorun + " " + dynopt + " " 
                    cmd=cmd + '"' + ifname + '"'
#            print("Run DYNAC using ",dorun," ",dynopt," ",ifname)
            self.dynraw = QtCore.QProcess()
            if (self.checkBox3.isChecked() != 0 ):   
                self.inpLog.clear()            # Clear text in the log frame before running again
            self.firstTransport = True 
            self.dynraw.readyReadStandardOutput.connect(self.procServer2)
#            self.dynraw.readyReadStandardOutput.connect(partial(self.procServer3, self.dynraw))
            self.pos1=self.cursor.position()
            self.dynraw.finished.connect(self.dynraw_done)
#            self.dynraw.start(cmd)
            self.dynraw.start(dorun,[dynopt,ifname])

######################################################
    def procServer3(self, other):                    #
######################################################
        global pos1
        self.inpLog.insertPlainText("\n")                
        income = self.dynraw.readAllStandardOutput().data()
        pincome = income.decode('utf-8','ignore').strip()
        pincome.replace("\r\n","")
        if "Transport" in pincome:  
            if self.firstTransport:
                self.firstTransport = False
                self.pos1=self.cursor.position()        
                self.inpLog.insertPlainText(pincome)
            else:
                self.cursor.setPosition(self.pos1, 1)        
                self.inpLog.setTextCursor(self.cursor)        
                self.inpLog.insertPlainText(pincome)
        else:
            self.inpLog.insertPlainText(pincome)

######################################################
    def procServer2(self):                           #
######################################################
        global pos1
        self.inpLog.insertPlainText("\n")                
        income = self.dynraw.readAllStandardOutput().data()
        pincome = income.decode('utf-8','ignore').strip()
        pincome.replace("\r\n","")
        if "Transport" in pincome:  
            if self.firstTransport:
                self.firstTransport = False
                self.pos1=self.cursor.position()        
                self.inpLog.insertPlainText(pincome)
            else:
#                self.cursor.setPosition(self.pos1, 1) 
                self.cursor.setPosition(self.pos1, QTextCursor.MoveMode.KeepAnchor)       
                self.inpLog.setTextCursor(self.cursor)        
                self.inpLog.insertPlainText(pincome)
        else:
            self.inpLog.insertPlainText(pincome)

######################################################
    def procServer1(self):                           #
#       print text from plotit (gnuplot) to text box #
######################################################
        income = self.plotitraw.readAllStandardOutput().data()
        pincome = income.decode('utf-8','ignore').strip()
        self.log_it(pincome+"\n")

######################################################
    def getuser(self):                               #
######################################################
        global cuser
#        print("In GetUser ", platform.system())
        if (platform.system() != 'Windows') :
            cuser=subprocess.getoutput(['whoami'])
#        print("User=",cuser)


######################################################
    def dynraw_done(self):                           #
######################################################
        #dynac execution done; change button color back to blue    
        self.RightBtn2.setStyleSheet("color : #0000ff; ") 
        self.inpLog.insertPlainText("\n")                
        self.inpLog.insertPlainText("\n")                
        self.inpLog.verticalScrollBar().setValue(self.inpLog.verticalScrollBar().maximum())
        self.inpLog.repaint()

######################################################
    def plotitraw_done(self):                        #
######################################################
        #plotit execution done; change button color back to blue    
        self.PlotBtn1.setStyleSheet("color : #0000ff; ") 
        self.inpLog.insertPlainText("\n")                
        self.inpLog.insertPlainText("\n")                
        self.inpLog.verticalScrollBar().setValue(self.inpLog.verticalScrollBar().maximum())
        self.inpLog.repaint()

######################################################
    def splotitraw_done(self):                        #
######################################################
        #plotit execution done; change button color back to blue    
        self.PlotBtn1s.setStyleSheet("color : #0000ff; ") 
        self.inpLog.insertPlainText("\n")                
        self.inpLog.insertPlainText("\n")                
        self.inpLog.verticalScrollBar().setValue(self.inpLog.verticalScrollBar().maximum())
        self.inpLog.repaint()

    def get_r1(self):
        dummy=0 
#        print('R1 selected')

    def get_r2(self):
        dummy=0 
#        print('R2 Selected')
        
    def get_cb1(self):
        dummy=0 
#        print('CB1 selected')

    def get_cb2(self):
        dummy=0 
#        print('CB2 Selected')
        
    def get_cb3(self):
        if (self.checkBox3.isChecked() != 0 ):   
            dummy=0 
#            print('CB3 Selected')
        else:    
            dummy=0 
#            print('CB3 Not Selected')

    def get_cb4(self):
        dummy=0 
#        print('CB4 Selected')
        
######################################################
    def get_dynpath(self):                           #
        '''Get DYNAC path'''  
#   !!!  this function currently not in use  !!!     #
######################################################
        global dynpath, default_dfpath, default_ugpath
        if (platform.system() == 'Windows') :
            mylist=subprocess.run(['cmd','/c','cd'], stdout=subprocess.PIPE)
            dynpath=mylist.stdout.decode('utf-8')
            # remove crt
            dynpath=dynpath[:-6] + "bin"
            default_dfpath=dynpath[:-3] + "datafiles"
            default_ugpath=dynpath[:-3] + "help"
        else:
            dynpath=subprocess.getoutput(['pwd'])
            dynpath=dynpath[:-4] + "bin"
            default_dfpath=dynpath[:-3] + "datafiles"
            default_ugpath=dynpath[:-3] + "help"
        myplatform=platform.system()
        systembin="Running on " + platform.system() + " with DYNAC binary in " + dynpath
        return systembin


if __name__ == '__main__':
    app = QtWidgets.QApplication(sys.argv)
    if (platform.system() == 'Darwin') :
        app.setFont(QFont('Helvetica Neue',13))
# next shows icon in tray top of MAC, bottom if linux or windows (if tray at bottom)
#        trayIcon = QtWidgets.QSystemTrayIcon()
#        trayIcon.setIcon(QtGui.QIcon(dynpath.strip()  + os.sep + 'dynicon.png'))
#        trayIcon.setVisible(True)
#        trayIcon.show()
#        app.QSystemTrayIcon(QtGui.QIcon(dynpath.strip()  + os.sep + 'dynicon.png'), app).show()
# next shows icon in dock at bottom     
#    app.setWindowIcon(QtGui.QIcon(dynpath.strip()  + os.sep + 'dynicon.png'))
#    app.setToolTip('DGUI')
#    app.setWindowIconText('DGUI')
    # creating main window
    mw = MainWindow()
# add the icon add the top left of the window    
    mw.setWindowIcon(QtGui.QIcon(dynpath.strip()  + os.sep + 'dynicon.png'))    
    mw.move(10, 20)
    mw.show()
#    sys.exit(app.exec_())
    sys.exit(app.exec())
    
   
    
    
