# DYNAC-download-V7R4
**DYNAC DOWNLOAD PAGE for DYNAC V7R4, a Multiparticle Beam Simulation Code for linacs and beam transport lines.**  
LINUX, MAC and WINDOWS downloads for the simulation code DYNAC V7R4 can be found on this page. Instructions for installation after downloading are in the file: [readme.txt](https://github.com/dynac-source/DYNAC-download/blob/main/readme.txt). This file is also contained in the full package below.

**MODIFICATIONS PERTAINING TO DYNAC V7R4 (28-Dec-2024)**  
Please see the [change log](https://github.com/dynac-source/DYNAC-download/blob/main/modV7R4.txt) for a full list of changes with respect to V7R3.  
DYNAC V7R4 has some minor additions, modifications and corrections compared to the previous revision.


**REQUIRED SOFTWARE FOR DYNAC**  
DYNAC needs to be compiled with gfortran, which is available from the [gfortran web site](http://gcc.gnu.org/wiki/GFortranBinaries). DYNAC has been successfully tested with gfortran/gcc 14.2.0 and older.
GNU Plot (ZIP format) for WINDOWS can be obtained from the [gnuplot web site](http://sourceforge.net/projects/gnuplot/).
Using the same link you can find a GNU Plot download for the MAC (download not always required for LINUX, as gnuplot is standard with some flavors of LINUX). On the MAC, it is suggested to install gnuplot with the wxt terminal (e.g. brew install gnuplot --with-wxmac --with-cairo), as it is considerably faster than the aqua terminal and the xquartz/X11 terminal (as it is used by DGUI). 

DYNAC has been succesfully tested on LINUX (Mint 22.0 and older, Red Hat 4.4.7-17), MAC (Sequoia, Catalina, Mojave and some older) and WINDOWS (11 and older).

**OPEN ISSUES (28-Dec-2024)**  
The charge stripper model requires further development (e.g. energy loss model).

**DYNAC V7R4 (FULL PACKAGE)**  
DYNAC source, data, plot and help files (for WINDOWS, ZIP format) [dynacv7r4_w.zip](https://github.com/dynac-source/DYNAC-download-V7R4/blob/main/dynacv7r4_w.zip)  
DYNAC source, data, plot and help files (for LINUX/MAC, tar/gz format) [dynacv7r4.tar.gz](https://github.com/dynac-source/DYNAC-download-V7R4/blob/main/dynacv7r4.tar.gz)  

*Note: To unzip the linux/mac version, type: tar xvfz dynacv7r4.tar.gz*   

**DYNAC V7R4 (INDIVIDUAL FILES)**  
DYNAC User Guide (PDF format) [dynac_UG.pdf](https://github.com/dynac-source/DYNAC-download-V7R4/blob/main/dynac_UG.pdf)  
DYNAC input file (example) for an electron gun (text format) [egun_example2.in](https://github.com/dynac-source/DYNAC-download/blob/main/egun_example2.in)  
DYNAC input file (example) for describing the field in an electron gun (text format; to be used with the egun example above) [egun_field.txt](https://github.com/dynac-source/DYNAC-download/files/6633699/egun_field.txt)  
DYNAC input file for the SNS H- MEBT (Medium Energy Beam Transport) line and DTL Tank 1 (text format) [sns_mebt_dtl1.in](https://github.com/dynac-source/DYNAC-download/blob/main/sns_mebt_dtl1.in)  
DYNAC source (for WINDOWS, ZIP format) [dynacv7r4_w_source.zip](https://github.com/dynac-source/DYNAC-download-V7R4/blob/main/dynacv7r4_w_source.zip)  
Script to compile the DYNAC source (for WINDOWS, .bat file) [comv7.bat](https://github.com/dynac-source/DYNAC-download/blob/main/comv7.bat)  
DYNAC source file (for LINUX and MAC, tar/gz format) [dynacv7r4_source.tar.gz](https://github.com/dynac-source/DYNAC-download-V7R4/blob/main/dynacv7r4_source.tar.gz)  
Script to compile the DYNAC source (for LINUX and MAC, text format) [comv7](https://github.com/dynac-source/DYNAC-download/blob/main/comv7)  


[dyndat.f90](https://github.com/dynac-source/DYNAC-download-V7R4/blob/main/dyndat_V3R5.f90) (source file (V3R5) in text format) is used for GNUPLOT based plots.  
Script to compile the dyndat source (for WINDOWS, .bat file) [complt.bat](https://github.com/dynac-source/DYNAC-download/blob/main/complt.bat)  
Script to compile the dyndat source (for LINUX and MAC, text format) [complt](https://github.com/dynac-source/DYNAC-download/blob/main/complt)  

*Note: To unzip the linux/mac version of the source, type: tar xvfz dynacv7r4_source.tar.gz*

**OTHER DYNAC UTILITIES**  
[DGUI](https://github.com/dynac-source/DYNAC-download-V7R4/blob/main/README.md#dgui-v2r7-download), a DYNAC Graphical User Interface.  
[ptq2dyn.f](https://github.com/dynac-source/DYNAC-download/blob/main/ptq2dyn.f) : prepares the input data file used by the RFQPTQ card. Source file in text format, compile with:  
*gfortran ptq2dyn.f -o ptq2dyn*  
An alternative to the above mentioned DYNAC GUI has been developed at [MSU](https://github.com/NSCLAlt/DynacGUI).

# DGUI V2R7-download
**DGUI, a DYNAC Graphical User Interface**  
DGUI V2R7 is a Python3 based GUI to DYNAC. Alternatively, the DYNAC code can be exectued from the terminal.  
DGUI has been tested on LINUX (Mint 22 and older), MAC (Sequoia, Catalina and Mojave) and WINDOWS (11, 10) and works best with DYNAC V7R4.
Instructions for installation after downloading are in the [DGUI User Guide](https://github.com/dynac-source/DYNAC-download-V7R4/blob/main/dgui_UG.pdf).  
Modifications pertaining to DGUI V2R7 (28-Dec-2024) are listed in [dgui_modV2R7.txt](https://github.com/dynac-source/DYNAC-download-V7R4/blob/main/dgui_modV2R7.txt). This file also contains the changes in DGUI V2R7 as compared to previous versions.   

Please refer to the DGUI User Guide for download and installation instructions.  
DGUI source (.py) [dgui.py](https://github.com/dynac-source/DYNAC-download-V7R4/blob/main/dgui.py)  
DGUI icon (.png) to be stored in directory dynac/bin [dynicon.png](https://github.com/dynac-source/DYNAC-download/blob/main/dynicon.png)  
DGUI example .ini file for linux and MAC [dgui_example_linmac.ini](https://github.com/dynac-source/DYNAC-download/blob/main/dgui_example_linmac.ini)  
DGUI example .ini file for  Windows [dgui_example_windows.ini](https://github.com/dynac-source/DYNAC-download/blob/main/dgui_example_windows.ini)  
DGUI User Guide (pdf format) [DGUI User Guide](https://github.com/dynac-source/DYNAC-download-V7R4/blob/main/dgui_UG.pdf)  

*Note: The example .ini file needs to be renamed to dgui.ini (see User Guide)*  

**CONTACT**  
Eugene Tanke  
Email: dynacatgithub at gmail.com  

Updated 28-Dec-2024
  
  
