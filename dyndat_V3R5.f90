        PROGRAM dyndat
        implicit real(8) (a-h,o-z)
        character(len=256), dimension(10) :: myarg
        character(len=256) :: command,ppath,inarg,fname,txt
        character(len=256) :: fwpath
        character(len=255), dimension(20) :: pfnm
        character(len=80) :: title
        character(len=40), dimension(20) :: labels
        character(len=40) :: vtext
        character(len=23) :: filenm
        character(len=16) :: termtype
        character(len=7), dimension(20) :: cccst
        character(len=2) :: backslash
        character(len=1) :: sepa
        common/prtcnt/imax
        common/wfil2/title,labels
        common/wfil2l/uxmin,uxmax,uymin,uymax
        common/fichier/filenm,pfnm
        common/fpath/ppath
        common/iopsys/iopsy,termtype,ltt,s2gr
        common/chstat1/cst(1000002),cstat(20),fcstat(20),ncstat,mcstat
        common/chstat2/cccst
        common/pscl/yminsk,iskale
        common/files/fname,lpath
        common/grtyp/igrtyp
        common/gui/dgui
        common/p2d/xxpar(100,100), yypar(100,100), xyar(100,100), &
                   zzpar(100,100), zxar(100,100), zyar(100,100)
        DIMENSION dx(1000002),dxp(1000002),data(10)
        parameter (backslash="\\")
        dimension icstat(20),zstat(20)
        dimension cx(1000002),cxp(1000002),ctrx1(1000002),ctry1(1000002)
        dimension cy(1000002),cyp(1000002),ctrx2(1000002),ctry2(1000002)
        dimension cz(1000002),czp(1000002),ctrx3(1000002),ctry3(1000002)
        common/wfil20/xmin(10),xmax(10),ymin(10),ymax(10)
        common/wfil120/xxpmax,yypmax,xymax,zzpmax,zxmax,zymax,ndx,ndy,bex
        common/mingw/mg
        logical mg,dgui,s2gr
        dimension larg(4),bex(30)
! V1.2  Original released version, compatible with g77
! V2.0  Formatting changed to be compatible with gfortran and with WGNUPLOT gp440win32
! V2.1  Replaced 'dots' with 'points' syntax
! V2.2  Fixed compiler warnings related to array sizing of ctrx1,2,3 and ctry1,2,3
! V2.3  Mods to add MAC as a valid operating system for gnuplot
! V2.4  Allow for non-integer charge states
! V2.5  Allow for longer file names under the save option
!       Made dyndat compatible with MINGW gfortran in view of different result for ctime
!       function than standard gfortran
!       Fixed bug related to MAC: on a MAC 02 Apr is shown as 2 Apr (on windows as
!       02 Apr); fill the blank with the character '0' to avoid error message on file
!       copy
!       Fix color of points used in particle plots for MAC (changed from grey to black)
! V2.6  Change from 100k to 250k macro particles
! V2.7  Various changes related to changes in the names of certain GNUPLOT parameters; these
!       changes in dyndat.f were needed to for instance avoid that the dots in the
!       particle distributions would no longer be plotted.
! V2.8  Also read plotfile path; this was introduced to be compatible with the DYNAC GUI
!       Allow to read the terminal type for MAC
! V2.9  Change maximum number of macro particles to 1000000. Add code that allows for
!       interfacing with DGUI (-dg option).
! V2.10 For QT terminal, line colors in gnuplot are platform dependent. Fixed this for
!       xx'-yy'-xy-zz' an xz-yz plots
! V2.11 Added 2-D plots for xx'-yy'-xy-zz' an xz-yz.
! V3.0  Source code changed from .f to .f90; updated deprecated gnuplot syntax. Use
!       wxt as default terminal for all 3 operating systems.
! V3.1  Slightly larger windows for Windows OS. Changed to full path (as opposed to assuming
!       local directory usage) for all 3 operating systems. As a consequence, when this
!       version is started by dgui, it will write the gnuplot files to the directory that
!       was selected in dgui. One can now save density plots.
! V3.2  Added the possibility of saving to png, jpeg or gif files. This is achieved by setting
!       the -tt option with any of these 3 gnuplot terminal types.
! V3R3  Align version/revision numbering scheme to the one of DYNAC. Charge states are now 
!       printed with more significant digits.
! V3R4  Fixed issue with vertical position of titles for some X11 plots 
! V3R5  Updated to be compatible with gnuplot v6.0, e.g. -noraise option no longer valid
!
!
! if mg=.true., use MINGW on windows, which has a different result for ctime function than
! standard gfortran
! default is mg=.false.
        mg=.false.
! if plotit is called by dgui, then create all plots in separate windows
        dgui=.false.
! s2gr will be set to .true. if the terminal type is png, jpeg or gif
        s2gr=.false.        
        ppath=''
        lpath=0
        llpath=0
        command=''
        fname=''
        termtype=''
        sepa=''
        narg=0
        vtext='PLOTIT V3R5 28-Dec-2024'
        DO
          call get_command_argument(narg, inarg, length, istat)
          larg(narg+1)=LEN_TRIM(inarg)
          if(larg(narg+1).eq.0) exit
          narg=narg+1
          myarg(narg)=TRIM(inarg)
        ENDDO
!        write(6,*) 'Lengths',larg(2),larg(3)
        iopsy=0
        do i=2,narg
          txt=myarg(i)
          if(txt(1:1).ne.'-') then
! the input argument is the type of operating system
! iopsy=1 --> LINUX   GNUPLOT version
            if(txt(1:1).eq.'L' .or. txt(1:1).eq.'l') iopsy=1
! iopsy=2 --> WINDOWS GNUPLOT version
            if(txt(1:1).eq.'W' .or. txt(1:1).eq.'w') then
              iopsy=2
              if(txt(2:3).eq.'MG' .or. txt(2:3).eq.'mg') mg=.true.
            endif
! iopsy=3 --> MAC     GNUPLOT version
            if(txt(1:1).eq.'M' .or. txt(1:1).eq.'m') iopsy=3
          else
            if(txt(1:3).eq.'-tt') then
              termtype=''
              termtype(1:larg(i)-3)=txt(4:larg(i))
              ltt=LEN_TRIM(termtype)
            endif
            if(txt(1:2).eq.'-p') then
              ppath(1:larg(i)-2)=txt(3:larg(i))
              lpm=LEN(ppath)
              do k=lpm,1,-1
                if(ppath(k:k).eq.'\' .or. ppath(k:k).eq.'/') then
                  lpath=k
                  sepa=ppath(k:k)
                  exit
                endif
              enddo
              llpath=LEN_TRIM(ppath)
            endif
            if(txt(1:3).eq.'-dg') then
              dgui=.true.
              termtype=''
              if(iopsy.eq.1) termtype='x11'
!              if(iopsy.eq.2) termtype='windows'
              if(iopsy.eq.2) termtype='qt'
              if(iopsy.eq.3) termtype='x11'
              ltt=LEN_TRIM(termtype)
            endif
            if(txt(1:2).eq.'-h') then
!     print out of help message, starting with PLOTIT version
              write(6,'(A)') vtext
              write(6,*)'Command format:'
              write(6,*)'dynplt [-h] X [-tt] [-p] [-dg]'
              write(6,*)'where X is the operating system, which ', &
                        'needs to be one of the following 3:'
              write(6,'(a)') '   L or l for LINUX'
              write(6,'(a)') '   M or m for MAC'
              write(6,'(a)') '   W or w for WINDOWS'
              write(6,*)'Optional arguments:'
              write(6,*)'-h will list the argument options (this list)'
              write(6,*)'-p can be used to specify the datafile path. ', &
                        'There should be no '
              write(6,*)'   space between -p and the path. This ', &
                        'option is used by the'
              write(6,*)'   DYNAC GUI.'
              write(6,*)'-tt can be used to specify the GNUPLOT ', &
                        'terminal type, which defaults to:'
              write(6,*)'   wxt for LINUX'
              write(6,*)'   wxt for MAC'
              write(6,*)'   wxt for WINDOWS'
              stop
            endif
            if(txt(1:2).eq.'-v') then
!     print out of PLOTIT version
              write(6,'(A)') vtext
              stop
            endif            
          endif
        enddo
! iopsy=1 --> LINUX   GNUPLOT version
! iopsy=2 --> WINDOWS GNUPLOT version
! iopsy=3 --> MAC     GNUPLOT version
        write(6,'(A)') vtext
        if(iopsy.eq.1) then
          write(6,'(A)') 'PLOTIT for LINUX'
          if(ltt.eq.0) then
            write(6,*)'GNUPLOT TERMINAL TYPE NOT DEFINED'
            termtype='wxt'
            write(6,*)'USING TERMINAL TYPE ',termtype
          else
            write(6,'(A,A)')'GNUPLOT TERMINAL TYPE ',termtype
          endif
        elseif(iopsy.eq.2) then
          write(6,'(A)') 'PLOTIT for WINDOWS'
          if(ltt.eq.0) then
            write(6,*)'GNUPLOT TERMINAL TYPE NOT DEFINED'
            termtype='wxt'
            write(6,*)'USING TERMINAL TYPE ',termtype
          else
            write(6,'(A,A)')'GNUPLOT TERMINAL TYPE ',termtype
          endif
        elseif(iopsy.eq.3) then
          write(6,'(A)') 'PLOTIT for MAC'
          if(ltt.eq.0) then
            write(6,*)'GNUPLOT TERMINAL TYPE NOT DEFINED'
            termtype='wxt'
            write(6,*)'USING TERMINAL TYPE ',termtype
          else
            write(6,'(A,A)')'GNUPLOT TERMINAL TYPE ',termtype
          endif
        else
! iopsy=0 --> ERROR: system not found
          write(6,'(a)') 'Error in operating system type entry'
          write(6,'(a)') 'Operating system type required'
          write(6,'(a)') '  for LINUX   GNUPLOT use L or l'
          write(6,'(a)') '  for MAC     GNUPLOT use M or m'
          write(6,'(a)') '  for WINDOWS GNUPLOT use W or w'
          write(6,'(a)') 'Type'
          write(6,'(a)') 'plotit -h'
          write(6,'(a)') 'for help.'
          stop
        endif
        ltt=LEN_TRIM(termtype)
        s2gr=.false.
        if(termtype(1:ltt).eq.'gif' .or. termtype(1:ltt).eq.'GIF' &
          .or. termtype(1:ltt).eq.'png' .or. termtype(1:ltt).eq.'PNG' &
          .or. termtype(1:ltt).eq.'jpeg' .or. termtype(1:ltt).eq.'JPEG')  s2gr=.true.
        if(lpath.ne.0) then
!          write(6,*) 'PlotpathL=',llpath,ppath(1:llpath)
          if(iopsy.ne.2) then
            if(ppath(llpath:llpath).eq.'"') llpath=llpath-1
            if(ppath(llpath:llpath).eq.'/' .or.  &
               ppath(llpath:llpath).eq.backslash) then
              fname=ppath(1:llpath)
            else
              llpath=llpath+1
              ppath(llpath:llpath)=sepa
              fname=ppath(1:llpath)
            endif
            lpath=llpath
!            write(6,*) 'PlotpathS=',llpath,ppath(1:llpath)
          else
!            write(6,*) 'PlotpathW=',llpath,ppath(1:llpath)
            fname=ppath(1:llpath)
            if(ppath(llpath:llpath).eq.'"') then
              ppath(llpath:llpath)=backslash
              fname=ppath(1:llpath)
!              write(6,*) 'PlotpathW2=',llpath,ppath(1:llpath)
            endif
            lpath=llpath
          endif
        else
          ppath=''
        endif
        write(6,*)
! igrtyp is type of graph (there is no igrtyp=8,9,10,13,14,15,20,21,23,24,25,26)
! single charge state, no zones:
!        igrtyp=1  for xx'-yy'-xy-zz' plots
!        igrtyp=2  for zx-zy plots & profiles
!        igrtyp=3  for xz-yz envelopes
!        igrtyp=4  for dW envelope
!        igrtyp=5  for dPHI envelope
! multi charge state, no zones:
!        igrtyp=6  for xx'-yy'-xy-zz' plots for multi-charge state beam
!        igrtyp=7  for zx-zy plots & profiles for multi-charge state beam
!
! with zones, single charge state:
!        igrtyp=11 for xx'-yy'-xy-zz' plots with ZONES card
!        igrtyp=12 for zx-zy plots & profiles with ZONES card
!
!        igrtyp=16 for xx'-yy'-xy-zz' density plots
!        igrtyp=18 for zx-zy density plots & profiles
!
!        igrtyp=17 or 19 or 22 or 27 log scale in bunch profiles
        ncstat=1
        data(1)=0.
        do i=1,20
          pfnm(i)=''
        enddo
        fname(lpath+1:lpath+11)='dynac01.plt'
        pfnm(1)=fname(lpath+1:lpath+11)
        fname(lpath+1:lpath+11)='dynac02.plt'
        pfnm(2)=fname(lpath+1:lpath+11)
        fname(lpath+1:lpath+11)='dynac03.plt'
        pfnm(3)=fname(lpath+1:lpath+11)
        fname(lpath+1:lpath+11)='dynac04.plt'
        pfnm(4)=fname(lpath+1:lpath+11)
        fname(lpath+1:lpath+11)='dynac05.plt'
        pfnm(5)=fname(lpath+1:lpath+11)
        fname(lpath+1:lpath+11)='dynac06.plt'
        pfnm(6)=fname(lpath+1:lpath+11)
        fname(lpath+1:lpath+11)='dynac07.plt'
        pfnm(7)=fname(lpath+1:lpath+11)
        fname(lpath+1:lpath+11)='dynac08.plt'
        pfnm(8)=fname(lpath+1:lpath+11)
        fname(lpath+1:lpath+11)='dynac09.plt'
        pfnm(9)=fname(lpath+1:lpath+11)
        fname(lpath+1:lpath+11)='dynac10.plt'
        pfnm(10)=fname(lpath+1:lpath+11)
        fname(lpath+1:lpath+11)='dynac11.plt'
        pfnm(11)=fname(lpath+1:lpath+11)
        fname(lpath+1:lpath+11)='dynac12.plt'
        pfnm(12)=fname(lpath+1:lpath+11)
        fname(lpath+1:lpath+11)='dynac13.plt'
        pfnm(13)=fname(lpath+1:lpath+11)
        fname(lpath+1:lpath+11)='dynac14.plt'
        pfnm(14)=fname(lpath+1:lpath+11)
        fname(lpath+1:lpath+11)='dynac15.plt'
        pfnm(15)=fname(lpath+1:lpath+11)
        fname(lpath+1:lpath+11)='dynac16.plt'
        pfnm(16)=fname(lpath+1:lpath+11)
        fname(lpath+1:lpath+11)='dynac17.plt'
        pfnm(17)=fname(lpath+1:lpath+11)
        fname(lpath+1:lpath+11)='dynac18.plt'
        pfnm(18)=fname(lpath+1:lpath+11)
        fname(lpath+1:lpath+11)='dynac19.plt'
        pfnm(19)=fname(lpath+1:lpath+11)
        fname(lpath+1:lpath+11)='dynac20.plt'
        pfnm(20)=fname(lpath+1:lpath+11)
        fname(lpath+1:lpath+11)=''
        if(iopsy.eq.1 .or. iopsy.eq.3) then
! LINUX or MAC
!          command(1:40)="test ! -e savedplots && mkdir savedplots"
          if(llpath.eq.0)then
            command="test ! -e savedplots && mkdir savedplots"
            CALL System(trim(command))
          else
            command="test ! -e "//ppath(1:llpath)//"savedplots && mkdir "//ppath(1:llpath)//"savedplots"
            CALL System(trim(command))
          endif
        else
! WINDOWS
          if(llpath.eq.0)then
            command="if not exist savedplots\\*.* echo creating plots directory"
            CALL System(trim(COMMAND))
            command="if not exist savedplots\\*.* mkdir savedplots"
            CALL System(trim(COMMAND))
            command="if not exist savedplots\\*.* copy ..\\bin\\tst savedplots"
            CALL System(trim(COMMAND))
          else
            command="if not exist "//ppath(1:llpath)//"savedplots\*.* "//"echo creating plots directory" 
            lcmd=len_trim(command)          
            CALL System(COMMAND(1:lcmd))
            command="if not exist "//ppath(1:llpath)//"savedplots\*.* "//"mkdir "//ppath(1:llpath)//"savedplots" 
            lcmd=len_trim(command)          
            CALL System(COMMAND(1:lcmd))
          endif
        endif
        command=""
        fprec=epsilon(data(1))
        IF (abs(data(1)).le.fprec) THEN
          fname(lpath+1:lpath+9)='emit.plot'
          lfnam=len_trim(fname)
          OPEN(unit=66,file=fname)
          data(1)=1.
          data(2)=0.
        ENDIF
10      READ(66,*,end=20) igrtyp
        iskale=0
        IF(igrtyp.eq.17 .or. igrtyp.eq.22 .or. igrtyp.eq.27)THEN
! log scale in bunch profiles
          igrtyp=igrtyp-15
          iskale=1
          read(66,*) yminsk
        ELSEIF(igrtyp.eq.19)THEN
! also log scale in bunch profiles
          igrtyp=igrtyp-1
          iskale=1
          read(66,*) yminsk
        ENDIF
        IF (igrtyp.eq.1 .or. igrtyp.eq.6 .or. igrtyp.eq.11) THEN
! x-x', y-y', x-y, z-z' plots
          data(2)=data(2)+1.
          data(5)=igrtyp
          nplot=int(data(2))
          if (igrtyp.eq.6) then
            read(66,*) ncstat
            read(66,*) (cstat(j),j=1,ncstat)
            do j=1,ncstat
!              icstat(j)=int(cstat(j))
!              write(command(2:5),'(I2)') icstat(j)
              write(command(2:8),'(f7.3)') cstat(j)
              command(1:1)=' '
              cccst(j)=command(1:8)
            enddo
          endif
          if (igrtyp.eq.11) then
            read(66,*) ncstat
            read(66,*) (zstat(j),j=1,ncstat)
            do j=1,ncstat
              cstat(j)=zstat(j)
              write(command(1:4),'(F4.2)') zstat(j)
!              command(1:1)=' '
              cccst(j)=command(1:4)
            enddo
          endif
          READ(66,'(a80)') title(1:80)
          READ(66,*) uxmin,uxmax,uymin,uymax
          xmin(1)=uxmin
          xmax(1)=uxmax
          ymin(1)=uymin
          ymax(1)=uymax
          DO i=1,201
            READ(66,*) dx(i),dxp(i)
            ctrx1(i)=dx(i)
            ctry1(i)=dxp(i)
          ENDDO
          READ(66,*) imax
          if (igrtyp.eq.1) then
            DO i=1,imax
              READ(66,*) dx(i),dxp(i)
              cx(i)=dx(i)
              cxp(i)=dxp(i)
            ENDDO
          else
            DO i=1,imax
              READ(66,*) dx(i),dxp(i),cst(i)
              cx(i)=dx(i)
              cxp(i)=dxp(i)
            ENDDO
          endif
          labels(1)='x (cm)'
          labels(2)='xp (mrad)'
          READ(66,*) uxmin,uxmax,uymin,uymax
          xmin(2)=uxmin
          xmax(2)=uxmax
          ymin(2)=uymin
          ymax(2)=uymax
          DO i=1,201
            READ(66,*) dx(i),dxp(i)
            ctrx2(i)=dx(i)
            ctry2(i)=dxp(i)
          ENDDO
          READ(66,*) imax
          if (igrtyp.eq.1) then
            DO i=1,imax
              READ(66,*) dx(i),dxp(i)
              cy(i)=dx(i)
              cyp(i)=dxp(i)
            ENDDO
          else
            DO i=1,imax
              READ(66,*) dx(i),dxp(i),cst(i)
              cy(i)=dx(i)
              cyp(i)=dxp(i)
            ENDDO
          endif
          labels(3)='y (cm)'
          labels(4)='yp (mrad)'
          READ(66,*) uxmin,uxmax,uymin,uymax
          xmin(3)=uxmin
          xmax(3)=uxmax
          ymin(3)=uymin
          ymax(3)=uymax
          labels(5)='x (cm)'
          labels(6)='y (cm)'
          READ(66,*) uxmin,uxmax,uymin,uymax
          xmin(4)=uxmin
          xmax(4)=uxmax
          ymin(4)=uymin
          ymax(4)=uymax
          DO i=1,201
            READ(66,*) dx(i),dxp(i)
            ctrx3(i)=dx(i)
            ctry3(i)=dxp(i)
          ENDDO
          READ(66,*) imax
          if (igrtyp.eq.1) then
            DO i=1,imax
              READ(66,*) dx(i),dxp(i)
              cz(i)=dx(i)
              czp(i)=dxp(i)
            ENDDO
          else
            DO i=1,imax
              READ(66,*) dx(i),dxp(i),cst(i)
              cz(i)=dx(i)
              czp(i)=dxp(i)
            ENDDO
          endif
          labels(7)='z (deg)'
          labels(8)='zp (MeV)'
          call wfile10(0,imax,cx,cxp,cy,cyp,cz,czp)
          call wfile10(1,imax,ctrx1,ctry1,ctrx2,ctry2,ctrx3,ctry3)
          isave=0
          call wfile20(isave,nplot)
          if(iopsy.eq.1) then
! LINUX
            command=''
            !command="gnuplot -noraise -geometry 500x515-250+25 "//trim(ppath)//"dynac.gnu"
            command="gnuplot "//trim(ppath)//"dynac.gnu"
            CALL System(trim(command))
          elseif(iopsy.eq.3) then
! MAC
            command=''
            command="gnuplot "//trim(ppath)//"dynac.gnu"
            CALL System(trim(command))
          else
! WINDOWS
            command=''
            if(dgui) then
              command='wgnuplot "'//trim(ppath)//'dynac.gnu"'
            else  
              command="wgnuplot "//trim(ppath)//"dynac.gnu"
            endif  
            CALL System(trim(command))
          endif
        ENDIF
        IF (igrtyp.eq.2 .or. igrtyp.eq.7 .or. igrtyp.eq.12) THEN
! z-x, z-y distribution plots & profiles
          data(5)=igrtyp
          data(2)=data(2)+1.
          nplot=int(data(2))
          if (igrtyp.eq.7) then
            read(66,*) ncstat
            read(66,*) (cstat(j),j=1,ncstat)
            do j=1,ncstat
!              icstat(j)=int(cstat(j))
!              write(command(2:5),'(I2)') icstat(j)
              write(command(2:8),'(F7.3)') cstat(j)
              command(1:1)=' '
              cccst(j)=command(1:8)
            enddo
          endif
          if (igrtyp.eq.12) then
            read(66,*) ncstat
            read(66,*) (zstat(j),j=1,ncstat)
            do j=1,ncstat
              cstat(j)=zstat(j)
              write(command(1:4),'(f4.2)') zstat(j)
              cccst(j)=command(1:4)
            enddo
          endif
          READ(66,'(a80)') title(1:80)
          READ(66,*) uxmin,uxmax,uymin,uymax
          xmin(1)=uxmin
          xmax(1)=uxmax
          ymin(1)=uymin
          ymax(1)=uymax
          READ(66,*) imax
          if (igrtyp.eq.2) then
            DO i=1,imax
              READ(66,*) dx(i),dxp(i)
              cx(i)=dx(i)
              cxp(i)=dxp(i)
            ENDDO
          else
            DO i=1,imax
              READ(66,*) dx(i),dxp(i),cst(i)
              cx(i)=dx(i)
              cxp(i)=dxp(i)
            ENDDO
          endif
          labels(1)='z (cm)'
          labels(2)='x (cm)'
          READ(66,*) uxmin,uxmax,uymin,uymax
          xmin(2)=uxmin
          xmax(2)=uxmax
          ymin(2)=uymin
          ymax(2)=uymax
          READ(66,*) imax
          if (igrtyp.eq.2) then
            DO i=1,imax
              READ(66,*) dx(i),dxp(i)
              cy(i)=dx(i)
              cyp(i)=dxp(i)
            ENDDO
          else
            DO i=1,imax
              READ(66,*) dx(i),dxp(i),cst(i)
              cy(i)=dx(i)
              cyp(i)=dxp(i)
            ENDDO
          endif
          labels(3)='z (cm)'
          labels(4)='y (cm)'
          call wfile11(imax,cx,cxp,cy,cyp)
          isave=0
          call wfile21(isave,nplot)
! profiles
          fwpath=''
          fwpath=trim(ppath)//"dynac01.pro"
          OPEN(unit=70,file=trim(fwpath))
          fwpath=trim(ppath)//"dynac02.pro"
          OPEN(unit=71,file=trim(fwpath))
          fwpath=trim(ppath)//"dynac03.pro"
          OPEN(unit=72,file=trim(fwpath))
          fwpath=trim(ppath)//"dynac04.pro"
          OPEN(unit=73,file=trim(fwpath))
          fwpath=trim(ppath)//"dynac05.pro"
          OPEN(unit=74,file=trim(fwpath))
          fwpath=trim(ppath)//"dynac06.pro"
          OPEN(unit=75,file=trim(fwpath))
          rewind(unit=70)
          rewind(unit=71)
          rewind(unit=72)
          rewind(unit=73)
          rewind(unit=74)
          rewind(unit=75)
          READ(66,*) imx
          DO i=1,imx
             READ(66,*) dx(i),dxp(i)
             if (abs(dxp(i)).le.fprec) dxp(i)=1.e-8
             write(70,*)dx(i),dxp(i)
          ENDDO
          READ(66,*) imy
          DO i=1,imy
             READ(66,*) dx(i),dxp(i)
             if (abs(dxp(i)).le.fprec) dxp(i)=1.e-8
             write(71,*)dx(i),dxp(i)
          ENDDO
          READ(66,*) imz
          DO i=1,imz
             READ(66,*) dx(i),dxp(i)
             if (abs(dxp(i)).le.fprec) dxp(i)=1.e-8
             write(72,*)dx(i),dxp(i)
          ENDDO
          READ(66,*) imxp
          DO i=1,imxp
             READ(66,*) dx(i),dxp(i)
             if (abs(dxp(i)).le.fprec) dxp(i)=1.e-8
             write(73,*)dx(i),dxp(i)
          ENDDO
          READ(66,*) imyp
          DO i=1,imyp
             READ(66,*) dx(i),dxp(i)
             if (abs(dxp(i)).le.fprec) dxp(i)=1.e-8
             write(74,*)dx(i),dxp(i)
          ENDDO
          READ(66,*) imzp
          DO i=1,imzp
             READ(66,*) dx(i),dxp(i)
             if (abs(dxp(i)).le.fprec) dxp(i)=1.e-8
             write(75,*)dx(i),dxp(i)
          ENDDO
          close(70)
          close(71)
          close(72)
          close(73)
          close(74)
          close(75)
          if(iopsy.eq.1) then
! LINUX
            command=''
            !command="gnuplot -noraise -geometry 500x515-250+25 "//trim(ppath)//"dynac.gnu"
            command="gnuplot "//trim(ppath)//"dynac.gnu"
            CALL System(trim(command))
          elseif(iopsy.eq.3) then
! MAC
            command=''
            command="gnuplot "//trim(ppath)//"dynac.gnu"
            CALL System(trim(command))
          else
! WINDOWS
            command=''
            if(dgui) then
              command='wgnuplot "'//trim(ppath)//'dynac.gnu"'
            else  
              command="wgnuplot "//trim(ppath)//"dynac.gnu"
            endif  
            CALL System(trim(command))
          endif
        ENDIF
        IF (igrtyp.eq.18) THEN
! z-x, z-y density plots & profiles
          data(5)=igrtyp
          data(2)=data(2)+1.
          nplot=int(data(2))
          READ(66,'(a80)') title(1:80)
          READ(66,*) uxmin,uxmax,uymin,uymax
          READ(66,*) bex(17),bex(18),bex(19),bex(20)
          xmin(1)=uxmin
          xmax(1)=uxmax
          ymin(1)=uymin
          ymax(1)=uymax
          READ(66,*) imax,ndx,ndy
          DO i=1,ndx
            DO j=1,ndy
              READ(66,*) zxar(i,j)
            ENDDO
          ENDDO
          labels(1)='z (cm)'
          labels(2)='x (cm)'
          READ(66,*) zxmax
          READ(66,*) uxmin,uxmax,uymin,uymax
          READ(66,*) bex(21),bex(22),bex(23),bex(24)
          xmin(2)=uxmin
          xmax(2)=uxmax
          ymin(2)=uymin
          ymax(2)=uymax
          DO i=1,ndx
            DO j=1,ndy
              READ(66,*) zyar(i,j)
            ENDDO
          ENDDO
          READ(66,*) zymax
          labels(3)='z (cm)'
          labels(4)='y (cm)'
          call wfile111(ndx,ndy)
          isave=0
          call wfile121(isave,nplot)
! profiles
          fwpath=''
          fwpath=trim(ppath)//"dynac01.pro"
          OPEN(unit=70,file=trim(fwpath))
          fwpath=trim(ppath)//"dynac02.pro"
          OPEN(unit=71,file=trim(fwpath))
          fwpath=trim(ppath)//"dynac03.pro"
          OPEN(unit=72,file=trim(fwpath))
          fwpath=trim(ppath)//"dynac04.pro"
          OPEN(unit=73,file=trim(fwpath))
          fwpath=trim(ppath)//"dynac05.pro"
          OPEN(unit=74,file=trim(fwpath))
          fwpath=trim(ppath)//"dynac06.pro"
          OPEN(unit=75,file=trim(fwpath))
          rewind(unit=70)
          rewind(unit=71)
          rewind(unit=72)
          rewind(unit=73)
          rewind(unit=74)
          rewind(unit=75)
          READ(66,*) imx
          DO i=1,imx
             READ(66,*) dx(i),dxp(i)
             if (abs(dxp(i)).le.fprec) dxp(i)=1.e-8
             write(70,*)dx(i),dxp(i)
          ENDDO
          READ(66,*) imy
          DO i=1,imy
             READ(66,*) dx(i),dxp(i)
             if (abs(dxp(i)).le.fprec) dxp(i)=1.e-8
             write(71,*)dx(i),dxp(i)
          ENDDO
          READ(66,*) imz
          DO i=1,imz
             READ(66,*) dx(i),dxp(i)
             if (abs(dxp(i)).le.fprec) dxp(i)=1.e-8
             write(72,*)dx(i),dxp(i)
          ENDDO
          READ(66,*) imxp
          DO i=1,imxp
             READ(66,*) dx(i),dxp(i)
             if (abs(dxp(i)).le.fprec) dxp(i)=1.e-8
             write(73,*)dx(i),dxp(i)
          ENDDO
          READ(66,*) imyp
          DO i=1,imyp
             READ(66,*) dx(i),dxp(i)
             if (abs(dxp(i)).le.fprec) dxp(i)=1.e-8
             write(74,*)dx(i),dxp(i)
          ENDDO
          READ(66,*) imzp
          DO i=1,imzp
             READ(66,*) dx(i),dxp(i)
             if (abs(dxp(i)).le.fprec) dxp(i)=1.e-8
             write(75,*)dx(i),dxp(i)
          ENDDO
          close(70)
          close(71)
          close(72)
          close(73)
          close(74)
          close(75)
          if(iopsy.eq.1) then
! LINUX
            command=''
            !command="gnuplot -noraise -geometry 500x515-250+25 "//trim(ppath)//"dynac.gnu"
            command="gnuplot "//trim(ppath)//"dynac.gnu"
            CALL System(trim(command))
          elseif(iopsy.eq.3) then
! MAC
            command=''
            command="gnuplot "//trim(ppath)//"dynac.gnu"
            CALL System(trim(command))
          else
! WINDOWS
            command=''
            if(dgui) then
              command='wgnuplot "'//trim(ppath)//'dynac.gnu"'
            else  
              command="wgnuplot "//trim(ppath)//"dynac.gnu"
            endif  
            CALL System(trim(command))
          endif
        ENDIF
        IF (igrtyp.eq.16) THEN
! x-x', y-y', x-y, z-z' 2D density plots
          data(2)=data(2)+1.
          data(5)=igrtyp
          nplot=int(data(2))
          READ(66,'(a80)') title(1:80)
          READ(66,*) uxmin,uxmax,uymin,uymax
          READ(66,*) bex(1),bex(2),bex(3),bex(4)
          xmin(1)=uxmin
          xmax(1)=uxmax
          ymin(1)=uymin
          ymax(1)=uymax
          READ(66,*) imax,ndx,ndy
          DO i=1,ndx
            DO j=1,ndy
              READ(66,*) xxpar(i,j)
            ENDDO
          ENDDO
          labels(1)='x (cm)'
          labels(2)='xp (mrad)'
          READ(66,*) xxpmax
!
          READ(66,*) uxmin,uxmax,uymin,uymax
          READ(66,*) bex(5),bex(6),bex(7),bex(8)
          xmin(2)=uxmin
          xmax(2)=uxmax
          ymin(2)=uymin
          ymax(2)=uymax
          DO i=1,ndx
            DO j=1,ndy
              READ(66,*) yypar(i,j)
            ENDDO
          ENDDO
          labels(3)='y (cm)'
          labels(4)='yp (mrad)'
          READ(66,*) yypmax
!
          READ(66,*) uxmin,uxmax,uymin,uymax
          READ(66,*) bex(9),bex(10),bex(11),bex(12)
          xmin(3)=uxmin
          xmax(3)=uxmax
          ymin(3)=uymin
          ymax(3)=uymax
          DO i=1,ndx
            DO j=1,ndy
              READ(66,*) xyar(i,j)
            ENDDO
          ENDDO
          labels(5)='x (cm)'
          labels(6)='y (cm)'
          READ(66,*) xymax
!
          READ(66,*) uxmin,uxmax,uymin,uymax
          READ(66,*) bex(13),bex(14),bex(15),bex(16)
          xmin(4)=uxmin
          xmax(4)=uxmax
          ymin(4)=uymin
          ymax(4)=uymax
          DO i=1,ndx
            DO j=1,ndy
              READ(66,*) zzpar(i,j)
            ENDDO
          ENDDO
          labels(7)='z (deg)'
          labels(8)='zp (MeV)'
          READ(66,*) zzpmax
! Write the histogrammed data which will be plotted by GNU to a file          
          call wfile110(ndx,ndy)
          isave=0
! Write the .gnu gnuplot file for the xx'-yy'-xy-zz' 2D plot          
          call wfile120(isave,nplot)
          if(iopsy.eq.1) then
! LINUX
            command=''
            !command="gnuplot -noraise -geometry 500x515-250+25 "//trim(ppath)//"dynac.gnu"
            command="gnuplot "//trim(ppath)//"dynac.gnu"
            CALL System(trim(command))
          elseif(iopsy.eq.3) then
! MAC
            command=''
            command="gnuplot "//trim(ppath)//"dynac.gnu"
            CALL System(trim(command))
          else
! WINDOWS
            command=''
            if(dgui) then
              command='wgnuplot "'//trim(ppath)//'dynac.gnu"'
            else  
              command="wgnuplot "//trim(ppath)//"dynac.gnu"
            endif  
            CALL System(trim(command))
          endif
        ENDIF
        IF (igrtyp.eq.3) THEN
! x,y envelopes as f(z)
          data(5)=igrtyp
          data(2)=data(2)+1.
          nplot=int(data(2))
          READ(66,'(a80)') title(1:80)
          READ(66,*) uxmin,uxmax,uymin,uymax
          xmin(1)=uxmin
          xmax(1)=uxmax
          ymin(1)=uymin
          ymax(1)=uymax
          READ(66,*) imax
          DO i=1,imax
            READ(66,*) dx(i),dxp(i)
            cx(i)=dx(i)
            cxp(i)=dxp(i)
          ENDDO
          READ(66,*) imax
          DO i=1,imax
            READ(66,*) dx(i),dxp(i)
            cy(i)=dx(i)
            cyp(i)=dxp(i)
          ENDDO
          labels(1)='z (m)'
          labels(2)='x,y (cm)'
          call wfile3(imax,cx,cxp,cy,cyp)
          isave=0
          call wfile2(isave,2,nplot)
          if(iopsy.eq.1) then
! LINUX
            command=''
            !command="gnuplot -noraise -geometry 500x515-250+25 "//trim(ppath)//"dynac.gnu"
            command="gnuplot "//trim(ppath)//"dynac.gnu"
            CALL System(trim(command))
          elseif(iopsy.eq.3) then
! MAC
            command=''
            command="gnuplot "//trim(ppath)//"dynac.gnu"
            CALL System(trim(command))
          else
! WINDOWS
            command=''
            if(dgui) then
              command='wgnuplot "'//trim(ppath)//'dynac.gnu"'
            else  
              command="wgnuplot "//trim(ppath)//"dynac.gnu"
            endif  
            CALL System(trim(command))
          endif
        ENDIF
        IF (igrtyp.eq.4) THEN
! dW/W envelope as f(z)
          data(5)=igrtyp
          data(2)=data(2)+1.
          nplot=int(data(2))
          READ(66,'(a80)') title(1:80)
          READ(66,*) uxmin,uxmax,uymin,uymax
          xmin(1)=uxmin
          xmax(1)=uxmax
          ymin(1)=uymin
          ymax(1)=uymax
          READ(66,*) imax
          DO i=1,imax
            READ(66,*) dx(i),dxp(i)
            cx(i)=dx(i)
            cxp(i)=dxp(i)
          ENDDO
          labels(1)='z (m)'
          labels(2)='dW/W (per mille)'
          call wfile1(imax,cx,cxp,cy,cyp)
          isave=0
          call wfile2(isave,3,nplot)
          if(iopsy.eq.1) then
! LINUX
            command=''
            !command="gnuplot -noraise -geometry 500x515-250+25 "//trim(ppath)//"dynac.gnu"
            command="gnuplot "//trim(ppath)//"dynac.gnu"
            CALL System(trim(command))
          elseif(iopsy.eq.3) then
! MAC
            command=''
            command="gnuplot "//trim(ppath)//"dynac.gnu"
            CALL System(trim(command))
          else
! WINDOWS
            command=''
            if(dgui) then
              command='wgnuplot "'//trim(ppath)//'dynac.gnu"'
            else  
              command="wgnuplot "//trim(ppath)//"dynac.gnu"
            endif  
            CALL System(trim(command))
          endif
        ENDIF
        IF (igrtyp.eq.5) THEN
! dPHI envelope as f(z)
          data(5)=igrtyp
          data(2)=data(2)+1.
          nplot=int(data(2))
          READ(66,'(a80)') title(1:80)
          READ(66,*) uxmin,uxmax,uymin,uymax
          xmin(1)=uxmin
          xmax(1)=uxmax
          ymin(1)=uymin
          ymax(1)=uymax
          READ(66,*) imax
          DO i=1,imax
            READ(66,*) dx(i),dxp(i)
            cx(i)=dx(i)
            cxp(i)=dxp(i)
          ENDDO
          labels(1)='z (m)'
          labels(2)='dPHI (deg)'
          call wfile1(imax,cx,cxp,cy,cyp)
          isave=0
          call wfile2(isave,4,nplot)
          if(iopsy.eq.1) then
! LINUX
            command=''
            !command="gnuplot -noraise -geometry 500x515-250+25 "//trim(ppath)//"dynac.gnu"
            command="gnuplot "//trim(ppath)//"dynac.gnu"
            CALL System(trim(command))
          elseif(iopsy.eq.3) then
! MAC
            command=''
            command="gnuplot "//trim(ppath)//"dynac.gnu"
            CALL System(trim(command))
          else
! WINDOWS
            command=''
            if(dgui) then
              command='wgnuplot "'//trim(ppath)//'dynac.gnu"'
            else  
              command="wgnuplot "//trim(ppath)//"dynac.gnu"
            endif  
            CALL System(trim(command))
          endif
        ENDIF
! ask if the file should be saved when plotit is called from the terminal,
! but not if the terminal type is gif, jpeg or png
        if(.not. s2gr) then
          if(.not. dgui) call savefile
        endif  
        write(6,'(A,i3,A)') 'Plot ',int(data(2)),' has been plotted'
        write(6,*)
        GOTO 10
20      write(6,'(A,I3)')'Total number of plots: ',int(data(2))
        data(4)=1.
        CLOSE(66)
        data(3)=float(imax)
        END PROGRAM dyndat
!> *******************************************************************
!! SUBROUTINE wfile1
!! Writes the data points which will be plotted by GNU to a file as
!! ell as the contour data for the ellipse
!< *******************************************************************
        SUBROUTINE wfile1(imax,x,xp,cx,cy)
        implicit real(8) (a-h,o-z)
        common/iopsys/iopsy,termtype,ltt,s2gr
        common/fpath/ppath
        dimension x(1000002),xp(1000002),cx(300),cy(300)
        character(len=280) :: command
        character(len=256) :: ppath,fwpath
        character(len=16) :: termtype
        logical s2gr
        command=""
        if(iopsy.eq.1 .or. iopsy.eq.3) then
! LINUX or MAC
          command="rm -f "//trim(ppath)//"dynac.cnt"
        else
! WINDOWS
          command="if exist "//trim(ppath)//"dynac.cnt del "//trim(ppath)//"dynac.cnt"
        endif
        CALL System(COMMAND)
        fwpath=''
        fwpath=trim(ppath)//"dynac.cnt"
        OPEN(unit=48,file=trim(fwpath))
        DO i=1,201
          write(48,*) cx(i),cy(i)
        ENDDO
        CLOSE(48)
        command=""
        if(iopsy.eq.1 .or. iopsy.eq.3) then
! LINUX or MAC
          command="rm -f "//trim(ppath)//"dynac.plt"
        else
! WINDOWS
          command="if exist "//trim(ppath)//"dynac.plt del "//trim(ppath)//"dynac.plt"
        endif
        CALL System(COMMAND)
        fwpath=''
        fwpath=trim(ppath)//"dynac.plt"
        OPEN(unit=49,file=trim(fwpath))
        DO i=1,imax
          write(49,*) x(i),xp(i)
        ENDDO
        CLOSE(49)
        RETURN
        END SUBROUTINE wfile1
!> *******************************************************************
!! SUBROUTINE wfile2
!! Writes the .GNU file containing the commands to be executed by
!! GNUPLOT
!< *******************************************************************
        SUBROUTINE wfile2(isave,icontr,ipn)
        implicit real(8) (a-h,o-z)
        common/wfil2/title,labels
        common/wfil2l/uxmin,uxmax,uymin,uymax
        common/fichier/filenm,pfnm
        common/fpath/ppath
        common/iopsys/iopsy,termtype,ltt,s2gr
        common/gui/dgui
        logical dgui,s2gr
        character(len=255), dimension(20) :: pfnm
        character(len=255) :: txt,outtxt
        character(len=280) :: command
        character(len=80) :: title
        character(len=256) :: ppath,fwpath,myfrmt
        character(len=40), dimension(20) :: labels
        character(len=33) :: paf
        character(len=23) :: filenm
        character(len=16) :: termtype
        character(len=3) :: cpn,cfn
        character(len=2) :: backslash
        parameter (backslash="\\")
        command=""
        if(iopsy.eq.1 .or. iopsy.eq.3) then
! LINUX or MAC
          command=""
          command="rm -f "//trim(ppath)//"dynac.gnu"
        else
! WINDOWS
          command=""
          command="if exist "//trim(ppath)//"dynac.gnu del "//trim(ppath)//"dynac.gnu"
        endif
        CALL System(COMMAND)
        if (isave.eq.0) then
          fwpath=''
          fwpath=trim(ppath)//"dynac.gnu"
          OPEN(unit=50,file=trim(fwpath))
        else
          call fn
          if (icontr.eq.2) filenm(1:2)='s3'
          if (icontr.eq.3) filenm(1:2)='s4'
          if (icontr.eq.4) filenm(1:2)='s5'
          filenm(18:21)='.gnu'
          paf(1:10)='savedplots'
          if(iopsy.eq.1 .or. iopsy.eq.3) then
! LINUX or MAC
            paf(11:11)="/"
          else
! WINDOWS
            paf(11:11)=backslash
          endif
          paf(12:32)=filenm(1:21)
          OPEN(unit=50,file=paf(1:32))
        endif
        write(50,"('set style data dots')")
        ytitle=0.99
        txt=''
        ipn=ipn-1
        cpn='   '
        txt=''
        txt='set terminal '//termtype(1:ltt)
! number the plot window and number the file
!2020debug
        cfn='000'
        cpn=''
        if(ipn+1.lt.10) then
          write(cpn(1:1),'(I1)') ipn+1
          write(cfn(3:3),'(I1)') ipn+1
        elseif(ipn.lt.100) then
          write(cpn(1:2),'(I2)') ipn+1
          write(cfn(2:3),'(I2)') ipn+1
        else
          write(cpn(1:3),'(I3)') ipn+1
          write(cfn(1:3),'(I3)') ipn+1
        endif   
!        write(cpn,'(I3.3)') ipn+1
        outtxt=''
        outtxt="set output '"//trim(ppath)//"dynplot"//cfn//"."//termtype(1:ltt)//"'"
        if(iopsy.eq.1) then
! LINUX
          if(dgui) then
! then number the plot window and let it persist
            if(s2gr) then
! then number the plot window 
              txt=trim(txt)//' '//cpn//' size 900,500'
              write(50,'(A)') trim(txt)
              write(50,'(A)') trim(outtxt)
            else
! then number the plot window and let it persist
              txt=trim(txt)//' '//cpn//' persist title "DYNAC" size 900,500'
              write(50,'(A)') trim(txt)
            endif
          else
            if(s2gr) then
! then number the plot window
              txt=trim(txt)//' size 900,500'
              write(50,'(A)') trim(txt)
              write(50,'(A)') trim(outtxt)
            else  
              txt=trim(txt)//' title "DYNAC" size 900,500'
              write(50,'(A)') trim(txt)
            endif
          endif
          ytitle=0.985
        elseif(iopsy.eq.3) then
! MAC
          if(dgui) then
            if(s2gr) then
! then number the plot window 
              txt=trim(txt)//' '//cpn//' size 900,500'
              write(50,'(A)') trim(txt)
              write(50,'(A)') trim(outtxt)
            else
! then number the plot window and let it persist
              txt=trim(txt)//' '//cpn//' persist title "DYNAC" size 900,500'
              write(50,'(A)') trim(txt)
            endif
          else
            if(s2gr) then
! then number the plot window
              txt=trim(txt)//' size 900,500'
              write(50,'(A)') trim(txt)
              write(50,'(A)') trim(outtxt)
            else  
              txt=trim(txt)//' title "DYNAC" size 900,500'
              write(50,'(A)') trim(txt)
            endif
          endif
          if(dgui) then
            ytitle=0.985
          else
            ytitle=0.995
          endif
        else
! WINDOWS
          if(dgui) then
            if(s2gr) then
! then number the plot window 
              txt=trim(txt)//' '//cpn//' size 900,500'
              write(50,'(A)') trim(txt)
              write(50,'(A)') trim(outtxt)
            else
! then number the plot window and let it persist
              txt=trim(txt)//' '//cpn//' persist title "DYNAC" size 900,500'
              write(50,'(A)') trim(txt)
            endif
          else
            if(s2gr) then
! then number the plot window
              txt=trim(txt)//' size 900,500'
              write(50,'(A)') trim(txt)
              write(50,'(A)') trim(outtxt)
            else  
              txt=trim(txt)//' title "DYNAC" size 900,500'
              write(50,'(A)') trim(txt)
            endif
          endif
        endif
        write(50,"('unset key')")
        if(iopsy.eq.1) then
! LINUX
          if(s2gr) then
            ytitle=0.980
          elseif(dgui) then
            ytitle=0.985
          else
            ytitle=0.980
          endif
        else
          ytitle=0.97
        endif
        write(50,'(A,A,A,F5.3)') 'set label "',trim(title),'" at screen 0.1 ,',ytitle
        write(50,'(A,A,A)') 'set xlabel "',trim(labels(1)),'"'
        write(50,'(A,A,A)') 'set ylabel "',trim(labels(2)),'"'
        write(50,"('set xrange [',f8.2,':',f8.2,']')") uxmin,uxmax
        write(50,"('set yrange [',f12.5,':',f12.5,']')") uymin,uymax
        if(iopsy.eq.1) then
! LINUX
!2024          write(50,"('set size 1., 1.')")
          write(50,"('set size 1., 0.98')")
        else
          write(50,"('set size 1., 0.98')")
        endif
        write(50,"('set samples 50')")
        if (icontr.eq.2) then
          if (isave.eq.0) then
            if(iopsy.le.3) then
              fwpath=''
              fwpath=trim(ppath)//"dynac.plt"
              myfrmt=''
              myfrmt="plot '"//trim(fwpath)//"' using 1:2 with lines, "
              write(50,'(A,A1)') trim(myfrmt),backslash
              myfrmt=''
              myfrmt="     '"//trim(fwpath)//"' using 3:4 with lines"
              write(50,'(A)') trim(myfrmt)
            else
              write(50,1012) backslash
              write(50,1013)
            endif
          else
            filenm(18:21)='.plt'
            write(50,2012) filenm,backslash
            write(50,2013) filenm
          endif
        endif
        if (icontr.eq.3) then
          if (isave.eq.0) then
            if(iopsy.le.3) then
              fwpath=''
              fwpath=trim(ppath)//"dynac.plt"
              myfrmt=''
              myfrmt="plot '"//trim(fwpath)//"' using 1:2 with lines"
              write(50,'(A)') trim(myfrmt)
            else
              write(50,1014)
            endif
          else
            filenm(18:21)='.plt'
            write(50,3014) filenm
          endif
        endif
        if (icontr.eq.4) then
          if (isave.eq.0) then
            if(iopsy.le.3) then
              fwpath=''
              fwpath=trim(ppath)//"dynac.plt"
              myfrmt=''
              myfrmt="plot '"//trim(fwpath)//"' using 1:2 with lines"
              write(50,'(A)') trim(myfrmt)
            else
              write(50,1014)
            endif
          else
            filenm(18:21)='.plt'
            write(50,4014) filenm
          endif
        endif
        if(.not. s2gr) then
          if(.not. dgui) write(50,'(A)')'pause -1 "hit return to continue"'
        endif  
1012    format('plot "dynac.plt" using 1:2 with lines, ',A1)
1013    format('"dynac.plt" using 3:4 with lines')
1014    format('plot "dynac.plt" using 1:2 with lines')
2012    format('plot "',a21,'" using 1:2 with lines, ',A1)
2013    format('"',a21,'" using 3:4 with lines')
3014    format('plot "',a21,'" using 1:2 with lines')
4014    format('plot "',a21,'" using 1:2 with lines')
        close (50)
        RETURN
        END SUBROUTINE wfile2
!> *******************************************************************
!! SUBROUTINE wfile3
!! Writes the data points which will be plotted by GNU to a file
!< *******************************************************************
        SUBROUTINE wfile3(imax,x,xp,y,yp)
        implicit real(8) (a-h,o-z)
        common/iopsys/iopsy,termtype,ltt,s2gr
        common/fpath/ppath
        dimension x(1000002),xp(1000002),y(1000002),yp(1000002)
        character(len=256) :: ppath,fwpath
        character(len=280) :: command
        character(len=16) :: termtype
        logical s2gr
        command=""
        if(iopsy.eq.1 .or. iopsy.eq.3) then
! LINUX or MAC
          command="rm -f "//trim(ppath)//"dynac.plt"
        else
! WINDOWS
          command="if exist "//trim(ppath)//"dynac.plt del "//trim(ppath)//"dynac.plt"
        endif
        CALL System(COMMAND)
        fwpath=''
        fwpath=trim(ppath)//"dynac.plt"
        OPEN(unit=48,file=trim(fwpath))
        DO i=1,imax
          write(48,*) x(i),xp(i),y(i),yp(i)
        ENDDO
        CLOSE(48)
        RETURN
        END SUBROUTINE wfile3
!> *******************************************************************
!! SUBROUTINE wfile10
!! Writes the data points which will be plotted by GNU to a file and
!! stores the ellips contour
!< *******************************************************************
        SUBROUTINE wfile10(icont,imax,x,xp,y,yp,z,zp)
        implicit real(8) (a-h,o-z)
        common/fichier/filenm,pfnm
        common/fpath/ppath
        common/iopsys/iopsy,termtype,ltt,s2gr
        common/chstat1/cst(1000002),cstat(20),fcstat(20),ncstat,mcstat
        common/chstat2/cccst
        common/grtyp/igrtyp
        dimension x(1000002),xp(1000002),y(1000002),yp(1000002)
        dimension z(1000002),zp(1000002)
        character(len=256) :: ppath,fwpath
        character(len=255), dimension(20) :: pfnm
        character(len=280) :: command
        character(len=23) :: filenm
        character(len=16) :: termtype
        character(len=7), dimension(20) :: cccst
        logical s2gr
        if (icont.eq.1) then
          command=""
          if(iopsy.eq.1 .or. iopsy.eq.3) then
! LINUX or MAC
            command="rm -f "//trim(ppath)//"dynac.cnt"            
          else
! WINDOWS
            command="if exist "//trim(ppath)//"dynac.cnt del "//trim(ppath)//"dynac.cnt"
          endif
          CALL System(COMMAND)
          fwpath=''
          fwpath=trim(ppath)//"dynac.cnt"
          OPEN(unit=51,file=trim(fwpath))
          DO i=1,201
            write(51,*) x(i),xp(i),y(i),yp(i),z(i),zp(i)
          ENDDO
          CLOSE(51)
        endif
        if (icont.eq.0) then
! store particle coordinates
          if(igrtyp.eq.1) then
            command=""
            if(iopsy.eq.1 .or. iopsy.eq.3) then
! LINUX or MAC
              command="rm -f "//trim(ppath)//"dynac.plt"
            else
! WINDOWS
              command="if exist "//trim(ppath)//"dynac.plt del "//trim(ppath)//"dynac.plt"
            endif
            CALL System(trim(COMMAND))
!            OPEN(unit=52,file='dynac.plt')
            fwpath=''
            fwpath=trim(ppath)//"dynac.plt"
            OPEN(unit=52,file=trim(fwpath))
            DO i=1,imax
              write(52,*) x(i),xp(i),y(i),yp(i),z(i),zp(i)
            ENDDO
            CLOSE(52)
          else
            mcstat=0
            fprec=epsilon(cstat(1))
            DO j=1,ncstat
              klm=0
              DO i=1,imax
                if(abs(cst(i)-cstat(j)).le.fprec ) then
                  klm=klm+1
                endif
              ENDDO
              if(igrtyp.eq.6) write(6,111) klm,cstat(j)
111           format(i7,' particles with charge state ',f9.5)
              if(igrtyp.eq.11) then
                if(j.eq.1) write(6,222) klm,cstat(j)
222           format(i7,' particles originally within zone ', &
                &     'delimited by  0.0  and ',f5.2,'*RMS')
                if(j.gt.1 .and. j.lt.ncstat) &
                &     write(6,223)klm,cstat(j-1),cstat(j)
223           format(i7,' particles originally within zone ', &
                &     'delimited by ',f5.2,' and ',f5.2,'*RMS')
                if(j.eq.ncstat) write(6,224) klm,cstat(j-1)
224           format(i7,' particles beyond ',f5.2,'*RMS')
              endif
              if(klm.ne.0) then
                mcstat=mcstat+1
                iunit=20+j
                if(iopsy.eq.1 .or. iopsy.eq.3) then
! LINUX or MAC
                  command=''
                  command="rm -f "//trim(ppath)//trim(pfnm(j))
                  CALL System(trim(COMMAND))
                else
! WINDOWS
                  command=''
                  command="if exist "//trim(ppath)//trim(pfnm(j))//" del "//trim(ppath)//trim(pfnm(j))
                  CALL System(trim(COMMAND))
                endif
                fcstat(mcstat)=cstat(j)
                if(iopsy.le.3) then
                  fwpath=''
                  fwpath=trim(ppath)//trim(pfnm(j))
                  OPEN(unit=iunit,file=trim(fwpath))
                else
                  OPEN(unit=iunit,file=pfnm(j))
                endif
                DO i=1,imax
                  if(abs(cst(i)-cstat(j)).le.fprec ) then
                    write(iunit,*) x(i),xp(i),y(i),yp(i),z(i),zp(i)
                  endif
                ENDDO
                CLOSE(iunit)
              endif
            ENDDO
          endif
        endif
        RETURN
        END SUBROUTINE wfile10
!> *******************************************************************
!! SUBROUTINE wfile11
!! Writes the data points which will be plotted by GNU to a file
!< *******************************************************************
        SUBROUTINE wfile11(imax,x,xp,y,yp)
        implicit real(8) (a-h,o-z)
        common/fichier/filenm,pfnm
        common/fpath/ppath
        common/iopsys/iopsy,termtype,ltt,s2gr
        common/chstat1/cst(1000002),cstat(20),fcstat(20),ncstat,mcstat
        common/chstat2/cccst
        common/grtyp/igrtyp
        dimension x(1000002),xp(1000002),y(1000002),yp(1000002)
        character(len=256) :: ppath,fwpath
        character(len=255), dimension(20) :: pfnm
        character(len=280) :: command
        character(len=23) :: filenm
        character(len=16) :: termtype
        character(len=7), dimension(20) :: cccst
        logical s2gr
! store particle coordinates
        IF(igrtyp.eq.2) then
          command=""
          if(iopsy.eq.1 .or. iopsy.eq.3) then
! LINUX or MAC
            command="rm -f "//trim(ppath)//"dynac.plt"
          else
! WINDOWS
            command="if exist "//trim(ppath)//"dynac.plt del "//trim(ppath)//"dynac.plt"
          endif
          CALL System(trim(COMMAND))
          if(iopsy.le.3) then
            fwpath=''
            fwpath=trim(ppath)//"dynac.plt"
            OPEN(unit=52,file=trim(fwpath))
          else
            OPEN(unit=52,file='dynac.plt')
          endif
          DO i=1,imax
            write(52,*) x(i),xp(i),y(i),yp(i)
          ENDDO
          CLOSE(52)
        ELSE
          mcstat=0
          fprec=epsilon(cstat(1))
          DO j=1,ncstat
            klm=0
            DO i=1,imax
              if(abs(cst(i)-cstat(j)).le.fprec ) then
                klm=klm+1
              endif
            ENDDO
            if(igrtyp.eq.7) write(6,111) klm,cstat(j)
111         format(i7,' particles with charge state ',f9.5)
            if(igrtyp.eq.12) then
              if(j.eq.1) write(6,222) klm,cstat(j)
222           format(i7,' particles originally within zone ', &
                     'delimited by  0.0  and ',f5.2,'*RMS')
                if(j.gt.1 .and. j.lt.ncstat) &
                     write(6,223)klm,cstat(j-1),cstat(j)
223           format(i7,' particles originally within zone ', &
                     'delimited by ',f5.2,' and ',f5.2,'*RMS')
              if(j.eq.ncstat) write(6,224) klm,cstat(j-1)
224           format(i7,' particles beyond ',f5.2,'*RMS')
            endif
            if(klm.ne.0) then
              mcstat=mcstat+1
              iunit=20+j
              if(iopsy.eq.1 .or. iopsy.eq.3) then
! LINUX or MAC
                command="rm -f "//trim(ppath)//trim(pfnm(j))
                CALL System(trim(COMMAND))                                
              else
! WINDOWS
                command=''
                command="if exist "//trim(ppath)//trim(pfnm(j))//" del "//trim(ppath)//trim(pfnm(j))
                CALL System(trim(COMMAND))                                
              endif
              fcstat(mcstat)=cstat(j)
              if(iopsy.le.3) then
                fwpath=''
                fwpath=trim(ppath)//trim(pfnm(j))
                OPEN(unit=iunit,file=trim(fwpath))
              else
                OPEN(unit=iunit,file=pfnm(j))
              endif  
              DO i=1,imax
                if(abs(cst(i)-cstat(j)).le.fprec ) then
                  write(iunit,*) x(i),xp(i),y(i),yp(i)
                endif
              ENDDO
              CLOSE(iunit)
            endif
          ENDDO
        ENDIF
        RETURN
        END SUBROUTINE wfile11
!> *******************************************************************
!! SUBROUTINE wfile21
!! xz-yz distribution plots and x,y,z & x',y',z' profiles
!< *******************************************************************
        SUBROUTINE wfile21(isave,ipn)
        implicit real(8) (a-h,o-z)
        common/prtcnt/imax
        common/wfil2/title,labels
        common/fichier/filenm,pfnm
        common/fpath/ppath
        common/iopsys/iopsy,termtype,ltt,s2gr
        common/chstat1/cst(1000002),cstat(20),fcstat(20),ncstat,mcstat
        common/chstat2/cccst
        common/pscl/yminsk,iskale
        common/gui/dgui
        common/grtyp/igrtyp
        logical dgui,s2gr
        character(len=256) :: ppath,fwpath,myfrmt
        character(len=255), dimension(20) :: pfnm
        character(len=255) :: strng,fnm,txt,outtxt
        character(len=280) :: command
        character(len=80) :: title
        character(len=40), dimension(20) :: labels
        character(len=40) :: labels3,labels4
        character(len=33) :: paf
        character(len=23) :: filenm
        character(len=16) :: termtype
        character(len=7) :: parcnt
        character(len=7), dimension(20) :: cccst
        character(len=7) :: toc
        character(len=7) :: indxx
        character(len=3) :: cpn,cfn
        character(len=2) :: backslash,indx
        common/wfil20/xmin(10),xmax(10),ymin(10),ymax(10)
        parameter (backslash="\\")
        command=""
        write(6,"(i7,' particles total')") imax
        write(parcnt,'(I7)') imax
        if(iopsy.eq.1 .or. iopsy.eq.3) then
! LINUX or MAC
          command="rm -f dynac.gnu"
        else
! WINDOWS
          command="if exist "//trim(ppath)//"dynac.gnu del "//trim(ppath)//"dynac.gnu"
        endif
        CALL System(COMMAND)
        if(isave.eq.0) then
          if(iopsy.le.3) then
            fwpath=''
            fwpath=trim(ppath)//"dynac.gnu"
            OPEN(unit=50,file=trim(fwpath))
          else
            OPEN(unit=50,file='dynac.gnu')
          endif
        else
          call fn
          filenm(1:2)='s2'
          filenm(18:21)='.gnu'
          paf(1:10)='savedplots'
          if(iopsy.eq.1) then
! LINUX
            paf(11:11)="/"
          elseif(iopsy.eq.3) then
! MAC
            paf(11:11)="/"
          else
! WINDOWS
            paf(11:11)=backslash
          endif
          paf(12:32)=filenm(1:21)
          OPEN(unit=50,file=paf(1:32))
        endif
        write(50,"('set style data dots',/,'set pointsize 0.01')")
        ipn=ipn-1
        cpn='   '
        txt=''
        txt='set terminal '//termtype(1:ltt)
! number the plot window 
        cfn='000'
        cpn=''
        if(ipn+1.lt.10) then
          write(cpn(1:1),'(I1)') ipn+1
          write(cfn(3:3),'(I1)') ipn+1
        elseif(ipn.lt.100) then
          write(cpn(1:2),'(I2)') ipn+1
          write(cfn(2:3),'(I2)') ipn+1
        else
          write(cpn(1:3),'(I3)') ipn+1
          write(cfn(1:3),'(I3)') ipn+1
        endif   
!        write(cpn,'(I3.3)') ipn+1
        outtxt=''
        outtxt="set output '"//trim(ppath)//"dynplot"//cfn//"."//termtype(1:ltt)//"'"
        ytitle=0.99
        if(iopsy.eq.1) then
! LINUX
          write(50,'(A,A7,A)') 'set label "',parcnt,' particles" at screen 0.45,0.51'
          if(dgui) then
            if(s2gr) then
! then number the plot window 
              txt=trim(txt)//' '//cpn//' size 750,625'
              write(50,'(A)') trim(txt)
              write(50,'(A)') trim(outtxt)
            else
! then number the plot window and let it persist
              txt=trim(txt)//' '//cpn//' persist title "DYNAC" size 750,625'
              write(50,'(A)') trim(txt)
            endif
          else
            if(s2gr) then
! then number the plot window
              txt=trim(txt)//' size 750,625'
              write(50,'(A)') trim(txt)
              write(50,'(A)') trim(outtxt)
            else  
              txt=trim(txt)//' title "DYNAC" size 750,625'
              write(50,'(A)') trim(txt)
            endif
          endif
          ytitle=0.985
        elseif(iopsy.eq.3) then
! MAC
          write(50,'(A,A7,A)') 'set label "',parcnt,' particles" at screen 0.45,0.51'
          if(dgui) then
            if(s2gr) then
! then number the plot window 
              txt=trim(txt)//' '//cpn//' size 750,625'
              write(50,'(A)') trim(txt)
              write(50,'(A)') trim(outtxt)
              ytitle=0.985
            else
! then number the plot window and let it persist
              txt=trim(txt)//' '//cpn//' persist title "DYNAC" size 750,625'
              write(50,'(A)') trim(txt)
              ytitle=0.990
            endif
          else
            if(s2gr) then
! then number the plot window
              txt=trim(txt)//' size 750,625'
              write(50,'(A)') trim(txt)
              write(50,'(A)') trim(outtxt)
              ytitle=0.985
            else  
              txt=trim(txt)//' title "DYNAC" size 750,625'
              write(50,'(A)') trim(txt)
              ytitle=0.990
            endif
          endif
        else
! WINDOWS
          write(50,'(A,A7,A)') 'set label "',parcnt,' particles" at screen 0.45,0.52'
          if(dgui) then
            ytitle=0.99
            if(s2gr) then
! then number the plot window 
              txt=trim(txt)//' '//cpn//' size 817,768'
              write(50,'(A)') trim(txt)
              write(50,'(A)') trim(outtxt)
            else
! then number the plot window and let it persist
              txt=trim(txt)//' '//cpn//' persist title "DYNAC" size 817,768'
              write(50,'(A)') trim(txt)
            endif
          else
            ytitle=0.985
            if(s2gr) then
! then number the plot window
              txt=trim(txt)//' size 817,768'
              write(50,'(A)') trim(txt)
              write(50,'(A)') trim(outtxt)
              ytitle=0.990
            else  
              txt=trim(txt)//' title "DYNAC" size 817,768'
              write(50,'(A)') trim(txt)
            endif
          endif
        endif
        if(ncstat.eq.1) then
          write(50,"('unset key')")
        else
          if(iopsy.eq.1) then
! LINUX
            if(igrtyp.eq.12) then
              if(s2gr) then
                write(50,"('set key at screen 0.6, 0.96 spacing 0.9', &
                & ' samplen 1 textcolor rgb variable ')")
              elseif(dgui) then
                write(50,"('set key at screen 0.58, 0.95 spacing 0.9', &
                & ' samplen 1 textcolor rgb variable ')")
              else
                write(50,"('set key at screen 0.59, 0.95 spacing 0.9', &
                & ' samplen 1 textcolor rgb variable ')")
              endif              
            else
              if(s2gr) then
                write(50,"('set key at screen 0.57, 0.96 spacing 0.8', &
                & ' samplen 1 textcolor rgb variable ')")
              elseif(dgui)then
                write(50,"('set key at screen 0.555, 0.97 spacing 0.9', &
                & ' samplen 1 textcolor rgb variable ')")
              else
                write(50,"('set key at screen 0.565, 0.97 spacing 0.9', &
                & ' samplen 1 textcolor rgb variable ')")
              endif              
            endif
          elseif(iopsy.eq.3) then
! MAC
            if(igrtyp.eq.12) then
              if(s2gr) then
                write(50,"('set key at screen 0.6, 0.96 spacing 0.9', &
                & ' samplen 1 textcolor rgb variable ')")
              else
!                write(50,"('set key at screen 0.59, 0.95 spacing 0.8', &
                write(50,"('set key at screen 0.58, 0.95 spacing 0.9', &
                & ' samplen 1 textcolor rgb variable ')")
              endif              
            else
              if(s2gr) then
                write(50,"('set key at screen 0.57, 0.97 spacing 0.9', &
                & ' samplen 1 textcolor rgb variable ')")
              else
!                write(50,"('set key at screen 0.57, 0.95 spacing 0.8', &
                write(50,"('set key at screen 0.555, 0.97 spacing 0.9', &
                & ' samplen 1 textcolor rgb variable ')")
              endif              
            endif
          else
! WINDOWS
            if(igrtyp.eq.12) then
              if(s2gr) then
                write(50,"('set key at screen 0.585, 0.95 spacing 0.9', &
                & ' samplen 1 textcolor rgb variable ')")   
              else
                write(50,"('set key at screen 0.585, 0.95 spacing 0.9', &
                & ' samplen 1 textcolor rgb variable ')")            
              endif                        
            else
              if(s2gr) then
                write(50,"('set key at screen 0.565, 0.97 spacing 0.9', &
                & ' samplen 1 textcolor rgb variable ')")   
              else
                write(50,"('set key at screen 0.565, 0.97 spacing 0.8', &
                & ' samplen 1 textcolor rgb variable ')")             
              endif                       
            endif
          endif
        endif
        write(50,'(A,A,A,F5.3)') 'set label "',trim(title),'" at screen 0.13 ,',ytitle
        write(50,"('set multiplot')")
! x-z
        write(50,"('set size 0.5,0.5')")
        if(iopsy.eq.1) then
! LINUX
          write(50,"('set origin 0.,0.49')")
        else
          write(50,"('set origin 0.,0.5')")
        endif
        write(50,'(A,A,A)') 'set xlabel "',trim(labels(1)),'"'
        write(50,'(A,A,A)') 'set ylabel "',trim(labels(2)),'"'
        write(50,"('set xrange [',f8.2,':',f8.2,']')") xmin(1),xmax(1)
        write(50,"('set yrange [',f12.6,':',f12.6,']')") ymin(1),ymax(1)
        if(igrtyp.eq.2) then
          if(isave.eq.0) then
            if(iopsy.le.3) then
              fwpath=''
              fwpath=trim(ppath)//"dynac.plt"
              myfrmt=''
              if(termtype.eq.'qt' .or. s2gr) then
                myfrmt="plot '"//trim(fwpath)//"' using 1:2 with dots lc 8"
                write(50,'(A)') trim(myfrmt)
              else
                myfrmt="plot '"//trim(fwpath)//"' using 1:2 with dots lc 0"
                write(50,'(A)') trim(myfrmt)
              endif
            elseif(iopsy.eq.1 .and. termtype.eq.'qt') then
              write(50,7008)
            else
              write(50,1008)
            endif
          else
            filenm(18:21)='.plt'
            if(iopsy.eq.3) then
! MAC
              write(50,8808) filenm(1:21)
            elseif(iopsy.eq.2 .and. termtype.eq.'qt') then
              write(50,8808) filenm(1:21)
            elseif(iopsy.eq.1 .and. termtype.eq.'qt') then
              write(50,7808) filenm(1:21)
            else
              write(50,2008) filenm(1:21)
            endif
          endif
        else
          strng=pfnm(1)
          toc=cccst(1)
          indxx=toc(1:7)
          if(isave.eq.0) then
            strng=pfnm(1)
            toc=cccst(1)
            indxx=toc(1:7)
            if(iopsy.le.3) then
              fwpath=''
              fwpath=trim(ppath)//trim(pfnm(1))
              myfrmt=''
              myfrmt="plot '"//trim(fwpath)//"' using 1:2 title '"//indxx//"' with dots lc 1, "
              write(50,'(A,A1)') trim(myfrmt),backslash
            else           
              write(50,3003) pfnm(1),indxx,backslash
            endif
            do j=2,mcstat-1
              strng=''
              write(strng,'(I2)') j
              toc=cccst(j)
              indxx=toc(1:7)
              if(iopsy.le.3) then
                fwpath=''
                fwpath=trim(ppath)//trim(pfnm(j))
                myfrmt=''
                myfrmt="     '"//trim(fwpath)//"' using 1:2 title '"//indxx//"' with dots lc "//trim(strng)//", "
                write(50,'(A,A1)') trim(myfrmt),backslash
              else           
                write(50,3033) pfnm(j),indxx,j,backslash
              endif
            enddo
            strng=''
            write(strng,'(I2)') mcstat
            if(igrtyp.eq.12) then
              toc=cccst(mcstat-1)
              indxx=toc(1:4)
              if(iopsy.le.3) then
                fwpath=''
                fwpath=trim(ppath)//trim(pfnm(mcstat))
                myfrmt=''
                myfrmt="     '"//trim(fwpath)//"' using 1:2 title ' >"//indxx//"' with dots lc "//trim(strng)
                write(50,'(A)') trim(myfrmt)
              else           
                write(50,3013) pfnm(j),indxx,mcstat
              endif
            else
              toc=cccst(mcstat)
              indxx=toc(1:7)
              if(iopsy.le.3) then
                fwpath=''
                fwpath=trim(ppath)//trim(pfnm(mcstat))
                myfrmt=''
                myfrmt="     '"//trim(fwpath)//"' using 1:2 title '"//indxx//"' with dots lc "//trim(strng)
                write(50,'(A)') trim(myfrmt)
              else           
                write(50,3043) pfnm(j),indxx,mcstat
              endif
            endif
          else
            filenm(18:23)=strng(6:11)
            write(50,5003) filenm,indxx,backslash
            do j=2,mcstat-1
              strng=pfnm(j)
              toc=cccst(j)
              indxx=toc(1:4)
              filenm(18:23)=strng(6:11)
              write(50,5033) filenm,indxx,j,backslash
            enddo
            strng=pfnm(mcstat)
            filenm(18:23)=strng(6:11)
            if(igrtyp.eq.12) then
              toc=cccst(mcstat-1)
              indxx=toc(1:4)
              write(50,5013) filenm,indxx,mcstat
            else
              toc=cccst(mcstat)
              indxx=toc(1:4)
              write(50,5043) filenm,indxx,mcstat
            endif
          endif
        endif
        write(50,"('unset key')")
        write(50,"('unset label')")
! y-z
        write(50,"('set size 0.5,0.5')")
        if(iopsy.eq.1) then
! LINUX
          write(50,"('set origin 0.5,0.49')")
        else
          write(50,"('set origin 0.5,0.5')")
        endif
        write(50,'(A,A,A)') 'set xlabel "',trim(labels(3)),'"'
        write(50,'(A,A,A)') 'set ylabel "',trim(labels(4)),'"'
        write(50,"('set xrange [',f8.2,':',f8.2,']')") xmin(2),xmax(2)
        write(50,"('set yrange [',f12.6,':',f12.6,']')") ymin(2),ymax(2)
        if(igrtyp.eq.2) then
          if(isave.eq.0) then
            if(iopsy.le.3) then
              fwpath=''
              fwpath=trim(ppath)//"dynac.plt"
              myfrmt=''
              if(termtype.eq.'qt' .or. s2gr) then
                myfrmt="plot '"//trim(fwpath)//"' using 3:4 with dots lc 8"
                write(50,'(A)') trim(myfrmt)
              else
                myfrmt="plot '"//trim(fwpath)//"' using 3:4 with dots lc 0"
                write(50,'(A)') trim(myfrmt)
              endif
            elseif(iopsy.eq.1 .and. termtype.eq.'qt') then
              write(50,7010)
            else
              write(50,1010)
            endif
          else
            filenm(18:21)='.plt'
            if(iopsy.eq.3) then
              write(50,8810) filenm(1:21)
            elseif(iopsy.eq.2 .and. termtype.eq.'qt') then
              write(50,8810) filenm(1:21)
            elseif(iopsy.eq.1 .and. termtype.eq.'qt') then
              write(50,7810) filenm(1:21)
            else
              write(50,2010) filenm(1:21)
            endif
          endif
        else
          if(isave.eq.0) then
            if(iopsy.le.3) then
              fwpath=''
              fwpath=trim(ppath)//trim(pfnm(1))
              myfrmt=''
              myfrmt="plot '"//trim(fwpath)//"' using 3:4 with dots lc 1, "
              write(50,'(A,A1)') trim(myfrmt),backslash
            else           
              write(50,4003) pfnm(1),backslash
            endif
            do j=2,mcstat-1
              strng=''
              write(strng,'(I2)') j
              if(iopsy.le.3) then
                fwpath=''
                fwpath=trim(ppath)//trim(pfnm(j))
                myfrmt=''
                myfrmt="     '"//trim(fwpath)//"' using 3:4 with dots lc "//trim(strng)//", "
                write(50,'(A,A1)') trim(myfrmt),backslash
              else           
                write(50,4033) pfnm(j),j,backslash
              endif
            enddo
            strng=''
            write(strng,'(I2)') mcstat
            if(iopsy.le.3) then
              fwpath=''
              fwpath=trim(ppath)//trim(pfnm(mcstat))
              myfrmt=''
              myfrmt="     '"//trim(fwpath)//"' using 3:4 with dots lc "//trim(strng)
              write(50,'(A)') trim(myfrmt)
            else           
              write(50,4013) pfnm(mcstat),mcstat
            endif
          else
            strng=pfnm(1)
            filenm(18:23)=strng(6:11)
            write(50,6003) filenm,backslash
            do j=2,mcstat-1
              strng=pfnm(j)
              filenm(18:23)=strng(6:11)
              write(50,6033) filenm,j,backslash
            enddo
            strng=pfnm(mcstat)
            filenm(18:23)=strng(6:11)
            write(50,6013) filenm,mcstat
          endif
        endif
! x,y,z profiles
! new start
        labels3="X, Y, Z (RMS multiples)"
        labels4="N (normalized)"
        xmin2=-5.
        xmax2=5.
        ymin2=0.
        if(iskale.eq.1) then
          if (yminsk.gt.0. .and. yminsk.lt.1.) then
            ymin2=yminsk
          else
            write(6,*) '***         Error in logscale          ***'
            write(6,*) '** log scale minimum defaults to 1.E-06 **'
            ymin2=1.e-6
          endif
        endif
        ymax2=1.
! new end
        write(50,"('set size 0.5,0.5')")
        write(50,"('set origin 0.,0.')")
        write(50,'(A,A,A)') 'set xlabel "',trim(labels3),'"'
        write(50,'(A,A,A)') 'set ylabel "',trim(labels4),'"'
        if(iskale.eq.1) then
          write(50,"('set logscale y')")
          write(50,9012)
        endif
        write(50,"('set xrange [',f8.2,':',f8.2,']')") xmin2,xmax2
        write(50,"('set yrange [',f12.6,':',f12.6,']')") ymin2,ymax2
        if(iopsy.eq.1) then
! LINUX        
          if(s2gr) then
            write(50,"('set key at screen 0.55, 0.47 spacing 0.9', &
            & ' samplen 1 textcolor rgb variable ')")
          else
            write(50,"('set key at screen 0.545, 0.46 spacing 0.9', &
            & ' samplen 1 textcolor rgb variable ')")
          endif              
        elseif(iopsy.eq.3) then
! MAC
          if(s2gr) then
            write(50,"('set key at screen 0.535, 0.475 spacing 0.9', &
            &     ' samplen 1 textcolor rgb variable ')")
          else
            write(50,"('set key at screen 0.535, 0.46 spacing 0.9', &
            &     ' samplen 1 textcolor rgb variable ')")
          endif              
        else
! WINDOWS
          if(s2gr) then
            write(50,"('set key at screen 0.545, 0.47 spacing 1.0', &
            &     ' samplen 1 textcolor rgb variable ')")    
          else
            write(50,"('set key at screen 0.545, 0.47 spacing 1.0', &
            &     ' samplen 1 textcolor rgb variable ')")             
          endif         
        endif
        if(isave.eq.0) then
          if(iopsy.le.3) then
            fnm=pfnm(1)
            nn=len_trim(fnm)
            fnm(nn-2:nn)='pro'
            indx=' X'
            fwpath=''
            fwpath=trim(ppath)//trim(fnm)
            myfrmt=''
            myfrmt="plot '"//trim(fwpath)//"' using 1:2 title '"//indx//"' with lines, "
            write(50,'(A,A1)') trim(myfrmt),backslash
            fnm=pfnm(2)
            nn=len_trim(fnm)
            fnm(nn-2:nn)='pro'
            indx=' Y'
            fwpath=''
            fwpath=trim(ppath)//trim(fnm)
            myfrmt=''
            myfrmt="     '"//trim(fwpath)//"' using 1:2 title '"//indx//"' with lines ls 2, "
            write(50,'(A,A1)') trim(myfrmt),backslash
            fnm=pfnm(3)
            nn=len_trim(fnm)
            fnm(nn-2:nn)='pro'
            indx=' Z'
            fwpath=''
            fwpath=trim(ppath)//trim(fnm)
            myfrmt=''
            myfrmt="     '"//trim(fwpath)//"' using 1:2 title '"//indx//"' with lines ls 3"
            write(50,'(A)') trim(myfrmt)
          else
            fnm=pfnm(1)
            nn=len_trim(fnm)
            fnm(nn-2:nn)='pro'
            indx=' X'
            write(50,4303) fnm,indx,backslash
            fnm=pfnm(2)
            nn=len_trim(fnm)
            fnm(nn-2:nn)='pro'
            indx=' Y'
            write(50,4305) fnm,indx,backslash
            fnm=pfnm(3)
            nn=len_trim(fnm)
            fnm(nn-2:nn)='pro'
            indx=' Z'
            write(50,4313) fnm,indx
          endif
        else
          strng=pfnm(1)
          filenm(18:20)=strng(6:8)
          filenm(21:23)='pro'
          indx=' X'
          write(50,6303) filenm,indx,backslash
          strng=pfnm(2)
          filenm(18:20)=strng(6:8)
          filenm(21:23)='pro'
          indx=' Y'
          write(50,6305) filenm,indx,backslash
          strng=pfnm(3)
          filenm(18:20)=strng(6:8)
          filenm(21:23)='pro'
          indx=' Z'
          write(50,6313) filenm,indx
        endif
        write(50,"('unset key')")
        if(iskale.eq.1) then
          write(50,"('unset logscale y')")
          write(50,9014)
        endif
!
! xp,yp,zp profiles
! new start
        labels3="Xp, Yp, Zp (RMS multiples)"
        labels4="N (normalized)"
        xmin2=-5.
        xmax2=5.
        ymin2=0.
        if(iskale.eq.1) then
          if (yminsk.gt.0. .and. yminsk.lt.1.) then
            ymin2=yminsk
          else
            ymin2=1.e-6
          endif
        endif
        ymax2=1.
! new end
        write(50,"('set size 0.5,0.5')")
        write(50,"('set origin 0.5,0.')")
        write(50,'(A,A,A)') 'set xlabel "',trim(labels3),'"'
        write(50,'(A,A,A)') 'set ylabel "',trim(labels4),'"'
        if(iskale.eq.1) then
          write(50,"('set logscale y')")
          write(50,9012)
        endif
        write(50,"('set xrange [',f8.2,':',f8.2,']')") xmin2,xmax2
        write(50,"('set yrange [',f12.6,':',f12.6,']')") ymin2,ymax2
        if(iopsy.eq.1) then
          if(s2gr) then
            write(50,"('set key at screen 0.55, 0.17 spacing 0.9', &
            & ' samplen 1 textcolor rgb variable ')")
          else
            write(50,"('set key at screen 0.545, 0.18 spacing 0.9', &
            & ' samplen 1 textcolor rgb variable ')")
          endif
        elseif(iopsy.eq.3) then
          if(s2gr) then
            write(50,"('set key at screen 0.545, 0.18 spacing 0.9', &
            &     ' samplen 1 textcolor rgb variable ')")
          else
            write(50,"('set key at screen 0.535, 0.18 spacing 0.9', &
            &     ' samplen 1 textcolor rgb variable ')")
          endif          
        else
! Windows
          if(s2gr) then
            write(50,"('set key at screen 0.545, 0.18 spacing 1.0', &
            &     ' samplen 1 textcolor rgb variable ')")    
          else
            write(50,"('set key at screen 0.545, 0.18 spacing 1.0', &
            &     ' samplen 1 textcolor rgb variable ')")            
          endif        
        endif
        if(isave.eq.0) then
          if(iopsy.le.3) then
            fnm=pfnm(4)
            nn=len_trim(fnm)
            fnm(nn-2:nn)='pro'
            indx='Xp'
            fwpath=''
            fwpath=trim(ppath)//trim(fnm)
            myfrmt=''
            myfrmt="plot '"//trim(fwpath)//"' using 1:2 title '"//indx//"' with lines, "
            write(50,'(A,A1)') trim(myfrmt),backslash
            fnm=pfnm(5)
            nn=len_trim(fnm)
            fnm(nn-2:nn)='pro'
            indx='Yp'
            fwpath=''
            fwpath=trim(ppath)//trim(fnm)
            myfrmt=''
            myfrmt="     '"//trim(fwpath)//"' using 1:2 title '"//indx//"' with lines ls 2, "
            write(50,'(A,A1)') trim(myfrmt),backslash
            fnm=pfnm(6)
            nn=len_trim(fnm)
            fnm(nn-2:nn)='pro'
            indx='Zp'
            fwpath=''
            fwpath=trim(ppath)//trim(fnm)
            myfrmt=''
            myfrmt="     '"//trim(fwpath)//"' using 1:2 title '"//indx//"' with lines ls 3"
            write(50,'(A)') trim(myfrmt)
          else
            fnm=pfnm(4)
            nn=len_trim(fnm)
            fnm(nn-2:nn)='pro'
            indx='Xp'
            write(50,4303) fnm,indx,backslash
            fnm=pfnm(5)
            nn=len_trim(fnm)
            fnm(nn-2:nn)='pro'
            indx='Yp'
            write(50,4305) fnm,indx,backslash
            fnm=pfnm(6)
            nn=len_trim(fnm)
            fnm(nn-2:nn)='pro'
            indx='Zp'
            write(50,4313) fnm,indx
          endif
        else
          strng=pfnm(4)
          filenm(18:20)=strng(6:8)
          filenm(21:23)='pro'
          indx='Xp'
          write(50,6303) filenm,indx,backslash
          strng=pfnm(5)
          filenm(18:20)=strng(6:8)
          filenm(21:23)='pro'
          indx='Yp'
          write(50,6305) filenm,indx,backslash
          strng=pfnm(6)
          filenm(18:20)=strng(6:8)
          filenm(21:23)='pro'
          indx='Zp'
          write(50,6313) filenm,indx
        endif
        write(50,"('unset key')")
        if(iskale.eq.1) then
          write(50,"('unset logscale y')")
          write(50,9014)
        endif
!
        write(50,"('unset multiplot')")
        if(.not. s2gr) then
          if(.not. dgui) write(50,'(A)')'pause -1 "hit return to continue"'
        endif  
1008    format('plot "dynac.plt" using 1:2 with dots lc 0')
7008    format('plot "dynac.plt" using 1:2 with dots lc 7')
1010    format('plot "dynac.plt" using 3:4 with dots lc 0')
7010    format('plot "dynac.plt" using 3:4 with dots lc 7')
2008    format('plot "',a21,'" using 1:2 with dots lc 0')
2010    format('plot "',a21,'" using 3:4 with dots lc 0')
7808    format('plot "',a21,'" using 1:2 with dots lc 7')
7810    format('plot "',a21,'" using 3:4 with dots lc 7')
8808    format('plot "',a21,'" using 1:2 with dots lc 8')
8810    format('plot "',a21,'" using 3:4 with dots lc 8')
3003    format('plot "',a11,'" using 1:2 title "',A4, &
               '" with dots lc 1, ',A1)
3033    format('     "',a11,'" using 1:2 title "',A4, &
               '" with dots lc ',I2,',',A1)
3013    format('     "',a11,'" using 1:2 title " >',A4, &
               '" with dots lc ',I2)
3043    format('     "',a11,'" using 1:2 title " ',A4, &
               '" with dots lc ',I2)
4003    format('plot "',a11,'" using 3:4', &
               ' with dots lc 1, ',A1)
4303    format('plot "',a11,'" using 1:2 title "',A2, &
               '" with lines, ',A1)
4305    format('     "',a11,'" using 1:2 title "',A2, &
               '" with lines ls 2, ',A1)
4313    format('     "',a11,'" using 1:2 title "',A2, &
               '" with lines ls 3')
4033    format('     "',a11,'" using 3:4', &
               ' with dots lc ',I2,',',A1)
4013    format('     "',a11,'" using 3:4', &
               ' with dots lc ',I2)
5003    format('plot "',a23,'" using 1:2 title "',A4, &
               '" with dots lc 1, ',A1)
5033    format('     "',a23,'" using 1:2 title "',A4, &
               '" with dots lc ',I2,',',A1)
5013    format('     "',a23,'" using 1:2 title " >',A4, &
               '" with dots lc ',I2)
5043    format('     "',a23,'" using 1:2 title " ',A4, &
               '" with dots lc ',I2)
6003    format('plot "',a23,'" using 3:4', &
               ' with dots lc 1, ',A1)
6033    format('     "',a23,'" using 3:4', &
               ' with dots lc ',I2,',',A1)
6013    format('     "',a23,'" using 3:4', &
               ' with dots lc ',I2)
6303    format('plot "',a23,'" using 1:2 title "',A2, &
               '" with lines, ',A1)
6305    format('     "',a23,'" using 1:2 title "',A2, &
               '" with lines ls 2, ',A1)
6313    format('     "',a23,'" using 1:2 title "',A2, &
               '" with lines ls 3')
9012    format('set format y "%.0t.E%+02T"')
9014    format('set format y "%g"')
        close (50)
        RETURN
        END SUBROUTINE wfile21
!> *******************************************************************
!! SUBROUTINE wfile121
!! xz-yz density plots and x,y,z & x',y',z' profiles
!< *******************************************************************
        SUBROUTINE wfile121(isave,ipn)
        implicit real(8) (a-h,o-z)
        common/prtcnt/imax
        common/wfil2/title,labels
        common/wfil120/xxpmax,yypmax,xymax,zzpmax,zxmax,zymax,ndx,ndy,bex
        common/fichier/filenm,pfnm
        common/fpath/ppath
        common/iopsys/iopsy,termtype,ltt,s2gr
        character(len=256) :: ppath,fwpath,myfrmt
        character(len=255), dimension(20) :: pfnm
        character(len=255) :: strng,fnm,txt,outtxt
        character(len=80) :: command,title
        character(len=50), dimension(3) :: cols
        character(len=40), dimension(20) :: labels
        character(len=40) :: labels3,labels4
        character(len=33) :: paf
        character(len=23) :: filenm
        character(len=16) :: termtype
        character(len=8) :: hm,vm,ho,vo,hr,vr,sc
        character(len=7) :: parcnt
        character(len=3) :: cpn,cfn
        character(len=2) :: backslash,indx
        common/pscl/yminsk,iskale
        common/gui/dgui
        logical dgui,s2gr
        common/wfil20/xmin(10),xmax(10),ymin(10),ymax(10)
        dimension bex(30)
        parameter (backslash="\\")
        command=""
        write(6,"(i7,' particles total')") imax
        write(parcnt,'(I7)') imax
        if(iopsy.eq.1 .or. iopsy.eq.3) then
! LINUX or MAC
          command="rm -f dynac.gnu"
        else
! WINDOWS
          command="if exist "//trim(ppath)//"dynac.gnu del "//trim(ppath)//"dynac.gnu"
        endif
        CALL System(COMMAND)
        if(isave.eq.0) then
!          OPEN(unit=50,file='dynac.gnu')
          fwpath=''
          fwpath=trim(ppath)//"dynac.gnu"
          OPEN(unit=50,file=trim(fwpath))
        else
          call fn
          filenm(1:2)='s2'
          filenm(18:21)='.gnu'
          paf(1:10)='savedplots'
          if(iopsy.eq.1) then
! LINUX
            paf(11:11)="/"
          elseif(iopsy.eq.3) then
! MAC
            paf(11:11)="/"
          else
! WINDOWS
            paf(11:11)=backslash
          endif
          paf(12:32)=filenm(1:21)
          OPEN(unit=50,file=paf(1:32))
        endif
        write(50,"('set style data dots',/,'set pointsize 0.01')")
        ipn=ipn-1
        cpn='   '
        txt=''
        txt='set terminal '//termtype(1:ltt)
! number the plot window 
!2020debug
        cfn='000'
        cpn=''
        if(ipn+1.lt.10) then
          write(cpn(1:1),'(I1)') ipn+1
          write(cfn(3:3),'(I1)') ipn+1
        elseif(ipn.lt.100) then
          write(cpn(1:2),'(I2)') ipn+1
          write(cfn(2:3),'(I2)') ipn+1
        else
          write(cpn(1:3),'(I3)') ipn+1
          write(cfn(1:3),'(I3)') ipn+1
        endif   
!        write(cpn,'(I3.3)') ipn+1
        outtxt=''
        outtxt="set output '"//trim(ppath)//"dynplot"//cfn//"."//termtype(1:ltt)//"'"
        ytitle=0.99
        if(iopsy.eq.1) then
! LINUX
          write(50,'(A,A7,A)') 'set label "',parcnt,' particles" at screen 0.45,0.51'
          if(dgui) then
            if(s2gr) then
! then number the plot window 
              txt=trim(txt)//' '//cpn//' size 750,625'
              write(50,'(A)') trim(txt)
              write(50,'(A)') trim(outtxt)
            else
! then number the plot window and let it persist
              txt=trim(txt)//' '//cpn//' persist title "DYNAC" size 750,625'
              write(50,'(A)') trim(txt)
            endif
          else
            if(s2gr) then
! then number the plot window
              txt=trim(txt)//' size 750,625'
              write(50,'(A)') trim(txt)
              write(50,'(A)') trim(outtxt)
            else  
              txt=trim(txt)//' title "DYNAC" size 750,625'
              write(50,'(A)') trim(txt)
            endif
          endif
          ytitle=0.985
        elseif(iopsy.eq.3) then
! MAC
          write(50,'(A,A7,A)') 'set label "',parcnt,' particles" at screen 0.45,0.51'
          if(dgui) then
            if(s2gr) then
! then number the plot window 
              txt=trim(txt)//' '//cpn//' size 750,625'
              write(50,'(A)') trim(txt)
              write(50,'(A)') trim(outtxt)
              ytitle=0.980
            else
! then number the plot window and let it persist
              txt=trim(txt)//' '//cpn//' persist title "DYNAC" size 750,625'
              write(50,'(A)') trim(txt)
              ytitle=0.985
            endif
          else
            if(s2gr) then
! then number the plot window
              txt=trim(txt)//' size 750,625'
              write(50,'(A)') trim(txt)
              write(50,'(A)') trim(outtxt)
              ytitle=0.980              
            else  
              txt=trim(txt)//' title "DYNAC" size 750,625'
              write(50,'(A)') trim(txt)
              ytitle=0.990              
            endif
          endif
        else
! WINDOWS
          write(50,'(A,A7,A)') 'set label "',parcnt,' particles" at screen 0.45,0.52'
          if(dgui) then
            if(s2gr) then
! then number the plot window 
              txt=trim(txt)//' '//cpn//' size 817,768'
              write(50,'(A)') trim(txt)
              write(50,'(A)') trim(outtxt)
              ytitle=0.985 
            else
! then number the plot window and let it persist
              txt=trim(txt)//' '//cpn//' persist title "DYNAC" size 817,768'
              write(50,'(A)') trim(txt)
            endif
          else
            if(s2gr) then
! then number the plot window
              txt=trim(txt)//' size 817,768'
              write(50,'(A)') trim(txt)
              write(50,'(A)') trim(outtxt)
              ytitle=0.985              
            else  
              txt=trim(txt)//' title "DYNAC" size 817,768'
              write(50,'(A)') trim(txt)
            endif
          endif
        endif
        write(50,"('unset key')")
        write(50,'(A,A,A,F5.3)') 'set label "',trim(title),'" at screen 0.13 ,',ytitle
        write(50,"('set multiplot')")
! x-z
        write(50,"('set size 0.495,0.55')")
        if(iopsy.eq.1) then
! LINUX
          write(50,"('set origin 0.,0.49')")
        else
          write(50,"('set origin 0.,0.5')")
        endif
        write(50,'(A,A,A)') 'set xlabel "',trim(labels(1)),'"'
        write(50,'(A,A,A)') 'set ylabel "',trim(labels(2)),'"'
        write(50,"('set xrange [',f8.2,':',f8.2,']')") xmin(1),xmax(1)
        write(50,"('set yrange [',f12.6,':',f12.6,']')") ymin(1),ymax(1)
        write(50,"('set dgrid3d 20,20')")
        write(50,"('set pm3d map interpolate 0,0')")
        write(50,5002)
! splot 'dynac.plt' u (xmul*($1-10.5)/20.):(xpmul*($2-10.5)/20.):(100*$3/fmax)
        hm=''
        vm=''
        ho=''
        vo=''
        hr=''
        vr=''
        sc=''
        write(hm,'(F8.3)') bex(18)-bex(17)
        write(vm,'(F8.3)') bex(20)-bex(19)
        write(ho,'(F8.3)') float(ndx+1)/2.
        write(vo,'(F8.3)') float(ndy+1)/2.
        write(hr,'(F8.3)') float(ndx)
        write(vr,'(F8.3)') float(ndy)
        write(sc,'(F8.3)') zxmax
        cols(1)='('//hm//'*($1-'//ho//')/'//hr//')'
        cols(2)='('//vm//'*($2-'//vo//')/'//vr//')'
        cols(3)='(100.*$3/'//sc//')'
        if(isave.eq.0) then
          fwpath=''
          fwpath=trim(ppath)//"dynac.plt"
          myfrmt=''
          myfrmt="splot '"//trim(fwpath)//"' u "//trim(cols(1))//':'//trim(cols(2))//':'//trim(cols(3))
          write(50,'(A)') trim(myfrmt)
        else
          filenm(18:21)='.plt'
          write(50,2008)filenm(1:21),trim(cols(1)),trim(cols(2)),trim(cols(3))
        endif
        write(50,"('unset key')")
        write(50,"('unset label')")
! y-z
        write(50,"('set size 0.495,0.55')")
        if(iopsy.eq.1) then
! LINUX
          write(50,"('set origin 0.495,0.49')")
        else
          write(50,"('set origin 0.5,0.5')")
        endif
        write(50,'(A,A,A)') 'set xlabel "',trim(labels(3)),'"'
        write(50,'(A,A,A)') 'set ylabel "',trim(labels(4)),'"'
        write(50,"('set xrange [',f8.2,':',f8.2,']')") xmin(2),xmax(2)
        write(50,"('set yrange [',f12.6,':',f12.6,']')") ymin(2),ymax(2)
        hm=''
        vm=''
        sc=''
        write(hm,'(F8.3)') bex(22)-bex(21)
        write(vm,'(F8.3)') bex(24)-bex(23)
        write(sc,'(F8.3)') zymax
        cols(1)='('//hm//'*($1-'//ho//')/'//hr//')'
        cols(2)='('//vm//'*($2-'//vo//')/'//vr//')'
        cols(3)='(100.*$4/'//sc//')'
        if(isave.eq.0) then
          fwpath=''
          fwpath=trim(ppath)//"dynac.plt"
          myfrmt=''
          myfrmt="splot '"//trim(fwpath)//"' u "//trim(cols(1))//':'//trim(cols(2))//':'//trim(cols(3))
          write(50,'(A)') trim(myfrmt)
       else
          filenm(18:21)='.plt'
          write(50,2008)filenm(1:21),trim(cols(1)),trim(cols(2)),trim(cols(3))
        endif
! x,y,z profiles
! new start
        labels3="X, Y, Z (RMS multiples)"
        labels4="N (normalized)"
        xmin2=-5.
        xmax2=5.
        ymin2=0.
        if(iskale.eq.1) then
          if (yminsk.gt.0. .and. yminsk.lt.1.) then
            ymin2=yminsk
          else
            write(6,*) '***         Error in logscale          ***'
            write(6,*) '** log scale minimum defaults to 1.E-06 **'
            ymin2=1.e-6
          endif
        endif
        ymax2=1.
! new end
        write(50,"('set size 0.5,0.5')")
        write(50,"('set origin 0.,0.')")
        write(50,'(A,A,A)') 'set xlabel "',trim(labels3),'"'
        write(50,'(A,A,A)') 'set ylabel "',trim(labels4),'"'
        if(iskale.eq.1) then
          write(50,"('set logscale y')")
          write(50,9012)
        endif
        write(50,"('set xrange [',f8.2,':',f8.2,']')") xmin2,xmax2
        write(50,"('set yrange [',f12.6,':',f12.6,']')") ymin2,ymax2
        if(iopsy.eq.1) then
          if(s2gr) then
            write(50,"('set key at screen 0.545, 0.45 spacing 0.9', &
            & ' samplen 1 textcolor rgb variable ')")
          else
            write(50,"('set key at screen 0.545, 0.45 spacing 0.9', &
            & ' samplen 1 textcolor rgb variable ')")
          endif
        elseif(iopsy.eq.3) then
          if(s2gr) then
            write(50,"('set key at screen 0.545, 0.46 spacing 0.9', &
            &     ' samplen 1 textcolor rgb variable ')")
          else
            write(50,"('set key at screen 0.535, 0.475 spacing 0.9', &
            &     ' samplen 1 textcolor rgb variable ')")
          endif         
        else
          write(50,"('set key at screen 0.545, 0.47 spacing 1.0', &
          &     ' samplen 1 textcolor rgb variable ')")
        endif
        if(isave.eq.0) then
          fnm=pfnm(1)
          nn=len_trim(fnm)
          fnm(nn-2:nn)='pro'
          indx=' X'
          fwpath=trim(ppath)//trim(fnm)
          myfrmt=''
          myfrmt="plot '"//trim(fwpath)//"' using 1:2 title '"//indx//"' with lines, "
          write(50,'(A,A1)') trim(myfrmt),backslash
          fnm=pfnm(2)
          nn=len_trim(fnm)
          fnm(nn-2:nn)='pro'
          indx=' Y'
          fwpath=trim(ppath)//trim(fnm)
          myfrmt=''
          myfrmt="     '"//trim(fwpath)//"' using 1:2 title '"//indx//"' with lines ls 2, "
          write(50,'(A,A1)') trim(myfrmt),backslash
          fnm=pfnm(3)
          nn=len_trim(fnm)
          fnm(nn-2:nn)='pro'
          indx=' Z'
          fwpath=''
          fwpath=trim(ppath)//trim(fnm)
          myfrmt=''
          myfrmt="     '"//trim(fwpath)//"' using 1:2 title '"//indx//"' with lines ls 3"
          write(50,'(A)') trim(myfrmt)
        else
          strng=pfnm(1)
          filenm(18:20)=strng(6:8)
          filenm(21:23)='pro'
          indx=' X'
          write(50,6303) filenm,indx,backslash
          strng=pfnm(2)
          filenm(18:20)=strng(6:8)
          filenm(21:23)='pro'
          indx=' Y'
          write(50,6305) filenm,indx,backslash
          strng=pfnm(3)
          filenm(18:20)=strng(6:8)
          filenm(21:23)='pro'
          indx=' Z'
          write(50,6313) filenm,indx
        endif
        write(50,"('unset key')")
        if(iskale.eq.1) then
          write(50,"('unset logscale y')")
          write(50,9014)
        endif
!
! xp,yp,zp profiles
! new start
        labels3="Xp, Yp, Zp (RMS multiples)"
        labels4="N (normalized)"
        xmin2=-5.
        xmax2=5.
        ymin2=0.
        if(iskale.eq.1) then
          if (yminsk.gt.0. .and. yminsk.lt.1.) then
            ymin2=yminsk
          else
            ymin2=1.e-6
          endif
        endif
        ymax2=1.
! new end
        write(50,"('set size 0.5,0.5')")
        write(50,"('set origin 0.5,0.')")
        write(50,'(A,A,A)') 'set xlabel "',trim(labels3),'"'
        write(50,'(A,A,A)') 'set ylabel "',trim(labels4),'"'
        if(iskale.eq.1) then
          write(50,"('set logscale y')")
          write(50,9012)
        endif
        write(50,"('set xrange [',f8.2,':',f8.2,']')") xmin2,xmax2
        write(50,"('set yrange [',f12.6,':',f12.6,']')") ymin2,ymax2
        if(iopsy.eq.1) then
          if(s2gr) then
            write(50,"('set key at screen 0.545, 0.19 spacing 0.9', &
            & ' samplen 1 textcolor rgb variable ')")
          else
            write(50,"('set key at screen 0.545, 0.19 spacing 0.9', &
            & ' samplen 1 textcolor rgb variable ')")
          endif
        elseif(iopsy.eq.3) then
          if(s2gr) then
            write(50,"('set key at screen 0.545, 0.18 spacing 0.9', &
            &     ' samplen 1 textcolor rgb variable ')")
          else
            write(50,"('set key at screen 0.535, 0.17 spacing 0.9', &
            &     ' samplen 1 textcolor rgb variable ')")
          endif                   
        else
          write(50,"('set key at screen 0.545, 0.17 spacing 1.0', &
          &     ' samplen 1 textcolor rgb variable ')")
        endif
        if(isave.eq.0) then
          fnm=pfnm(4)
          nn=len_trim(fnm)
          fnm(nn-2:nn)='pro'
          indx='Xp'
          fwpath=''
          fwpath=trim(ppath)//trim(fnm)
          myfrmt=''
          myfrmt="plot '"//trim(fwpath)//"' using 1:2 title '"//indx//"' with lines, "
          write(50,'(A,A1)') trim(myfrmt),backslash
          fnm=pfnm(5)
          nn=len_trim(fnm)
          fnm(nn-2:nn)='pro'
          indx='Yp'
          fwpath=''
          fwpath=trim(ppath)//trim(fnm)
          myfrmt=''
          myfrmt="     '"//trim(fwpath)//"' using 1:2 title '"//indx//"' with lines ls 2, "
          write(50,'(A,A1)') trim(myfrmt),backslash
          fnm=pfnm(6)
          nn=len_trim(fnm)
          fnm(nn-2:nn)='pro'
          indx='Zp'
          fwpath=''
          fwpath=trim(ppath)//trim(fnm)
          myfrmt=''
          myfrmt="     '"//trim(fwpath)//"' using 1:2 title '"//indx//"' with lines ls 3"
          write(50,'(A)') trim(myfrmt)
        else
          strng=pfnm(4)
          filenm(18:20)=strng(6:8)
          filenm(21:23)='pro'
          indx='Xp'
          write(50,6303) filenm,indx,backslash
          strng=pfnm(5)
          filenm(18:20)=strng(6:8)
          filenm(21:23)='pro'
          indx='Yp'
          write(50,6305) filenm,indx,backslash
          strng=pfnm(6)
          filenm(18:20)=strng(6:8)
          filenm(21:23)='pro'
          indx='Zp'
          write(50,6313) filenm,indx
        endif
        write(50,"('unset key')")
        if(iskale.eq.1) then
          write(50,"('unset logscale y')")
          write(50,9014)
        endif
!
        write(50,"('unset multiplot')")
        if(.not. s2gr) then
          if(.not. dgui) write(50,'(A)')'pause -1 "hit return to continue"'
        endif  
!1008    format('splot "dynac.plt" u ',a,':',a,':',a)
2008    format('splot "',a21,'" u ',a,':',a,':',a)
5002    format('set palette defined ( 0 "white", 1 "pink", ', &
               '2 "purple", 3  "blue", 4 "green", 5 "yellow",', &
               '6 "orange", 7 "red", 8 "black" )')
!4303    format('plot "',a11,'" using 1:2 title "',A2, &
!               '" with lines, ',A1)
!4305    format('     "',a11,'" using 1:2 title "',A2, &
!               '" with lines ls 2, ',A1)
!4313    format('     "',a11,'" using 1:2 title "',A2, &
!               '" with lines ls 3')
6303    format('plot "',a23,'" using 1:2 title "',A2, &
               '" with lines, ',A1)
6305    format('     "',a23,'" using 1:2 title "',A2, &
               '" with lines ls 2, ',A1)
6313    format('     "',a23,'" using 1:2 title "',A2, &
               '" with lines ls 3')
9012    format('set format y "%.0t.E%+02T"')
9014    format('set format y "%g"')
        close (50)
        RETURN
        END SUBROUTINE wfile121
!> *******************************************************************
!! SUBROUTINE wfile20
!! xx'-yy'-xy-zz'
!< *******************************************************************
        SUBROUTINE wfile20(isave,ipn)
        implicit real(8) (a-h,o-z)
        character(len=256) :: fname,command,lfname
        character(len=256) :: ppath,fwpath,myfrmt
        character(len=255), dimension(20) :: pfnm
        character(len=255) :: txt,outtxt
        character(len=80) :: title
        character(len=40), dimension(20) :: labels
        character(len=33) :: paf
        character(len=23) :: filenm
        character(len=16) :: termtype
        character(len=255) :: strng
        character(len=7) :: parcnt
        character(len=7), dimension(20) :: cccst
        character(len=7) :: toc
        character(len=7) :: indxx
        character(len=3) :: cpn,cfn
        character(len=2) :: backslash
        common/prtcnt/imax
        common/chstat1/cst(1000002),cstat(20),fcstat(20),ncstat,mcstat
        common/chstat2/cccst
        common/wfil2/title,labels
        common/fichier/filenm,pfnm
        common/fpath/ppath
        common/iopsys/iopsy,termtype,ltt,s2gr
        common/files/fname,lpath
        common/gui/dgui
        common/grtyp/igrtyp
        logical dgui,s2gr
        common/wfil20/xmin(10),xmax(10),ymin(10),ymax(10)
        parameter (backslash="\\")
        command=''
        lfname=''
        write(parcnt,'(I7)') imax
        write(6,"(i7,' particles total')") imax
        ytitle=0.995
        if(iopsy.eq.1) then
! LINUX
          command(1:7)='rm -f "'
          command(8:lpath+7)=fname(1:lpath)
          command(lpath+8:lpath+17)='dynac.gnu"'
          ytitle=0.985
        elseif(iopsy.eq.3) then
! MAC
          command(1:7)='rm -f "'
          command(8:lpath+7)=fname(1:lpath)
          command(lpath+8:lpath+17)='dynac.gnu"'
          if(dgui) then
            if(s2gr) then
              ytitle=0.985
            else
              ytitle=0.99
            endif
          else
            if(s2gr) then
              ytitle=0.985
            else
              ytitle=0.992
            endif
          endif
        else
! WINDOWS
          if(dgui) then
            ytitle=0.99
          else
            if(s2gr) then
              ytitle=0.988
            else
              ytitle=0.985
            endif
          endif          
          command="if exist "//trim(ppath)//"dynac.gnu del "//trim(ppath)//"dynac.gnu"
        endif
        CALL System(trim(COMMAND))
        IF (isave.eq.0) then
          fwpath=''
          fwpath=trim(ppath)//"dynac.gnu"
          OPEN(unit=50,file=trim(fwpath))
        ELSE
          call fn
          filenm(1:2)='s1'
          filenm(18:21)='.gnu'
          paf(1:10)='savedplots'
          if(iopsy.eq.1) then
! LINUX
            paf(11:11)="/"
          elseif(iopsy.eq.3) then
! MAC
            paf(11:11)="/"
          else
! WINDOWS
            paf(11:11)=backslash
          endif
          paf(12:32)=filenm(1:21)
          OPEN(unit=50,file=paf(1:32))
        ENDIF
        write(50,"('set style data dots',/,'set pointsize 0.01')")
        if(mcstat.eq.1) then
          write(50,"('unset key')")
        else
          if(iopsy.eq.1) then
            if(igrtyp.eq.11) then
              if(s2gr) then
                write(50,"('set key at screen 0.59, 0.96 spacing 0.9 maxcols 1', &
              & ' samplen 1 horizontal textcolor rgb variable ')")
              elseif(dgui) then
                write(50,"('set key at screen 0.58, 0.95 spacing 0.8', &
              & ' samplen 1 textcolor rgb variable ')")
              else
                write(50,"('set key at screen 0.59, 0.95 spacing 0.8', &
              & ' samplen 1 textcolor rgb variable ')")
              endif
            else
              if(s2gr) then
                write(50,"('set key at screen 0.57, 0.96 spacing 0.8 maxcols 1', &
              & ' samplen 1 horizontal textcolor rgb variable ')")
              elseif(dgui)then
                write(50,"('set key at screen 0.555, 0.97 spacing 0.8', &
              & ' samplen 1 textcolor rgb variable ')")
              else
                write(50,"('set key at screen 0.565, 0.97 spacing 0.8', &
              & ' samplen 1 textcolor rgb variable ')")
              endif
            endif
          elseif(iopsy.eq.3) then
            if(igrtyp.eq.11) then
              if(s2gr) then
                write(50,"('set key at screen 0.59, 0.95 spacing 0.9 maxcols 1', &
              & ' samplen 1 horizontal textcolor rgb variable ')")
              else
                write(50,"('set key at screen 0.58, 0.95 spacing 0.9', &
              & ' samplen 1 textcolor rgb variable ')")
              endif
            else
              if(s2gr) then
                write(50,"('set key at screen 0.575, 0.97 spacing 0.9 maxcols 1', &
              & ' samplen 1 horizontal textcolor rgb variable ')")
              else
                write(50,"('set key at screen 0.555, 0.97 spacing 0.9 maxcols 1', &
              & ' samplen 1 horizontal textcolor rgb variable ')")
              endif             
            endif
          else
            if(igrtyp.eq.11) then
              write(50,"('set key at screen 0.585, 0.95 spacing 0.9', &
              & ' samplen 1 textcolor rgb variable ')")
            else
              if(s2gr) then
                write(50,"('set key at screen 0.565, 0.97 spacing 0.9', &
                & ' samplen 1 textcolor rgb variable ')")   
              else
                write(50,"('set key at screen 0.565, 0.97 spacing 0.8', &
                & ' samplen 1 textcolor rgb variable ')")            
              endif            
            endif
          endif
        endif
        write(50,"('set size 1.0, 1.0')")
        write(50,'(A,A,A,F5.3)') 'set label "',trim(title),'" at screen 0.13 ,',ytitle
        ipn=ipn-1
        cpn=''
        txt=''
        txt='set terminal '//termtype(1:ltt)
! number the plot window 
!2020debug
        cfn='000'
        cpn=''
        if(ipn+1.lt.10) then
          write(cpn(1:1),'(I1)') ipn+1
          write(cfn(3:3),'(I1)') ipn+1
        elseif(ipn.lt.100) then
          write(cpn(1:2),'(I2)') ipn+1
          write(cfn(2:3),'(I2)') ipn+1
        else
          write(cpn(1:3),'(I3)') ipn+1
          write(cfn(1:3),'(I3)') ipn+1
        endif   
!        write(cpn,'(I3.3)') ipn+1
        outtxt=''
        outtxt="set output '"//trim(ppath)//"dynplot"//cfn//"."//termtype(1:ltt)//"'"
        if(iopsy.eq.1) then
! LINUX
          write(50,'(A,A7,A)') 'set label "',parcnt,' particles" at screen 0.45,0.51'
          if(dgui) then
            if(s2gr) then
! then number the plot window 
              txt=trim(txt)//' '//cpn//' size 750,625'
              write(50,'(A)') trim(txt)
              write(50,'(A)') trim(outtxt)
            else
! then number the plot window and let it persist
              txt=trim(txt)//' '//cpn//' persist title "DYNAC" size 750,625'
              write(50,'(A)') trim(txt)
            endif
          else
            if(s2gr) then
! then number the plot window
              txt=trim(txt)//' size 750,625'
              write(50,'(A)') trim(txt)
              write(50,'(A)') trim(outtxt)
            else  
              txt=trim(txt)//' title "DYNAC" size 750,625'
              write(50,'(A)') trim(txt)
            endif
          endif
          ytitle=0.985
        elseif(iopsy.eq.3) then
! MAC
          write(50,'(A,A7,A)') 'set label "',parcnt,' particles" at screen 0.45,0.51'
          if(dgui) then
            if(s2gr) then
! then number the plot window 
              txt=trim(txt)//' '//cpn//' size 750,625'
              write(50,'(A)') trim(txt)
              write(50,'(A)') trim(outtxt)
            else
! then number the plot window and let it persist
              txt=trim(txt)//' '//cpn//' persist title "DYNAC" size 750,625'
              write(50,'(A)') trim(txt)
            endif
            ytitle=0.995
          else
            if(s2gr) then
! then number the plot window
              txt=trim(txt)//' size 750,625'
              write(50,'(A)') trim(txt)
              write(50,'(A)') trim(outtxt)
            else  
              txt=trim(txt)//' title "DYNAC" size 750,625'
              write(50,'(A)') trim(txt)
            endif
            ytitle=0.985
          endif
        else
! WINDOWS
          write(50,'(A,A7,A)') 'set label "',parcnt,' particles" at screen 0.45,0.52'
          if(dgui) then
            if(s2gr) then
! then number the plot window 
              txt=trim(txt)//' '//cpn//' size 817,768'
              write(50,'(A)') trim(txt)
              write(50,'(A)') trim(outtxt)
            else
! then number the plot window and let it persist
              txt=trim(txt)//' '//cpn//' persist title "DYNAC" size 817,768'
              write(50,'(A)') trim(txt)
            endif
          else
            if(s2gr) then
! then number the plot window
              txt=trim(txt)//' size 817,768'
              write(50,'(A)') trim(txt)
              write(50,'(A)') trim(outtxt)
            else  
              txt=trim(txt)//' title "DYNAC" size 817,768'
              write(50,'(A)') trim(txt)
            endif
          endif
        endif
        write(50,"('set multiplot')")
! x-xp
        write(50,"('set size 0.5,0.5')")

        if(iopsy.eq.1) then
! LINUX
          write(50,"('set origin 0.,0.49')")
        else
          write(50,"('set origin 0.,0.5')")
        endif
        write(50,'(A,A,A)') 'set xlabel "',trim(labels(1)),'"'
        write(50,'(A,A,A)') 'set ylabel "',trim(labels(2)),'"'
        write(50,"('set xrange [',f8.2,':',f8.2,']')") xmin(1),xmax(1)
        write(50,"('set yrange [',f12.5,':',f12.5,']')") ymin(1),ymax(1)
        IF(igrtyp.eq.1) then
          IF (isave.eq.0) then
            if(iopsy.le.3) then
              fwpath=''
              fwpath=trim(ppath)//"dynac.plt"
              myfrmt=''
              if(termtype.eq.'qt' .or. s2gr) then
                myfrmt="plot '"//trim(fwpath)//"' using 1:2 title '' with dots lc 8, "
                write(50,'(A,A1)') trim(myfrmt),backslash
              else
                myfrmt="plot '"//trim(fwpath)//"' using 1:2 title '' with dots lc 0, "
                write(50,'(A,A1)') trim(myfrmt),backslash
              endif
            elseif(iopsy.eq.1 .and. termtype.eq.'qt') then
              write(50,7008) backslash
!            elseif(iopsy.eq.1 .and. s2gr) then
!              write(50,7008) backslash
            else
              write(50,1008) backslash
            endif
            fwpath=''
            fwpath=trim(ppath)//"dynac.cnt"
            myfrmt=''
            myfrmt="     '"//trim(fwpath)//"' using 1:2 title '' with lines"
            write(50,'(A)') trim(myfrmt)
          ELSE
            filenm(18:21)='.plt'
            write(50,2022) filenm(1:21),backslash
            filenm(18:21)='.cnt'
            write(50,2019) filenm(1:21)
          ENDIF
        ELSE
          IF (isave.eq.0) then
            toc=cccst(1)
            indxx=toc(1:7)
            if(iopsy.le.3) then
              fwpath=''
              fwpath=trim(ppath)//trim(pfnm(1))
              myfrmt=''
              myfrmt="plot '"//trim(fwpath)//"' using 1:2 title '"//indxx//"' with dots lc 1, "
              write(50,'(A,A1)') trim(myfrmt),backslash
            else           
              write(50,3001) pfnm(1),indxx,backslash
            endif
            do j=2,mcstat
              strng=''
              write(strng,'(I2)') j
              if(j.eq.mcstat .and. igrtyp.eq.11) then
                toc=cccst(j-1)
                indxx=toc(1:4)
                if(iopsy.le.3) then
                  fwpath=''
                  fwpath=trim(ppath)//trim(pfnm(j))
                  myfrmt=''
                  myfrmt="     '"//trim(fwpath)//"' using 1:2 title ' >"//indxx//"' with dots lc "//trim(strng)//", "
                  write(50,'(A,A1)') trim(myfrmt),backslash
                else           
                  write(50,3018) pfnm(j),indxx,j,backslash
                endif
              else           
                toc=cccst(j)
                indxx=toc(1:7)
                if(iopsy.le.3) then
                  fwpath=''
                  fwpath=trim(ppath)//trim(pfnm(j))
                  myfrmt=''
                  myfrmt="     '"//trim(fwpath)//"' using 1:2 title '"//indxx//"' with dots lc "//trim(strng)//", "
                  write(50,'(A,A1)') trim(myfrmt),backslash
                else           
                  write(50,3008) pfnm(j),indxx,j,backslash
                endif
              endif
            enddo
            if(iopsy.le.3) then
              fwpath=''
              fwpath=trim(ppath)//"dynac.cnt"
              myfrmt=''
              myfrmt="     '"//trim(fwpath)//"' using 1:2 title '' with lines"
              write(50,'(A)') trim(myfrmt)
            else           
              write(50,1009)
            endif            
          ELSE
            strng=pfnm(1)
            toc=cccst(1)
            indxx=toc(1:4)
            filenm(18:23)=strng(6:11)
            write(50,4001) filenm,indxx,backslash
            do j=2,mcstat
              strng=pfnm(j)
              filenm(18:23)=strng(6:11)
              if(j.eq.mcstat .and. igrtyp.eq.11) then
                toc=cccst(j-1)
                indxx=toc(1:4)
                write(50,4018) filenm,indxx,j,backslash
              else
                toc=cccst(j)
                indxx=toc(1:4)
                write(50,4008) filenm,indxx,j,backslash
              endif
            enddo
            filenm(18:21)='.cnt'
            write(50,2019) filenm(1:21)
          ENDIF
        ENDIF
        write(50,"('unset key')")
        write(50,"('unset label')")
! y-yp
        write(50,"('set size 0.5,0.5')")
        if(iopsy.eq.1) then
! LINUX
          write(50,"('set origin 0.5,0.49')")
        else
          write(50,"('set origin 0.5,0.5')")
        endif
        write(50,'(A,A,A)') 'set xlabel "',trim(labels(3)),'"'
        write(50,'(A,A,A)') 'set ylabel "',trim(labels(4)),'"'
        write(50,"('set xrange [',f8.2,':',f8.2,']')") xmin(2),xmax(2)
        write(50,"('set yrange [',f12.5,':',f12.5,']')") ymin(2),ymax(2)
        IF(igrtyp.eq.1) then
          IF (isave.eq.0) then
            if(iopsy.le.3) then
              fwpath=''
              fwpath=trim(ppath)//"dynac.plt"
              myfrmt=''
              if(termtype.eq.'qt' .or. s2gr) then
                myfrmt="plot '"//trim(fwpath)//"' using 3:4 title '' with dots lc 8, "
                write(50,'(A,A1)') trim(myfrmt),backslash
              else
                myfrmt="plot '"//trim(fwpath)//"' using 3:4 title '' with dots lc 0, "
                write(50,'(A,A1)') trim(myfrmt),backslash
              endif
            elseif(iopsy.eq.1 .and. termtype.eq.'qt') then
              write(50,7011) backslash
            else
              write(50,1011) backslash
            endif
            fwpath=''
            fwpath=trim(ppath)//"dynac.cnt"
            myfrmt=''
            myfrmt="     '"//trim(fwpath)//"' using 3:4 title '' with lines"
            write(50,'(A)') trim(myfrmt)
          ELSE
            filenm(18:21)='.plt'
            write(50,2031) filenm(1:21),backslash
            filenm(18:21)='.cnt'
            write(50,2042) filenm(1:21)
          ENDIF
        ELSE
          IF (isave.eq.0) then
            if(iopsy.le.3) then
              fwpath=''
              fwpath=trim(ppath)//trim(pfnm(1))
              myfrmt=''
              myfrmt="plot '"//trim(fwpath)//"' using 3:4 with dots lc 1, "
              write(50,'(A,A1)') trim(myfrmt),backslash
            else           
              write(50,3002) pfnm(1),backslash
            endif
            do j=2,mcstat
              if(iopsy.le.3) then
                strng=''
                write(strng,'(I2)') j
                fwpath=''
                fwpath=trim(ppath)//trim(pfnm(j))
                myfrmt=''
                myfrmt="     '"//trim(fwpath)//"' using 3:4 with dots lc "//trim(strng)//", "
                write(50,'(A,A1)') trim(myfrmt),backslash
              else           
                write(50,3011) pfnm(j),j,backslash
              endif
            enddo
            if(iopsy.le.3) then
              fwpath=''
              fwpath=trim(ppath)//"dynac.cnt"
              myfrmt=''
              myfrmt="     '"//trim(fwpath)//"' using 3:4 with lines"
              write(50,'(A)') trim(myfrmt)
            else           
              write(50,1012)
            endif            
          ELSE
            strng=pfnm(1)
            filenm(18:23)=strng(6:11)
            write(50,4002) filenm,backslash
            do j=2,mcstat
              strng=pfnm(j)
              filenm(18:23)=strng(6:11)
              write(50,4011) filenm,j,backslash
            enddo
            filenm(18:21)='.cnt'
            write(50,2042) filenm(1:21)
          ENDIF
        ENDIF
! x-y
        write(50,"('set size 0.5,0.5')")
        write(50,"('set origin 0.,0.')")
        write(50,'(A,A,A)') 'set xlabel "',trim(labels(5)),'"'
        write(50,'(A,A,A)') 'set ylabel "',trim(labels(6)),'"'
        write(50,"('set xrange [',f8.2,':',f8.2,']')") xmin(3),xmax(3)
        write(50,"('set yrange [',f12.5,':',f12.5,']')") ymin(3),ymax(3)
        IF(igrtyp.eq.1) then
          IF (isave.eq.0) then
            if(iopsy.le.3) then
              fwpath=''
              fwpath=trim(ppath)//"dynac.plt"
              myfrmt=''
              if(termtype.eq.'qt' .or. s2gr) then
                myfrmt="plot '"//trim(fwpath)//"' using 1:3 title '' with dots lc 8"
                write(50,'(A)') trim(myfrmt)
              else
                myfrmt="plot '"//trim(fwpath)//"' using 1:3 title '' with dots lc 0"
                write(50,'(A)') trim(myfrmt)
              endif
            elseif(iopsy.eq.1 .and. termtype.eq.'qt') then
              write(50,7013)
            else
              write(50,1013)
            endif
          ELSE
            filenm(18:21)='.plt'
            write(50,2033) filenm(1:21)
          ENDIF
        ELSE
          IF (isave.eq.0) then
            if(iopsy.le.3) then
              fwpath=''
              fwpath=trim(ppath)//trim(pfnm(1))
              myfrmt=''
              myfrmt="plot '"//trim(fwpath)//"' using 1:3 with dots lc 1, "
              write(50,'(A,A1)') trim(myfrmt),backslash
            else           
              write(50,3003) pfnm(1),backslash
            endif
            do j=2,mcstat-1
              if(iopsy.le.3) then
                strng=''
                write(strng,'(I2)') j
                fwpath=''
                fwpath=trim(ppath)//trim(pfnm(j))
                myfrmt=''
                myfrmt="     '"//trim(fwpath)//"' using 1:3 with dots lc "//trim(strng)//", "
                write(50,'(A,A1)') trim(myfrmt),backslash
              else           
                write(50,3033) pfnm(j),j,backslash
              endif
            enddo
            if(iopsy.le.3) then
              strng=''
              write(strng,'(I2)') mcstat
              fwpath=''
              fwpath=trim(ppath)//trim(pfnm(mcstat))
              myfrmt=''
              myfrmt="     '"//trim(fwpath)//"' using 1:3 with dots lc "//trim(strng)
              write(50,'(A)') trim(myfrmt)
            else           
              write(50,3013) pfnm(mcstat),mcstat
            endif
          ELSE
            strng=pfnm(1)
            filenm(18:23)=strng(6:11)
            write(50,4003) filenm,backslash
            do j=2,mcstat-1
              strng=pfnm(j)
              filenm(18:23)=strng(6:11)
              write(50,4033) filenm,j,backslash
            enddo
            strng=pfnm(mcstat)
            filenm(18:23)=strng(6:11)
            write(50,4013) filenm,mcstat
          ENDIF
        ENDIF
! dW-dPHI
        write(50,"('set size 0.5,0.5')")
        write(50,"('set origin 0.5,0.')")
        write(50,'(A,A,A)') 'set xlabel "',trim(labels(7)),'"'
        write(50,'(A,A,A)') 'set ylabel "',trim(labels(8)),'"'
        write(50,"('set xrange [',f8.2,':',f8.2,']')") xmin(4),xmax(4)
        write(50,"('set yrange [',f12.5,':',f12.5,']')") ymin(4),ymax(4)
        IF(igrtyp.eq.1) then
          IF (isave.eq.0) then
            if(iopsy.le.3) then
              fwpath=''
              fwpath=trim(ppath)//"dynac.plt"
              myfrmt=''
              if(termtype.eq.'qt' .or. s2gr) then
                myfrmt="plot '"//trim(fwpath)//"' using 5:6 with dots lc 8, "
                write(50,'(A,A1)') trim(myfrmt),backslash
              else
                myfrmt="plot '"//trim(fwpath)//"' using 5:6 with dots lc 0, "
                write(50,'(A,A1)') trim(myfrmt),backslash
              endif
            elseif(iopsy.eq.1 .and. termtype.eq.'qt') then
              write(50,7014) backslash
            else
              write(50,1014) backslash
            endif
            fwpath=''
            fwpath=trim(ppath)//"dynac.cnt"
            myfrmt=''
            myfrmt="     '"//trim(fwpath)//"' using 5:6 with lines"
            write(50,'(A)') trim(myfrmt)
          ELSE
            filenm(18:21)='.plt'
            write(50,2024) filenm(1:21),backslash
            filenm(18:21)='.cnt'
            write(50,2025) filenm(1:21)
          ENDIF
        ELSE
          IF (isave.eq.0) then
            if(iopsy.le.3) then
              fwpath=''
              fwpath=trim(ppath)//trim(pfnm(1))
              myfrmt=''
              myfrmt="plot '"//trim(fwpath)//"' using 5:6 with dots lc 1, "
              write(50,'(A,A1)') trim(myfrmt),backslash
            else           
              write(50,3004) pfnm(1),backslash
            endif
            do j=2,mcstat
              if(iopsy.le.3) then
                strng=''
                write(strng,'(I2)') j
                fwpath=''
                fwpath=trim(ppath)//trim(pfnm(j))
                myfrmt=''
                myfrmt="     '"//trim(fwpath)//"' using 5:6 with dots lc "//trim(strng)//", "
                write(50,'(A,A1)') trim(myfrmt),backslash
              else           
                write(50,3014) pfnm(j),j,backslash
              endif
            enddo
            if(iopsy.le.3) then
              fwpath=''
              fwpath=trim(ppath)//"dynac.cnt"
              myfrmt=''
              myfrmt="     '"//trim(fwpath)//"' using 5:6 with lines"
              write(50,'(A)') trim(myfrmt)
            else           
              write(50,1015)
            endif            
          ELSE
            strng=pfnm(1)
            filenm(18:23)=strng(6:11)
            write(50,4004) filenm,backslash
            do j=2,mcstat
              strng=pfnm(j)
              filenm(18:23)=strng(6:11)
              write(50,4014) filenm,j,backslash
            enddo
            filenm(18:21)='.cnt'
            write(50,2025) filenm(1:21)
          ENDIF
        ENDIF
        write(50,"('unset multiplot')")
        if(.not. s2gr) then
          if(.not. dgui) write(50,'(A)')'pause -1 "hit return to continue"'
        endif  
1008    format('plot "dynac.plt" using 1:2 title "" with ', &
               'dots lc 0, ',A1)
7008    format('plot "dynac.plt" using 1:2 title "" with ', &
               'dots lc 7, ',A1)
1009    format('     "dynac.cnt" using 1:2 title "" with lines')
1011    format('plot "dynac.plt" using 3:4 with', &
               ' dots lc 0, ',A1)
7011    format('plot "dynac.plt" using 3:4 with', &
               ' dots lc 7, ',A1)
1012    format('     "dynac.cnt" using 3:4 with lines')
1013    format('plot "dynac.plt" using 1:3', &
               ' with dots lc 0')
7013    format('plot "dynac.plt" using 1:3', &
               ' with dots lc 7')
1014    format('plot "dynac.plt" using 5:6 with', &
               ' dots lc 0, ',A1)
7014    format('plot "dynac.plt" using 5:6 with', &
               ' dots lc 7, ',A1)
1015    format('     "dynac.cnt" using 5:6 with lines')
2022    format('plot "',a21,'" using 1:2 title "" with ', &
               'dots lc 0, ',A1)
2019    format('     "',a21,'" using 1:2 title "" with lines')
2031    format('plot "',a21,'" using 3:4 with', &
               ' dots lc 0, ',A1)
2042    format('     "',a21,'" using 3:4 with lines')
2033    format('plot "',a21,'" using 1:3', &
               ' with dots lc 0')
2024    format('plot "',a21,'" using 5:6 with', &
               ' dots lc 0, ',A1)
2025    format('     "',a21,'" using 5:6 with lines')
3001    format('plot "',a11,'" using 1:2 title "',A5, &
               '" with dots lc 1, ',A1)
3008    format('     "',a11,'" using 1:2 title "',A5, &
               '" with dots lc ',I2,',',A1)
3018    format('     "',a11,'" using 1:2 title " >',A5, &
               '" with dots lc ',I2,',',A1)
3002    format('plot "',a11,'" using 3:4 with', &
               ' dots lc 1, ',A1)
3011    format('     "',a11,'" using 3:4 with', &
               ' dots lc ',I2,',',A1)
3003    format('plot "',a11,'" using 1:3', &
               ' with dots lc 1, ',A1)
3033    format('     "',a11,'" using 1:3', &
               ' with dots lc ',I2,',',A1)
3013    format('     "',a11,'" using 1:3', &
               ' with dots lc ',I2)
3004    format('plot "',a11,'" using 5:6 with', &
               ' dots lc 1, ',A1)
3014    format('     "',a11,'" using 5:6 with', &
               ' dots lc ',I2,',',A1)
4001    format('plot "',a23,'" using 1:2 title "',A4, &
               '" with dots lc 1, ',A1)
4008    format('     "',a23,'" using 1:2 title "',A4, &
               '" with dots lc ',I2,',',A1)
4018    format('     "',a23,'" using 1:2 title " >',A4, &
               '" with dots lc ',I2,',',A1)
4002    format('plot "',a23,'" using 3:4 with', &
               ' dots lc 1, ',A1)
4011    format('     "',a23,'" using 3:4 with', &
               ' dots lc ',I2,',',A1)
4003    format('plot "',a23,'" using 1:3', &
               ' with dots lc 1, ',A1)
4033    format('     "',a23,'" using 1:3', &
               ' with dots lc ',I2,',',A1)
4013    format('     "',a23,'" using 1:3', &
               ' with dots lc ',I2)
4004    format('plot "',a23,'" using 5:6 with', &
               ' dots lc 1, ',A1)
4014    format('     "',a23,'" using 5:6 with', &
               ' dots lc ',I2,',',A1)
        close (50)
        RETURN
        END SUBROUTINE wfile20
!> *******************************************************************
!! SUBROUTINE wfile120
!! xx'-yy'-xy-zz' 2D plot
!< *******************************************************************
        SUBROUTINE wfile120(isave,ipn)
        implicit real(8) (a-h,o-z)
        common/prtcnt/imax
        common/wfil2/title,labels
        common/wfil20/xmin(10),xmax(10),ymin(10),ymax(10)
        common/wfil120/xxpmax,yypmax,xymax,zzpmax,zxmax,zymax,ndx,ndy,bex
        common/fichier/filenm,pfnm
        common/fpath/ppath
        common/iopsys/iopsy,termtype,ltt,s2gr
        common/files/fname,lpath
        common/gui/dgui
        logical dgui,s2gr
        character(len=256) :: fname,command,lfname
        character(len=256) :: ppath,fwpath,myfrmt
        character(len=255), dimension(20) :: pfnm
        character(len=255) :: txt,outtxt
        character(len=80) :: title
        character(len=50), dimension(3) :: cols
        character(len=40), dimension(20) :: labels
        character(len=33) :: paf
        character(len=23) :: filenm
        character(len=16) :: termtype
        character(len=8) :: hm,vm,ho,vo,hr,vr,sc
        character(len=7) :: parcnt
        character(len=3) :: cpn,cfn
        character(len=2) :: backslash
        dimension bex(30)
        parameter (backslash="\\")
        command=''
        lfname=''
        write(parcnt,'(I7)') imax
        write(6,"(i7,' particles total')") imax
        ytitle=0.985
        if(iopsy.eq.1) then
! LINUX
          command(1:7)='rm -f "'
          command(8:lpath+7)=fname(1:lpath)
          command(lpath+8:lpath+17)='dynac.gnu"'
          ytitle=0.985
        elseif(iopsy.eq.3) then
! MAC
          command(1:7)='rm -f "'
          command(8:lpath+7)=fname(1:lpath)
          command(lpath+8:lpath+17)='dynac.gnu"'
          if(dgui) then
            ytitle=0.985
          elseif(s2gr) then
            ytitle=0.98
          else
            ytitle=0.992
          endif
        else
! WINDOWS
          command="if exist "//trim(ppath)//"dynac.gnu del "//trim(ppath)//"dynac.gnu"
        endif
        CALL System(COMMAND)
        IF (isave.eq.0) then
          fwpath=''
          fwpath=trim(ppath)//"dynac.gnu"
          OPEN(unit=50,file=trim(fwpath))          
        ELSE
          call fn
          filenm(1:2)='s1'
          filenm(18:21)='.gnu'
          paf(1:10)='savedplots'
          if(iopsy.eq.1) then
! LINUX
            paf(11:11)="/"
          elseif(iopsy.eq.3) then
! MAC
            paf(11:11)="/"
          else
! WINDOWS
            paf(11:11)=backslash
          endif
          paf(12:32)=filenm(1:21)
          OPEN(unit=50,file=paf(1:32))
        ENDIF
        write(50,"('unset key')")
        write(50,"('set size 1.0, 1.0')")
        write(50,'(A,A,A,F5.3)') 'set label "',trim(title),'" at screen 0.13 ,',ytitle
        ipn=ipn-1
        cpn='   '
        txt=''
        txt='set terminal '//termtype(1:ltt)
! number the plot window 
!2020debug
        cfn='000'
        cpn=''
        if(ipn+1.lt.10) then
          write(cpn(1:1),'(I1)') ipn+1
          write(cfn(3:3),'(I1)') ipn+1
        elseif(ipn.lt.100) then
          write(cpn(1:2),'(I2)') ipn+1
          write(cfn(2:3),'(I2)') ipn+1
        else
          write(cpn(1:3),'(I3)') ipn+1
          write(cfn(1:3),'(I3)') ipn+1
        endif   
!        write(cpn,'(I3.3)') ipn+1
        outtxt=''
        outtxt="set output '"//trim(ppath)//"dynplot"//cfn//"."//termtype(1:ltt)//"'"
        if(iopsy.eq.1) then
! LINUX
          write(50,'(A,A7,A)') 'set label "',parcnt,' particles" at screen 0.44,0.51'
          txt(1:13)='set terminal '
          txt(14:13+ltt)=termtype(1:ltt)
          if(dgui) then
            if(s2gr) then
! then number the plot window 
              txt=trim(txt)//' '//cpn//' size 750,625'
              write(50,'(A)') trim(txt)
              write(50,'(A)') trim(outtxt)
            else
! then number the plot window and let it persist
              txt=trim(txt)//' '//cpn//' persist title "DYNAC" size 750,625'
              write(50,'(A)') trim(txt)
            endif
          else
            if(s2gr) then
! then number the plot window
              txt=trim(txt)//' size 750,625'
              write(50,'(A)') trim(txt)
              write(50,'(A)') trim(outtxt)
            else  
              txt=trim(txt)//' title "DYNAC" size 750,625'
              write(50,'(A)') trim(txt)
            endif
          endif
          ytitle=0.985
        elseif(iopsy.eq.3) then
! MAC
          write(50,'(A,A7,A)') 'set label "',parcnt,' particles" at screen 0.45,0.51'
          txt(1:13)='set terminal '
          txt(14:13+ltt)=termtype(1:ltt)
          if(dgui) then
            if(s2gr) then
! then number the plot window 
              txt=trim(txt)//' '//cpn//' size 750,625'
              write(50,'(A)') trim(txt)
              write(50,'(A)') trim(outtxt)
            else
! then number the plot window and let it persist
              txt=trim(txt)//' '//cpn//' persist title "DYNAC" size 750,625'
              write(50,'(A)') trim(txt)
            endif
            ytitle=0.995
          else
            if(s2gr) then
! then number the plot window
              txt=trim(txt)//' size 750,625'
              write(50,'(A)') trim(txt)
              write(50,'(A)') trim(outtxt)
            else  
              txt=trim(txt)//' title "DYNAC" size 750,625'
              write(50,'(A)') trim(txt)
            endif
            ytitle=0.985
          endif
        else
! WINDOWS
          write(50,'(A,A7,A)') 'set label "',parcnt,' particles" at screen 0.45,0.52'
          txt(1:13)='set terminal '
          txt(14:13+ltt)=termtype(1:ltt)
          if(dgui) then
            if(s2gr) then
! then number the plot window 
              txt=trim(txt)//' '//cpn//' size 817,768'
              write(50,'(A)') trim(txt)
              write(50,'(A)') trim(outtxt)
            else
! then number the plot window and let it persist
              txt=trim(txt)//' '//cpn//' persist title "DYNAC" size 817,768'
              write(50,'(A)') trim(txt)
            endif
          else
            if(s2gr) then
! then number the plot window
              txt=trim(txt)//' size 817,768'
              write(50,'(A)') trim(txt)
              write(50,'(A)') trim(outtxt)
            else  
              txt=trim(txt)//' title "DYNAC" size 817,768'
              write(50,'(A)') trim(txt)
            endif
          endif
        endif
        write(50,"('set multiplot')")
! x-xp
        write(50,"('set size 0.495,0.55')")

        if(iopsy.eq.1) then
! LINUX
          write(50,"('set origin 0.,0.49')")
        else
          write(50,"('set origin 0.,0.5')")
        endif
        write(50,'(A,A,A)') 'set xlabel "',trim(labels(1)),'"'
        write(50,'(A,A,A)') 'set ylabel "',trim(labels(2)),'"'
        write(50,"('set xrange [',f8.2,':',f8.2,']')") xmin(1),xmax(1)
        write(50,"('set yrange [',f12.5,':',f12.5,']')") ymin(1),ymax(1)
        write(50,"('set dgrid3d 20,20')")
        write(50,"('set pm3d map interpolate 0,0')")
        write(50,5002)
! splot 'dynac.plt' u (xmul*($1-10.5)/20.):(xpmul*($2-10.5)/20.):(100*$3/fmax)
        hm=''
        vm=''
        ho=''
        vo=''
        hr=''
        vr=''
        sc=''
        write(hm,'(F8.3)') bex(2)-bex(1)
        write(vm,'(F8.3)') bex(4)-bex(3)
        write(ho,'(F8.3)') float(ndx+1)/2.
        write(vo,'(F8.3)') float(ndy+1)/2.
        write(hr,'(F8.3)') float(ndx)
        write(vr,'(F8.3)') float(ndy)
        write(sc,'(F8.3)') xxpmax
        cols(1)='('//hm//'*($1-'//ho//')/'//hr//')'
        cols(2)='('//vm//'*($2-'//vo//')/'//vr//')'
        cols(3)='(100.*$3/'//sc//')'
        IF (isave.eq.0) then
          fwpath=''
          fwpath=trim(ppath)//"dynac.plt"
          myfrmt=''
          myfrmt="splot '"//trim(fwpath)//"' u "//trim(cols(1))//':'//trim(cols(2))//':'//trim(cols(3))
          write(50,'(A)') trim(myfrmt)
        ELSE
          filenm(18:21)='.plt'
          write(50,2022)filenm(1:21),trim(cols(1)),trim(cols(2)),trim(cols(3))
        ENDIF
        write(50,"('unset key')")
        write(50,"('unset label')")
! y-yp
        write(50,"('set size 0.495,0.55')")
        if(iopsy.eq.1) then
! LINUX
          write(50,"('set origin 0.495,0.49')")
        else
          write(50,"('set origin 0.5,0.5')")
        endif
        write(50,'(A,A,A)') 'set xlabel "',trim(labels(3)),'"'
        write(50,'(A,A,A)') 'set ylabel "',trim(labels(4)),'"'
        write(50,"('set xrange [',f8.2,':',f8.2,']')") xmin(2),xmax(2)
        write(50,"('set yrange [',f12.5,':',f12.5,']')") ymin(2),ymax(2)
        hm=''
        vm=''
        sc=''
        write(hm,'(F8.3)') bex(6)-bex(5)
        write(vm,'(F8.3)') bex(8)-bex(7)
        write(sc,'(F8.3)') yypmax
        cols(1)='('//hm//'*($1-'//ho//')/'//hr//')'
        cols(2)='('//vm//'*($2-'//vo//')/'//vr//')'
        cols(3)='(100.*$4/'//sc//')'
        IF (isave.eq.0) then
          fwpath=''
          fwpath=trim(ppath)//"dynac.plt"
          myfrmt=''
          myfrmt="splot '"//trim(fwpath)//"' u "//trim(cols(1))//':'//trim(cols(2))//':'//trim(cols(3))
          write(50,'(A)') trim(myfrmt)
        ELSE
          filenm(18:21)='.plt'
          write(50,2022)filenm(1:21),trim(cols(1)),trim(cols(2)),trim(cols(3))
        ENDIF
! x-y
        write(50,"('set size 0.495,0.55')")
        write(50,"('set origin 0.,0.')")
        write(50,'(A,A,A)') 'set xlabel "',trim(labels(5)),'"'
        write(50,'(A,A,A)') 'set ylabel "',trim(labels(6)),'"'
        write(50,"('set xrange [',f8.2,':',f8.2,']')") xmin(3),xmax(3)
        write(50,"('set yrange [',f12.5,':',f12.5,']')") ymin(3),ymax(3)
        hm=''
        vm=''
        sc=''
        write(hm,'(F8.3)') bex(10)-bex(9)
        write(vm,'(F8.3)') bex(12)-bex(11)
        write(sc,'(F8.3)') xymax
        cols(1)='('//hm//'*($1-'//ho//')/'//hr//')'
        cols(2)='('//vm//'*($2-'//vo//')/'//vr//')'
        cols(3)='(100.*$5/'//sc//')'
        IF (isave.eq.0) then
          fwpath=''
          fwpath=trim(ppath)//"dynac.plt"
          myfrmt=''
          myfrmt="splot '"//trim(fwpath)//"' u "//trim(cols(1))//':'//trim(cols(2))//':'//trim(cols(3))
          write(50,'(A)') trim(myfrmt)
        ELSE
          filenm(18:21)='.plt'
          write(50,2022)filenm(1:21),trim(cols(1)),trim(cols(2)),trim(cols(3))
        ENDIF
! dW-dPHI
        write(50,"('set size 0.495,0.55')")
        if(iopsy.eq.1) then
! LINUX
          write(50,"('set origin 0.495,0.')")
        else
          write(50,"('set origin 0.5,0.')")
        endif
        write(50,'(A,A,A)') 'set xlabel "',trim(labels(7)),'"'
        write(50,'(A,A,A)') 'set ylabel "',trim(labels(8)),'"'
        write(50,"('set xrange [',f8.2,':',f8.2,']')") xmin(4),xmax(4)
        write(50,"('set yrange [',f12.5,':',f12.5,']')") ymin(4),ymax(4)
        hm=''
        vm=''
        sc=''
        write(hm,'(F8.3)') bex(14)-bex(13)
        write(vm,'(F8.3)') bex(16)-bex(15)
        write(sc,'(F8.3)') zzpmax
        cols(1)='('//hm//'*($1-'//ho//')/'//hr//')'
        cols(2)='('//vm//'*($2-'//vo//')/'//vr//')'
        cols(3)='(100.*$6/'//sc//')'
        IF (isave.eq.0) then
          fwpath=''
          fwpath=trim(ppath)//"dynac.plt"
          myfrmt=''
          myfrmt="splot '"//trim(fwpath)//"' u "//trim(cols(1))//':'//trim(cols(2))//':'//trim(cols(3))
          write(50,'(A)') trim(myfrmt)
        ELSE
          filenm(18:21)='.plt'
          write(50,2022)filenm(1:21),trim(cols(1)),trim(cols(2)),trim(cols(3))
        ENDIF
        write(50,"('unset multiplot')")
        if(.not. s2gr) then
          if(.not. dgui) write(50,'(A)') 'pause -1 "hit return to continue"'
        endif  
2022    format('splot "',a21,'" u ',a,':',a,':',a)
5002    format('set palette defined ( 0 "white", 1 "pink", ', &
               '2 "purple", 3  "blue", 4 "green", 5 "yellow",', &
               '6 "orange", 7 "red", 8 "black" )')
        close (50)
        RETURN
        END SUBROUTINE wfile120
!> *******************************************************************
!! SUBROUTINE fn
!!
!< *******************************************************************
        SUBROUTINE fn
        implicit real(8) (a-h,o-z)
        common/fichier/filenm,pfnm
        common/mingw/mg
        character(len=255), dimension(20) :: pfnm
        character(len=30) :: iitime
        character(len=23) :: filenm
        logical mg
        integer(8) inttim
        inttim=time8()
        iitime=ctime(inttim)
        if (mg) then
! using MINGW style gfortran 03/30/10 20:51:06 (10 is 2010)
          if(iitime(1:2).eq.'01')filenm(3:5)='Jan'
          if(iitime(1:2).eq.'02')filenm(3:5)='Feb'
          if(iitime(1:2).eq.'03')filenm(3:5)='Mar'
          if(iitime(1:2).eq.'04')filenm(3:5)='Apr'
          if(iitime(1:2).eq.'05')filenm(3:5)='May'
          if(iitime(1:2).eq.'06')filenm(3:5)='Jun'
          if(iitime(1:2).eq.'07')filenm(3:5)='Jul'
          if(iitime(1:2).eq.'08')filenm(3:5)='Aug'
          if(iitime(1:2).eq.'09')filenm(3:5)='Sep'
          if(iitime(1:2).eq.'10')filenm(3:5)='Oct'
          if(iitime(1:2).eq.'11')filenm(3:5)='Nov'
          if(iitime(1:2).eq.'12')filenm(3:5)='Dec'
          filenm(6:7)=iitime(4:5)
          filenm(8:9)='20'
          filenm(10:11)=iitime(7:8)
          filenm(12:13)=iitime(10:11)
          filenm(14:15)=iitime(13:14)
          filenm(16:17)=iitime(16:17)
        else
! using standard gfortran Tue Mar 30 20:51:06 2010
          filenm(3:5)=iitime(5:7)
          filenm(6:7)=iitime(9:10)
          filenm(8:11)=iitime(21:24)
          filenm(12:13)=iitime(12:13)
          filenm(14:15)=iitime(15:16)
          filenm(16:17)=iitime(18:19)
        endif
! on a MAC 02 Apr is shown as 2 Apr (on windows as 02 Apr)
! fill the blank with the character '0' to avoid error message on file copy
        if(filenm(6:6).eq.' ') filenm(6:6)='0'
        return
        END SUBROUTINE fn
!> *******************************************************************
!! SUBROUTINE mkfrmt(i,fmt)
!! this routine makes a (variable) A format
!! e.g. if the integer i=11, the character format fmt will
!! be fmt=(A11)
!< *******************************************************************
        SUBROUTINE mkfrmt(i,fmt)
        implicit real(8) (a-h,o-z)
        character(len=6) :: fmt
        fmt(1:1)='('
        fmt(2:2)='A'
        fmt(3:6)=''
        if (i.lt.10) then
          fmt(3:3)=char(i+48)
          fmt(4:4)=')'
        elseif (i.lt.100) then
          j1=i/10
          j2=i-j1*10
          fmt(3:3)=char(j1+48)
          fmt(4:4)=char(j2+48)
          fmt(5:5)=')'
        elseif (i.lt.1000) then
          j1=i/100
          j=i-j1*100
          j2=j/10
          j3=j-j2*10
          fmt(3:3)=char(j1+48)
          fmt(4:4)=char(j2+48)
          fmt(5:5)=char(j3+48)
          fmt(6:6)=')'
        endif
        return
        END SUBROUTINE mkfrmt
!> *******************************************************************
!! SUBROUTINE savefile
!! 
!!
!< *******************************************************************
        SUBROUTINE savefile
        implicit real(8) (a-h,o-z)
        character(len=256) :: command,ppath,fname
        character(len=255), dimension(20) :: pfnm
        character(len=132) :: ras
        character(len=80) :: title
        character(len=40), dimension(20) :: labels
        character(len=23) :: filenm
        character(len=16) :: termtype
        character(len=255) :: strng
        character(len=6) :: fmt
        character(len=7), dimension(20) :: cccst
        character(len=2) :: backslash
        character(len=1) :: fsave
        common/prtcnt/imax
        common/wfil2/title,labels
        common/wfil2l/uxmin,uxmax,uymin,uymax
        common/wfil120/xxpmax,yypmax,xymax,zzpmax,zxmax,zymax,ndx,ndy,bex
        common/fichier/filenm,pfnm
        common/iopsys/iopsy,termtype,ltt,s2gr
        common/chstat1/cst(1000002),cstat(20),fcstat(20),ncstat,mcstat
        common/chstat2/cccst
        common/pscl/yminsk,iskale
        common/files/fname,lpath
        common/gui/dgui
        common/grtyp/igrtyp
        parameter (backslash="\\")
        common/mingw/mg
        logical mg,dgui,s2gr
        dimension bex(30)
! plot number only used in conjunction with dgui, not with plotit call from terminal
! dummy value here
        nplot=0
        if(iopsy.eq.1 .or. iopsy.eq.3) then
! LINUX or MAC
         write(6,'(a)',ADVANCE='NO')'Save plot file (y/n/p(rint)/e(xit)/q(uit)/<cr>=n)? '
        else
! WINDOWS
         write(6,'(a)',ADVANCE='NO')'Save plot file (y/n/e(xit)/q(uit)/<cr>=n)? '
        endif
        read(5,'(A)',ADVANCE='YES') fsave
        fsave=fsave(1:1)
        if (fsave.eq.' ' .or. fsave.eq.'n' .or. fsave.eq.'N')then
            write(6,*) ' No files saved'
        elseif (fsave.eq.'e' .or. fsave.eq.'E' .or. &
                fsave.eq.'q' .or. fsave.eq.'Q') then
          stop
        elseif (fsave.eq.'p' .or. fsave.eq.'P') then
! currently for linux and MAC only
          if(iopsy.eq.1 .or. iopsy.eq.3) then
! LINUX or MAC
            OPEN(unit=50,file='dynac.gnu')
            OPEN(unit=51,file='dynacp.gnu')
            write(51,7334)
            write(51,7335)
7333        read(50,'(a)',END=3456) ras
            if (ras(1:5).ne.'pause') then
              k=len_trim(ras)
              call mkfrmt(k,fmt)
              write(51,fmt) ras(1:k)
            endif
7334        FORMAT('set output "dynac.ps"')
7335        FORMAT('set term postscript color')
            goto 7333
3456        continue
            close(50)
            close(51)
            command(1:18)="gnuplot dynacp.gnu"
            CALL System(COMMAND(1:18))
            command(1:13)="lpr dynac.ps"
            CALL System(COMMAND(1:13))
          endif
        elseif (fsave.eq.'y' .or. fsave.eq.'Y') then
! filename format: sXMmmDDYYYYHHMMSSaa.eee (see user guide)
          isave=1
          IF (igrtyp.eq.1 .or. igrtyp.eq.6 .or. igrtyp.eq.11) THEN
! x-xp', y-xp', x-y, z-zp' plots
            call wfile20(isave,nplot)
            if(igrtyp.eq.1 .or. igrtyp.eq.16) then
              filenm(18:21)='.plt'
              if(iopsy.eq.1 .or. iopsy.eq.3) then
! LINUX or MAC
                command(1:24)="cp dynac.plt savedplots/"
                command(25:45)=filenm(1:21)
                CALL System(COMMAND(1:45))
                filenm(18:21)='.cnt'
                command(1:24)="cp dynac.cnt savedplots/"
                command(25:45)=filenm(1:21)
                CALL System(COMMAND(1:45))
              else
! WINDOWS
                command(1:25)="copy dynac.plt savedplots"
                command(26:26)=backslash
                command(27:47)=filenm(1:21)
                CALL System(COMMAND(1:47))
                filenm(18:21)='.cnt'
                command(1:25)="copy dynac.cnt savedplots"
                command(27:47)=filenm(1:21)
                CALL System(COMMAND(1:47))
              endif
            else
              if(iopsy.eq.1 .or. iopsy.eq.3) then
! LINUX or MAC
                do j=1,mcstat
                  strng=pfnm(j)
                  filenm(18:23)=strng(6:11)
                  command(1:3)="cp "
                  command(4:14)=pfnm(j)
                  command(15:26)=' savedplots/'
                  command(27:49)=filenm(1:23)
                  CALL System(COMMAND(1:49))
                enddo
                filenm(18:21)='.cnt'
                command(1:24)="cp dynac.cnt savedplots/"
                command(25:45)=filenm(1:21)
                CALL System(COMMAND(1:45))
              else
! WINDOWS
                do j=1,mcstat
                  strng=pfnm(j)
                  filenm(18:23)=strng(6:11)
                  command(1:5)="copy "
                  command(6:16)=pfnm(j)
                  command(17:27)=' savedplots'
                  command(28:28)=backslash
                  command(29:51)=filenm(1:23)
                  CALL System(COMMAND(1:51))
                enddo
                filenm(18:21)='.cnt'
                command(1:25)="copy dynac.cnt savedplots"
                command(26:26)=backslash
                command(27:47)=filenm(1:21)
                CALL System(COMMAND(1:47))
              endif
            endif
          ENDIF
          IF (igrtyp.eq.2 .or. igrtyp.eq.7 .or. igrtyp.eq.12) THEN
! z-x, z-y plots
            call wfile21(isave,nplot)
            if(igrtyp.eq.2) then
              filenm(18:21)='.plt'
              if(iopsy.eq.1 .or. iopsy.eq.3) then
! LINUX or MAC
                command(1:24)="cp dynac.plt savedplots/"
                command(25:45)=filenm(1:21)
                CALL System(COMMAND(1:45))
              else
! WINDOWS
                command(1:25)="copy dynac.plt savedplots"
                command(26:26)=backslash
                command(27:47)=filenm(1:21)
                CALL System(COMMAND(1:47))
              endif
            else
              if(iopsy.eq.1 .or. iopsy.eq.3) then
! LINUX or MAC
                do j=1,mcstat
                  strng=pfnm(j)
                  filenm(18:23)=strng(6:11)
                  command(1:3)="cp "
                  command(4:14)=pfnm(j)
                  command(15:26)=' savedplots/'
                  command(27:49)=filenm(1:23)
                  CALL System(COMMAND(1:49))
                enddo
              else
! WINDOWS
                do j=1,mcstat
                  strng=pfnm(j)
                  filenm(18:23)=strng(6:11)
                  command(1:5)="copy "
                  command(6:16)=pfnm(j)
                  command(17:27)=' savedplots'
                  command(28:28)=backslash
                  command(29:51)=filenm(1:23)
                  CALL System(COMMAND(1:51))
                enddo
              endif
            endif
! profiles
            filenm(20:23)='.pro'
            if(iopsy.eq.1 .or. iopsy.eq.3) then
! LINUX or MAC
              command(1:26)="cp dynac01.pro savedplots/"
              filenm(18:19)='01'
              command(27:49)=filenm(1:23)
              CALL System(COMMAND(1:49))
              command(1:26)="cp dynac02.pro savedplots/"
              filenm(18:19)='02'
              command(27:49)=filenm(1:23)
              CALL System(COMMAND(1:49))
              command(1:26)="cp dynac03.pro savedplots/"
              filenm(18:19)='03'
              command(27:49)=filenm(1:23)
              CALL System(COMMAND(1:49))
              command(1:26)="cp dynac04.pro savedplots/"
              filenm(18:19)='04'
              command(27:49)=filenm(1:23)
              CALL System(COMMAND(1:49))
              command(1:26)="cp dynac05.pro savedplots/"
              filenm(18:19)='05'
              command(27:49)=filenm(1:23)
              CALL System(COMMAND(1:49))
              command(1:26)="cp dynac06.pro savedplots/"
              filenm(18:19)='06'
              command(27:49)=filenm(1:23)
              CALL System(COMMAND(1:49))
            else
! WINDOWS
! s2Jan14200420535101.pro
              command(1:27)="copy dynac01.pro savedplots"
              command(28:28)=backslash
              filenm(18:19)='01'
              command(29:51)=filenm(1:23)
              CALL System(COMMAND(1:51))
              command(1:27)="copy dynac02.pro savedplots"
              command(28:28)=backslash
              filenm(18:19)='02'
              command(29:51)=filenm(1:23)
              CALL System(COMMAND(1:51))
              command(1:27)="copy dynac03.pro savedplots"
              command(28:28)=backslash
              filenm(18:19)='03'
              command(29:51)=filenm(1:23)
              CALL System(COMMAND(1:51))
              command(1:27)="copy dynac04.pro savedplots"
              command(28:28)=backslash
              filenm(18:19)='04'
              command(29:51)=filenm(1:23)
              CALL System(COMMAND(1:51))
              command(1:27)="copy dynac05.pro savedplots"
              command(28:28)=backslash
              filenm(18:19)='05'
              command(29:51)=filenm(1:23)
              CALL System(COMMAND(1:51))
              command(1:27)="copy dynac06.pro savedplots"
              command(28:28)=backslash
              filenm(18:19)='06'
              command(29:51)=filenm(1:23)
              CALL System(COMMAND(1:51))
            endif
          ENDIF
          IF (igrtyp.eq.3) THEN
! x,y envelopes as f(z)
            call wfile2(isave,2,nplot)
            filenm(18:21)='.plt'
            if(iopsy.eq.1 .or. iopsy.eq.3) then
! LINUX or MAC
              command(1:24)="cp dynac.plt savedplots/"
              command(25:45)=filenm(1:21)
              CALL System(COMMAND(1:45))
            else
! WINDOWS
              command(1:25)="copy dynac.plt savedplots"
              command(26:26)=backslash
              command(27:47)=filenm(1:21)
              CALL System(COMMAND(1:47))
            endif
          ENDIF
          IF (igrtyp.eq.4) THEN
! dW/W envelope as f(z)
            call wfile2(isave,3,nplot)
            filenm(18:21)='.plt'
            if(iopsy.eq.1 .or. iopsy.eq.3) then
! LINUX or MAC
              command(1:24)="cp dynac.plt savedplots/"
              command(25:45)=filenm(1:21)
              CALL System(COMMAND(1:45))
            else
! WINDOWS
              command(1:25)="copy dynac.plt savedplots"
              command(26:26)=backslash
              command(27:47)=filenm(1:21)
              CALL System(COMMAND(1:47))
            endif
          ENDIF
          IF (igrtyp.eq.5) THEN
! dPHI envelope as f(z)
            call wfile2(isave,4,nplot)
            filenm(18:21)='.plt'
            if(iopsy.eq.1 .or. iopsy.eq.3) then
! LINUX or MAC
              command(1:24)="cp dynac.plt savedplots/"
              command(25:45)=filenm(1:21)
              CALL System(COMMAND(1:45))
            else
! WINDOWS
              command(1:25)="copy dynac.plt savedplots"
              command(26:26)=backslash
              command(27:47)=filenm(1:21)
              CALL System(COMMAND(1:47))
            endif
          ENDIF
          IF (igrtyp.eq.16) THEN
! x-xp', y-xp', x-y, z-zp' density plots
            call wfile120(isave,nplot)
            if(igrtyp.eq.1 .or. igrtyp.eq.16) then
              filenm(18:21)='.plt'
              if(iopsy.eq.1 .or. iopsy.eq.3) then
! LINUX or MAC
                command(1:24)="cp dynac.plt savedplots/"
                command(25:45)=filenm(1:21)
                CALL System(COMMAND(1:45))
                filenm(18:21)='.cnt'
                command(1:24)="cp dynac.cnt savedplots/"
                command(25:45)=filenm(1:21)
                CALL System(COMMAND(1:45))
              else
! WINDOWS
                command(1:25)="copy dynac.plt savedplots"
                command(26:26)=backslash
                command(27:47)=filenm(1:21)
                CALL System(COMMAND(1:47))
                filenm(18:21)='.cnt'
                command(1:25)="copy dynac.cnt savedplots"
                command(27:47)=filenm(1:21)
                CALL System(COMMAND(1:47))
              endif
            else
              if(iopsy.eq.1 .or. iopsy.eq.3) then
! LINUX or MAC
                do j=1,mcstat
                  strng=pfnm(j)
                  filenm(18:23)=strng(6:11)
                  command(1:3)="cp "
                  command(4:14)=pfnm(j)
                  command(15:26)=' savedplots/'
                  command(27:49)=filenm(1:23)
                  CALL System(COMMAND(1:49))
                enddo
                filenm(18:21)='.cnt'
                command(1:24)="cp dynac.cnt savedplots/"
                command(25:45)=filenm(1:21)
                CALL System(COMMAND(1:45))
              else
! WINDOWS
                do j=1,mcstat
                  strng=pfnm(j)
                  filenm(18:23)=strng(6:11)
                  command(1:5)="copy "
                  command(6:16)=pfnm(j)
                  command(17:27)=' savedplots'
                  command(28:28)=backslash
                  command(29:51)=filenm(1:23)
                  CALL System(COMMAND(1:51))
                enddo
                filenm(18:21)='.cnt'
                command(1:25)="copy dynac.cnt savedplots"
                command(26:26)=backslash
                command(27:47)=filenm(1:21)
                CALL System(COMMAND(1:47))
              endif
            endif
          ENDIF
          IF (igrtyp.eq.18) THEN
! z-x, z-y density plots
            call wfile121(isave,nplot)
            filenm(18:21)='.plt'
            if(iopsy.eq.1 .or. iopsy.eq.3) then
! LINUX or MAC
              command(1:24)="cp dynac.plt savedplots/"
              command(25:45)=filenm(1:21)
              CALL System(COMMAND(1:45))
            else
! WINDOWS
              command(1:25)="copy dynac.plt savedplots"
              command(26:26)=backslash
              command(27:47)=filenm(1:21)
              CALL System(COMMAND(1:47))
            endif
! profiles
            filenm(20:23)='.pro'
            if(iopsy.eq.1 .or. iopsy.eq.3) then
! LINUX or MAC
              command(1:26)="cp dynac01.pro savedplots/"
              filenm(18:19)='01'
              command(27:49)=filenm(1:23)
              CALL System(COMMAND(1:49))
              command(1:26)="cp dynac02.pro savedplots/"
              filenm(18:19)='02'
              command(27:49)=filenm(1:23)
              CALL System(COMMAND(1:49))
              command(1:26)="cp dynac03.pro savedplots/"
              filenm(18:19)='03'
              command(27:49)=filenm(1:23)
              CALL System(COMMAND(1:49))
              command(1:26)="cp dynac04.pro savedplots/"
              filenm(18:19)='04'
              command(27:49)=filenm(1:23)
              CALL System(COMMAND(1:49))
              command(1:26)="cp dynac05.pro savedplots/"
              filenm(18:19)='05'
              command(27:49)=filenm(1:23)
              CALL System(COMMAND(1:49))
              command(1:26)="cp dynac06.pro savedplots/"
              filenm(18:19)='06'
              command(27:49)=filenm(1:23)
              CALL System(COMMAND(1:49))
            else
! WINDOWS
! s2Jan14200420535101.pro
              command(1:27)="copy dynac01.pro savedplots"
              command(28:28)=backslash
              filenm(18:19)='01'
              command(29:51)=filenm(1:23)
              CALL System(COMMAND(1:51))
              command(1:27)="copy dynac02.pro savedplots"
              command(28:28)=backslash
              filenm(18:19)='02'
              command(29:51)=filenm(1:23)
              CALL System(COMMAND(1:51))
              command(1:27)="copy dynac03.pro savedplots"
              command(28:28)=backslash
              filenm(18:19)='03'
              command(29:51)=filenm(1:23)
              CALL System(COMMAND(1:51))
              command(1:27)="copy dynac04.pro savedplots"
              command(28:28)=backslash
              filenm(18:19)='04'
              command(29:51)=filenm(1:23)
              CALL System(COMMAND(1:51))
              command(1:27)="copy dynac05.pro savedplots"
              command(28:28)=backslash
              filenm(18:19)='05'
              command(29:51)=filenm(1:23)
              CALL System(COMMAND(1:51))
              command(1:27)="copy dynac06.pro savedplots"
              command(28:28)=backslash
              filenm(18:19)='06'
              command(29:51)=filenm(1:23)
              CALL System(COMMAND(1:51))
            endif            
          ENDIF
          write(6,*) ' Saved ',filenm(1:17),' in savedplots directory'
          isave=0
        ENDIF
        return
        END SUBROUTINE savefile
!> *******************************************************************
!! SUBROUTINE wfile110
!! Writes the histogrammed data which will be plotted by GNU to a file
!< *******************************************************************
        SUBROUTINE wfile110(ndx,ndy)
        implicit real(8) (a-h,o-z)
        common/iopsys/iopsy,termtype,ltt,s2gr
        common/p2d/xxpar(100,100), yypar(100,100), xyar(100,100), &
                   zzpar(100,100), zxar(100,100), zyar(100,100)
        common/fpath/ppath
        character(len=256) :: ppath,fwpath
        character(len=280) :: command
        character(len=16) :: termtype
        logical s2gr
! store histogrammed data
        command=""
        if(iopsy.eq.1 .or. iopsy.eq.3) then
! LINUX or MAC
          command="rm -f "//trim(ppath)//"dynac.plt"
        else
! WINDOWS
          command="if exist "//trim(ppath)//"dynac.plt del "//trim(ppath)//"dynac.plt"
        endif
        CALL System(trim(COMMAND))
        fwpath=''
        fwpath=trim(ppath)//"dynac.plt"
        OPEN(unit=52,file=trim(fwpath))
        DO i=1,ndx
          DO j=1,ndy
            write(52,*) i,j,xxpar(i,j),yypar(i,j),xyar(i,j), &
                   zzpar(i,j)
          ENDDO
        ENDDO
        CLOSE(52)
        RETURN
        END SUBROUTINE wfile110
!> *******************************************************************
!! SUBROUTINE wfile111
!! Writes the Z-X and Z-Y histogrammed data to a file
!< *******************************************************************
        SUBROUTINE wfile111(ndx,ndy)
        implicit real(8) (a-h,o-z)
        common/iopsys/iopsy,termtype,ltt,s2gr
        common/p2d/xxpar(100,100), yypar(100,100), xyar(100,100), &
                   zzpar(100,100), zxar(100,100), zyar(100,100)
        common/fpath/ppath
        character(len=256) :: ppath,fwpath
        character(len=280) :: command
        character(len=16) :: termtype
        logical s2gr
! store histogrammed data
        command=""
        if(iopsy.eq.1 .or. iopsy.eq.3) then
! LINUX or MAC
          command="rm -f "//trim(ppath)//"dynac.plt"
        else
! WINDOWS
          command="if exist "//trim(ppath)//"dynac.plt del "//trim(ppath)//"dynac.plt"
        endif
        CALL System(trim(COMMAND))
        fwpath=''
        fwpath=trim(ppath)//"dynac.plt"
        OPEN(unit=52,file=trim(fwpath))
        DO i=1,ndx
          DO j=1,ndy
            write(52,*) i,j,zxar(i,j),zyar(i,j)
          ENDDO
        ENDDO
        CLOSE(52)
        END SUBROUTINE wfile111

