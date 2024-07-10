      subroutine dr3d
c     This subroutine generates the 3-D plot of the contours.

      common/conts/xcont,ycont,zcont,npts,nslc,pxlsiz
      common/vectors/numvec,xcon(200),ycon(200),zcon(200),
     &  xvec(200),yvec(200),zvec(200)
      common/maxes/xmin,xmax,ymin,ymax,zmin,zmax
      common/ctr/xcenter,ycenter,zcenter
      common/looks/worb,vecyn
      common/troch/ltslc

      character * 1 axis,vecyn,worb
      character * 80 subjid
      real pxlsiz,angle
      real xcont(300,2000),ycont(300,2000),zcont(300)
      real xmin,xmax,ymin,ymax,xtext,ytext,ztext
      integer ipt,islc,npts(300),nslc,numvec,ltslc,over,loc
      integer * 4 length,dev,xori,yori,ixsiz,iysiz,x,y
      integer * 2 ichoice,val,active
      real xcon,ycon,zcon,xvec,yvec,zvec

      include 'gl/fgl.h'
      include 'gl/fdevice.h'

c     This prevents the program from going into the background.
      call foregr

      open(50,file='subjid')
      read(50,10)subjid
10    format(a80)
      loc=index(subjid,' ')

      call keepas(1,1)
      call minsiz(1000,1000)
      length=14
      iwin=winope("contours",length)
      call mmode(MVIEWI)

c     set up double buffer mode
c     need to use swapbu to swap buffers
      call double()
      call gconfi()

c     set up queueing for mouse
      call qdevic(WINSHU)
      call qdevic(LEFTMO)
      call qdevic(RIGHTM)
      call qdevic(MIDDLE)
      call qdevic(KEYBD)

c     find size of plotting area and middle of it so that it can be translated
c     to the origin. Note that x's and y's are always positive.
      xmin=10000000
      xmax=0
      ymin=10000000
      ymax=0
      do 100 islc=1,nslc
      do 200 ipt=1,npts(islc)
      if(xcont(islc,ipt).lt.xmin)xmin=xcont(islc,ipt)
      if(xcont(islc,ipt).gt.xmax)xmax=xcont(islc,ipt)
      if(ycont(islc,ipt).lt.ymin)ymin=ycont(islc,ipt)
      if(ycont(islc,ipt).gt.ymax)ymax=ycont(islc,ipt)
200   continue
100   continue
      if(zcont(1).lt.zcont(nslc))then
        zmin=zcont(1)
        zmax=zcont(nslc)
      else
        zmin=zcont(nslc)
        zmax=zcont(1)
      endif

      xcenter=(xmin+xmax)/2.
      ycenter=(ymin+ymax)/2.
      zcenter=(zmin+zmax)/2.

      write(6,*)'center=',xcenter,ycenter,zcenter

c     Translate object to be centered about object coordinate system.
c     Don't want to use transl because transl translates object AND
c     its coordinate system.  Since all rotations occur about the
c     object coordinate system (which is fixed relative to the object)
c     any rotation would then cause a large translation as well relative
c     to the window.
      do 300 islc=1,nslc
      do 400 ipt=1,npts(islc)
        xcont(islc,ipt)=xcont(islc,ipt)-xcenter
        ycont(islc,ipt)=ycont(islc,ipt)-ycenter
400   continue
        zcont(islc)=zcont(islc)-zcenter
300   continue

c     do the same for the centroid (point 1) and
c     contact points of vectors to be drawn. 
      if(vecyn.eq.'y')then
          xvec(1)=xvec(1)-xcenter
          yvec(1)=yvec(1)-ycenter
          zvec(1)=zvec(1)-zcenter
c       after 5th vector, the even indices are for contact points.
c       odd ones are for shaft and force vectors
        do 423 i=6,numvec,2
          xvec(i)=xvec(i)-xcenter
          yvec(i)=yvec(i)-ycenter
          zvec(i)=zvec(i)-zcenter
423     continue
      endif

c     adjust limits of contours to reflect translation
      xmin=xmin-xcenter
      xmax=xmax-xcenter
      ymin=ymin-ycenter
      ymax=ymax-ycenter
      zmin=zmin-zcenter
      zmax=zmax-zcenter

c     make scaling square, allow for rotation, and
c     give an additional factor of safety in window sizing
      winmin=amin1(xmin,ymin,zmin)
      winmax=amax1(xmax,ymax,zmax)
      safe=amax1(xmax,ymax,zmax)
      winmin=winmin-safe/2.
      winmax=winmax+safe/2.

      call ortho(winmin,winmax,winmin,winmax,-winmax,-winmin)
c     write(6,*)'winmin,winmax,winmin,winmax,-winmax,-winmin'
c     write(6,*)winmin,winmax,winmin,winmax,-winmax,-winmin
c     write(6,*)'xmin,xmax=',xmin,xmax
c     write(6,*)'ymin,ymax=',ymin,ymax
c     write(6,*)'zmin,zmax=',zmin,zmax
      over=0

c     draw contours
      call rotcon(-920,'X')
      

      x=0
600   continue
       dev=qread(val)
        if(dev.eq.WINSHU)then
          go to 5000
        else if(dev.eq.REDRAW)then
          call reshap()
          call drcont
        else if(dev.eq.KEYBD.and.val.eq.61)then
c         up arrow
          if(ltslc.eq.0)then
            ltslc=1
          else
            ltslc=ltslc+1
          endif
          call drcont
        else if(dev.eq.KEYBD.and.val.eq.45)then
c         down arrow
          if(ltslc.eq.0)then
            ltslc=nslc
          else
            ltslc=ltslc-1
          endif
          call drcont
        else if(dev.eq.KEYBD.and.char(val).eq.'q')then
c         Q pressed
c         save current lesser trochanter slice number to file and quit
          open(51,file='ltfile.txt',status='new',err=800)
          write(51,*)ltslc
          close(51)
          stop
800       call color(1)
          xtext=xcont(nslc,1)
          ytext=ycont(nslc,1)
          ztext=zcont(nslc)
c         xtext=winmin+0.1*(winmax-winmin)
c         ytext=winmin+0.5*(winmax-winmin)
c         ztext=winmin+0.5*(winmax-winmin)
          call cmov(xtext,ytext,ztext)
          call charst('ltfile.txt already exists',25)
          call charst('Overwrite? (y/n)',17)
          call swapbu()
          over=1
        else if(dev.eq.KEYBD.and.char(val).eq.'x')then
          stop
        else if(dev.eq.KEYBD.and.over.eq.1.and.char(val).eq.'y')then
c         overwrite ltfile with new ltslc value and quit
          open(51,file='ltfile.txt')
          write(51,*)ltslc
          close(51)
          stop
        else if(dev.eq.KEYBD.and.over.eq.1.and.char(val).eq.'n')then
          over=0
          call drcont
        else if(dev.eq.KEYBD.and.char(val).eq.'m')then
          open(52,file='QAproblem')
          write(52,*)subjid(1:loc-1),' missing contours'
          close(52)
          stop
        else if(dev.eq.KEYBD.and.char(val).eq.'s')then
          open(52,file='QAproblem')
          write(52,*)subjid(1:loc-1),' icorrect slice thickness'
          write(52,*)subjid(1:loc-1),' resave using autoct'
          close(52)
          stop
        else if(dev.eq.KEYBD.and.char(val).eq.'f')then
          open(52,file='QAproblem')
          write(52,*)subjid(1:loc-1),' problem with head contours'
          close(52)
          stop
        else if(dev.eq.KEYBD.and.char(val).eq.'t')then
          open(52,file='QAproblem')
          write(52,*)subjid(1:loc-1),' problem with troch contours'
          close(52)
          stop
        else if(dev.eq.LEFTMO.or.dev.eq.RIGHTM.or.dev.eq.MIDDLE)then
          x=getval(MOUSEX)
          call getori(xori,yori)
          call getsiz(ixsiz,iysiz)
c         reference x to left corner of window, put x=0 at middle of window,
c         scale so that half window equals 15 degrees (900 tenths of degrees)
          x=((x-xori-ixsiz/2)*300)/ixsiz
          if(dev.eq.LEFTMO)then
            axis='X'
          else if(dev.eq.MIDDLE)then
            axis='Y'
          else if(dev.eq.RIGHTM)then
            axis='Z'
          endif
          call rotcon(x,axis)
        endif
      go to 600

5000  continue
      call gexit()

99    format(a1)
      return
      end


      subroutine drcont
c     this subroutine clears the screen and draws the contours and vectors

      common/ctr/xcenter,ycenter,zcenter
      common/conts/xcont,ycont,zcont,npts,nslc,pxlsiz
      common/maxes/xmin,xmax,ymin,ymax,zmin,zmax
      common/looks/worb,vecyn
      common/vectors/numvec,xcon(200),ycon(200),zcon(200),
     &  xvec(200),yvec(200),zvec(200)
      common/troch/ltslc

      real xcont(300,2000),ycont(300,2000),zcont(300)
      real pxlsiz
      character * 1 worb,vecyn
      integer ipt,islc,npts(300),nslc,numvec,ltslc
      integer * 4 ix,iy
      real point(3),xcon,ycon,vcon,xvec,yvec,zvec

      include 'gl/fgl.h'

      call reshap

      if(worb.eq.'b')then
        call color(0)
      else
        call color(7)
      endif
      call clear()
      if(worb.eq.'b')then
        call color(2)
      else
        call color(0)
      endif
      do 300 islc=1,nslc
       point(3)=zcont(islc)
       if(islc.eq.ltslc)call color(1)
       call bgnclo()
       do 400 ipt=1,npts(islc)
       point(1)=xcont(islc,ipt)
       point(2)=ycont(islc,ipt)
       call v3f(point)
400    continue
       call endclo()
       if(islc.eq.ltslc)then
         if(worb.eq.'b')then
           call color(2)
         else
           call color(0)
         endif
      endif
300   continue

c     write(6,*)'point:', point

      if(vecyn.eq.'y')then
c     draw vectors
      call color(4)
      do 333 i=2,4
c       write(6,*)'xvec,yvec,zvec=',xvec(i),yvec(i),zvec(i)
        call bgnlin()
c       draw centroid
        point(1)=xvec(1)
        point(2)=yvec(1)
        point(3)=zvec(1)
        call v3f(point)
c       draw direction of eigenvectors and shaft axis
c       scale vector by a convenient constant, e.g. zmax
        point(1)=xvec(1)+xvec(i)*zmax
        point(2)=yvec(1)+yvec(i)*zmax
        point(3)=zvec(1)+zvec(i)*zmax
        call v3f(point)
        call endlin()
        call cmov(point(1),point(2),point(3))
        if(i.eq.2)then
          call charst('approx neck axis',16)
        else if(i.eq.4)then
          call charst('approx posterior',16)
c       else if(i.eq.5)then
c         call charst('approx shaft axis',17)
        endif
333   continue
      call color(3)
      do 444 i=6,numvec,2
        call bgnlin()
        point(1)=xvec(i)
        point(2)=yvec(i)
        point(3)=zvec(i)
        call v3f(point)
c       write(6,*)'contact point:',point(1),point(2),point(3)
        if(i.eq.6)then
c         I want shaft to be drawn downward
          point(1)=xvec(i)+xvec(i-1)*zmax/2.0
          point(2)=yvec(i)+yvec(i-1)*zmax/2.0
          point(3)=zvec(i)+zvec(i-1)*zmax/2.0
        else
c         draw force away from contact point
          point(1)=xvec(i)-xvec(i-1)*zmax/2.0
          point(2)=yvec(i)-yvec(i-1)*zmax/2.0
          point(3)=zvec(i)-zvec(i-1)*zmax/2.0
        endif
        call v3f(point)
c       write(6,*)'force point:',point(1),point(2),point(3)
        call endlin()
        call cmov(point(1),point(2),point(3))
        if(i.eq.6)then
          call charst('approx shaft axis',17)
        else
        call charst('Force vector',12)
        endif
444   continue

      endif
      call swapbu()

      return
      end


      subroutine rotcon(rotx,axis)
c     This subroutine sets up rotation
c     note that you want to use the y coord to indicate rotation about X axis
      integer * 4 rotx
      character * 1 axis

c     call pushma
      call rotate(rotx,axis)

      call drcont
c     call popmat
      rotx=0

      return
      end
