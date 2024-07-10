      subroutine setup
c     Joyce Keyak

c     this subroutine writes all prompts to the screen and reads in
c     all user specified variables and file names


      common/conts/xcont,ycont,zcont,npts,nslc,pxlsiz
      common/fnames/cfname
      common/looks/worb,vecyn
      common/troch/ltslc
      common/vectors/numvec,xcon(200),ycon(200),zcon(200),
     &  xvec(200),yvec(200),zvec(200)

      character * 32 cfname(300),cfiles
      character * 1 worb,vecyn
      real pxlsiz,xcon,ycon,zcon
      real xcont(300,2000),ycont(300,2000),zcont(300)
      integer islc,nslc,npts(300),numvec,ltslc

c     make cfiles file
      call system("mkcfiles")

      vecyn='n'
c     write(6,*)'This program draws a set of contours in 3-D.'

c     write(6,*)'Enter name of file that contains contour file names'
c     read(5,3)cfiles
      open(2,file='cfiles')

c     Read contour file names and echo
c     write(6,*)'Contour File Names:'
c     write(6,*)
      islc=0
25    continue
        islc=islc+1
        read(2,3,err=30)cfname(islc)
        call rmspce(cfname(islc))
        if(islc.gt.300)then
          write(6,*)'There are more than 300 contours.'
          write(6,*)'Program cannot handle this many.'
          stop
        endif
c       write(6,*)cfname(islc)
      go to 25
30    continue
      nslc=islc-1
      close(2)

c     write(6,*)
c     write(6,*)'File names for',nslc,' contour(s) have been read'
c     write(6,*)

c     write(6,*)'Enter pixel conversion factor (mm/pixel)'
c     read(5,*)pxlsiz
c     pxlsiz=1.06
      open(12,file='morphometric')
      do 36 i=1,21
36    read(12,*)
      read(12,*)pxlsiz

c     write(6,*)
c10    write(6,*)'Do you want black or white background (b/w)?'
c     read(5,9)worb
9     format(a1)
      worb='b'
c     worb='w'
c     if(worb.ne.'b'.and.worb.ne.'w')go to 10
c     write(6,*)'Pixel size=',pxlsiz
c     write(6,*)
c     write(6,*)
c     write(6,*)
c     write(6,*)
      open(1,file='vectors',status='old',err=84)
      vecyn='y'
      do 83 i=1,200
c       read centroid (line 1), x,y,z components of unit vectors
c       and contact points with respective force vectors (alternating)
        read(1,*,err=86)xvec(i),yvec(i),zvec(i)
        numvec=i
83    continue
86    close(1)
84    continue

c     initialize ltslc -- to be drawn in a different color,
c     moved up or down and written to file.
      ltslc=0

c     check if there is already an ltfile.txt file
      open(21,file='ltfile.txt',status='old',err=22)
c       there is an existing ltfile; use that value
        read(21,*,err=22)ltslc
22    continue
      close(21)

      write(6,*)'POSITION POINTER IN GRAPHICS WINDOW.'
      write(6,*)
      write(6,*)'CLICK:'
      write(6,*)'  LEFT mouse button to rotate about X axis'
      write(6,*)'  MIDDLE mouse button to rotate about Y axis'
      write(6,*)'  RIGHT mouse button to rotate about Z axis'
      write(6,*)
      write(6,*)'MOVE POINTER TO LEFT OF CENTER FOR COUNTER-CLOCKWISE'
      write(6,*)'ROTATION, RIGHT OF CENTER FOR CLOCKWISE ROTATION.'
      write(6,*)'  (Rotation angle increases when pointer'
      write(6,*)'   is positioned farther from center.)'
      write(6,*)
      write(6,*)'Red contour should indicate center of lesser trochanter
     &.'
      write(6,*)'Use +/- keys to select a higher or lower contour.'
      write(6,*)'Type "Q" to save lesser trochanter slice number and qui
     &t.'
      write(6,*)
      write(6,*)'If there is a problem, type one of the following'
      write(6,*)'  letters to create a file called "QAproblem" and'
      write(6,*)'  the description of the problem will be written'
      write(6,*)'  to that file. conts3d will then close.'
      write(6,*)'    m = one or more contours are missing'
      write(6,*)'    s = the slice thickness is incorrect. This would'
      write(6,*)'        be the case if the contours look stretched or'
      write(6,*)'        squished in the S-I direction.'
      write(6,*)'    f = there is a problem with the femoral head'
      write(6,*)'        contours, e.g. most superior contour was'
      write(6,*)'        not derived so head looks flat on top.'
      write(6,*)'    t = there is a problem with trochanter contours'

3     format(a32)
      return
      end

      subroutine rmspce(fname)
c     This subroutine removes excess spaces (due to fixed format input)
c     from the file name.  (SGI doesn't automatically get rid of spaces.)

      character * 32 fname

      do 10 i=1,32
      if(fname(i:i).ne.' ')go to 20
10    continue
      write(6,*)'File name is all spaces.  I do not like this.'
      stop

20    continue
      fname=fname(i:len(fname))

      return
      end
