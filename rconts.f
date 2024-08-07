      subroutine rconts
c Joyce Keyak

c This subroutine reads the contour data.

c Local variables:
c irow(ipt), icol(ipt) = row, col coordinates of digital CT scan data of contour
c point ipt.irow, icol corresponds to Y, X after multiplication by the
c pixel size

         common/conts/xcont, ycont, zcont, npts, nslc, pxlsiz
         common/fnames/cfname

         character*32 cfname(300)
         real xcont(300, 2000), ycont(300, 2000), zcont(300), pxlsiz
         integer ipt, islc, irow(2000), icol(2000), npts(300), nslc

         do 100 islc = 1, nslc
            open (1, file=cfname(islc), status='old')
            read (1, *) npts(islc), zcont(islc)
            ! write (6, *) 'reading slice', islc, 'npts=', npts(islc)

c scaling by CT scan pixel size
            do 200 ipt = 1, npts(islc)
               read (1, *) irow(ipt), icol(ipt)
               xcont(islc, ipt) = real(icol(ipt))*pxlsiz
               ycont(islc, ipt) = real(irow(ipt))*pxlsiz
200            continue

               close (1)
100            continue

            end
