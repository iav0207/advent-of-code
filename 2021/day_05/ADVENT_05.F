      program advent_2021_05
         implicit none
         integer                          :: n, m, p, i, j, part
         integer, dimension(4, 500)       :: f
         integer, dimension(500, 4)       :: a
         integer, dimension(1000, 1000)   :: field

         part = 2 ! part of the task spec, one of [1, 2]

         n = 500  ! number of lines
         m = 4    ! numbers per input row
         p = 1000 ! field size (both dimensions)

c        Read the inputs

         read (*,*) f
         a = transpose(f)

c        Draw the lines

         do i = 1,n
            if (part.gt.1.or.orthog(a(i,:))) then
               call draw(field, a(i,:))
            end if
         end do
c        call print_matrix(field, p, p)

c        Print results
         print *, count(field.ge.2)

      contains

      logical function orthog(a) result(x)
         integer, intent(in)  :: a(4)

         x = a(1).eq.a(3).or.a(2).eq.a(4)

      end function orthog

      subroutine draw(diagram, line)
         integer              :: diagram(:,:)
         integer, intent(in)  :: line(4)
         integer              :: x1, y1, x2, y2
         integer              :: i, x, y, dx, dy, xlen, ylen

         x1 = line(1);  y1 = line(2)
         x2 = line(3);  y2 = line(4)
         xlen = x2-x1;  dx = merge(0, sign(1, xlen), xlen.eq.0)
         ylen = y2-y1;  dy = merge(0, sign(1, ylen), ylen.eq.0)

         x = x1;        y = y1;
         do i = 1,max(abs(xlen),abs(ylen))+1
            diagram(x,y) = diagram(x,y) + 1
            x = x + dx
            y = y + dy
         end do

      end subroutine draw

      subroutine print_matrix(array, n, m)

         integer, intent(in) :: array(n,m)
         integer, intent(in) :: n, m
         integer :: i

         do i = 1,n
            print*, array(i,:)
         end do

      end subroutine print_matrix

      end program  advent_2021_05

