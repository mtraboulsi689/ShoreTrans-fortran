!> @brief
!! collection of helper functions to be used
!! inside the program
!> @details
!! common functions to be used in the program \n
!! they are split into different types: \n
!! - string manipulation and file handling \n
!! - math functions \n
!! - error handling \n
!! - misc functions \n

module st_helper
#ifndef STANDALONE
   use Constants
#endif
   use st_defaults
   interface num2str
      !> @brief convert a number to a string.
      !! handles both integer and real numbers
      !! @param[inout] num number to be converted
      !! @return string representation of the number
      module procedure num2str_int
      module procedure num2str_real
   end interface

contains

   !
   ! SECTION: string manipulation and File I/O
   !
   !> @brief clean line from comments and whitespace
   !! @details
   !! removes comments and whitespace from a line of text \n
   !! @param[inout] line line to be cleaned
   !! @author adapted from LXShore
   !! @date 2022-02-18
   !! @return cleaned line
   subroutine clean_line(line)
      implicit none
      character(len=*), intent(inout) :: line !< line to be cleaned
      integer :: space_ind, line_len, &
         quotel_ind, quoter_ind,comment_ind
      line = trim(line)
      line_len = len(trim(line))
      ! find start and end of quoted string
      quotel_ind = scan(line(1:line_len), '''')
      quoter_ind = scan(line(1:line_len),'''', .true.)
      ! handle unbalanced quotes
      if (quotel_ind .gt. 0) then
         if (quotel_ind .ge. quoter_ind) then
            call logger(0, 'Error: unbalanced quotes in line: '&
               //adj(line))
            stop
         end if
      else
         quotel_ind = line_len ! no quote was seen, set to end line
      end if

      ! check for comment before quotes
      comment_ind = scan(line(1:quotel_ind), '!') ! position of comment
      if ( comment_ind.ne.0 ) then
         line = line(1:comment_ind-1)
      end if
      ! check for comment after quotes
      comment_ind = scan(line(quoter_ind+1:line_len), '!')
      if ( comment_ind.ne.0 ) then
         line = line(1:quoter_ind)
      end if
      ! find start and end of spaces (space removal)
      space_ind = scan(line(1:line_len), ' ') ! first space
      ! removing spaces
      do while(space_ind.ne.0 .and.space_ind.lt.quotel_ind)
         ! remove space
         line = line(1:space_ind-1)//line(space_ind+1:)
         ! recalculate indices
         line = trim(line)
         line_len = len(trim(line))
         quotel_ind = scan(line(1:line_len), '''')
         quoter_ind = scan(line(1:line_len),'''', .true.)
         space_ind = scan(line(1:line_len), ' ')
         if (quotel_ind .eq. 0) quotel_ind = line_len
      end do
   end subroutine clean_line

   !> @brief convert a string to lower case
   !! @details
   !! Adapted from http://www.star.le.ac.uk/~cgp/fortran.html \n
   !! function to convert a string to lower case
   !! @author: original author Clive Page
   !! @param[in] str_in string to be converted
   !! @param[out] str_out converted string
   function to_lower(str_in) result(str_out)
      implicit none
      character(len=*), intent(in) :: str_in
      character(len=len(str_in)) :: str_out
      integer :: i,j !< loop indices
      do i = 1, len(str_in)
         j = iachar(str_in(i:i)) ! get ascii value
         if (j>= iachar("A") .and. j<=iachar("Z") ) then
            str_out(i:i) = achar(iachar(str_in(i:i))+32) ! convert to lower case
         else
            str_out(i:i) = str_in(i:i)
         end if
      end do
   end function to_lower

   !> @brief write an array to a file
   !> @param[in] out_name name of file to be written to
   !> @param[in] arr array to be written
   !> @param[in] arr2 second array to be written (optional)
   subroutine write_to(out_name, arr, arr2_)
      implicit none

      character(len=*), intent(in) :: out_name
      real(kind=8), intent (in) :: arr(:)
      real(kind=8), intent(in), optional :: arr2_(:)
      real(kind=8), allocatable :: arr2(:)
      integer :: ios, i
      integer, parameter :: fid_write = 19
      character(charlen) :: out_name_tmp, iomsg

      call execute_command_line('mkdir -p ' // adj(dir_name) // '/outputs', &
         exitstat=ios)
      if (ios .ne. 0) then
         call logger(0, 'Error: could not create outputs directory')
         stop
      end if
      out_name_tmp = adj(dir_name) &
         // '/outputs/' // adj(out_name)
      if (present(arr2_)) then
         allocate(arr2(size(arr2_)))
         arr2 = arr2_
      end if
      ! open the file (will overwrite if exists)
      open(unit=fid_write, file=adj(out_name_tmp), &
         status='unknown', iostat=ios,  &
         form = 'formatted', iomsg=iomsg)
      ! handle cases where file cannot be opened
      if (ios .ne. 0) then
         call logger(0, 'Error: could not open file ' &
            //adj(out_name_tmp)// &
            ' for writing: '//iomsg)
         stop
      end if
      ! write the array
      if (present(arr2_)) then
         do i = 1, size(arr,1)
            write(fid_write, '(F0.4, A, F0.4)') arr(i), ' ', arr2(i)
         end do
      else
         do i = 1, size(arr,1)
            write(fid_write, '(F0.4)') arr(i)
         end do
      end if

      close(fid_write)
   end subroutine write_to

   !> @brief log a message to the log file
   !> @details log a message to the log file \n
   !! priority levels: \n
   !!   0: error message \n
   !!   1: warning message \n
   !!   2: info message    \n
   !!   3: extended info message \n
   !!   4: debug message \n
   !!   -2: initialisation message  \n
   !!   -1: delete log file message \n
   !> @param[in] priority priority level of message
   !> @param[in] message message to be logged
   subroutine logger(priority, msg)
      implicit none
      integer, intent(in) :: priority
      character(len=*), intent(in) :: msg
      integer, parameter :: fid_log = 20
      integer :: ios
      character(charlen) :: log_file_tmp, iomsg
      character(10) :: date_str
      character(8) :: time_str
      ! do not log messages with priority < verbose
      if (verbose .lt. priority)  return
      ! get current date and time
      call formatted_date(date_str, time_str)
      log_file_tmp = adj(dir_name) &
         // '/log_' // date_str // '.log'

      if (priority .eq. -10) then
         open(fid_log, iostat=ios, file=adj(log_file_tmp), &
            status='old')
         if (ios .ne. 0) then
            print *, 'Error: could not open log file ' &
               //adj(log_file_tmp)// &
               ' for writing: '//iomsg
            stop
         end if
         close(fid_log, status='delete') ! delete log file
      else if (priority .eq. -2) then ! overwrites log file
         ! initialize log file
#ifdef STANDALONE
         open(unit=fid_log, file=adj(log_file_tmp), &
            status='unknown', iostat=ios, iomsg=iomsg)
         if (ios /= 0) then
            print *, 'ERROR: could not open log file: '
            print *, '>> ' ,  log_file_tmp
            print *, '>>  ', iomsg
         end if
         write(fid_log, *) 'Log file created at: ', &
            date_str , ' ' , time_str
         write(fid_log, *)  msg
#endif
      else
         if (verbose .lt. 0) return
#ifdef STANDALONE
         open(unit=fid_log, file=trim(adjustl(log_file_tmp)), &
            position='append', iostat=ios, iomsg=iomsg)
         if (ios /= 0) then
            print *, 'ERROR: could not open log file: '
            print *, '>> ', log_file_tmp
            print *, '>>  ', iomsg
         end if
#endif
         !write(fid_log, '(A)', advance='no') time_str
         print *, adj(msg)
         select case(priority)

          case(0)
            write(fid_log, *) 'ERROR: ', adj(msg)
            print *, 'ERROR: ', adj(msg)
          case(1)
            write(fid_log, *) 'WARN: ', adj(msg)
          case(2)
            ! main info message
            write(fid_log, *)  'INFO: ', adj(msg)
          case(3)
            ! more detailed info
            write(fid_log, *)  'INFO: ', adj(msg)
          case(4)
            ! debug message
            write(fid_log, *)  'DEBUG: ', adj(msg)
          case default
            ! any other messages that can be logged
            write(fid_log, *)  adj(msg)
         end select
      end if
#ifdef STANDALONE
      close(fid_log)
#endif
   end subroutine logger

   !> @brief get the directory name
   !> @details get the directory name from the argument list \n
   !! provided such as ./main.exe [dir_name] \n
   !! if no directory name is provided, use the current directory \n
   !> @return directory name
   subroutine get_dirname()
      implicit none
      integer :: num_args, err
      character(len=1024) :: temp_dir
      num_args = command_argument_count()

      if (num_args == 0) then
         ! Try to get directory from environment variable PWD
         call get_environment_variable("PWD", temp_dir, status=err)

         if (err /= 0) then
            print *, "FATAL ERROR: Can't get current directory (PWD not set)"
            stop
         end if

         dir_name = trim(temp_dir)

      else
         call get_command_argument(1, dir_name)
      end if
   end subroutine get_dirname


   !
   ! END SECTION: string manipulation and file handling
   !

   !
   ! SECTION: mathmatical functions
   !
   !> @brief equal operation for 2 reals
   !> @details equal operation for 2 reals. \n
   !! assumes that the 2 reals are within a certain tolerance (eps = 1e-6)
   !> @param[in] a first real
   !> @param[in] b second real
   !> @return true if a and b are equal, false otherwise
   function eql(a, b) result(t)
      implicit none
      real(kind=8), intent(in) :: a, b
      logical :: t
      if (abs(a-b) .lt. eps) then
         t = .true.
      else
         t = .false.
      end if
   end function eql

   !> @brief linear interpolation (similar to Matlab's interp1)
   !> @details
   !! performs the linear interpolation over array x_in given the
   !! values x0,x1,y0,y1: y = y0 + (y1-y0)/(x1-x0)*(x-x0)
   !! @param[in] x_in array of x values
   !! @param[in] x0 x value of first point
   !! @param[in] x1 x value of second point
   !! @param[in] y0 y value of first point
   !! @param[in] y1 y value of second point
   !! @return array of interpolated values (y_out: same size as x_in)
   function interp1(x0, x1, y0, y1, x_in) result(y_out)
      real(kind=8), intent(in) :: x0, x1, y0, y1
      real(kind=8), intent(in) ::  x_in(:)
      real(kind=8), allocatable :: y_out(:)
      allocate(y_out(size(x_in)))
      y_out = y0 + (x_in-x0)*(y1-y0)/(x1-x0) ! vectorized operation
   end function interp1


   !> @brief locate a value in a sorted array
   !> @details
   !! locates the index of the last element in array x_in that is less
   !! than or equal to xval. \n
   !! @author adapted from: https://github.com/astrofrog/fortranlib
   !! @param[in] x_in array of x values
   !! @param[in] xval x value to locate
   !! @return ipos index of the element
   function locate(x_in, xval) result(ipos)
      real(kind=8), intent(in) :: x_in(:)
      real(kind=8), intent(in) :: xval
      integer :: ipos
      integer :: n, ju,jl, jm ! indices
      logical :: ascnd ! ascending order
      n = size(x_in)
      ascnd = x_in(n) .gt. x_in(1) ! ascending order
      jl = 0
      ju = n + 1
      do
         if (ju-jl .le. 1) exit ! found
         jm = (ju+jl)/2 ! midpoint
         if (ascnd .eqv. (xval .ge. x_in(jm))) then
            jl = jm
         else
            ju = jm
         end if
      end do


      if (eql(xval, x_in(1))) then
         ipos = 1
      else if (eql(xval, x_in(n))) then
         ipos = n
      else if (ascnd .and. &
         (xval .gt. x_in(n) .or. xval .lt. x_in(1))) then
         ipos = -1
      else if (.not.ascnd .and. &
         (xval .lt. x_in(n) .or. xval .gt. x_in(1))) then
         ipos = -1
      else
         ipos = jl
      end if
   end function locate

   !> @brief linear interpolation (for a vector)
   !> @details
   !! performs the linear interpolation from array x1 to array x2
   !! given an array of y values y1. \n
   !! @param[in] x1 array of x values
   !! @param[in] x2 array of x values
   !! @param[in] y1 array of y values
   !! @param[out] y2 interpolated values
   function interp1_vec(x1, x2, y1) result(y2)
      real(kind=8), intent(in) :: x1(:), x2(:), y1(:)
      real(kind=8), allocatable :: y2(:)
      integer :: ipos, n2, n1, i
      n2 = size(x2)
      n1 = size(x1)
      allocate(y2(n2))
      do i=1,n2
         ipos = locate(x1, x2(i))
         if(ipos .eq. n1) then
            y2(i) = y1(n1)
         else if (ipos .eq. 0) then
            y2(i) = y1(1)
         else if(ipos .eq. -1) then
            if ((x1(n1) .gt. x1(1) .and. x2(i) .lt. x1(1)) .or. &
               (x1(n1) .lt. x1(1) .and. x2(i) .gt. x1(1))) then
               y2(i) = y1(1)
            else
               y2(i) = y1(n1)
            end if
         else
            y2(i:i) = interp1(x1(ipos), x1(ipos+1), &
               y1(ipos), y1(ipos+1), x2(i:i))
         end if
      end do
   end function interp1_vec
   !> @brief trapezoidal integration (similar to Matlab's trapz)
   !> @details
   !! performs the trapezoidal integration over array x_in given the
   !! values y_in.
   !! @param[in] x_in array of x values
   !! @param[in] y_in array of y values
   !! @return z_out trapezoidal integration of y_in
   function trapz(x_in,y_in) result(z_out)
      real(kind=8), intent(in) :: x_in(:), y_in(:)
      integer :: n
      real(kind=8):: z_out
      n = size(x_in)
      z_out = 0.5*dx*(y_in(1)+y_in(n) + 2.*sum(y_in(2:n-1)))
   end function trapz

   !
   ! END SECTION: mathmatical functions
   !

   !
   ! SECTION: miscellaneous functions
   !

   !> @brief get the current date and time
   !> @details get the current date and time in the format:
   !! YYYY_MM_DD and HH:MM:SS
   !! @param[out] date current date
   !! @param[out] time current time
   subroutine formatted_date(date, time)
      implicit none
      character(len=10), intent(out) :: date
      character(len=8), intent(out) :: time
      integer, dimension(8) :: vals
      character(10) :: date_str, time_str

      call date_and_time(values=vals)

      write(date_str, '(I4.4,A,I2.2,A,I2.2)') vals(1), '_', &
         vals(2), '_', vals(3)
      write(time_str, '(I2.2,A,I2.2,A,I2.2)') vals(5), ':', &
         vals(6), ':', vals(7)

      date = trim(adjustl(date_str))
      time = trim(adjustl(time_str))
   end subroutine formatted_date

   !> @brief trim leading and trailing spaces
   !! @param[in] str_in string to trim
   !! @return trimmed string
   function adj(str_in) result(str_out)
      implicit none
      character(len=*), intent(in) :: str_in
      character(len=:), allocatable :: str_out
      str_out = trim(adjustl(str_in))
   end function adj

   !> @brief convert an int to string
   !! @param[in] number integer to convert
   !! @return number as string
   function num2str_int(number)
      integer,intent(in) :: number
      character(len=6)   :: num2str_int
      character(len=6)   :: tmp
      write(tmp,'(I6)')number
      num2str_int = tmp
   end function

   !> @brief convert a real to string
   !! @param[in] number real to convert
   !! @return number as string
   function num2str_real(number)
      real(kind=8), intent(in)    :: number
      character(len=25)   :: num2str_real, tmp
      write(tmp,'(F25.8)') number
      num2str_real = tmp
   end function

   !> @brief get the current time (start clock)
   !! @details returns the current time in seconds to measure the
   !! execution time of a routine.
   !! @return time in seconds
   function get_time() result(t)
      implicit none
      integer :: count
      real(kind=8) :: count_rate, t

      call system_clock(count, count_rate)
      t = count * 1.d0/count_rate
   end function get_time
   !
   ! END SECTION: miscellaneous functions
   !
end module st_helper
