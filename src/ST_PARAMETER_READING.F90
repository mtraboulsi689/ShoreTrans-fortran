!@brief read the data from the parameter file
!> @details
!! parameter file name should be: parameters.dat \n
!! allowed comments start with '!' \n
!! format: \n
!! variable = value (on each line) \n
!! the parameters file should be in the main directory of the test case \n
!!
module st_parameter_reading

   public :: read_parameters

   private
contains
   !@brief read the data from the parameter file
   subroutine read_parameters(current_dir)

      use st_defaults
      use st_helper
      implicit none

      integer :: ios, n, eq_ind, int_value
      character(len=*), intent(in) :: current_dir
      character(len=charlen) :: parameter_filename
      character(len=charlen) :: line, label
      character(len=charlen) :: iomsg

      ! define the parameter file name to read from
      parameter_filename =  adj(current_dir) &
         // '/parameters.dat'


      ! read parameter file
      open(fid, file=adj(parameter_filename), &
         status='old', iostat = ios, iomsg = iomsg)
      if ( ios /= 0 ) then ! can't read file
         call logger(0, 'can not open parameter file ' // &
            adj(parameter_filename))
         call logger(0,  adj(iomsg))
         stop
      end if
      ! read each parameter and assign to valueS
      n = 0
      do while (ios == 0)
         n = n + 1 ! line number
         read(fid, '(A)', iostat=ios) line
         if ( ios .gt. 0 ) then
            call logger(0, 'error reading parameter file ' // &
               adj(parameter_filename) //' at line '// &
               adj(num2str(n)))
            call logger(0,  adj(line))
            stop
         else if (ios.lt.0) then ! end of line
            call logger(3, 'end of parameter file ' // &
               adj(parameter_filename))
            exit
         end if
         call clean_line(line) ! remove spaces
         eq_ind = scan(line, '=') ! find '='
         label = to_lower(line(1:eq_ind-1))
         line = line(eq_ind+1:)
         call logger(3, 'Reading ' // adj(label) // ' = ' // &
            adj(line))
         if (label == '') then
            if (line .ne. '') then
               call logger(1, 'no equal sign in line:' // &
                  adj(line) // ' at line ' // &
                  adj(num2str(n)) )
            end if
         end if

         select case(label)
            ! filenames
          case ('xshorefilename')
            read (line, *, iostat=ios) xshorefilename
          case ('doc')
            read (line, *, iostat=ios) doc
          case ('doc_index')
            read (line, *, iostat=ios) doc_index
          case ('doc2')
            read (line, *, iostat=ios) doc2
          case ('doc2_index')
            read (line, *, iostat=ios) doc2_index
          case ('ds')
            read (line, *, iostat=ios) ds
          case ('toe_crest')
            read (line, *, iostat=ios) toe_crest
          case ('toe_crest_index')
            read (line, *, iostat=ios) toe_crest_index
          case ('dv_input')
            read (line, *, iostat=ios) dv_input
          case ('dx')
            read (line, *, iostat=ios) dx
          case ('slump')
            read (line, *, iostat=ios) slump%switch
          case ('dune_slope')
            read (line, *, iostat=ios) slump%slope
          case ('slump_cap')
            read (line, *, iostat=ios) slump%cap
          case ('rock')
            read (line, *, iostat=ios) rock
          case ('verbose')
            read (line, *, iostat=ios) verbose
          case ('rollover')
            read (line, *, iostat=ios) rollover
          case ('roll_backslope')
            read (line, *, iostat=ios) roll_backSlope
          case ('wall')
            read (line, *, iostat=ios) wall%switch
          case ('wall_x')
            read (line, *, iostat=ios) wall%x
          case ('wall_level')
            read (line, *, iostat=ios) wall%level
          case ('wall_overwash')
            read (line, *, iostat=ios) int_value
            wall%overwash = int_value .ne. 0
          case ('wall_z_min_check')
            read (line, *, iostat=ios) int_value
            wall%z_min_check = int_value .ne. 0
          case ('wall_z_min')
            read (line, *, iostat=ios) wall%z_min
          case ('wall_no_erode')
            read (line, *, iostat=ios) int_value
            wall%no_erode = int_value .ne. 0
          case ('redist_ratio')
            read (line, *, iostat=ios) wall%redist_ratio
          case ('xi_test')
            read (line, *, iostat=ios) xi_test
          case ('') ! skip empty lines
          case default
            call logger(1, 'unknown parameter ' // &
               adj(label) // ' in ' // &
               adj(parameter_filename))
         end select

         if (ios.gt.0) then
            call logger(0, 'error reading parameter file ' // &
               adj(parameter_filename) //' at line '// &
               adj(num2str(n)))
            call logger(0,  adj(line))
            stop
         end if
      end do
      close(fid)
   end subroutine read_parameters

end module st_parameter_reading
