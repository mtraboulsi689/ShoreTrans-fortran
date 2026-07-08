!> @brief initialization module
!! @details
!! module to set the initial values of variables \n
!! checks if some variables are not initialized and sets them to default values
!! also reads the cross shore profile

module st_initialization

#ifndef STANDALONE
   use Constants
   use SharedVariables
#endif
   use st_defaults
   use st_helper
   use st_error_checking
   implicit none
   public :: setup_shoretrans
   private

contains


   !> @brief initialization subroutine
   !> @details
   !> subroutine to set the initial values of variables \n
   !> checks if some variables are not initialized and sets them to default values
   !> also reads the cross shore profile
   subroutine setup_shoretrans
      ! check errors
      call check_errors()
      ! verbose output
      if (verbose .EQ. -1) call logger(-10, 'Deleting log file') ! delete log file
      call logger(3, 'logging level set to ' // num2str(verbose))

      ! read cross shore profile
      if(xshorefilename.NE.nans) call read_xshore()
      dx = X(2) - X(1)

      allocate(z0_rock(n_pts))

      call logger(3, 'Number of points in profile: ' // &
         adj(num2str(n_pts)))

      call setup_toe_crest ! setup the values of toe_crest & index
      call setup_doc ! setup doc level
      call setup_rock_layer ! rock layer
      call setup_wall ! setup wall level
      ! sediment flux (volume), default is 0.0
      if (.not. eql(dv_input, 0.d0)) then ! volume change
         call logger(3, 'Sediment (budget/deficit) = ' // &
            adj(num2str(dv_input)))
      end if

      if (rollover .EQ. 0) then
         ! slump (erosion of dunes) default is True, i.e. slump
         call logger(3, 'slumping set to: ' // &
            adj(num2str(slump%switch)) // ' with slope: ' // &
            adj(num2str(slump%slope)) // ' and cap: ' // &
            adj(num2str(slump%cap)))
      else
         call logger(3, 'Rollover set to: '// adj(num2str(rollover)) // &
            ' with back slope angle (deg): ' // adj(num2str(roll_backSlope)) )

      end if


   end subroutine setup_shoretrans

   subroutine setup_wall
      integer, dimension(n_pts) :: tmp_array
      if (wall%switch .eq. 0) return
      call logger(3, "Seawall is on")
      if (eql(wall%x, nanr)) then ! no wall_x, use wall_level
         tmp_array = 1
         where(z.lt.wall%level) tmp_array = 0
         if (all(tmp_array .eq. 1)) then
            call logger(0, 'No profile point is below wall_level')
            stop
         end if
         wall%index = minloc(tmp_array, 1)
         wall%x = x(wall%index)
         call logger(3, 'wall level is ' // adj(num2str(wall%level)))
         call logger(3, 'wall index is ' // adj(num2str(wall%x)))
      else
         ! wall_x is set directly
         wall%index = minloc(abs(x - wall%x), 1)
         wall%level = z(wall%index)

         call logger(3, 'wall index is ' // adj(num2str(wall%x)))
         call logger(3, 'wall level is ' // adj(num2str(wall%level)))

      end if

      ! wall fully depleted
      if (wall%index .lt. n_pts) then
         if (z(wall%index + 1) .lt. wall%z_min) wall_z_initial =.true.
      end if
   end subroutine setup_wall

   subroutine setup_rock_layer
      ! setup z value onshore
      where ((x .le. x(toe_crest_index)) .and. (z .lt. z_rock)) z = z_rock
      if (rock.eq.1) call logger(3, 'rock layer enabled')
      z0_rock = z
      where(z_rock .gt. z) z0_rock = z_rock
   end subroutine setup_rock_layer

   subroutine setup_toe_crest
      integer, dimension(n_pts) :: tmp_array
      ! defaults for the toe/crest values
      ! if not set, then the default values are the
      ! maximum of the profile
      if (eql(toe_crest , nanr) .and. (toe_crest_index.eq.nani)) then
         toe_crest = maxval(z)
         tmp_array = 1
         where(z .ge. toe_crest) tmp_array = 0
         toe_crest_index = n_pts - minloc(tmp_array(n_pts:1:-1), 1)+1

         call logger(1, 'none of the toe/crest values were set, '// &
            'using the maximum of the profile')
         call logger(3, 'setting toe/crest value: ' // &
            adj(num2str(toe_crest)) // ' at index: ' // &
            adj(num2str(toe_crest_index)))
      end if

      ! ! if only one option is not set,
      ! then use it to set the other option
      if (toe_crest_index.EQ.nani) then
         tmp_array = 1
         where(z .ge. toe_crest) tmp_array = 0
         toe_crest_index = n_pts - minloc(tmp_array(n_pts:1:-1), 1)+1
         call logger(3, 'setting toe/crest index: ' // &
            adj(num2str(toe_crest_index)))
      else if (eql(toe_crest,nanr)) then
         toe_crest = z(toe_crest_index)
         call logger(3, 'toe/crest index: ' // &
            adj(num2str(toe_crest_index)))
         call logger(3, 'setting toe/crest value: ' // &
            adj(num2str(toe_crest)))
      end if
   end subroutine setup_toe_crest


   subroutine setup_doc
      integer, dimension(n_pts) :: tmp_array
      ! Depth of closure (1 and 2)
      ! set up doc_index (upper depth of closure)
      if (eql(doc , nanr)) then
         doc = minval(z)
         call logger(1, 'Depth of closure not set, using doc=: ' &
            // adj(num2str(doc)))
      else
         call logger(3, 'Depth of closure set to: ' // &
            adj(num2str(doc)))
      end if

      tmp_array = 1
      where(z .ge. doc) tmp_array = 0
      doc_index = n_pts - minloc(tmp_array(n_pts:1:-1), 1) + 2
      if(doc_index.gt.size(z)) then
         call logger(1, 'DOC level not reached. Using min(z).')
         doc_index = size(z)
      end if
      call logger(3, 'Depth of closure index set to: ' // &
         adj(num2str(doc_index)))

      !! doc2
      !todo: is this really necessary?
      if (eql(doc2 , nanr)) then
         doc2 = doc - 0.1d0
         call logger(1, 'Depth of closure 2 not set, using doc2= ' &
            // adj(num2str(doc2)))
      else
         call logger(3, 'Depth of closure 2 set to: ' // &
            adj(num2str(doc2)))
      end if
      tmp_array = 1
      where(z .ge. doc2) tmp_array = 0
      doc2_index = n_pts - minloc(tmp_array(n_pts:1:-1), 1) + 2
      if(doc2_index.gt.size(z)) then
         call logger(1, 'DOC2 index not reached, using min(z)')
         doc2_index = size(z)
      end if
      call logger(3, 'Depth of closure 2 index set to: ' // &
         adj(num2str(doc2_index)))
   end subroutine setup_doc

   subroutine read_xshore()
      implicit none
      integer :: n_tmp, ios, i
      character(charlen) :: line
      ! read cross-shore profile
      !> cross-shore filename
      xshorefilename = adj(dir_name)  // '/inputs/' // xshorefilename
      call logger(3, 'Reading cross-shore profile from ' // &
         adj(xshorefilename))

      open(unit=fid, file=adj(xshorefilename), status='old'&
         , iostat=ios)
      ! if ios is not 0, an error occured
      if ( ios /= 0 ) then
         call logger(0, 'Error opening file ' // &
            adj(xshorefilename))
         close(fid)
         stop
      end if
      ! skip comment lines
      do
         read(fid, '(A)', iostat=ios) line
         call clean_line(line)
         if (line .ne. '') exit ! skip comment lines
      end do

      ! we expect the first line to be the number of points
      read(line, *, iostat=ios) n_tmp
      allocate(x_tmp(n_tmp), z_tmp(n_tmp), rock_tmp(n_tmp))

      ! read the rest of the file
      if (rock .ne. 1) then ! no rock profile
         do i = 1, n_tmp
            read(fid, *, iostat=ios) x_tmp(i), z_tmp(i)
         end do
         rock_tmp = z_tmp - 100.
      else
         do i = 1, n_tmp
            read(fid, *, iostat=ios) x_tmp(i), z_tmp(i), rock_tmp(i)
         end do
      end if
      close(fid)
      n_pts = n_tmp
      call initialize_profile(x,z)
   end subroutine read_xshore

   subroutine initialize_profile(x,z)
      real(kind=8), dimension(:), allocatable :: x, z
      real(kind=8):: dx_tmp ! current dx
      integer i
      dx_tmp = x_tmp(2) - x_tmp(1) ! assume uniform spacing
      ! check if we need to interpolate the cross-shore profile
      if (dx.le.0) then ! dx not set
         dx = dx_tmp
         allocate(x(n_pts), z(n_pts), z_rock(n_pts))
         x = x_tmp
         z = z_tmp
         z_rock = rock_tmp
      else
         ! dx should always be +ve
         n_pts = nint((x_tmp(n_pts) - x_tmp(1)) / dx) +1
         allocate(x(n_pts), z(n_pts), z_rock(n_pts))
         do i=1,n_pts
            x(i) = x_tmp(1) + dx * (i-1)
         end do
         ! interpolate new profile
         z = interp1_vec(x_tmp, x, z_tmp)
         if (rock .eq. 0) then
            z_rock = z - 100.d0
         else
            z_rock = interp1_vec(x_tmp, x, rock_tmp)
         end if
      end if
      deallocate(x_tmp,z_tmp,rock_tmp)
   end subroutine initialize_profile
end module st_initialization
