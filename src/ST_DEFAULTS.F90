!> @brief
!! default variables used by the whole program
!> @details
!! contains the default values for the variables used by the whole program
!!
!! created: 2022-02-18
module st_defaults
#ifndef STANDALONE
   Use Constants
   implicit none
   logical :: standalone = .false. ! run from lxshore
#else
   implicit none
   ! Constants
   real(kind=8), parameter :: nanr = -999.d0 !< not a number (real)
   integer, parameter :: nani = -999 !< not a number (integer)
   integer, parameter :: charlen = 256 !< default length of strings
   character(charlen), parameter :: nans = 'Undefined' !< undefined strings
   logical :: standalone = .true. ! standalone shoretrans
   ! specify either a z-value or an index (1-based)
   ! slump variables
   type slump_type
      integer :: switch  !< 0=no slump, 1=slump
      real(kind=8) :: slope  !< dunes get eroded, this is the final slope
      real(kind=8) :: cap   !< ignore slump above this depth.
      !! prevents slump for high cliffs
   end type slump_type
   type(slump_type) :: slump = slump_type(1, 30.d0, 100.d0) !< default values

   type wall_type
      integer :: switch  !< 0=no wall, 1=wall
      real(kind=8) :: level  !< z level at wall index
      real(kind=8) :: x ! wall x location
      integer :: index   ! wall index
      logical :: overwash ! if true, sediment is allowed to pile behind the wall
      logical :: z_min_check ! check to see if pt offshore of wall has eroded below WALL_Z_MIN
      real(kind=8) :: z_min
      logical :: no_erode ! prevents change behind wall
      real(kind=8) :: redist_ratio
   end type wall_type
   type(wall_type) :: wall = wall_type(0, nanr, nanr, nani, &
      .false., .false., -2, .true., 0.33d0) !< default values
   real(kind=8), parameter :: pi = 3.141592653589793d0 ! value of pi
   integer :: rollover = 0 !< 0=off, 1=on,2=on with no height preservation
   real(kind=8) :: roll_backSlope = 4 !< angle in degrees of the onshore slope
#endif
   ! shoreline variables
   integer :: n_pts !< number of points in shoreline (size of x,z arrays)
   real(kind=8), allocatable, dimension(:) :: x(:), z(:) !< cross shore x and z

   real(kind=8), allocatable, dimension(:) ::  z_final(:) !< final z values
   integer, parameter :: fid = 18 ! id of files (read)
   !> general options
   real(kind=8) :: ds = 0.d0 !< sea level rise (m)
   real(kind=8), parameter :: eps = 1.d-6 !< convergence criterion
   real(kind=8) :: toe_crest = nanr !< toe crest elevation (m)
   ! options for cross-shore profile
   ! Note: convention used is for z is positive
   ! upwards (unlike lxshore)
   real(kind=8) :: doc = nanr !< (upper) depth of closure (m)
   real(kind=8) :: doc2 = nanr  !< (lower) depth of closure (m)
   integer :: doc_index = nani !< index of dc
   integer :: doc2_index = nani !< index of dc2
   integer :: toe_crest_index = nani !< index of toe_crest
   ! internal variables
   character(charlen) :: dir_name = '' !< directory of case to be run
   character(charlen) :: xshorefilename = nans !< name of file containing x and z
   ! temp variables for interpolation
   real(kind=8), allocatable, dimension(:) :: x_tmp, z_tmp, rock_tmp
   !> verbosity level
   !! 0 = only errors
   !! 1 = errors and warnings
   !! 2 = errors, warnings and info
   !! 3 = errors, warnings, extra info
   !! 4 = errors, warnings, extra info and debug
   !! -1 = no output
   integer :: verbose = 2

   real(kind=8) :: xi_test = nanr !< force a fixed translation (grid units, may be fractional)

   ! initial variables
   real(kind=8) :: h  !< height of active profile
   real(kind=8) :: w  !< width of active profile
   real(kind=8) :: dx = nanr !< cross-shore step size
   real(kind=8) :: dv !< volume difference between adjacent profiles
   real(kind=8) :: dv_input = 0.d0 !< input volume change
   ! options for rock layer
   integer :: rock = 0 !< 0=no rock, 1=rock
   real(kind=8), allocatable :: z_rock(:) !< rock layer z-values
   real(kind=8), allocatable :: z0_rock(:) !< to calculate the volume
   real(kind=8), allocatable :: z_nowall(:) !< before the wall
   logical :: wall_z_initial = .false. ! has erosion exeeded z_min for wall
end module st_defaults
