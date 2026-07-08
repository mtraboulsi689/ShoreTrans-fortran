!> @brief main logic of the program
!! @details contains the main functionalities to translate \n
!! the profile and calculate the error of the translation \n
!! only main loop is used outside of this module.
module st_translate_profile
#ifndef STANDALONE
   use constants
   use SharedVariables
#endif
   use st_defaults
   use st_helper
   use st_slump
   use st_wall_volume


   implicit none
   real(kind=8) :: xi !< translation distance (grid units, may be fractional)
   !> below this fraction of a grid cell, a shift counts as integer
   real(kind=8), parameter :: sub_tol = 1.d-9
   public :: translate_profile, xi



   private
contains

   !> @brief find the translation distance that closes the volume budget
   !! @details the volume error dv(xi) increases monotonically with xi
   !! (translating the profile offshore retains more volume), so the
   !! root of dv(xi) = 0 is found by bracketing a sign change and
   !! solving inside the bracket with Brent's method. xi is continuous:
   !! sub-grid translations are handled by get_profile, so the closure
   !! of the volume budget does not depend on dx. The Bruun estimate
   !! seeds the bracket search; dv is near-linear in xi for sandy
   !! profiles, so the seed usually lands within a few points of the root.
   subroutine translate_profile()

      integer :: x_upp, x_low, x_tmp
      real(kind=8) :: lo, hi, step
      real(kind=8) :: f_lo, f_hi
      character(len=charlen) :: msg
      allocate(z_final(n_pts))
      ! active profile height/width (also used by smooth_profile)
      h = toe_crest - doc
      w = x(doc_index) - x(toe_crest_index)

      if(.not. eql(xi_test, nanr)) then
         call logger(2, 'Using xi=' // adj(num2str(xi_test)) // ' grid points')
         xi = xi_test
         call get_profile(z_final, xi)
         call logger(2, 'Final volume error (dv): ' // adj(num2str(dv)) // ' m3')
         return
      end if
      ! translation bounds imposed by the profile extent
      x_upp = min(n_pts - doc_index -1, doc_index - 2 - toe_crest_index)
      x_low = - min( toe_crest_index-1, n_pts-doc_index-1)
      if(x_low .gt. x_upp) then
         call logger(4, 'Switching bounds as x_low > x_upp')
         x_tmp = x_low
         x_low = x_upp
         x_upp = x_tmp
      end if
      if (wall%switch.eq.1) x_low = max(x_low, 1 - wall%index)
      if (x_low .gt. x_upp) then
         call logger(0, 'No valid translation range (wall too close to '// &
            'the onshore boundary). Extend the profile onshore.')
         stop
      end if
      write(msg, '(A,I8,A,I8,A)') 'Bounds: x_upp = ', x_upp, ' grid points, x_low = ', x_low, ' grid points'
      call logger(3, adj(msg))

      ! seed with the Bruun estimate, clamped inside the bounds
      lo = min(max(bruun_estimate(), real(x_low, 8)), real(x_upp, 8))
      f_lo = evaluate_f(lo)
      hi = lo
      f_hi = f_lo

      ! expand away from the seed, doubling the step, until dv changes
      ! sign or a bound is reached. Invariant afterwards: either
      ! f_lo <= 0 <= f_hi (bracket found) or the same sign holds across
      ! the whole range (budget cannot be closed within the bounds).
      if (.not. eql(f_lo, 0.d0)) then
         step = 1.d0
         if (f_lo .gt. 0.d0) then ! volume surplus: root is onshore
            do while (f_lo .gt. 0.d0 .and. lo .gt. x_low)
               hi = lo
               f_hi = f_lo
               lo = max(real(x_low, 8), lo - step)
               f_lo = evaluate_f(lo)
               step = 2.d0 * step
            end do
         else ! volume deficit: root is offshore
            do while (f_hi .lt. 0.d0 .and. hi .lt. x_upp)
               lo = hi
               f_lo = f_hi
               hi = min(real(x_upp, 8), hi + step)
               f_hi = evaluate_f(hi)
               step = 2.d0 * step
            end do
         end if

         if (f_lo .gt. 0.d0) then
            xi = x_low
            call logger(1, 'Volume budget cannot be closed within the '// &
               'onshore bound, using xi = ' // adj(num2str(x_low)))
         else if (f_hi .lt. 0.d0) then
            xi = x_upp
            call logger(1, 'Volume budget cannot be closed within the '// &
               'offshore bound, using xi = ' // adj(num2str(x_upp)))
         else
            xi = brent_root(lo, hi, f_lo, f_hi)
         end if
      else
         xi = lo
      end if

      call get_profile(z_final, xi)
      if (wall%z_min_check) call enforce_wall_z_min(z_final, xi, x_upp)
      write(msg, '(A,F12.4,A,F12.4,A)') 'Final xi: ', xi*dx, ' m (', xi, ' grid points)'
      call logger(2, adj(msg))
      call logger(2, 'Final volume error (dv): ' // adj(num2str(dv)) // ' m3')
      if (abs(dv) .gt. h) call logger(1, 'Large volume error, check results')
   end subroutine translate_profile


   !> @brief volume error for a candidate translation distance
   function evaluate_f(xi_est)
      real(kind=8), intent(in) :: xi_est
      real(kind=8) :: evaluate_f
      character(len=charlen) :: msg
      call get_profile(z_final, xi_est)
      evaluate_f = dv
      write(msg, *) 'xi = ', xi_est, 'error = ', evaluate_f
      call logger(3, adj(msg) )
   end function evaluate_f

   !> @brief Brent's method root finder on the volume error
   !! @details standard Brent algorithm (inverse quadratic / secant
   !! steps with a bisection fallback), starting from a bracket
   !! [ax, bx] with f(ax) and f(bx) of opposite sign.
   !! Converges to |dv| < eps or an interval below xi_tol.
   !! @param[in] ax, bx the bracket (grid units)
   !! @param[in] fax, fbx volume errors at ax and bx
   !! @return the root (grid units)
   function brent_root(ax, bx, fax, fbx) result(b)
      real(kind=8), intent(in) :: ax, bx, fax, fbx
      real(kind=8) :: a, b, c, d, e, fa, fb, fc
      real(kind=8) :: p, q, r, s, tol1, xm
      integer :: iter
      integer, parameter :: itmax = 60
      real(kind=8), parameter :: xi_tol = 1.d-6 ! interval tolerance (grid units)

      a = ax
      fa = fax
      b = bx
      fb = fbx
      c = b
      fc = fb
      d = b - a
      e = d
      do iter = 1, itmax
         if ((fb .gt. 0.d0 .and. fc .gt. 0.d0) .or. &
            (fb .lt. 0.d0 .and. fc .lt. 0.d0)) then
            c = a ! reset c so that the root stays in [b, c]
            fc = fa
            d = b - a
            e = d
         end if
         if (abs(fc) .lt. abs(fb)) then ! b is the best estimate
            a = b
            b = c
            c = a
            fa = fb
            fb = fc
            fc = fa
         end if
         tol1 = 2.d0 * epsilon(1.d0) * abs(b) + 0.5d0 * xi_tol
         xm = 0.5d0 * (c - b)
         if (abs(xm) .le. tol1 .or. eql(fb, 0.d0)) return
         if (abs(e) .ge. tol1 .and. abs(fa) .gt. abs(fb)) then
            s = fb / fa ! attempt inverse quadratic interpolation
            if (eql(a, c)) then
               p = 2.d0 * xm * s
               q = 1.d0 - s
            else
               q = fa / fc
               r = fb / fc
               p = s * (2.d0*xm*q*(q - r) - (b - a)*(r - 1.d0))
               q = (q - 1.d0) * (r - 1.d0) * (s - 1.d0)
            end if
            if (p .gt. 0.d0) q = -q
            p = abs(p)
            if (2.d0*p .lt. min(3.d0*xm*q - abs(tol1*q), abs(e*q))) then
               e = d ! interpolation accepted
               d = p / q
            else
               d = xm ! fall back to bisection
               e = d
            end if
         else
            d = xm
            e = d
         end if
         a = b
         fa = fb
         if (abs(d) .gt. tol1) then
            b = b + d
         else
            b = b + sign(tol1, xm)
         end if
         fb = evaluate_f(b)
      end do
      call logger(1, 'Root finder reached max iterations')
   end function brent_root

   subroutine enforce_wall_z_min(z_tmp, xi_tmp, x_upp)
      real(kind=8), allocatable, intent(inout) :: z_tmp(:)
      real(kind=8), intent(inout) :: xi_tmp
      integer, intent(in) :: x_upp

      if (.not. wall_z_min_reached(z_tmp)) return
      do while (wall_z_min_reached(z_tmp) .and. xi_tmp .lt. real(x_upp, 8))
         xi_tmp = min(real(x_upp, 8), xi_tmp + 1.d0)
         call get_profile(z_tmp, xi_tmp)
      end do
      if (wall_z_min_reached(z_tmp)) then
         call logger(1, 'wall_z_min reached at offshore translation bound')
      else
         call logger(1, 'wall_z_min reached, reducing wall erosion')
      end if
   end subroutine enforce_wall_z_min

   logical function wall_z_min_reached(z_tmp)
      real(kind=8), dimension(:), intent(in) :: z_tmp

      wall_z_min_reached = .false.
      if (wall%switch .eq. 0 .or. .not. wall%z_min_check) return
      if (wall_z_initial) return
      if (wall%index .ge. n_pts) return
      wall_z_min_reached = z_tmp(wall%index + 1) .le. wall%z_min
   end function wall_z_min_reached

   !> @brief Estimate the shoreline recession
   !! @details Estimate the shoreline recession using the
   !! Bruun method xi = - ds * (W/h)
   !! where ds is the sea level rise and W and h are the
   !! width and height of the active profile
   !! it is used as an initial guess for the main loop
   !! @return the estimate of the shoreline recession/progression
   function bruun_estimate() result(xi_est)
      real(kind=8) :: xi_est
      ! xi is the one calculate from bruun rule
      ! plus any additional (sources/sinks)
      xi_est = ((-ds * w / h) + (dv_input / h)) / dx ! in grid units
      ! catch any xi values that are too large
      if (- xi_est .ge. toe_crest_index) then
         xi_est = real(1 - toe_crest_index, 8)
         call logger(1, "xi is too large, setting to"// &
            num2str(1 - toe_crest_index ))
      end if
      ! log calculated values
      call logger (3, 'ds = '//adj(num2str(ds)))
      call logger (3, 'h = '//adj(num2str(h)))
      call logger (3, 'w = '//adj(num2str(w)))
      call logger (3, 'Initial estimate (bruun) xi = ' &
         // adj(num2str(xi_est * dx)) // ' m (' // adj(num2str(xi_est)) &
         // ')')
   end function bruun_estimate

   !> @brief smooth the profile at the base of the active
   !! zone
   !! @details an interpolation is made from the
   !! base of the active profile (XD1, ZD1) to a point onshore/offshore
   !! of the profile
   !! a minimum interpolation distance of 10% the width is assumed
   !! @param[in] xi_tmp the shoreline recession/progression
   !! @param[inout] z_out the profile to be smoothed
   !! @return the smoothed profile
   subroutine smooth_profile(z_out, xi_tmp, z_nowall)
      integer, intent(in) :: xi_tmp
      real(kind=8), dimension(n_pts), intent(inout) :: z_out, z_nowall
      integer :: st_min, start_ind, end_ind ! smoothing profile
      ! smoothing the profile
      if (xi_tmp .le. 0) then
         st_min = nint(w * 0.1d0 / dx) ! minimum smoothing window in grid points
         start_ind = min((doc_index -1 + xi_tmp), &
            (doc_index - st_min)) ! min smoothing window
         end_ind = doc_index
      else ! xi > 0
         start_ind = doc_index - 1
         end_ind = doc_index + xi_tmp
      end if

      ! additional checks to remove errors
      if (start_ind .le. 0) then
         start_ind = 1
         call logger(1, 'The smoothing points are beyond the edge of the profile. '// &
            'Begin smoothing from the start of the profile. ')
      end if
      if(end_ind .ge. n_pts) then
         end_ind = n_pts
         call logger(1, 'The smoothing points are beyond the edge of the profile. '// &
            'End smoothing at the end of the profile.')
      end if
      z_out(start_ind+1:end_ind) =  interp1(x(start_ind), x(end_ind),&
         z_out(start_ind), z(end_ind), &
         x(start_ind+1:end_ind))
      z_nowall(start_ind+1:end_ind) = z_out(start_ind+1:end_ind)

   end subroutine smooth_profile

   !> @brief reset the elevation between the toe_crest and
   !! the end of the profile
   !! @details if rollover is off, prevent elevation increase between
   !! toe_crest and the end of the profile
   !! this allows for accretion between the wall and the crest
   !! also maintains the crest of the profile in case the profile
   !! is marching offshore
   subroutine reset_elevation(z_tmp, xi_tmp)
      real(kind=8), dimension(n_pts), intent(inout) :: z_tmp
      integer, intent(in) :: xi_tmp ! current xi value
      if (rollover .eq. 0) then
         where(z_tmp .gt. z .and. x .le. x(toe_crest_index)) z_tmp = z
      else if(rollover .eq. 2) then
         where(z_tmp .gt. z .and. x .lt. x(toe_crest_index)) z_tmp = toe_crest
      end if

      ! As the profile marches offshore,
      ! this maintains the crest of the barrier
      if(xi_tmp > 0) then
         z_tmp(toe_crest_index +1 : toe_crest_index+xi_tmp) = &
            toe_crest + ds
      end if
   end subroutine reset_elevation

   subroutine rollover_profile(z_tmp, xi_tmp)
      real(kind=8), dimension(n_pts) , intent(inout) :: z_tmp ! current z
      real(kind=8), dimension(n_pts) :: z_noWash
      integer, intent(in) :: xi_tmp ! current xi value
      real(kind=8) :: z_back, z_step, z_step_n
      integer :: ind_wash

      z_noWash = z_tmp
      ind_wash = toe_crest_index + xi_tmp
      z_back = z_tmp(ind_wash)
      z_step = dx * tan(pi/180 * roll_backSlope )
      z_step_n = z_step
      do
         ind_wash = ind_wash - 1
         if(ind_wash .le. 0) exit ! check for edge cases
         z_tmp(ind_wash) = z_back - z_step_n
         z_step_n = z_step_n + z_step
         ! if overwash has dipped below existing profile, bring it back up
         if (z_tmp(ind_wash) .le. z_noWash(ind_wash)) then
            z_tmp(ind_wash) = z_noWash(ind_wash)
            exit
         end if

      end do

   end subroutine rollover_profile

   ! raise profile that is below the rock profile
   subroutine raise_rock(z_tmp)
      real(kind=8), dimension(n_pts), intent(inout) :: z_tmp
      if(rock.eq. 0) return
      where (z_tmp .le. z_rock) z_tmp = z_rock
   end subroutine raise_rock

   !> @brief translate the profile
   !! @details translate the profile by xi (grid units, may be
   !! fractional). Integer shifts use the original index-based pipeline.
   !! Fractional shifts blend the two fully processed integer profiles
   !! that bracket xi, which keeps dv(xi) continuous while preserving
   !! exact integer behavior.
   !! @param[in] xi_tmp the shoreline recession/progression (grid units)
   !! @param[inout] z1 the profile to be translated
   subroutine get_profile(z1, xi_tmp)
      implicit none
      real(kind=8), allocatable, intent(out) :: z1(:) ! translated profile
      real(kind=8), intent(in) :: xi_tmp ! translation (grid units)
      integer :: xi_lo, xi_up ! integer shifts bracketing xi_tmp
      real(kind=8) :: frac ! fractional part of the shift
      real(kind=8), allocatable :: z_lo(:), z_up(:)
      real(kind=8) :: dv_lo, dv_up
      logical :: integer_shift

      xi_lo = floor(xi_tmp)
      frac = xi_tmp - xi_lo
      integer_shift = .false.
      if (frac .lt. sub_tol) then ! snap to an integer shift
         frac = 0.d0
         integer_shift = .true.
      else if (frac .gt. 1.d0 - sub_tol) then
         frac = 0.d0
         xi_lo = xi_lo + 1
         integer_shift = .true.
      end if
      xi_up = xi_lo
      if (frac .gt. 0.d0) xi_up = xi_lo + 1

      if (integer_shift) then
         call get_profile_integer(z1, xi_lo)
      else
         call get_profile_integer(z_lo, xi_lo)
         dv_lo = dv
         call get_profile_integer(z_up, xi_up)
         dv_up = dv

         allocate(z1(n_pts))
         z1 = (1.d0 - frac) * z_lo + frac * z_up
         dv = (1.d0 - frac) * dv_lo + frac * dv_up
      end if
   end subroutine get_profile

   !> @brief translate and post-process the profile for an integer shift
   subroutine get_profile_integer(z1, xi_tmp)
      implicit none
      real(kind=8), allocatable, intent(out) :: z1(:) ! translated profile
      integer, intent(in) :: xi_tmp ! integer translation (grid units)
      integer, dimension(:), allocatable :: active_ind ! active indices
      integer :: active_size, i ! active size and loop index
      real(kind=8) :: v0, v1 ! volumes of current and translated profiles

      active_size = doc_index - 1 - toe_crest_index - xi_tmp
      if (active_size .le. 0) then
         call logger(0, 'get_profile: active_size <= 0'// &
            ' can not translate profile')
         stop
      end if

      allocate(active_ind(active_size)) ! active zone
      allocate(z1(n_pts)) ! same dimension as the two arrays

      ! raise the profile by SLR
      z1 = z
      z1(toe_crest_index:doc_index-1) = z1(toe_crest_index:doc_index-1)&
         + ds
      z_nowall = z1 ! for wall calculation
      ! active zone is the zone that is translated
      active_ind = (/(i, i=(toe_crest_index+xi_tmp),(doc_index-1))/)
      z1(active_ind) = z1(active_ind - xi_tmp) ! move profile to the right

      if (wall%switch.eq.1.and. .not.wall%overwash) then
         z_nowall(active_ind) = z1(active_ind)
         where(x.le.x(wall%index))  z1 = z
      end if
      call raise_rock(z1) ! reset profile above rock profile
      call reset_elevation(z1, xi_tmp) ! reset elevation at the end of the profile
      call smooth_profile(z1, xi_tmp, z_nowall) ! interpolate at the end of the profile
      ! slump profile (erosion of dunes)
      if (rollover .eq. 0) then
         call slump_profile(z1, xi_tmp)
      else if (rollover .gt. 0) then ! 1 or 2
         call rollover_profile(z1, xi_tmp)
      end if
      call redistribute_volume(z1, z_nowall, xi_tmp)
      call raise_rock(z1) ! last check for rock
      ! calculate volume difference
      v0 = trapz(x, z0_rock - doc2)
      v1 = trapz(x, z1 - doc2)
      dv = v1 - v0 - dv_input ! volume difference (error)
   end subroutine get_profile_integer
end module st_translate_profile
