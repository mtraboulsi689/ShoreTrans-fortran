module st_wall_volume
   ! calculates the (hypothetical) volume eroded from "onshore of the wall"
#ifndef STANDALONE
   use Constants
   use SharedVariables
#endif
   use st_defaults
   use st_helper

   implicit none

   public :: redistribute_volume
   private
contains


   subroutine redistribute_volume(z1, z_nowall, xi_tmp)
      ! redistribute the volume behind the wall
      real(kind=8), dimension(n_pts), intent(inout) :: z1
      real(kind=8), dimension(n_pts), intent(in) :: z_nowall
      integer, intent(in) :: xi_tmp ! current xi value
      real(kind=8) :: dv_behind_wall, len_walldoc
      integer, dimension(n_pts) :: tmp_ind
      integer :: ind_st, ind_en


      if(wall%switch .eq. 0 ) return
      if(wall_z_initial) then
         z1 = z
         return ! already at max erosion
      end if
      call wall_volume(z1, z_nowall, xi_tmp, dv_behind_wall)
      where(z1 .lt. doc .and. x .le. x(doc_index)) z1 = doc
      if (dv_behind_wall.le. eps) return ! no volume to redistribute

      ! find point that is offshore the wall
      tmp_ind = 1
      where( (x .ge. x(wall%index)) .and. (z1.ge.z_rock)) tmp_ind = 0

      ind_st = minloc(tmp_ind,1)

      len_walldoc = x(doc_index) - x(ind_st)
      if (len_walldoc.le.eps) then
         ! no wall redistribution
         call logger(1, 'Profile is fully depleted, no wall redistribution possible')
         return
      end if

      tmp_ind = 1
      where (x .ge. x(ind_st) + wall%redist_ratio * len_walldoc)&
         tmp_ind = 0
      ind_en =  minloc(tmp_ind,1)
      if (ind_en .le. ind_st .or. x(ind_en) - x(ind_st) .le. eps) then
         call logger(1, 'Wall redistribution span is too short')
         return
      end if
      ! linear interpolation to redistribute volume from wall
      ! to cross-over point
      z1(ind_st:ind_en) = z1(ind_st:ind_en) - &
         interp1(x(ind_st), x(ind_en), &
         2 * dv_behind_wall/ (x(ind_en)-x(ind_st)), 0.d0, &
         x(ind_st:ind_en))
      where(z1 .lt. doc .and. x .le. x(doc_index)) z1 = doc
   end subroutine redistribute_volume

   subroutine wall_volume(z1, z_nowall, xi_tmp, dv_behind_wall)
      ! calculate volume behind wall
      real(kind=8), dimension(n_pts), intent(inout) :: z1
      real(kind=8), dimension(n_pts), intent(in) :: z_nowall
      integer, intent(in) :: xi_tmp ! current xi value
      real(kind=8), intent(out) :: dv_behind_wall
      real(kind=8), dimension(n_pts) :: z_nowall_cap, z_tocr_block
      real(kind=8) :: toe_crest2, v_nowall,  v_tocr_block

      toe_crest2 = toe_crest + ds
      z_nowall_cap = z_nowall
      if (wall%no_erode) then
         where(x.lt.x(wall%index)) z1 = z
      end if

      if (wall%index + xi_tmp .le. 0) then
         call logger(0, 'cannot translate enough behind wall. increase onshore profile')
         stop
      end if

      ! puts a cap on z_noWall at toe/crest level post-SLR
      z_nowall_cap(1:wall%index + xi_tmp) = toe_crest2
      where( (z_nowall_cap .gt. toe_crest2) .and. &
         (x .lt. x(wall%index))) z_nowall_cap = toe_crest2
      ! calculate volume behind wall
      v_nowall = trapz(x(1:wall%index), z_nowall_cap(1:wall%index) + doc)

      ! calculation of volume behind the wall
      z_tocr_block = toe_crest2
      where( (z .le. toe_crest2 ) .and. &
         (x.gt.x(wall%index +xi_tmp)) .and. &
         (x.lt.x(wall%index)) ) z_tocr_block = z

      v_tocr_block = trapz(x(1:wall%index), z_tocr_block(1:wall%index) +doc)

      dv_behind_wall = v_tocr_block - v_nowall
   end subroutine wall_volume

end module st_wall_volume
