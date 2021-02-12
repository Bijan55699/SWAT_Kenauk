      subroutine giw_drain

      use parm

      integer :: j, giw_hru_number
      real:: hru_area_ha, hru_area_m2, qdaym3, latqm3, gwqm3
      real:: giwhruarea_ha, giwhruarea_m2, inqdaymm, inlatqmm, ingwqmm

      !!INITIALIZE
      giw_hru_number = 0
      j = 0
      j = ihru
      hru_area_ha = 0.
      hru_area_m2 = 0.
      qdaym3 = 0.
      latqm3 = 0.
      gwqm3 = 0. ! added by Mohammad to include the shallow aquifer flow from upstream giws to downstream ones
      giwhruarea_ha = 0.
      giwhruarea_m2 = 0.
      inqdaymm = 0.
      inlatqmm = 0.
      ingwqmm = 0. ! Mohammad: input groundwater flow 9in mm0 from upstream HRUs

      !!DECLARE AREA VALUES
      hru_area_ha = hru_ha(j)
      hru_area_m2 = hru_area_ha * 10000.

      !!DETERMINE WHICH GIW THE SUBJECT HRU DRAINS TO
      giw_hru_number = drain_to(j)
      if (giw_hru_number == 0 ) then
        write(*,*) "giw_drain SUBROUTINE ERROR 1; EXIT"
        call Exit(1)
      end if

      giwhruarea_ha = hru_ha(giw_hru_number)
      giwhruarea_m2 = giwhruarea_ha * 10000.
      qdaym3 = (qday/1000.) * hru_area_m2
      latqm3 = (latq(j)/1000.) * hru_area_m2
      gwqm3 = (gw_q(j)/1000.) * hru_area_m2  !added by Mohammad
      inqdaymm = (qdaym3/giwhruarea_m2)*1000.
      inlatqmm = (latqm3/giwhruarea_m2)*1000.
      ingwqmm = (gwqm3/giwhruarea_m2)*1000. ! added by Mohammad

      if (sol_tmp(1,j) > 0.) then

          !!ADD HRU FLOWS TO GIW VOLUME
          giw_vol_mm(giw_hru_number) = giw_vol_mm(giw_hru_number) +
     &      inqdaymm + inlatqmm + ingwqmm

          !!ADD HRU FLOWS TO GIW FLOW IN (FOR RECORD KEEPING)
          giw_flwin_mm(giw_hru_number) = giw_flwin_mm(giw_hru_number)+
     &      inqdaymm + inlatqmm + ingwqmm

          !!ADD HRU FLOWS TO GIW FLOW IN INDIVIDUALS (FOR RECORDS ONLY)
          giw_flwin_qday_mm(giw_hru_number) =
     &      giw_flwin_qday_mm(giw_hru_number) + inqdaymm
          giw_flwin_latq_mm(giw_hru_number) =
     &      giw_flwin_latq_mm(giw_hru_number) + inlatqmm
          giw_flwin_gwq_mm(giw_hru_number) =
     &      giw_flwin_gwq_mm(giw_hru_number) + ingwqmm

          !!ADD HRU FLOWS TO FLOWIN SHARE (FOR WATER BALANCE CHECK)
          giw_flwin_share_mm = qday + latq(j) + gw_q(j)  ! the gw_q(j) is added by Mohammad

          !!ZERO HRU FLOWS
          qday = 0.
          latq(j) = 0.
          gw_q(j) = 0. ! added by Mohammad

      else

          !!ADD HRU FLOWS TO GIW VOLUME
          giw_vol_mm(giw_hru_number) = giw_vol_mm(giw_hru_number) +
     &      inqdaymm

          !!ADD HRU FLOWS TO GIW FLOW IN (FOR RECORD KEEPING)
          giw_flwin_mm(giw_hru_number) = giw_flwin_mm(giw_hru_number)+
     &      inqdaymm

          !!ADD HRU FLOWS TO GIW FLOW IN INDIVIDUALS (FOR RECORDS ONLY)
          giw_flwin_qday_mm(giw_hru_number) =
     &      giw_flwin_qday_mm(giw_hru_number) + inqdaymm

          !!ADD HRU FLOWS TO FLOWIN SHARE (FOR WATER BALANCE CHECK)
          giw_flwin_share_mm = qday

          !!ZERO HRU FLOWS
          qday = 0.

      end if

      return
      end
