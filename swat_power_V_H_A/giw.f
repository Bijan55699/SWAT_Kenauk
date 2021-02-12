      subroutine giw

      use parm

      integer :: j
      real, parameter :: pi = 3.1416
      real :: giw_vol_m3
      real :: giw_sa_ha, giw_seepage_out_m3,a,b
      real :: hru_area_ha, hru_area_m2
      real :: k_to_use

      !!INITIALIZE
      giw_vol_m3 = 0.
      giw_seepage_out_m3 = 0.
      giw_sa_ha = 0.
      hru_area_ha = 0.
      hru_area_m2 = 0.
      k_to_use = 0.
      j = 0
	a=0.
	b=0.

      !!DECLARE SUBJECT HRU NUMBER
      j = ihru

      !!IF GIW_FLAG IS 0 THEN THIS MODULE WAS CALLED IN ERROR.
      if (giw_flag(j) == 0) then
        write(*,*) "giw SUBROUTINE ERROR 1"
        call Exit(1)
      end if

      !!DECLARE AREA VALUES
      hru_area_ha = hru_ha(j)
      hru_area_m2 = hru_area_ha * 10000.

      !!ADD PRECIP TO GIW RESERVOIR
      giw_precip_in_mm(j) = 0.
      giw_precip_in_mm(j) = precipday
      giw_vol_mm(j) = giw_vol_mm(j) + giw_precip_in_mm(j)
      
      !! Kenauk V-H relationship: V = 192.28*hmax
      !if (j==55) then
       !   giw_vol_m3 = 34.85 * (giw_vol_mm(j)/1000.) 
      !else
          giw_vol_m3 = 192.28 * (giw_vol_mm(j)/1000.) 
      !end if
      
      
      
      !!giw_vol_m3 = (giw_vol_mm(j)/1000.)*hru_area_m2
      
      !!! Mohammad: reduce the VoloutRAF_prev from the RAF feature
      !if (RAF_flag(j) == 1) then
      !    giw_vol_m3 = giw_vol_m3 - VoloutRAF_prev(j)
      !    if (giw_vol_m3 <= 0.) then
      !        giw_vol_m3 = 0.
      !    end if
      !    giw_sa_m2(j) = giw_vol_m3/0.96535
      !    giw_vol_mm(j) = (giw_vol_m3*1000.)/hru_area_m2
      !    VoloutRAF_prev(j) = 0.         
      !end if
          
      

      !!CALCULATE GIW SURFACE AREA
      !!giw_vol_m3 = (giw_vol_mm(j)/1000.)*hru_area_m2
            !!giw_sa_ha = (pi/10000.)*
     !!&  (((3.*giw_vol_m3/(pi*hru_slp(j))))**.6666)
      !!if (giw_sa_ha > hru_area_ha) giw_sa_ha = hru_area_ha
      !!giw_sa_m2(j) = giw_sa_ha * 10000.
      
      !! Mohammad: calculating surface are of GIW using this relation derived for Kenauk wetlands: V = 0.96535 *SA**(1.4575)
     ! !if (j==55) then 
         ! a = log10(giw_vol_m3/0.0075)
	   ! b = 1.67
         ! giw_sa_m2(j) = 10**(a/b)
	   ! a=0.
	  !  b=0.
       !   giw_sa_ha = giw_sa_m2(j)*0.0001
      !else         
	    a = log10(giw_vol_m3/0.0132)
	    b = 1.4575	
          giw_sa_m2(j) = 10**(a/b)
	    a=0.
	    b=0.
          giw_sa_ha = giw_sa_m2(j)*0.0001
      !end if
      
      !! end Mohammad
      
      
      
      !!giw_sa_ha = (pi/10000.)*
       !!&  (((3.*giw_vol_m3/(pi*hru_slp(j))))**.6666)
      !if (giw_sa_ha > hru_area_ha) giw_sa_ha = hru_area_ha
      !giw_sa_m2(j) = giw_sa_ha * 10000.

      !!CALCULATE OVERLAND SPILLAGE
      giw_spillage_out_mm(j) = 0.
      if (giw_vol_mm(j) > giw_volx_mm(j)) then
        giw_spillage_out_mm(j) = giw_vol_mm(j) - giw_volx_mm(j)
        giw_spillage_out_mm(j) = Max(0.,giw_spillage_out_mm(j))
        giw_vol_mm(j) = giw_volx_mm(j)
        !RAF_vol_m3(j) = 0.
        !RAF_vol_mm(j) = 0.
        !RAF_area_m2(j) = 0.
        
      elseif (giw_vol_mm(j) <= giw_volx_mm(j)) then  
      !elseif (RAF_flag(j)==1) then
          giw_spillage_out_mm(j) = 0. !! for a RAF feature, the spillage is defined differently.
      !    RAF_vol_m3(j) = giw_vol_m3
      !    RAF_vol_mm(j) = giw_vol_mm(j)
      !    RAF_area_m2(j) = giw_sa_m2(j)
      !    call RAF
          
      end if
      
      qday = qday + giw_spillage_out_mm(j)

      !!DETERMINE SAT K VALUE TO USE IN CALCULATION OF GIW SEEPAGE
      !!THE BELOW COMES FROM THE EXISTING CODE AND SPECIFIES THAT IF
      !!NO SAT K VALUE HAS BEEN SET FOR THE GIW THEN THE SAT K VALUE
      !!OF THE TOP SOIL HORIZON IN THE HRU WILL BE USED.
      !if (giw_k(j) > 0.) then
      !  k_to_use = giw_k(j)
      !else
      !  k_to_use = sol_k(1,j)
      !endif
      k_to_use = wet_k(j)
      k_to_use = k_to_use * cbedgiw

      !!CALCULATE SEEPAGE FOR THE GIW. IF THE SOIL TEMP IN THE TOP SOIL
      !!HORIZON IS LESS THAN OR EQUAL TO ZERO, SEEPAGE FROM THE GIW IS
      !!ZERO. IF THE SOIL TEMP IN THE TOP SOIL HORIZON IS GREATER THAN
      !!ZERO THEN CALCULATE ACCORDING TO A SERIES OF EQUATIONS. THE
      !!EQUATIONS BELOW COME FROM THE SWAT2009 THEORETICAL DOCUMENTATION
      !!(EQUATIONS 8:1.3.7-8:.1.3.9 ON PAGE 528. THESE EQUATIONS SPECIFY
      !!THAT SEEPAGE IS CALCULATED DIFFERENTLY ACCORDING TO THE SOIL
      !!WATER CONTENT AND ITS RELATIONSHIP TO THE FIELD CAPACITY WITHIN
      !!THE TOP SOIL HORIZON WITHIN THE HRU.

      !!CALCULATE GIW SURFACE AREA
      !if (j==55) then
      !    giw_vol_m3 = 34.85 * (giw_vol_mm(j)/1000.) 
      !else
      !    giw_vol_m3 = 192.28 * (giw_vol_mm(j)/1000.) 
      !end if
      
     !! if (j==55) then 
     !!     a = log10(giw_vol_m3/0.0075)
	    !!b = 1.67
     !!     giw_sa_m2(j) = 10**(a/b)
	    !!a=0.
	    !!b=0.
     !!     giw_sa_ha = giw_sa_m2(j)*0.0001
     !! else         
	    !!a = log10(giw_vol_m3/0.0132)
	    !!b = 1.4575	
     !!     giw_sa_m2(j) = 10**(a/b)
	    !!a=0.
	    !!b=0.
     !!     giw_sa_ha = giw_sa_m2(j)*0.0001
     !! end if
      
      
      giw_vol_m3 = 192.28 * (giw_vol_mm(j)/1000.)  
      a = log10(giw_vol_m3/0.0132)
	b = 1.4575	
      giw_sa_m2(j) = 10**(a/b)
	a=0.
	b=0.
      giw_sa_ha = giw_sa_m2(j)*0.0001
      
      !!giw_vol_m3 = (giw_vol_mm(j)/1000.)*hru_area_m2
      ! giw_sa_ha = (pi/10000.)*
      !&  (((3.*giw_vol_m3/(pi*hru_slp(j))))**.6666)
      !if (giw_sa_ha > hru_area_ha) giw_sa_ha = hru_area_ha
      !giw_sa_m2(j) = giw_sa_ha * 10000.
      
      !! Mohammad: calculating surface are of GIW using this relation derived for Kenauk wetlands: V = 0.96535 *SA**(1.4575)
	!a = log10(giw_vol_m3/0.0132)
	!b = 1.4575	
 !     giw_sa_m2(j) = 10**(a/b)
	!a=0.
	!b=0.
	!     !
 !     !giw_sa_m2(j) = giw_vol_m3/0.96535
      !giw_sa_ha = giw_sa_m2(j)*0.0001
      !! end Mohammad
      
      
      
      !! giw_vol_m3 = (giw_vol_mm(j)/1000.)*hru_area_m2
      !! giw_sa_ha = (pi/10000.)*
      !! &  (((3.*giw_vol_m3/(pi*hru_slp(j))))**.6666)
      !if (giw_sa_ha > hru_area_ha) giw_sa_ha = hru_area_ha
      !giw_sa_m2(j) = giw_sa_ha * 10000.

      giw_seepage_out_mm(j) = 0.
      if (sol_tmp(1,j) > 0.) then
        if (sol_st(1,j) < (0.5*sol_fc(1,j))) then
            giw_seepage_out_m3 = 0.
            giw_seepage_out_m3 = 240. * k_to_use * giw_sa_ha
            
     !!       if (j==55) then
     !!             giw_seepage_out_mm(j) = (giw_seepage_out_m3 *1000.)/
     !!&            34.85
     !!       else
     !!             giw_seepage_out_mm(j) = (giw_seepage_out_m3 *1000.)/
     !!&            192.28
     !!       end if
            giw_seepage_out_mm(j) = (giw_seepage_out_m3*1000.0) /
     &         192.28
            
        end if
        if (sol_st(1,j) >= (0.5*sol_fc(1,j)) .and.
     &      sol_st(1,j) < sol_fc(1,j)) then
            giw_seepage_out_m3 = 240. *
     &      (1-(sol_st(1,j)/sol_fc(1,j))) * k_to_use * giw_sa_ha
     !!       giw_seepage_out_mm(j) = (giw_seepage_out_m3/hru_area_m2) *
     !!&          1000.
            
     !!         if (j==55) then
     !!             giw_seepage_out_mm(j) = (giw_seepage_out_m3 *1000.)/
     !!&         34.85
     !!         else
     !!            giw_seepage_out_mm(j) = (giw_seepage_out_m3 *1000.)/
     !!&         192.28
     !!         end if
            
         giw_seepage_out_mm(j) = (giw_seepage_out_m3*1000.0) /
     &          192.28
        end if
        if (sol_st(1,j) >= sol_fc(1,j)) then
            giw_seepage_out_m3 = 0.
            giw_seepage_out_mm(j) = 0.
        end if
        if (sol_tmp(1,j) <= 0.) then
            giw_seepage_out_m3 = 0.
            giw_seepage_out_mm(j) = 0.
        end if
      end if
      if(giw_seepage_out_mm(j) > giw_vol_mm(j)) then
        giw_seepage_out_mm(j) = giw_vol_mm(j)
      end if
      giw_vol_mm(j) = giw_vol_mm(j) - giw_seepage_out_mm(j)

      !!NOTE: DO *NOT* ADD THE giw_seepage_out_m3 PARAMETER TO ANY OF
      !!THE HRU FLOW PARAMETERS HERE. THE giw_seepage_out_m3 PARAMETER
      !!WILL BE ADDED TO inflpcp PARAMETER IN THE subbasin.f SUBROUTINE
      !!ALMOST IMMEDIATELY AFTER THIS GIW CALL (CALLED FOLLOWING THE
      !!surface.f CALL).

      !!NOTE: CALCULATION OF ET FROM THE GIW HAS BEEN MOVED TO THE
      !!ETACT.F SUBROUTINE.

      !!RECALCULATE GIW SURFACE AREA
     !! if (j==55) then
     !!     giw_vol_m3 = 34.85 * (giw_vol_mm(j)/1000.) 
     !! else
     !!     giw_vol_m3 = 192.28 * (giw_vol_mm(j)/1000.) 
     !! end if
     !! 
     !! if (j==55) then 
     !!     a = log10(giw_vol_m3/0.0075)
	    !!b = 1.67
     !!     giw_sa_m2(j) = 10**(a/b)
	    !!a=0.
	    !!b=0.
     !!     giw_sa_ha = giw_sa_m2(j)*0.0001
     !! else         
	    !!a = log10(giw_vol_m3/0.0132)
	    !!b = 1.4575	
     !!     giw_sa_m2(j) = 10**(a/b)
	    !!a=0.
	    !!b=0.
     !!     giw_sa_ha = giw_sa_m2(j)*0.0001
     !! end if
      
      
      
      
      
      
      giw_vol_m3 = 192.28 * (giw_vol_mm(j)/1000.)  
      
      a = log10(giw_vol_m3/0.0132)
	b = 1.4575	
      giw_sa_m2(j) = 10**(a/b)
	a=0.
	b=0.
	!     !
 !     !giw_sa_m2(j) = giw_vol_m3/0.96535
      giw_sa_ha = giw_sa_m2(j)*0.0001
      
      !!giw_vol_m3 = (giw_vol_mm(j)/1000.)*hru_area_m2
      !!giw_sa_ha = (pi/10000.)*
      !!&  (((3.*giw_vol_m3/(pi*hru_slp(j))))**.6666)
      !if (giw_sa_ha > hru_area_ha) giw_sa_ha = hru_area_ha
      !giw_sa_m2(j) = giw_sa_ha * 10000.
      
      !! Mohammad: calculating surface are of GIW using this relation derived for Kenauk wetlands: V = 0.96535 *SA**(1.4575)
	!a = log10(giw_vol_m3/0.0132)
	!b = 1.4575	
 !     giw_sa_m2(j) = 10**(a/b)
	!a=0.
	!b=0.	
	!     !
 !     !giw_sa_m2(j) = giw_vol_m3/0.96535
 !     giw_sa_ha = giw_sa_m2(j)*0.0001
      !! end Mohammad

      !!Record volume at end of time step for subsequent time step records
      giw_vol_prev_mm(j) = giw_vol_mm(j)
      

      return
      end