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

      if (j==55) then
          giw_vol_m3 = 0.0000003 * (giw_vol_mm(j) ** 2.9583)
      elseif (j==110) then
          !giw_vol_m3 = 0.00004 * (giw_vol_mm(j) ** 2.1055)
          giw_vol_m3 =  (giw_vol_mm(j) * 0.0272)
      elseif (j==664) then
          giw_vol_m3 = (giw_vol_mm(j) / 8.2652)
      elseif (j==607) then
          giw_vol_m3 = (giw_vol_mm(j) / 2.3718)
      elseif (j==816) then
          giw_vol_m3 = (giw_vol_mm(j) / 5.518)
      elseif (j==1076) then
          giw_vol_m3 = 0.324 * (giw_vol_mm(j) ** 1.2413)
      elseif (j==1799) then
          giw_vol_m3 = 0.1412 * (giw_vol_mm(j) ** 1.2452)
      elseif (j==1676) then
          giw_vol_m3 = 0.0031 * (giw_vol_mm(j) ** 1.7185)
      elseif (j==1210) then
          giw_vol_m3 = (9/1000000.0) * (giw_vol_mm(j) ** 2.196)
      elseif (j==426) then
          giw_vol_m3 = (3/1000000.0) *(giw_vol_mm(j) ** 2.537)
      else
            giw_vol_m3 = 0.0005 * (giw_vol_mm(j) ** 1.8291) 
                     
      end if
            
      !! Mohammad: calculating surface are of GIW using this relation derived for Kenauk wetlands: V = 0.96535 *SA**(1.4575)

        if (j==55) then 
            giw_sa_m2(j)  = 18.72 * (giw_vol_m3 ** 0.6)
            giw_sa_ha = giw_sa_m2(j)*0.0001
        elseif (j==110) then
            !giw_sa_m2(j)  = 16.32 * (giw_vol_m3 ** 0.5861)
            !giw_sa_ha = giw_sa_m2(j)*0.0001
            giw_sa_m2(j)  =  (giw_vol_m3 / 0.1572)
            giw_sa_ha = giw_sa_m2(j)*0.0001
        elseif (j==664) then
            giw_sa_m2(j)  = (giw_vol_m3 / 0.0904)
            giw_sa_ha = giw_sa_m2(j)*0.0001
        elseif (j==607) then
            giw_sa_m2(j)  = (giw_vol_m3 / 0.0683)
            giw_sa_ha = giw_sa_m2(j)*0.0001
        elseif (j==816) then
            giw_sa_m2(j)  = 44.86 * (giw_vol_m3 ** 0.5227)
            giw_sa_ha = giw_sa_m2(j)*0.0001
        elseif (j==1076) then
            giw_sa_m2(j)  = 86.058 * (giw_vol_m3 ** 0.5242)
            giw_sa_ha = giw_sa_m2(j)*0.0001
        elseif (j==1799) then
            giw_sa_m2(j)  = 58.911 * (giw_vol_m3 ** 0.5973)
            giw_sa_ha = giw_sa_m2(j)*0.0001
        elseif (j==1676) then
            giw_sa_m2(j)  = 47.155 * (giw_vol_m3 ** 0.4877)
            giw_sa_ha = giw_sa_m2(j)*0.0001
        elseif (j==1210) then
            giw_sa_m2(j)  = 11.43 * (giw_vol_m3 ** 0.532)
            giw_sa_ha = giw_sa_m2(j)*0.0001
        elseif (j==426) then 
            giw_sa_m2(j)  = 19.75 * (giw_vol_m3 ** 0.552)
            giw_sa_ha = giw_sa_m2(j)*0.0001
        else         
            giw_sa_m2(j)  = 3.4426 * (giw_vol_m3 ** 0.6861)
            giw_sa_ha = giw_sa_m2(j)*0.0001
        end if  
          
      
      !! end Mohammad
      

      !!CALCULATE OVERLAND SPILLAGE
      giw_spillage_out_mm(j) = 0.
      if (giw_vol_mm(j) > giw_volx_mm(j)) then
        giw_spillage_out_mm(j) = giw_vol_mm(j) - giw_volx_mm(j)
        giw_spillage_out_mm(j) = Max(0.,giw_spillage_out_mm(j))
        giw_vol_mm(j) = giw_volx_mm(j)
        
      elseif (giw_vol_mm(j) <= giw_volx_mm(j)) then  
          giw_spillage_out_mm(j) = 0. !! for a RAF feature, the spillage is defined differently.
          
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
      if (j==55) then
          giw_vol_m3 = 0.0000003 * (giw_vol_mm(j) ** 2.9583)
      elseif (j==110) then
          !giw_vol_m3 = 0.00004 * (giw_vol_mm(j) ** 2.1055)
          giw_vol_m3 =  (giw_vol_mm(j) * 0.0272)
      elseif (j==664) then
          giw_vol_m3 = (giw_vol_mm(j) / 8.2652)
      elseif (j==607) then
          giw_vol_m3 = (giw_vol_mm(j) / 2.3718)
      elseif (j==816) then
          giw_vol_m3 = (giw_vol_mm(j) / 5.518)
      elseif (j==1076) then
          giw_vol_m3 = 0.324 * (giw_vol_mm(j) ** 1.2413)
      elseif (j==1799) then
          giw_vol_m3 = 0.1412 * (giw_vol_mm(j) ** 1.2452)
      elseif (j==1676) then
          giw_vol_m3 = 0.0031 * (giw_vol_mm(j) ** 1.7185)
      elseif (j==1210) then
          giw_vol_m3 = (9/1000000.0) * (giw_vol_mm(j) ** 2.196)
      elseif (j==426) then
          giw_vol_m3 = (3/1000000.0) *(giw_vol_mm(j) ** 2.537)
      else
          giw_vol_m3 = 0.0005 * (giw_vol_mm(j) ** 1.8291)             
      end if
      
        if (j==55) then 
            giw_sa_m2(j)  = 18.72 * (giw_vol_m3 ** 0.6)
            giw_sa_ha = giw_sa_m2(j)*0.0001
        elseif (j==110) then
            !giw_sa_m2(j)  = 16.32 * (giw_vol_m3 ** 0.5861)
            !giw_sa_ha = giw_sa_m2(j)*0.0001
            giw_sa_m2(j)  =  (giw_vol_m3 / 0.1572)
            giw_sa_ha = giw_sa_m2(j)*0.0001
        elseif (j==664) then
            giw_sa_m2(j)  = (giw_vol_m3 / 0.0904)
            giw_sa_ha = giw_sa_m2(j)*0.0001
        elseif (j==607) then
            giw_sa_m2(j)  = (giw_vol_m3 / 0.0683)
            giw_sa_ha = giw_sa_m2(j)*0.0001
        elseif (j==816) then
            giw_sa_m2(j)  = 44.86 * (giw_vol_m3 ** 0.5227)
            giw_sa_ha = giw_sa_m2(j)*0.0001
        elseif (j==1076) then
            giw_sa_m2(j)  = 86.058 * (giw_vol_m3 ** 0.5242)
            giw_sa_ha = giw_sa_m2(j)*0.0001
        elseif (j==1799) then
            giw_sa_m2(j)  = 58.911 * (giw_vol_m3 ** 0.5973)
            giw_sa_ha = giw_sa_m2(j)*0.0001
        elseif (j==1676) then
            giw_sa_m2(j)  = 47.155 * (giw_vol_m3 ** 0.4877)
            giw_sa_ha = giw_sa_m2(j)*0.0001
        elseif (j==1210) then
            giw_sa_m2(j)  = 11.43 * (giw_vol_m3 ** 0.532)
            giw_sa_ha = giw_sa_m2(j)*0.0001
        elseif (j==426) then
            giw_sa_m2(j)  = 19.75 * (giw_vol_m3 ** 0.552)
            giw_sa_ha = giw_sa_m2(j)*0.0001
        else         
            giw_sa_m2(j)  = 3.4426 * (giw_vol_m3 ** 0.6861)
            giw_sa_ha = giw_sa_m2(j)*0.0001
        end if  
      


      giw_seepage_out_mm(j) = 0.
      if (sol_tmp(1,j) > 0.) then
        if (sol_st(1,j) < (0.5*sol_fc(1,j))) then
            giw_seepage_out_m3 = 0.
            giw_seepage_out_m3 = 240. * k_to_use * giw_sa_ha
            
        if (j==55) then  ! for HP1
          giw_seepage_out_mm(j) = 160.3 *(giw_seepage_out_m3 ** 0.3380)
        elseif (j == 110) then ! for S1
          !giw_seepage_out_mm(j) = 118.15 *(giw_seepage_out_m3 ** 0.4745)
          giw_seepage_out_mm(j) = (giw_seepage_out_m3 / 0.0272)
        elseif (j == 664) then ! for HP16
          giw_seepage_out_mm(j) = (giw_seepage_out_m3 * 8.2652)
        elseif (j == 607) then ! for HP17
          giw_seepage_out_mm(j) = (giw_seepage_out_m3 * 2.3718)
        elseif (j == 816) then ! for HP20
          giw_seepage_out_mm(j) = (giw_seepage_out_m3 * 5.518)
        elseif (j == 1076) then ! for HP26
          giw_seepage_out_mm(j) = 2.52 * (giw_seepage_out_m3 ** 0.802)
        elseif (j == 1799) then ! for HP27
          giw_seepage_out_mm(j) = 4.884* (giw_seepage_out_m3 ** 0.8)
        elseif (j == 1676) then ! for HP28
          giw_seepage_out_mm(j) = 28.77* (giw_seepage_out_m3 ** 0.5812)
        elseif (j == 1210) then ! for S6
          giw_seepage_out_mm(j) = 195.0* (giw_seepage_out_m3 ** 0.4553)
        elseif (j == 426) then ! for HP35
          giw_seepage_out_mm(j) = 141.72* (giw_seepage_out_m3 ** 0.3934)
        else
          giw_seepage_out_mm(j) = 63.79 * (giw_seepage_out_m3 ** 0.5467)  ! other wetlands
        end if

        end if
        if (sol_st(1,j) >= (0.5*sol_fc(1,j)) .and.
     &      sol_st(1,j) < sol_fc(1,j)) then
            giw_seepage_out_m3 = 240. *
     &      (1-(sol_st(1,j)/sol_fc(1,j))) * k_to_use * giw_sa_ha
            
        if (j==55) then  ! for HP1
          giw_seepage_out_mm(j) = 160.3 *(giw_seepage_out_m3 ** 0.3380)
        elseif (j == 110) then ! for S1
          !giw_seepage_out_mm(j) = 118.15 *(giw_seepage_out_m3 ** 0.4745)
          giw_seepage_out_mm(j) = (giw_seepage_out_m3 / 0.0272)
        elseif (j == 664) then ! for HP16
          giw_seepage_out_mm(j) = (giw_seepage_out_m3 * 8.2652)
        elseif (j == 607) then ! for HP17
          giw_seepage_out_mm(j) = (giw_seepage_out_m3 * 2.3718)
        elseif (j == 816) then ! for HP20
          giw_seepage_out_mm(j) = (giw_seepage_out_m3 * 5.518)
        elseif (j == 1076) then ! for HP26
          giw_seepage_out_mm(j) = 2.52 * (giw_seepage_out_m3 ** 0.5242)
        elseif (j == 1799) then ! for HP27
          giw_seepage_out_mm(j) = 4.884* (giw_seepage_out_m3 ** 0.8)
        elseif (j == 1676) then ! for HP28
          giw_seepage_out_mm(j) = 28.77* (giw_seepage_out_m3 ** 0.802)
        elseif (j == 1210) then ! for S6
          giw_seepage_out_mm(j) = 195.0* (giw_seepage_out_m3 ** 0.4553) 
        elseif (j == 426) then ! for HP35
          giw_seepage_out_mm(j) = 141.72* (giw_seepage_out_m3 ** 0.3934)
        else
          giw_seepage_out_mm(j) = 63.79 * (giw_seepage_out_m3 ** 0.5467)  ! other wetlands
        end if

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
      
      if (j==55) then
          giw_vol_m3 = 0.0000003 * (giw_vol_mm(j) ** 2.9583)
      elseif (j==110) then
          !giw_vol_m3 = 0.00004 * (giw_vol_mm(j) ** 2.1055)
          giw_vol_m3 =  (giw_vol_mm(j) * 0.0272)
      elseif (j==664) then
          giw_vol_m3 = (giw_vol_mm(j) / 8.2652)
      elseif (j==607) then
          giw_vol_m3 = (giw_vol_mm(j) / 2.3718)
      elseif (j==816) then
          giw_vol_m3 = (giw_vol_mm(j) / 5.518)
      elseif (j==1076) then
          giw_vol_m3 = 0.324 * (giw_vol_mm(j) ** 1.2413)
      elseif (j==1799) then
          giw_vol_m3 = 0.1412 * (giw_vol_mm(j) ** 1.2452)
      elseif (j==1676) then
          giw_vol_m3 = 0.0031 * (giw_vol_mm(j) ** 1.7185)
      elseif (j==1210) then
          giw_vol_m3 = (9/1000000.0) * (giw_vol_mm(j) ** 2.196)
      elseif (j==426) then
          giw_vol_m3 = (3/1000000.0) *(giw_vol_mm(j) ** 2.537)
      else
            giw_vol_m3 = 0.0005 * (giw_vol_mm(j) ** 1.8291) 
                     
      end if
      
        if (j==55) then 
            giw_sa_m2(j)  = 18.72 * (giw_vol_m3 ** 0.6)
            giw_sa_ha = giw_sa_m2(j)*0.0001
        elseif (j==110) then
            !giw_sa_m2(j)  = 16.32 * (giw_vol_m3 ** 0.5861)
            !giw_sa_ha = giw_sa_m2(j)*0.0001
            giw_sa_m2(j)  =  (giw_vol_m3 / 0.1572)
            giw_sa_ha = giw_sa_m2(j)*0.0001
        elseif (j==664) then
            giw_sa_m2(j)  = (giw_vol_m3 / 0.0904)
            giw_sa_ha = giw_sa_m2(j)*0.0001
        elseif (j==607) then
            giw_sa_m2(j)  = (giw_vol_m3 / 0.0683)
            giw_sa_ha = giw_sa_m2(j)*0.0001
        elseif (j==816) then
            giw_sa_m2(j)  = 44.86 * (giw_vol_m3 ** 0.5227)
            giw_sa_ha = giw_sa_m2(j)*0.0001
        elseif (j==1076) then
            giw_sa_m2(j)  = 86.058 * (giw_vol_m3 ** 0.5242)
            giw_sa_ha = giw_sa_m2(j)*0.0001
        elseif (j==1799) then
            giw_sa_m2(j)  = 58.911 * (giw_vol_m3 ** 0.5973)
            giw_sa_ha = giw_sa_m2(j)*0.0001
        elseif (j==1676) then
            giw_sa_m2(j)  = 47.155 * (giw_vol_m3 ** 0.4877)
            giw_sa_ha = giw_sa_m2(j)*0.0001
        elseif (j==1210) then
            giw_sa_m2(j)  = 11.43 * (giw_vol_m3 ** 0.532)
            giw_sa_ha = giw_sa_m2(j)*0.0001
        elseif (j==426) then
            giw_sa_m2(j)  = 19.75 * (giw_vol_m3 ** 0.552)
            giw_sa_ha = giw_sa_m2(j)*0.0001
        else         
            giw_sa_m2(j)  = 3.4426 * (giw_vol_m3 ** 0.6861)
            giw_sa_ha = giw_sa_m2(j)*0.0001
        end if 

      !!Record volume at end of time step for subsequent time step records
      giw_vol_prev_mm(j) = giw_vol_mm(j)
      

      return
      end