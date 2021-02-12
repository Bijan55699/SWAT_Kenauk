      subroutine etact

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates potential plant transpiration for Priestley-
!!    Taylor and Hargreaves ET methods, and potential and actual soil
!!    evaporation. NO3 movement into surface soil layer due to evaporation
!!    is also calculated.


!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    canstor(:)   |mm H2O        |amount of water held in canopy storage
!!    elevb(:,:)   |m             |elevation at center of band in subbasin
!!    elevb_fr(:,:)|none          |fraction of subbasin area within elevation
!!                                |band
!!    ep_max       |mm H2O        |maximum amount of transpiration (plant et)
!!                                |that can occur on current day in HRU
!!    esco(:)      |none          |soil evaporation compensation factor
!!    ihru         |none          |HRU number
!!    ipet         |none          |code for potential ET method
!!                                |0 Priestley-Taylor method
!!                                |1 Penman/Monteith method
!!                                |2 Hargreaves method
!!    laiday(:)    |m**2/m**2     |leaf area index
!!    pet_day      |mm H2O        |potential evapotranspiration on current day
!!                                |in HRU
!!    pot_vol(:)   |m**3 H2O      |current volume of water stored in the
!!                                |depression/impounded area
!!    sno_hru(:)   |mm H2O        |amount of water in snow in HRU on current day
!!    snoeb(:,:)   |mm H2O        |snow water content in elevation band on
!!                                |current day
!!    sol_cov(:)   |kg/ha         |amount of residue on soil surface
!!    sol_fc(:,:)  |mm H2O        |amount of water available to plants in soil
!!                                |layer at field capacity (fc - wp water)
!!    sol_nly(:)   |none          |number of soil layers in profile
!!    sol_no3(:,:) |kg N/ha       |amount of nitrogen stored in the nitrate
!!                                |pool
!!    sol_st(:,:)  |mm H2O        |amount of water stored in the soil layer on
!!                                |current day
!!    sol_z(:,:)   |mm            |depth to bottom of soil layer
!!    tavband(:,:) |deg C         |average temperature for the day in band in HRU
!!    tmpav(:)     |deg C         |average air temperature on current day for
!!                                |HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    canev        |mm H2O        |amount of water evaporated from canopy
!!                                |storage
!!    ep_max       |mm H2O        |maximum amount of transpiration (plant et)
!!                                |that can occur on current day in HRU
!!    es_day       |mm H2O        |actual amount of evaporation (soil et) that
!!                                |occurs on day in HRU
!!    sno_hru(:)   |mm H2O        |amount of water in snow in HRU on current day
!!    sno3up       |kg N/ha       |amount of nitrate moving upward in the soil
!!                                |profile in watershed
!!    snoeb(:,:)   |mm H2O        |snow water content in elevation band on
!!                                |current day
!!    snoev        |mm H2O        |amount of water in snow lost through
!!                                |sublimation on current day
!!    sol_st(:,:)  |mm H2O        |amount of water stored in the soil layer on
!!                                |current day
!!    sol_sw(:)    |mm H2O        |amount of water stored in the soil profile
!!                                |on current day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cej          |
!!    dep          |mm            |soil depth from which evaporation will occur
!!                                |in current soil layer
!!    eaj          |none          |weighting factor to adjust PET for impact of
!!                                |plant cover
!!    effnup       |
!!    eos1         |none          |variable to hold intermediate calculation
!!                                |result
!!    eosl         |mm H2O        |maximum amount of evaporation that can occur
!!                                |from soil profile
!!    es_max       |mm H2O        |maximum amount of evaporation (soil et)
!!                                |that can occur on current day in HRU
!!    esd          |mm            |maximum soil depth from which evaporation
!!                                |is allowed to occur
!!    esleft       |mm H2O        |potenial soil evap that is still available
!!    etco         |
!!    evz          |
!!    evzp         |
!!    ib           |none          |counter
!!    j            |none          |HRU number
!!    ly           |none          |counter
!!    no3up        |kg N/ha       |amount of nitrate moving upward in profile
!!    pet          |mm H2O        |amount of PET remaining after water stored
!!                                |in canopy is evaporated
!!    sev          |mm H2O        |amount of evaporation from soil layer
!!    sumsnoeb     |mm H2O        |amount of snow in elevation bands whose air
!!                                |temperature is greater than 0 degrees C
!!    xx           |none          |variable to hold intermediate calculation
!!                                |result
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Min, Max
!!    SWAT: Expo

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j, ib, ly
!!    real, parameter :: esd = 500., etco = 0.80, effnup = 0.1
      real :: esd, etco, effnup
      real :: no3up, es_max, eos1, xx, cej, eaj, pet, esleft
      real :: es_max_prev,a,b,sam2
      real :: sumsnoeb, evzp, eosl, dep, evz, sev, pet_m3, hru_area_ha
      real :: hru_area_m2, giw_sa_ha, giw_evap_out_m3, giw_vol_m3
      real, parameter :: pi = 3.1416

      j = 0
      j = ihru
      esd = 500.
      etco = 0.80
      effnup = 0.1
      hru_area_ha = 0.
      !Mohammad
      if(giw_flag(j) == 1) then
          hru_area_ha = SAHAGIW(j)
          Pet_GIW(j) = pet_day
          hru_ha(j) = SAHAGIW(j)
      else
          hru_area_ha = hru_ha(j)
      end if
      
      hru_area_m2 = 0.
      hru_area_m2 = hru_area_ha * 10000.
      giw_sa_ha = 0.
      pet = 0.
      pet = pet_day
      
      pet_m3 = 0.
      es_max_prev = 0.
      giw_evap_out_mm = 0.
      giw_evap_out_m3 = 0.
      es_max = 0.
      giw_vol_m3 = 0.
      a=0.
      b=0.
      sam2=0.

      !! evaporate canopy storage first
      !! canopy storage is calculated by the model only if the Green & Ampt
      !! method is used to calculate surface runoff. The curve number methods
      !! take canopy effects into account in the equations. For either of the
      !! CN methods, canstor will always equal zero.
      pet = pet - canstor(j)
      if (pet < 0.) then
        canstor(j) = -pet
        canev = pet_day
        pet = 0.
        ep_max = 0.
        es_max = 0.
      else
        canev = canstor(j)
        canstor(j) = 0.
      endif

      if (pet > 1.0e-6) then
        !
        !! compute potential plant evap for methods other that Penman-Monteith
        !if (ipet /= 1) then
        !    if (laiday(j) <= 3.0) then
        !        ep_max = laiday(j) * pet / 3.
        !    else
        !        ep_max = pet
        !    end if
        !    if (ep_max < 1.0e-6) ep_max = 0.
        !end if

        !
        !!GEVENSON - CALCULATE E FROM WETLAND
        !!MOST OF THESE EQUATIONS COME FROM THE SWAT2009 THEORETICAL DOCUMENTATION (EQUATIONS 8:1.3.5-8:1.3.6 ON PAGE 528)
        if (giw_flag(j) == 1) then

            if (tmpav(j) > 0.) then
                
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
                    giw_vol_m3 = (9/1000000.0) *(giw_vol_mm(j) ** 2.196)
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
            else         
                giw_sa_m2(j)  = 3.4426 * (giw_vol_m3 ** 0.6861)
                giw_sa_ha = giw_sa_m2(j)*0.0001
            end if  
        
                
           if (laiday(j) < evlai) then
            giw_evap_out_m3 = 5.0 * (1. - laiday(j) / evlai) * pet * 
     &       giw_sa_ha
            giw_evap_out_m3 = Min(giw_evap_out_m3, giw_vol_m3)
            giw_vol_m3 = giw_vol_m3 - giw_evap_out_m3
           else
               giw_evap_out_m3=0.
          endif

              if (j==55) then
               giw_evap_out_mm = 160.3 * (giw_evap_out_m3 ** 0.3380)
              elseif (j==110) then
               !giw_evap_out_mm = 118.15 * (giw_evap_out_m3 ** 0.4745)
               giw_evap_out_mm =  (giw_evap_out_m3 / 0.0272) 
              elseif (j==664) then
               giw_evap_out_mm = (giw_evap_out_m3 * 8.2652)
              elseif (j==607) then
               giw_evap_out_mm = (giw_evap_out_m3 * 2.3718)
              elseif (j==816) then
               giw_evap_out_mm = (giw_evap_out_m3 * 5.518)
              elseif (j==1076) then
               giw_evap_out_mm = 2.52 * (giw_evap_out_m3 ** 0.5242)
              elseif (j==1799) then
               giw_evap_out_mm = 4.884* (giw_evap_out_m3 ** 0.8)
              elseif (j==1676) then
               giw_evap_out_mm = 28.77* (giw_evap_out_m3 ** 0.802)
              elseif (j==1210) then
               giw_evap_out_mm = 195.0* (giw_evap_out_m3 ** 0.4553)
              else
              giw_evap_out_mm = 63.79 * (giw_evap_out_m3 ** 0.5467) 
              end if
                
               
                if (giw_evap_out_mm > pet) giw_evap_out_mm = pet
                if(giw_evap_out_mm >= giw_vol_mm(j)) then
                    giw_evap_out_mm = giw_vol_mm(j)
                end if
                giw_vol_mm(j) = giw_vol_mm(j) - giw_evap_out_mm
                !
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
        else         
            giw_sa_m2(j)  = 3.4426 * (giw_vol_m3 ** 0.6861)
            giw_sa_ha = giw_sa_m2(j)*0.0001
        end if  
        
		      pet = pet - giw_evap_out_mm
		      !pet = pet * (1.-(giw_sa_ha/hru_ha(j)))
            else
                giw_evap_out_m3 = 0.
                giw_evap_out_mm = 0.
            end if
            
        end if
        if (giw_flag(j) == 1) then           
            if (sol_tmp(1,j) > 0.) then
              if (laiday(j) <= 3.0) then
                ep_max = laiday(j) * pet / 3.
              else
                ep_max = pet
              end if
            else
                ep_max = 0.
            end if
        end if
                    
        if (ep_max < 1.0e-6) ep_max = 0.
        !
        !! compute potential soil evaporation
        cej = -5.e-5
        eaj = 0.
        es_max = 0.
        eos1 = 0.
        if (sno_hru(j) >= 0.5) then
          eaj = 0.5
        else
          eaj = Exp(cej * (sol_cov(j)+ 0.1))
        end if
        es_max = pet * eaj
        eos1 = pet / (es_max + ep_max + 1.e-10)
        eos1 = es_max * eos1
        es_max = Min(es_max, eos1)
        es_max = Max(es_max, 0.)
!        if (pot_vol(j) > 1.e-4) es_max = 0.

        !! make sure maximum plant and soil ET doesn't exceed potential ET
        !!if (pet_day < es_max + ep_max) then
          !!es_max = pet_day - ep_max
          if (pet < es_max + ep_max) then
            es_max = pet * es_max / (es_max + ep_max)
            ep_max = pet * ep_max / (es_max + ep_max)
          end if
          if (pet < es_max + ep_max) then
            es_max = pet - ep_max - 1.0e-6
          end if
        !!end if

        !! initialize soil evaporation variables
        esleft = 0.
        esleft = es_max

        !! compute sublimation
        if (elevb_fr(1,hru_sub(j)) <= 0.) then
          !! compute sublimation without elevation bands
          if (tmpav(j) > 0.) then
            if (sno_hru(j) >= esleft) then
              !! take all soil evap from snow cover
              sno_hru(j) = sno_hru(j) - esleft
              snoev = snoev + esleft
              esleft = 0.
            else
              !! take all soil evap from snow cover then start taking from soil
              esleft = esleft - sno_hru(j)
              snoev = snoev + sno_hru(j)
              sno_hru(j) = 0.
            endif
          endif
        else
          !! elevation bands
          sumsnoeb = 0.
          !! calculate air temp in elevation bands and sum snow
          !! for elevation bands with temp > 0 deg C
          do ib = 1, 10
            if (elevb_fr(ib,hru_sub(j)) <= 0.) exit
            if (tavband(ib,j) > 0.) then 
              sumsnoeb = sumsnoeb +                                     
     &                             snoeb(ib,j) * elevb_fr(ib,hru_sub(j))
            end if
          end do
          
          !! compute sublimation from elevation bands
          if (sumsnoeb >= esleft .and. sumsnoeb > 0.01) then
            do ib = 1, 10
              if (elevb_fr(ib,hru_sub(j)) <= 0.) exit
              if (tavband(ib,j) > 0.) then
                snoev = snoev + snoeb(ib,j) * (esleft / sumsnoeb) *     
     &                                           elevb_fr(ib,hru_sub(j))
                snoeb(ib,j) = snoeb(ib,j) - snoeb(ib,j) * (esleft /     
     &                                                         sumsnoeb)
              end if
            end do
          else
            do ib = 1, 10
              if (elevb_fr(ib,hru_sub(j)) <= 0.) exit
              if (tavband(ib,j) > 0.) then
                snoev = snoev + snoeb(ib,j) * elevb_fr(ib,hru_sub(j))
                snoeb(ib,j) = 0.
              end if
            end do
          end if
          esleft = esleft - snoev
          sno_hru(j) = sno_hru(j) - snoev
        endif

!! take soil evap from each soil layer
        evzp = 0.
        eosl = 0.
        eosl = esleft
        do ly = 1, sol_nly(j)

        !! depth exceeds max depth for soil evap (esd)
        dep = 0.
        if (ly == 1) then
          dep = sol_z(1,j)
        else
          dep = sol_z(ly-1,j)
        endif
        
        if (dep < esd) then
          !! calculate evaporation from soil layer
          evz = 0.
          sev = 0.
          xx = 0.
          evz = eosl * sol_z(ly,j) / (sol_z(ly,j) + Exp(2.374 -         
     &       .00713 * sol_z(ly,j)))
          sev = evz - evzp * esco(j)
          evzp = evz
          if (sol_st(ly,j) < sol_fc(ly,j)) then
            xx =  2.5 * (sol_st(ly,j) - sol_fc(ly,j)) / sol_fc(ly,j)
            sev = sev * Expo(xx)
          end if
          sev = Min(sev, sol_st(ly,j) * etco)

          if (sev < 0.) sev = 0.
          if (sev > esleft) sev = esleft

          !! adjust soil storage, potential evap
          if (sol_st(ly,j) > sev) then
            esleft = esleft - sev
            sol_st(ly,j) = Max(1.e-6, sol_st(ly,j) - sev)
          else
            esleft = esleft - sol_st(ly,j)
            sol_st(ly,j) = 0.
          endif
        endif

        !! compute no3 flux from layer 2 to 1 by soil evaporation
        if (ly == 2) then
          no3up = 0.
          no3up = effnup * sev * sol_no3(2,j) / (sol_st(2,j) + 1.e-6)
          no3up = Min(no3up, sol_no3(2,j))
          sno3up = sno3up + no3up * hru_dafr(j)
          sol_no3(2,j) = sol_no3(2,j) - no3up
          sol_no3(1,j) = sol_no3(1,j) + no3up
        endif

      end do

      !! update total soil water content
      sol_sw(j) = 0.
      do ly = 1, sol_nly(j)
        sol_sw(j) = sol_sw(j) + sol_st(ly,j)
      end do

      !! calculate actual amount of evaporation from soil
      es_day = es_max - esleft
      if (es_day < 0.) es_day = 0.

      end if

      return
      end
