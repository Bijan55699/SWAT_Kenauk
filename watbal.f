      subroutine watbal

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine computes the daily water balance for each HRU
!!    changes in storage should equal water losses from the system
!!    write statements can be uncommented for model debugging

!!    this subroutine will give errors for HRUs receiving irrigation water
!!    from reaches or reservoirs

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    aird(:)     |mm H2O        |amount of water applied to HRU on current
!!                               |day
!!    bsprev      |mm H2O        |surface runoff lagged from prior day
!!    bss(1,:)    |mm H2O        |amount of lateral flow lagged
!!    bssprev     |mm H2O        |lateral flow lagged from prior day of
!!                               |simulation
!!    curyr       |none          |current year of simulation
!!    deepst(:)   |mm H2O        |depth of water in deep aquifer
!!    deepstp     |mm H2O        |depth of water in deep aquifer in HRU
!!    ep_day      |mm H2O        |actual amount of transpiration that occurs on
!!                               |day in HRU
!!    es_day      |mm H2O        |actual amount of evaporation (from soil) that
!!                               |occurs on day in HRU
!!    etday       |mm H2O        |actual amount of evapotranspiration that
!!                               |occurs on day in HRU
!!    gw_q(:)     |mm H2O        |groundwater contribution to streamflow from
!!                               |HRU on current day
!!    gwseep      |mm H2O        |amount of water recharging deep aquifer on
!!                               |current day
!!    iida        |julian date   |current day of simulation
!!    inflpcp     |mm H2O        |amount of precipitation that infiltrates into
!!                               |soil (enters soil)
!!    ihru        |none          |HRU number
!!    latq(:)     |mm H2O        |amount of water in lateral flow in HRU for the
!!                               |day
!!    precipday   |mm H2O        |precipitation for the day in HRU
!!    qday        |mm H2O        |surface runoff loading to main channel for
!!                               |day in HRU (includes effects of transmission
!!                               |losses)
!!    qtile       |mm H2O        |drainage tile flow for day in HRU
!!    rchrg(:)    |mm H2O        |amount of water recharging both aquifers on
!!                               |current day in HRU
!!    revapday    |mm H2O        |amount of water moving from the shallow
!!                               |aquifer into the soil profile or being taken
!!                               |up by plant roots in the shallow aquifer
!!    sepbtm(:)   |mm H2O        |seepage leaving the bottom of the soil profile
!!                               |on day in HRU
!!    shallst(:)  |mm H2O        |depth of water in shallow aquifer
!!    shallstp    |mm H2O        |depth of water in shallow aquifer in HRU on
!!                               |previous day
!!    sno_hru(:)  |mm H2O        |amount of water stored as snow
!!    snoev       |mm H2O        |amount of water in snow lost through
!!                               |sublimation on current day in HRU
!!    snofall     |mm H2O        |amount of precipitation falling as freezing
!!                               |rain/snow on day in HRU
!!    snomlt      |mm H2O        |amount of water in snow melt for the day in
!!                               |HRU
!!    snoprev     |mm H2O        |amount of water stored as snow on previous day
!!    sol_sw(:)   |mm H2O        |amount of water stored in soil profile on any
!!                               |given day
!!    subp(:)     |mm H2O        |precipitation for the day in HRU
!!    surf_bs(1,:)|mm H2O        |amount of surface runoff lagged over one
!!                               |day
!!    swprev      |mm H2O        |amount of water stored in soil profile in the
!!                               |HRU on the previous day
!!    tloss       |mm H2O        |amount of water removed from surface runoff
!!    twlpnd      |mm H2O        |water lost through seepage from ponds on day
!!                               |in HRU
!!    twlwet      |mm H2O        |water lost through seepage from wetlands on
!!                               |day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    dstor       |mm H2O        |change in storage (snow, soil, shallow
!!                               |and deep aquifers)
!!    h2oloss     |mm H2O        |net movement of water out of system
!!    j           |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Abs

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j, sub_number
      real :: dstor, h2oloss, dif, giw_q_in_mm

      j = 0
      j = ihru
      sub_number = 0
      sub_number = inum1
      hru_area_ha = hru_ha(j)
      hru_area_m2 = hru_area_ha * 10000.


      dstor = 0.
      h2oloss = 0.
      giw_q_in_mm = 0.

    !! NOTE giw_vol_mm_begin is *not* volume on previous time step but instead
    !! the volume after upgradient HRUs have added inflow

      if (ievent<3) then
        if (giw_flag(j) == 1) then
            dstor = sno_hru(j) - snoprev + sol_sw(j) - swprev +
     &        shallst(j) - shallstp + deepst(j) - deepstp +
     &        surf_bs(1,j) - bsprev + bss(1,j) - bssprev +
     &        canstor(j) - canstor_prev +
     &        giw_vol_mm(j) - giw_vol_mm_begin
        end if
        if (giw_flag(j) == 0) then
            dstor = sno_hru(j) - snoprev + sol_sw(j) - swprev +
     &        shallst(j) - shallstp + deepst(j) - deepstp +
     &        surf_bs(1,j) - bsprev + bss(1,j) - bssprev +
     &        canstor(j) - canstor_prev
        end if
      else
        dstor = sno_hru(j) - snoprev + sol_sw(j) - swprev +
     &      shallst(j) - shallstp + deepst(j) - deepstp +
     &      hhsurf_bs(1,j,nstep) - bsprev + bss(1,j) - bssprev
      end if

      if (drain_to(j) /= 0) then
        h2oloss = subp(j) - qday - latq(j) - qtile - etday - gw_q(j)
     &          + aird(j) - revapday + rchrg(j) - sepbtm(j) - tloss -
     &          gw_qdeep(j) - giw_flwin_share_mm
      end if
      if (drain_to(j) == 0) then
        h2oloss = subp(j) - qday - latq(j) - qtile - etday - gw_q(j)
     &          + aird(j) - revapday + rchrg(j) - sepbtm(j) - tloss -
     &          gw_qdeep(j)
      end if

      dif = 0.
      dif = Abs(dstor - h2oloss)
!      if (Abs(dstor - h2oloss)>0.001) then
!        write(17,*) sub_number,";",j,";",curyr,";",iida,";",
!     &      giw_flag(j),";",drain_to(j),";",dstor,";",h2oloss,";",
!     &      dif,";",sno_hru(j),";",snoprev,";",sol_sw(j),";",swprev,";",
!     &      shallst(j),";",shallstp,";",deepst(j),";",deepstp,";",
!     &      surf_bs(1,j),";",bsprev,";",bss(1,j),";",bssprev,";",
!     &      subp(j),";",qday,";",
!     &      latq(j),";",qtile,";",gw_q(j),";",aird(j),";",
!     &      revapday,";",rchrg(j),";",sepbtm(j),";",tloss,";",
!     &      gw_qdeep(j),";",giw_flwin_share_mm,";",etday,";",
!     &      ep_day,";",es_day,";",giw_evap_out_mm,";",
!     &      snoev,";",giw_vol_mm(j),";",giw_vol_mm_begin,";",
!     &      giw_precip_in_mm(j),";",giw_spillage_out_mm(j),";",
!     &      qday
!      end if

      if (Abs(dstor - h2oloss)>0.003) then

        !!Note: giw_vol_mm_begin is the quantity of water in the giw
        !!at the beginning of the giw hru's simulation in the subbasin.f
        !!loop. Thus, it is the quantity of water in the giw after all
        !!upgradient hrus have added inflow to the giw reservoir.
        !!giw_vol_prev_mm(j) is the quantity of water in the giw prior
        !!to all upgradient hru's adding water.
        if(giw_flag(j) == 1) then
            giw_q_in_mm = giw_vol_mm_begin - giw_vol_prev_mm(j)
        end if

      write(17,*) sub_number,";",j,";",curyr,";",iida,";",
     & giw_flag(j),";",drain_to(j),";",dstor,";",h2oloss,";",
     & dif,";",qday,";",latq(j),";",gw_q(j),";",gw_qdeep(j),";",
     & etday,";",giw_vol_prev_mm(j),";",giw_vol_mm_begin,";",
     & giw_vol_mm(j),";",giw_volx_mm(j),";",giw_q_in_mm,";",
     & giw_precip_in_mm(j),";",giw_spillage_out_mm(j),";",
     & giw_evap_out_mm,";",giw_flwin_share_mm,";",
     & record_giw_seepage_in_mm

      end if

      return
      end
