      subroutine readgiw (giw_file_title)

      use parm

      character (len=80) :: titldum
      character (len=13) :: giw_file_title
      character(len=20) :: help
      integer :: local_hru_number,global_hru_number,line_drain_to
      integer :: line_giw_type, subbasin_number
      integer :: line_OID,line_giw_typo ! Mohammad
      integer :: line_giw_flag, j, line_RAF_flag
      real :: line_giw_k, line_giw_volx_m3, line_giw_volx_mm
      real :: line_lat,line_lon,line_areaha  ! Mohammad      
      real :: line_Z_RAF, line_Z_SW
      real :: hru_area_ha, hru_area_m2
      
      cevapgiw=0. ! coefficient of evaporation from GIW
      cbedgiw =0. ! coefficient of bed of GIW
      
      !Mohammad: adding giw_rav as a calibration parameter
      open(1,FILE='para_IW.txt',action='read') ! the file already exist
      read(1,*) help
      read(1,1000) cevapgiw,cbedgiw
      close(1)
      !End Mohammad
      
      subbasin_number = i

      call caps(giw_file_title)
      open(444,file=giw_file_title)
      read(444,5100) titldum

      do j = 1, hrutot(subbasin_number)

        local_hru_number = 0
        global_hru_number = 0
        hru_area_ha = 0.
        hru_area_m2 = 0.
        line_drain_to = 0
        line_giw_type = 0
        line_giw_flag = 0
        
        line_giw_k = 0.
        line_giw_volx_m3 = 0.
        line_giw_volx_mm = 0.
        
        line_OID = 0
        line_giw_typo = 0
        line_lat = 0.
        line_lon = 0.
        line_areaha = 0.

        read(444,5200) global_hru_number,local_hru_number,
     &      line_drain_to,line_giw_flag,line_giw_type,
     &      line_giw_k,line_giw_volx_m3,line_lon,line_lat,line_OID,
     &      line_giw_typo,line_areaha
        

        !!DECLARE AREA VALUES
        !hru_area_ha = hru_ha(global_hru_number)
        !hru_area_m2 = hru_area_ha * 10000.

        giw_flag(global_hru_number) = line_giw_flag
        giw_type(global_hru_number) = line_giw_type
        drain_to(global_hru_number) = line_drain_to
        lat_giw(global_hru_number) = line_lat  ! Mohammad
        lon_giw(global_hru_number) = line_lon  ! Mohammad
        OID_giw(global_hru_number) = line_OID  ! Mohammad
        giw_typo(global_hru_number) = line_giw_typo ! Mohammad
        SAHAGIW(global_hru_number) = line_areaha ! Mohammad: surface area of GIW in ha
        
        hru_area_ha = SAHAGIW(global_hru_number)
        hru_area_m2 = hru_area_ha * 10000.
        
        ! calculation of VolmaxGIW for Kenauk changed based on givn surface area( in ha) in the last column of .giw files

        !line_giw_volx_mm = (line_giw_volx_m3/hru_area_m2)*1000.
        
        !if (global_hru_number == 55) then
        !  line_giw_volx_mm = 1000. * (line_giw_volx_m3 / 34.85)  ! V = 192.28 *h
        !else
        !  line_giw_volx_mm = 1000. * (line_giw_volx_m3 / 192.28)  ! V = 192.28 *h
        !end if
        
        if (global_hru_number == 55) then  ! for HP1
          line_giw_volx_mm = 160.3 * (line_giw_volx_m3 ** 0.3380)  
        elseif (global_hru_number == 110) then ! for S1
          line_giw_volx_mm = 118.15 * (line_giw_volx_m3 ** 0.4745)
        elseif (global_hru_number == 664) then ! for HP16
          line_giw_volx_mm = (line_giw_volx_m3 * 8.2652)
        elseif (global_hru_number == 607) then ! for HP17
          line_giw_volx_mm = (line_giw_volx_m3 * 2.3718)
        elseif (global_hru_number == 816) then ! for HP20
          line_giw_volx_mm = (line_giw_volx_m3 * 5.518)
        elseif (global_hru_number == 1076) then ! for HP26
          line_giw_volx_mm = 2.52 * (line_giw_volx_m3 ** 0.802)
        elseif (global_hru_number == 1799) then ! for HP27
          line_giw_volx_mm = 4.884* (line_giw_volx_m3 ** 0.8)
        elseif (global_hru_number == 1676) then ! for HP28
          line_giw_volx_mm = 28.77* (line_giw_volx_m3 ** 0.5812)
        elseif (global_hru_number == 1210) then ! for S6
          line_giw_volx_mm = 195.0* (line_giw_volx_m3 ** 0.4553) 
        else
          line_giw_volx_mm = 63.79 * (line_giw_volx_m3 ** 0.5467)  ! other wetlands
        end if
        
        
        giw_volx_mm(global_hru_number) = line_giw_volx_mm
        giw_vol_mm(global_hru_number) = 0.05 * line_giw_volx_mm
        if(line_giw_k > 0.) then
            giw_k(global_hru_number) = line_giw_k
        end if
        if(line_giw_k <= 0.) then
            giw_k(global_hru_number) = 0.
        end if

      end do
      close(444)

      return
 5100 format (a)
 5200 format (5i15,2f15.3,2f15.3,2i15,f15.4)
 1000 format (f7.5,1X,f7.5)
      end
