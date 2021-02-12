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
        !  line_giw_volx_mm = (line_giw_volx_m3/34.85)*1000.  ! V = 192.28 *h
        !else
          line_giw_volx_mm = (line_giw_volx_m3/192.28)*1000.  ! V = 192.28 *h
        !end if
        
        
        giw_volx_mm(global_hru_number) = line_giw_volx_mm
        giw_vol_mm(global_hru_number) = 0.5 * line_giw_volx_mm
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
 1000 format (f4.2,1X,f4.2)
      end
