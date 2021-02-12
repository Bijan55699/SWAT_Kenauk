      subroutine recordgiw

        use parm

        integer :: subbasin_number,global_hru_number,local_hru_number
        integer :: year, julian_day, j
        real :: hru_area_ha,hru_area_m2
        character(len=50) :: dir_name, txt_name, cmd_command,hruid

        if (curyr > nyskip) then

            j = 0
            j = ihru
            hru_area_ha = 0.
            hru_area_ha = hru_ha(j)
            hru_area_m2 = 0.
            hru_area_m2 = hru_area_ha * 10000.
            subbasin_number = 0
            subbasin_number = inum1
            global_hru_number = 0
            global_hru_number = j
            local_hru_number = 0
            local_hru_number = iihru
            year = 0
            year = iyr
            julian_day = 0
            julian_day = iida

            if(giw_vol_mm(j) < 0.)then
                write(*,*)"recordgiw SUBROUTINE ERROR 1"
                call Exit(1)
            end if
            
            open(unit=99,file="output.giw",
     &              access='stream',form='formatted',action='write',
     &              position='append')
            write(99,101) subbasin_number,",",global_hru_number,",",
     &          local_hru_number,",",year,",",julian_day,",",
     &          giw_vol_mm(j),",",giw_volx_mm(j),",",giw_sa_m2(j),",",
     &          giw_type(j),",",giw_spillage_out_mm(j),",",
     &          giw_seepage_out_mm(j),",",giw_evap_out_mm,",",
     &          giw_flwin_mm(j),",",giw_flwin_qday_mm(j),",",
     &          giw_flwin_latq_mm(j),",",giw_flwin_gwq_mm(j),",",
     &          giw_flwin_deepgw_mm(j),",",giw_precip_in_mm(j),",",
     &          record_giw_seepage_in_mm,",",tmpav(j),",",etday,",",
     &          ep_day,",",es_day,",",canev,",",lon_giw(j),",",
     &          lat_giw(j),",",OID_giw(j),",",giw_typo(j),",",
     $          pet_GIW(j),",",sol_st(1,j),",", sol_fc(1,j)      
      !      close(99)

        end if

      return
  101 format(5(1i0,1a1),3(1f0.3,1a1),1(1i0,1a1),10(1f0.3,1a1),
     &  4(1f0.3,1a1),1f0.3,1a1,2(1f0.3,1a1),2(1i0,1a1),2(1f0.3,1a1),
     &  1f0.3)
      end
