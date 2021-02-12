      subroutine recordgiwprelim

        use parm

        logical :: dir_e, txt_e
        integer :: j, slen
        character(len=50) :: dir_name, txt_name, cmd_command, hruid

        inquire(file="output.giw",exist=dir_e)
        if(dir_e) then
            cmd_command = "del output.giw"
            call execute_command_line(cmd_command)
        end if
        !cmd_command = "mkdir GIWOUTPUT\\"
        !call execute_command_line(cmd_command)

        !do j = 1,mhru
        !    if (giw_flag(j) == 1) then
        !        write(hruid,'(I0)') j
        !        txt_name = "GIWOUTPUT\\GIW"//hruid
                open(unit=99,file="output.giw",
     &              access='stream',form='formatted',action='write',
     &              position='append')
                write(99,*) "SUBBASIN,GLOBAL_HRU,"//
     &          "LOCAL_HRU,YEAR,J_DATE,VOL_MM,"//
     &          "VOLX_MM,AREA_M2,TYPE_CODE,"//
     &          "SPILL_OUT_MM,SEEP_OUT_MM,ET_OUT_MM,"//
     &          "FLWIN_TOTAL_MM,FLWIN_QDAY_MM,"//
     &          "FLWIN_LATQ_MM,FLWIN_GWQ_MM,"//
     &          "FLWIN_DPGWQ_MM,PRECIP_IN_MM,SEEP_IN_MM,"//
     &          "AIR_TEMP_C,ET_MM_HRUTOTAL,TRANSPIRATION_MM,"//
     &          "SOIL_EVAP_MM,CANOPY_EVAP_MM"
        !        close(99)
        !    end if
        !end do

        !do j = 1,mhru
        !    if (giw_flag(j) == 1) then
        !        write(hruid,'(I0)') j
        !        txt_name = "GIWOUTPUT\\GIW"//hruid
        !        inquire(file=trim(txt_name)//".csv",exist=txt_e)
        !        if(.not.txt_e) then
        !            write(*,*)"recordgiwprelim subroutine fail; ERROR 1"
        !            call Exit(1)
        !        end if
        !    end if
        !end do

        return
        end
