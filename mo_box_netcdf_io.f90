MODULE mo_box_netcdf_io

  USE netcdf

  IMPLICIT NONE

  PUBLIC netcdf_dimensions, netcdf_input
  PUBLIC netcdf_output_ccn, netcdf_output_ccnspec
  PUBLIC netcdf_output_nconc, netcdf_output_mconc

  !PRIVATE

  !--- Subroutines:

CONTAINS

 !--- get input dimensions from reanalysis
  SUBROUTINE netcdf_dimensions(ifile, nT, nLe, nLa, nLo, nhym, nhyi)
   
  IMPLICIT NONE
   
    ! Input parameters
    CHARACTER(len=*), INTENT(in)	:: ifile

    ! Output parameters
    INTEGER,INTENT(out)			:: nT, nLe, nLa, nLo, nhym, nhyi    


    ! Local parameters   
    INTEGER				:: status, ncID, timeID, levID, latID, lonID, hymID, hyiID
    LOGICAL				:: fileexists


    INQUIRE(file=ifile,exist=fileexists)
    IF (fileexists) then
 

      status = nf90_OPEN(ifile,0,ncID)  			!OPEN netCDF file
       IF(status /= nf90_NoErr) CALL handle_err(status)
       !WRITE(*,*) 'file ID', ncID    

      status = nf90_inq_dimid(ncID,'time',timeID) 
       IF(status /= nf90_NoErr) CALL handle_err(status)
       !WRITE(*,*) 'time ID', timeID	  

      status = nf90_inquire_DIMENSION(ncID, timeID, len = nT)	!get number of Time
       IF(status /= nf90_NoErr) CALL handle_err(status)
       !WRITE(*,*) 'ntimes', nT	 

      status = nf90_inq_dimid(ncID,'lev',levID) 
       IF(status /= nf90_NoErr) CALL handle_err(status)
       !WRITE(*,*) 'lev ID', levID	  

      status = nf90_inquire_DIMENSION(ncID, levID, len = nLe)	!get number of Lev
       IF(status /= nf90_NoErr) CALL handle_err(status)
       !WRITE(*,*) 'nLev', nLe 	 

      status = nf90_inq_dimid(ncID,'lat',latID) 
       IF(status /= nf90_NoErr) CALL handle_err(status)
       !WRITE(*,*) 'lat ID', latID	  

      status = nf90_inquire_DIMENSION(ncID, latID, len = nLa)	!get number of Lat   
       IF(status /= nf90_NoErr) CALL handle_err(status)
       !WRITE(*,*) 'nLat', nLa	 

      status = nf90_inq_dimid(ncID,'lon',lonID) 
       IF(status /= nf90_NoErr) CALL handle_err(status)
       !WRITE(*,*) 'lon ID', lonID	  

      status = nf90_inquire_DIMENSION(ncID, lonID, len = nLo)	!get number of Lon
       IF(status /= nf90_NoErr) CALL handle_err(status)
       !WRITE(*,*) 'nLon', nLo
       
      status = nf90_inq_dimid(ncID,'nhym', hymID) 
       IF(status /= nf90_NoErr) CALL handle_err(status)
       !WRITE(*,*) 'hym ID', hymID	  

      status = nf90_inquire_DIMENSION(ncID, hymID, len = nhym)	!get number of hybrid coeff
       IF(status /= nf90_NoErr) CALL handle_err(status)
       !WRITE(*,*) 'nhym', nhym       	 

      status = nf90_inq_dimid(ncID,'nhyi', hyiID) 
       IF(status /= nf90_NoErr) CALL handle_err(status)
       !WRITE(*,*) 'hyi ID', hyiID	  

      status = nf90_inquire_DIMENSION(ncID, hyiID, len = nhyi)	!get number of hybrid coeff
       IF(status /= nf90_NoErr) CALL handle_err(status)
       !WRITE(*,*) 'nhyi', nhyi       	 



      !--- close netCDF file
      status = nf90_close(ncID)
       IF(status /= nf90_NoErr) CALL handle_err(status)  

    ELSE
      STOP "Stopped: input file does not exist"
    ENDIF
   
  
  END SUBROUTINE netcdf_dimensions




  SUBROUTINE netcdf_input(ifile1, ifile2, nT, nLe, nLa, nLo, nhym, nhyi, &
  			VarTime, VarLev, VarLat, VarLon, VarHyam, VarHybm, VarHyai, VarHybi, &
 		    	VarSP, VarZ, VarP, VarT, VarQ, &
			VarMix_SSs, VarMix_SSm, VarMix_SSl, &
		    	VarMix_DUs, VarMix_DUm, VarMix_DUl, &
		    	VarMix_OMn, VarMix_OMh, VarMix_BCn, VarMix_BCh, VarMix_SU)

  USE mo_kind,          ONLY: dp

  IMPLICIT NONE
   
    ! Input parameters
    CHARACTER(len=*), INTENT(in)	:: ifile1, ifile2
    INTEGER,INTENT(in)			:: nT, nLe, nLa, nLo, nhym, nhyi

    ! Output parameters
    REAL(dp), INTENT(out)		:: VarTime(nT)
    REAL(dp), INTENT(out)		:: VarLev(nLe)    
    REAL(dp), INTENT(out)               :: VarLat(nLa)    
    REAL(dp), INTENT(out)		:: VarLon(nLo)
    REAL(dp), INTENT(out)               :: VarHyam(nhym)
    REAL(dp), INTENT(out)               :: VarHybm(nhym)
    REAL(dp), INTENT(out)               :: VarHyai(nhyi)
    REAL(dp), INTENT(out)               :: VarHybi(nhyi)    
    REAL(dp), INTENT(out)               :: VarSP(nLo,nLa,nT)
    REAL(dp), INTENT(out)               :: VarZ(nLo,nLa,nT)    
    REAL(dp), INTENT(out)		:: VarP(nLo,nLa,nLe,nT)  
    REAL(dp), INTENT(out)		:: VarT(nLo,nLa,nLe,nT)  
    REAL(dp), INTENT(out)		:: VarQ(nLo,nLa,nLe,nT)             
    REAL(dp), INTENT(out)		:: VarMix_SSs(nLo,nLa,nLe,nT) 
    REAL(dp), INTENT(out)		:: VarMix_SSm(nLo,nLa,nLe,nT)     
    REAL(dp), INTENT(out)		:: VarMix_SSl(nLo,nLa,nLe,nT)     
    REAL(dp), INTENT(out)		:: VarMix_DUs(nLo,nLa,nLe,nT)     
    REAL(dp), INTENT(out)		:: VarMix_DUm(nLo,nLa,nLe,nT)     
    REAL(dp), INTENT(out)		:: VarMix_DUl(nLo,nLa,nLe,nT)  
    REAL(dp), INTENT(out)		:: VarMix_OMn(nLo,nLa,nLe,nT)  
    REAL(dp), INTENT(out)		:: VarMix_OMh(nLo,nLa,nLe,nT)  
    REAL(dp), INTENT(out)		:: VarMix_BCn(nLo,nLa,nLe,nT)  
    REAL(dp), INTENT(out)		:: VarMix_BCh(nLo,nLa,nLe,nT)  
    REAL(dp), INTENT(out)		:: VarMix_SU(nLo,nLa,nLe,nT)

    ! Local parameters 
          
    INTEGER				:: lo, la, it
    INTEGER				:: status, ncID1, ncID2
    INTEGER				:: timeID, levID, latID, lonID, varID
    INTEGER				:: qID, tID, spID, zID, hyamID, hybmID, hyaiID, hybiID 
    INTEGER				:: varID1, varID2, varID3, varID4, varID5
    INTEGER				:: varID6, varID7, varID8, varID9,varID10, varID11 
    CHARACTER(len=7)			:: Paramname(11)



    ! specify aerosol parameters to be read in
    Paramname =  (/'aermr01', 'aermr02', 'aermr03', 'aermr04', 'aermr05', 'aermr06', &
 		   'aermr07', 'aermr08', 'aermr09', 'aermr10', 'aermr11'/) 	     


    !-----------------------------------------------------
    !--- open  netCDF file1
    status = nf90_OPEN(ifile1,0,ncID1) 		
      IF(status /= nf90_NoErr) CALL handle_err(status)
      !WRITE(*,*) 'file ID1', ncID1    

    ! time
    status = nf90_inq_varid(ncID1,'time',timeID) 
      IF(status /= nf90_NoErr) CALL handle_err(status)
      !WRITE(*,*) 'time ID', timeID	       
    status = nf90_get_var(ncID1,timeID,VarTime)		
      IF(status /= nf90_NoErr) CALL handle_err(status)

    ! lev
    status = nf90_inq_varid(ncID1,'lev',levID) 
      IF(status /= nf90_NoErr) CALL handle_err(status)
      !WRITE(*,*) 'lev ID', levID	       
    status = nf90_get_var(ncID1,levID,VarLev)		
      IF(status /= nf90_NoErr) CALL handle_err(status)
 
    ! hyam
    status = nf90_inq_varid(ncID1,'hyam',hyamID) 
      IF(status /= nf90_NoErr) CALL handle_err(status)	       
      !WRITE(*,*) 'hyam ID', hyamID
    status = nf90_get_var(ncID1,hyamID,VarHyam)		
      IF(status /= nf90_NoErr) CALL handle_err(status)

   ! hybm
    status = nf90_inq_varid(ncID1,'hybm',hybmID) 
      IF(status /= nf90_NoErr) CALL handle_err(status)	       
      !WRITE(*,*) 'hybm ID', hybmID
    status = nf90_get_var(ncID1,hybmID,VarHybm)		
      IF(status /= nf90_NoErr) CALL handle_err(status)

    ! hyai
    status = nf90_inq_varid(ncID1,'hyai',hyaiID) 
      IF(status /= nf90_NoErr) CALL handle_err(status)	       
      !WRITE(*,*) 'hyai ID', hyaiID
    status = nf90_get_var(ncID1,hyaiID,VarHyai)		
      IF(status /= nf90_NoErr) CALL handle_err(status)

   ! hybi
    status = nf90_inq_varid(ncID1,'hybi',hybiID) 
      IF(status /= nf90_NoErr) CALL handle_err(status)	       
      !WRITE(*,*) 'hybi ID', hybiID
    status = nf90_get_var(ncID1,hybiID,VarHybi)		
      IF(status /= nf90_NoErr) CALL handle_err(status)
 
    ! lat
    status = nf90_inq_varid(ncID1,'lat',latID) 
      IF(status /= nf90_NoErr) CALL handle_err(status)
      !WRITE(*,*) 'lat ID', latID	       
    status = nf90_get_var(ncID1,latID,VarLat)		
      IF(status /= nf90_NoErr) CALL handle_err(status)

    ! lon
    status = nf90_inq_varid(ncID1,'lon',lonID) 
      IF(status /= nf90_NoErr) CALL handle_err(status)
      !WRITE(*,*) 'lon ID', lonID	       
    status = nf90_get_var(ncID1,lonID,VarLon)		
    IF(status /= nf90_NoErr) CALL handle_err(status)

    ! temperature
    status = nf90_inq_varid(ncID1,'t',tID)
      IF(status /= nf90_NoErr) CALL handle_err(status)
      !WRITE(*,*) 't ID', tID
    status = nf90_get_var(ncID1, tID, VarT)
    IF(status /= nf90_NoErr) CALL handle_err(status)

    ! specific humidity
    status = nf90_inq_varid(ncID1,'q',qID)
      IF(status /= nf90_NoErr) CALL handle_err(status)
      !WRITE(*,*) 'q ID', qID
    status = nf90_get_var(ncID1, qID, VarQ)
    IF(status /= nf90_NoErr) CALL handle_err(status)


    

   ! mass mixing ratio for each aerosol tracer
    status = nf90_inq_varid(ncID1,Paramname(1),varID1) 
      IF(status /= nf90_NoErr) CALL handle_err(status)
      !WRITE(*,*) 'variable ID1', varID1	     
    status = nf90_get_var(ncID1, varID1, VarMix_SSs)
      IF(status /= nf90_NoErr) CALL handle_err(status)

    status = nf90_inq_varid(ncID1,Paramname(2),varID2) 
      IF(status /= nf90_NoErr) CALL handle_err(status)
      !WRITE(*,*) 'variable ID2', varID2	     
    status = nf90_get_var(ncID1, varID2, VarMix_SSm)
      IF(status /= nf90_NoErr) CALL handle_err(status)

    status = nf90_inq_varid(ncID1,Paramname(3),varID3) 
      IF(status /= nf90_NoErr) CALL handle_err(status)
      !WRITE(*,*) 'variable ID3', varID3	     
    status = nf90_get_var(ncID1, varID3, VarMix_SSl)
      IF(status /= nf90_NoErr) CALL handle_err(status)

    status = nf90_inq_varid(ncID1,Paramname(4),varID4) 
      IF(status /= nf90_NoErr) CALL handle_err(status)
      !WRITE(*,*) 'variable ID4', varID4	     
    status = nf90_get_var(ncID1, varID4, VarMix_DUs)
      IF(status /= nf90_NoErr) CALL handle_err(status)

    status = nf90_inq_varid(ncID1,Paramname(5),varID5) 
      IF(status /= nf90_NoErr) CALL handle_err(status)
      !WRITE(*,*) 'variable ID5', varID5	     
    status = nf90_get_var(ncID1, varID5, VarMix_DUm)
      IF(status /= nf90_NoErr) CALL handle_err(status)

    status = nf90_inq_varid(ncID1,Paramname(6),varID6) 
      IF(status /= nf90_NoErr) CALL handle_err(status)
      !WRITE(*,*) 'variable ID6', varID6	     
    status = nf90_get_var(ncID1, varID6, VarMix_DUl)
      IF(status /= nf90_NoErr) CALL handle_err(status)
!msh
    status = nf90_inq_varid(ncID1,Paramname(7),varID7) 
      IF(status /= nf90_NoErr) CALL handle_err(status)
      !WRITE(*,*) 'variable ID7', varID7	     
    status = nf90_get_var(ncID1, varID7, VarMix_OMh)
      IF(status /= nf90_NoErr) CALL handle_err(status)
!msh
    status = nf90_inq_varid(ncID1,Paramname(8),varID8) 
      IF(status /= nf90_NoErr) CALL handle_err(status)
      !WRITE(*,*) 'variable ID8', varID8	     
    status = nf90_get_var(ncID1, varID8, VarMix_OMn)
      IF(status /= nf90_NoErr) CALL handle_err(status)
!msh
    status = nf90_inq_varid(ncID1,Paramname(9),varID9) 
      IF(status /= nf90_NoErr) CALL handle_err(status)
      !WRITE(*,*) 'variable ID9', varID9	     
    status = nf90_get_var(ncID1, varID9, VarMix_BCh)
      IF(status /= nf90_NoErr) CALL handle_err(status)
!msh
    status = nf90_inq_varid(ncID1,Paramname(10),varID10) 
      IF(status /= nf90_NoErr) CALL handle_err(status)
      !WRITE(*,*) 'variable ID10', varID10	     
    status = nf90_get_var(ncID1, varID10, VarMix_BCn)
      IF(status /= nf90_NoErr) CALL handle_err(status)

    status = nf90_inq_varid(ncID1,Paramname(11),varID11) 
      IF(status /= nf90_NoErr) CALL handle_err(status)
      !WRITE(*,*) 'variable ID11', varID11	     
    status = nf90_get_var(ncID1, varID11, VarMix_SU)
      IF(status /= nf90_NoErr) CALL handle_err(status)

    !--- close netCDF file1
    status = nf90_close(ncID1)
      IF(status /= nf90_NoErr) CALL handle_err(status)  




    !-----------------------------------------------------
    !--- open  netCDF file2
    status = nf90_OPEN(ifile2,0,ncID2) 		
      IF(status /= nf90_NoErr) CALL handle_err(status)
      !WRITE(*,*) 'file ID2', ncID2    

    ! surface pressure
    status = nf90_inq_varid(ncID2,'SP',spID) 
      IF(status /= nf90_NoErr) CALL handle_err(status)
      !WRITE(*,*) 'sp ID', spID 	      
    status = nf90_get_var(ncID2, spID, VarSP)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   
    ! surface geopotential
    status = nf90_inq_varid(ncID2,'Z',zID)
      IF(status /= nf90_NoErr) CALL handle_err(status)
      !WRITE(*,*) 'z ID', zID
    status = nf90_get_var(ncID2, zID, VarZ)
    IF(status /= nf90_NoErr) CALL handle_err(status)

    !--- close netCDF file2
    status = nf90_close(ncID2)
      IF(status /= nf90_NoErr) CALL handle_err(status)  



    !WRITE(*,*) 'VarSP', VarSP(1,1,:), ' VarQ10', VarQ(1,1,10,:),' VarQ50', VarQ(1,1,50,:)
    !WRITE(*,*) 'VarHybm', VarHybm(:), ' VarHyam', VarHyam(:)


      DO lo = 1,nLo 
       DO la = 1,nLa
        DO it = 1,nT		  
        VarP(lo,la,:,it) = VarHybm(:)*VarSP(lo,la,it)+VarHyam(:)
        ENDDO
       ENDDO
      ENDDO

    !WRITE(*,*) 'VarP10', VarP(1,1,10,:), ' VarP50', VarP(1,1,50,:)



  END SUBROUTINE netcdf_input  


!--------- OUTPUT --------------------------
!--------------------------------------------------------------------------------------------------
  SUBROUTINE netcdf_output_ccn(ofile1, year, mon, day, &
  			 nT, nLe, nLa, nLo, nhym, nhyi, &
 			 VarTime, VarLev, VarLat, VarLon, VarHyam, VarHybm, VarHyai, VarHybi,&
                         VarSP, VarZ, geop, &
			 VarCCN_01, VarCCN_02, VarCCN_04, &
			 VarCCN_06, VarCCN_08, VarCCN_10)
                         

  USE mo_kind,          ONLY: dp

  IMPLICIT NONE

  ! Input parameters
  CHARACTER(len=*), INTENT(in)       			:: ofile1
  CHARACTER(len=4), INTENT(in)       			:: year
  CHARACTER(len=2), INTENT(in)       			:: mon
  CHARACTER(len=2), INTENT(in)                          :: day
  INTEGER,  INTENT(in)         				:: nT, nLe, nLa, nLo, nhym, nhyi  

  REAL(dp), INTENT(in), DIMENSION(nT)			:: VarTime
  REAL(dp), INTENT(in), DIMENSION(nLe)			:: VarLev
  REAL(dp), INTENT(in), DIMENSION(nLa)			:: VarLat
  REAL(dp), INTENT(in), DIMENSION(nLo)			:: VarLon
  REAL(dp), INTENT(in), DIMENSION(nhym)                 :: VarHyam, VarHybm
  REAL(dp), INTENT(in), DIMENSION(nhyi)                 :: VarHyai, VarHybi
  REAL(dp), INTENT(in), DIMENSION(nLo,nLa,nT)           :: VarSP, VarZ 
  REAL(dp), INTENT(in), DIMENSION(nLo,nLa,nLe,nT)       :: geop
  REAL(dp), INTENT(in), DIMENSION(nLo,nLa,nLe,nT)      	:: VarCCN_01, VarCCN_02, VarCCN_04
  REAL(dp), INTENT(in), DIMENSION(nLo,nLa,nLe,nT)      	:: VarCCN_06, VarCCN_08, VarCCN_10
 

  ! Local parameters
  INTEGER :: ncid1, status 
  INTEGER :: timeID, levID, latID, lonID, hymID, hyiID
  INTEGER :: var1, var2, var3, var4,var5, var6, var7, var8, var9
  INTEGER :: var500, var600
  INTEGER :: var10, var20, var30, var40, var50, var60  


  REAL, PARAMETER	:: FillAtt = -9999.99 
  !REAL(dp), PARAMETER	:: FillAtt2 = -9999.99
  CHARACTER(len=31)	:: timestr = "hours since YYYY-MM-DD 00:00:00"      

  !--- Define the parameters
  timestr(13:16) = year
  timestr(18:19) = mon
  timestr(21:22) = day
 
   
   !------------------------------------------------------------------------
   !--- create output netCDF file
   status = nf90_create(path = ofile1, cmode = NF90_NETCDF4, ncid = ncid1)
    IF(status /= nf90_noerr) CALL handle_err(status)
    !WRITE(*,*) 'def nc'

   !--- Define the dimensions
   status = nf90_def_dim(ncid1, "time", nf90_unlimited, timeID)
    IF(status /= nf90_noerr) CALL handle_err(status)
   status = nf90_def_dim(ncid1, "lev", nLe, levID)
    IF(status /= nf90_noerr) CALL handle_err(status)   
   status = nf90_def_dim(ncid1, "lat", nLa, latID)
    IF(status /= nf90_noerr) CALL handle_err(status)
   status = nf90_def_dim(ncid1, "lon", nLo, lonID)
    IF(status /= nf90_noerr) CALL handle_err(status)
   status = nf90_def_dim(ncid1, "nhym", nhym, hymID)
    IF(status /= nf90_noerr) CALL handle_err(status)  
   status = nf90_def_dim(ncid1, "nhyi", nhyi, hyiID)
    IF(status /= nf90_noerr) CALL handle_err(status)        
   !WRITE(*,*) 'def dim'

   !--- Define global attributes
   status = nf90_put_att(ncid1, NF90_GLOBAL, "author", "Karoline Block")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid1, NF90_GLOBAL, "institution", "University of Leipzig")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid1, NF90_GLOBAL, "source data", &
    "European Centre for Medium-Range Weather Forecasts (ECMWF) Atmospheric Composition Reanalysis 4 (EAC4)")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid1, NF90_GLOBAL, "attribution", "Copernicus Atmosphere Monitoring Service (CAMS)")
    IF(status /= nf90_NoErr) CALL handle_err(status)    
   !WRITE(*,*) 'def global att'       

   ! Time   
   status = nf90_def_var(ncid1, "time", nf90_double,(/ timeID /), var1)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid1, var1, "long_name", "time")
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   status = nf90_put_att(ncid1, var1, "units", timestr)
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   status = nf90_put_att(ncid1, var1, "calendar", "proleptic_gregorian")
    IF(status /= nf90_NoErr) CALL handle_err(status)	       
   status = nf90_put_att(ncid1, var1, "axis", "T")
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   !status = nf90_put_att(ncid1, var1, "_FillValue", FillAtt2)
   ! IF(status /= nf90_NoErr) CALL handle_err(status)	   
   !WRITE(*,*) 'def time'

   ! Lon  
   status = nf90_def_var(ncid1, "lon", nf90_double,(/ lonID /), var2)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid1, var2, "long_name", "longitude")
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   status = nf90_put_att(ncid1, var2, "units", "degrees_east")
    IF(status /= nf90_NoErr) CALL handle_err(status)	    
   status = nf90_put_att(ncid1, var2, "axis", "X")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   !status = nf90_put_att(ncid1, var2, "_FillValue", FillAtt2)
   ! IF(status /= nf90_NoErr) CALL handle_err(status)	   
   !WRITE(*,*) 'def lon'

   ! Lat   
   status = nf90_def_var(ncid1, "lat", nf90_double,(/ latID /), var3)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid1, var3, "long_name", "latitude")
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   status = nf90_put_att(ncid1, var3, "units", "degrees_north")
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   status = nf90_put_att(ncid1, var3, "axis", "Y")
    IF(status /= nf90_NoErr) CALL handle_err(status)  
   !status = nf90_put_att(ncid1, var3, "_FillValue", FillAtt2)    
   ! IF(status /= nf90_NoErr) CALL handle_err(status)	   
   !WRITE(*,*) 'def lat'
   
   ! Lev 
   status = nf90_def_var(ncid1, "lev", nf90_double,(/ levID /), var4)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid1, var4, "long_name", "hybrid level at layer midpoints")
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   status = nf90_put_att(ncid1, var4, "units", "level")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid1, var4, "axis", "Z")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid1, var4, "positive", "down")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid1, var4, "formula", "mlev=hyam+hybm*aps")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid1, var4, "formula_terms", "ap: hyam b: hybm ps: aps")
    IF(status /= nf90_NoErr) CALL handle_err(status)        	    
   !status = nf90_put_att(ncid1, var4, "_FillValue", FillAtt2)
   ! IF(status /= nf90_NoErr) CALL handle_err(status)	   
   !WRITE(*,*) 'def lev'
   
   ! Hyam
   status = nf90_def_var(ncid1, "hyam", nf90_double,(/ hymID /), var5)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid1, var5, "long_name", "hybrid A coefficient at layer midpoints")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid1, var5, "units", "Pa")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   !status = nf90_put_att(ncid1, var5, "_FillValue", FillAtt2)
   ! IF(status /= nf90_NoErr) CALL handle_err(status)
   !WRITE(*,*) 'def hyam'

   ! Hybm
   status = nf90_def_var(ncid1, "hybm", nf90_double,(/ hymID /), var6)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid1, var6, "long_name", "hybrid B coefficient at layer midpoints")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid1, var6, "units", "1")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   !status = nf90_put_att(ncid1, var6, "_FillValue", FillAtt2)
   ! IF(status /= nf90_NoErr) CALL handle_err(status)
   !WRITE(*,*) 'def hybm'

   ! Hyai
   status = nf90_def_var(ncid1, "hyai", nf90_double,(/ hyiID /), var500)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid1, var500, "long_name", "hybrid A coefficient at layer interfaces")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid1, var500, "units", "Pa")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   !status = nf90_put_att(ncid1, var500, "_FillValue", FillAtt2)
   ! IF(status /= nf90_NoErr) CALL handle_err(status)
   !WRITE(*,*) 'def hyai'

   ! Hybi
   status = nf90_def_var(ncid1, "hybi", nf90_double,(/ hyiID /), var600)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid1, var600, "long_name", "hybrid B coefficient at layer interfaces")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid1, var600, "units", "1")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   !status = nf90_put_att(ncid1, var600, "_FillValue", FillAtt2)
   ! IF(status /= nf90_NoErr) CALL handle_err(status)
   !WRITE(*,*) 'def hybi'

   ! SP
   status = nf90_def_var(ncid1, "sp", nf90_float,(/ lonID, latID, timeID /), var7)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid1, var7, "long_name", "surface pressure")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid1, var7, "units", "Pa")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid1, var7, "code", "134")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid1, var7, "table", "128")
    IF(status /= nf90_NoErr) CALL handle_err(status)   
   status = nf90_put_att(ncid1, var7, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   !WRITE(*,*) 'def sp' 

   ! Z
   status = nf90_def_var(ncid1, "z", nf90_float,(/ lonID, latID, timeID /), var8)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid1, var8, "long_name", "surface geopotential")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid1, var8, "units", "m**2 s**-2")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid1, var8, "code", "129")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid1, var8, "table", "128")
    IF(status /= nf90_NoErr) CALL handle_err(status)   
   status = nf90_put_att(ncid1, var8, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   !WRITE(*,*) 'def z' 

   ! Geopotential height
   status = nf90_def_var(ncid1, "gh", nf90_float,(/ lonID, latID, levID, timeID /), var9)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid1, var9, "long_name", "geopotential height")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid1, var9, "units", "gpm")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid1, var9, "code", "156")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid1, var9, "table", "128")            
    IF(status /= nf90_NoErr) CALL handle_err(status)   
   status = nf90_put_att(ncid1, var9, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   !WRITE(*,*) 'def gh'


   ! CCN_01
   status = nf90_def_var(ncid1, "ccn_ss01", nf90_float,(/ lonID, latID, levID, timeID /), var10)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid1, var10, "long_name", "cloud condensation nuclei at 0.1% supersaturation")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid1, var10, "units", "m**-3")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid1, var10, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   !WRITE(*,*) 'def ccn_01'

   ! CCN_02
   status = nf90_def_var(ncid1, "ccn_ss02", nf90_float,(/ lonID, latID, levID, timeID /), var20)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid1, var20, "long_name", "cloud condensation nuclei at 0.2% supersaturation")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid1, var20, "units", "m**-3")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid1, var20, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   !WRITE(*,*) 'def ccn_02'

   ! CCN_04
   status = nf90_def_var(ncid1, "ccn_ss04", nf90_float,(/ lonID, latID, levID, timeID /), var30)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid1, var30, "long_name", "cloud condensation nuclei at 0.4% supersaturation")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid1, var30, "units", "m**-3")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid1, var30, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   !WRITE(*,*) 'def ccn_04'

   ! CCN_06
   status = nf90_def_var(ncid1, "ccn_ss06", nf90_float,(/ lonID, latID, levID, timeID /), var40)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid1, var40, "long_name", "cloud condensation nuclei at 0.6% supersaturation")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid1, var40, "units", "m**-3")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid1, var40, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   !WRITE(*,*) 'def ccn_06'


   ! CCN_08
   status = nf90_def_var(ncid1, "ccn_ss08", nf90_float,(/ lonID, latID, levID, timeID /), var50)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid1, var50, "long_name", "cloud condensation nuclei at 0.8% supersaturation")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid1, var50, "units", "m**-3")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid1, var50, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   !WRITE(*,*) 'def ccn_08'

   ! CCN_10
   status = nf90_def_var(ncid1, "ccn_ss10", nf90_float,(/ lonID, latID, levID, timeID /), var60)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid1, var60, "long_name", "cloud condensation nuclei at 1% supersaturation")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid1, var60, "units", "m**-3")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid1, var60, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   !WRITE(*,*) 'def ccn_10'


   !--- Leave define mode
   status = nf90_enddef(ncid1)
    IF(status /= nf90_NoErr) CALL handle_err(status)  

   !--- write data
   status = nf90_put_var(ncid1, var1, VarTime)	        ! Time  
    IF(status /= nf90_NoErr) CALL handle_err(status) 
    !WRITE(*,*) 'time'
   status = nf90_put_var(ncid1, var2, VarLon)	        ! Lon  
    IF(status /= nf90_NoErr) CALL handle_err(status) 
    !WRITE(*,*) 'lon'
   status = nf90_put_var(ncid1, var3, VarLat)	        ! Lat  
    IF(status /= nf90_NoErr) CALL handle_err(status) 
    !WRITE(*,*) 'lat'
   status = nf90_put_var(ncid1, var4, VarLev)	        ! Lev  
   IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'lev'
   status = nf90_put_var(ncid1, var5, VarHyam)          ! hyam
   IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'hyam'
   status = nf90_put_var(ncid1, var6, VarHybm)          ! hybm
   IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'hybm'
   status = nf90_put_var(ncid1, var500, VarHyai)        ! hyai
   IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'hyai'
   status = nf90_put_var(ncid1, var600, VarHybi)        ! hybi
   IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'hybi'        
   status = nf90_put_var(ncid1, var7, VarSP)            ! SP
   IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'sp'
   status = nf90_put_var(ncid1, var8, VarZ)             ! Z
   IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'z'
   status = nf90_put_var(ncid1, var9, REAL(geop))	! GH
    IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'gh'


   status = nf90_put_var(ncid1, var10, REAL(VarCCN_01))
    IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'ccn_ss01'
   status = nf90_put_var(ncid1, var20, REAL(VarCCN_02))
    IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'ccn_ss02'
   status = nf90_put_var(ncid1, var30, REAL(VarCCN_04))
    IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'ccn_ss04'
   status = nf90_put_var(ncid1, var40, REAL(VarCCN_06))
    IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'ccn_ss06'
   status = nf90_put_var(ncid1, var50, REAL(VarCCN_08))
    IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'ccn_ss08'
   status = nf90_put_var(ncid1, var60, REAL(VarCCN_10))
    IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'ccn_ss10'

    
   !--- close output netCDF file
   status = nf90_close(ncid1)
    IF(status /= nf90_NoErr) CALL handle_err(status)   
 
 END SUBROUTINE netcdf_output_ccn

 !WRITE(*,*) ' '  
 !WRITE(*,*) 'FINISHED OUTPUT CCN'




!--------------------------------------------------------------------------------------------------
  SUBROUTINE  netcdf_output_ccnspec(ofile2, year, mon, day, &
  			 	nT, nLe, nLa, nLo, nhym, nhyi, &
 			 	VarTime, VarLev, VarLat, VarLon, VarHyam, VarHybm, VarHyai, VarHybi, &
                         	VarSP, VarZ, geop, &
				VarCCN_02_SSs,VarCCN_02_SSm,VarCCN_02_SSl, &
				VarCCN_02_OMh,VarCCN_02_BCh,VarCCN_02_SU, &
				VarCCN_08_SSs,VarCCN_08_SSm,VarCCN_08_SSl, &
				VarCCN_08_OMh,VarCCN_08_BCh,VarCCN_08_SU)  
                           

  USE mo_kind,          ONLY: dp

  IMPLICIT NONE

  ! Input parameters
  CHARACTER(len=*), INTENT(in)       			:: ofile2
  CHARACTER(len=4), INTENT(in)       			:: year
  CHARACTER(len=2), INTENT(in)       			:: mon
  CHARACTER(len=2), INTENT(in)                          :: day
  INTEGER,  INTENT(in)         				:: nT, nLe, nLa, nLo, nhym, nhyi  

  REAL(dp), INTENT(in), DIMENSION(nT)			:: VarTime
  REAL(dp), INTENT(in), DIMENSION(nLe)			:: VarLev
  REAL(dp), INTENT(in), DIMENSION(nLa)			:: VarLat
  REAL(dp), INTENT(in), DIMENSION(nLo)			:: VarLon
  REAL(dp), INTENT(in), DIMENSION(nhym)                 :: VarHyam, VarHybm
  REAL(dp), INTENT(in), DIMENSION(nhyi)                 :: VarHyai, VarHybi
  REAL(dp), INTENT(in), DIMENSION(nLo,nLa,nT)           :: VarSP, VarZ 
  REAL(dp), INTENT(in), DIMENSION(nLo,nLa,nLe,nT)       :: geop

  REAL(dp), INTENT(in), DIMENSION(nLo,nLa,nLe,nT) 	:: VarCCN_02_SSs, VarCCN_02_SSm, VarCCN_02_SSl
  REAL(dp), INTENT(in), DIMENSION(nLo,nLa,nLe,nT) 	:: VarCCN_02_OMh, VarCCN_02_BCh, VarCCN_02_SU
  REAL(dp), INTENT(in), DIMENSION(nLo,nLa,nLe,nT) 	:: VarCCN_08_SSs, VarCCN_08_SSm, VarCCN_08_SSl
  REAL(dp), INTENT(in), DIMENSION(nLo,nLa,nLe,nT)       :: VarCCN_08_OMh, VarCCN_08_BCh, VarCCN_08_SU  

 
  ! Local parameters
  INTEGER :: ncid2, status 
  INTEGER :: timeID, levID, latID, lonID, hymID, hyiID
  INTEGER :: var1, var2, var3, var4, var5, var6, var7, var8, var9
  INTEGER :: var500, var600
  INTEGER :: var21, var22, var23, var24, var25, var26
  INTEGER :: var51, var52, var53, var54, var55, var56

  REAL, PARAMETER	:: FillAtt = -9999.99
  !REAL(dp), PARAMETER	:: FillAtt2 = -9999.99
  CHARACTER(len=31)	:: timestr = "hours since YYYY-MM-DD 00:00:00"      

  !--- Define the parameters
  timestr(13:16) = year
  timestr(18:19) = mon
  timestr(21:22) = day
 
   
   !------------------------------------------------------------------------
   !--- create output netCDF file
   status = nf90_create(path = ofile2, cmode = NF90_NETCDF4, ncid = ncid2)
    IF(status /= nf90_noerr) CALL handle_err(status)
    !WRITE(*,*) 'def nc'

   !--- Define the dimensions
   status = nf90_def_dim(ncid2, "time", nf90_unlimited, timeID)
    IF(status /= nf90_noerr) CALL handle_err(status)
   status = nf90_def_dim(ncid2, "lev", nLe, levID)
    IF(status /= nf90_noerr) CALL handle_err(status)   
   status = nf90_def_dim(ncid2, "lat", nLa, latID)
    IF(status /= nf90_noerr) CALL handle_err(status)
   status = nf90_def_dim(ncid2, "lon", nLo, lonID)
    IF(status /= nf90_noerr) CALL handle_err(status)
   status = nf90_def_dim(ncid2, "nhym", nhym, hymID)
    IF(status /= nf90_noerr) CALL handle_err(status)
   status = nf90_def_dim(ncid2, "nhyi", nhyi, hyiID)
    IF(status /= nf90_noerr) CALL handle_err(status)        
   !WRITE(*,*) 'def dim'

   !--- Define global attributes
   status = nf90_put_att(ncid2, NF90_GLOBAL, "author", "Karoline Block")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, NF90_GLOBAL, "institution", "University of Leipzig")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, NF90_GLOBAL, "source data", &
    "European Centre for Medium-Range Weather Forecasts (ECMWF) Atmospheric Composition Reanalysis 4 (EAC4)")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, NF90_GLOBAL, "attribution", "Copernicus Atmosphere Monitoring Service (CAMS)")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   !WRITE(*,*) 'def global att'

   ! Time   
   status = nf90_def_var(ncid2, "time", nf90_double,(/ timeID /), var1)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var1, "long_name", "time")
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   status = nf90_put_att(ncid2, var1, "units", timestr)
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   status = nf90_put_att(ncid2, var1, "calendar", "proleptic_gregorian")
    IF(status /= nf90_NoErr) CALL handle_err(status)	       
   status = nf90_put_att(ncid2, var1, "axis", "T")
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   !status = nf90_put_att(ncid2, var1, "_FillValue", FillAtt2)
   ! IF(status /= nf90_NoErr) CALL handle_err(status)	   
   !WRITE(*,*) 'def time'

   ! Lon  
   status = nf90_def_var(ncid2, "lon", nf90_double,(/ lonID /), var2)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var2, "long_name", "longitude")
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   status = nf90_put_att(ncid2, var2, "units", "degrees_east")
    IF(status /= nf90_NoErr) CALL handle_err(status)	    
   status = nf90_put_att(ncid2, var2, "axis", "X")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   !status = nf90_put_att(ncid2, var2, "_FillValue", FillAtt2)
   ! IF(status /= nf90_NoErr) CALL handle_err(status)	   
   !WRITE(*,*) 'def lon'

   ! Lat   
   status = nf90_def_var(ncid2, "lat", nf90_double,(/ latID /), var3)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var3, "long_name", "latitude")
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   status = nf90_put_att(ncid2, var3, "units", "degrees_north")
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   status = nf90_put_att(ncid2, var3, "axis", "Y")
    IF(status /= nf90_NoErr) CALL handle_err(status)  
   !status = nf90_put_att(ncid2, var3, "_FillValue", FillAtt2)    
   ! IF(status /= nf90_NoErr) CALL handle_err(status)	   
   !WRITE(*,*) 'def lat'
   
   ! Lev 
   status = nf90_def_var(ncid2, "lev", nf90_double,(/ levID /), var4)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var4, "long_name", "hybrid level at layer midpoints")
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   status = nf90_put_att(ncid2, var4, "units", "level")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var4, "axis", "Z")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var4, "positive", "down")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var4, "formula", "mlev=hyam+hybm*aps")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var4, "formula_terms", "ap: hyam b: hybm ps: aps")
    IF(status /= nf90_NoErr) CALL handle_err(status)        	    
   !status = nf90_put_att(ncid2, var4, "_FillValue", FillAtt2)
   ! IF(status /= nf90_NoErr) CALL handle_err(status)	   
   !WRITE(*,*) 'def lev'
   
   ! Hyam
   status = nf90_def_var(ncid2, "hyam", nf90_double,(/ hymID /), var5)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var5, "long_name", "hybrid A coefficient at layer midpoints")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var5, "units", "Pa")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   !status = nf90_put_att(ncid2, var5, "_FillValue", FillAtt2)
   ! IF(status /= nf90_NoErr) CALL handle_err(status)
   !WRITE(*,*) 'def hyam'

   ! Hybm
   status = nf90_def_var(ncid2, "hybm", nf90_double,(/ hymID /), var6)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var6, "long_name", "hybrid B coefficient at layer midpoints")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var6, "units", "1")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   !status = nf90_put_att(ncid2, var6, "_FillValue", FillAtt2)
   ! IF(status /= nf90_NoErr) CALL handle_err(status)
   !WRITE(*,*) 'def hybm'

   ! Hyai
   status = nf90_def_var(ncid2, "hyai", nf90_double,(/ hyiID /), var500)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var500, "long_name", "hybrid A coefficient at layer interfaces")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var500, "units", "Pa")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   !status = nf90_put_att(ncid2, var500, "_FillValue", FillAtt2)
   ! IF(status /= nf90_NoErr) CALL handle_err(status)
   !WRITE(*,*) 'def hyai'

   ! Hybi
   status = nf90_def_var(ncid2, "hybi", nf90_double,(/ hyiID /), var600)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var600, "long_name", "hybrid B coefficient at layer interfaces")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var600, "units", "1")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   !status = nf90_put_att(ncid2, var600, "_FillValue", FillAtt2)
   ! IF(status /= nf90_NoErr) CALL handle_err(status)
   !WRITE(*,*) 'def hybi'

   ! SP
   status = nf90_def_var(ncid2, "sp", nf90_float,(/ lonID, latID, timeID /), var7)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var7, "long_name", "surface pressure")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var7, "units", "Pa")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var7, "code", "134")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var7, "table", "128")
    IF(status /= nf90_NoErr) CALL handle_err(status)   
   status = nf90_put_att(ncid2, var7, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   !WRITE(*,*) 'def sp' 

   ! Z
   status = nf90_def_var(ncid2, "z", nf90_float,(/ lonID, latID, timeID /), var8)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var8, "long_name", "surface geopotential")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var8, "units", "m**2 s**-2")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var8, "code", "129")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var8, "table", "128")
    IF(status /= nf90_NoErr) CALL handle_err(status)   
   status = nf90_put_att(ncid2, var8, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   !WRITE(*,*) 'def z' 

   ! Geopotential height
   status = nf90_def_var(ncid2, "gh", nf90_float,(/ lonID, latID, levID, timeID /), var9)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var9, "long_name", "geopotential height")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var9, "units", "gpm")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var9, "code", "156")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var9, "table", "128")            
    IF(status /= nf90_NoErr) CALL handle_err(status)   
   status = nf90_put_att(ncid2, var9, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'def gh'


   
   ! CCN_02_SSs
   status = nf90_def_var(ncid2, "ccn_ss02_SSs", nf90_float,(/ lonID, latID, levID, timeID /), var21)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var21, "long_name", &
   "small mode sea salt cloud condensation nuclei at 0.2% supersaturation")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var21, "units", "m**-3")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var21, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   !WRITE(*,*) 'def ccn_02_SSs'
   
   ! CCN_02_SSm
   status = nf90_def_var(ncid2, "ccn_ss02_SSm", nf90_float,(/ lonID, latID, levID, timeID /), var22)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var22, "long_name", &
   "medium mode sea salt cloud condensation nuclei at 0.2% supersaturation")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var22, "units", "m**-3")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var22, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   !WRITE(*,*) 'def ccn_02_SSm'
   
   ! CCN_02_SSl
   status = nf90_def_var(ncid2, "ccn_ss02_SSl", nf90_float,(/ lonID, latID, levID, timeID /), var23)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var23, "long_name", &
   "large mode sea salt cloud condensation nuclei at 0.2% supersaturation")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var23, "units", "m**-3")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var23, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   !WRITE(*,*) 'def ccn_02_SSl'
   
   ! CCN_02_OMh
   status = nf90_def_var(ncid2, "ccn_ss02_OMh", nf90_float,(/ lonID, latID, levID, timeID /), var24)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var24, "long_name", &
   "hydrophilic organic matter cloud condensation nuclei at 0.2% supersaturation")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var24, "units", "m**-3")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var24, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   !WRITE(*,*) 'def ccn_02_OMh'
   
   ! CCN_02_BCh
   status = nf90_def_var(ncid2, "ccn_ss02_BCh", nf90_float,(/ lonID, latID, levID, timeID /), var25)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var25, "long_name", &
   "hydrophilic black carbon cloud condensation nuclei at 0.2% supersaturation")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var25, "units", "m**-3")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var25, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   !WRITE(*,*) 'def ccn_02_BCh'

   ! CCN_02_SU
   status = nf90_def_var(ncid2, "ccn_ss02_SO4", nf90_float,(/ lonID, latID, levID, timeID /), var26)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var26, "long_name", &
   "sulfate cloud condensation nuclei at 0.2% supersaturation")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var26, "units", "m**-3")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var26, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   !WRITE(*,*) 'def ccn_02_SU'

   
   ! CCN_08_SSs
   status = nf90_def_var(ncid2, "ccn_ss08_SSs", nf90_float,(/ lonID, latID, levID, timeID /), var51)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var51, "long_name", &
   "small mode sea salt cloud condensation nuclei at 0.8% supersaturation")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var51, "units", "m**-3")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var51, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   !WRITE(*,*) 'def ccn_08_SSs'
   
   ! CCN_08_SSm
   status = nf90_def_var(ncid2, "ccn_ss08_SSm", nf90_float,(/ lonID, latID, levID, timeID /), var52)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var52, "long_name", &
   "medium mode sea salt cloud condensation nuclei at 0.8% supersaturation")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var52, "units", "m**-3")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var52, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   !WRITE(*,*) 'def ccn_08_SSm'
   
   ! CCN_08_SSl
   status = nf90_def_var(ncid2, "ccn_ss08_SSl", nf90_float,(/ lonID, latID, levID, timeID /), var53)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var53, "long_name", &
   "large mode sea salt cloud condensation nuclei at 0.8% supersaturation")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var53, "units", "m**-3")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var53, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   !WRITE(*,*) 'def ccn_08_SSl'
   
   ! CCN_08_OMh
   status = nf90_def_var(ncid2, "ccn_ss08_OMh", nf90_float,(/ lonID, latID, levID, timeID /), var54)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var54, "long_name", &
   "hydrophilic organic matter cloud condensation nuclei at 0.8% supersaturation")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var54, "units", "m**-3")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var54, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   !WRITE(*,*) 'def ccn_08_OMh'
   
   ! CCN_08_BCh
   status = nf90_def_var(ncid2, "ccn_ss08_BCh", nf90_float,(/ lonID, latID, levID, timeID /), var55)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var55, "long_name", &
   "hydrophilic black carbon cloud condensation nuclei at 0.8% supersaturation")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var55, "units", "m**-3")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var55, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   !WRITE(*,*) 'def ccn_08_BCh'

   ! CCN_08_SU
   status = nf90_def_var(ncid2, "ccn_ss08_SO4", nf90_float,(/ lonID, latID, levID, timeID /), var56)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var56, "long_name", &
   "sulfate cloud condensation nuclei at 0.8% supersaturation")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var56, "units", "m**-3")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid2, var56, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   !WRITE(*,*) 'def ccn_08_SU'

   !--- Leave define mode
   status = nf90_enddef(ncid2)
    IF(status /= nf90_NoErr) CALL handle_err(status)  

   !--- write data
   status = nf90_put_var(ncid2, var1, VarTime)	        ! Time  
    IF(status /= nf90_NoErr) CALL handle_err(status) 
    !WRITE(*,*) 'time'
   status = nf90_put_var(ncid2, var2, VarLon)	        ! Lon  
    IF(status /= nf90_NoErr) CALL handle_err(status) 
    !WRITE(*,*) 'lon'
   status = nf90_put_var(ncid2, var3, VarLat)	        ! Lat  
    IF(status /= nf90_NoErr) CALL handle_err(status) 
    !WRITE(*,*) 'lat'
   status = nf90_put_var(ncid2, var4, VarLev)	        ! Lev  
   IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'lev'
   status = nf90_put_var(ncid2, var5, VarHyam)          ! hyam
   IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'hyam'
   status = nf90_put_var(ncid2, var6, VarHybm)          ! hybm
   IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'hybm'
   status = nf90_put_var(ncid2, var500, VarHyai)        ! hyai
   IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'hyai'
   status = nf90_put_var(ncid2, var600, VarHybi)        ! hybi
   IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'hybi'        
   status = nf90_put_var(ncid2, var7, VarSP)            ! SP
   IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'sp'
   status = nf90_put_var(ncid2, var8, VarZ)             ! Z
   IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'z'
   status = nf90_put_var(ncid2, var9, REAL(geop))	! GH
    IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'gh'


   status = nf90_put_var(ncid2, var21, REAL(VarCCN_02_SSs))
    IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'ccn_ss02_SSs'
   status = nf90_put_var(ncid2, var22, REAL(VarCCN_02_SSm))
    IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'ccn_ss02_SSm'
   status = nf90_put_var(ncid2, var23, REAL(VarCCN_02_SSl))
    IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'ccn_ss02_SSl'
   status = nf90_put_var(ncid2, var24, REAL(VarCCN_02_OMh))
    IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'ccn_ss02_OMh'
   status = nf90_put_var(ncid2, var25, REAL(VarCCN_02_BCh))
    IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'ccn_ss02_BCh'
   status = nf90_put_var(ncid2, var26, REAL(VarCCN_02_SU))
    IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'ccn_ss02_SU'

   status = nf90_put_var(ncid2, var51, REAL(VarCCN_08_SSs))
    IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'ccn_ss08_SSs'
   status = nf90_put_var(ncid2, var52, REAL(VarCCN_08_SSm))
    IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'ccn_ss08_SSm'
   status = nf90_put_var(ncid2, var53, REAL(VarCCN_08_SSl))
    IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'ccn_ss08_SSl'
   status = nf90_put_var(ncid2, var54, REAL(VarCCN_08_OMh))
    IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'ccn_ss08_OMh'
   status = nf90_put_var(ncid2, var55, REAL(VarCCN_08_BCh))
    IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'ccn_ss08_BCh'
   status = nf90_put_var(ncid2, var56, REAL(VarCCN_08_SU))
    IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'ccn_ss08_SU'

    
   !--- close output netCDF file
   status = nf90_close(ncid2)
    IF(status /= nf90_NoErr) CALL handle_err(status)   
 
  END SUBROUTINE netcdf_output_ccnspec

  !WRITE(*,*) ' '  
  !WRITE(*,*) 'FINISHED OUTPUT CCNSPEC'




!--------------------------------------------------------------------------------------------------
  SUBROUTINE  netcdf_output_nconc(ofile3, year, mon, day, &
  			 	nT, nLe, nLa, nLo, nhym, nhyi, &
 			 	VarTime, VarLev, VarLat, VarLon, VarHyam, VarHybm, VarHyai, VarHybi, &
                         	VarSP, VarZ, geop, &
				VarNC,VarNC_SSs,VarNC_SSm,VarNC_SSl, &
				VarNC_DUs,VarNC_DUm,VarNC_DUl, &
				VarNC_OMn,VarNC_OMh,VarNC_BCn, &
				VarNC_BCh, VarNC_SU) 



  USE mo_kind,          ONLY: dp

  IMPLICIT NONE

  ! Input parameters
  CHARACTER(len=*), INTENT(in)       			:: ofile3
  CHARACTER(len=4), INTENT(in)       			:: year
  CHARACTER(len=2), INTENT(in)       			:: mon
  CHARACTER(len=2), INTENT(in)                          :: day
  INTEGER,  INTENT(in)         				:: nT, nLe, nLa, nLo, nhym, nhyi  

  REAL(dp), INTENT(in), DIMENSION(nT)			:: VarTime
  REAL(dp), INTENT(in), DIMENSION(nLe)			:: VarLev
  REAL(dp), INTENT(in), DIMENSION(nLa)			:: VarLat
  REAL(dp), INTENT(in), DIMENSION(nLo)			:: VarLon
  REAL(dp), INTENT(in), DIMENSION(nhym)                 :: VarHyam, VarHybm
  REAL(dp), INTENT(in), DIMENSION(nhyi)                 :: VarHyai, VarHybi
  REAL(dp), INTENT(in), DIMENSION(nLo,nLa,nT)           :: VarSP, VarZ
  REAL(dp), INTENT(in), DIMENSION(nLo,nLa,nLe,nT) 	:: geop
  
  REAL(dp), INTENT(in), DIMENSION(nLo,nLa,nLe,nT) 	:: VarNC,VarNC_SSs,VarNC_SSm,VarNC_SSl
  REAL(dp), INTENT(in), DIMENSION(nLo,nLa,nLe,nT) 	:: VarNC_DUs,VarNC_DUm,VarNC_DUl
  REAL(dp), INTENT(in), DIMENSION(nLo,nLa,nLe,nT) 	:: VarNC_OMn,VarNC_OMh,VarNC_BCn
  REAL(dp), INTENT(in), DIMENSION(nLo,nLa,nLe,nT)       :: VarNC_BCh, VarNC_SU

 
  ! Local parameters
  INTEGER :: ncid3, status 
  INTEGER :: timeID, levID, latID, lonID, hymID, hyiID
  INTEGER :: var1, var2, var3, var4, var5, var6, var7, var8, var9
  INTEGER :: var500, var600  
  INTEGER :: var70, var71, var72, var73, var74, var75
  INTEGER :: var76, var77, var78, var79, var710, var711

  REAL, PARAMETER	:: FillAtt = -9999.99
  !REAL(dp), PARAMETER	:: FillAtt2 = -9999.99
  CHARACTER(len=31)	:: timestr = "hours since YYYY-MM-DD 00:00:00"      

  !--- Define the parameters
  timestr(13:16) = year
  timestr(18:19) = mon
  timestr(21:22) = day
 
   
   !------------------------------------------------------------------------
   !--- create output netCDF file
   status = nf90_create(path = ofile3, cmode = NF90_NETCDF4, ncid = ncid3)
    IF(status /= nf90_noerr) CALL handle_err(status)
    !WRITE(*,*) 'def nc'

   !--- Define the dimensions
   status = nf90_def_dim(ncid3, "time", nf90_unlimited, timeID)
    IF(status /= nf90_noerr) CALL handle_err(status)
   status = nf90_def_dim(ncid3, "lev", nLe, levID)
    IF(status /= nf90_noerr) CALL handle_err(status)   
   status = nf90_def_dim(ncid3, "lat", nLa, latID)
    IF(status /= nf90_noerr) CALL handle_err(status)
   status = nf90_def_dim(ncid3, "lon", nLo, lonID)
    IF(status /= nf90_noerr) CALL handle_err(status)
   status = nf90_def_dim(ncid3, "nhym", nhym, hymID)
    IF(status /= nf90_noerr) CALL handle_err(status) 
   status = nf90_def_dim(ncid3, "nhyi", nhyi, hyiID)
    IF(status /= nf90_noerr) CALL handle_err(status)       
    !WRITE(*,*) 'def dim'

   !--- Define global attributes
   status = nf90_put_att(ncid3, NF90_GLOBAL, "author", "Karoline Block")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid3, NF90_GLOBAL, "institution", "University of Leipzig")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid3, NF90_GLOBAL, "source data", &
    "European Centre for Medium-Range Weather Forecasts (ECMWF) Atmospheric Composition Reanalysis 4 (EAC4)")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid3, NF90_GLOBAL, "attribution", "Copernicus Atmosphere Monitoring Service (CAMS)")
    IF(status /= nf90_NoErr) CALL handle_err(status)

   ! Time   
   status = nf90_def_var(ncid3, "time", nf90_double,(/ timeID /), var1)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid3, var1, "long_name", "time")
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   status = nf90_put_att(ncid3, var1, "units", timestr)
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   status = nf90_put_att(ncid3, var1, "calendar", "proleptic_gregorian")
    IF(status /= nf90_NoErr) CALL handle_err(status)	       
   status = nf90_put_att(ncid3, var1, "axis", "T")
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   !status = nf90_put_att(ncid3, var1, "_FillValue", FillAtt2)
   ! IF(status /= nf90_NoErr) CALL handle_err(status)	   
   !WRITE(*,*) 'def time'

   ! Lon  
   status = nf90_def_var(ncid3, "lon", nf90_double,(/ lonID /), var2)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid3, var2, "long_name", "longitude")
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   status = nf90_put_att(ncid3, var2, "units", "degrees_east")
    IF(status /= nf90_NoErr) CALL handle_err(status)	    
   status = nf90_put_att(ncid3, var2, "axis", "X")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   !status = nf90_put_att(ncid3, var2, "_FillValue", FillAtt2)
   ! IF(status /= nf90_NoErr) CALL handle_err(status)	   
    !WRITE(*,*) 'def lon'

   ! Lat   
   status = nf90_def_var(ncid3, "lat", nf90_double,(/ latID /), var3)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid3, var3, "long_name", "latitude")
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   status = nf90_put_att(ncid3, var3, "units", "degrees_north")
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   status = nf90_put_att(ncid3, var3, "axis", "Y")
    IF(status /= nf90_NoErr) CALL handle_err(status)  
   !status = nf90_put_att(ncid3, var3, "_FillValue", FillAtt2)    
   ! IF(status /= nf90_NoErr) CALL handle_err(status)	   
    !WRITE(*,*) 'def lat'
   
   ! Lev 
   status = nf90_def_var(ncid3, "lev", nf90_double,(/ levID /), var4)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid3, var4, "long_name", "hybrid level at layer midpoints")
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   status = nf90_put_att(ncid3, var4, "units", "level")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid3, var4, "axis", "Z")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid3, var4, "positive", "down")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid3, var4, "formula", "mlev=hyam+hybm*aps")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid3, var4, "formula_terms", "ap: hyam b: hybm ps: aps")
    IF(status /= nf90_NoErr) CALL handle_err(status)        	    
   !status = nf90_put_att(ncid3, var4, "_FillValue", FillAtt2)
   ! IF(status /= nf90_NoErr) CALL handle_err(status)	   
    !WRITE(*,*) 'def lev'
   
   ! Hyam
   status = nf90_def_var(ncid3, "hyam", nf90_double,(/ hymID /), var5)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid3, var5, "long_name", "hybrid A coefficient at layer midpoints")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid3, var5, "units", "Pa")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   !status = nf90_put_att(ncid3, var5, "_FillValue", FillAtt2)
   ! IF(status /= nf90_NoErr) CALL handle_err(status)
   !WRITE(*,*) 'def hyam'

   ! Hybm
   status = nf90_def_var(ncid3, "hybm", nf90_double,(/ hymID /), var6)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid3, var6, "long_name", "hybrid B coefficient at layer midpoints")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid3, var6, "units", "1")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   !status = nf90_put_att(ncid3, var6, "_FillValue", FillAtt2)
   ! IF(status /= nf90_NoErr) CALL handle_err(status)
   !WRITE(*,*) 'def hybm'

   ! Hyai
   status = nf90_def_var(ncid3, "hyai", nf90_double,(/ hyiID /), var500)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid3, var500, "long_name", "hybrid A coefficient at layer interfaces")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid3, var500, "units", "Pa")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   !status = nf90_put_att(ncid3, var500, "_FillValue", FillAtt2)
   ! IF(status /= nf90_NoErr) CALL handle_err(status)
   !WRITE(*,*) 'def hyai'

   ! Hybi
   status = nf90_def_var(ncid3, "hybi", nf90_double,(/ hyiID /), var600)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid3, var600, "long_name", "hybrid B coefficient at layer interfaces")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid3, var600, "units", "1")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   !status = nf90_put_att(ncid3, var600, "_FillValue", FillAtt2)
   ! IF(status /= nf90_NoErr) CALL handle_err(status)
   !WRITE(*,*) 'def hybi'
   
   ! SP
   status = nf90_def_var(ncid3, "sp", nf90_float,(/ lonID, latID, timeID /), var7)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid3, var7, "long_name", "surface pressure")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid3, var7, "units", "Pa")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid3, var7, "code", "134")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid3, var7, "table", "128")
    IF(status /= nf90_NoErr) CALL handle_err(status)   
   status = nf90_put_att(ncid3, var7, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   !WRITE(*,*) 'def SP' 

   ! Z
   status = nf90_def_var(ncid3, "z", nf90_float,(/ lonID, latID, timeID /), var8)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid3, var8, "long_name", "surface geopotential")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid3, var8, "units", "m**2 s**-2")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid3, var8, "code", "129")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid3, var8, "table", "128")
    IF(status /= nf90_NoErr) CALL handle_err(status)   
   status = nf90_put_att(ncid3, var8, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   !WRITE(*,*) 'def z' 

   ! Geopotential height
   status = nf90_def_var(ncid3, "gh", nf90_float,(/ lonID, latID, levID, timeID /), var9)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid3, var9, "long_name", "geopotential height")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid3, var9, "units", "gpm")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid3, var9, "code", "156")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid3, var9, "table", "128")            
    IF(status /= nf90_NoErr) CALL handle_err(status)   
   status = nf90_put_att(ncid3, var9, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'def gh'
    
    
   status = nf90_def_var(ncid3, "nconc", nf90_float,(/ lonID, latID, levID, timeID /), var70)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid3, var70, "long_name", "total aerosol number concentration")
    IF(status /= nf90_NoErr) CALL handle_err(status)	  
   status = nf90_put_att(ncid3, var70, "units", "m**-3")
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   status = nf90_put_att(ncid3, var70, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)	  
 
   status = nf90_def_var(ncid3, "nconc_SSs", nf90_float,(/ lonID, latID, levID, timeID /), var71)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid3, var71, "long_name", "number concentration of small mode sea salt")
    IF(status /= nf90_NoErr) CALL handle_err(status)	  
   status = nf90_put_att(ncid3, var71, "units", "m**-3")
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   status = nf90_put_att(ncid3, var71, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)	  

   status = nf90_def_var(ncid3, "nconc_SSm", nf90_float,(/ lonID, latID, levID, timeID /), var72)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid3, var72, "long_name", "number concentration of medium mode sea salt")
    IF(status /= nf90_NoErr) CALL handle_err(status)	  
   status = nf90_put_att(ncid3, var72, "units", "m**-3")
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   status = nf90_put_att(ncid3, var72, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)	  

   status = nf90_def_var(ncid3, "nconc_SSl", nf90_float,(/ lonID, latID, levID, timeID /), var73)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid3, var73, "long_name", "number concentration of large mode sea salt")
    IF(status /= nf90_NoErr) CALL handle_err(status)	  
   status = nf90_put_att(ncid3, var73, "units", "m**-3")
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   status = nf90_put_att(ncid3, var73, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)	  

   status = nf90_def_var(ncid3, "nconc_DUs", nf90_float,(/ lonID, latID, levID, timeID /), var74)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid3, var74, "long_name", "number concentration of small mode dust")
    IF(status /= nf90_NoErr) CALL handle_err(status)	  
   status = nf90_put_att(ncid3, var74, "units", "m**-3")
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   status = nf90_put_att(ncid3, var74, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)	  

   status = nf90_def_var(ncid3, "nconc_DUm", nf90_float,(/ lonID, latID, levID, timeID /), var75)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid3, var75, "long_name", "number concentration of medium mode dust")
    IF(status /= nf90_NoErr) CALL handle_err(status)	  
   status = nf90_put_att(ncid3, var75, "units", "m**-3")
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   status = nf90_put_att(ncid3, var75, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)	  

   status = nf90_def_var(ncid3, "nconc_DUl", nf90_float,(/ lonID, latID, levID, timeID /), var76)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid3, var76, "long_name", "number concentration of large mode dust")
    IF(status /= nf90_NoErr) CALL handle_err(status)	  
   status = nf90_put_att(ncid3, var76, "units", "m**-3")
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   status = nf90_put_att(ncid3, var76, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)	  

   status = nf90_def_var(ncid3, "nconc_OMh", nf90_float,(/ lonID, latID, levID, timeID /), var77)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid3, var77, "long_name", &
   "number concentration of hydrophilic organic matter")
    IF(status /= nf90_NoErr) CALL handle_err(status)	  
   status = nf90_put_att(ncid3, var77, "units", "m**-3")
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   status = nf90_put_att(ncid3, var77, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)	  

   status = nf90_def_var(ncid3, "nconc_OMn", nf90_float,(/ lonID, latID, levID, timeID /), var78)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid3, var78, "long_name", &
   "number concentration of non-hydrophilic organic matter")
    IF(status /= nf90_NoErr) CALL handle_err(status)	  
   status = nf90_put_att(ncid3, var78, "units", "m**-3")
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   status = nf90_put_att(ncid3, var78, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)	  

   status = nf90_def_var(ncid3, "nconc_BCh", nf90_float,(/ lonID, latID, levID, timeID /), var79)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid3, var79, "long_name", &
   "number concentration of hydrophilic black carbon")
    IF(status /= nf90_NoErr) CALL handle_err(status)	  
   status = nf90_put_att(ncid3, var79, "units", "m**-3")
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   status = nf90_put_att(ncid3, var79, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)

   status = nf90_def_var(ncid3, "nconc_BCn", nf90_float,(/ lonID, latID, levID, timeID /), var710)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid3, var710, "long_name", &
   "number concentration of non-hydrophilic black carbon")
    IF(status /= nf90_NoErr) CALL handle_err(status)	  
   status = nf90_put_att(ncid3, var710, "units", "m**-3")
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   status = nf90_put_att(ncid3, var710, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)	  

   status = nf90_def_var(ncid3, "nconc_SO4", nf90_float,(/ lonID, latID, levID, timeID /), var711)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid3, var711, "long_name", "number concentration of sulfate")
    IF(status /= nf90_NoErr) CALL handle_err(status)	  
   status = nf90_put_att(ncid3, var711, "units", "m**-3")
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   status = nf90_put_att(ncid3, var711, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)

   !--- Leave define mode
   status = nf90_enddef(ncid3)
    IF(status /= nf90_NoErr) CALL handle_err(status)  

   !--- write data
   status = nf90_put_var(ncid3, var1, VarTime)	        ! Time  
    IF(status /= nf90_NoErr) CALL handle_err(status) 
    !WRITE(*,*) 'time'
   status = nf90_put_var(ncid3, var2, VarLon)	        ! Lon  
    IF(status /= nf90_NoErr) CALL handle_err(status) 
    !WRITE(*,*) 'lon'
   status = nf90_put_var(ncid3, var3, VarLat)	        ! Lat  
    IF(status /= nf90_NoErr) CALL handle_err(status) 
    !WRITE(*,*) 'lat'
   status = nf90_put_var(ncid3, var4, VarLev)	        ! Lev  
   IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'lev'
   status = nf90_put_var(ncid3, var5, VarHyam)          ! hyam
   IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'hyam'
   status = nf90_put_var(ncid3, var6, VarHybm)          ! hybm
   IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'hybm'
   status = nf90_put_var(ncid3, var500, VarHyai)        ! hyai
   IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'hyai'
   status = nf90_put_var(ncid3, var600, VarHybi)        ! hybi
   IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'hybi'        
   status = nf90_put_var(ncid3, var7, VarSP)            ! SP
   IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'sp'
   status = nf90_put_var(ncid3, var8, VarZ)             ! Z
   IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'z'
   status = nf90_put_var(ncid3, var9, REAL(geop))	! GH
    IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'gh'


   status = nf90_put_var(ncid3, var70, REAL(VarNC))
    IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'nconc'
   status = nf90_put_var(ncid3, var71, REAL(VarNC_SSs))
    IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'nconc_SSs'
   status = nf90_put_var(ncid3, var72, REAL(VarNC_SSm))
    IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'nconc_SSm'
   status = nf90_put_var(ncid3, var73, REAL(VarNC_SSl))
    IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'nconc_SSl'   
   status = nf90_put_var(ncid3, var74, REAL(VarNC_DUs))
    IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'nconc_DUs'
   status = nf90_put_var(ncid3, var75, REAL(VarNC_DUm))
    IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'nconc_DUm'
   status = nf90_put_var(ncid3, var76, REAL(VarNC_DUl))
    IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'nconc_DUl'           
   status = nf90_put_var(ncid3, var77, REAL(VarNC_OMh))
    IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'nconc_OMh'    
   status = nf90_put_var(ncid3, var78, REAL(VarNC_OMn))
    IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'nconc_OMn'           
   status = nf90_put_var(ncid3, var79, REAL(VarNC_BCh))
    IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'nconc_BCh'
   status = nf90_put_var(ncid3, var710, REAL(VarNC_BCn))
    IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'nconc_BCn'
   status = nf90_put_var(ncid3, var711, REAL(VarNC_SU))
    IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'nconc_SU'
  
   !--- close output netCDF file
   status = nf90_close(ncid3)
    IF(status /= nf90_NoErr) CALL handle_err(status)   
 
  END SUBROUTINE netcdf_output_nconc

  !WRITE(*,*) ' '  
  !WRITE(*,*) 'FINISHED OUTPUT NC'



!--------------------------------------------------------------------------------------------------
  SUBROUTINE  netcdf_output_mconc(ofile4, year, mon, day, &
  			 	nT, nLe, nLa, nLo, nhym, nhyi, &
 			 	VarTime, VarLev, VarLat, VarLon, VarHyam, VarHybm, VarHyai, VarHybi, &
                         	VarSP, VarZ, geop, VarMC,VarMC_SSs,VarMC_SSm,VarMC_SSl, &
				VarMC_DUs,VarMC_DUm,VarMC_DUl, &
				VarMC_OMn,VarMC_OMh,VarMC_BCn, &
				VarMC_BCh, VarMC_SU) 



  USE mo_kind,          ONLY: dp

  IMPLICIT NONE

  ! Input parameters
  CHARACTER(len=*), INTENT(in)       			:: ofile4
  CHARACTER(len=4), INTENT(in)       			:: year
  CHARACTER(len=2), INTENT(in)       			:: mon
  CHARACTER(len=2), INTENT(in)                          :: day
  INTEGER,  INTENT(in)         				:: nT, nLe, nLa, nLo, nhym, nhyi  

  REAL(dp), INTENT(in), DIMENSION(nT)			:: VarTime
  REAL(dp), INTENT(in), DIMENSION(nLe)			:: VarLev
  REAL(dp), INTENT(in), DIMENSION(nLa)			:: VarLat
  REAL(dp), INTENT(in), DIMENSION(nLo)			:: VarLon
  REAL(dp), INTENT(in), DIMENSION(nhym)                 :: VarHyam, VarHybm
  REAL(dp), INTENT(in), DIMENSION(nhyi)                 :: VarHyai, VarHybi
  REAL(dp), INTENT(in), DIMENSION(nLo,nLa,nT)           :: VarSP, VarZ
  REAL(dp), INTENT(in), DIMENSION(nLo,nLa,nLe,nT) 	:: geop

  REAL(dp), INTENT(in), DIMENSION(nLo,nLa,nLe,nT) 	:: VarMC,VarMC_SSs,VarMC_SSm,VarMC_SSl
  REAL(dp), INTENT(in), DIMENSION(nLo,nLa,nLe,nT) 	:: VarMC_DUs,VarMC_DUm,VarMC_DUl
  REAL(dp), INTENT(in), DIMENSION(nLo,nLa,nLe,nT) 	:: VarMC_OMn,VarMC_OMh,VarMC_BCn
  REAL(dp), INTENT(in), DIMENSION(nLo,nLa,nLe,nT)       :: VarMC_BCh, VarMC_SU

 
  ! Local parameters
  INTEGER :: ncid4, status 
  INTEGER :: timeID, levID, latID, lonID, hymID, hyiID
  INTEGER :: var1, var2, var3, var4, var5, var6, var7, var8, var9
  INTEGER :: var500, var600
  INTEGER :: var80, var81, var82, var83, var84, var85
  INTEGER :: var86, var87, var88, var89, var810, var811

  REAL, PARAMETER	:: FillAtt = -9999.99
  !REAL(dp), PARAMETER	:: FillAtt2 = -9999.99
  CHARACTER(len=31)	:: timestr = "hours since YYYY-MM-DD 00:00:00"      

  !--- Define the parameters
  timestr(13:16) = year
  timestr(18:19) = mon
  timestr(21:22) = day
 
   
   !------------------------------------------------------------------------
   !--- create output netCDF file
   status = nf90_create(path = ofile4, cmode = NF90_NETCDF4, ncid = ncid4)
    IF(status /= nf90_noerr) CALL handle_err(status)
    !WRITE(*,*) 'def nc'

   !--- Define the dimensions
   status = nf90_def_dim(ncid4, "time", nf90_unlimited, timeID)
    IF(status /= nf90_noerr) CALL handle_err(status)
   status = nf90_def_dim(ncid4, "lev", nLe, levID)
    IF(status /= nf90_noerr) CALL handle_err(status)   
   status = nf90_def_dim(ncid4, "lat", nLa, latID)
    IF(status /= nf90_noerr) CALL handle_err(status)
   status = nf90_def_dim(ncid4, "lon", nLo, lonID)
    IF(status /= nf90_noerr) CALL handle_err(status)
   status = nf90_def_dim(ncid4, "nhym", nhym, hymID)
    IF(status /= nf90_noerr) CALL handle_err(status)    
   status = nf90_def_dim(ncid4, "nhyi", nhyi, hyiID)
    IF(status /= nf90_noerr) CALL handle_err(status)    
    !WRITE(*,*) 'def dim'

   !--- Define global attributes
   status = nf90_put_att(ncid4, NF90_GLOBAL, "author", "Karoline Block")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid4, NF90_GLOBAL, "institution", "University of Leipzig")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid4, NF90_GLOBAL, "source data", &
    "European Centre for Medium-Range Weather Forecasts (ECMWF) Atmospheric Composition Reanalysis 4 (EAC4)")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid4, NF90_GLOBAL, "attribution", "Copernicus Atmosphere Monitoring Service (CAMS)")
    IF(status /= nf90_NoErr) CALL handle_err(status)

   ! Time   
   status = nf90_def_var(ncid4, "time", nf90_double,(/ timeID /), var1)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid4, var1, "long_name", "time")
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   status = nf90_put_att(ncid4, var1, "units", timestr)
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   status = nf90_put_att(ncid4, var1, "calendar", "proleptic_gregorian")
    IF(status /= nf90_NoErr) CALL handle_err(status)	       
   status = nf90_put_att(ncid4, var1, "axis", "T")
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   !status = nf90_put_att(ncid4, var1, "_FillValue", FillAtt2)
   ! IF(status /= nf90_NoErr) CALL handle_err(status)	   
   !WRITE(*,*) 'def time'

   ! Lon  
   status = nf90_def_var(ncid4, "lon", nf90_double,(/ lonID /), var2)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid4, var2, "long_name", "longitude")
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   status = nf90_put_att(ncid4, var2, "units", "degrees_east")
    IF(status /= nf90_NoErr) CALL handle_err(status)	    
   status = nf90_put_att(ncid4, var2, "axis", "X")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   !status = nf90_put_att(ncid4, var2, "_FillValue", FillAtt2)
   ! IF(status /= nf90_NoErr) CALL handle_err(status)	   
    !WRITE(*,*) 'def lon'

   ! Lat   
   status = nf90_def_var(ncid4, "lat", nf90_double,(/ latID /), var3)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid4, var3, "long_name", "latitude")
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   status = nf90_put_att(ncid4, var3, "units", "degrees_north")
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   status = nf90_put_att(ncid4, var3, "axis", "Y")
    IF(status /= nf90_NoErr) CALL handle_err(status)  
   !status = nf90_put_att(ncid4, var3, "_FillValue", FillAtt2)    
   ! IF(status /= nf90_NoErr) CALL handle_err(status)	   
    !WRITE(*,*) 'def lat'
   
   ! Lev 
   status = nf90_def_var(ncid4, "lev", nf90_double,(/ levID /), var4)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid4, var4, "long_name", "hybrid level at layer midpoints")
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   status = nf90_put_att(ncid4, var4, "units", "level")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid4, var4, "axis", "Z")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid4, var4, "positive", "down")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid4, var4, "formula", "mlev=hyam+hybm*aps")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid4, var4, "formula_terms", "ap: hyam b: hybm ps: aps")
    IF(status /= nf90_NoErr) CALL handle_err(status)        	    
   !status = nf90_put_att(ncid4, var4, "_FillValue", FillAtt2)
   ! IF(status /= nf90_NoErr) CALL handle_err(status)	   
    !WRITE(*,*) 'def lev'
   
   ! Hyam
   status = nf90_def_var(ncid4, "hyam", nf90_double,(/ hymID /), var5)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid4, var5, "long_name", "hybrid A coefficient at layer midpoints")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid4, var5, "units", "Pa")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   !status = nf90_put_att(ncid4, var5, "_FillValue", FillAtt2)
   ! IF(status /= nf90_NoErr) CALL handle_err(status)
   !WRITE(*,*) 'def hyam'

   ! Hybm
   status = nf90_def_var(ncid4, "hybm", nf90_double,(/ hymID /), var6)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid4, var6, "long_name", "hybrid B coefficient at layer midpoints")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid4, var6, "units", "1")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   !status = nf90_put_att(ncid4, var6, "_FillValue", FillAtt2)
   ! IF(status /= nf90_NoErr) CALL handle_err(status)
   !WRITE(*,*) 'def hybm'

   ! Hyai
   status = nf90_def_var(ncid4, "hyai", nf90_double,(/ hyiID /), var500)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid4, var500, "long_name", "hybrid A coefficient at layer interfaces")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid4, var500, "units", "Pa")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   !status = nf90_put_att(ncid4, var500, "_FillValue", FillAtt2)
   ! IF(status /= nf90_NoErr) CALL handle_err(status)
   !WRITE(*,*) 'def hyai'

   ! Hybi
   status = nf90_def_var(ncid4, "hybi", nf90_double,(/ hyiID /), var600)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid4, var600, "long_name", "hybrid B coefficient at layer interfaces")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid4, var600, "units", "1")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   !status = nf90_put_att(ncid4, var600, "_FillValue", FillAtt2)
   ! IF(status /= nf90_NoErr) CALL handle_err(status)
   !WRITE(*,*) 'def hybi'

   ! SP
   status = nf90_def_var(ncid4, "sp", nf90_float,(/ lonID, latID, timeID /), var7)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid4, var7, "long_name", "surface pressure")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid4, var7, "units", "Pa")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid4, var7, "code", "134")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid4, var7, "table", "128")
    IF(status /= nf90_NoErr) CALL handle_err(status)   
   status = nf90_put_att(ncid4, var7, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   !WRITE(*,*) 'def SP' 

   ! Z
   status = nf90_def_var(ncid4, "z", nf90_float,(/ lonID, latID, timeID /), var8)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid4, var8, "long_name", "surface geopotential")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid4, var8, "units", "m**2 s**-2")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid4, var8, "code", "129")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid4, var8, "table", "128")
    IF(status /= nf90_NoErr) CALL handle_err(status)   
   status = nf90_put_att(ncid4, var8, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   !WRITE(*,*) 'def Z' 
 
   ! Geopotential height
   status = nf90_def_var(ncid4, "gh", nf90_float,(/ lonID, latID, levID, timeID /), var9)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid4, var9, "long_name", "geopotential height")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid4, var9, "units", "gpm")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid4, var9, "code", "156")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid4, var9, "table", "128")            
    IF(status /= nf90_NoErr) CALL handle_err(status)   
   status = nf90_put_att(ncid4, var9, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'def geop'
    
    
   status = nf90_def_var(ncid4, "mconc", nf90_float,(/ lonID, latID, levID, timeID /), var80)
    IF(status /= nf90_NoErr) CALL handle_err(status) 
   status = nf90_put_att(ncid4, var80, "long_name", "total aerosol mass concentration")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid4, var80, "units", "kg m**-3")
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid4, var80, "_FillValue", FillAtt) 
    IF(status /= nf90_NoErr) CALL handle_err(status)
  
   status = nf90_def_var(ncid4, "mconc_SSs", nf90_float,(/ lonID, latID, levID, timeID /), var81)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid4, var81, "long_name", "mass concentration of small mode sea salt")
    IF(status /= nf90_NoErr) CALL handle_err(status)	  
   status = nf90_put_att(ncid4, var81, "units", "kg m**-3")
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   status = nf90_put_att(ncid4, var81, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)	  

   status = nf90_def_var(ncid4, "mconc_SSm", nf90_float,(/ lonID, latID, levID, timeID /), var82)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid4, var82, "long_name", "mass concentration of medium mode sea salt")
    IF(status /= nf90_NoErr) CALL handle_err(status)	  
   status = nf90_put_att(ncid4, var82, "units", "kg m**-3")
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   status = nf90_put_att(ncid4, var82, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)	  

   status = nf90_def_var(ncid4, "mconc_SSl", nf90_float,(/ lonID, latID, levID, timeID /), var83)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid4, var83, "long_name", "mass concentration of large mode sea salt")
    IF(status /= nf90_NoErr) CALL handle_err(status)	  
   status = nf90_put_att(ncid4, var83, "units", "kg m**-3")
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   status = nf90_put_att(ncid4, var83, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)	  

   status = nf90_def_var(ncid4, "mconc_DUs", nf90_float,(/ lonID, latID, levID, timeID /), var84)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid4, var84, "long_name", "mass concentration of small mode dust")
    IF(status /= nf90_NoErr) CALL handle_err(status)	  
   status = nf90_put_att(ncid4, var84, "units", "kg m**-3")
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   status = nf90_put_att(ncid4, var84, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)	  

   status = nf90_def_var(ncid4, "mconc_DUm", nf90_float,(/ lonID, latID, levID, timeID /), var85)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid4, var85, "long_name", "mass concentration of medium mode dust")
    IF(status /= nf90_NoErr) CALL handle_err(status)	  
   status = nf90_put_att(ncid4, var85, "units", "kg m**-3")
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   status = nf90_put_att(ncid4, var85, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)	  

   status = nf90_def_var(ncid4, "mconc_DUl", nf90_float,(/ lonID, latID, levID, timeID /), var86)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid4, var86, "long_name", "mass concentration of large mode dust")
    IF(status /= nf90_NoErr) CALL handle_err(status)	  
   status = nf90_put_att(ncid4, var86, "units", "kg m**-3")
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   status = nf90_put_att(ncid4, var86, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)	  

   status = nf90_def_var(ncid4, "mconc_OMh", nf90_float,(/ lonID, latID, levID, timeID /), var87)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid4, var87, "long_name", "mass concentration of hydrophilic organic matter")
    IF(status /= nf90_NoErr) CALL handle_err(status)	  
   status = nf90_put_att(ncid4, var87, "units", "kg m**-3")
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   status = nf90_put_att(ncid4, var87, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)	  

   status = nf90_def_var(ncid4, "mconc_OMn", nf90_float,(/ lonID, latID, levID, timeID /), var88)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid4, var88, "long_name", "mass concentration of non-hydrophilic organic matter")
    IF(status /= nf90_NoErr) CALL handle_err(status)	  
   status = nf90_put_att(ncid4, var88, "units", "kg m**-3")
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   status = nf90_put_att(ncid4, var88, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)	  

   status = nf90_def_var(ncid4, "mconc_BCh", nf90_float,(/ lonID, latID, levID, timeID /), var89)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid4, var89, "long_name", "mass concentration of hydrophilic black carbon")
    IF(status /= nf90_NoErr) CALL handle_err(status)	  
   status = nf90_put_att(ncid4, var89, "units", "kg m**-3")
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   status = nf90_put_att(ncid4, var89, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)

   status = nf90_def_var(ncid4, "mconc_BCn", nf90_float,(/ lonID, latID, levID, timeID /), var810)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid4, var810, "long_name", "mass concentration of non-hydrophilic black carbon")
    IF(status /= nf90_NoErr) CALL handle_err(status)	  
   status = nf90_put_att(ncid4, var810, "units", "kg m**-3")
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   status = nf90_put_att(ncid4, var810, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)	  

   status = nf90_def_var(ncid4, "mconc_SO4", nf90_float,(/ lonID, latID, levID, timeID /), var811)
    IF(status /= nf90_NoErr) CALL handle_err(status)
   status = nf90_put_att(ncid4, var811, "long_name", "mass concentration of sulfate")
    IF(status /= nf90_NoErr) CALL handle_err(status)	  
   status = nf90_put_att(ncid4, var811, "units", "kg m**-3")
    IF(status /= nf90_NoErr) CALL handle_err(status)	   
   status = nf90_put_att(ncid4, var811, "_FillValue", FillAtt)
    IF(status /= nf90_NoErr) CALL handle_err(status)

   !--- Leave define mode
   status = nf90_enddef(ncid4)
    IF(status /= nf90_NoErr) CALL handle_err(status)  

   !--- write data
   status = nf90_put_var(ncid4, var1, VarTime)	        ! Time  
    IF(status /= nf90_NoErr) CALL handle_err(status) 
    !WRITE(*,*) 'time'
   status = nf90_put_var(ncid4, var2, VarLon)	        ! Lon  
    IF(status /= nf90_NoErr) CALL handle_err(status) 
    !WRITE(*,*) 'lon'
   status = nf90_put_var(ncid4, var3, VarLat)	        ! Lat  
    IF(status /= nf90_NoErr) CALL handle_err(status) 
    !WRITE(*,*) 'lat'
   status = nf90_put_var(ncid4, var4, VarLev)	        ! Lev  
   IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'lev'
   status = nf90_put_var(ncid4, var5, VarHyam)          ! hyam
   IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'hyam'
   status = nf90_put_var(ncid4, var6, VarHybm)          ! hybm
   IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'hybm'
   status = nf90_put_var(ncid4, var500, VarHyai)        ! hyai
   IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'hyai'
   status = nf90_put_var(ncid4, var600, VarHybi)        ! hybi
   IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'hybi'        
   status = nf90_put_var(ncid4, var7, VarSP)            ! SP
   IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'sp'
   status = nf90_put_var(ncid4, var8, VarZ)             ! Z
   IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'z'
   status = nf90_put_var(ncid4, var9, REAL(geop))	! GH
    IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'gh'


   status = nf90_put_var(ncid4, var80, REAL(VarMC))
    IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'mconc'
   status = nf90_put_var(ncid4, var81, REAL(VarMC_SSs))
    IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'mconc_SSs'
   status = nf90_put_var(ncid4, var82, REAL(VarMC_SSm))
    IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'mconc_SSm'
   status = nf90_put_var(ncid4, var83, REAL(VarMC_SSl))
    IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'mconc_SSl'   
   status = nf90_put_var(ncid4, var84, REAL(VarMC_DUs))
    IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'mconc_DUs'
   status = nf90_put_var(ncid4, var85, REAL(VarMC_DUm))
    IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'mconc_DUm'
   status = nf90_put_var(ncid4, var86, REAL(VarMC_DUl))
    IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'mconc_DUl'           
   status = nf90_put_var(ncid4, var87, REAL(VarMC_OMh))
    IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'mconc_OMh'    
   status = nf90_put_var(ncid4, var88, REAL(VarMC_OMn))
    IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'mconc_OMn'           
   status = nf90_put_var(ncid4, var89, REAL(VarMC_BCh))
    IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'mconc_BCh'
   status = nf90_put_var(ncid4, var810, REAL(VarMC_BCn))
    IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'mconc_BCn'
   status = nf90_put_var(ncid4, var811, REAL(VarMC_SU))
    IF(status /= nf90_NoErr) CALL handle_err(status)
    !WRITE(*,*) 'mconc_SU'
  
   !--- close output netCDF file
   status = nf90_close(ncid4)
    IF(status /= nf90_NoErr) CALL handle_err(status)   
 
  END SUBROUTINE netcdf_output_mconc

  !WRITE(*,*) ' '  
  !WRITE(*,*) 'FINISHED OUTPUT MC'





  !--- ERROR check:
  SUBROUTINE handle_err(status)
       INTEGER, INTENT (in) :: status
    
       IF(status /= nf90_noerr) then
	 PRINT *, trim(nf90_strerror(status))
	 STOP "Stopped"
       END IF
  END SUBROUTINE handle_err	 
                  
		  
		   
END MODULE mo_box_netcdf_io



