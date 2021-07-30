RECURSIVE Subroutine CEMAOutputRoutines
    Use MAIN 
    Use GLOBAL
    Use SCREENC
    Use GEOMC
    Use KINETIC
    USE NAMESC
    Use CEMAVars
    
    ! Type declarations
    IMPLICIT NONE
    
    Character(3) CrackStatus(IMX)
    Integer nGas, GenBODNum
    SAVE
    RETURN
    
    ENTRY WriteCEMASedimentModelOutput
    
        !Bed Elevation
        If(WriteBESnp)Then
            Write(CEMASNPOutFilN,'("Bed elevation(m)")')
            Write(CEMASNPOutFilN,'("JDAY = ",f14.6)')JDAY
            Write(CEMASNPOutFilN,'(<IMX>i10)')(SegNumI, SegNumI = 1, IMX)
            Write(CEMASNPOutFilN,'(<IMX>f10.4)')(BedElevation(SegNumI), SegNumI = 1, IMX)
            
            Write(CEMASNPOutFilN,'("Bed elevation layer(m)")')
            Write(CEMASNPOutFilN,'("JDAY = ",f14.6)')JDAY
            Write(CEMASNPOutFilN,'(<IMX>i10)')(SegNumI, SegNumI = 1, IMX)
            Write(CEMASNPOutFilN,'(<IMX>f10.4)')(BedElevationLayer(SegNumI), SegNumI = 1, IMX)
            
            !Sediment concentration near bottom
            !Write(CEMASNPOutFilN,'("Sediment Concentration (gm/m³)")')
            !Write(CEMASNPOutFilN,'("JDAY = ",f8.2)')JDAY
            !Write(CEMASNPOutFilN,'(<IMX>i10)')(SegNumI, SegNumI = 1, IMX)
            !Write(CEMASNPOutFilN,'(<IMX>f10.3)')(CEMASedConc(SegNumI,KB(SegNumI)), SegNumI = 1, IMX) 
            !Write(CEMASNPOutFilN,'("Density (gm/m³)")')
            !Write(CEMASNPOutFilN,'("JDAY = ",f8.2)')JDAY
            !Write(CEMASNPOutFilN,'(<IMX>i10)')(SegNumI, SegNumI = 1, IMX)
            !Write(CEMASNPOutFilN,'(<IMX>f10.3)')(rho(KB(SegNumI),SegNumI), SegNumI = 1, IMX)   
            !Write(CEMASNPOutFilN,'("Density (gm/m³)")')
            !Write(CEMASNPOutFilN,'("JDAY = ",f8.2)')JDAY
            !Write(CEMASNPOutFilN,'(<IMX>i10)')(SegNumI, SegNumI = 1, IMX)
            !Write(CEMASNPOutFilN,'(<IMX>f10.3)')(rho(KB(SegNumI)-1,SegNumI), SegNumI = 1, IMX)   
        End If
        
        !Bed porosity
        If(WritePWSnp)Then
            Write(CEMASNPOutFilN,'("Bed porosity (%)")')
            Write(CEMASNPOutFilN,'("JDAY = ",f8.2)')JDAY
            Write(CEMASNPOutFilN,'(<IMX>i10)')(SegNumI, SegNumI = 1, IMX)
            Write(CEMASNPOutFilN,'(<IMX>f10.4)')(BedPorosity(SegNumI)*100, SegNumI = 1, IMX)
            Write(CEMASNPOutFilN,'("Total Volume of Sediments = ",e14.6," m3")')TotalSedimentsInBed
            Write(CEMASNPOutFilN,'("Total Porewater Volume = ",e14.6," m3")')TotalPoreWatVolume
            Write(CEMASNPOutFilN,'("Total Porewater Removed = ",e14.6," m3")')TotalPoreWatRemoved
!            Write(CEMATSR1OutFilN,'(f14.6 , "," ,i5,",",i5, ",", f14.6, ",", f14.6, ",", f14.6, ",", f14.6, ",", f14.6, ",", f14.6, ",", f14.6, ",", f14.6)')JDAY, KTWB(1), KB(4), z(4), BedElevation(4), BedElevationLayer(4), PorewaterRelRate(4)*86400.d0, BedPorosity(4)*100, BedConsolidRate(4)*86400.d0, EL(KTWB(1),4)-Z(4), CEMACumPWRelease(4)
        End If
        
        Write(CEMABtmLayFilN,'("Bottom Layer (KB)")')
        Write(CEMABtmLayFilN,'("JDAY = ",f8.2)')JDAY
        TempCntr1 = TempCntr1 + 1
        If(TempCntr1 > 10)Then
            TempCntr1 = 0
            Write(CEMABtmLayFilN,'(a)')"________________________________________________________________________________"
            Write(CEMABtmLayFilN,'(<IMX>i5)')(SegNumI, SegNumI = 1, IMX)
        End If
        Write(CEMABtmLayFilN,'(<IMX>i5)')(KB(SegNumI), SegNumI = 1, IMX)  
        
    Return
    
    ENTRY WriteCEMASedimentFluxOutput
        
        If(WriteCEMAMFTSedFlx)Then
            ! testing start
           ! write(4478,'(f8.3,(1000f10.3)))')jday,(MFTSedFlxVars(SegNumI,26), SegNumI = 2, IMX-1), (MFTSedFlxVars(SegNumI,27), SegNumI = 2, IMX-1),  &
           !   (MFTSedFlxVars(SegNumI,28), SegNumI = 2, IMX-1),(MFTSedFlxVars(SegNumI,29), SegNumI = 2, IMX-1),  &
           ! (MFTSedFlxVars(SegNumI,30), SegNumI = 2, IMX-1),(MFTSedFlxVars(SegNumI,25), SegNumI = 2, IMX-1),(MFTSedFlxVars(SegNumI,31),MFTSedFlxVars(SegNumI,32), SegNumI = 2, IMX-1),  &
           ! (MFTSedFlxVars(SegNumI,33),MFTSedFlxVars(SegNumI,34), SegNumI = 2, IMX-1),(MFTSedFlxVars(SegNumI,10),MFTSedFlxVars(SegNumI,11), SegNumI = 2, IMX-1),  &
           ! (MFTSedFlxVars(SegNumI,8),MFTSedFlxVars(SegNumI,9), SegNumI = 2, IMX-1),(MFTSedFlxVars(SegNumI,35),MFTSedFlxVars(SegNumI,36), SegNumI = 2, IMX-1)
            
           ! write(4479,'(f8.3,(1000f10.3)))')jday,(MFTSedFlxVars(SegNumI,37),MFTSedFlxVars(SegNumI,38), SegNumI = 2, IMX-1),(MFTSedFlxVars(SegNumI,39),MFTSedFlxVars(SegNumI,40), SegNumI = 2, IMX-1), &
           !   (MFTSedFlxVars(SegNumI,41),MFTSedFlxVars(SegNumI,42), SegNumI = 2, IMX-1),(MFTSedFlxVars(SegNumI,43),MFTSedFlxVars(SegNumI,44), SegNumI = 2, IMX-1)
           ! write(4480,'(f8.3,(1000f10.3)))')jday,(MFTSedFlxVars(SegNumI,45),MFTSedFlxVars(SegNumI,46), SegNumI = 2, IMX-1)
            
            Write(CEMASedFlxFilN2,'("JDAY = ",f8.2)')JDAY
            Write(CEMASedFlxFilN2,'(<IMX>i10)')(SegNumI, SegNumI = 1, IMX)
            Write(CEMASedFlxFilN2,'("SOD (gO2/m2/d)")')
            Write(CEMASedFlxFilN2,'(<IMX>f10.4)')(MFTSedFlxVars(SegNumI,26), SegNumI = 1, IMX)
            
            Write(CEMASedFlxFilN2,'("JDAY = ",f8.2)')JDAY
            Write(CEMASedFlxFilN2,'(<IMX>i10)')(SegNumI, SegNumI = 1, IMX)
            Write(CEMASedFlxFilN2,'("CSOD (gO2/m2/d)")')
            Write(CEMASedFlxFilN2,'(<IMX>f10.4)')(MFTSedFlxVars(SegNumI,27), SegNumI = 1, IMX)
            
            Write(CEMASedFlxFilN2,'("JDAY = ",f8.2)')JDAY
            Write(CEMASedFlxFilN2,'(<IMX>i10)')(SegNumI, SegNumI = 1, IMX)
            Write(CEMASedFlxFilN2,'("NSOD (gO2/m2/d)")')
            Write(CEMASedFlxFilN2,'(<IMX>f10.4)')(MFTSedFlxVars(SegNumI,28), SegNumI = 1, IMX)
            
            Write(CEMASedFlxFilN4,'(f8.2,<IMX>f10.4)')JDAY,(MFTSedFlxVars(SegNumI,26), SegNumI = 1, IMX)
            
            Write(CEMASedFlxFilN5,'(f8.2,<IMX>f10.1)')JDAY,(MFTSedFlxVars(SegNumI,29), SegNumI = 1, IMX)   ! POC
            Write(CEMASedFlxFilN6,'(f8.2,<IMX>f10.2)')JDAY,(MFTSedFlxVars(SegNumI,30), SegNumI = 1, IMX)   ! PON
            Write(CEMASedFlxFilN7,'(f8.2,<IMX>f10.3)')JDAY,(MFTSedFlxVars(SegNumI,47), SegNumI = 1, IMX)   ! POP
            
            Write(CEMASedFlxFilN1,'("JDAY = ",f8.2)')JDAY
            Write(CEMASedFlxFilN1,'(<IMX>i10)')(SegNumI, SegNumI = 1, IMX)
            Write(CEMASedFlxFilN1,'("POC (mg/l)")')
            Write(CEMASedFlxFilN1,'(<IMX>f10.4)')(MFTSedFlxVars(SegNumI,29), SegNumI = 1, IMX)
            
            Write(CEMASedFlxFilN1,'("JDAY = ",f8.2)')JDAY
            Write(CEMASedFlxFilN1,'(<IMX>i10)')(SegNumI, SegNumI = 1, IMX)
            Write(CEMASedFlxFilN1,'("PON (mg/l)")')
            Write(CEMASedFlxFilN1,'(<IMX>f10.4)')(MFTSedFlxVars(SegNumI,30), SegNumI = 1, IMX)
            
            Write(CEMASedFlxFilN1,'("JDAY = ",f8.2)')JDAY
            Write(CEMASedFlxFilN1,'(<IMX>i10)')(SegNumI, SegNumI = 1, IMX)
            Write(CEMASedFlxFilN1,'("SO4 (mg/l) Layer 1")')
            Write(CEMASedFlxFilN1,'(<IMX>f10.4)')(MFTSedFlxVars(SegNumI,25), SegNumI = 1, IMX)
            
            !Write(CEMASedFlxFilN1,'("JDAY = ",f8.2)')JDAY
            !Write(CEMASedFlxFilN1,'(<IMX>i10)')(SegNumI, SegNumI = 1, IMX)
            !Write(CEMASedFlxFilN1,'("SO4 (mg/l) Layer 2")')
            !Write(CEMASedFlxFilN1,'(<IMX>f10.4)')(MFTSedFlxVars(SegNumI,48), SegNumI = 1, IMX)
            
            Write(CEMASedFlxFilN3,'("JDAY = ",f8.2)')JDAY
            Write(CEMASedFlxFilN3,'(<IMX>i10)')(SegNumI, SegNumI = 1, IMX)
            Write(CEMASedFlxFilN3,'("H1 (m)")')
            Write(CEMASedFlxFilN3,'(<IMX>f10.4)')(SD_AerLayerThick(SegNumI), SegNumI = 1, IMX)
            
            Call CEMATempOutput
            
        End If
        
        If(IncludeCEMAGenBODConstituents)Then
            
            Write(CEMAOutFilN5,'("##################### JDAY = ",f8.2," #####################")')JDAY
            Write(CEMAOutFilN5,'(<IMX>i10)')(SegNumI, SegNumI = 1, IMX)
            Write(CEMAOutFilN5,'("Aerobic Layer")')
            Do GenBODNum = 1, NumGenBODConstituents
	            Write(CEMAOutFilN5,'(a)')SedGenBODName(GenBODNum)
                Write(CEMAOutFilN5,'(<IMX>f10.4)')(SedGenBODConc(GenBODNum,SegNumI,1), SegNumI = 1, IMX)
	        End Do !j
            Write(CEMAOutFilN5,'("Anaerobic Layer")')
            Do GenBODNum = 1, NumGenBODConstituents
	            Write(CEMAOutFilN5,'(a)')SedGenBODName(GenBODNum)
                Write(CEMAOutFilN5,'(<IMX>f10.4)')(SedGenBODConc(GenBODNum,SegNumI,2), SegNumI = 1, IMX)
	        End Do !j
	        
!	        Write(1715,'(f8.4,",",f14.6,",",f14.6,",",f14.6)')jday, SedGenBODConc(1,ds(1),1),SedGenBODConc(1,ds(1),2), O2(KB(ds(1)),ds(1))
	        
        End If
    
    Return
    
    Entry CEMATempOutput
        
        CrackStatus = " NO"
        Do SegNumI = 1, IMX
            If(CrackOpen(SegNumI))CrackStatus(SegNumI) = "YES"
        End Do !SegNumI
        
        Write(CEMAOutFilN1,'("************************  JDAY = ",f8.2)')JDAY
        Write(CEMAOutFilN1,'(<IMX>i10)')(SegNumI, SegNumI = 1, IMX)
        Write(CEMAOutFilN1,'("Radius (mm)")')
        Write(CEMAOutFilN1,'(<IMX>f10.4)')(BubbleRadiusSed(SegNumI)*1000, SegNumI = 1, IMX)
        !Write(CEMAOutFilN1,'("Pressure Bubble")')
        !Write(CEMAOutFilN1,'(<IMX>f10.1)')(PresBubbSed(SegNumI), SegNumI = 1, IMX)
        !Write(CEMAOutFilN1,'("Pressure Critical")')
        !Write(CEMAOutFilN1,'(<IMX>f10.1)')(PresCritSed(SegNumI), SegNumI = 1, IMX)
        Write(CEMAOutFilN1,'("Cg (gm/m³)")')
        Write(CEMAOutFilN1,'(<IMX>f10.1)')(CgSed(SegNumI), SegNumI = 1, IMX)
        Write(CEMAOutFilN1,'("C0 (gm/m³)")')
        Write(CEMAOutFilN1,'(<IMX>f10.1)')(C0Sed(SegNumI), SegNumI = 1, IMX)
        Write(CEMAOutFilN1,'("Ct (gm/m³)")')
        Write(CEMAOutFilN1,'(<IMX>f10.1)')(CtSed(SegNumI), SegNumI = 1, IMX)
        Write(CEMAOutFilN1,'("Crack Open")')
        Write(CEMAOutFilN1,'(<IMX>a10)')(CrackStatus(SegNumI), SegNumI = 1, IMX)
        !Write(CEMAOutFilN1,'("Bubbles released")')
        !Write(CEMAOutFilN1,'(<IMX>i10)')(MFTBubbReleased(SegNumI), SegNumI = 1, IMX)
        
        Write(CEMAOutFilN2,'("Gas concentration (gm/m³) at JDAY = ",f8.2)')JDAY
        Write(CEMAOutFilN3,'(<IMX>i10)')(SegNumI, SegNumI = 1, IMX)
        Do nGas = 1, 4
            If(nGas == 1)Write(CEMAOutFilN2,'("H2S Concentration (gm/m³)")')
            If(nGas == 2)Write(CEMAOutFilN2,'("CH4  Concentration (gm/m³)")')
            If(nGas == 3)Write(CEMAOutFilN2,'("NH3  Concentration (gm/m³)")')
            If(nGas == 4)Write(CEMAOutFilN2,'("CO2  Concentration (gm/m³)")')
            Write(CEMAOutFilN2,'(<IMX>f10.3)')(TConc(nGas,SegNumI), SegNumI = 1, IMX)
        End Do !nGas
        
        nGas=2 ! CH4 write
        !Write(CEMASedFlxFilN8,'(f8.2,<IMX>f10.2)')JDAY,(TConc(nGas,SegNumI), SegNumI = 1, IMX)
        Write(CEMASedFlxFilN8,'(f8.2,<IMX>f10.2)')JDAY,(DissolvedGasSediments(nGas,SegNumI), SegNumI = 1, IMX)
        nGas=1 ! H2S write
        !Write(CEMASedFlxFilN9,'(f8.2,<IMX>f10.2)')JDAY,(TConc(nGas,SegNumI), SegNumI = 1, IMX)
        Write(CEMASedFlxFilN9,'(f8.2,<IMX>f10.2)')JDAY,(DissolvedGasSediments(nGas,SegNumI), SegNumI = 1, IMX)
        Do nGas = 1, 4
            If(nGas == 1)Write(CEMAOutFilN6,'("H2S Concentration (gm/m³)")')
            If(nGas == 2)Write(CEMAOutFilN6,'("CH4  Concentration (gm/m³)")')
            If(nGas == 3)Write(CEMAOutFilN6,'("NH3  Concentration (gm/m³)")')
            If(nGas == 4)Write(CEMAOutFilN6,'("CO2  Concentration (gm/m³)")')
            Write(CEMAOutFilN6,'(<IMX>f10.3)')(DissolvedGasSediments(nGas,SegNumI), SegNumI = 1, IMX)
        End Do !nGas
        
        Write(CEMAOutFilN3,'("Gas Release to Atmosphere at JDAY = ",f8.2)')JDAY
        Write(CEMAOutFilN3,'(<IMX>i10)')(SegNumI, SegNumI = 1, IMX)
        Do nGas = 1, 4
            If(nGas == 1)Write(CEMAOutFilN3,'("H2S Release(gm/s)")')
            If(nGas == 2)Write(CEMAOutFilN3,'("CH4 Release (gm/s)")')
            If(nGas == 3)Write(CEMAOutFilN3,'("NH3 Release (gm/s)")')
            If(nGas == 4)Write(CEMAOutFilN3,'("CO2 Release (gm/s)")')
            Write(CEMAOutFilN3,'(<IMX>f10.3)')(BRRateAGasNet(SegNumI, nGas), SegNumI = 1, IMX)
        End Do !nGas
        
        DO JW=1,NWB
        Write(CEMAOutFilBub,'(F12.4,",",I5,",",6(E12.5,","))')JDAY,JW,(BubbleRelWB(JW,J),J=1,4), GasReleaseCH4/1000.    !,GasReleaseCO2/1000.   ! SW 7/1/2017 BubbleRelWB kg  GasReleaseCH4 kg C
        END DO
        BubbleRelWB=0.0
        
        
        !Write(CEMAOutFilN4,'(<5>f10.3)')JDAY,(BRRateAGasNet(5, nGas), nGas = 1, 4)
        !Write(227,'(f10.5, ",",f10.5)')jday, c1(kb(6),6,7)/1000.d0
        
    Return
        

End Subroutine CEMAOutputRoutines