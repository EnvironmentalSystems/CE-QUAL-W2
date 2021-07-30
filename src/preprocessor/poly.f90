! Scott Wells 2015 Converting CE-QUAL-W2 bathymeney to polygons using UTM coordinates from LAT/LONG
MODULE COMPUTE_POLY
INTEGER :: I, JJB, JW, JB, IDONE, JBJW, JBJW_1,JJW, ISIGN
REAL*8  :: PI, DIST, SLOPE, ANGLE, ANGLE1, ANGLE2

!                xnw,ynw                         xne,yne
!                _________________DLX____________________
!                |                                      |
!                |                                      |
!      DS        |                                      |           US
!    xw,yw       B             xc,yc                    B  xe,ye
!   SEGMENT      |                                      |
!   END          |                                      |
!                ________________DLX_____________________
!                xsw,ysw                               xse,yse
DATA PI/3.1415926535/
END MODULE COMPUTE_POLY

SUBROUTINE POLYGONS
USE COMPUTE_POLY
USE GRID; USE CONTROL, ONLY:IMX; USE GRIDC, ONLY: US, DS, BE, BS, KTWB, XF, EL, KB
USE CONTROL, ONLY: ERR, WIN, DOWNSTREAMDAM
IMPLICIT NONE
OPEN(200,FILE='Polygon.bln',STATUS='UNKNOWN')
OPEN(201,FILE='Segment_centers.txt',status='unknown')
! Read in bathymeney file or pass variaswes
   
   ! COMPUTE COORINDATES OF SEGEMNT CENTERS

   WRITE(201,'(A)')'Segment#, Branch#, Distance(m), XCenter(m), YCenter(m), UTMZONE, BottomElev(m)'
   
   ! make sure all angles are less than 2PI
   IF(DOWNSTREAMDAM)THEN
   DO JW=1,NWB
      ISIGN=1
      NPOINT           = 0                                                                                                       !TC 02/11/01
      NUP              = 0    ! GO FROM DS TO US
      NNBR             = 1
      NCBP             = 0
      NINTERNAL        = 0
      JB               = JBDN(JW)
      NPOINT(JB)       = 1    ! THIS SAYS WE HAVE FINISHED THIS BRANCH ALREADY IF =1, OTHERWISE =0
      XW(DS(JB))=XEASTING(JW)   ! THIS ONLY WORKS IF FIRST WATERBODY X/Y ARE AT A DAM - ALL OTHER BRANCHES
      YW(DS(JB))=YNORTHING(JW)
            
         IF(JW>1)THEN
             IF(XEASTING(JW)==XEASTING(JW-1).AND. YNORTHING(JW)==YNORTHING(JW-1))THEN
                 IDONE=0                 ! DETERMINE INTERNAL CONNECTIVITY TO CONNECT WATERBODIES 
                 DO JJW=JW-1,1,-1   ! this only works if JW is connected in a lower waterbody. Need to scan WBs for connectivity and reorder the search order.
                    IF(IDONE==1)EXIT
                 DO JBJW_1=BS(JJW),BE(JJW)
                     IF(IDONE==1)EXIT
                 
                     IF(UHS(JB) >= US(JBJW_1) .AND. UHS(JB) <= DS(JBJW_1))THEN
                         IF(UHS(JB)==US(JBJW_1))THEN
                            XE(US(JB))=XE(UHS(JB))
                            YE(US(JB))=YE(UHS(JB))
                            NUP=1
                         ELSEIF(UHS(JB)==DS(JBJW_1))THEN
                             NUP=1
                             XE(US(JB))=XW(UHS(JB))
                             YE(US(JB))=YW(UHS(JB))
                         ELSE
                            IF((PHI0(DHS(JB))-PHI0(DS(JB))) >= 0.0 .AND. (PHI0(DHS(JB))-PHI0(DS(JB))) <= PI)THEN   ! PUT ON LHS OF BRANCH JJB
                            XW(US(JB))=(XNW(UHS(JB))+XNE(UHS(JB)))/2.          ! ***check
                            YW(US(JB))=(YNW(UHS(JB))+YNE(UHS(JB)))/2.
                           ELSE                                                    ! PUT ON RHS OF BRANCH JJB
                            XW(US(JB))=(XSW(UHS(JB))+XSE(UHS(JB)))/2.
                            YW(US(JB))=(YSW(UHS(JB))+YSE(UHS(JB)))/2.
                           ENDIF
                         ENDIF
                     IDONE=1    
                     EXIT
                     ELSEIF(DHS(JB) >= US(JBJW_1) .AND. UHS(JB) <= DS(JBJW_1))THEN
                         IF(DHS(JB)==US(JBJW_1))THEN
                            XW(DS(JB))=XE(DHS(JB))
                            YW(DS(JB))=YE(DHS(JB))
                         ELSEIF(DHS(JB)==DS(JBJW_1))THEN
                             NUP=1
                             XE(DS(JB))=XW(DHS(JBJW_1))
                             YE(DS(JB))=YW(DHS(JBJW_1))
                         ELSE
                            IF((PHI0(DHS(JB))-PHI0(DS(JB))) >= 0.0 .AND. (PHI0(DHS(JB))-PHI0(DS(JB))) <= PI)THEN   ! PUT ON LHS OF BRANCH JJB
                            XW(DS(JB))=(XNW(DHS(JB))+XNE(DHS(JB)))/2.          ! ***check
                            YW(DS(JB))=(YNW(DHS(JB))+YNE(DHS(JB)))/2.
                           ELSE                                                    ! PUT ON RHS OF BRANCH JJB
                            XW(DS(JB))=(XSW(DHS(JB))+XSE(DHS(JB)))/2.
                            YW(DS(JB))=(YSW(DHS(JB))+YSE(DHS(JB)))/2.
                           ENDIF
                         ENDIF
                     IDONE=1
                     EXIT
                     ENDIF
                 ENDDO           
                ENDDO   
                IF(IDONE==0)THEN
                    !CALL ERRORS
                    !WRITE(ERR,'(A,I3,A,I3)')'POLY.F90: ERROR - BRANCH CONNECTIVITY FOR WATERBODY JW NOT LESS THAN JW: Cannot write out poly.bln and segment centers, WB:',JJW,' JB:',jbjw_1
                    !WRITE(WIN,*)'ERROR IN POLYGON SUBROUTINE: SEGMENT CENTERS WILL NOT BE WRITTEN OUT'
                    ! ASSUME LINEAR CONNECTION EVEN IF NO APPARENT CONNECTION BECAUSE OF SPILLWAY
                    NUP=1
                    XE(US(JB))=XW(DS(JB-1))
                    YE(US(JB))=YW(DS(JB-1))
                ENDIF
                 
             ENDIF
         ENDIF
         DO WHILE (NNBR <= (BE(JW)-BS(JW)+1))
        NCBP = NCBP+1
       
       
        IF (NINTERNAL == 0) THEN
          IF (NUP == 0) THEN
            DO I=DS(JB),US(JB),-1
              CALL DOIT_DS_US
            END DO
          ELSE
            DO I=US(JB),DS(JB)
              CALL DOIT_US_DS
            END DO
            NUP = 0
          END IF
 
        ELSEIF(NINTERNAL==1)THEN     ! only for ninternal=1 ...an internal connection

          CALL DOIT2
          DO I = UHS(JJB)+1, DS(JB)
              CALL DOIT_US_DS
          END DO
          DO I=UHS(JJB)-1,US(JB),-1
              CALL DOIT_DS_US
          END DO
          NINTERNAL = 0
        ELSEIF(NINTERNAL==2)THEN     ! only for ninternal=2 ...an internal connection

          CALL DOIT2
          DO I = DHS(JJB)+1, DS(JB)
              CALL DOIT_US_DS
          END DO
          DO I=DHS(JJB)-1,US(JB),-1
              CALL DOIT_DS_US
          END DO
          NINTERNAL = 0        
          
        END IF
        IF (NNBR == (BE(JW)-BS(JW)+1)) EXIT
        DO JB=BS(JW),BE(JW)
          IF (NPOINT(JB) /= 1) THEN
            DO JJB = BS(JW), BE(JW)
              IF (DHS(JB) >= US(JJB) .AND. DHS(JB) <= DS(JJB) .AND. NPOINT(JJB) == 1) THEN  ! 
                NPOINT(JB)       = 1     ! NOW PROCESS THIS BRANCH and DO NOT REPEAT ANALYSES OF THIS BRANCH
                NNBR             = NNBR+1
                IF(DHS(JB)==US(JJB))THEN
                XW(DS(JB))=XE(DHS(JB))
                YW(DS(JB))=YE(DHS(JB))
                ELSEIF(DHS(JB)==DS(JJB))THEN
                !NUP=1
                    ANGLE1=PHI0(DHS(JB))+PI/2.
                    IF(ANGLE1 > 2.*PI)ANGLE1=ANGLE1-2.*PI
                    ANGLE2=PHI0(DHS(JB))-PI/2.
                    IF(ANGLE2 > 2.*PI)ANGLE2=ANGLE2-2.*PI
                    IF(PHI0(DS(JB)) >= ANGLE2 .AND. PHI0(DS(JB)) <= PHI0(DHS(JB)))THEN 
                     XW(DS(JB))=(XSW(DHS(JB))+XSE(DHS(JB)))/2.
                     YW(DS(JB))=(YSW(DHS(JB))+YSE(DHS(JB)))/2.
                    ELSEIF(PHI0(DS(JB)) > PHI0(DHS(JB)) .OR. PHI0(DS(JB)) < ANGLE1)THEN 
                     XW(DS(JB))=(XNW(DHS(JB))+XNE(DHS(JB)))/2.          
                     YW(DS(JB))=(YNW(DHS(JB))+YNE(DHS(JB)))/2.
                    ELSE
                    XW(DS(JB))=XW(DHS(JB))
                    YW(DS(JB))=YW(DHS(JB))
                    !XW(DS(JB))=XE(DHS(JB))
                    !YW(DS(JB))=YE(DHS(JB))
                    ENDIF
                ELSE
                    IF((PHI0(DHS(JB))-PHI0(DS(JB))) >= 0.0 .AND. (PHI0(DHS(JB))-PHI0(DS(JB))) <= PI)THEN   ! PUT ON LHS OF BRANCH JJB
                    XW(DS(JB))=(XNW(DHS(JB))+XNE(DHS(JB)))/2.          ! switched 4/22
                    YW(DS(JB))=(YNW(DHS(JB))+YNE(DHS(JB)))/2.
                    !XW(DS(JB))=(XSW(DHS(JB))+XSE(DHS(JB)))/2.
                    !YW(DS(JB))=(YSW(DHS(JB))+YSE(DHS(JB)))/2. 
                   ELSE                                                    ! PUT ON RHS OF BRANCH JJB
                    XW(DS(JB))=(XSW(DHS(JB))+XSE(DHS(JB)))/2.
                    YW(DS(JB))=(YSW(DHS(JB))+YSE(DHS(JB)))/2.
                    !XW(DS(JB))=(XNW(DHS(JB))+XNE(DHS(JB)))/2.          ! ***check
                    !YW(DS(JB))=(YNW(DHS(JB))+YNE(DHS(JB)))/2.
                   ENDIF
                ENDIF
                EXIT
            END IF
            IF (UHS(JJB) == DS(JB) .AND. NPOINT(JJB) == 1) THEN
                NPOINT(JB)       = 1
                NNBR             = NNBR+1
                    XW(DS(JB))=XE(UHS(JJB))
                    YW(DS(JB))=YE(UHS(JJB))
                EXIT
            END IF
            IF (UHS(JJB) <= DS(JB) .AND. UHS(JJB) >= US(JB) .AND. NPOINT(JJB) == 1) THEN
                NNBR             = NNBR+1
                NINTERNAL        = 1      ! THIS IS CONNECTED IN THE MIDDLE OF A BRANCH INTERNALLY
                NPOINT(JB)       = 1
                XCS(UHS(JJB))=XE(US(JJB))    ! shouldn't this be either xcs or xcn?
                YCS(UHS(JJB))=YE(US(JJB))
                IF((PHI0(UHS(JJB))-PHI0(US(JJB)))>= 0.0 .AND. (PHI0(UHS(JJB))-PHI0(US(JJB))) <= PI)THEN   ! PUT ON LHS OF BRANCH JJB
                    ISIGN=-1
                ELSE                                                    ! PUT ON RHS OF BRANCH JJB
                    ISIGN=1    ! CONVERTS THE dist COMPUTATION FOR COMPUTING XC
                ENDIF
                
                
                
                I=UHS(JJB)
                EXIT
            END IF
            IF (UHS(JB) <= DS(JJB) .AND. UHS(JB) >= US(JJB) .AND. NPOINT(JJB) == 1) THEN
                NNBR             = NNBR+1
                NPOINT(JB)       = 1     ! NOTE FOR THIS CASE WE SWITCH XW AND XE SINCE WE ARE GOING BACKWARDS
                 IF((PHI0(UHS(JJB))-PHI0(DS(JB)))>= 0.0 .AND. (PHI0(UHS(JJB))-PHI0(DS(JB))) <= PI)THEN   ! PUT ON LHS OF BRANCH JJB
                    XE(US(JB))=(XSW(UHS(JJB))+XNW(UHS(JJB)))/2.
                    YE(US(JB))=(YSW(UHS(JJB))+YNW(UHS(JJB)))/2.
                 ELSE                                                    ! PUT ON RHS OF BRANCH JJB
                    XE(US(JB))=(XSE(UHS(JJB))+XNE(UHS(JJB)))/2.
                    YE(US(JB))=(YSE(UHS(JJB))+YNE(UHS(JJB)))/2.
                ENDIF
                NUP = 1     ! REVERSE ORDER OF BRANCH FROM US TO DS
                EXIT
            END IF
            IF (DHS(JJB) <= DS(JB) .AND. DHS(JJB) >= US(JB) .AND. NPOINT(JJB) == 1) THEN
                NNBR             = NNBR+1
                NPOINT(JB)       = 1     ! NOTE FOR THIS CASE WE SWITCH XW AND XE SINCE WE ARE GOING BACKWARDS
                XCS(DHS(JJB))=XW(DS(JJB))
                YCS(DHS(JJB))=YW(DS(JJB))
                IF((PHI0(DHS(JJB))-PHI0(DS(JJB)))>= 0.0 .AND. (PHI0(DHS(JJB))-PHI0(DS(JJB))) <= PI)THEN   ! PUT ON LHS OF BRANCH JJB
                    ISIGN=-1
                !    XE(US(JB))=(XSW(UHS(JJB))+XNW(UHS(JJB)))/2.
                !    YE(US(JB))=(YSW(UHS(JJB))+YNW(UHS(JJB)))/2.
                ELSE                                                    ! PUT ON RHS OF BRANCH JJB
                    ISIGN=1    ! CONVERTS THE dist COMPUTATION FOR COMPUTING XC
                !    XE(US(JB))=(XSE(UHS(JJB))+XNE(UHS(JJB)))/2.
                !    YE(US(JB))=(YSE(UHS(JJB))+YNE(UHS(JJB)))/2.
                ENDIF
                NINTERNAL        = 2      ! THIS IS CONNECTED IN THE MIDDLE OF A BRANCH INTERNALLY
                I=DHS(JJB)
                EXIT
            END IF
            END DO
            IF (NPOINT(JB) == 1) EXIT
          END IF
        END DO
    END DO     
   ENDDO
    ELSE
        
        JB=1
        JW=1
        XE(US(JB))=XEASTING(JW)   ! THIS ONLY WORKS IF FIRST WATERBODY X/Y ARE AT A DAM - ALL OTHER BRANCHES
        YE(US(JB))=YNORTHING(JW)
        DO JB=1,NBR   ! JUST DO LINEAR SYSTEM STARTING AT TOP
            ISIGN=1   
            DO I=US(JB),DS(JB)
              CALL DOIT_US_DS
            END DO
            IF(JB<NBR)THEN
            XE(US(JB+1))=XW(DS(JB))
            YE(US(JB+1))=YW(DS(JB))
            ENDIF
   
        ENDDO
    ENDIF
    
    CLOSE(200)
    CLOSE(201)
    RETURN
    END
        
        
    
    !                xnw,ynw                         xne,yne
!                _________________DLX____________________
!                |                                      |
!                |                                      |
!                |                                      |
!    xw,yw       B             xc,yc                    B  xe,ye
!   SEGMENT      |                                      |
!   END          |                                      |
!                ________________DLX_____________________
!                xsw,ysw                               xse,yse

    
    !IN CASE NINTERNAL=1 THEN
!                xnw,ynw       XCN,YCN                xne,yne
!                _________________DLX____________________
!                |                                      |
!                |                                      |
!                |                                      |
!    xw,yw       B             xc,yc                    B  xe,ye
!   SEGMENT      |                                      |
!   END          |                                      |
!                ________________DLX_____________________
!                xsw,ysw        XCS,YCS                xse,yse
    
    
        SUBROUTINE DOIT_DS_US
        USE COMPUTE_POLY; USE GRID; USE CONTROL, ONLY:IMX; USE GRIDC, ONLY: US, DS, BE, BS, KTWB, XF, EL, KB

        IMPLICIT NONE
        INTEGER :: ISP         
        ANGLE=PHI0(I)
        DIST=DLX(I)/2.
        IF(ANGLE> 3.*PI/2. .AND. ANGLE < 2.*PI)THEN
            ISP=1
            SLOPE=TAN(ANGLE)
            CALL X1(xw(I),SLOPE,DIST,XC(I))                         
            CALL Y1(yw(I),SLOPE,DIST,YC(I))
            CALL X1(XC(I),SLOPE,DIST,xe(I))   
            CALL Y1(YC(I),SLOPE,DIST,ye(I))   
            
        ELSEIF(ANGLE==2.*PI .OR. ANGLE==0.0)THEN
            ISP=2
            SLOPE=0.0
            CALL X1(xw(I),SLOPE,DIST,XC(I))                         
            CALL Y1(yw(I),SLOPE,DIST,YC(I))
            CALL X1(XC(I),SLOPE,DIST,xe(I))   
            CALL Y1(YC(I),SLOPE,DIST,ye(I))   
            
        ELSEIF(ANGLE==PI)THEN
            ISP=3
            SLOPE=0.0
            CALL X1(xw(I),SLOPE,-DIST,XC(I))                         
            CALL Y1(yw(I),SLOPE,-DIST,YC(I))
            CALL X1(XC(I),SLOPE,-DIST,xe(I))   
            CALL Y1(YC(I),SLOPE,-DIST,ye(I))   
            
        ELSEIF(ANGLE > PI .AND. ANGLE < 3.*PI/2.)THEN
            ISP=4
            SLOPE=TAN(PI/2.-3.*PI/2.+ANGLE)     ! DX/DY
            CALL X1(xw(I),SLOPE,-DIST,XC(I))                         
            CALL Y1(yw(I),SLOPE,-DIST,YC(I))
            CALL X1(XC(I),SLOPE,-DIST,xe(I))   
            CALL Y1(YC(I),SLOPE,-DIST,ye(I))     

        ELSEIF(ANGLE > PI/2. .AND. ANGLE < PI)THEN   ! OK
            ISP=5
            SLOPE=TAN(PI-ANGLE)
            CALL X1(xw(I),SLOPE,DIST,XC(I))                         
            CALL Y1(yw(I),SLOPE,-DIST,YC(I))
            CALL X1(XC(I),SLOPE,DIST,xe(I))   
            CALL Y1(YC(I),SLOPE,-DIST,ye(I))     

        ELSEIF(ANGLE>0.0 .AND. ANGLE < PI/2.)THEN   !OK
            ISP=6
            SLOPE=TAN(ANGLE)
            CALL X1(xw(I),SLOPE,DIST,XC(I))                         
            CALL Y1(yw(I),SLOPE,DIST,YC(I))
            CALL X1(XC(I),SLOPE,DIST,xe(I))   
            CALL Y1(YC(I),SLOPE,DIST,ye(I))   
            
        ELSEIF(ANGLE == PI/2.)THEN
             ISP=7
             XC(I)=xw(I)-DIST
             YC(I)=yw(I)
             xe(I)=XC(I)-DIST
             ye(I)=YC(I)

        ELSEIF(ANGLE == 3.*PI/2.)THEN
             XC(I)=xw(I)+DIST
             YC(I)=yw(I)
             xe(I)=XC(I)+DIST
             ye(I)=YC(I)

        ELSE
            WRITE(*,*)'Error - no case found for angle'
            ISP=9
        ENDIF
        WRITE(201,'(I3,",",I3,",",F15.2,",",F15.5,",",F15.5,",",I3,",",F10.3)')I,JB,XF(I),XC(I),YC(I),UTMZONE(JW),EL(KB(I)+1,I)
        IF(I>US(JB))THEN        
        XW(I-1)=XE(I)
        YW(I-1)=YE(I)
        ENDIF
   
        DIST=B(KTWB(JW),I)/2
        ANGLE=PHI0(I)+PI/2.
        IF(ANGLE> 2.*PI)ANGLE=ANGLE-2.*PI
        SLOPE=TAN(ANGLE)
        IF(ANGLE> 3.*PI/2. .AND. ANGLE < 2.*PI)THEN
            ISP=1
            CALL X1(XW(I),SLOPE,DIST,XNW(I))                         
            CALL Y1(YW(I),SLOPE,DIST,YNW(I))
            CALL X1(XW(I),SLOPE,-DIST,XSW(I))   
            CALL Y1(YW(I),SLOPE,-DIST,YSW(I)) 
            
            CALL X1(XE(I),SLOPE,DIST,XNE(I))                         
            CALL Y1(YE(I),SLOPE,DIST,YNE(I))
            CALL X1(XE(I),SLOPE,-DIST,XSE(I))   
            CALL Y1(YE(I),SLOPE,-DIST,YSE(I))   
        ELSEIF(ANGLE==2.*PI .OR. ANGLE==0.0)THEN
            ISP=2
            SLOPE=0.0
            CALL X1(XW(I),SLOPE,DIST,XNW(I))                         
            CALL Y1(YW(I),SLOPE,DIST,YNW(I))
            CALL X1(XW(I),SLOPE,-DIST,XSW(I))   
            CALL Y1(YW(I),SLOPE,-DIST,YSW(I))   
        
            CALL X1(XE(I),SLOPE,DIST,XNE(I))                         
            CALL Y1(YE(I),SLOPE,DIST,YNE(I))
            CALL X1(XE(I),SLOPE,-DIST,XSE(I))   
            CALL Y1(YE(I),SLOPE,-DIST,YSE(I)) 
        ELSEIF(ANGLE==PI)THEN
            ISP=3
            SLOPE=0.0
           CALL X1(XW(I),SLOPE,DIST,XNW(I))                         
            CALL Y1(YW(I),SLOPE,DIST,YNW(I))
            CALL X1(XW(I),SLOPE,-DIST,XSW(I))   
            CALL Y1(YW(I),SLOPE,-DIST,YSW(I))   
        
            CALL X1(XE(I),SLOPE,DIST,XNE(I))                         
            CALL Y1(YE(I),SLOPE,DIST,YNE(I))
            CALL X1(XE(I),SLOPE,-DIST,XSE(I))   
            CALL Y1(YE(I),SLOPE,-DIST,YSE(I)) 
        ELSEIF(ANGLE > PI .AND. ANGLE < 3.*PI/2.)THEN
            ISP=4
           CALL X1(XW(I),SLOPE,DIST,XNW(I))                         
            CALL Y1(YW(I),SLOPE,DIST,YNW(I))
            CALL X1(XW(I),SLOPE,-DIST,XSW(I))   
            CALL Y1(YW(I),SLOPE,-DIST,YSW(I))   
        
            CALL X1(XE(I),SLOPE,DIST,XNE(I))                         
            CALL Y1(YE(I),SLOPE,DIST,YNE(I))
            CALL X1(XE(I),SLOPE,-DIST,XSE(I))   
            CALL Y1(YE(I),SLOPE,-DIST,YSE(I)) 
        ELSEIF(ANGLE > PI/2. .AND. ANGLE < PI)THEN   ! OK
            ISP=5
           CALL X1(XW(I),SLOPE,DIST,XNW(I))                         
            CALL Y1(YW(I),SLOPE,DIST,YNW(I))
            CALL X1(XW(I),SLOPE,-DIST,XSW(I))   
            CALL Y1(YW(I),SLOPE,-DIST,YSW(I))  
        
            CALL X1(XE(I),SLOPE,DIST,XNE(I))                         
            CALL Y1(YE(I),SLOPE,DIST,YNE(I))
            CALL X1(XE(I),SLOPE,-DIST,XSE(I))   
            CALL Y1(YE(I),SLOPE,-DIST,YSE(I)) 
        ELSEIF(ANGLE>0.0 .AND. ANGLE < PI/2.)THEN   !OK
            ISP=6
           CALL X1(XW(I),SLOPE,DIST,XNW(I))                         
            CALL Y1(YW(I),SLOPE,DIST,YNW(I))
            CALL X1(XW(I),SLOPE,-DIST,XSW(I))   
            CALL Y1(YW(I),SLOPE,-DIST,YSW(I))  
        
            CALL X1(XE(I),SLOPE,DIST,XNE(I))                         
            CALL Y1(YE(I),SLOPE,DIST,YNE(I))
            CALL X1(XE(I),SLOPE,-DIST,XSE(I))   
            CALL Y1(YE(I),SLOPE,-DIST,YSE(I)) 
        ELSEIF(ANGLE == PI/2.)THEN
            ISP=7
             XNW(I)=XW(I)-DIST
             YNW(I)=YW(I)
             XSW(I)=XW(I)+DIST
             YSW(I)=YW(I)
             XNE(I)=XE(I)-DIST
             YNE(I)=YE(I)
             XSE(I)=XE(I)+DIST
             YSE(I)=YE(I)
        ELSEIF(ANGLE == 3.*PI/2.)THEN
            ISP=8
            XNW(I)=XW(I)+DIST
             YNW(I)=YW(I)
             XSW(I)=XW(I)-DIST
             YSW(I)=YW(I)
             XNE(I)=XE(I)+DIST
             YNE(I)=YE(I)
             XSE(I)=XE(I)-DIST
             YSE(I)=YE(I)
        ELSE
            ISP=9
            WRITE(*,*)'Error - no case found for angle'
        ENDIF
        WRITE(200,'(A,A,i3,A,i3,A,F6.1,A,F6.1,1X,I2)')'5,1','     Segment:',I,' Branch:',JB,' DX=',dlx(i),' B=',b(ktwb(jw),i),ISP
        WRITE(200,'(F15.4,",",F15.4,",",I3,",",F10.3)')XNW(I),YNW(I),UTMZONE(JW)
        WRITE(200,'(F15.4,",",F15.4,",",I3,",",F10.3)')XNE(I),YNE(I),UTMZONE(JW)
        WRITE(200,'(F15.4,",",F15.4,",",I3,",",F10.3)')XSE(I),YSE(I),UTMZONE(JW)
        WRITE(200,'(F15.4,",",F15.4,",",I3,",",F10.3)')XSW(I),YSW(I),UTMZONE(JW)
        WRITE(200,'(F15.4,",",F15.4,",",I3,",",F10.3)')XNW(I),YNW(I),UTMZONE(JW)
    
	RETURN
    END SUBROUTINE DOIT_DS_US
! ************************************************************    
    SUBROUTINE DOIT_US_DS    ! SAME ROUTINE AS ABOVE BUT DIFFERENT DIRECTION
        USE COMPUTE_POLY; USE GRID; USE CONTROL, ONLY:IMX; USE GRIDC, ONLY: US, DS, BE, BS, KTWB, XF, EL, KB

        IMPLICIT NONE
         
        ANGLE=PHI0(I)
        DIST=DLX(I)/2.
        IF(ANGLE> 3.*PI/2. .AND. ANGLE < 2.*PI)THEN
            SLOPE=TAN(2.*PI+ANGLE)     ! DX/DY    OK
            CALL X1(XE(I),SLOPE,-DIST,XC(I))                         
            CALL Y1(YE(I),SLOPE,-DIST,YC(I))
            CALL X1(XC(I),SLOPE,-DIST,XW(I))   
            CALL Y1(YC(I),SLOPE,-DIST,YW(I))   
        ELSEIF(ANGLE==2.*PI .OR. ANGLE==0.0)THEN
            SLOPE=0.0
            CALL X1(XE(I),SLOPE,-DIST,XC(I))                         
            CALL Y1(YE(I),SLOPE,-DIST,YC(I))
            CALL X1(XC(I),SLOPE,-DIST,XW(I))   
            CALL Y1(YC(I),SLOPE,-DIST,YW(I))    
        ELSEIF(ANGLE==PI)THEN
            SLOPE=0.0
            CALL X1(XE(I),SLOPE,DIST,XC(I))                         
            CALL Y1(YE(I),SLOPE,DIST,YC(I))
            CALL X1(XC(I),SLOPE,DIST,XW(I))   
            CALL Y1(YC(I),SLOPE,DIST,YW(I))   
        ELSEIF(ANGLE > PI .AND. ANGLE < 3.*PI/2.)THEN
            SLOPE=TAN(PI/2.-3.*PI/2.+ANGLE)     ! DX/DY
            CALL X1(XE(I),SLOPE,DIST,XC(I))                         
            CALL Y1(YE(I),SLOPE,DIST,YC(I))
            CALL X1(XC(I),SLOPE,DIST,XW(I))   
            CALL Y1(YC(I),SLOPE,DIST,YW(I))   
        ELSEIF(ANGLE > PI/2. .AND. ANGLE < PI)THEN   ! OK
            SLOPE=TAN(PI-ANGLE)
            CALL X1(XE(I),SLOPE,-DIST,XC(I))                         
            CALL Y1(YE(I),SLOPE,DIST,YC(I))
            CALL X1(XC(I),SLOPE,-DIST,XW(I))   
            CALL Y1(YC(I),SLOPE,DIST,YW(I)) 
        ELSEIF(ANGLE>0.0 .AND. ANGLE < PI/2.)THEN   !OK
            SLOPE=TAN(ANGLE)
            CALL X1(XE(I),SLOPE,-DIST,XC(I))                         
            CALL Y1(YE(I),SLOPE,-DIST,YC(I))
            CALL X1(XC(I),SLOPE,-DIST,XW(I))   
            CALL Y1(YC(I),SLOPE,-DIST,YW(I))
        ELSEIF(ANGLE == PI/2.)THEN
             XC(I)=XE(I)-DIST
             YC(I)=YE(I)
             XW(I)=XC(I)-DIST
             YW(I)=YC(I)
        ELSEIF(ANGLE == 3.*PI/2.)THEN
             XC(I)=XE(I)+DIST
             YC(I)=YE(I)
             XW(I)=XC(I)+DIST
             YW(I)=YC(I)
        ELSE
            WRITE(*,*)'Error - no case found for angle'
        ENDIF
        !WRITE(201,'(I3,",",I3,",",F15.5,",",F15.5,",",I3,",",F10.3)')I,JB,XC(I),YC(I),UTMZONE(JW),EL(KB(I)+1,I)
        WRITE(201,'(I3,",",I3,",",F15.2,",",F15.5,",",F15.5,",",I3,",",F10.3)')I,JB,XF(I),XC(I),YC(I),UTMZONE(JW),EL(KB(I)+1,I)
        IF(I<DS(JB))THEN        
        XE(I+1)=XW(I)
        YE(I+1)=YW(I)
        ENDIF
      
! define polygon corners NW AND SW AND NE AND SE
        DIST=B(KTWB(JW),I)/2
        ANGLE=PHI0(I)+PI/2.
        IF(ANGLE> 2.*PI)ANGLE=ANGLE-2.*PI
        SLOPE=TAN(ANGLE)

        IF(ANGLE> 3.*PI/2. .AND. ANGLE < 2.*PI)THEN

            CALL X1(XW(I),SLOPE,DIST,XNW(I))                         
            CALL Y1(YW(I),SLOPE,DIST,YNW(I))
            CALL X1(XW(I),SLOPE,-DIST,XSW(I))   
            CALL Y1(YW(I),SLOPE,-DIST,YSW(I)) 
            
            CALL X1(XE(I),SLOPE,DIST,XNE(I))                         
            CALL Y1(YE(I),SLOPE,DIST,YNE(I))
            CALL X1(XE(I),SLOPE,-DIST,XSE(I))   
            CALL Y1(YE(I),SLOPE,-DIST,YSE(I))   
        ELSEIF(ANGLE==2.*PI .OR. ANGLE==0.0)THEN
            SLOPE=0.0
            CALL X1(XW(I),SLOPE,DIST,XNW(I))                         
            CALL Y1(YW(I),SLOPE,DIST,YNW(I))
            CALL X1(XW(I),SLOPE,-DIST,XSW(I))   
            CALL Y1(YW(I),SLOPE,-DIST,YSW(I))   
   
            CALL X1(XE(I),SLOPE,DIST,XNE(I))                         
            CALL Y1(YE(I),SLOPE,DIST,YNE(I))
            CALL X1(XE(I),SLOPE,-DIST,XSE(I))   
            CALL Y1(YE(I),SLOPE,-DIST,YSE(I)) 
        ELSEIF(ANGLE==PI)THEN
            SLOPE=0.0
           CALL X1(XW(I),SLOPE,DIST,XNW(I))                         
            CALL Y1(YW(I),SLOPE,DIST,YNW(I))
            CALL X1(XW(I),SLOPE,-DIST,XSW(I))   
            CALL Y1(YW(I),SLOPE,-DIST,YSW(I))   
 
            CALL X1(XE(I),SLOPE,DIST,XNE(I))                         
            CALL Y1(YE(I),SLOPE,DIST,YNE(I))
            CALL X1(XE(I),SLOPE,-DIST,XSE(I))   
            CALL Y1(YE(I),SLOPE,-DIST,YSE(I)) 
        ELSEIF(ANGLE > PI .AND. ANGLE < 3.*PI/2.)THEN

           CALL X1(XW(I),SLOPE,DIST,XNW(I))                         
            CALL Y1(YW(I),SLOPE,DIST,YNW(I))
            CALL X1(XW(I),SLOPE,-DIST,XSW(I))   
            CALL Y1(YW(I),SLOPE,-DIST,YSW(I))   

            CALL X1(XE(I),SLOPE,DIST,XNE(I))                         
            CALL Y1(YE(I),SLOPE,DIST,YNE(I))
            CALL X1(XE(I),SLOPE,-DIST,XSE(I))   
            CALL Y1(YE(I),SLOPE,-DIST,YSE(I)) 
        ELSEIF(ANGLE > PI/2. .AND. ANGLE < PI)THEN   ! OK
 
           CALL X1(XW(I),SLOPE,DIST,XNW(I))                         
            CALL Y1(YW(I),SLOPE,DIST,YNW(I))
            CALL X1(XW(I),SLOPE,-DIST,XSW(I))   
            CALL Y1(YW(I),SLOPE,-DIST,YSW(I))  

            CALL X1(XE(I),SLOPE,DIST,XNE(I))                         
            CALL Y1(YE(I),SLOPE,DIST,YNE(I))
            CALL X1(XE(I),SLOPE,-DIST,XSE(I))   
            CALL Y1(YE(I),SLOPE,-DIST,YSE(I)) 
        ELSEIF(ANGLE>0.0 .AND. ANGLE < PI/2.)THEN   !OK

           CALL X1(XW(I),SLOPE,DIST,XNW(I))                         
            CALL Y1(YW(I),SLOPE,DIST,YNW(I))
            CALL X1(XW(I),SLOPE,-DIST,XSW(I))   
            CALL Y1(YW(I),SLOPE,-DIST,YSW(I))  
 
            CALL X1(XE(I),SLOPE,DIST,XNE(I))                         
            CALL Y1(YE(I),SLOPE,DIST,YNE(I))
            CALL X1(XE(I),SLOPE,-DIST,XSE(I))   
            CALL Y1(YE(I),SLOPE,-DIST,YSE(I)) 
        ELSEIF(ANGLE == PI/2.)THEN
             XNW(I)=XW(I)-DIST
             YNW(I)=YW(I)
             XSW(I)=XW(I)+DIST
             YSW(I)=YW(I)
             XNE(I)=XE(I)-DIST
             YNE(I)=YE(I)
             XSE(I)=XE(I)+DIST
             YSE(I)=YE(I)
        ELSEIF(ANGLE == 3.*PI/2.)THEN
            XNW(I)=XW(I)+DIST
             YNW(I)=YW(I)
             XSW(I)=XW(I)-DIST
             YSW(I)=YW(I)
             XNE(I)=XE(I)+DIST
             YNE(I)=YE(I)
             XSE(I)=XE(I)-DIST
             YSE(I)=YE(I)
        ELSE
            WRITE(*,*)'Error - no case found for angle'
        ENDIF

        WRITE(200,'(A,A,i3,A,i3,A,F6.1,A,F6.1)')'5,1','     Segment:',I,' Branch:',JB,' DX=',dlx(i),' B=',b(ktwb(jw),i)
        WRITE(200,'(F15.4,",",F15.4,",",I3,",",F10.3)')XNW(I),YNW(I),UTMZONE(JW)
        WRITE(200,'(F15.4,",",F15.4,",",I3,",",F10.3)')XNE(I),YNE(I),UTMZONE(JW)
        WRITE(200,'(F15.4,",",F15.4,",",I3,",",F10.3)')XSE(I),YSE(I),UTMZONE(JW)
        WRITE(200,'(F15.4,",",F15.4,",",I3,",",F10.3)')XSW(I),YSW(I),UTMZONE(JW)
        WRITE(200,'(F15.4,",",F15.4,",",I3,",",F10.3)')XNW(I),YNW(I),UTMZONE(JW)
    
	RETURN
    END SUBROUTINE DOIT_US_DS
 !**********************************************************************   
    SUBROUTINE DOIT2                 ! ONLY FOR  IF(NINTERNAL==1)THEN
        USE COMPUTE_POLY; USE GRID; USE CONTROL, ONLY:IMX; USE GRIDC, ONLY: US, DS, BE, BS, KTWB, XF, EL, KB

        IMPLICIT NONE
        
        DIST=REAL(ISIGN)*B(KTWB(JW),I)/2
        ANGLE=PHI0(I)+PI/2.   
        IF(ANGLE> 2.*PI)ANGLE=ANGLE-2.*PI
        SLOPE=TAN(ANGLE)
        IF(ANGLE> 3.*PI/2. .AND. ANGLE < 2.*PI)THEN
            !SLOPE=TAN(2.*PI+ANGLE)     ! DX/DY    OK
            DIST=-DIST
            CALL X1(XCS(I),SLOPE,DIST,XC(I))                         
            CALL Y1(YCS(I),SLOPE,DIST,YC(I))
        ELSEIF(ANGLE==2.*PI .OR. ANGLE==0.0)THEN
            SLOPE=0.0
            DIST=-DIST
            CALL X1(XCS(I),SLOPE,DIST,XC(I))                         
            CALL Y1(YCS(I),SLOPE,DIST,YC(I))
        ELSEIF(ANGLE==PI)THEN
            SLOPE=0.0
             CALL X1(XCS(I),SLOPE,DIST,XC(I))                         
            CALL Y1(YCS(I),SLOPE,DIST,YC(I))
        ELSEIF(ANGLE > PI .AND. ANGLE < 3.*PI/2.)THEN
            !SLOPE=TAN(PI/2.-3.*PI/2.+ANGLE)     ! DX/DY
             CALL X1(XCS(I),SLOPE,DIST,XC(I))                         
            CALL Y1(YCS(I),SLOPE,DIST,YC(I))
        ELSEIF(ANGLE > PI/2. .AND. ANGLE < PI)THEN   ! OK
            !SLOPE=TAN(PI-ANGLE)
             CALL X1(XCS(I),SLOPE,-DIST,XC(I))                         
            CALL Y1(YCS(I),SLOPE,DIST,YC(I))
        ELSEIF(ANGLE>0.0 .AND. ANGLE < PI/2.)THEN   !OK
            !SLOPE=TAN(ANGLE)
             CALL X1(XCS(I),SLOPE,-DIST,XC(I))                         
            CALL Y1(YCS(I),SLOPE,-DIST,YC(I))
        ELSEIF(ANGLE == PI/2.)THEN
             XC(I)=XCS(I)-DIST
             YC(I)=YCS(I)
        ELSEIF(ANGLE == 3.*PI/2.)THEN
             XC(I)=XCS(I)+DIST
             YC(I)=YCS(I)
        ELSE
            WRITE(*,*)'Error - no case found for angle'
        ENDIF
        !WRITE(201,'(I3,",",I3,",",F15.5,",",F15.5,",",I3,",",F10.3)')I,JB,XC(I),YC(I),UTMZONE(JW),EL(KB(I)+1,I)
        WRITE(201,'(I3,",",I3,",",F15.2,",",F15.5,",",F15.5,",",I3,",",F10.3)')I,JB,XF(I),XC(I),YC(I),UTMZONE(JW),EL(KB(I)+1,I)
   
        ANGLE=PHI0(I)   !+PI/2.     
        DIST=DLX(I)/2.
        IF(ANGLE> 3.*PI/2. .AND. ANGLE < 2.*PI)THEN
            SLOPE=TAN(2.*PI+ANGLE)     ! DX/DY    OK
            CALL X1(XC(I),SLOPE,-DIST,XW(I))       ! CHECK - MAY BE BACKWARDS????
            CALL Y1(YC(I),SLOPE,-DIST,YW(I))  
            CALL X1(XC(I),SLOPE,DIST,XE(I))   
            CALL Y1(YC(I),SLOPE,DIST,YE(I)) 
        ELSEIF(ANGLE==2.*PI .OR. ANGLE==0.0)THEN
            SLOPE=0.0
            CALL X1(XC(I),SLOPE,-DIST,XW(I))   
            CALL Y1(YC(I),SLOPE,-DIST,YW(I))   
            CALL X1(XC(I),SLOPE,DIST,XW(I))   
            CALL Y1(YC(I),SLOPE,DIST,YW(I)) 
        ELSEIF(ANGLE==PI)THEN
            SLOPE=0.0
            CALL X1(XC(I),SLOPE,DIST,XW(I))   
            CALL Y1(YC(I),SLOPE,DIST,YW(I)) 
            CALL X1(XC(I),SLOPE,-DIST,XE(I))   
            CALL Y1(YC(I),SLOPE,-DIST,YE(I)) 
        ELSEIF(ANGLE > PI .AND. ANGLE < 3.*PI/2.)THEN
            SLOPE=TAN(PI/2.-3.*PI/2.+ANGLE)     ! DX/DY
            CALL X1(XC(I),SLOPE,DIST,XW(I))   
            CALL Y1(YC(I),SLOPE,DIST,YW(I)) 
            CALL X1(XC(I),SLOPE,-DIST,XE(I))   
            CALL Y1(YC(I),SLOPE,-DIST,YE(I)) 
        ELSEIF(ANGLE > PI/2. .AND. ANGLE < PI)THEN   ! OK
            SLOPE=TAN(PI-ANGLE)
            CALL X1(XC(I),SLOPE,-DIST,XW(I))   
            CALL Y1(YC(I),SLOPE,DIST,YW(I))  
            CALL X1(XC(I),SLOPE,DIST,XE(I))   
            CALL Y1(YC(I),SLOPE,-DIST,YE(I)) 
        ELSEIF(ANGLE>0.0 .AND. ANGLE < PI/2.)THEN   !OK
            SLOPE=TAN(ANGLE)
            CALL X1(XC(I),SLOPE,-DIST,XW(I))   
            CALL Y1(YC(I),SLOPE,-DIST,YW(I))
            CALL X1(XC(I),SLOPE,DIST,XE(I))   
            CALL Y1(YC(I),SLOPE,DIST,YE(I)) 
        ELSEIF(ANGLE == PI/2.)THEN
             XW(I)=XC(I)-DIST
             YW(I)=YC(I)
             XE(I)=XC(I)+DIST
             YE(I)=YC(I)
        ELSEIF(ANGLE == 3.*PI/2.)THEN
             XW(I)=XC(I)+DIST
             YW(I)=YC(I)
             XE(I)=XC(I)-DIST
             YE(I)=YC(I)
        ELSE
            WRITE(*,*)'Error - no case found for angle'
        ENDIF
      !  WRITE(201,'(I3,",",I3,",",F15.5,",",F15.5,",",I3,",",F10.3)')I,JB,XC(I),YC(I),UTMZONE(JW),EL(KB(I)+1,I)
   IF(NINTERNAL==1)THEN
        XE(I-1)=XW(I)   ! CHECK ORDER IF CORRECT...
        YE(I-1)=YW(I)
        XW(I+1)=XE(I)   ! CHECK ORDER IF CORRECT...THIS IS A TRICK TO USE THE OLD CODE TO DO THIS
        YW(I+1)=YE(I)    ! ?
   ELSE
        XE(I+1)=XW(I)   ! CHECK ORDER IF CORRECT...
        YE(I+1)=YW(I)
        XW(I-1)=XE(I)   ! CHECK ORDER IF CORRECT...THIS IS A TRICK TO USE THE OLD CODE TO DO THIS
        YW(I-1)=YE(I)    
   ENDIF
   
        
! define polygon corners NW AND SW AND NE AND SE

        DIST=B(KTWB(JW),I)/2
        ANGLE=PHI0(I)+PI/2.
        IF(ANGLE> 2.*PI)ANGLE=ANGLE-2.*PI
        IF(ANGLE> 3.*PI/2. .AND. ANGLE < 2.*PI)THEN
            SLOPE=TAN(2.*PI+ANGLE)     ! DX/DY    OK
            CALL X1(XW(I),SLOPE,DIST,XNW(I))                         
            CALL Y1(YW(I),SLOPE,DIST,YNW(I))
            CALL X1(XW(I),SLOPE,-DIST,XSW(I))   
            CALL Y1(YW(I),SLOPE,-DIST,YSW(I)) 
            
            CALL X1(XE(I),SLOPE,DIST,XNE(I))                         
            CALL Y1(YE(I),SLOPE,DIST,YNE(I))
            CALL X1(XE(I),SLOPE,-DIST,XSE(I))   
            CALL Y1(YE(I),SLOPE,-DIST,YSE(I))   
        ELSEIF(ANGLE==2.*PI .OR. ANGLE==0.0)THEN
            SLOPE=0.0
            CALL X1(XW(I),SLOPE,DIST,XNW(I))                         
            CALL Y1(YW(I),SLOPE,DIST,YNW(I))
            CALL X1(XW(I),SLOPE,-DIST,XSW(I))   
            CALL Y1(YW(I),SLOPE,-DIST,YSW(I))   
   
            CALL X1(XE(I),SLOPE,DIST,XNE(I))                         
            CALL Y1(YE(I),SLOPE,DIST,YNE(I))
            CALL X1(XE(I),SLOPE,-DIST,XSE(I))   
            CALL Y1(YE(I),SLOPE,-DIST,YSE(I)) 
        ELSEIF(ANGLE==PI)THEN
            SLOPE=0.0
           CALL X1(XW(I),SLOPE,DIST,XNW(I))                         
            CALL Y1(YW(I),SLOPE,DIST,YNW(I))
            CALL X1(XW(I),SLOPE,-DIST,XSW(I))   
            CALL Y1(YW(I),SLOPE,-DIST,YSW(I))   
 
            CALL X1(XE(I),SLOPE,DIST,XNE(I))                         
            CALL Y1(YE(I),SLOPE,DIST,YNE(I))
            CALL X1(XE(I),SLOPE,-DIST,XSE(I))   
            CALL Y1(YE(I),SLOPE,-DIST,YSE(I)) 
        ELSEIF(ANGLE > PI .AND. ANGLE < 3.*PI/2.)THEN
            SLOPE=TAN(PI/2.-3.*PI/2.+ANGLE)     ! DX/DY
           CALL X1(XW(I),SLOPE,DIST,XNW(I))                         
            CALL Y1(YW(I),SLOPE,DIST,YNW(I))
            CALL X1(XW(I),SLOPE,-DIST,XSW(I))   
            CALL Y1(YW(I),SLOPE,-DIST,YSW(I))   

            CALL X1(XE(I),SLOPE,DIST,XNE(I))                         
            CALL Y1(YE(I),SLOPE,DIST,YNE(I))
            CALL X1(XE(I),SLOPE,-DIST,XSE(I))   
            CALL Y1(YE(I),SLOPE,-DIST,YSE(I)) 
        ELSEIF(ANGLE > PI/2. .AND. ANGLE < PI)THEN   ! OK
            SLOPE=TAN(PI-ANGLE)
           CALL X1(XW(I),SLOPE,DIST,XNW(I))                         
            CALL Y1(YW(I),SLOPE,DIST,YNW(I))
            CALL X1(XW(I),SLOPE,-DIST,XSW(I))   
            CALL Y1(YW(I),SLOPE,-DIST,YSW(I))  

            CALL X1(XE(I),SLOPE,DIST,XNE(I))                         
            CALL Y1(YE(I),SLOPE,DIST,YNE(I))
            CALL X1(XE(I),SLOPE,-DIST,XSE(I))   
            CALL Y1(YE(I),SLOPE,-DIST,YSE(I)) 
        ELSEIF(ANGLE>0.0 .AND. ANGLE < PI/2.)THEN   !OK
            SLOPE=TAN(ANGLE)
           CALL X1(XW(I),SLOPE,DIST,XNW(I))                         
            CALL Y1(YW(I),SLOPE,DIST,YNW(I))
            CALL X1(XW(I),SLOPE,-DIST,XSW(I))   
            CALL Y1(YW(I),SLOPE,-DIST,YSW(I))  
 
            CALL X1(XE(I),SLOPE,DIST,XNE(I))                         
            CALL Y1(YE(I),SLOPE,DIST,YNE(I))
            CALL X1(XE(I),SLOPE,-DIST,XSE(I))   
            CALL Y1(YE(I),SLOPE,-DIST,YSE(I)) 
        ELSEIF(ANGLE == PI/2.)THEN
             XNW(I)=XW(I)-DIST
             YNW(I)=YW(I)
             XSW(I)=XW(I)+DIST
             YSW(I)=YW(I)
             XNE(I)=XE(I)-DIST
             YNE(I)=YE(I)
             XSE(I)=XE(I)+DIST
             YSE(I)=YE(I)
        ELSEIF(ANGLE == 3.*PI/2.)THEN
            XNW(I)=XW(I)+DIST
             YNW(I)=YW(I)
             XSW(I)=XW(I)-DIST
             YSW(I)=YW(I)
             XNE(I)=XE(I)+DIST
             YNE(I)=YE(I)
             XSE(I)=XE(I)-DIST
             YSE(I)=YE(I)
        ELSE
            WRITE(*,*)'Error - no case found for angle'
        ENDIF

        WRITE(200,'(A,A,i3,A,i3)')'5,1','       Segment:',I,' Branch:',JB
        WRITE(200,'(F15.4,",",F15.4,",",I3,",",F10.3)')XNW(I),YNW(I),UTMZONE(JW)
        WRITE(200,'(F15.4,",",F15.4,",",I3,",",F10.3)')XNE(I),YNE(I),UTMZONE(JW)
        WRITE(200,'(F15.4,",",F15.4,",",I3,",",F10.3)')XSE(I),YSE(I),UTMZONE(JW)
        WRITE(200,'(F15.4,",",F15.4,",",I3,",",F10.3)')XSW(I),YSW(I),UTMZONE(JW)
        WRITE(200,'(F15.4,",",F15.4,",",I3,",",F10.3)')XNW(I),YNW(I),UTMZONE(JW)
        
!    ENDDO
   
     
    
	RETURN
	END SUBROUTINE DOIT2
      SUBROUTINE X1(XOLD,SLOPE,T,XNEW)   ! COMPUTE COORDINATES OF NEW POINT GIVEN OLD POINT, DISTANCE AND SLOPE
      REAL*8 :: XOLD,SLOPE,T,XNEW
      XNEW=(T/(SQRT(1+SLOPE**2)))*SLOPE+XOLD
      RETURN
      END
      SUBROUTINE Y1(YOLD,SLOPE,T,YNEW)
	  REAL*8 :: YOLD,SLOPE,T,YNEW
       YNEW=(T/(SQRT(1+SLOPE**2)))+YOLD
      RETURN
      END