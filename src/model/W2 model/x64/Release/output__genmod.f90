        !COMPILER-GENERATED INTERFACE MODULE: Mon Jul 19 15:13:11 2021
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE OUTPUT__genmod
          INTERFACE 
            RECURSIVE SUBROUTINE OUTPUT(JDAY,IUPR,IDPR,KBR,ISNP,BL,NBL)
              USE GLOBAL
              REAL(KIND=8) :: JDAY
              INTEGER(KIND=4) :: IUPR
              INTEGER(KIND=4) :: IDPR
              INTEGER(KIND=4) :: KBR
              INTEGER(KIND=4) :: ISNP(IMX,NWB)
              INTEGER(KIND=4) :: BL(IMX)
              INTEGER(KIND=4) :: NBL
            END SUBROUTINE OUTPUT
          END INTERFACE 
        END MODULE OUTPUT__genmod
