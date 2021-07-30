        !COMPILER-GENERATED INTERFACE MODULE: Mon Jul 19 15:11:38 2021
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE RTBIS__genmod
          INTERFACE 
            RECURSIVE FUNCTION RTBIS(X1,X2,XACC,U,Y,V)
              REAL(KIND=8), INTENT(IN) :: X1
              REAL(KIND=8), INTENT(IN) :: X2
              REAL(KIND=8), INTENT(IN) :: XACC
              REAL(KIND=8), INTENT(IN) :: U
              REAL(KIND=8), INTENT(IN) :: Y
              REAL(KIND=8), INTENT(IN) :: V
              REAL(KIND=8) :: RTBIS
            END FUNCTION RTBIS
          END INTERFACE 
        END MODULE RTBIS__genmod
