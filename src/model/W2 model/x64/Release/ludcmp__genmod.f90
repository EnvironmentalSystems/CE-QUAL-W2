        !COMPILER-GENERATED INTERFACE MODULE: Mon Jul 19 15:11:37 2021
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE LUDCMP__genmod
          INTERFACE 
            RECURSIVE SUBROUTINE LUDCMP(A,N,NP,INDX,D)
              INTEGER(KIND=4) :: NP
              REAL(KIND=8) :: A(NP,NP)
              INTEGER(KIND=4) :: N
              INTEGER(KIND=4) :: INDX(NP)
              REAL(KIND=8) :: D
            END SUBROUTINE LUDCMP
          END INTERFACE 
        END MODULE LUDCMP__genmod
