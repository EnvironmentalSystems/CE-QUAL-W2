        !COMPILER-GENERATED INTERFACE MODULE: Wed Jul 21 10:02:25 2021
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE INPUT_ERROR__genmod
          INTERFACE 
            SUBROUTINE INPUT_ERROR(UNIT,ERR,AID,MESSAGE)
              INTEGER(KIND=4) :: UNIT
              INTEGER(KIND=4) :: ERR
              CHARACTER(LEN=8) :: AID
              CHARACTER(LEN=70) :: MESSAGE
            END SUBROUTINE INPUT_ERROR
          END INTERFACE 
        END MODULE INPUT_ERROR__genmod
