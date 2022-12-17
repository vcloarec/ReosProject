!                   ******************
                    SUBROUTINE COEFMAT
!                   ******************
!
     &(PERIAF,DT,M,AM,NPERIAF)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    ESTABLISHES THE COEFFICIENT MATRICE USED FOR SPECTRUM
!+                ANALYSIS. THE THEORY EMPLOYED HERE IS THE LEAST MEAN
!+                SQUARE ERROR METHOD.
!
!reference  "SIMULATION DES COURANTS DE MAREE EN MANCHE ET PROCHE ATLANTIQUE",
!+                       EDF REPORT, J. M. JANIN ET. AL., PP 27-28.
!
!history  CHUN WANG
!+        28/07/2006
!+        V5P7
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AM             |<->| (2NPERIAF*2NPERIAF) COEFFICIENT MATRIX,
!| DT             |-->| TIME INTERVAL.
!| M              |-->| NUMBER OF SAMPLING POINTS
!| NPERIAF        |-->| NUMBER OF WAVES
!| PERIAF         |-->| PERIOD OF WAVES.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,          INTENT(IN   ) :: NPERIAF,M
      DOUBLE PRECISION, INTENT(IN   ) :: DT,PERIAF(NPERIAF)
      DOUBLE PRECISION, INTENT(INOUT) :: AM(2*NPERIAF,2*NPERIAF)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!                        500>NPERIAF
      DOUBLE PRECISION W(500),PI
      DOUBLE PRECISION A,B,C,D,AA,BB
      INTEGER I,J
!
      INTRINSIC COS,SIN
!
!-----------------------------------------------------------------------
!
      PI = 4.D0 * ATAN( 1.D0 )
!
      DO I = 1, NPERIAF
        W(I)=2.D0*PI/PERIAF(I)
      ENDDO
!
      DO I = 1, NPERIAF
        DO J = 1, NPERIAF
          AA = (W(I)+W(J))*DT
          BB = (W(I)-W(J))*DT
          A = (-1.D0+COS(AA)+COS(M*AA)-COS((M+1)*AA))/(2-2*COS(AA))
          C = (SIN(AA)+SIN(M*AA)-SIN((M+1)*AA))/(2-2*COS(AA))
          IF(I.EQ.J) THEN
            B = M*1.D0
            D = 0.D0
          ELSE
            B = (-1+COS(BB)+COS(M*BB)-COS((M+1)*BB))/(2-2*COS(BB))
            D = (SIN(BB)+SIN(M*BB)-SIN((M+1)*BB))/(2-2*COS(BB))
          ENDIF
          AM(I,J) = (A+B)/(2*M)
          AM(I,NPERIAF+J) = (C-D)/(2*M)
          AM(I+NPERIAF,J) = (C+D)/(2*M)
          AM(I+NPERIAF,NPERIAF+J) = (B-A)/(2*M)
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
