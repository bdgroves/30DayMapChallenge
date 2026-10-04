      PROGRAM BIGCRK
C     ------------------------------------------------------------------
C     BIG CREEK ABOVE GROVELAND, CALIFORNIA.  A LINE PRINTER MAP.
C     #30DAYMAPCHALLENGE 2026, DAY 10, PROMPTING ONLY.
C
C     READS  GRID.DAT     NC NR DX DY, THEN NR ROWS OF ELEVATION (FT,
C                         -9999 OUTSIDE THE BASIN), THEN NR ROWS OF
C                         FLAGS (0 LAND, 1 STREAM, 2 GAGE).
C     WRITES PRINT.TXT    132 COLUMNS, ASA CARRIAGE CONTROL IN COLUMN
C                         1: '1' NEW PAGE, ' ' NEXT LINE, '+' PRINT
C                         OVER THE LAST LINE.  DARK CLASSES ARE MADE
C                         THE WAY SYMAP MADE THEM, BY OVERPRINTING.
C     ------------------------------------------------------------------
      INTEGER MXC, MXR, NCL, NPASS
      PARAMETER (MXC=130, MXR=100, NCL=8, NPASS=4)
      INTEGER NC, NR, I, J, K, P, IC, NIN, KNT(NCL), IE(MXC,MXR)
      INTEGER IFL(MXC,MXR), ICL(MXC,MXR), EMIN, EMAX, LO(NCL), HI(NCL)
      INTEGER IGR, JGR, LEFT, RMIN, RMAX
      REAL DX, DY, AREA, SUM, EMEAN, HYP, PCT, W
      CHARACTER*1 SYM(NCL,NPASS)
      CHARACTER*130 LINE
      LOGICAL ANY
C     SYMBOLS, LIGHT TO DARK.  A BLANK MEANS NOTHING ON THAT PASS.
      DATA (SYM(1,P),P=1,NPASS) /'.',' ',' ',' '/
      DATA (SYM(2,P),P=1,NPASS) /'-',' ',' ',' '/
      DATA (SYM(3,P),P=1,NPASS) /'+',' ',' ',' '/
      DATA (SYM(4,P),P=1,NPASS) /'=',' ',' ',' '/
      DATA (SYM(5,P),P=1,NPASS) /'X',' ',' ',' '/
      DATA (SYM(6,P),P=1,NPASS) /'O','X',' ',' '/
      DATA (SYM(7,P),P=1,NPASS) /'O','X','-',' '/
      DATA (SYM(8,P),P=1,NPASS) /'O','X','A','V'/
C
      OPEN (10, FILE='GRID.DAT', STATUS='OLD')
      READ (10,*) NC, NR, DX, DY
      IF (NC .GT. MXC .OR. NR .GT. MXR) STOP 'GRID TOO BIG'
      DO 10 J = 1, NR
        READ (10,*) (IE(I,J), I=1,NC)
   10 CONTINUE
      DO 20 J = 1, NR
        READ (10,*) (IFL(I,J), I=1,NC)
   20 CONTINUE
      CLOSE (10)
C
C     RANGE, MEAN AND AREA OF THE CELLS INSIDE THE BASIN
      EMIN = 99999
      EMAX = -99999
      NIN = 0
      SUM = 0.0
      DO 40 J = 1, NR
        DO 30 I = 1, NC
          IF (IE(I,J) .GT. -9000) THEN
            NIN = NIN + 1
            SUM = SUM + IE(I,J)
            EMIN = MIN(EMIN, IE(I,J))
            EMAX = MAX(EMAX, IE(I,J))
          END IF
   30   CONTINUE
   40 CONTINUE
      EMEAN = SUM / NIN
      AREA = NIN * DX * DY / 1.0E6
      HYP = (EMEAN - EMIN) / REAL(EMAX - EMIN)
C
C     EIGHT EQUAL CLASSES FROM A ROUND HUNDRED, EACH A MULTIPLE OF
C     25 FEET WIDE, SO THE TOP CLASS ENDS JUST ABOVE THE HIGHEST CELL
      RMIN = EMIN
      RMAX = EMAX
      EMIN = (EMIN / 100) * 100
      W = 25.0 * ((EMAX - EMIN + 25*NCL - 1) / (25*NCL))
      EMAX = EMIN + NINT(W) * NCL
      DO 50 K = 1, NCL
        LO(K) = EMIN + NINT((K-1) * W)
        HI(K) = EMIN + NINT(K * W)
        KNT(K) = 0
   50 CONTINUE
      IGR = 0
      JGR = 0
      DO 70 J = 1, NR
        DO 60 I = 1, NC
          ICL(I,J) = 0
          IF (IE(I,J) .GT. -9000) THEN
            K = INT((IE(I,J) - EMIN) / W) + 1
            IF (K .GT. NCL) K = NCL
            IF (K .LT. 1) K = 1
            ICL(I,J) = K
            KNT(K) = KNT(K) + 1
          END IF
          IF (IFL(I,J) .EQ. 2) THEN
            IGR = I
            JGR = J
          END IF
   60   CONTINUE
   70 CONTINUE
C
      OPEN (20, FILE='PRINT.TXT', STATUS='REPLACE')
      LEFT = (130 - NC) / 2
      WRITE (20,900)
      WRITE (20,901)
      WRITE (20,902) AREA, AREA / 2.58999
      WRITE (20,903) NINT(DX), NINT(DY)
      WRITE (20,904)
C
C     THE MAP.  EACH ROW IS PRINTED UP TO NPASS TIMES, OVERPRINTED.
      DO 120 J = 1, NR
        DO 110 P = 1, NPASS
          LINE = ' '
          ANY = .FALSE.
          DO 100 I = 1, NC
            IC = LEFT + I
            IF (ICL(I,J) .GT. 0) THEN
              IF (IFL(I,J) .EQ. 1) THEN
C               STREAMS PRINT AS GAPS: WHITE LINES THROUGH THE TONE
                CONTINUE
              ELSE IF (IFL(I,J) .EQ. 2) THEN
                IF (P .EQ. 1) LINE(IC:IC) = 'G'
                IF (P .EQ. 1) ANY = .TRUE.
              ELSE IF (SYM(ICL(I,J),P) .NE. ' ') THEN
                LINE(IC:IC) = SYM(ICL(I,J),P)
                ANY = .TRUE.
              END IF
            ELSE IF (P .EQ. 1 .AND. EDGE(I,J)) THEN
              LINE(IC:IC) = '*'
              ANY = .TRUE.
            END IF
  100     CONTINUE
          IF (P .EQ. 1) THEN
            WRITE (20,905) LINE
          ELSE IF (ANY) THEN
            WRITE (20,906) LINE
          END IF
  110   CONTINUE
  120 CONTINUE
C
C     THE LEGEND, SYMAP FASHION: EACH CLASS WITH ITS RANGE, ITS SYMBOL
C     PRINTED AS A SOLID BLOCK, AND ITS SHARE OF THE BASIN.
      WRITE (20,907)
      WRITE (20,908)
      DO 140 K = 1, NCL
        PCT = 100.0 * KNT(K) / NIN
        DO 130 P = 1, NPASS
          IF (P .EQ. 1) THEN
            WRITE (20,909) K, LO(K), HI(K), (SYM(K,1), I=1,10),
     &                     KNT(K), PCT
          ELSE IF (SYM(K,P) .NE. ' ') THEN
            WRITE (20,910) (SYM(K,P), I=1,10)
          END IF
  130   CONTINUE
  140 CONTINUE
      WRITE (20,911)
      WRITE (20,912) RMIN, RMAX, NINT(EMEAN), HYP
      WRITE (20,913) NIN
      WRITE (20,914)
      CLOSE (20)
      STOP
C
  900 FORMAT ('1', 30X, 'BIG CREEK ABOVE GROVELAND, CALIFORNIA')
  901 FORMAT (' ', 30X, 'USGS GAGE 11284400 . ELEVATION OF THE BASIN',
     &        ', FEET')
  902 FORMAT (' ', 30X, 'DRAINAGE AREA', F8.1, ' SQ KM', F8.1,
     &        ' SQ MI')
  903 FORMAT (' ', 30X, 'ONE CHARACTER =', I4, ' M ACROSS BY', I4,
     &        ' M DOWN.  NORTH IS UP.')
  904 FORMAT (' ')
  905 FORMAT (' ', A130)
  906 FORMAT ('+', A130)
  907 FORMAT (' ')
  908 FORMAT (' ', 20X, 'CLASS   FEET            SYMBOL       ',
     &        'CELLS   PCT OF AREA')
  909 FORMAT (' ', 20X, I3, I8, ' -', I6, 4X, 10A1, I10, F10.1)
  910 FORMAT ('+', 20X, 3X, 8X, 2X, 6X, 4X, 10A1)
  911 FORMAT (' ', 20X, 'G  THE GAGE   * THE BASIN EDGE   BLANK ',
     &        'LINES ARE STREAMS')
  912 FORMAT (' ', 20X, 'LOW', I6, '   HIGH', I6, '   MEAN', I6,
     &        '   HYPSOMETRIC INTEGRAL', F6.2)
  913 FORMAT (' ', 20X, I6, ' CELLS IN THE BASIN')
  914 FORMAT (' ', 20X, 'END OF JOB.  BIGCRK, GNU FORTRAN.')
C
      CONTAINS
C     A CELL OUTSIDE THE BASIN THAT TOUCHES ONE INSIDE IT
      LOGICAL FUNCTION EDGE(I, J)
      INTEGER I, J, II, JJ
      EDGE = .FALSE.
      DO 210 JJ = MAX(1,J-1), MIN(NR,J+1)
        DO 200 II = MAX(1,I-1), MIN(NC,I+1)
          IF (ICL(II,JJ) .GT. 0) EDGE = .TRUE.
  200   CONTINUE
  210 CONTINUE
      END FUNCTION
      END PROGRAM
