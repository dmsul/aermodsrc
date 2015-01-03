      SUBROUTINE EVLOOP
C***********************************************************************
C                 EVLOOP Module of AERMOD EVENT Option
C
C        PURPOSE: Controls Main Calculation Loop Through Events
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To remove mixed-mode math in calculation of
C                    IENDHR - 4/19/93
C
C        INPUTS:  Source, Receptor and Setup Options
C
C        OUTPUTS: Update Hourly Results
C
C        CALLED FROM:   MAIN
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: IEVYR
      DOUBLE PRECISION :: GRPAVE_Test
      LOGICAL FOPEN, L_FIRSTCALL

C     Variable Initializations
      MODNAM = 'EVLOOP'
      DATA L_FIRSTCALL/.TRUE./

      GRPAVE_Test = 0.0D0
      EOF   = .FALSE.
      FOPEN = .FALSE.

C     Flush HRVAL, AVEVAL, GRPAVE and GRPVAL    ---   CALL EV_FLUSH
      CALL EV_FLUSH

      DO WHILE (FULLDATE.LT.IEDATE .AND. .NOT.EOF)
C        Retrieve Hourly Meteorology Data for Current Day   ---   CALL MEREAD
         CALL MEREAD

C        Check for Hourly Emissions File
         INQUIRE (UNIT=IHREMI,OPENED=FOPEN)
         IF (FOPEN) THEN
C*          Retrieve Hourly Emissions from File for Current Day---   CALL HQREAD
C*          Set ILINE = 1 if L_FIRSTCALL for determining whether VOLUME and AREA 
C*          source inputs include hourly sigmas
            IF (L_FIRSTCALL) ILINE = 1
            CALL EV_HRQREAD(L_FIRSTCALL)
         END IF

         IF (L_BACKGRND) THEN
C-----      Extract BACKGRND concentrations, if available
            CALL BGREAD
         END IF

         IF (PVMRM .OR. OLM) THEN
C-----      Extract Ozone Data; L_FIRSTCALL used to initialize array
C           of O3 values used to apply minimum O3 for stable hours
            CALL O3READ
         END IF

C ---    Set L_FIRSTCALL to .F.
         L_FIRSTCALL = .FALSE.

C        Write Out Update to the Screen for the PC Version
         WRITE(*,909) JDAY, IYR
 909     FORMAT('+','Now Processing Events For Day No. ',I4,' of ',I4)

         IF (IPROC(JDAY).EQ.1 .AND. .NOT.RUNERR) THEN
C           Begin The Event Loop
            DO IEVENT = 1, NUMEVE

C              Calculate year of event for multiple year data files
               IEVYR = INT(EVDATE(IEVENT)/1000000)
               IF (EVJDAY(IEVENT) .EQ. JDAY .AND.
     &                      IEVYR .EQ. IYEAR) THEN

                  IENDHR = EVDATE(IEVENT) -
     &                 INT(EVDATE(IEVENT)/100)*100
                  ISTAHR = IENDHR - EVAPER(IEVENT) + 1
                 
C                 Begin Hourly LOOP
                  DO IHOUR = ISTAHR, IENDHR
C ---                Assign IHOUR to KHOUR, used for profile met data
                     KHOUR = IHOUR
C ---                Assign O3MISS logical for this hour
                     O3MISS = L_AO3MISS(IHOUR)
C                    Retrieve Hourly Data for Current Event ---   CALL METEXT
                     CALL EV_METEXT
C                    Retrieve Hourly Ozone Value
                     IF (PVMRM .OR. OLM) THEN
                        O3CONC = EV_O3CONC(IHOUR)
                     END IF
C*                   Process Hourly Emissions from File, if needed
                     IF (HOURLY) THEN
C*                      Begin Source Loop
                        DO ISRC = 1, NUMSRC
                          IF (QFLAG(ISRC) .EQ. 'HOURLY') THEN
C*                           Retrieve Source Parameters for This Hour  ---   CALL HRQEXT
                             CALL HRQEXT(ISRC)
                          END IF
                        END DO
C*                      End Source Loop
                     END IF
C*----
                     IF (CLMHR .AND. CLMPRO) THEN
C                       Check for Calm Hr & Processing and
C                       Increment Counters
                        EV_NUMHRS = EV_NUMHRS + 1
                        EV_NUMCLM = EV_NUMCLM + 1
                     ELSE IF (MSGHR .AND. MSGPRO) THEN
C                       Check for Missing Hour & Processing and
C                       Increment Counters
                        EV_NUMHRS = EV_NUMHRS + 1
                        EV_NUMMSG = EV_NUMMSG + 1
                     ELSE IF (ZI .LE. 0.0D0) THEN
C                       Write Out The Informational Message &
C                       Increment Counters
                        WRITE(DUMMY,'(I8.8)') KURDAT
                        CALL ERRHDL(PATH,MODNAM,'I','470',DUMMY)
                        EV_NUMHRS = EV_NUMHRS + 1
                     ELSE
C                       Set CALCS Flag, Increment Counters
C                       & Calculate HRVAL
                        CALCS = .TRUE.
                        EV_NUMHRS = EV_NUMHRS + 1
C                       Calculate CONC or DEPOS Values      ---   CALL EVCALC
                        CALL EVCALC
                     END IF

                     IF (PVMRM .AND. .NOT.O3MISS .AND.          ! jop 9/30/06
     &                               .NOT.CLMHR  .AND. 
     &                               .NOT.MSGHR  .AND.
     &                                           .NOT.PSDCREDIT) THEN
C ---                   Process Hourly Values for PVMRM Option
                        CALL PVMRM_CALC('ALLSRCS')
               
                     ELSE IF (PVMRM .AND. .NOT.O3MISS .AND.   ! jop 9/30/06
     &                                    .NOT.CLMHR  .AND. 
     &                                    .NOT.MSGHR  .AND. 
     &                                                PSDCREDIT) THEN
C ---                   Process Hourly Values for PVMRM Option and PSD credits
C ---                   Need to process two separate sets of sources - the
C                       increment consumption sources ('NAAQSRC') and the 
C                       increment expanding sources ('ALLBASE')
                        CALL PVMRM_CALC('NAAQSRC')
                        CALL PVMRM_CALC('ALLBASE')

                     ELSE IF (OLM .AND. .NOT.O3MISS 
     &                            .AND. .NOT.CLMHR  .AND. 
     &                                              .NOT.MSGHR) THEN
C ---                   Process Hourly Values for OLM Option
                        CALL OLM_CALC

                     ELSE IF (ARM2 .AND. .NOT.CLMHR .AND. 
     &                                              .NOT.MSGHR) THEN
C ---                   Process Hourly Values for ARM2 Option
                        CALL ARM2_CALC

                     ELSE IF (ARM  .AND. .NOT.CLMHR .AND. 
     &                                              .NOT.MSGHR) THEN
C ---                   Process Hourly Values for ARM Option
                        CALL ARM_CALC

                     END IF

                  END DO
C                 End Hourly LOOP

C                 Calculate Applicable Averages             ---   CALL AVEREV
                  CALL AVEREV

C                 Print Out Model Results                   ---   CALL OUTPUT
                  CALL EV_OUTPUT

C ---             Compare calculated EVENT concentration (GRPAVE) to "original"   
C                 EVENT concentration included on EVENTPER keyword (EV_OrigConc)
                  IF( EV_OrigConc(IEVENT) .GT. 0.0D0 ) THEN
C ---                Since "original" EVENT concentration is read from input file
C                    with 5 decimal places, first round the internal results to 
C                    5 decimal places to avoid spurious messages if the original
C                    concentration is less than 10.0.
                     IF( GRPAVE(IDXEV(IEVENT)) .LT. 10.0D0 )THEN
                        GRPAVE_Test = DBLE(IDNINT(1.0D5 * 
     &                                GRPAVE(IDXEV(IEVENT))))/
     &                                1.0D5
                     ELSE
C ---                   Use original value for comparison
                        GRPAVE_Test = GRPAVE(IDXEV(IEVENT))
                     END IF

                     IF( DABS((EV_OrigConc(IEVENT)-GRPAVE_Test)/
     &                         EV_OrigConc(IEVENT)) .GT. 2.0D-6 )THEN
C                      WRITE Warning Message
                       CALL ERRHDL(PATH,MODNAM,'W','497',EVNAME(IEVENT))
                     END IF
                  END IF

C                 Flush HRVAL, AVEVAL, GRPAVE and GRPVAL    ---   CALL EV_FLUSH
                  CALL EV_FLUSH

C                 Reset CALCS Flag
                  CALCS = .FALSE.

C                 Reset the Counters
                  EV_NUMHRS = 0
                  EV_NUMCLM = 0
                  EV_NUMMSG = 0

               END IF   ! IF-ENDIF block on events for this JDAY

            END DO
C           End Event LOOP

         END IF
      END DO
C     End Loop Through Meteorology Data

      RETURN
      END

      SUBROUTINE MEREAD
C***********************************************************************
C                MEREAD Module of the AMS/EPA Regulatory Model - AERMOD - EVENT
C
C        PURPOSE: Controls Extraction and Quality Assurance of
C                 One Day of Meteorological Data for EVENT Processing
C
C        PROGRAMMER: ROGER BRODE, JEFF WANG
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  Modified met data arrays to include an additional
C                   array index, since these arrays are also used by 
C                   the OU MAXDCONT post-processing option.
C                   R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
C
C        MODIFIED:  Modified code for processing multi-year data
C                   files to determine if header record is present
C                   between years for concatenated files.  Use presence
C                   of colon (':') as only criterion for header record.
C                   Use warning messages if UAIR and SURF IDs don't
C                   match input runstream file for multiple years since
C                   AERMOD allows mismatch for single year files.
C                   Modified check for stable or missing hours in 
C                   calculation of solar irradiance (QSW) for use
C                   in deposition calculations.
C                   Modified to check first hour of met data files
C                   to determine if file starts on hour 01. If not,
C                   cycle through hour loop until loop index matches
C                   hour in data file.
C                   R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C        MODIFIED:  Modified code for reading the header record of the
C                   surface file to use a character variable for the
C                   AERMET version date field, in order to allow for
C                   the future use of screening meteorology that is not
C                   directly linked to a specific version under the
C                   of the AERMET processor under the SCREEN option.
C                   R.W. Brode, U.S. EPA/OAQPS/AQMG, 01/26/2007
C
C        MODIFIED:  To assign non-array logicals STABLE and UNSTAB
C                   for use in subroutine COMPTG.
C                   R.W. Brode, U.S. EPA/OAQPS/AQMG, 01/24/2007
C
C        MODIFIED:  To remove support for unformatted meteorological
C                   data files.
C                   R.W. Brode, PES, Inc., 4/10/2000
C
C        MODIFIED:  To incorporate modifications to date processing
C                   for Y2K compliance, including use of date window
C                   variables (ISTRT_WIND and ISTRT_CENT) and calculation
C                   of 10-digit date variable (FULLDATE) with 4-digit
C                   year for date comparisons.
C                   Also modified calls to METDAT insteaad of EV_METDAT
C                   to allow use of same routine for both normal and
C                   EVENT processing.
C                   R.W. Brode, PES, Inc., 5/12/99
C
C        INPUTS:  Meteorology File Specifications
C
C        OUTPUTS: Arrays of Meteorological Variables for One Day
C
C        CALLED FROM:   EVLOOP
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C---- Constants used in the computation of QSW
      DOUBLE PRECISION, PARAMETER :: C1=5.31D-13, C2=60.0D0, C3=1.12D0,
     &                               STEFB= 5.67D-08
      DOUBLE PRECISION :: RN, Es25, FVREF
      
      INTEGER :: I, IHR, IJDAY, IDATCHK, IUSI, ISSI,
     &           JFLAG, LEVEL
     
      CHARACTER (LEN=8)   :: CUSI, CSSI, COSI
      CHARACTER (LEN=6)   :: SPEC1, SPEC2, SPEC3
      CHARACTER (LEN=256) :: BUFFER

C     Variable Initializations
      MODNAM = 'MEREAD'
      PATH   = 'MX'

C     READ Meteorology Data Based on Format --
C     When DRY deposition is modeled, U-star, L, and z0 (surface
C     roughness length) are read in addition to the standard 
C     data.  
C
C     When WET deposition is modeled, ipcode (precip.
C     code) and prate (precip. rate in mm/hr) must also be added to
C     each hourly record.
C     The format statement allows for all additional data:

C     Calculate the MMDDHH variable to check for end of the year
      IDATCHK = KURDAT - INT(KURDAT/1000000)*1000000
      IF ((IMONTH.EQ.12 .AND. IDAY.EQ.31 .AND. IHOUR.EQ.24) .OR.
     &    IDATCHK .EQ. 123124) THEN
C        End of year has been reached - check for presence of header
C        record at beginning of next year for multi-year data files.
         READ(MFUNIT,'(A256)',ERR=998,END=1000,IOSTAT=IOERRN) BUFFER

C ---    First check for ':' as indicator of header record, then extract 
C        AERMET version date, C_METVER, and station IDs
         IF (INDEX(BUFFER,':') .EQ. 0) THEN
C           Record does not contain colon. Assume it must be regular
C           met data record, so backspace met file before proceeding.
            BACKSPACE MFUNIT
         ELSE
C           Record contains colons. Assume it is a header record, and
C           extract AERMET version date, C_METVER, and check station 
C           IDs before proceeding in order to flag potential for use 
C           of different stations in multi-year data files.
            IF( INDEX(BUFFER,'VERSION:') .NE. 0 )THEN
C              Extract AERMET version date from embedded header record
               READ(BUFFER(INDEX(BUFFER,'VERSION:')+8:
     &                     INDEX(BUFFER,'VERSION:')+13),'(A6)')
     &                                                 C_METVER
            ELSEIF( BUFFER(93:98) .NE. '      ' )THEN
C              The 'VERSION:' keyword is missing from header so assign columns 
C              93-98 to C_METVER
               C_METVER = BUFFER(93:98)
            ELSE
C              AERMET version not found in header record, issue fatal error message
               CALL ERRHDL(PATH,MODNAM,'E','395','No Version')      
            ENDIF

C ---       Read Lat/Lon from header record BUFFER
            READ(BUFFER,1900,ERR=99,IOSTAT=IOERRN) ALAT, ALON
 1900       FORMAT(2A10)

C ---       Now extract UA, SF, and OS station IDs from header record
            IF( INDEX(BUFFER,'UA_ID:') .GE. 0 )THEN
               READ(BUFFER(INDEX(BUFFER,'UA_ID:')+7:
     &                     INDEX(BUFFER,'UA_ID:')+15),'(A)') CUSI
            ELSE
               CUSI = '        '
            END IF
            CALL STONUM(CUSI,8,FNUM,IMIT)
            IF (IMIT .EQ. 1) THEN
               IUSI = NINT(FNUM)
            ELSE
               IUSI = 0
            END IF

            IF( INDEX(BUFFER,'SF_ID:') .GE. 0 )THEN
               READ(BUFFER(INDEX(BUFFER,'SF_ID:')+7:
     &                     INDEX(BUFFER,'SF_ID:')+15),'(A)') CSSI
            ELSE
               CSSI = '        '
            END IF
            CALL STONUM(CSSI,8,FNUM,IMIT)
            IF (IMIT .EQ. 1) THEN
               ISSI = NINT(FNUM)
            ELSE
               ISSI = 0
            END IF

            IF (ISSI .NE. IDSURF) THEN
C              Write Warning Message:  SURFDATA id mismatch
               CALL ERRHDL(PATH,MODNAM,'W','530','SURFDATA')
            END IF
            IF (IUSI .NE. IDUAIR) THEN
C              Write Warning Message:  UAIRDATA id mismatch
               CALL ERRHDL(PATH,MODNAM,'W','530','UAIRDATA')
            END IF
         END IF

         GO TO 1001

C        Error reading 'header record' - assume that header record is
C        missing.  Backspace met file and continue processing.
 998     BACKSPACE MFUNIT

      END IF

1001  CONTINUE

      HOUR_LOOP: DO IHR = 1, NHR
C
C---- READ surface scaling meteorology data based on format
C
      IF( LDPART .OR. LWPART .OR. LDGAS .OR. LWGAS )THEN
C        Read record from ASCII scalar parameter file using FREE format
C        with deposition variables
C
C ---    First read date variables to check for problems
         READ( MFUNIT, *, END=1000, ERR=99, IOSTAT=IOERRN ) IYEAR,
     &         IMONTH, IDAY, IJDAY, IHOUR
C       
         IF (IHOUR .EQ. IHR) THEN
C ---       Data file hour matches loop hour; backspace and read full record
            BACKSPACE MFUNIT
            READ( MFUNIT, *, END=1000, ERR=99, IOSTAT=IOERRN ) IYEAR,
     &       IMONTH, IDAY, IJDAY, IHOUR, ASFCHF(IHR,1), AUSTAR(IHR,1),
     &       AWSTAR(IHR,1),AVPTGZI(IHR,1),AZICONV(IHR,1),AZIMECH(IHR,1),
     &       AOBULEN(IHR,1), ASFCZ0(IHR,1),ABOWEN(IHR,1),AALBEDO(IHR,1),
     &       AUREF(IHR,1), AWDREF(IHR,1), AUREFHT(IHR,1), ATA(IHR,1),
     &       ATREFHT(IHR,1), IAPCODE(IHR,1), APRATE(IHR,1), ARH(IHR,1),
     &       ASFCP(IHR,1), NACLOUD(IHR,1)
         ELSE IF (IHOUR .GT. IHR) THEN
C ---       Data file starts after hour 01; 
C           Issue warning, backspace file and skip to Profile file
            WRITE(DUMMY,'(2X,3I2)',ERR=99) IMONTH, IDAY, IHR
            CALL ERRHDL(PATH,MODNAM,'W','489',DUMMY)
            BACKSPACE MFUNIT
            GO TO 888
         ELSE
C ---       Data file hour is less than loop hour;
C           could be problem with data file or use of 00-23 hour convention
C           Issue error message:
            WRITE(DUMMY,'(2X,3I2)',ERR=99) IMONTH, IDAY, IHOUR
            CALL ERRHDL(PATH,MODNAM,'E','490',DUMMY)
            EXIT HOUR_LOOP
         END IF

C        Calculate solar irradiance, QSW, from Heat Flux, Bowen ratio,
C        albedo and cloud cover, for use in gas deposition algorithm.
C        Include check for ABOWEN < 0 for non-standard inputs.
         IF (AOBULEN(IHR,1).GT.0.0D0 .OR. AOBULEN(IHR,1).LT.-99990.0D0
     &      .OR. ATA(IHR,1).LT.0.0D0 .OR. 
     &       AALBEDO(IHR,1).EQ.1.0D0 .OR. ABOWEN(IHR,1).LE.0.0D0) THEN
C           Hour is stable or missing or inappropriate surface chars.
            AQSW(IHR,1) = 0.0D0
         ELSE
            RN = (1.D0 + 1.D0/ABOWEN(IHR,1))*ASFCHF(IHR,1)/0.9D0
            AQSW(IHR,1) = (RN*(1.D0+C3)-C1*ATA(IHR,1)**6+
     &                     STEFB*ATA(IHR,1)**4 -
     &                    C2*0.1D0*DBLE(NACLOUD(IHR,1))) / 
     &                   (1.D0-AALBEDO(IHR,1))
         END IF
C
C        Save precipitation rates for two previous hours
         IF (IHR .EQ. 1) THEN
            Aprec2(IHR,1) = APrate(NHR-1,1)
            Aprec1(IHR,1) = APrate(NHR,1)
         ELSE IF (IHR .EQ. 2) THEN
            Aprec2(IHR,1) = APrate(NHR,1)
            Aprec1(IHR,1) = APrate(IHR-1,1)
         ELSE
            Aprec2(IHR,1) = APrate(IHR-2,1)
            Aprec1(IHR,1) = APrate(IHR-1,1)
         END IF

C        Set variables for dry deposition
         IF (LDPART .OR. LDGAS) THEN
            IF (ATA(IHR,1).LT.0.0D0 .OR. APRATE(IHR,1).LT.0.0D0) THEN
               AWNEW(IHR,1) = AWOLD(IHR,1)
            ELSE
c ...          Compute saturation vapor pressure based on CMAQ formula
               AEsTa(IHR,1) = 0.6112D0*DEXP(19.83D0 - 
     &                        5417.4D0/ATA(IHR,1))
               Es25 = 3.167D0
               AWnew(IHR,1) = Wold+APrec1(IHR,1)-
     &                       0.5D0*f2*AEsTa(IHR,1)/Es25
               Wold = AWnew(IHR,1)
               Af2(IHR,1) = AWnew(IHR,1)/200.D0
               if (Af2(IHR,1).le.0.01D0) Af2(IHR,1) = 0.01D0
               if (Af2(IHR,1).gt.1.0D0) Af2(IHR,1) = 1.0D0
               f2 = Af2(IHR,1)
            END IF
         END IF

      ELSE
C        Read record from ASCII scalar parameter file without deposition
C        parameters, using FREE format
C
C ---    First read date variables to check for problems
         READ( MFUNIT, *, END=1000, ERR=99, IOSTAT=IOERRN ) IYEAR,
     &         IMONTH, IDAY, IJDAY, IHOUR
C
         IF (IHOUR .EQ. IHR) THEN
C ---       Data file hour matches loop hour; backspace and read full record
            BACKSPACE MFUNIT
            READ( MFUNIT, *, END=1000, ERR=99, IOSTAT=IOERRN ) IYEAR,
     &         IMONTH, IDAY, IJDAY, IHOUR, ASFCHF(IHR,1), AUSTAR(IHR,1),
     &         AWSTAR(IHR,1), AVPTGZI(IHR,1), AZICONV(IHR,1), 
     &         AZIMECH(IHR,1), AOBULEN(IHR,1), ASFCZ0(IHR,1), 
     &         ABOWEN(IHR,1), AALBEDO(IHR,1), AUREF(IHR,1), 
     &         AWDREF(IHR,1), AUREFHT(IHR,1), ATA(IHR,1),
     &         ATREFHT(IHR,1)
         ELSE IF (IHOUR .GT. IHR) THEN
C ---       Data file starts after hour 01; 
C           Issue warning, backspace file and skip to Profile file
            WRITE(DUMMY,'(2X,3I2)',ERR=99) IMONTH, IDAY, IHR
            CALL ERRHDL(PATH,MODNAM,'W','489',DUMMY)
            BACKSPACE MFUNIT
            GO TO 888
         ELSE
C ---       Data file hour is less than loop hour;
C           could be problem with data file or use of 00-23 hour convention
C           Issue error message:
            WRITE(DUMMY,'(2X,3I2)',ERR=99) IMONTH, IDAY, IHOUR
            CALL ERRHDL(PATH,MODNAM,'E','490',DUMMY)
            EXIT HOUR_LOOP
         END IF
C
      END IF

C     Set the stability logical variables
      IF( AOBULEN(IHR,1) .GT. 0.0D0 ) THEN
         AUNSTAB(IHR,1) = .FALSE.
         ASTABLE(IHR,1) = .TRUE.
C        Also set non-array variables for use in COMPTG
         UNSTAB = .FALSE.
         STABLE = .TRUE.
      ELSE
         AUNSTAB(IHR,1) = .TRUE.
         ASTABLE(IHR,1) = .FALSE.
C        Also set non-array variables for use in COMPTG
         UNSTAB = .TRUE.
         STABLE = .FALSE.
      END IF

C --- Assign Sector IDs by hour for sector-varying BACKGRND if needed
      IF (L_Backgrnd) THEN
         IF (AWDREF(IHR,1) .LE. 0.0D0 .OR. 
     &       AWDREF(IHR,1) .GT. 360.0D0) THEN
C ---       Hour is calm or missing; set ABGSECT = 0
            ABGSECT(IHR) = 0
         ELSE
C ---       Valid wind direction is available
C ---       Assign sector ID for direction-varying BACKGRND
            FVREF = AWDREF(IHR,1) + 180.0D0
            IF (FVREF .GT. 360.0D0) THEN
               FVREF = FVREF - 360.0D0
            END IF
            IF (L_BGSector) THEN
               IF (FVREF .LT. BGSECT(1) .OR. 
     &             FVREF .GE. BGSECT(NUMBGSects) ) THEN
                  ABGSECT(IHR) = NUMBGSects
               ELSE
                  DO I = 1, NUMBGSects-1
                     IF (FVREF .GE. BGSECT(I) .AND. 
     &                   FVREF .LT. BGSECT(I+1)) THEN
                        ABGSECT(IHR) = I
                        EXIT
                     END IF
                  END DO
               END IF
            ELSE
               ABGSECT(IHR) = 1
            END IF
         END IF
      END IF

C --- Assign Sector IDs by hour for direction-varying background O3 if needed
      IF (L_O3SECTOR) THEN
         IF (AWDREF(IHR,1) .LE. 0.0D0 .OR. 
     &       AWDREF(IHR,1) .GT. 360.0D0) THEN
C ---       Hour is calm or missing; set AO3SECT = 0
            AO3SECT(IHR) = 0
         ELSE
C ---       Valid wind direction is available
C ---       Assign sector ID for direction-varying background O3
            FVREF = AWDREF(IHR,1) + 180.0D0
            IF (FVREF .GT. 360.0D0) THEN
               FVREF = FVREF - 360.0D0
            END IF
            IF (L_O3Sector) THEN
               IF (FVREF .LT. O3SECT(1) .OR. 
     &             FVREF .GE. O3SECT(NUMO3Sects) ) THEN
                  AO3SECT(IHR) = NUMO3Sects
               ELSE
                  DO I = 1, NUMO3Sects-1
                     IF (FVREF .GE. O3SECT(I) .AND. 
     &                   FVREF .LT. O3SECT(I+1)) THEN
                        AO3SECT(IHR) = I
                        EXIT
                     END IF
                  END DO
               END IF
            ELSE
               AO3SECT(IHR) = 1
            END IF
         END IF
      ELSE
C ---    No O3SECTORs; assign 1 to AO3SECT array
         AO3SECT(IHR) = 1
      END IF

C---- Initialize the profile data to missing;
C     READ profile data based on format
C

C --- Branch here if surface data file starts after hour 01
C
 888  CONTINUE
 
      CALL PFLINI ()
      LEVEL = 1
      JFLAG = 0
C     Read record from ASCII profile file using FREE format; compute
C     sigma_V from sigma_A and wind speed
C --- First read date variables to check for problems
      READ( MPUNIT, *, END=1000, ERR=98, IOSTAT=IOERRN ) KYEAR,
     &         KMONTH, KDAY, KHOUR
C       
      IF (KHOUR .EQ. IHR) THEN
C ---    Data file hour matches loop hour; backspace and read full record
         BACKSPACE MPUNIT

      DO WHILE( JFLAG .EQ. 0 )
         READ( MPUNIT, *, END=1000, ERR=98, IOSTAT=IOERRN ) KYEAR,
     &       KMONTH, KDAY, KHOUR, PFLHT(LEVEL), JFLAG,
     &       PFLWD(LEVEL), PFLWS(LEVEL), PFLTA(LEVEL),
     &       PFLSA(LEVEL), PFLSW(LEVEL)

C        Convert the data to the required units
         CALL PFLCNV (LEVEL)

C        Set the number of profile levels to current index, store
C        the 'top of profile' flag, and increment level if not at top
C        Check that the level does not exceed the maximum allowable
         NPLVLS = LEVEL
         ANPLVLS(IHR,1) = LEVEL
         AIFLAG(IHR,LEVEL,1) = JFLAG
         APFLHT(IHR,LEVEL,1) = PFLHT(LEVEL)
         APFLWD(IHR,LEVEL,1) = PFLWD(LEVEL)
         APFLWS(IHR,LEVEL,1) = PFLWS(LEVEL)
         APFLTA(IHR,LEVEL,1) = PFLTA(LEVEL)
         APFLSA(IHR,LEVEL,1) = PFLSA(LEVEL)
         APFLSV(IHR,LEVEL,1) = PFLSV(LEVEL)
         APFLSW(IHR,LEVEL,1) = PFLSW(LEVEL)
         IF( JFLAG .EQ. 0 )THEN
            LEVEL = LEVEL + 1

            IF( LEVEL .GT. MXPLVL )THEN
               IF( .NOT. PFLERR )THEN
C                 WRITE Error Message: Number of profile levels
C                                      exceeds maximum allowable
                  WRITE(DUMMY,'(I8)') MXPLVL
                  CALL ERRHDL(PATH,MODNAM,'E','465',DUMMY)
                  PFLERR = .TRUE.
                  RUNERR = .TRUE.
               END IF

C              Limit the number of levels to the maximum allowable
               LEVEL = MXPLVL
            END IF

         END IF

      END DO

      ELSE IF (KHOUR .GT. IHR) THEN
C ---    Data file starts after hour 01; 
C        Backspace file and cycle hour loop
         BACKSPACE MPUNIT
         CYCLE HOUR_LOOP

      ELSE
C ---    Data file hour is less than loop hour;
C        could be problem with data file or use of 00-23 hour convention
C        Issue error message:
         WRITE(DUMMY,'(2X,3I2)',ERR=99) IMONTH, IDAY, IHOUR
         CALL ERRHDL(PATH,MODNAM,'E','489',DUMMY)
         EXIT HOUR_LOOP
      END IF

C     Compute the vertical potential temperature gradient profile
      IF( .NOT. RUNERR ) THEN
         NTGLVL = 0
         CALL COMPTG ()
         ANTGLVL(IHR,1) = NTGLVL
         DO I = 1, NTGLVL
           APFLTG(IHR,I,1)  = PFLTG(I)
           APFLTGZ(IHR,I,1) = PFLTGZ(I)
         END DO
      END IF

      END DO HOUR_LOOP

C     Set the date variables
      CALL SET_DATES

      GO TO 999

C     WRITE Error Messages:  Error Reading Met Data File

 98   CALL ERRHDL(PATH,MODNAM,'E','510','PROFFILE')
      RUNERR = .TRUE.
      GO TO 999

 99   CALL ERRHDL(PATH,MODNAM,'E','510','SURFFILE')
      RUNERR = .TRUE.
      GO TO 999

 1000 EOF = .TRUE.
 
C     Set the date variables
      CALL SET_DATES

 999  RETURN
      END

      SUBROUTINE EV_METEXT
C***********************************************************************
C                EV_METEXT Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Controls Extraction and Quality Assurance of
C                 One Hour of Meteorological Data
C
C        PROGRAMMER: ROGER BRODE, JEFF WANG
C        MODIFIED BY D. Strimaitis, SRC (for Wet & Dry DEPOSITION)
C
C        DATE:    November 8, 1993
C
C        MODIFIED:   Modified met data arrays to include an additional
C                    array index, since these arrays are also used by 
C                    the OU MAXDCONT post-processing option.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
C
C        MODIFIED:   To remove unused data array (NDAY).
C                    R.W. Brode, PES, Inc., 4/10/2000
C
C        MODIFIED:   To incorporate modifications to date processing
C                    for Y2K compliance, including use of date window
C                    variables (ISTRT_WIND and ISTRT_CENT) and calculation
C                    of 10-digit date variable (FULLDATE) with 4-digit
C                    year for date comparisons.
C                    R.W. Brode, PES, Inc., 5/12/99
C
C        MODIFIED:   To add determination of season index (ISEAS).
C                    R.W. Brode, PES, Inc. - 12/2/98
C
C        MODIFIED BY D. Strimaitis, SRC (for Dry DEPOSITION)
C        (DATE:    February 15, 1993)
C
C        MODIFIED:   To avoid potential math error due to negative
C                    ambient temperatures in calculating the square
C                    root of the stability parameter, RTOFS - 4/19/93
C
C        MODIFIED:
C        7/27/94     J. Paumier, PES, Inc.
C                    The variables for displacement height, ZDM and
C                    AZDM(), were removed from the input to and output
C                    from ISC-COMPDEP.  The following format statements
C                    also were affected: 9009, 9026, 9032, 9033
C
C*       7/27/94     J. Hardikar, PES, Inc.
C*                   Added code to calculate reference wind speed at 10m
C*                   to be used for OPENPIT source algorithms
C
C        INPUTS:  Meteorology File Specifications
C
C        OUTPUTS: Meteorological Variables for One Hour
C
C        CALLED FROM:   EVLOOP
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      INTEGER I

C     Variable Initializations
      MODNAM = 'EV_METEXT'
      PATH   = 'MX'

C     Save Value of Last YR/MN/DY/HR and Previous Hour
      IPDATE = KURDAT
      IPHOUR = IHOUR

C     Set Meteorological Variables for This Hour
      SFCHF  = ASFCHF(IHOUR,1)
      UREF   = AUREF(IHOUR,1)
      UREFHT = AUREFHT(IHOUR,1)
      TA     = ATA(IHOUR,1)
      TREFHT = ATREFHT(IHOUR,1)
      WDREF  = AWDREF(IHOUR,1)
      USTAR  = AUSTAR(IHOUR,1)
      WSTAR  = AWSTAR(IHOUR,1)
      ZICONV = AZICONV(IHOUR,1)
      ZIMECH = AZIMECH(IHOUR,1)
      OBULEN = AOBULEN(IHOUR,1)
      VPTGZI = AVPTGZI(IHOUR,1)
      SFCZ0  = ASFCZ0(IHOUR,1)
      BOWEN  = ABOWEN(IHOUR,1)
      ALBEDO = AALBEDO(IHOUR,1)
      IPCODE = IAPCODE(IHOUR,1)
      PRATE  = APRATE(IHOUR,1)
      RH     = ARH(IHOUR,1)
      SFCP   = ASFCP(IHOUR,1)
      NCLOUD = NACLOUD(IHOUR,1)
      QSW    = AQSW(IHOUR,1)
      Wnew   = AWnew(IHOUR,1)
      f2     = Af2(IHOUR,1)
      EsTa   = AEsTa(IHOUR,1)
      Prec1  = APrec1(IHOUR,1)
      Prec2  = APrec2(IHOUR,1)

      NPLVLS = ANPLVLS(IHOUR,1)

      IFLAG(1:NPLVLS) = AIFLAG(IHOUR,1:NPLVLS,1)
      PFLHT(1:NPLVLS) = APFLHT(IHOUR,1:NPLVLS,1)
      PFLWD(1:NPLVLS) = APFLWD(IHOUR,1:NPLVLS,1)
      PFLWS(1:NPLVLS) = APFLWS(IHOUR,1:NPLVLS,1)
      PFLTA(1:NPLVLS) = APFLTA(IHOUR,1:NPLVLS,1)
      PFLSA(1:NPLVLS) = APFLSA(IHOUR,1:NPLVLS,1)
      PFLSV(1:NPLVLS) = APFLSV(IHOUR,1:NPLVLS,1)
      PFLSW(1:NPLVLS) = APFLSW(IHOUR,1:NPLVLS,1)

      NTGLVL = ANTGLVL(IHOUR,1)

      PFLTG(1:NTGLVL)  = APFLTG(IHOUR,1:NTGLVL,1)
      PFLTGZ(1:NTGLVL) = APFLTGZ(IHOUR,1:NTGLVL,1)

C     Set Meteorological Variables for Current Hour
      CALL SET_METDATA

 999  RETURN
      END

      SUBROUTINE EV_HRQREAD(L_FIRSTCALL)
C***********************************************************************
C*                  EV_HQREAD Module of AERMOD
C* 
C*         PURPOSE: To Read a 24-hour Block of Hourly Emissions Data
C* 
C*         PROGRAMMER:  Jayant Hardikar, Roger Brode
C* 
C*         DATE:    September 15, 1993
C* 
C*         INPUTS:  Variable QFLAG and Current Source Number Being Processed
C* 
C*         OUTPUTS: Source Arrays
C*          
C*         MODIFIED:  REMOVED THE 'POINT' SOURCE CONDITION, SO IT APPLIES 
C*                    TO ALL SOURCE TYPES, EXCEPT SAVING THE TEMP & VEL
C* 
C*         CALLED FROM:  EVLOOP
C************************************************************************
C*
C*    Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: IS, IHR
      INTEGER :: ILSAVE
      LOGICAL :: L_FIRSTCALL
      LOGICAL :: EOF_SAVE

C*    Variable Initializations
      MODNAM = 'EV_HRQREAD'
C*    Save current value of EOF from MEREAD
      EOF_SAVE = EOF
C*    Reinitialize EOF = .F. for HRQREAD
      EOF = .FALSE.
      
      ILSAVE = ILINE

      HOUR_LOOP: DO IHR = 1, NHR
         IQLINE = IQLINE + 1
         SOURCE_LOOP: DO IS = 1, NUMSRC
            IF (QFLAG(IS) .EQ. 'HOURLY') THEN

               IF (L_FIRSTCALL) ILINE = 1
C ---          Assign ILINE = 1 for first call to HRQREAD

               CALL HRQREAD (IS)

               IF (.NOT.EOF .AND. IHR .EQ. NHR) THEN
C*                Check for Date and Time Consistency with Met Data; 
C*                If Failed, Issue Fatal Error
                  IF (FULLDATE .NE. FULLHRQ) THEN
C*                   WRITE Error Message - Date mismatch
                     WRITE(DUMMY,'(I10.10)') FULLDATE
                     CALL ERRHDL(PATH,MODNAM,'E','455',DUMMY)
                     RUNERR = .TRUE.
                     EXIT HOUR_LOOP
                  END IF
               ELSE IF (EOF) THEN
C ---             EOF reached in HRQREAD; reassign EOF based on MEREAD
C                 Exit hour loop to avoid read error in HRQREAD
                  EOF = EOF_SAVE
                  EXIT HOUR_LOOP
               END IF

               EV_HRQS(IS,IHR) = HRQS

               IF (SRCTYP(IS)(1:5) .EQ. 'POINT') THEN
                  EV_HRTS(IS,IHR) = HRTS
                  EV_HRVS(IS,IHR) = HRVS
               ELSE IF (SRCTYP(IS) .EQ. 'VOLUME' .AND. 
     &                                           L_HRLYSIG(IS)) THEN
                  EV_HRHS(IS,IHR) = HRHS
                  EV_HRSY(IS,IHR) = HRSY
                  EV_HRSZ(IS,IHR) = HRSZ
               ELSE IF ((SRCTYP(IS)(1:4) .EQ. 'AREA' .OR. 
     &                        SRCTYP(IS) .EQ. 'LINE') .AND. 
     &                                           L_HRLYSIG(IS)) THEN
                  EV_HRHS(IS,IHR) = HRHS
                  EV_HRSZ(IS,IHR) = HRSZ
               ELSE IF (SRCTYP(IS) .EQ. 'OPENPIT') THEN
                  EV_HRTS(IS,IHR) = HRTS
               END IF

            END IF
         END DO SOURCE_LOOP
      END DO HOUR_LOOP

      RETURN
      END

      SUBROUTINE EVCALC
C***********************************************************************
C                 EVCALC Module of AERMOD EVENT Option
C
C        PURPOSE: Controls Flow and Processing of CALCulation Modules
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  Modified to include user-specified background
C                   concentrations through the SO BACKGRND option.
C                   R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
C
C        MODIFIED:  To set NUMREC = 1 and use PCALC, VCALC, ACALC, and
C                   OCALC subroutines.  R.W. Brode, PES, Inc. - 12/2/98
C
C        INPUTS:  Arrays of Source Parameters
C                 Arrays of Receptor Locations
C                 Meteorological Variables for One Hour
C
C        OUTPUTS: Array of 1-hr CONC or DEPOS Values for Each Source/Receptor
C
C        CALLED FROM:   EVLOOP
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE

      DOUBLE PRECISION :: BCKGRD

      CHARACTER MODNAM*12
      
C     Variable Initializations
      MODNAM = 'EVCALC'
      PATH   = 'CN'

C     Set NUMREC = 1 to allow use of PCALC, VCALC, ACALC, and OCALC subroutines
      NUMREC = 1
C --- Assign IGRP for this event
      IGRP = IDXEV(IEVENT)

C     Begin Source LOOP
      SOURCE_LOOP: DO ISRC = 1, NUMSRC
         IF (IGROUP(ISRC,IGRP) .EQ. 1) THEN
            IF (SRCTYP(ISRC)(1:5) .EQ. 'POINT') THEN
C              Calculate Point Source Values                        ---   CALL PCALC
               CALL PCALC
            ELSE IF (SRCTYP(ISRC) .EQ. 'VOLUME') THEN
C              Calculate Volume Source Values                       ---   CALL VCALC
               CALL VCALC
            ELSE IF (SRCTYP(ISRC)(1:4) .EQ. 'AREA' .OR.
     &               SRCTYP(ISRC) .EQ. 'LINE') THEN
C              Calculate AREA/AREAPOLY/AREACIRC/LINE Source Values  ---   CALL ACALC
               CALL ACALC
            ELSE IF (SRCTYP(ISRC) .EQ. 'OPENPIT') THEN
C              Calculate OpenPit Source Values                      ---   CALL OCALC
               CALL OCALC
            END IF
         END IF
      END DO SOURCE_LOOP
C     End Source LOOP

      IF (L_BACKGRND .AND. .NOT.ARM2 .AND. .NOT.ARM) THEN
C ---    User-specified background concentrations are included; 
C        add to modeled concentrations by source group. 
C        Note that EV_SUMBACK is not called for ARM and ARM2
C        options since ARM/ARM2 ratios are based on group ALL
C        without background; this is done in sub ARM_CALC and
C        sub ARM2_CALC
         CALL EV_SUMBACK
      END IF
      
      RETURN
      END

      SUBROUTINE EV_SUMVAL
C***********************************************************************
C                 EV_SUMVAL Module of the AMS/EPA Regulatory Model - AERMOD - EVENT
C
C        PURPOSE: Sums HRVAL to AVEVAL and ANNVAL Arrays
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  Modified to include GRPAVE variable to account for
C                   user-specified background for the full averaging
C                   period based user-specified background concentrations 
C                   through the SO BACKGRND option.
C                   R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
C
C        INPUTS:  HRVAL - Hourly Value for (IHOUR,ISRC) Combination
C                 Averaging Period Options
C                 Source Groupings
C
C        OUTPUTS: Updated Sums of AVEVAL and ANNVAL Arrays
C
C        CALLED FROM:   PCALC
C                       VCALC
C                       ACALC
C                       OCALC
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'EV_SUMVAL'

      HRVALS(IHOUR,ISRC) = HRVAL(1)
      EV_AVEVAL(ISRC)    = EV_AVEVAL(ISRC) + HRVAL(1)
      GRPAVE(IGRP)       = GRPAVE(IGRP) + HRVAL(1)
      GRPVAL(IDXEV(IEVENT),IHOUR) = 
     &      GRPVAL(IDXEV(IEVENT),IHOUR) + HRVAL(1)

      RETURN
      END

      SUBROUTINE EV_SUMBACK
C***********************************************************************
C                 EV_SUMBACK Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Sums Background Values to AVEVAL and ANNVAL Arrays
C
C        PROGRAMMER: Roger Brode
C
C        DATE:       February 28, 2011
C
C        INPUTS:  
C
C        OUTPUTS: Updated Sums of AVEVAL and ANNVAL Arrays
C
C        CALLED FROM:   PCALC
C                       VCALC
C                       ACALC
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      DOUBLE PRECISION :: BCKGRD
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'EV_SUMBACK'

      BCKGRD = EV_BGCONC(IHOUR)
      IF (GRP_BACK(IGRP)) THEN
         GRPVAL(IGRP,IHOUR) = GRPVAL(IGRP,IHOUR) + BCKGRD
         BACKHR(IGRP,IHOUR) = BACKHR(IGRP,IHOUR) + BCKGRD
         GRPAVE(IGRP)  = GRPAVE(IGRP)  + BCKGRD
         BACKAVE(IGRP) = BACKAVE(IGRP) + BCKGRD
      END IF

      RETURN
      END

      SUBROUTINE O3READ
C***********************************************************************
C*                  O3READ Module of AERMOD
C* 
C*         PURPOSE: To Read a 24-hour Block of Ozone Data
C* 
C*         PROGRAMMER:  Roger Brode
C* 
C*         DATE:    October 17, 2005
C* 
C          MODIFIED:  Modified to assign EV_O3CONC(IHR) value based on 
C                     the O3BACK variable from CO OZONEVAL keyword when 
C                     no background hourly ozone file or CO O3VALUES inputs
C                     are available.
C                     R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/29/2012
C
C          MODIFIED:  Modified to use background ozone values input
C                     through the CO O3VALUES option to substitute for 
C                     missing hourly ozonce concentrations.
C                     R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
C
C*         INPUTS:
C* 
C*         OUTPUTS:
C*          
C*         CALLED FROM:
C************************************************************************
C*
C*    Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      DOUBLE PRECISION :: O3MIN, O3MAX24, O3DUMMY
      DOUBLE PRECISION :: O3SUB(6)
      DOUBLE PRECISION :: EV_O3TEMP(24)
      INTEGER :: I, J, IHR, IO3YR, IO3MN, IO3DY, IO3HR, IO3YR2
      INTEGER :: FULLO3HR(6)

C*    Variable Initializations
      MODNAM  = 'O3READ'

C --- Initialize full date variable for all sectors to 0
      FULLO3HR(:)  = 0
C --- Initialize O3SUB substitution value to 0
      O3SUB(:)     = 0.0D0
      EV_O3TEMP(:) = 0.0D0
C --- Initialize O3MISS logical array to FALSE for all hours
      L_AO3MISS(:) = .FALSE.

C --- Loop through the current day
      DO IHR = 1, 24

C ---    Initialize EV_O3CONC to 0.0
         EV_O3CONC(IHR) =   0.0D0
         EV_O3TEMP(IHR) = -99.0D0
         O3SUB(:) = -99.0D0
         O3MIN    = -99.0D0

C ---    Assign local IHR index to global IHOUR index; since this
C        may be used to identify temporally-varying O3 values
         IHOUR = IHR
         
C ---    Assign O3SECT array value to scalar variable
         IO3SECT = AO3SECT(IHR)

         DO I = 1, NUMO3Sects
C ---       Loop through O3SECTORs

            O3SUB(I) = -99.0D0

C ---       Check for temporally-varying ozone concentrations from O3VALUES
C           keyword; used to fill in for missing hourly data.
            IF (L_O3VALUES(I)) THEN
               CALL OZONVALS(I,O3SUB(I))
            ELSE IF (L_O3VAL(I)) THEN
               O3SUB(I) = O3BACK(I)
            ELSE
               O3SUB(I) = 0.0D0
            END IF

            IF (L_O3Hourly) THEN
C ---          Hourly O3 data is available; read and process the data

               IF (L_O3FILE(I)) THEN
C ---             Hourly O3 file available for current sector

                  IF (I .EQ. IO3SECT) THEN
C ---                This is the applicable sector for this hour; read next hour of O3 data

                     IF (O3FORM(I) .EQ. 'FREE') THEN
                        READ(IO3UNT(I),*,ERR=99,END=999) IO3YR, IO3MN,
     &                                                   IO3DY, IO3HR,
     &                                                   EV_O3CONC(IHR)
                     ELSE
                        READ(IO3UNT(I),O3FORM(I),ERR=99,END=999)
     &                                                   IO3YR, IO3MN,
     &                                                   IO3DY, IO3HR, 
     &                                                   EV_O3CONC(IHR)
                     END IF

C ---                Determine 4-digit year
                     IF (IO3YR .LE. 99) THEN
                        IO3YR2 = IO3YR
                        IF (IO3YR2 .GE. ISTRT_WIND .AND. 
     &                                       IO3YR2 .LE. 99) THEN
                           IO3YR  = ISTRT_CENT*100 + IO3YR2
                        ELSE IF (IO3YR2 .LT. ISTRT_WIND) THEN
                           IO3YR  = (ISTRT_CENT+1)*100 + IO3YR2
                        END IF
                     END IF
                   
C ---                Calculate full date for this hour of O3 data
                     FULLO3HR(I) = IO3YR*1000000 + IO3MN*10000 + 
     &                                               IO3DY*100 + IO3HR

                     IF (EV_O3CONC(IHR) .GT. 0.0D0 .AND. 
     &                   EV_O3CONC(IHR) .LT. 900.0D0) THEN
C ---                   Valid hourly value; convert to ug/m3 if needed
                        IF (O3FILUNITS .EQ. 'PPB') THEN
                           EV_O3CONC(IHR) = EV_O3CONC(IHR) * O3_PPB
                        ELSE IF (O3FILUNITS .EQ. 'PPM') then
                           EV_O3CONC(IHR) = EV_O3CONC(IHR) * O3_PPB
                        END IF
                        IF (ASTABLE(IHR,1)) THEN
C                          Use min of 40 ppb (78.4ug/m3) and max from previous 24 hrs
                           O3MAX24 = MIN ( 78.40D0, 
     &                             MAXVAL( O3_Max24hr(:,AO3SECT(IHR)) ))
C                          Adjust minimum O3 value based on OBULEN
                           IF (AOBULEN(IHR,1).GT.0.0D0 .AND. 
     &                         AOBULEN(IHR,1).LE.50.0D0) THEN
                              O3MIN = O3MAX24
                           ELSE IF (AOBULEN(IHR,1) .GT. 250.0D0) THEN
                              O3MIN = 0.0D0
                           ELSE
                              O3MIN = O3MAX24 * (250.D0-AOBULEN(IHR,1))/
     &                                           200.D0
                           END IF
                        ELSE
                           O3MIN = -9.0D0
                        END IF
C ---                   Save this hour's O3CONC to array of previous 
C                       24 values, before applying minimum value
                        O3_Max24hr(IHR,IO3SECT) = EV_O3CONC(IHR)
                        EV_O3CONC(IHR) = MAX( EV_O3CONC(IHR), O3MIN )
                     ELSE IF (L_O3VALUES(IO3SECT) .OR.
     &                           L_O3VAL(IO3SECT)) THEN
C ---                   Hourly O3 values is missing; assign O3VALS value;
C                       these have already been converted to ug/m3
                        EV_O3CONC(IHR) = O3SUB(IO3SECT)
C ---                   Assign 0.0 to O3_Max24hr array
                        O3_Max24hr(IHR,IO3SECT) = 0.0D0
                        IF (.NOT. L_SkipMessages) THEN
                           WRITE(DUMMY,'(I10.10)') FULLO3HR(I)
                           CALL ERRHDL(PATH,MODNAM,'I','458',DUMMY)
                        END IF
                     ELSE
C ---                   Assign L_AO3MISS logical to TRUE for this hour
                        L_AO3MISS(IHR) = .TRUE.
C ---                   Assign 0.0 to O3_Max24hr array
                        O3_Max24hr(IHR,IO3SECT) = 0.0D0
                        IF (.NOT. L_SkipMessages) THEN
                           WRITE(DUMMY,'(I10.10)') FULLO3HR(I)
                           CALL ERRHDL(PATH,MODNAM,'I','459',DUMMY)
                        END IF
                     END IF

                  ELSE
C ---                This is not applicable sector for this hour, or hour is calm/missing; 
C                    however, read O3 values to keep track of 24hr max value for this sector
                     IF (O3FORM(I) .EQ. 'FREE') THEN
                       READ(IO3UNT(I),*,ERR=99,END=999) IO3YR, IO3MN,
     &                                                  IO3DY, IO3HR,
     &                                                  EV_O3TEMP(IHR)
                     ELSE
                       READ(IO3UNT(I),O3FORM(I),ERR=99,END=999)
     &                                                  IO3YR, IO3MN,
     &                                                  IO3DY, IO3HR,
     &                                                  EV_O3TEMP(IHR)
                     END IF

C ---                Determine 4-digit year
                     IF (IO3YR .LE. 99) THEN
                        IO3YR2 = IO3YR
                        IF (IO3YR2 .GE. ISTRT_WIND .AND. 
     &                                       IO3YR2 .LE. 99) THEN
                           IO3YR  = ISTRT_CENT*100 + IO3YR2
                        ELSE IF (IO3YR2 .LT. ISTRT_WIND) THEN
                           IO3YR  = (ISTRT_CENT+1)*100 + IO3YR2
                        END IF
                     END IF
                   
C ---                Calculate full date for this hour of O3 data
                     FULLO3HR(I) = IO3YR*1000000 + IO3MN*10000 + 
     &                                               IO3DY*100 + IO3HR

                     IF (EV_O3TEMP(IHR) .GT. 0.0D0 .AND. 
     &                   EV_O3TEMP(IHR) .LT. 900.0D0) THEN
C ---                   Valid hourly value; convert to ug/m3 if needed
                        IF (O3FILUNITS .EQ. 'PPB') THEN
                           EV_O3TEMP(IHR) = EV_O3TEMP(IHR) * O3_PPB
                        ELSE IF (O3FILUNITS .EQ. 'PPM') then
                           EV_O3TEMP(IHR) = EV_O3TEMP(IHR) * O3_PPB
                        END IF
C ---                   Save this hour's O3CONC to array of previous 
C                       24 values
                        O3_Max24hr(IHR,I) = EV_O3TEMP(IHR)
                     ELSE IF (L_O3VALUES(I) .OR.
     &                           L_O3VAL(I)) THEN
C ---                   Hourly O3 values is missing; assign O3VALS value;
C                       these have already been converted to ug/m3
                        EV_O3TEMP(IHR) = O3SUB(I)
C ---                   Assign 0.0 to O3_Max24hr array
                        O3_Max24hr(IHR,I) = 0.0D0
                        IF (.NOT. L_SkipMessages) THEN
                           WRITE(DUMMY,'(I10.10)') FULLO3HR(I)
                           CALL ERRHDL(PATH,MODNAM,'I','458',DUMMY)
                        END IF
                     ELSE
C ---                   Assign 0.0 to O3_Max24hr array for full conversion
                        O3_Max24hr(IHR,I) = 0.0D0
                        IF (.NOT. L_SkipMessages) THEN
                           WRITE(DUMMY,'(I10.10)') FULLO3HR(I)
                           CALL ERRHDL(PATH,MODNAM,'I','459',DUMMY)
                        END IF
                     END IF
                  END IF

               END IF   ! IF-THEN block for reading hourly O3FILEs

           ELSE
C ---         No hourly O3 data available; apply O3SUB based on non-hourly data
C             if this is the applicable sector
              IF (I .EQ. IO3SECT) THEN
                 EV_O3CONC(IHR) = O3SUB(I)
              END IF

           END IF

         END DO      ! END of Sector Loop

         IF (L_AO3MISS(IHR)) THEN
C ---       No O3 value available for this hour; assign 0.0 to EV_O3CONC 
C           for full conversion and issue informational message
            EV_O3CONC(IHR) = 0.0D0
            IF (.NOT. L_SkipMessages) THEN
               WRITE(DUMMY,'(I10.10)') 100*(FULLDATE/100)+IHR
               CALL ERRHDL(PATH,MODNAM,'I','459',DUMMY)
            END IF
         END IF
                    
      END DO     ! Hour of Hour Loop

      DO I = 1, NUMO3Sects
C ---    Loop through O3SECTORs
         IF (FULLO3HR(I) .GT. 0) THEN
C*          Recalculate full date with last value of IO3HR (should be = 24) for 
C*          comparison to FULLDATE, since FULLDATE is set by MEREAD based on a 
C*          loop through one day of meteorological data and reflects HR 24.
C*          Check for Date and Time Consistency ; If Failed, Issue Fatal Error
            IF (FULLDATE .NE. FULLO3HR(I)) THEN
C*             WRITE Error Message - Date mismatch
               WRITE(DUMMY,'(I10.10)') FULLO3HR(I)
               CALL ERRHDL(PATH,MODNAM,'E','457',DUMMY)
               RUNERR = .TRUE.
            END IF
         END IF
      END DO

      GO TO 1000

C*    Write Error Message for Error Reading Hourly Ozone File
 99   CONTINUE
      WRITE(DUMMY,'(''O3FILE SECT'',I1)') DUMMY
      CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)
      RUNERR = .TRUE.

 999  CONTINUE
 
C --- End of file reached on O3 file

1000  RETURN
      END

      SUBROUTINE BGREAD
C***********************************************************************
C*                  BGREAD Module of AERMOD
C* 
C*         PURPOSE: To Read a 24-hour Block of Background Data
C* 
C*         PROGRAMMER:  Roger Brode
C* 
C*         DATE:    February 28, 2011
C* 
C*         MODIFIED:   Modified subroutine BGREAD to move the unit conversion for 
C*                     hourly background concentrations to follow the READ statements 
C*                     to avoid "double counting" unit conversion for non-hourly 
C*                     background since unit conversion for BGSUB has already been 
C*                     applied in sub_BGVAL.
C*                     R.W. Brode, U.S. EPA/OAQPS/AQMG, XX/YY/2013
C*         
C*         INPUTS:
C* 
C*         OUTPUTS:
C*          
C*         CALLED FROM:
C************************************************************************
C*
C*    Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: NDAY(12), ISEA_NDX(12)
      INTEGER :: I, IA, IY, IM, ID, J, NL, NUMSW
      DOUBLE PRECISION :: BGVALUES(24), BGSUB(6), BGDUMMY
      INTEGER :: IHR, IBGYR, IBGMN, IBGDY, IBGHR, IBGYR2
      INTEGER :: FULLBGHR(6)

C     Variable Initializations
      DATA NDAY/31,59,90,120,151,181,212,243,273,304,334,365/
      DATA ISEA_NDX/1,1,2,2,2,3,3,3,4,4,4,1/
      
      MODNAM  = 'BGREAD'
      FULLBGHR(:) = 0
      BGSUB(:)    = -99.0D0

      DO IHR = 1, 24

C ---    Initialize EV_BGCONC to missing
         EV_BGCONC(IHR) = -99.0D0

C ---    Assign local IHR index to global IHOUR index; since this
C        may be used to identify temporally-varying BG values
         IHOUR = IHR
         
C ---    Assign BGSECT array value to scalar variable
         IBGSECT = ABGSECT(IHR)

         DO I = 1, NUMBGSects

C ---       Reinitialize BGSUB for this sector
            BGSUB(I) = 0.0D0

C ---       Check for temporally-varying background to substitute for missing hours
            IF (L_BGValues(I)) THEN
               CALL BGVAL(I,BGSUB(I))
            ELSE
               BGSUB(I) = 0.0D0
            END IF
            
            IF (L_BGFile(I)) THEN
C ---          Hourly BACKGRND data file available

               IF (I .EQ. IBGSECT) THEN
C ---             This is the applicable sector; read hourly BGCONC

C*                Retrieve hourly background concentrations      
                  IF (BGFORM(I) .EQ. 'FREE') THEN
                     READ(IBGUNT(I),*,ERR=99,END=999) IBGYR, IBGMN, 
     &                                                IBGDY, IBGHR, 
     &                                                EV_BGCONC(IHR)
                  ELSE
                     READ(IBGUNT(I),BGFORM(I),ERR=99,END=999) 
     &                                                IBGYR, IBGMN,
     &                                                IBGDY, IBGHR,
     &                                                EV_BGCONC(IHR)
                  END IF

C ---             Adjust background concentration units to UG/M3 if needed 
C                 for hourly background; unit conversion for BGSUB is 
C                 applied in subroutine BGVAL;
C                 conversion is based on reference temperature (25C) and
C                 pressure (1013.25 mb)
                  IF (EV_BGCONC(IHR) .GE. 0.0D0) THEN
                     IF (POLLUT .EQ. 'NO2') THEN
                        IF (BackUnits .EQ. 'PPB') THEN
                           EV_BGCONC(IHR) = EV_BGCONC(IHR) / NO2_PPB
                        ELSE IF (BackUnits .EQ. 'PPM') THEN
                           EV_BGCONC(IHR) = EV_BGCONC(IHR) / NO2_PPM
                        END IF
                     ELSE IF (POLLUT .EQ. 'SO2') THEN
                        IF (BackUnits .EQ. 'PPB') THEN
                           EV_BGCONC(IHR) = EV_BGCONC(IHR) / SO2_PPB
                        ELSE IF (BackUnits .EQ. 'PPM') THEN
                           EV_BGCONC(IHR) = EV_BGCONC(IHR) / SO2_PPM
                        END IF
                     ELSE IF (POLLUT .EQ. 'CO') THEN
                        IF (BackUnits .EQ. 'PPB') THEN
                           EV_BGCONC(IHR) = EV_BGCONC(IHR) * CO_PPB
                        ELSE IF (BackUnits .EQ. 'PPM') THEN
                           EV_BGCONC(IHR) = EV_BGCONC(IHR) * CO_PPM
                        END IF
                     END IF
                  END IF

C ---             Check for use of 2-digit year in background file, adjust to 4-digit
C                 year for comparison with FULLDATE based on met data file
                  IF (IBGYR .LE. 99) THEN
                     IBGYR2 = IBGYR
                     IF (IBGYR2 .GE. ISTRT_WIND .AND. 
     &                                          IBGYR2 .LE. 99) THEN
                        IBGYR  = ISTRT_CENT*100 + IBGYR2
                     ELSE IF (IBGYR2 .LT. ISTRT_WIND) THEN
                        IBGYR  = (ISTRT_CENT+1)*100 + IBGYR2
                     END IF
                  END IF
              
C ---             Calculate full date for this hour of BACKGRND data
                  FULLBGHR(I) = IBGYR*1000000 + IBGMN*10000 + IBGDY*100 
     &                                                      + IBGHR
      
               ELSE
C ---             This is not applicable sector for this hour; read record without data

                  IF (BGFORM(I) .EQ. 'FREE') THEN
                     READ(IBGUNT(I),*,ERR=99,END=999) IBGYR, IBGMN, 
     &                                                IBGDY, IBGHR 
                  ELSE
                     READ(IBGUNT(I),BGFORM(I),ERR=99,END=999) 
     &                                                IBGYR, IBGMN,
     &                                                IBGDY, IBGHR
                  END IF
              
C ---             Check for use of 2-digit year in background file, adjust to 4-digit
C                 year for comparison with FULLDATE based on met data file
                  IF (IBGYR .LE. 99) THEN
                     IBGYR2 = IBGYR
                     IF (IBGYR2 .GE. ISTRT_WIND .AND. 
     &                                          IBGYR2 .LE. 99) THEN
                        IBGYR  = ISTRT_CENT*100 + IBGYR2
                     ELSE IF (IBGYR2 .LT. ISTRT_WIND) THEN
                        IBGYR  = (ISTRT_CENT+1)*100 + IBGYR2
                     END IF
                  END IF

C ---             Calculate full date for this hour of BACKGRND data
                  FULLBGHR(I) = IBGYR*1000000 + IBGMN*10000 + IBGDY*100 
     &                                                      + IBGHR

               END IF

            END IF

         END DO    ! NUMBGSects Loop

         IF (EV_BGCONC(IHR) .LT. 0.0D0) THEN
C ---       Hourly BGCONC is missing; look for substitution values
            IF (IBGSECT .GT. 0) THEN
C ---          Valid BGSECT defined, check for hourly values for this 
C              sector, and then for non-hourly values to substitute
               IF (L_BGFile(IBGSECT)) THEN
                  IF (L_BGValues(IBGSECT)) THEN
C                    Hourly background value is missing but non-hourly 
C                    values have been specified for substitution, 
C                    which were processed in subroutine BGVAL;
                     EV_BGCONC(IHR) = BGSUB(IBGSECT)
C                    Write informational message 
                     WRITE(DUMMY,'(I10.10)') 100*(FULLDATE/100)+IHR
                     CALL ERRHDL(PATH,MODNAM,'I','453',DUMMY)
C ---                Increment counter for number of missing BGval substitutions
                     NSubBGHOUR = NSubBGHOUR + 1
                  ELSE
C                    Hourly background value is missing for this sector and no 
C                    non-hourly values specified for substitution;
C                    Write Error message 
                     WRITE(DUMMY,'(I10.10)') 100*(FULLDATE/100)+IHR
                     CALL ERRHDL(PATH,MODNAM,'E','452',DUMMY)
                     RUNERR = .TRUE.
                     GO TO 1000
                  END IF
               ELSE
                  IF (L_BGValues(IBGSECT)) THEN
C                    Hourly background value is missing but non-hourly 
C                    values have been specified for substitution, 
C                    which were processed in subroutine BGVAL;
                     EV_BGCONC(IHR) = BGSUB(IBGSECT)
                  END IF
               END IF
            ELSE
C ---          IBGSECT .LE. 0 due to calm/msg hr; Set EV_BGCONC to 0 and exit
               EV_BGCONC(IHR) = 0.0D0
            END IF
         END IF
               
      END DO    ! Hour Loop

C*    Check for Date and Time Consistency Across all Sectors; If Failed, Issue Fatal Error
      DO I = 1, NUMBGSects
         IF (L_BGFile(I)) THEN
C*          Recalculate full date with last value of IBGHR (should be = 24) for 
C*          comparison to FULLDATE, since FULLDATE is set by MEREAD based on a 
C*          loop through one day of meteorological data.
C*          Check for Date and Time Consistency ; If Failed, Issue Fatal Error
            FULLBGHR(I) = IBGYR*1000000 + IBGMN*10000 + IBGDY*100 +IBGHR
            IF (FULLDATE .NE. FULLBGHR(I)) THEN
C*             WRITE Error Message - Date mismatch
               WRITE(DUMMY,'(I10.10)') FULLBGHR(I)
               CALL ERRHDL(PATH,MODNAM,'E','454',DUMMY)
               RUNERR = .TRUE.
            END IF
         END IF
      END DO

      GO TO 1000

C*    Write Error Message for Error Reading Hourly BACKGRND File
 99   CONTINUE
      WRITE(DUMMY,'(''BGFILE SECT'',I1)') IBGSECT
      CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)
      RUNERR = .TRUE.
      GO TO 1000

 999  CONTINUE
 
1000  RETURN
      END
