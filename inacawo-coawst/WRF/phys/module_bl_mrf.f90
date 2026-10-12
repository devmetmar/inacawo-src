































MODULE module_bl_mrf

    PUBLIC      MRF, MRFINIT
    
    PRIVATE     

CONTAINS

    SUBROUTINE MRF( U3D,V3D,TH3D,T3D,QV3D,QC3D,P3D,PI3D,             &
                    RUBLTEN,RVBLTEN,RTHBLTEN,                        &
                    RQVBLTEN,RQCBLTEN,                               &
                    CP,G,ROVCP,R,ROVG,                               &
                    dz8w,z,XLV,RV,PSFC,                              &
                    p1000mb,                                         &
                    ZNT,UST,ZOL,HOL,PBL,PSIM,PSIH,                   &
                    XLAND,HFX,QFX,TSK,GZ1OZ0,WSPD,BR,                &
                    DT,DTMIN,KPBL2D,                                 &
                    SVP1,SVP2,SVP3,SVPT0,EP1,EP2,KARMAN,EOMEG,STBOLT,&
                    flag_qi,                                         &
                    ids,ide, jds,jde, kds,kde,                       &
                    ims,ime, jms,jme, kms,kme,                       &
                    its,ite, jts,jte, kts,kte,                       &
                    QI3D,RQIBLTEN,                                   &      
                    regime                                           )

        IMPLICIT NONE

        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        

        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        

        

        INTEGER,  INTENT(IN   )   ::      ids,ide, jds,jde, kds,kde, &
                                          ims,ime, jms,jme, kms,kme, &
                                          its,ite, jts,jte, kts,kte


        REAL,     INTENT(IN   )   ::      P1000mb
        REAL,     INTENT(IN   )   ::      DT,DTMIN,CP,G,ROVCP,       &
                                          ROVG,R,XLV,RV

        REAL,     INTENT(IN )     ::     SVP1,SVP2,SVP3,SVPT0
        REAL,     INTENT(IN )     ::     EP1,EP2,KARMAN,EOMEG,STBOLT

        REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
                  INTENT(IN   )   ::                           QV3D, &
                                                               QC3D, &
                                                                P3D, &
                                                               PI3D, &
                                                               TH3D, &
                                                                T3D, &
                                                               dz8w, &
                                                                  z

        REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
                  INTENT(INOUT)   ::                        RUBLTEN, &
                                                            RVBLTEN, &
                                                           RTHBLTEN, &
                                                           RQVBLTEN, &
                                                           RQCBLTEN

        REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                  INTENT(IN   )   ::                          XLAND, &
                                                                HFX, &
                                                                QFX

        REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                  INTENT(INOUT)   ::                            HOL, &
                                                                UST, &
                                                                PBL, &
                                                                ZNT

        LOGICAL,  INTENT(IN)      ::                        FLAG_QI

        REAL,     DIMENSION( ims:ime, jms:jme ), INTENT(IN   )  ::    &
                                                                PSIM, &
                                                                PSIH

        REAL,     DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)   ::   &
                                                                WSPD

        REAL,     DIMENSION( ims:ime, jms:jme ), INTENT(IN   )   ::   &
                                                              GZ1OZ0, &
                                                                  BR

        REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                  INTENT(IN   )               ::               PSFC

        REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                  INTENT(IN   )   ::                            TSK

        REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                  INTENT(INOUT)   ::                            ZOL

        INTEGER,  DIMENSION( ims:ime, jms:jme )                    , &
                  INTENT(OUT  )   ::                         KPBL2D

        REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
                  INTENT(IN   )   ::                            U3D, &
                                                                V3D

        REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                  OPTIONAL                                         , &
                  INTENT(INOUT)   ::                         REGIME

        REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
                  INTENT(INOUT)   ::                       RQIBLTEN

        REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
                  OPTIONAL                                         , &
                  INTENT(IN   )   ::                           QI3D

        

        REAL     DZQ( its:ite, kte )
        REAL    QI2D( its:ite, kte )

        INTEGER     I, J, K, KLP1, NK

        

        KLP1 = KTE + 1

        DO J=jts,jte

            IF ( PRESENT( QI3D ) ) THEN
                DO k=1, kte
                    NK=KLP1 - K
                    DO i=its,ite
                        DZQ(I,K)  = dz8w(i,NK,j)
                        QI2D(I,K) = QI3D(I,K,J)
                    END DO
                END DO
            ELSE
                DO k=1, kte
                    NK=KLP1 - K
                    DO i=its,ite
                        DZQ(I,K)  = dz8w(i,NK,j)
                        QI2D(I,K) = 0.0
                    END DO
                END DO
            END IF


            CALL MRF2D( U3D(ims,kms,j), V3D(ims,kms,j), T3D(ims,kms,j),         &
                        QV3D(ims,kms,j), QC3D(ims,kms,j), QI2D,                 &
                        P3D(ims,kms,j), PI3D(ims,kms,j),                        &
                        RUBLTEN(ims,kms,j), RVBLTEN(ims,kms,j),                 &
                        RTHBLTEN(ims,kms,j), RQVBLTEN(ims,kms,j),               &
                        RQCBLTEN(ims,kms,j), RQIBLTEN(ims,kms,j),               &
                        DT, p1000mb,                                            &
                        CP, G, ROVCP, R, ROVG,                                  &
                        DZQ, XLV, Rv,                                           &
                        PSFC(ims,j), ZNT(ims,j),                                &
                        UST(ims,j), ZOL(ims,j),                                 &
                        HOL(ims,j), PBL(ims,j), PSIM(ims,j),                    &
                        PSIH(ims,j), XLAND(ims,j), HFX(ims,j), QFX(ims,j),      &
                        GZ1OZ0(ims,j), WSPD(ims,j), BR(ims,j),                  &
                        KPBL2D(ims,j),                                          &
                        SVP1, SVP2, SVP3, SVPT0, EP1, EP2, KARMAN, EOMEG,       &
                        STBOLT, flag_qi,                                        &
                        ims,ime, kms,kme,                                       &
                        its,ite, kts,kte                                        )

        END DO

    END SUBROUTINE MRF





    SUBROUTINE MRF2D( U2D, V2D, T2D, QV2D, QC2D, QI2D, P2D, PI2D,               &
                      U2DTEN, V2DTEN, TH2DTEN, QV2DTEN, QC2DTEN, QI2DTEN,       &
                      DT, p1000mb,                                              &
                      CP, G, ROVCP, R, ROVG,                                    &
                      DZQ, XLV, RV, PSFCPA,                                     &
                      ZNT, UST, ZOL, HOL, PBL, PSIM, PSIH,                      &
                      XLAND, HFX, QFX, GZ1OZ0, WSPD, BR, KPBL1D,                &
                      SVP1, SVP2, SVP3, SVPT0, EP1, EP2, KARMAN, EOMEG, STBOLT, &
                      flag_qi,                                                  &
                      ims,ime, kms,kme,                                         &
                      its,ite, kts,kte                                          )

      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      

      IMPLICIT NONE

      

      INTEGER, INTENT(IN   ) :: ims,ime, kms,kme, its,ite, kts,kte

      LOGICAL, INTENT(IN   ) :: FLAG_QI

      REAL,    INTENT(IN   ) :: P1000mb
      REAL,    INTENT(IN   ) :: DT, CP, G, ROVCP
      REAL,    INTENT(IN   ) :: ROVG, R, XLV, RV
      REAL,    INTENT(IN   ) :: SVP1, SVP2, SVP3, SVPT0
      REAL,    INTENT(IN   ) :: EP1, EP2, KARMAN, EOMEG, STBOLT

      REAL,    DIMENSION( ims:ime, KME ), INTENT(IN   ) :: QV2D, QC2D, P2D, PI2D, T2D
      REAL,    DIMENSION( ims:ime, KME ), INTENT(IN   ) :: U2D, V2D

      REAL,    DIMENSION( its:ite, KTE ), INTENT(IN   ) :: DZQ, QI2D

      REAL,    DIMENSION( ims:ime, KME ), INTENT(INOUT) :: U2DTEN,  V2DTEN , TH2DTEN
      REAL,    DIMENSION( ims:ime, KME ), INTENT(INOUT) :: QV2DTEN, QC2DTEN, QI2DTEN

      REAL,    DIMENSION( ims:ime ), INTENT(INOUT) :: HOL, UST, PBL, ZNT
      REAL,    DIMENSION( ims:ime ), INTENT(INOUT) :: WSPD, ZOL

      REAL,    DIMENSION( ims:ime ), INTENT(IN   ) :: XLAND, HFX, QFX
      REAL,    DIMENSION( ims:ime ), INTENT(IN   ) :: PSIM, PSIH
      REAL,    DIMENSION( ims:ime ), INTENT(IN   ) :: GZ1OZ0, BR
      REAL,    DIMENSION( ims:ime ), INTENT(IN   ) :: PSFCPA

      INTEGER, DIMENSION( ims:ime ), INTENT(  OUT) :: KPBL1D

      

      REAL, PARAMETER :: RLAM    = 150.0
      REAL, PARAMETER :: PRMIN   =   0.5
      REAL, PARAMETER :: PRMAX   =   4.0
      REAL, PARAMETER :: XKZMIN  =   0.01
      REAL, PARAMETER :: XKZMAX  =1000.0
      REAL, PARAMETER :: RIMIN   =-100.0
      REAL, PARAMETER :: BRCR    =   0.5
      REAL, PARAMETER :: CFAC    =   7.8
      REAL, PARAMETER :: SFCFRAC =   0.1
      REAL, PARAMETER :: CKZ     =   0.001
      REAL, PARAMETER :: ZFMIN   =   1.0E-8
      REAL, PARAMETER :: APHI5   =   5.0
      REAL, PARAMETER :: APHI16  =  16.0
      REAL, PARAMETER :: GAMCRT  =   3.0
      REAL, PARAMETER :: GAMCRQ  =   2.0E-3

      INTEGER, PARAMETER :: PFAC = 2

      

      REAL,    DIMENSION( its:ite, KTE )   :: THVX, DZA, ZA, ZQ
      REAL,    DIMENSION( its:ite, KTE )   :: XKZM, XKZH
      REAL,    DIMENSION( its:ite, KTE )   :: A1, A2, AD, AU, AL

      REAL,    DIMENSION( its:ite ) :: RHOX, WSPD1, GOVRTH
      REAL,    DIMENSION( its:ite ) :: PBL0
      REAL,    DIMENSION( its:ite ) :: XKZMKL, TH1, HTFAC, HQFAC
      REAL,    DIMENSION( its:ite ) :: ZL1, THERMAL, WSCALE
      REAL,    DIMENSION( its:ite ) :: HGAMT, HGAMQ, BRDN, BRUP
      REAL,    DIMENSION( its:ite ) :: PHIM, PHIH, CPM, DPRN
      REAL,    DIMENSION( its:ite ) :: DUSFC, DVSFC, DTSFC, DQSFC

      INTEGER, DIMENSION( its:ite ) :: KPBL, KPBL0

      LOGICAL, DIMENSION( its:ite ) :: PBLFLG, SFCFLG, STABLE

      INTEGER :: KL, KLM, KLP1, N, I ,K, KK, L, NK, IDIM, KDIM

      REAL    ::  ZOLN, X, Y, THCON, TVCON, TV1
      REAL    ::  ZL, DU, DV
      REAL    ::  PSIX
      REAL    ::  DT4, RDT, SPDK2, FM, FH, HOL1, GAMFAC, VPERT, PRNUM
      REAL    ::  ZFAC, XKZO, SS, RI, QMEAN, TMEAN, ALPH, CHI, ZK, RL2, DK, SRI
      REAL    ::  BRINT, DTODSD, DSIG, RDZ, DSDZT, DSDZQ, DSDZ2
      REAL    ::  DTODSU

      

      KL   = kte
      KLP1 = kte+1
      KLM  = kte-1
      IDIM = ITE-ITS+1
      KDIM = KTE-KTS+1

      DT4 = 2.0*DT
      RDT = 1.0/DT4

      
      
      

      DO I = its,ite
          ZQ(I,KTE)   = DZQ(I,KTE)
          ZA(I,KTE)   = 0.5*DZQ(I,KTE)
      END DO

      DO K = KTE-1, 1, -1
      DO I = its,ite
          ZQ(I,K) =       ZQ(I,K+1) + DZQ(I,K) 
          ZA(I,K)  = 0.5*(ZQ(I,K+1) +  ZQ(I,K))
          DZA(I,K) = ZA(I,K) - ZA(I,K+1)
      END DO
      END DO

      DO  K = 1,kte
          NK = KLP1 - K
          DO  I = its,ite
              THCON     = ( P1000mb / P2D(I,NK) )**ROVCP
              TVCON     = (1. + EP1*QV2D(I,NK))
              THVX(I,K) = T2D(I,NK)*THCON*TVCON
          END DO
      END DO

      DO I = its,ite

          

          CPM(I) = CP*(1.0 + 0.8*QV2D(I,1))

          TVCON   = (1.+EP1*QV2D(I,1))
          TV1     = T2D(I,1)*TVCON
          RHOX(I) = PSFCPA(I)/(R*TV1)

          THCON     = ( P1000mb / P2D(I,1) )**ROVCP
          TH1(I)    = T2D(I,1)*THCON
          GOVRTH(I) = G/TH1(I)

          

          
          

          WSPD1(I) = SQRT( U2D(I,1)**2 + V2D(I,1)**2 )+1.E-9
          PSIX     = GZ1OZ0(I)-PSIM(I)
          UST(I)   = KARMAN*WSPD(I)/PSIX
          IF ( XLAND(I) .LT. 1.5 ) UST(I) = MAX( UST(I), 0.1 )

          KPBL(I)    = KL
          PBL(I)     = ZQ(I,KL)
          KPBL0(I)   = KL
          PBL0(I)    = ZQ(I,KL)
          PBLFLG(I)  = .TRUE.
          SFCFLG(I)  = ( BR(I).LE.0.0 )
          ZL1(I)     = ZA(I,KL)
          THERMAL(I) = THVX(I,KL)

          STABLE(I) = .FALSE.
          BRUP(I)   = BR(I)
      END DO

      

      DO K = KLM,1,-1
          NK = KLP1 - K
          DO I = its,ite
              IF ( .NOT.STABLE(I) ) THEN
                  BRDN(I) = BRUP(I)
                  SPDK2   = MAX( U2D(I,NK)**2 + V2D(I,NK)**2, 1.0 )
                  BRUP(I) = (THVX(I,K)-THERMAL(I))*G*ZA(I,K)/(THVX(I,KL)*SPDK2)
                  KPBL(I) = K
                  STABLE(I) = BRUP(I).GT.BRCR
              END IF
          END DO
      END DO

      DO I = its,ite
          K = KPBL(I)
          IF ( BRDN(I).GE.BRCR ) THEN
              PBL(I) = ZA(I,K+1)
          ELSEIF ( BRUP(I).LE.BRCR ) THEN
              PBL(I) = ZA(I,K)
          ELSE
              BRINT = (BRCR - BRDN(I))/(BRUP(I) - BRDN(I))
              PBL(I) = ZA(I,K+1) + BRINT*(ZA(I,K) - ZA(I,K+1))
          END IF

          IF ( PBL(I).LT.ZQ(I,KPBL(I)+1) )  KPBL(I) = KPBL(I)+1

          FM = GZ1OZ0(I) - PSIM(I)
          FH = GZ1OZ0(I) - PSIH(I)
          HOL(I) = MAX( BR(I)*FM*FM/FH, RIMIN )
          IF ( SFCFLG(I) ) THEN
              HOL(I) = MIN( HOL(I), -ZFMIN )
          ELSE
              HOL(I) = MAX( HOL(I), ZFMIN )
          END IF

          HOL1 = HOL(I)*PBL(I)/ZL1(I)*SFCFRAC
          HOL(I) = -HOL(I)*PBL(I)/ZL1(I)
          IF ( SFCFLG(I) ) THEN
              PHIH(I) = 1.0 / SQRT(1. - APHI16*HOL1 )
              PHIM(I) = SQRT( PHIH(I) )
          ELSE
              PHIM(I) = (1. + APHI5*HOL1)
              PHIH(I) = PHIM(I)
          END IF
          WSCALE(I) = UST(I)/PHIM(I)
          WSCALE(I) = MIN(WSCALE(I),UST(I)*APHI16)
          WSCALE(I) = MAX(WSCALE(I),UST(I)/APHI5)

          
          

          IF ( SFCFLG(I) ) THEN
              GAMFAC = CFAC/( RHOX(I)*WSCALE(I) )
              HGAMT(I) = MIN( GAMFAC*HFX(I)/CPM(I), GAMCRT )
              HGAMQ(I) = MIN( GAMFAC*QFX(I),        GAMCRQ )

              IF ( XLAND(I) .GE. 1.5 )  HGAMQ(I) = 0.0

              VPERT = HGAMT(I)+EP1*TH1(I)*HGAMQ(I)
              VPERT = MIN(VPERT,GAMCRT)
              THERMAL(I) = THERMAL(I) + MAX(VPERT,0.)
              HGAMT(I)   = MAX( HGAMT(I), 0.0 )
              HGAMQ(I)   = MAX( HGAMQ(I), 0.0 )
          ELSE
              HGAMT(I)  = 0.0
              HGAMQ(I)  = 0.0
              PBLFLG(I) = .FALSE.
          END IF

          IF ( PBLFLG(I) ) THEN
              KPBL(I) = KL
              PBL(I)  = ZQ(I,KL)

              

              STABLE(I) = .FALSE.
              BRUP(I)   = BR(I)
          END IF
      END DO
      
      DO K = KLM,1,-1
          NK = KLP1 - K
          DO I = its,ite
              IF ( .NOT.STABLE(I).AND.PBLFLG(I) ) THEN
                  BRDN(I) = BRUP(I)
                  SPDK2   = MAX( U2D(I,NK)**2 + V2D(I,NK)**2, 1.0 )
                  BRUP(I) = (THVX(I,K)-THERMAL(I))*G*ZA(I,K)/(THVX(I,KL)*SPDK2)
                  KPBL(I) = K
                  STABLE(I) = BRUP(I).GT.BRCR
              END IF
          END DO
      END DO

      DO I = its,ite
          IF ( PBLFLG(I) ) THEN
              K = KPBL(I)
              IF ( BRDN(I).GE.BRCR ) THEN
                  PBL(I) = ZA(I,K+1)
              ELSEIF ( BRUP(I).LE.BRCR ) THEN
                  PBL(I) = ZA(I,K)
              ELSE
                  BRINT = (BRCR-BRDN(I))/(BRUP(I)-BRDN(I))
                  PBL(I) = ZA(I,K+1) + BRINT*(ZA(I,K) - ZA(I,K+1))
              END IF

              IF ( PBL(I).LT.ZQ(I,KPBL(I)+1) )  KPBL(I)   = KPBL(I)+1
              IF ( KPBL(I).LE.1 )               PBLFLG(I) = .FALSE.
          END IF

          

          IF ( PBLFLG(I) ) THEN
              STABLE(I) = .FALSE.
              BRUP(I)   = BR(I)
          END IF
      END DO

      DO K = KLM,1,-1
          NK = KLP1 - K
          DO I = its,ite
              IF ( .NOT.STABLE(I).AND.PBLFLG(I) ) THEN
                  BRDN(I) = BRUP(I)
                  SPDK2   = MAX( U2D(I,NK)**2 + V2D(I,NK)**2, 1.0 )
                  BRUP(I) = (THVX(I,K)-THERMAL(I))*G*ZA(I,K)/(THVX(I,KL)*SPDK2)
                  KPBL0(I) = K
                  STABLE(I) =  ( BRUP(I).GT.0.0 )
              END IF
          END DO
      END DO

      DO I = its,ite

          IF ( PBLFLG(I) ) THEN     
              K = KPBL0(I)
              IF ( BRDN(I).GE.0.0 ) THEN
                  PBL0(I) = ZA(I,K+1)
              ELSEIF ( BRUP(I).LE.0.0 ) THEN
                  PBL0(I) = ZA(I,K)
              ELSE
                  BRINT = (0.0-BRDN(I))/(BRUP(I)-BRDN(I))
                  PBL0(I) = ZA(I,K+1) + BRINT*(ZA(I,K)-ZA(I,K+1))
              END IF              

              IF ( PBL0(I).LT.ZQ(I,KPBL0(I)+1) )  KPBL0(I) = KPBL0(I)+1
              IF ( KPBL0(I).LE.1 )                PBLFLG(I) = .FALSE.
          END IF

          PRNUM = PHIH(I)/PHIM(I) + CFAC*KARMAN*SFCFRAC
          PRNUM = MIN( PRNUM, PRMAX )
          PRNUM = MAX( PRNUM, PRMIN )
          DPRN(I) = 1.0 / PRNUM

      END DO

      DO K = 2, kte
          NK = KLP1 - K
          DO I = its,ite
              XKZO = CKZ*DZA(I,K-1)

              IF ( KPBL(I).LT.K ) THEN      

                  ZFAC  = MAX((1.-(ZQ(I,K)-ZL1(I))/(PBL(I)-ZL1(I))),ZFMIN)
                  XKZM(I,K) = XKZO + WSCALE(I)*KARMAN*ZQ(I,K)*ZFAC**PFAC
                  XKZH(I,K) = XKZM(I,K)*DPRN(I)

              ELSE      

                  DU = U2D(I,NK+1) - U2D(I,NK)
                  DV = V2D(I,NK+1) - V2D(I,NK)
                  SS = (DU**2 + DV**2 )/DZA(I,K-1)**2 + 1.0E-9
                  RI = GOVRTH(I)*(THVX(I,K-1)-THVX(I,K))/(SS*DZA(I,K-1))
                  IF ( (QC2D(I,NK)   + QI2D(I,NK)  ).GT.0.01E-3 .AND.      &
                       (QC2D(I,NK+1) + QI2D(I,NK+1)).GT.0.01E-3 ) THEN        
                      QMEAN = 0.5*(QV2D(I,NK) + QV2D(I,NK+1))
                      TMEAN = 0.5*(T2D(I,NK)  +  T2D(I,NK+1))
                      ALPH  = XLV*QMEAN/(R*TMEAN)
                      CHI   = XLV*XLV*QMEAN/(CP*RV*TMEAN**2)
                      RI    = (1. + ALPH)*( RI - G*G*(CHI-ALPH)/(SS*TMEAN*CP*(1.+CHI) ) )
                  END IF
                  ZK  = KARMAN*ZQ(I,K)
                  RL2 = (ZK*RLAM/(RLAM + ZK))**2
                  DK  = RL2*SQRT(SS)
                  IF ( RI.LT.0. ) THEN          
                      SRI       = SQRT(-RI)
                      XKZM(I,K) = XKZO + DK*(1+8.*(-RI)/(1+1.746*SRI))
                      XKZH(I,K) = XKZO + DK*(1+8.*(-RI)/(1+1.286*SRI))
                  ELSE                          
                      XKZH(I,K) = XKZO + DK/(1+5.*RI)**2
                      PRNUM     = 1.0 + 2.1*RI
                      PRNUM     = MIN(PRNUM,PRMAX)
                      XKZM(I,K) = (XKZH(I,K)-XKZO)*PRNUM + XKZO
                  END IF

              END IF        

              XKZM(I,K) = MAX( MIN( XKZM(I,K), XKZMAX ), XKZMIN )
              XKZH(I,K) = MAX( MIN( XKZH(I,K), XKZMAX ), XKZMIN )

          END DO
      END DO

      

      DO I = its,ite
          AD(I,1)   = 1.0
          A1(I,1)   = T2D(I,1)  + DT4*HFX(I)/(RHOX(I)*ZQ(I,KL)*CPM(I))
          A2(I,1)   = QV2D(I,1) + DT4*QFX(I)/(RHOX(I)*ZQ(I,KL))
          AU(i,KTE) = 0.0
          AL(i,KTE) = 0.0
          
          IF ( PBLFLG(I) ) THEN
              HTFAC(I) = G/CP - HGAMT(I)/PBL(I)
              HQFAC(I) =      - HGAMQ(I)/PBL(I)
          END IF

      END DO

      DO K = kte,2,-1
          NK = KLP1 - K
          DO I = its,ite
              DTODSD = DT4/DZQ(I,K)
              DTODSU = DT4/DZQ(I,K-1)
              IF ( PBLFLG(I).AND.KPBL(I).LT.K ) THEN
                  DSDZT = XKZH(I,K)*HTFAC(I)
                  DSDZQ = XKZH(I,K)*HQFAC(I)
                  A2(I,NK)   = A2(I,NK)     + DTODSD*DSDZQ
                  A2(I,NK+1) = QV2D(I,NK+1) - DTODSU*DSDZQ
              ELSE
                  DSDZT = XKZH(I,K)*(G/CP)
                  A2(I,NK+1) = QV2D(I,NK+1)
              END IF
              DSDZ2    = XKZH(I,K)/DZA(I,K-1)
              AU(I,NK) = -DTODSD*DSDZ2
              AL(I,NK) = -DTODSU*DSDZ2
              AD(I,NK) = AD(I,NK)-AU(I,NK)
              AD(I,NK+1) = 1.-AL(I,NK)
              A1(I,NK)   = A1(I,NK)    + DTODSD*DSDZT
              A1(I,NK+1) = T2D(I,NK+1) - DTODSU*DSDZT
          END DO
      END DO

      

      CALL TRIDI2( AL, AD, AU, A1, A2, AU, A1,A2, IDIM, KDIM )

      

      DO K=1,kte
      DO I = its,ite
          TH2DTEN(I,K) = (A1(I,K) -  T2D(I,K))*RDT / PI2D(I,K)
          QV2DTEN(I,K) = (A2(I,K) - QV2D(I,K))*RDT
      END DO
      END DO

      

      DO I = its,ite
          AD(I,1)   = 1.0
          ZFAC      = 1.0 - DT4 * WSPD1(I) * UST(I)**2 / ( ZQ(I,KL) * WSPD(I)**2 )
          A1(I,1)   = ZFAC*U2D(I,1)
          A2(I,1)   = ZFAC*V2D(I,1)
          AU(i,KTE) = 0.0
          AL(i,KTE) = 0.0
      END DO

      DO K = kte,2,-1
          NK = KLP1 - K
          DO I = its,ite
              DTODSD   = DT4/DZQ(I,K)
              DTODSU   = DT4/DZQ(I,K-1)
              DSDZ2    = XKZM(I,K)/DZA(I,K-1)
              AU(I,NK) = -DTODSD*DSDZ2
              AL(I,NK) = -DTODSU*DSDZ2
              AD(I,NK) = AD(I,NK) - AU(I,NK)
              AD(I,NK+1) = 1.0 - AL(I,NK)
              A1(I,NK+1) = U2D(I,NK+1)
              A2(I,NK+1) = V2D(I,NK+1)
          END DO
      END DO

      

      CALL TRIDI2( AL, AD, AU, A1, A2, AU, A1,A2, IDIM, KDIM )

      

      DO K = kte,1,-1
      DO I = its,ite
          U2DTEN(I,K) = (A1(I,K) - U2D(I,K))*RDT
          V2DTEN(I,K) = (A2(I,K) - V2D(I,K))*RDT
      END DO
      END DO

      

      DO I = its,ite
          AD(I,1)   = 1.0
          A1(I,1)   = QC2D(I,1)
          A2(I,1)   = QI2D(I,1)
          AU(i,KTE) = 0.0
          AL(i,KTE) = 0.0
      END DO

      DO K = kte,2,-1
          NK = KLP1 - K
          DO I = its,ite
              DTODSD = DT4/DZQ(I,K)
              DTODSU = DT4/DZQ(I,K-1)
              A1(I,NK+1) = QC2D(I,NK+1)
              A2(I,NK+1) = QI2D(I,NK+1)
              DSDZ2      = XKZH(I,K)/DZA(I,K-1)
              AU(I,NK)   = -DTODSD*DSDZ2
              AL(I,NK)   = -DTODSU*DSDZ2
              AD(I,NK)   = AD(I,NK) - AU(I,NK)
              AD(I,NK+1) = 1.0 - AL(I,NK)
          END DO
      END DO

      

      CALL TRIDI2( AL, AD, AU, A1, A2, AU, A1,A2, IDIM, KDIM )

      IF ( flag_QI ) THEN
          DO K=1,kte
          DO I = its,ite
              QC2DTEN(I,K) = (A1(I,K) - QC2D(I,K))*RDT
              QI2DTEN(I,K) = (A2(I,K) - QI2D(I,K))*RDT
          END DO
          END DO
      ELSE
          DO K=1,kte
          DO I = its,ite
              QC2DTEN(I,K) = (A1(I,K) - QC2D(I,K))*RDT
          END DO
          END DO
      END IF


      DO  I=its,ite
          KPBL1D(I)= KPBL0(I)
          PBL(I)   = PBL0(I)
      END DO

    END SUBROUTINE MRF2D





    SUBROUTINE TRIDI2( CL, CM, CU, R1, R2, AU, A1, A2, IDIM, KDIM )

        IMPLICIT NONE

        INTEGER, INTENT(IN ) :: IDIM, KDIM

        REAL, DIMENSION( IDIM, 2:KDIM+1 ), INTENT(IN   ) :: CL
        REAL, DIMENSION( IDIM, KDIM ),     INTENT(IN   ) :: CM, R1, R2
        REAL, DIMENSION( IDIM, KDIM ),     INTENT(INOUT) :: AU, CU, A1, A2

        REAL    :: FK
        INTEGER :: I, K

        

        DO I = 1, IDIM
            FK = 1./CM(I,1)
            AU(I,1) = FK*CU(I,1)
            A1(I,1) = FK*R1(I,1)
            A2(I,1) = FK*R2(I,1)
        END DO

        DO K = 2, KDIM-1
        DO I = 1, IDIM
            FK = 1./(CM(I,K)-CL(I,K)*AU(I,K-1))
            AU(I,K) = FK*CU(I,K)
            A1(I,K) = FK*(R1(I,K)-CL(I,K)*A1(I,K-1))
            A2(I,K) = FK*(R2(I,K)-CL(I,K)*A2(I,K-1))
        END DO
        END DO

        DO I = 1, IDIM
            FK = 1./(CM(I,KDIM)-CL(I,KDIM)*AU(I,KDIM-1))
            A1(I,KDIM) = FK*(R1(I,KDIM)-CL(I,KDIM)*A1(I,KDIM-1))
            A2(I,KDIM) = FK*(R2(I,KDIM)-CL(I,KDIM)*A2(I,KDIM-1))
        END DO

        DO K = KDIM-1,1,-1
        DO I = 1, IDIM
            A1(I,K) = A1(I,K)-AU(I,K)*A1(I,K+1)
            A2(I,K) = A2(I,K)-AU(I,K)*A2(I,K+1)
        END DO
        END DO

        RETURN

    END SUBROUTINE TRIDI2





    SUBROUTINE mrfinit(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,          &
                      RQCBLTEN,RQIBLTEN,P_QI,P_FIRST_SCALAR,       &
                      restart, allowed_to_read ,                   &
                      ids, ide, jds, jde, kds, kde,                &
                      ims, ime, jms, jme, kms, kme,                &
                      its, ite, jts, jte, kts, kte                 )
        IMPLICIT NONE

        LOGICAL , INTENT(IN)          :: restart , allowed_to_read
        INTEGER , INTENT(IN)          ::  ids, ide, jds, jde, kds, kde, &
                                          ims, ime, jms, jme, kms, kme, &
                                          its, ite, jts, jte, kts, kte
        INTEGER , INTENT(IN)          ::  P_QI,P_FIRST_SCALAR

        REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) :: &
                                                              RUBLTEN, &
                                                              RVBLTEN, &
                                                              RTHBLTEN, &
                                                              RQVBLTEN, &
                                                              RQCBLTEN, &
                                                              RQIBLTEN
        INTEGER :: i, j, k, itf, jtf, ktf

        jtf=min0(jte,jde-1)
        ktf=min0(kte,kde-1)
        itf=min0(ite,ide-1)

        IF ( .not.restart ) THEN

          DO j=jts,jtf
          DO k=kts,ktf
          DO i=its,itf
             RUBLTEN(i,k,j)=0.
             RVBLTEN(i,k,j)=0.
             RTHBLTEN(i,k,j)=0.
             RQVBLTEN(i,k,j)=0.
             RQCBLTEN(i,k,j)=0.
          END DO
          END DO
          END DO
        END IF

        IF (P_QI .ge. P_FIRST_SCALAR .and. .not.restart) THEN
           DO j=jts,jtf
           DO k=kts,ktf
           DO i=its,itf
              RQIBLTEN(i,k,j)=0.
           END DO
           END DO
           END DO
        END IF

   END SUBROUTINE mrfinit


END MODULE module_bl_mrf

