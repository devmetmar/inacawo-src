#if (NMM_CORE == 1)
MODULE module_diag_pld
CONTAINS
   SUBROUTINE diag_pld_stub
   END SUBROUTINE diag_pld_stub
END MODULE module_diag_pld
#else
!WRF:MEDIATION_LAYER:PHYSICS
!

MODULE module_diag_pld
CONTAINS

SUBROUTINE pld ( u,v,w,t,qv,zp,zb,pp,pb,p,p8w,                   &        
                 msfux,msfuy,msfvx,msfvy,msftx,msfty,            &        
                 f,e,cosalpha,sinalpha,w0avg,refl_10cm,          &        
                 relvort,u10,v10,t2,q2,swdown,                   &        
                 qc_curr,qr_curr,qi_curr,qs_curr,qg_curr,        &        
                 snowh,msft,dx,terht,rainc,rainsh,rainnc,snownc, &        
                 graupelnc,cfracl,cfracm,cfrach,coszen,curr_time,&        
                 cu_act_flag,cu_exp_flag,                        &
                 cutop,cubot,ishall,cu_top_ta,cudt,              &
                 rainncv,raincv,dt_advec,                        &
                 afw_turbdex,afw_llturb,afw_llturblgt,           &
                 afw_llturbmdt,afw_llturbsvr,afw_fits,afw_llws,  &
                 uphlmx,wspdmx10,hail_afwa,torn_afwa,br,         &
                 landmask,lakemask,lsm_lays,soilthk,tslb,smcrel, &
                 mubase,mupert,                                  & !! CAWO           
                 use_tot_or_hyd_p,missing,                       &        
                 num_press_levels,max_press_levels,press_levels, &        
                 firstime,dt_pld,cu_physics,kfeta_shal_only,     &
                 kfeta_shal_dlim,grell_shal_only,ntied_shal_only,&
                 lrestart,do_radar_ref,                          &        
                 p_pl,u_pl,v_pl,t_pl,rh_pl,ght_pl,s_pl,          &        
                 td_pl,q_pl,                                     &
                 wdir_pl,thta_pl,wwnd_pl,tcon_pl,                &
                 cconfr_pl,rnwtfr_pl,snowfr_pl,graufr_pl,        &
                 avor_pl,pvor_pl,                                &                 
                 twet1,twmean,tw2,maskfrac,qclay1,qilay1,qrlay1, &
                 qslay1,qglay1,qnilay1,rain,raliqd,rafzra,rasnow,&
                 raicep,raliqdcum,rafzracum,rasnowcum,dysnowcum, &
                 dysnratcu,raicepcum,                            &                 
                 prate_inst,talay1,tnsfc,twbgt2,                 &
                 ta2,apta2,tdew2,rhum2,tdsig1,ux10,vx10,wspd10,  &
                 kpbl,pblh,gspd10,                               &                                  
                 slprs,cfraclo,cfracmd,cfrachi,z1000,            &                 
                 snolev,mixlev,frzlev,z850,z700,                 &                 
                 z500,z250,uwind850,vwind850,                    &                 
                 rh850,ta850,uwind700,vwind700,                  &                 
                 rh700,ta700,uwind500,vwind500,                  &                 
                 rh500,ta500,                                    &
                 uwind300,vwind300,rh300,ta300,                  &
                 uwind250,vwind250,                              &                            
                 rh250,ta250,dz1000_850,dz1000_500,dz850_700,    &
                 avor850,avor700,avor500,avor250,                &
                 snodpt,rfl_1kmagl,rfl_maxagl,rfl_mxcagl,        &
                 rflmpsc1km,rflmpscmax,etp_18dbz,etp_mpsc,       &
                 vilh,radarvilh,refl_htrgen,                     &
                 cfractot,mucape,cinh,mlcape,cinm,sbcape,cins,   &
                 srh0_1km,srh0_3km,ehi_1km,ehi_3km,              &
                 wdir700,thtae700,                               &
                 wdir300,wspd300,wdir250,wspd250,z300,wwind700,  &
                 visib,ceiling,flghtreg,rain_last,rain_rate,     &
                 sn_nc_last,sn_nc_rate,gr_nc_last,gr_nc_rate,    &
                 lq_nc_last,lq_nc_rate,radcv_last,radcv_rate,    &
                 rascv_last,rascv_rate,rannc_last,rannc_rate,    &
                 snowcv,graupelcv,liquidcv,                      &
                 sn_cv_last,sn_cv_rate,gr_cv_last,gr_cv_rate,    &
                 lq_cv_last,lq_cv_rate,ptyp_fip,                 &
                 ptyp_cat,cat_rain,cat_fzrn,cat_icep,cat_snow,   &
                 cu_act_last,cu_activ,cu_top_last,cu_top_m,      &
                 cu_bot_last,cu_bot_m,ishall_last,cu_ctt_last,   &
                 cu_kbot_lst,cu_ktop_lst,                        &
                 icgpot35,sldpot35,icgpot30,sldpot30,            &                 
                 icgpot25,sldpot25,icgpot20,sldpot20,            &         
                 icgpot15,sldpot15,icgpot10,sldpot10,            &             
                 icgpot05,sldpot05,icgpot01,sldpot01,            &
                 turbdx01,turbdx02,turbdx03,turbdx04,            &
                 turbdx05,turbdx06,turbdx07,                     &
                 llturb,llturblgt,llturbmdt,llturbsvr,fits,llws, &
                 icgpot45,sldpot45,icgpot40,sldpot40,            &
                 uphelmax,wspd10mx,afw_hail,afw_torn,            &
                 wbfrzlev,                                       &                                  
                 qv_col,cnvresmask,fogflag,                      &
                 apta2c,qv2,spechum,soiltlay1,smrellay1,smreltot,&                                                                                                         
                 !v36_non_oper:: other icing/aviation types: clr;mxd;rim
                 jstcald_htr,                                    &                 
                 ! CAWO                                          &
                 ta050,ta100,ta150,ta200,ta350,ta400,ta450,ta550,&
                 ta600,ta650,ta750,ta800,ta900,ta950,ta1000,     &
                 td050,td100,td150,td200,td250,td300,td350,td400,&
                 td450,td500,td550,td600,td650,td700,td750,td800,&
                 td850,td900,td950,td1000,                       &
                 rh050,rh100,rh150,rh200,rh350,rh400,rh450,rh550,&
                 rh600,rh650,rh750,rh800,rh900,rh950,rh1000,     &
                 qv050,qv100,qv150,qv200,qv250,qv300,qv350,qv400,&
                 qv450,qv500,qv550,qv600,qv650,qv700,qv750,qv800,&
                 qv850,qv900,qv950,qv1000,                       &
                 wspd050,wspd100,wspd150,wspd200,wspd350,wspd400,&
                 wspd450,wspd500,wspd550,wspd600,wspd650,wspd750,&
                 wspd800,wspd850,wspd900,wspd950,wspd1000,       &
                 w_dir050,w_dir100,w_dir150,w_dir200,w_dir350,   &
                 w_dir400,w_dir450,w_dir500,w_dir550,w_dir600,   &
                 w_dir650,w_dir750,w_dir800,w_dir850,w_dir900,   &
                 w_dir950,w_dir1000,                             &
                 theta050,theta100,theta150,theta200,theta250,   &
                 theta300,theta350,theta400,theta450,theta500,   &
                 theta550,theta600,theta650,theta700,theta750,   &
                 theta800,theta850,theta900,theta950,theta1000,  &
                 z050,z100,z150,z200,z350,z400,z450,z550,z600,   &
                 z650,z750,z800,z900,z950,                       &
                 wwind050,wwind100,wwind150,wwind200,wwind250,   &
                 wwind300,wwind350,                              &
                 wwind400,wwind450,wwind500,wwind550,wwind600,   &
                 wwind650,wwind750,wwind800,wwind850,wwind900,   &
                 wwind950,wwind1000,                             &
                 clconfrc050,clconfrc100,clconfrc150,clconfrc200,&
                 clconfrc250,clconfrc300,clconfrc350,clconfrc400,&
                 clconfrc450,clconfrc500,clconfrc550,clconfrc600,&
                 clconfrc650,clconfrc700,clconfrc750,clconfrc800,&
                 clconfrc850,clconfrc900,clconfrc950,clconfrc1000,&
                 rnwatfrc050,rnwatfrc100,rnwatfrc150,rnwatfrc200,&
                 rnwatfrc250,rnwatfrc300,rnwatfrc350,rnwatfrc400,&
                 rnwatfrc450,rnwatfrc500,rnwatfrc550,rnwatfrc600,&
                 rnwatfrc650,rnwatfrc700,rnwatfrc750,rnwatfrc800,&
                 rnwatfrc850,rnwatfrc900,rnwatfrc950,rnwatfrc1000,&
                 snowfrc050,snowfrc100,snowfrc150,snowfrc200,    &
                 snowfrc250,snowfrc300,snowfrc350,snowfrc400,    & 
                 snowfrc450,snowfrc500,snowfrc550,snowfrc600,    &
                 snowfrc650,snowfrc700,snowfrc750,snowfrc800,    &
                 snowfrc850,snowfrc900,snowfrc950,snowfrc1000,   &
                 grplfrc050,grplfrc100,grplfrc150,grplfrc200,grplfrc250,grplfrc300,&
                 grplfrc350,grplfrc400,grplfrc450,grplfrc500,grplfrc550,grplfrc600,&
                 grplfrc650,grplfrc700,grplfrc750,grplfrc800,grplfrc850,grplfrc900,&
                 grplfrc950,grplfrc1000,                               &
                 avor050,avor100,avor150,avor200,avor300,avor350,&
                 avor400,avor450,avor550,avor600,avor650,avor750,&
                 avor800,avor900,avor950,avor1000,               &
                 pvor050,pvor100,pvor150,pvor200,pvor250,pvor300,&
                 pvor350,pvor400,pvor450,pvor500,pvor550,pvor600,&
                 pvor650,pvor700,pvor750,pvor800,pvor850,pvor900,&
                 pvor950,pvor1000,                               &
                 zpvor1_5,zpvor2_0,tpvor1_5,tpvor2_0,wdir_10,ratotlcum,rafrzn,&
                 rafrzncum,rafrznfr,rafrzncumfr,dryairmtot,pressfc,&                 
                 u10_earth,v10_earth,                            &
                 ! CAWO End 
                 ids,ide, jds,jde, kds,kde,                      &        
                 ims,ime, jms,jme, kms,kme,                      &        
                 its,ite, jts,jte, kts,kte                       )
   
   USE module_model_constants                                                                                                  
                                                                                                                            
   IMPLICIT NONE                                                                                                               
                                                                                                                               
   real, parameter                                                 :: PCONST = 10000.                                          
                                                                                                                            
   !  Input variables                                                                                                          
                                                                                                                            
   INTEGER, INTENT(IN   )                                          :: ids,ide, jds,jde, kds,kde, &                             
                                                                      ims,ime, jms,jme, kms,kme, &                             
                                                                      its,ite, jts,jte, kts,kte                                
   REAL   , INTENT(IN   ) , DIMENSION(ims:ime , jms:jme)           :: msfux,msfuy,msfvx,msfvy,msftx,msfty, &                   
                                                                      f,e,cosalpha,sinalpha                                    
   REAL   , INTENT(IN   ) , DIMENSION(ims:ime , jms:jme)           :: u10,v10,t2,q2,swdown,snowh,msft,terht,rainc,rainnc
   REAL   , INTENT(IN   ) , DIMENSION(ims:ime , jms:jme)           :: rainncv,raincv                 
   REAL   , INTENT(IN   ) , DIMENSION(ims:ime , jms:jme)           :: snownc,graupelnc,rainsh,coszen                           
   REAL   , INTENT(INOUT) , DIMENSION(ims:ime , jms:jme)           :: cfracl,cfracm,cfrach
   INTEGER, INTENT(IN   ) , DIMENSION(ims:ime , jms:jme)           :: kpbl
   REAL,    INTENT(IN   ) , DIMENSION(ims:ime , jms:jme)           :: pblh                                           
   REAL   , INTENT(IN   ) , DIMENSION(ims:ime , kms:kme , jms:jme) :: qc_curr,qr_curr,qi_curr,qs_curr,qg_curr                  
   REAL   , INTENT(IN   ) , DIMENSION(ims:ime , kms:kme , jms:jme) :: w0avg                                                    
   REAL   , INTENT(IN   ) , DIMENSION(ims:ime , kms:kme , jms:jme) :: refl_10cm                                                

   INTEGER, INTENT(IN   )                                          :: use_tot_or_hyd_p                                         
   REAL   , INTENT(IN   )                                          :: missing                                                  
   REAL   , INTENT(IN   ) , DIMENSION(ims:ime , kms:kme , jms:jme) :: u,v,w,t,qv,zp,zb,pp,pb,p,p8w                             
   INTEGER, INTENT(IN   )                                          :: num_press_levels, max_press_levels
   INTEGER, INTENT(IN   )                                          :: cu_physics, kfeta_shal_only, grell_shal_only
   REAL   , INTENT(IN   )                                          :: kfeta_shal_dlim
   INTEGER, INTENT(IN   )                                          :: ntied_shal_only                          
   LOGICAL, INTENT(IN   )                                          :: firstime,lrestart                                                 
   REAL   , INTENT(IN   ) , DIMENSION(max_press_levels)            :: press_levels                                             
   REAL   , INTENT(IN   )                                          :: dx,dt_pld,curr_time,dt_advec                                     
   LOGICAL, INTENT(IN   ) , DIMENSION(ims:ime , jms:jme)           :: cu_act_flag
   LOGICAL, INTENT(IN   ) , DIMENSION(ims:ime , jms:jme)           :: cu_exp_flag                                                 
   REAL   , INTENT(IN   ) , DIMENSION(ims:ime , jms:jme)           :: cutop     !! real version of k-indx of cum cld top   
   REAL   , INTENT(IN   ) , DIMENSION(ims:ime , jms:jme)           :: cubot     !! real version of k-indx of cum cld base   
   INTEGER, INTENT(IN   ) , DIMENSION(ims:ime , jms:jme)           :: ishall  
   REAL   , INTENT(IN   ) , DIMENSION(ims:ime , jms:jme)           :: cu_top_ta !! cloud top temp. of parameterized convection                                                                     
   REAL   , INTENT(IN   )                                          :: cudt
   INTEGER, INTENT(IN   )                                          :: do_radar_ref
   REAL   , INTENT(IN   ) , DIMENSION(ims:ime , kms:kme , jms:jme) :: relvort
   REAL   , INTENT(IN   ) , DIMENSION(ims:ime , 7, jms:jme)        :: afw_turbdex
   REAL   , INTENT(IN   ) , DIMENSION(ims:ime , jms:jme)           :: afw_llturb
   REAL   , INTENT(IN   ) , DIMENSION(ims:ime , jms:jme)           :: afw_llturblgt
   REAL   , INTENT(IN   ) , DIMENSION(ims:ime , jms:jme)           :: afw_llturbmdt
   REAL   , INTENT(IN   ) , DIMENSION(ims:ime , jms:jme)           :: afw_llturbsvr
   REAL   , INTENT(IN   ) , DIMENSION(ims:ime , jms:jme)           :: afw_fits
   REAL   , INTENT(IN   ) , DIMENSION(ims:ime , jms:jme)           :: afw_llws
   REAL   , INTENT(IN   ) , DIMENSION(ims:ime , jms:jme)           :: uphlmx
   REAL   , INTENT(IN   ) , DIMENSION(ims:ime , jms:jme)           :: wspdmx10   
   REAL   , INTENT(IN   ) , DIMENSION(ims:ime , jms:jme)           :: hail_afwa   
   REAL   , INTENT(IN   ) , DIMENSION(ims:ime , jms:jme)           :: torn_afwa
   REAL   , INTENT(IN   ) , DIMENSION(ims:ime , jms:jme)           :: br
   INTEGER, INTENT(IN   )                                          :: lsm_lays
   REAL   , INTENT(IN   ) , DIMENSION(ims:ime , lsm_lays, jms:jme) :: smcrel
   REAL   , INTENT(IN   ) , DIMENSION(ims:ime , lsm_lays, jms:jme) :: tslb
   REAL   , INTENT(IN   ) , DIMENSION( lsm_lays )                  :: soilthk
   REAL   , INTENT(IN   ) , DIMENSION(ims:ime , jms:jme)           :: landmask
   REAL   , INTENT(IN   ) , DIMENSION(ims:ime , jms:jme)           :: lakemask    
     
   !  Output variables                                                                                                         
                                                                                                                            
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , kms:kme , jms:jme)          :: refl_htrgen
   REAL   , INTENT(  OUT) ,  DIMENSION(num_press_levels)                     :: p_pl                                           
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , num_press_levels , jms:jme) :: u_pl,v_pl,t_pl,rh_pl,ght_pl,s_pl
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , num_press_levels , jms:jme) :: td_pl,q_pl
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , num_press_levels , jms:jme) :: wdir_pl,thta_pl,wwnd_pl,tcon_pl
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , num_press_levels , jms:jme) :: cconfr_pl,rnwtfr_pl,snowfr_pl,graufr_pl
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , num_press_levels , jms:jme) :: avor_pl,pvor_pl             
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: twet1                                                         
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: twmean
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: tw2                                                           
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: twbgt2
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: maskfrac                                                      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: qclay1
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: qilay1
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: qrlay1                                              
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: qslay1                                                        
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: qglay1                                                        
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: qnilay1                                                       
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rain                                                          
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: talay1                                                        
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: tnsfc                                                         
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: ta2                                                           
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: apta2                                                         
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: tdew2
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rhum2                                                            
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: tdsig1                                                        
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: ux10                                                          
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: vx10                                                          
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: wspd10
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: gspd10                                                           
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: slprs                                                         
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: cfraclo                                                       
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: cfracmd                                                       
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: cfrachi                                                       
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: z1000                                                         
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: snolev                                                        
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: mixlev                                                        
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: frzlev                                                        
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: z850                                                          
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: uwind850                                                      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: vwind850                                                      
   REAL   , DIMENSION(ims:ime , jms:jme)                    :: wspd850                                                         
   REAL   , DIMENSION(ims:ime , jms:jme)                    :: wdir850                                                         
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rh850                                                         
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: ta850                                                         
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: z700                                                          
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: uwind700                                                      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: vwind700                                                      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: wwind700                                                      
   REAL   , DIMENSION(ims:ime , jms:jme)                    :: wspd700                                                         
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: wdir700                                                       
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rh700                                                         
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: ta700                                                         
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: thtae700                                                      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: z500                                                          
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: uwind500                                                      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: vwind500                                                      
   REAL   , DIMENSION(ims:ime , jms:jme)                    :: wspd500                                                         
   REAL   , DIMENSION(ims:ime , jms:jme)                    :: wdir500                                                         
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rh500                                                         
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: ta500                                                         
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: z300                                                          
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: uwind300                                                        
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: vwind300                                                        
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: wspd300                                                       
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: wdir300                                                       
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rh300                                                           
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: ta300                                                           
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: z250                                                          
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: uwind250                                                      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: vwind250                                                      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: wspd250                                                       
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: wdir250                                                       
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rh250                                                         
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: ta250                                                         
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: dz1000_850                                                    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: dz1000_500                                                    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: dz850_700                                                     
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: avor850
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: avor700
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: avor500
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: avor250
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: snodpt                                                        
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rfl_1kmagl                                                    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rfl_maxagl
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rfl_mxcagl                                                         
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rflmpsc1km                                                    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rflmpscmax                                                    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: etp_18dbz                                                     
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: etp_mpsc                                                      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: vilh                                                          
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: radarvilh                                                     
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: cfractot                                                      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: mucape                                                        
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: cinh                                                          
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: mlcape                                                        
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: cinm                                                          
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: sbcape                                                        
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: cins                                                          
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: srh0_1km                                                      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: srh0_3km                                                      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: ehi_1km                                                       
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: ehi_3km                                                       
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: visib                                                         
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: ceiling                                                       
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: flghtreg                                                      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: fogflag   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: apta2c
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: qv2
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: spechum
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: soiltlay1   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: smrellay1
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: smreltot
   REAL   , INTENT(INOUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rain_last
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rain_rate
   REAL   , INTENT(INOUT) ,  DIMENSION(ims:ime , jms:jme)                    :: sn_nc_last
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: sn_nc_rate   
   REAL   , INTENT(INOUT) ,  DIMENSION(ims:ime , jms:jme)                    :: gr_nc_last
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: gr_nc_rate   
   REAL   , INTENT(INOUT) ,  DIMENSION(ims:ime , jms:jme)                    :: lq_nc_last
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: lq_nc_rate   

   REAL   , INTENT(INOUT) ,  DIMENSION(ims:ime , jms:jme)                    :: snowcv
   REAL   , INTENT(INOUT) ,  DIMENSION(ims:ime , jms:jme)                    :: graupelcv
   REAL   , INTENT(INOUT) ,  DIMENSION(ims:ime , jms:jme)                    :: liquidcv
   REAL   , INTENT(INOUT) ,  DIMENSION(ims:ime , jms:jme)                    :: sn_cv_last
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: sn_cv_rate   
   REAL   , INTENT(INOUT) ,  DIMENSION(ims:ime , jms:jme)                    :: gr_cv_last
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: gr_cv_rate   
   REAL   , INTENT(INOUT) ,  DIMENSION(ims:ime , jms:jme)                    :: lq_cv_last
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: lq_cv_rate      
   REAL   , INTENT(INOUT) ,  DIMENSION(ims:ime , jms:jme)                    :: radcv_last
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: radcv_rate   
   REAL   , INTENT(INOUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rascv_last
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rascv_rate   
   REAL   , INTENT(INOUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rannc_last
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rannc_rate     
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: ptyp_fip
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: ptyp_cat
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: cat_rain
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: cat_fzrn
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: cat_icep
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: cat_snow
   INTEGER, INTENT(INOUT) ,  DIMENSION(ims:ime , jms:jme)                    :: cu_act_last
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: cu_activ
   REAL   , INTENT(INOUT) ,  DIMENSION(ims:ime , jms:jme)                    :: cu_top_last
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: cu_top_m   
   REAL   , INTENT(INOUT) ,  DIMENSION(ims:ime , jms:jme)                    :: cu_bot_last
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: cu_bot_m   
   INTEGER, INTENT(INOUT) ,  DIMENSION(ims:ime , jms:jme)                    :: ishall_last           
   REAL   , INTENT(INOUT) ,  DIMENSION(ims:ime , jms:jme)                    :: cu_ctt_last   
   REAL   , INTENT(INOUT) ,  DIMENSION(ims:ime , jms:jme)                    :: cu_ktop_lst
   REAL   , INTENT(INOUT) ,  DIMENSION(ims:ime , jms:jme)                    :: cu_kbot_lst
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: raliqd
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rafzra
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rasnow
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: raicep
   REAL   , INTENT(INOUT) ,  DIMENSION(ims:ime , jms:jme)                    :: raliqdcum
   REAL   , INTENT(INOUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rafzracum
   REAL   , INTENT(INOUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rasnowcum
   REAL   , INTENT(INOUT) ,  DIMENSION(ims:ime , jms:jme)                    :: dysnowcum   
   REAL   , INTENT(INOUT) ,  DIMENSION(ims:ime , jms:jme)                    :: dysnratcu   
   REAL   , INTENT(INOUT) ,  DIMENSION(ims:ime , jms:jme)                    :: raicepcum
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: prate_inst                                                                                                                                  
                                                                                                                               
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: icgpot35 
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: sldpot35
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: icgpot30 
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: sldpot30  
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: icgpot25 
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: sldpot25
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: icgpot20 
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: sldpot20  
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: icgpot15 
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: sldpot15
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: icgpot10 
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: sldpot10       
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: icgpot05 
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: sldpot05
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: icgpot01 
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: sldpot01     

   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: turbdx01
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: turbdx02
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: turbdx03
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: turbdx04
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: turbdx05
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: turbdx06
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: turbdx07
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: llturb
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: llturblgt
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: llturbmdt
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: llturbsvr
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: fits
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: llws
   
   ! BAF   
   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: icgpot45 
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: sldpot45
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: icgpot40 
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: sldpot40     
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: uphelmax
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: wspd10mx
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: afw_hail
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: afw_torn
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: wbfrzlev      

   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: qv_col
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: cnvresmask

   LOGICAL, INTENT(INOUT)                                                    :: jstcald_htr 
   
   REAL   , DIMENSION(its:ite , num_press_levels , jts:jte) :: thtae_pl
   
   !! CAWO
   REAL   , INTENT(IN   ) ,  DIMENSION(ims:ime , jms:jme)                    :: mubase
   REAL   , INTENT(IN   ) ,  DIMENSION(ims:ime , jms:jme)                    :: mupert
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: wdir_10   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: pressfc
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: u10_earth
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: v10_earth
   REAL   , INTENT(INOUT) ,  DIMENSION(ims:ime , jms:jme)                    :: ratotlcum
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rafrzn   
   REAL   , INTENT(INOUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rafrzncum   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rafrznfr   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rafrzncumfr   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: dryairmtot
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: ta050      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: ta100      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: ta150      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: ta200      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: ta350      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: ta400      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: ta450      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: ta550      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: ta600      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: ta650      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: ta750      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: ta800      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: ta900      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: ta950      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: ta1000     
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: td050      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: td100      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: td150      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: td200      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: td250      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: td300      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: td350      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: td400      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: td450      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: td500      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: td550      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: td600      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: td650      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: td700      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: td750      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: td800      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: td850      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: td900      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: td950      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: td1000     
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rh050      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rh100      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rh150      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rh200      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rh350      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rh400      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rh450      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rh550      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rh600      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rh650      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rh750      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rh800      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rh900      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rh950      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rh1000     
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: qv050      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: qv100      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: qv150      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: qv200      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: qv250      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: qv300      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: qv350      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: qv400      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: qv450      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: qv500      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: qv550      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: qv600      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: qv650      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: qv700      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: qv750      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: qv800      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: qv850      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: qv900      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: qv950      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: qv1000      
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: wspd050    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: wspd100    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: wspd150    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: wspd200    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: wspd350    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: wspd400    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: wspd450
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: wspd550    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: wspd600    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: wspd650    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: wspd750    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: wspd800
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: wspd900    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: wspd950    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: wspd1000   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: w_dir050   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: w_dir100   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: w_dir150   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: w_dir200   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: w_dir350   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: w_dir400   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: w_dir450   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: w_dir500   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: w_dir550   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: w_dir600   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: w_dir650   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: w_dir750   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: w_dir800   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: w_dir850   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: w_dir900   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: w_dir950   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: w_dir1000  
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: theta050   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: theta100   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: theta150   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: theta200   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: theta250   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: theta300   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: theta350   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: theta400   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: theta450   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: theta500   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: theta550   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: theta600   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: theta650   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: theta700   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: theta750   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: theta800   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: theta850   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: theta900   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: theta950   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: theta1000  
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: z050       
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: z100       
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: z150       
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: z200       
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: z350       
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: z400       
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: z450       
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: z550       
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: z600       
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: z650       
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: z750       
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: z800       
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: z900       
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: z950       
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: wwind050   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: wwind100   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: wwind150   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: wwind200   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: wwind250
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: wwind300
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: wwind350   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: wwind400   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: wwind450   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: wwind500   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: wwind550   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: wwind600   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: wwind650   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: wwind750   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: wwind800   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: wwind850   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: wwind900   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: wwind950   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: wwind1000  
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: clconfrc050
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: clconfrc100
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: clconfrc150
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: clconfrc200
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: clconfrc250
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: clconfrc300
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: clconfrc350
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: clconfrc400
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: clconfrc450
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: clconfrc500
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: clconfrc550
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: clconfrc600
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: clconfrc650
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: clconfrc700
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: clconfrc750
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: clconfrc800
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: clconfrc850
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: clconfrc900
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: clconfrc950
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: clconfrc1000
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rnwatfrc050
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rnwatfrc100
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rnwatfrc150
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rnwatfrc200
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rnwatfrc250
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rnwatfrc300
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rnwatfrc350
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rnwatfrc400
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rnwatfrc450
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rnwatfrc500
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rnwatfrc550
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rnwatfrc600
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rnwatfrc650
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rnwatfrc700
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rnwatfrc750
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rnwatfrc800
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rnwatfrc850
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rnwatfrc900
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rnwatfrc950
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: rnwatfrc1000
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: snowfrc050 
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: snowfrc100 
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: snowfrc150 
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: snowfrc200 
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: snowfrc250 
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: snowfrc300 
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: snowfrc350 
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: snowfrc400 
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: snowfrc450 
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: snowfrc500 
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: snowfrc550 
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: snowfrc600 
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: snowfrc650 
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: snowfrc700 
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: snowfrc750 
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: snowfrc800 
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: snowfrc850 
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: snowfrc900 
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: snowfrc950 
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: snowfrc1000
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: grplfrc050 
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: grplfrc100 
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: grplfrc150 
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: grplfrc200 
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: grplfrc250 
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: grplfrc300 
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: grplfrc350 
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: grplfrc400 
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: grplfrc450 
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: grplfrc500 
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: grplfrc550 
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: grplfrc600 
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: grplfrc650 
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: grplfrc700 
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: grplfrc750 
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: grplfrc800 
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: grplfrc850 
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: grplfrc900 
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: grplfrc950 
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: grplfrc1000
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: avor050    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: avor100    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: avor150    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: avor200    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: avor300    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: avor350    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: avor400    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: avor450    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: avor550    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: avor600    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: avor650    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: avor750    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: avor800    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: avor900    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: avor950    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: avor1000   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: pvor050    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: pvor100    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: pvor150    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: pvor200    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: pvor250    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: pvor300    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: pvor350    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: pvor400    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: pvor450    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: pvor500    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: pvor550    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: pvor600    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: pvor650    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: pvor700    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: pvor750    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: pvor800    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: pvor850    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: pvor900    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: pvor950    
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: pvor1000
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: zpvor1_5   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: zpvor2_0   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: tpvor1_5   
   REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)                    :: tpvor2_0      

   !! CAWO pressure level and total for cloud condensate  
   REAL   :: TCONDW, tcondwu, tcondwd
   REAL   :: clconu, clcond, rnwatu, rnwatd, snowu, snowd, grplu, grpld
   REAL   :: clconp, rnwatp, snowp, grplp
   REAL   :: avor, ddp, dudp, dvdp, thetu,thetd, dthdp, dthdx, dthdy, dthz
   REAL, PARAMETER :: vcrit15 = 1.5
   REAL, PARAMETER :: vcrit20 = 2.0   
    
   REAL   , DIMENSION(its:ite , jts:jte)                    :: ccondfrc1
   REAL   , DIMENSION(its:ite , jts:jte)                    :: rainfrc1   
   REAL   , DIMENSION(its:ite , jts:jte)                    :: snowfrc1    
   REAL   , DIMENSION(its:ite , jts:jte)                    :: icefrc1

   !! TCONDW = "hydrometeors(qs_curr+qg_curr+qr_curr) + cloud_condensate(qc_curr + qi_curr) 
   !! "Cloud Fraction" = (qc_curr+qi_curr) / TCONDW
   !! "Rain Fraction"  = qr_curr / TCONDW
   !! "Snow Fraction"  = qs_curr / TCONDW
   !! "Ice Fraction"   = qg_curr / TCONDW 
   
   INTEGER :: p950,p900,p800,p750,p650,p600,p550,p450,p_400,p350,p200,p150,p100,p050
   INTEGER :: kv, kvl
   REAL   , DIMENSION(its:ite , jts:jte)                    :: wdir10                                                         
   REAL   , DIMENSION(its:ite , jts:jte)                    :: u10earth
   REAL   , DIMENSION(its:ite , jts:jte)                    :: v10earth
   REAL   , DIMENSION(its:ite , jts:jte)                    :: zpvor15
   REAL   , DIMENSION(its:ite , jts:jte)                    :: zpvor20
   REAL   , DIMENSION(its:ite , jts:jte)                    :: tpvor15
   REAL   , DIMENSION(its:ite , jts:jte)                    :: tpvor20
   REAL   , DIMENSION(its:ite , kts:kte , jts:jte)          :: potvort   

   !! End CAWO                                                  
                                                                                                                               
   !  Local variables                                                                                                          
                                                                                                                            
   REAL, PARAMETER :: eps = 0.622, t_kelvin = svpt0 , s1 = 243.5, s2 = svp2 , s3 = svp1*10., s4 = 611.0, s5 = 5418.12          
   REAL, PARAMETER :: bolton_con1 = log(s3)*s1
   REAL, PARAMETER :: bolton_con2 = s2+log(s3)
   REAL, PARAMETER :: drcp = 1./rcp
   REAL, PARAMETER :: lapse_dry = 0.0065, TC=273.16+17.5                                                                       
   REAL, PARAMETER :: expon = (r_d*lapse_dry)/g                                                                                
                                                                                                                               
   INTEGER, PARAMETER :: NEARSFC = 2                                                                                           
   REAL, PARAMETER :: DLAY = 1.0 / FLOAT( NEARSFC )                                                                            
   REAL, PARAMETER :: E2P  = 19.84659                                                                                          
   REAL, PARAMETER :: EZERO = 6.112                                                                                            
   REAL, PARAMETER :: D611 = 1.0 / EZERO                                                                                       
   REAL, PARAMETER :: C_1  = 0.0091379024                                                                                      
   REAL, PARAMETER :: C_2  = 6106.396                                                                                          
   REAL, PARAMETER :: C15  = 26.66082                                                                                          
   REAL, PARAMETER :: FW   = 0.0006355                                                                                         
   REAL, PARAMETER :: TFRZOUT = 263.15                                                                                         
   REAL, PARAMETER :: ESL2    = 29.65     !! also  ESLCON2                                                                     
   REAL, PARAMETER :: ESL1    = 17.67     !! also  ESLCON1                                                                     
   REAL, PARAMETER :: CTOK    = 273.15                                                                                         
   REAL, PARAMETER :: ESCR    = ESL1 * CTOK                                                                                    
   REAL, PARAMETER :: TKICE   = CTOK - 1.0                                                                                     
   REAL, PARAMETER :: TKLIQ   = CTOK + 2.0                                                                                     
   REAL, PARAMETER :: DFRZ    = 1.0 / ( TKLIQ - TKICE )                                                                        
   REAL, PARAMETER :: DZLAP   = 1000.0 / 6.5                                                                                   
   REAL, PARAMETER :: CUDT_MIN = .999998                                                                                                                               

   ! The following for wind gust estimation; based on ECMWF
   
   REAL            :: p_at_050up, p_at_150up, wspd_850g, wspd_950g, convshr_gust, wspd_pbl, hpbl, hfac, gust_delt
   REAL            :: gfac,wfac, glim400, glim1000
   INTEGER         :: kl_050, kl_150, pblk
   
   REAL            :: GFU, GFS, g_slop, rich_fac, gustfac

   ! The following for simulated reflectivity calculation                                                                      

   REAL,    DIMENSION(kts:kte)                                              :: z_e,dBZ                                             

   REAL            :: GONV, RONV, SONV                                                                                         
   REAL            :: FACTORB_G, FACTORB_R, FACTORB_S                                                                          
   REAL            :: TEMP_C                                                                                                   
   REAL            :: qra,qsn,qgr                                                                                              
   REAL            :: zagld, zaglu, dlz,facd,facu                                                                              
   INTEGER         :: kuu, kdd                                                                                                 

   REAL, PARAMETER :: PI = 3.141592653589793                                                                                   
   REAL, PARAMETER :: RHOWAT = 1000.                                                                                           
   REAL, PARAMETER :: RD = 287.04                                                                                              
   REAL, PARAMETER :: AGL1KM = 1000. !! meters above ground                                                                    

   !   Constants used to calculate variable intercepts
   REAL, PARAMETER :: R1 = 1.E-15                                                                                              
   REAL, PARAMETER :: RON = 8.E6                                                                                               
   REAL, PARAMETER :: RON2 = 1.E10                                                                                             
   REAL, PARAMETER :: SON = 2.E7                                                                                               
   REAL, PARAMETER :: GON = 5.E7                                                                                               
   REAL, PARAMETER :: RON_MIN = 8.E6                                                                                           
   REAL, PARAMETER :: RON_QR0 = 0.00010                                                                                        
   REAL, PARAMETER :: RON_DELQR0 = 0.25*RON_QR0                                                                                
   REAL, PARAMETER :: RON_CONST1R = (RON2-RON_MIN)*0.5                                                                         
   REAL, PARAMETER :: RON_CONST2R = (RON2+RON_MIN)*0.5                                                                         
   !   Other constants                                                                                                         
   REAL, PARAMETER :: GAMMA_SEVEN = 720.                                                                                       
   REAL, PARAMETER :: RHO_R = RHOWAT           ! 1000. KG M^-3                                                                 
   REAL, PARAMETER :: RHO_S = 100.             ! KG M^-3                                                                       
   REAL, PARAMETER :: RHO_G = 400.             ! KG M^-3                                                                       
   REAL, PARAMETER :: ALPHA = 0.224                                                                                            

   REAL, PARAMETER :: FACTOR_R = GAMMA_SEVEN * 1.E18 * (1./(PI*RHO_R))**1.75                                                   
   REAL, PARAMETER :: FACTOR_S = GAMMA_SEVEN * 1.E18 * (1./(PI*RHO_S))**1.75 * (RHO_S/RHOWAT)**2 * ALPHA                       
   REAL, PARAMETER :: FACTOR_G = GAMMA_SEVEN * 1.E18 * (1./(PI*RHO_G))**1.75 * (RHO_G/RHOWAT)**2 * ALPHA                       

   REAL    :: tva, rhoair, maxk_ze, ze_scheme, mxck_ze, rfl_zr                                                                                  
   REAL   , DIMENSION(its:ite , jts:jte)                    :: z_e_1km                                                         
   REAL   , DIMENSION(its:ite , jts:jte)                    :: z_e_max
   REAL   , DIMENSION(its:ite , jts:jte)                    :: z_e_mxc                                                            

   REAL, PARAMETER :: VILFAC = (0.00344 * 0.001) !! See comment below: search for AFWA                                         

   !! End simulated reflectivity                                                                                               

   !! The following for CAPE and CONVECTIVE INDEX calcuations                                                                  

   REAL, DIMENSION(150)                                           :: BUOY, ZREL, BENACCUM                                      

   REAL, PARAMETER :: THTECON1 = 3376.                                                                                         
   REAL, PARAMETER :: THTECON2 = 2.54                                                                                          
   REAL, PARAMETER :: THTECON3 = 0.81                                                                                          
   REAL, PARAMETER :: TLCLC1 = 2840.                                                                                           
   REAL, PARAMETER :: TLCLC2 = 3.5                                                                                             
   REAL, PARAMETER :: TLCLC3 = 4.805                                                                                           
   REAL, PARAMETER :: TLCLC4 = 55.                                                                                             
   REAL, PARAMETER :: GAMMA_RIP = RD/CP                                                                                        
   REAL, PARAMETER :: RM = .608                                                                                                
   REAL, PARAMETER :: CPMD = 0.887                                                                                             
   REAL, PARAMETER :: GAMMAMD = RM-CPMD                                                                                        
   !! REAL, PARAMETER :: DAVG = 500. !! meters                                                                                 
   REAL, PARAMETER :: DAVG = 10. !! meters                                                                                     
   REAL, PARAMETER :: DAVGML = 250. !! meters                                                                                  

   REAL    :: ev, ethmax, tlcl, eth, pavg, p22, p11, totthe, totqvp, totprs                                                    
   REAL    :: pup, pdn, thq, pp1, pp2, deltap                                                                                  
   REAL    :: prmbmx, qqmax, tkmax, qvppari, tmkpari, ethpari, ghtmax, zlcl                                                    
   REAL    :: qvplift, tmklift, tvenv, tvlift, ghtlift, eslift                                                                 
   REAL    :: facden, fac1, fac2, tmkenv, qvpenv, benamin                                                                      
   REAL    :: totght, dprs, prbmix, ghtmix, qvpmix, tmkmix, prbmxl, qvpmxl, tkamxl                                             
   INTEGER :: kmxte,cpm,ilcl,klcl,kmax,kel,klfc,kc                                                                             
   INTEGER :: klmix                                                                                                            
   INTEGER :: ktyp                                                                                                             

   !! End CAPE and CONVECTIVE INDEX items                                                                                      

   CHARACTER*256 MESG                                                                                                          
   LOGICAL LDBG                                                                                                                
   LOGICAL l1, l2, l3, found, kzfnd, lfog                                                                                            
                                                                                                                            
   INTEGER :: i, j, ke, kp, ke_h, ke_f,pdex,p1000,p850,p700,p500,p300,p250,k,klo,khi,kk,kl                                     
   REAL    :: pu, pd, pm , &                                                                                                   
              tu, td     , &                                                                                                   
              su, sd     , &                                                                                                   
              uu, ud     , &                                                                                                   
              vu, vd     , &                                                                                                   
              wu, wd     , &                                                                                                   
              zu, zd     , &                                                                                                   
              qu, qd, qm , &                                                                                                   
              eu, ed, em , &                                                                                                   
              du, dd,      &
              avu,avd                                                                                                           
   REAL    :: es, qs                                                                                 
                                                                                                                               
   REAL    :: p1,plo,phi,zlo,zhi,tlo,thi,t_sea_level,t_surf,z_half_lowest                                                      
   REAL    :: p_at_pconst, t_at_pconst, z_at_pconst, pwke,tkel                                                                 
   REAL    :: ptarget,dp,dpmin,tbotextrap,tvbotextrap,zbot,pbot,prku,zlev                                                      
   REAL    :: q1,e1,e2m,q2m,ee,ss,tw,ew,de,der,elog,tdsig,p2m,tw2m,ed2,ee2,d2m                                                                      
   REAL    :: tht2m
   REAL    :: qq,tdh                                                                                                           
   REAL    :: bot,dt,dz,dxtr,dytr,ds,dzf                                                                                       
   INTEGER :: kup,kin,n,l,istart,jstart,kclo,kcmi,kchi                                                                         

   !! Apparent Temperature Calculation                                                                                         

   !! REAL, PARAMETER :: C1A = -42.38                                                                                             
   !! REAL, PARAMETER :: C2A = 2.049                                                                                              
   !! REAL, PARAMETER :: C3A = 10.14                                                                                              
   !! REAL, PARAMETER :: C4A = -0.2248                                                                                            
   !! REAL, PARAMETER :: C5A = -0.006838                                                                                          
   !! REAL, PARAMETER :: C6A = -0.05482                                                                                           
   !! REAL, PARAMETER :: C7A = 0.001228                                                                                           
   !! REAL, PARAMETER :: C8A = 0.0008528                                                                                          
   !! REAL, PARAMETER :: C9A = -0.00000199
   
   REAL, PARAMETER :: C1A = -42.379                                                                                            
   REAL, PARAMETER :: C2A = 2.04901523                                                                                             
   REAL, PARAMETER :: C3A = 10.14333127                                                                                              
   REAL, PARAMETER :: C4A = -0.22475541                                                                                            
   REAL, PARAMETER :: C5A = -0.00683783                                                                                          
   REAL, PARAMETER :: C6A = -0.05481717                                                                                          
   REAL, PARAMETER :: C7A = 0.00122874                                                                                           
   REAL, PARAMETER :: C8A = 0.00085282                                                                                          
   REAL, PARAMETER :: C9A = -0.00000199
   REAL, PARAMETER :: INV17 = 1.0 / 17.0                                                                                          

   REAL    :: hadj
   REAL    :: tcel2,layspd,resid,es2,qs2                                                                                       

   !! End apparent temperature Calculation                                                                                     

   !! Wet-bulb Globe temperature Calculation
   
   REAL    :: wspd2, tnwb2, eps_a, bterm, cterm, grayterm, tglob2, twcel2
   
   REAL, PARAMETER :: loglaw2 = 0.30103
   !! REAL, PARAMETER :: swcoef = 0.021      !! from Hunter and Minyard
   REAL, PARAMETER :: swcoef = 0.0021     !! probable typo in H&M
   REAL, PARAMETER :: w2coef = 0.42
   REAL, PARAMETER :: tnwint = 1.93
   
   REAL, PARAMETER :: fr_dir = 0.67
   REAL, PARAMETER :: fr_dif = 0.33
   REAL, PARAMETER :: coszen_min = 1.0e-4
   REAL, PARAMETER :: alph_sps = 0.05
   REAL, PARAMETER :: alph_spl = 0.05
   REAL, PARAMETER :: epsa_coef = 0.575
   REAL, PARAMETER :: one_7th = 1.0/7.0
   REAL, PARAMETER :: stbolt95 = 0.95*STBOLT
   REAL, PARAMETER :: dstbol1_2 = 1.2 / STBOLT
   REAL, PARAMETER :: dstbolt = 1.0 / STBOLT
   REAL, PARAMETER :: bs1term =  fr_dir * 0.25 * dstbolt
   REAL, PARAMETER :: bs2term =  fr_dif * dstbol1_2
   REAL, PARAMETER :: h_coef = 0.315
   REAL, PARAMETER :: c1term = h_coef / stbolt95
   REAL, PARAMETER :: upow = 0.58
   REAL, PARAMETER :: numer_term = 7680000.00
   REAL, PARAMETER :: denom_term = 256000.00      

   !! Aviation related calculations                                                                                            

   REAL,    PARAMETER :: FT_PER_METER = 3.28084                                                                                
   REAL,    PARAMETER :: METER_PER_FT = 1.0/FT_PER_METER           
   INTEGER, PARAMETER :: FIPLEVS = 45                                                            
   REAL,    PARAMETER :: FLVINTVFT = 1000.0        !! flight level interval in feet                                            
   REAL,    PARAMETER :: FLVINTVMT = FLVINTVFT/FT_PER_METER                                                                    
   REAL,    PARAMETER :: TRANS_HT = 17990.0        !! transition height in feet
                                                                                                                               
   REAL, PARAMETER :: clgfac  = 1.0                                                                                            
   REAL, PARAMETER :: coeflc  = 144.7                                                                                          
   REAL, PARAMETER :: coeflp  = 2.24                                                                                           
   REAL, PARAMETER :: coeffc  = 327.8                                                                                          
   REAL, PARAMETER :: coeffp  = clgfac*10.36                                                                                   
   REAL, PARAMETER :: exponlc = 0.8800                                                                                         
   REAL, PARAMETER :: exponlp = 0.7500                                                                                         
   INTEGER, PARAMETER :: exponfc = 1                                                                                           
   !! REAL, PARAMETER :: exponfp = 0.7776                                                                                      
   INTEGER, PARAMETER :: exponfp = 1        !! revised: Roy Rasmussen - Dec 2003: snow-effect exponent                         
   REAL, PARAMETER :: exponfg = 0.75                                                                                           
   REAL, PARAMETER :: thcon   = .02                                                                                            
   REAL, PARAMETER :: const1  = -log(thcon)                                                                                    
   REAL, PARAMETER :: rhoice  = 917.0                                                                                          
   REAL, PARAMETER :: g_per_kg = 1000.                                                                                         
   REAL, PARAMETER :: km_per_m = .001                                                                                          
   REAL, PARAMETER :: m_per_km = 1000.                                                                                         
   REAL, PARAMETER :: CEILMAX = 9000.0                                                                                         
   REAL, PARAMETER :: CEILBND = 8999.99                                                                                        

   REAL,    PARAMETER :: vden_fogkm = 0.001 * ((.10*5280.0) / FT_PER_METER) !! approx. .1609km for very dense fog
   REAL,    PARAMETER :: den_fog_km = 0.001 * ((.25*5280.0) / FT_PER_METER) !! approx. .402km for dense fog threshold
   REAL,    PARAMETER :: mod_fog_km = 0.001 * ((1.0*5280.0) / FT_PER_METER) !! approx. 1.609km meters for MOD fog threshold
   REAL,    PARAMETER :: lgt_fog_km = 0.001 * ((3.0*5280.0) / FT_PER_METER) !! approx. 4.828km meters                                                                                                                            

   REAL     bextclw, bextcli, dta2, prk2, delt2, wgtb, dz3
   REAL,    PARAMETER :: bext10th = const1 / vden_fogkm       !! 24.30818   
   REAL,    PARAMETER :: bext_den = const1 / den_fog_km       !! 9.723273
   REAL,    PARAMETER :: bext_mod = const1 / mod_fog_km       !! 2.430818
   REAL,    PARAMETER :: bext_lgt = const1 / lgt_fog_km       !! 0.8102728
   REAL,    PARAMETER :: bext_max = coeflc * 0.25 ** exponlc  !! 0.25 g/m3 water max in surface-based cloud for fog purposes
                                                              !! results in bext_max  =   42.72241
   
   REAL,    PARAMETER :: cllwcvden = exp(  log(bext10th/coeflc) / exponlc ) !!  0.1317167 g/m3
   REAL,    PARAMETER :: cllwc_den = exp(  log(bext_den/coeflc) / exponlc ) !! .046498220 g/m3
   REAL,    PARAMETER :: cllwc_mod = exp(  log(bext_mod/coeflc) / exponlc ) !! .0096222628 g/m3    
   REAL,    PARAMETER :: cllwc_lgt = exp(  log(bext_lgt/coeflc) / exponlc ) !! .0027611752 g/m3
   
   REAL,    PARAMETER :: cliwc_den = exp(  log(bext_den/coeffc) / exponfc ) 
   REAL,    PARAMETER :: cliwc_mod = exp(  log(bext_mod/coeffc) / exponfc )   
   REAL,    PARAMETER :: cliwc_lgt = exp(  log(bext_lgt/coeffc) / exponfc )
   REAL,    PARAMETER :: exp_denom = ( 1.0 - exp( -3.0 ) )
   
   ! New Fog stuff V421 and following: 
   REAL    :: thetv2,thetv1,thtv2m,z_at_w2,delt1
   LOGICAL :: l_lay2_1_temp_invers
   LOGICAL :: l_lay2_1_statically_stable
   LOGICAL :: l_sfc_lay_stable
   LOGICAL :: l_spd10_vlight 
   LOGICAL :: l_spd10_light
   LOGICAL :: l_2m_LCL_in_lay1
   LOGICAL :: l_eqv_pseudo_stable
   LOGICAL :: lfog_weak, lfog_strong_wind, lfog_light_wind 
   
   ! Transitions between flight categories:                                                                                    
   ! Ceiling:     500 ft, 1000 ft, 3000 ft                                                                                     
   ! Visibility: 1 mile, 3 miles, 5 miles                                                                                      
   ! Values given are in meters for ceiling and km for visibility.                                                             

   REAL, PARAMETER :: clg1 = 152.4                                                                                             
   REAL, PARAMETER :: clg2 = 304.8                                                                                             
   REAL, PARAMETER :: clg3 = 914.4                                                                                             
   REAL, PARAMETER :: vis1 = 1.609                                                                                             
   REAL, PARAMETER :: vis2 = 4.828                                                                                             
   REAL, PARAMETER :: vis3 = 8.046                                                                                             

   REAL, PARAMETER :: coeffg = 4.0                                                                                             
   REAL, PARAMETER :: COEFFP_dry = 10.0 !! original was 17.7 Roy Rasmussen                                                     
   REAL, PARAMETER :: COEFFP_wet =  6.0 !! original was 4.18 Roy Rasmussen                                                     

   REAL,    DIMENSION( kts:kte )                   :: qclw, qclice, qctot, qrtot, qcond, qrlw
   REAL,    DIMENSION( kts:kte )                   :: conclc, concfc, conclp, concfp, concfg                                
   REAL    :: vovermd                                                                       
   REAL    :: qsnow, qgraup                                                                                          
   REAL    :: rhmax, qrh, visrh, temp_fac, coef_snow                                                                           
   REAL    :: exting, extingt, bextmx, vis_night, zen_fac, logterm                                                             
   REAL    :: FACA, FACN, FACR                                                                                                 
   LOGICAL :: clgfound, lkfnd, mkfnd, hkfnd                                                                                    
   REAL    :: shear_fac
   
   !! Wet-bulb Zero
   
   LOGICAL :: wbzfound
   REAL    :: faczk, zhtlw, dzht
   
   !! New 03km cloud fractional coverage scheme.
   
   REAL    :: cld_cond, rhfrac, cfrlomax, cfrmdmax, cfrhimax
   REAL,    DIMENSION( kts:kte )                   :: cfrlow, cfrmed, cfrhgh, cfrlay1                                                                                                            
                                                                                                                               
   !! Forecast Icing Potential (FIP) and SLD Aviation calculations                                                             
   !! include explicit microphysics package specific internal reflectivity results as a                                        
   !! bonus                                                                                                                    

   !! ptyp_fip type definitions                                                                                                
   !! 1: rain (RA); 2: snow (SN); 3: ice pellets (PE);                                                                         
   !! 4: freezing rain (FZRA); 5: Drizzle (DZ); 6: freezing drizzle (FZDZ)                                                     
                                                                                                                               
   !! Integer version of ptyp_fip
   
   INTEGER, DIMENSION(its:ite , jts:jte) :: ptyp_fipi   
   
   INTEGER, PARAMETER :: NONE = 0                                                                                              
   INTEGER, PARAMETER :: RAAN = 1                                                                                              
   INTEGER, PARAMETER :: SNOW = 2                                                                                              
   INTEGER, PARAMETER :: PELT = 3                                                                                              
   INTEGER, PARAMETER :: FZRA = 4                                                                                              
   INTEGER, PARAMETER :: DZZL = 5                                                                                              
   INTEGER, PARAMETER :: FZDZ = 6                                                                                              

   INTEGER :: k300,kclg2,k250,kclg1                                                                                            
   INTEGER :: kcitop_hi,kcibas_lo
   INTEGER :: kcibas( 10 ), kcitop( 10 )
   INTEGER :: ncilays, clay_iter, iter_max, ires_typ
   INTEGER :: ifip_situ
   REAL,  DIMENSION(its:ite , jts:jte) :: rain_this

   !! ifip_situ: Forecast Icing Potential (FIP) Major Situational Types:
   !!  (1) Clear-sky and no parameterized convection                                    
   !!  (2) Isolated shallow parameterized convection despite no resolved cld layers     
   !!  (3) Isolated deep parameterized convection despite no resolved cld layers        
   !!  (4) Single-layer clouds; no precip or DZ/RA with CTT < -12C                      
   !!  (5) Single-layer clouds; SNOW-only at the surface                                
   !!  (6) Single-layer clouds; primarily FZDZ,FZRA,PELT at sfc and CTT > -30C          
   !!  (7) Single-layer clouds; primarily FZDZ,FZRA,PELT at sfc and CTT < -30C          
   !!  (8) Single-layer clouds; CTT >= -12C; DZ/RA with T2 > -2C and no SNOW            
   !!  (9) Single-layer clouds plus param conv (i.e. CTT gradient); no precip or DZ/RA with CTT < -12C    
   !! (10) Single-layer clouds plus param conv (i.e. CTT gradient); SNOW-only at the surface  
   !! (11) Single-layer clouds plus param conv (i.e. CTT gradient); primarily FZDZ,FZRA,PELT at sfc and CTT > -30C    
   !! (12) Single-layer clouds plus param conv (i.e. CTT gradient); primarily FZDZ,FZRA,PELT at sfc and CTT < -30C   
   !! (13) Single-layer clouds plus param conv (i.e. CTT gradient); CTT >= -12C; DZ/RA with T2 > -2C and no SNOW
   !! (14) Multpl-layer clouds lowest deck; no precip or DZ/RA with CTT < -12C                             
   !! (15) Multpl-layer clouds lowest deck; SNOW-only at the surface                                       
   !! (16) Multpl-layer clouds lowest deck; primarily FZDZ,FZRA,PELT at sfc and CTT > -30C                 
   !! (17) Multpl-layer clouds lowest deck; primarily FZDZ,FZRA,PELT at sfc and CTT < -30C                 
   !! (18) Multpl-layer clouds lowest deck; CTT >= -12C; DZ/RA with T2 > -2C and no SNOW                   
   !!      ** Multpl-layer clouds higher decks; treat as no precip because of presence of dry layer        
   !!      ** Multpl-layer clouds btween decks; conditions related to deck above, use CTTmap from it.       
   !! (19) Multpl-layer clouds plus param conv (i.e. CTT gradient) lowest deck; no precip or DZ/RA with CTT < -12C  
   !! (20) Multpl-layer clouds plus param conv (i.e. CTT gradient) lowest deck; SNOW-only at the surface 
   !! (21) Multpl-layer clouds plus param conv (i.e. CTT gradient) lowest deck; primarily FZDZ,FZRA,PELT at sfc and CTT > -30C   
   !! (22) Multpl-layer clouds plus param conv (i.e. CTT gradient) lowest deck; primarily FZDZ,FZRA,PELT at sfc and CTT < -30C
   !! (23) Multpl-layer clouds plus param conv (i.e. CTT gradient) lowest deck; CTT >= -12C; DZ/RA with T2 > -2C and no SNOW  
   !!      ** Multpl-layer clouds plus param conv (i.e. CTT gradient) higher decks: as in 14-18 for resolved        
   !!      ** Multpl-layer clouds plus param conv (i.e. CTT gradient) btween decks: as in 14-18 for resolved          
   !! (24) Classic FRZA structure, single layer        
   !! (25) Classic FRZA structure, multiple layers     
   !! (26) Classic FRZA structure, single layer plus param convection
   !! (27) Classic FRZA structure, multiple layers plus param convection

   LOGICAL :: kdry,kwet,all_snow,kwarm,kfrzr,iso_deepr,iso_shalr,mlt_deepr
   REAL    :: cbot_tc( 10 ),ctop_tc( 10 ), cbot_ht( 10 ), ctop_ht( 10 )
   REAL    :: maxk_mpscdbz
   REAL    :: radcv_this, rascv_this, ratotcv_this                                                                                                        
   REAL    :: DDT_PLD !! 1.0/dt_pld                                                                                            
   REAL    :: kcitop_rh, kcibot_rh, kcilay_rh
   REAL    :: ravg, rrat
   REAL    :: ctop_cnvct,ctta_cnvct,cbot_cnvct,cdpt_cnvct,wght1,wght2,frc_convect,ctt
   REAL    :: cttmap, tmap, cnvtmap, rhmap, slwmap, pirepmap, vvmap, qpfmap   
   INTEGER :: cshl_cnvct,kbot_cnvct,ktop_cnvct,nlays,ktop,kbot,ml,fl
   INTEGER :: kconv
   REAL    :: cdepk,w0absmax
   LOGICAL :: lresdcon,lresdshal
   INTEGER :: lresdtyp !! 1: isolated; 2: resolved deep conv. plus additional layer(s)
   INTEGER :: kbot_cnvcr,ktop_cnvcr
   LOGICAL, SAVE :: kfshonly = .false.
   LOGICAL, SAVE :: gfshonly = .false.   
   LOGICAL, SAVE :: ntshonly = .false.
   
   REAL, PARAMETER :: MM_PER_100HRS = 2.7778e-6 !! rate in mm/second
   REAL, PARAMETER :: MM_PER_10HRS = 2.7778e-5 !! rate in mm/second
   REAL, PARAMETER :: MM_PER_2HRS = 1.38889e-4 !! rate in mm/second
   REAL, PARAMETER :: INCH_PER_HR = .007055556 !! rate in mm/second
   REAL, PARAMETER :: PRLIMRCONV = 0.1 *INCH_PER_HR !! .1 inch per hour in mm/second 
   REAL, PARAMETER :: CTOP_RH = 70.0
   REAL, PARAMETER :: CBOT_RH = 80.0
   REAL, PARAMETER :: CLAY_RH = 75.0

   !! Cloud fraction layer definitions                                                                                         

   REAL, PARAMETER :: clhtlw = clg1   !! 500ft == clg1                                                                         
   REAL, PARAMETER :: clhtmd = 1981.2 !! 6500ft                                                                                
   REAL, PARAMETER :: clhthi = 6096.0 !! 20000ft
   REAL, PARAMETER :: chig_slope = -0.11461
   REAL, PARAMETER :: chig_intcp = 2048.0 !! meters
   REAL, PARAMETER :: chi_breakpt = 4572.0 !! meters; above which utilize piecewise linear equation.                                                                              

   REAL, PARAMETER :: cfwght_low = (clhtmd-clhtlw)/(clhthi-clhtlw)                                                             
   REAL, PARAMETER :: cfwght_med = (clhthi-clhtmd)/(clhthi-clhtlw)                                                             

   REAL    :: cl_lowht, cl_medht, cl_hight  !! "terrain-following" layer heights for low, medium, high                         
                                                                                                                               
   REAL,    DIMENSION( kts:kte )                   :: icg_pot1d, sld_pot1d  
   REAL,    DIMENSION(its:ite , kts:kte , jts:jte) :: icg_pot3d, sld_pot3d

   REAL,    PARAMETER :: echo_top_maxdbz = 17.999 !! reflectivities in column must exceed this to search for echo tops         
   REAL     echo_htkm,facdn, facup,refl_genk, refl_genk1, refl_mpsk, refl_mpsku, refl_mpskd                                                                        
   INTEGER  kecho                                                                                                               
                                                                                                                               
   REAL,    SAVE, DIMENSION( FIPLEVS )  :: fl_mt, fl_ft
   INTEGER, SAVE                        :: ktrans
   REAL,    DIMENSION(its:ite , FIPLEVS , jts:jte) :: icg_potlv, sld_potlv 
   LOGICAL  kflfnd( FIPLEVS ), facd_big
   REAL     facfld( FIPLEVS ), facflu( FIPLEVS ), zmsld, zmslu, zstdd, zstdu 
   INTEGER  kfldd( FIPLEVS ), kfluu( FIPLEVS )  
   REAL     catsum,dzh
   REAL     rain_sum
   LOGICAL  lbranch225, lbranch222
   
   !! Integer version of ptyp_cat
   
   INTEGER, DIMENSION(its:ite , jts:jte) :: ptyp_cati
   
   INTEGER, PARAMETER :: CAT_NONE = 0
   INTEGER, PARAMETER :: CAT_RAAN = 1
   INTEGER, PARAMETER :: CAT_FZRA = 2   
   INTEGER, PARAMETER :: CAT_PELT = 3   
   INTEGER, PARAMETER :: CAT_SNNW = 4
   INTEGER, PARAMETER :: CAT_MIXD = 5

   !! End aviation-related calculations                                                                                        
                                                                                                                               
   REAL,    DIMENSION(its:ite , jts:jte)                                    :: t1                                              
   REAL,    DIMENSION(its:ite , jts:jte)                                    :: rh1                                             
   REAL,    DIMENSION(its:ite , jts:jte)                                    :: sp1                                             
   REAL,    DIMENSION(its:ite , jts:jte)                                    :: td1                                             
   REAL,    DIMENSION(its:ite , jts:jte)                                    :: tw1                                             
   REAL,    DIMENSION(its:ite , jts:jte)                                    :: td2                                             
   REAL,    DIMENSION(its:ite , jts:jte)                                    :: twm                                             
   REAL,    DIMENSION(its:ite , jts:jte)                                    :: tns                                             
   REAL,    DIMENSION(its:ite , jts:jte)                                    :: lfr                                             
   REAL,    DIMENSION(its:ite , jts:jte)                                    :: snl                                             
   REAL,    DIMENSION(its:ite , jts:jte)                                    :: mxl                                             
   REAL,    DIMENSION(its:ite , jts:jte)                                    :: frl                                             
   REAL,    DIMENSION(its:ite , jts:jte)                                    :: mucap                                           
   REAL,    DIMENSION(its:ite , jts:jte)                                    :: mucin                                           
   REAL,    DIMENSION(its:ite , jts:jte)                                    :: mlcap                                           
   REAL,    DIMENSION(its:ite , jts:jte)                                    :: mlcin                                           
   REAL,    DIMENSION(its:ite , jts:jte)                                    :: sbcap                                           
   REAL,    DIMENSION(its:ite , jts:jte)                                    :: sbcin                                           
   REAL,    DIMENSION(its:ite , jts:jte)                                    :: srh01                                           
   REAL,    DIMENSION(its:ite , jts:jte)                                    :: srh03                                           
   REAL,    DIMENSION(its:ite , jts:jte)                                    :: ehi01                                           
   REAL,    DIMENSION(its:ite , jts:jte)                                    :: ehi03                                           
   REAL,    DIMENSION(its:ite , jts:jte)                                    :: tf2                                             
   REAL,    DIMENSION(its:ite , jts:jte)                                    :: rh2                                             
   REAL,    DIMENSION(its:ite , jts:jte)                                    :: aptemp                                          
   REAL,    DIMENSION(its:ite , jts:jte)                                    :: vis                                             
   REAL,    DIMENSION(its:ite , jts:jte)                                    :: clg                                             
   REAL,    DIMENSION(its:ite , jts:jte)                                    :: freg                                            

   REAL, DIMENSION(its:ite , jts:jte)                                       :: lowcht                                          
   REAL, DIMENSION(its:ite , jts:jte)                                       :: midcht                                          
   REAL, DIMENSION(its:ite , jts:jte)                                       :: higcht                                          
   REAL, DIMENSION(its:ite , jts:jte)                                       :: spdshr                                          
   REAL, DIMENSION(its:ite , jts:jte)                                       :: optd                                            
   INTEGER, DIMENSION(its:ite , jts:jte)                                    :: level                                           

   REAL, DIMENSION(its:ite , jts:jte)                                       :: qcol

   REAL,    DIMENSION(kts:kte)                                              :: twh, tbh, rhk                                   
   REAL,    DIMENSION(kts:kte)                                              :: z_at_w, zht, prmb, dta, dtc, vvubs              
   REAL,    DIMENSION(kts:kte)                                              :: dz8w                                            
   REAL,    DIMENSION(kts:kte)                                              :: ucrsk,vcrsk                                  
   REAL                                                                     :: ucrs10,vcrs10,srh1,srh3,uearth,vearth,wdir,pmmb               
   REAL,    DIMENSION(kts:kte)                                              :: bext                                            
                                                                                                                               
   !! Additions for dynamic snow density algorithm based on ..... Cobb
   !! and Waldstreicher, 2005
   
   real evs, eis, qvi, prspa, rhice, rhwat, qvs, maxw, dmaxw
   real,    dimension(kts:kte)                                              :: wgtlays
   real,    dimension(kts:kte)                                              :: sndent
   integer, dimension(kts:kte)                                              :: klused
   integer, dimension(kts:kte)                                              :: wwgt_lay
   integer, dimension(kts:kte)                                              :: ncld_lay          
   real wgtsum, dttbl, snowratio, dyrasnow
   integer jj, ncbl, nvvskip, nclskip, nlvskip, nvvlay, ncldlay
   logical l_vvweight

   !! End Dynamic Snow Density Algorithm

   !! Soil-related   
   real totthik   
   !! End soil

   !! Statement Functions                                                                                                      

   REAL VIRTUAL, TMP, RMIX                                                                                                     
   VIRTUAL( TMP, RMIX ) = TMP*(EPS+RMIX)/(EPS*(1.+RMIX))

   REAL FINAL_UP, INIT_INTERST, SLW_MAP, PIREP_MAP, VV_MAP
   FINAL_UP( INIT_INTERST, SLW_MAP, PIREP_MAP, VV_MAP ) = INIT_INTERST + (1.0 - INIT_INTERST)*    &
                                                      (0.4*SLW_MAP + 0.35*PIREP_MAP + 0.25*VV_MAP)
   REAL FINALDWN                                                      
   FINALDWN( INIT_INTERST, SLW_MAP, PIREP_MAP, VV_MAP ) = INIT_INTERST*(1.0 + 0.25*VV_MAP) + (1.0 - INIT_INTERST)*  &                                                  
                                                      (0.4*SLW_MAP + 0.35*PIREP_MAP)
   REAL ZSTDATM, PFULL
   ZSTDATM( PFULL ) = 44330.8 - 4946.54*PFULL**0.1902632  !! CRC Handbook
         
   !...............................................................................................                            
   !                                                                                                                           
   !  Note on vertical indexing: Full-levels go 1:KME (e.g. 1-32, for a 31-layer model), with                                  
   !  full-level 1 at the terrain surface (bottom of lowest full level) and full-level KME at                                  
   !  the top of the upper most layer)                                                                                         
   !                                                                                                                           
   !  Silly, but transfer the small namelist.input array into the grid structure for output purposes.                          
   
   WRITE( MESG, '( A,F15.6 )' ) 'Inside PLD at current time: ',curr_time
   !! CALL wrf_message(MESG)
   
   jstcald_htr = .TRUE.  !! for passing back into KFETA, if that is being used.
   
   if( firstime ) then  !! dot-pt vars go 1 to ide; cross-pt 1 to ide-1
       WRITE( MESG, '( A,4I8 )' ) 'ITS,ITE,JTS,JTE: ',its,ite,jts,jte
       !! CALL wrf_message(MESG)
       if( ite .eq. ide ) then
          WRITE( MESG, '( A,2I8 )' ) 'ide-1,ite=',ide-1,ite
          !! CALL wrf_message(MESG)
       endif
       if( jte .eq. jde ) then
          WRITE( MESG, '( A,2I8 )' ) 'jde-1,jte=',jde-1,jte
          !! CALL wrf_message(MESG)
       endif
       if( its .eq. ids ) then
          WRITE( MESG, '( A,2I8 )' ) 'ids,its=',ids,its
          !! CALL wrf_message(MESG)
       endif
       if( jts .eq. jds ) then
          WRITE( MESG, '( A,2I8 )' ) 'jds,jts=',jds,jts
          !! CALL wrf_message(MESG)
       endif
   endif
   
   if( firstime ) then 
      WRITE( MESG, '( A,F15.6 )' ) 'First call to PLD, calculating flight levels at current time: ',curr_time
      !! CALL wrf_message(MESG)
      fl_mt(1) = flvintvft * meter_per_ft
      fl_ft(1) = flvintvft
      ktrans   = 1
      kl = 1      
      WRITE( MESG, '( A,I8,2X,2(F15.6,2X),I8 )' ) 'kl,fl_mt,fl_ft, ktrans = ',kl,fl_mt(1),fl_ft(1), ktrans
      !! CALL wrf_message(MESG)      
      do kl = 2,fiplevs
         fl_mt(kl) = fl_mt(kl-1) + flvintvft * meter_per_ft
         fl_ft(kl) = fl_ft(kl-1) + flvintvft
         if( fl_ft(kl) .lt. TRANS_HT ) ktrans = kl
         WRITE( MESG, '( A,I8,2X,2(F15.6,2X),I8 )' ) 'kl,fl_mt,fl_ft, ktrans = ',kl,fl_mt(kl),fl_ft(kl), ktrans
         !! CALL wrf_message(MESG) 
      enddo
      DO j = jts , MIN(jte,jde-1)
         DO i = its , MIN(ite,ide-1)
            if( fl_mt(ktrans) .le. terht(i,j) ) then 
               WRITE( MESG, '( A, 2I8 )' ) 'Terrain higher than transition flight-level at i,j=',i,j
               !! CALL wrf_message ( TRIM(MESG) )
            endif
         END DO
      END DO
   endif

   DDT_PLD = 1.0/dt_pld
   iter_max = 0

   DO kp = 1 , num_press_levels
      p_pl(kp) = press_levels(kp)
      if( nint(p_pl(kp)) .eq. 100000 ) p1000=kp
      if( nint(p_pl(kp)) .eq. 85000 )  p850=kp      
      if( nint(p_pl(kp)) .eq. 70000 )  p700=kp      
      if( nint(p_pl(kp)) .eq. 50000 )  p500=kp
      if( nint(p_pl(kp)) .eq. 30000 )  p300=kp            
      if( nint(p_pl(kp)) .eq. 25000 )  p250=kp
      !! CAWO
      if( nint(p_pl(kp)) .eq. 95000 )  p950=kp      
      if( nint(p_pl(kp)) .eq. 90000 )  p900=kp      
      if( nint(p_pl(kp)) .eq. 80000 )  p800=kp       
      if( nint(p_pl(kp)) .eq. 75000 )  p750=kp                
      if( nint(p_pl(kp)) .eq. 65000 )  p650=kp 
      if( nint(p_pl(kp)) .eq. 60000 )  p600=kp 
      if( nint(p_pl(kp)) .eq. 55000 )  p550=kp 
      if( nint(p_pl(kp)) .eq. 45000 )  p450=kp 
      if( nint(p_pl(kp)) .eq. 40000 )  p_400=kp 
      if( nint(p_pl(kp)) .eq. 35000 )  p350=kp 
      if( nint(p_pl(kp)) .eq. 20000 )  p200=kp 
      if( nint(p_pl(kp)) .eq. 15000 )  p150=kp 
      if( nint(p_pl(kp)) .eq. 10000 )  p100=kp 
      if( nint(p_pl(kp)) .eq. 05000 )  p050=kp
      !! CAWO    
   END DO

   find_kk : DO kp = 1,num_press_levels
      kk = kp
      if( p_pl(kp) .le. 40000.0 ) EXIT find_kk 
   END DO find_kk

   !! FIP and SLD array initializations

   icg_pot3d( :,:,: ) = 0.0   !! initialize to zero probability this output time step
   sld_pot3d( :,:,: ) = 0.0   !!   "         "  "      "          "    "      "    "
   refl_htrgen( :,:,: ) = -35.0  !! DBZ initialize each output time-step; for generic 3D reflectivity

   ptyp_fipi( :,: )   = NONE  !! INTEGER p-type array for this time-step
   ptyp_cati( :,: )   = CAT_NONE  !! INTEGER p-type array for this time-step   
   ptyp_cat( :,: )    = 0.0   !! and associated categorical arrays for output
   cat_rain( :,: )    = 0.0
   cat_fzrn( :,: )    = 0.0
   cat_icep( :,: )    = 0.0
   cat_snow( :,: )    = 0.0
   catsum             = 0.0
   rfl_maxagl( :,: ) = 0.0     !! paranoia
   rfl_mxcagl( :,: ) = 0.0     !! paranoia   

   icg_potlv( :,:,: ) = -0.9  !! flight-level arrays (will be missing for LVLS below ground)
   sld_potlv( :,:,: ) = -0.9  !! flight-level arrays (will be missing for LVLS below ground)
   
   cnvresmask( :,: ) = 0.0
   fogflag( :,: ) = 0.0
   
   !! End FIP and SLD 3D array initializations   
   
   if( firstime ) then
      if( kfeta_shal_only .eq. 1 .and. grell_shal_only .eq. 1 ) then
         WRITE( MESG, '( A )' ) 'BOTH kfeta_shal_only and grell_shal_only ==1 in namelist'
         CALL wrf_message(MESG)
         WRITE( MESG, '( A )' ) 'Correct namelist and re-run'
         CALL wrf_error_fatal ( TRIM(MESG) )
      endif   

      if( kfeta_shal_only .eq. 1 .and. ntied_shal_only .eq. 1 ) then
         WRITE( MESG, '( A )' ) 'BOTH kfeta_shal_only and ntied_shal_only ==1 in namelist'
         CALL wrf_message(MESG)
         WRITE( MESG, '( A )' ) 'Correct namelist and re-run'
         CALL wrf_error_fatal ( TRIM(MESG) )
      endif      
      
      if( grell_shal_only .eq. 1 .and. ntied_shal_only .eq. 1 ) then
         WRITE( MESG, '( A )' ) 'BOTH grell_shal_only and ntied_shal_only ==1 in namelist'
         CALL wrf_message(MESG)
         WRITE( MESG, '( A )' ) 'Correct namelist and re-run'
         CALL wrf_error_fatal ( TRIM(MESG) )
      endif

      if( grell_shal_only .eq. 1 .and. cu_physics .ne. 3 ) then
         WRITE( MESG, '( A )' ) 'grell_shal_only ==1 CAN ONLY be run with cu_physics=3 (Grell-Frietas)'
         CALL wrf_message(MESG)
         WRITE( MESG, '( A )' ) 'Correct namelist and re-run'
         CALL wrf_error_fatal ( TRIM(MESG) )
      endif

      if( kfeta_shal_only .eq. 1 .and. cu_physics .ne. 1 ) then
         WRITE( MESG, '( A )' ) 'kfeta_shal_only ==1 CAN ONLY be run with cu_physics=1 (KF_ETA scheme)'
         CALL wrf_message(MESG)
         WRITE( MESG, '( A )' ) 'Correct namelist and re-run'
         CALL wrf_error_fatal ( TRIM(MESG) )
      endif

      if( cudt .gt. cudt_min .and. cu_physics .ne. 1 ) then
         WRITE( MESG, '( A )' ) 'This cu_physics scheme requires CUDT==0'
         CALL wrf_message(MESG)
         WRITE( MESG, '( A )' ) 'Correct namelist and re-run'
         CALL wrf_error_fatal ( TRIM(MESG) )
      endif

      if( kfeta_shal_only .eq. 1 ) then
        kfshonly = .true.
        WRITE( MESG, '( A )' ) 'kfshonly = TRUE'
        !! CALL wrf_message(MESG)
        if( kfeta_shal_dlim .gt. 4000.00 .or. kfeta_shal_dlim .lt. 1000.00 ) then
            WRITE( MESG, '( A )' ) 'kfeta_shal_dlim must be between 1000.0 and 4000.0 meters'
            CALL wrf_message(MESG)
            WRITE( MESG, '( A )' ) 'Correct namelist and re-run'
            CALL wrf_error_fatal ( TRIM(MESG) )     
        endif      
      endif

      if( grell_shal_only .eq. 1 ) then
        gfshonly = .true.
        WRITE( MESG, '( A )' ) 'gfshonly = TRUE'
        !! CALL wrf_message(MESG)
      endif

      if( ntied_shal_only .eq. 1 ) then
        ntshonly = .true.
        WRITE( MESG, '( A )' ) 'ntshonly = TRUE'
        !! CALL wrf_message(MESG)
      endif

      !! At first time, don't have all the IC data needed for potential vorticity
      potvort(:,:,:) = 0.0      
      zpvor15(:,:)   = 0.0
      zpvor20(:,:)   = 0.0
      tpvor15(:,:)   = 0.0
      tpvor20(:,:)   = 0.0

      !!print*,'kts,kts+1,kte-2=',kts,kts+1,kte-2
      !!print*,'jts, jte-1=',jts, jte-1
      !!print*,'its, ite-1=',its, ite-1
      !!CALL wrf_error_fatal ( 'DEBUG' )

   else
     
      !! CAWO: Pre-calculate Potential Vorticity on eta layer half-levels

      jpv_loop : DO j = max(jts,jds+1) , min(jte,jde-2)  
         ipv_loop : DO i = max(its,ids+1) , MIN(ite,ide-2)   
            dxtr=dx/msftx(i,j)
            dytr=dx/msfty(i,j)
            kpv_loop : DO k = kts+1, kte-2
               avor = relvort(i,k,j) + f(i,j)    !! Try a vertically averaged avor here ...                
               pu = pp(i,k+1,j)+pb(i,k+1,j)
               pd = pp(i,k-1,j)+pb(i,k-1,j)
               ddp = 1.0 / ( pu-pd )      
               uu = 0.5 * ( u(i,k+1,j)+u(i+1,k+1,j) )
               ud = 0.5 * ( u(i,k-1,j)+u(i+1,k-1,j) )
               dudp = ddp*(uu -ud)
               vu = 0.5 * ( v(i,k+1,j)+v(i,k+1,j+1) )
               vd = 0.5 * ( v(i,k-1,j)+v(i,k-1,j+1) )            
               dvdp = ddp*(vu -vd)
               
               thetu = t(i,k+1,j)+t0
               thetd = t(i,k-1,j)+t0
               dthdp  = ddp*(thetu-thetd)
                              
               dthdx = (t(i+1,k,j)-t(i-1,k,j))/(2.0*dxtr)
               dthdy = (t(i,k,j+1)-t(i,k,j-1))/(2.0*dytr)
               
               potvort(i,k,j) = -1.0E6*g * (dthdp*avor - dvdp*dthdx + dudp*dthdy)               
               !! if( i .eq. 126 .and. j .eq. 60 .and. k .eq. kte-2 ) then
               !!   print*,'g=',g
               !!   print*,'At i,j=',i,j                  
               !!   print*,'avor=',avor
               !!   print*,'pu,pd=',pu,pd
               !!   print*,'pu-pd=',pu-pd
               !!   print*,'ddp=',ddp
               !!   print*,'uu,ud=',uu,ud
               !!   print*,'uu-ud=',uu-ud
               !!   print*,'dudp=',dudp
               !!   print*,'vu,vd=',vu,vd
               !!   print*,'vu-vd=',vu-vd                  
               !!   print*,'dvdp=',dvdp                  
               !!   print*,'thetu=',thetu
               !!   print*,'thetd=',thetd                  
               !!   print*,'dthdp=',dthdp
               !!   print*,'t(i+1,k,j),t(i-1,k,j)=',t(i+1,k,j),t(i-1,k,j)
               !!   print*,'t(i,k,j+1)-t(i,k,j-1)=',t(i,k,j+1)-t(i,k,j-1) 
               !!   print*,'dthdx=',dthdx
               !!   print*,'dthdy=',dthdy
               !!   print*,'potvort=',potvort(i,k,j)              
               !! else if( i .eq. 108. .and. j .eq. 2 .and. k .eq. kte-2 ) then
               !!   print*,'g=',g
               !!   print*,'At i,j=',i,j                  
               !!   print*,'avor=',avor
               !!   print*,'pu,pd=',pu,pd
               !!   print*,'pu-pd=',pu-pd
               !!   print*,'ddp=',ddp
               !!   print*,'uu,ud=',uu,ud
               !!   print*,'uu-ud=',uu-ud
               !!   print*,'dudp=',dudp
               !!   print*,'vu,vd=',vu,vd
               !!   print*,'vu-vd=',vu-vd                  
               !!   print*,'dvdp=',dvdp                  
               !!   print*,'thetu=',thetu
               !!   print*,'thetd=',thetd                  
               !!   print*,'dthdp=',dthdp
               !!   print*,'t(i+1,k,j),t(i-1,k,j)=',t(i+1,k,j),t(i-1,k,j)
               !!   print*,'t(i,k,j+1)-t(i,k,j-1)=',t(i,k,j+1)-t(i,k,j-1) 
               !!   print*,'dthdx=',dthdx
               !!   print*,'dthdy=',dthdy
               !!   print*,'potvort=',potvort(i,k,j)               
               !! endif                        
            END DO kpv_loop
         END DO ipv_loop
      END DO jpv_loop

      !! Now update the boundary values:
      !! dot-pt vars go 1 to ide; cross-pt 1 to ide-1; potvort is a cross-pt var 
      
      IF( its .eq. ids ) THEN
         DO j = jts, jte
         DO k = kts+1, kte-2
            potvort(ids,k,j) = potvort(ids+1,k,j)
         END DO
         END DO      
      ENDIF

      IF( jts .eq. jds ) THEN
         DO k = kts+1, kte-2
         DO i = its, ite
            potvort(i,k,jds) = potvort(i,k,jds+1)
         END DO
         END DO      
      ENDIF      

      IF( ite .eq. ide ) THEN
         DO j = jts, jte
         DO k = kts+1, kte-2
            potvort(ide-1,k,j) = potvort(ide-2,k,j)
         END DO
         END DO      
      ENDIF

      IF( jte .eq. jde ) THEN
         DO k = kts+1, kte-2
         DO i = its, ite
            potvort(i,k,jde-1) = potvort(i,k,jde-2)
         END DO
         END DO      
      ENDIF

      !! CAWO: Fill lowest and highest levels with nearest neighbors
      DO j = jts , MIN(jte,jde-1)
      DO i = its , MIN(ite,ide-1)
         potvort(i,kts,j) = potvort(i,kts+1,j)
         potvort(i,kte-1,j) = potvort(i,kte-2,j)
         !! print*,'i,j,pv-kte-1=',i,j,potvort(i,kte-2,j)
      enddo
      enddo

   endif !! End FIRSTIME

   !  Loop over each i,j location

   j_loop : DO j = jts , MIN(jte,jde-1)

   !  Initialize pressure level data to "un-initialized / missing"

      DO kp = 1 , num_press_levels
         DO i = its , ite
            u_pl  (i,kp,j)    = DIAG_MISSING
            v_pl  (i,kp,j)    = DIAG_MISSING
            t_pl  (i,kp,j)    = DIAG_MISSING
            rh_pl (i,kp,j)    = DIAG_MISSING
            ght_pl(i,kp,j)    = DIAG_MISSING
            s_pl  (i,kp,j)    = DIAG_MISSING
            td_pl (i,kp,j)    = DIAG_MISSING
            wdir_pl (i,kp,j)  = DIAG_MISSING
            q_pl (i,kp,j)     = DIAG_MISSING
            thtae_pl (i,kp,j) = DIAG_MISSING
            wwnd_pl (i,kp,j)  = DIAG_MISSING
            avor_pl (i,kp,j)  = DIAG_MISSING
   !! CAWO
            thta_pl (i,kp,j)  = DIAG_MISSING
            pvor_pl (i,kp,j)  = DIAG_MISSING
            cconfr_pl (i,kp,j)= DIAG_MISSING 
            rnwtfr_pl (i,kp,j)= DIAG_MISSING    
            snowfr_pl (i,kp,j)= DIAG_MISSING
            graufr_pl (i,kp,j)= DIAG_MISSING
   !! CAWO 
         END DO
      END DO

      i_loop : DO i = its , MIN(ite,ide-1)

         if( i .eq. MIN(ite,ide-1) .and. j .eq. MIN(jte,jde-1) ) then
            WRITE( MESG, '( A,2I8,2F15.6 )' ) 'At ite_jte,uv+1=',MIN(ite,ide-1),MIN(jte,jde-1),u(i+1,1,j),v(i,1,j+1)
            !! CALL wrf_message(MESG)
         endif
         
         !  For each i,j location, loop over the selected
         !  pressure levels to find

         cfracl(i,j) = 0.0
         cfracm(i,j) = 0.0
         cfrach(i,j) = 0.0
         prate_inst(i,j) = 0.0

         vis(i,j)  = 90.0     !! km
         clg(i,j)  = 0.0      !! calculated in km; converted to meters
         freg(i,j) = 1.0
         optd(i,j) = 0.0

         lowcht(i,j) = -1.0
         midcht(i,j) = -1.0 
         higcht(i,j) = -1.0

         etp_mpsc(i,j) = 0.0
         etp_18dbz(i,j) = 0.0
         vilh(i,j) = 0.0
         radarvilh(i,j) = 0.0
         
         wbfrzlev(i,j) = 0.0
         
         qcol( i,j ) =  0.0 

         if( i .eq. 347 .and. j .eq. 273 ) then
           ldbg = .true.
         else
           ldbg = .false.  
         endif
         
         ke_h = kts
         ke_f = kts
         kp_loop : DO kp = 1 , num_press_levels

            !  For this particular i,j and pressure level, find the
            !  eta levels that surround this point on half-levels.

            ke_loop_half : DO ke = ke_h , kte-2

               IF      ( use_tot_or_hyd_p .EQ. 1 ) THEN     !  total pressure
                  pu = pp(i,ke+1,j)+pb(i,ke+1,j)
                  pd = pp(i,ke  ,j)+pb(i,ke  ,j)
               ELSE IF ( use_tot_or_hyd_p .EQ. 2 ) THEN     !  hydrostatic pressure
                  pu = p(i,ke+1,j)
                  pd = p(i,ke  ,j)
               END IF
               pm = p_pl(kp)
               
               IF ( ( pd .GE. pm ) .AND. &
                    ( pu .LT. pm ) ) THEN

                  !  Found trapping pressure: up, middle, down.
                  !  We are doing first order interpolation.  
                  !  Now we just put in a list of diagnostics for this level.

                  !  1. Temperature (K)

                  tu = (t(i,ke+1,j)+t0)*(pu/p1000mb)**rcp
                  td = (t(i,ke  ,j)+t0)*(pd/p1000mb)**rcp
                  t_pl(i,kp,j) = ( tu * (pm-pd) + td * (pu-pm) ) / (pu-pd)
                  
                  tu = (t(i,ke+1,j)+t0)
                  td = (t(i,ke  ,j)+t0)
                  thta_pl(i,kp,j) = ( tu * (pm-pd) + td * (pu-pm) ) / (pu-pd)  !! CAWO potential temperature
                                                                                 
                  !if( ldbg .and. kp .eq. 2 ) then  !! 850mb
                  !   print*,'i,j,kp=',i,j,kp
                  !   print*,'ke,ke+1=',ke,ke+1
                  !   print*,'pu,pd=',pu,pd
                  !   print*,'t0,t,tu,td=',t0,t(i,ke+1,j),tu,td
                  !   WRITE (MESG , * ) 'DEBUG PLD'
                  !  CALL wrf_error_fatal ( TRIM(MESG) )
                  !endif                     

                  !  2. Speed (m s-1)

                  su = 0.5 * SQRT ( ( u(i,ke+1,j)+u(i+1,ke+1,j) )**2 + ( v(i,ke+1,j)+v(i,ke+1,j+1) )**2 ) 
                  sd = 0.5 * SQRT ( ( u(i,ke  ,j)+u(i+1,ke  ,j) )**2 + ( v(i,ke  ,j)+v(i,ke  ,j+1) )**2 ) 
                  s_pl(i,kp,j) = ( su * (pm-pd) + sd * (pu-pm) ) / (pu-pd)

                  !  3. U and V (m s-1)

                  uu = 0.5 *        ( u(i,ke+1,j)+u(i+1,ke+1,j) )
                  ud = 0.5 *        ( u(i,ke  ,j)+u(i+1,ke  ,j) )
                  u_pl(i,kp,j) = ( uu * (pm-pd) + ud * (pu-pm) ) / (pu-pd)

                  vu = 0.5 *                                           ( v(i,ke+1,j)+v(i,ke+1,j+1) )
                  vd = 0.5 *                                           ( v(i,ke  ,j)+v(i,ke  ,j+1) )
                  v_pl(i,kp,j) = ( vu * (pm-pd) + vd * (pu-pm) ) / (pu-pd)

                  uearth = u_pl(i,kp,j)*cosalpha(i,j)+v_pl(i,kp,j)*sinalpha(i,j)
                  vearth = v_pl(i,kp,j)*cosalpha(i,j)+u_pl(i,kp,j)*sinalpha(i,j)

                  wdir = atan2(-uearth,-vearth)*180./3.14159
                  if( wdir .lt. 0.0 ) wdir = wdir + 360.
                  wdir_pl(i,kp,j) = wdir

                  wu = w0avg(i,ke+1,j)
                  wd = w0avg(i,ke,j)

                  wwnd_pl(i,kp,j) = ( wu * (pm-pd) + wd * (pu-pm) ) / (pu-pd)

                  !  4a. Vorticity
                  
                  avu = relvort(i,ke+1,j)
                  avd = relvort(i,ke,j)
                  avor_pl (i,kp,j) =  ( avu * (pm-pd) + avd * (pu-pm) ) / (pu-pd)
                  avor_pl (i,kp,j) = avor_pl (i,kp,j) + f(i,j)

                  !  4b. Potential Vorticity -- CAWO
                  
                  avu = potvort(i,ke+1,j)
                  avd = potvort(i,ke,j)
                  pvor_pl (i,kp,j) =  ( avu * (pm-pd) + avd * (pu-pm) ) / (pu-pd)
                                   
                  !  5. Mixing ratio (kg/kg)

                  qu = MAX(qv(i,ke+1,j),0.)
                  qd = MAX(qv(i,ke  ,j),0.)
                  q_pl(i,kp,j) = ( qu * (pm-pd) + qd * (pu-pm) ) / (pu-pd)

                  !  6. Dewpoint (K) - Use Bolton's approximation

                  qu = MAX(qv(i,ke+1,j),1.0E-25)                     
                  qd = MAX(qv(i,ke  ,j),1.0E-25)                     
                  eu = qu * pu * 0.01 / ( eps + qu ) ! water vapor press (mb)
                  ed = qd * pd * 0.01 / ( eps + qd ) ! water vapor press (mb)
                  eu = max(eu, 0.001)
                  ed = max(ed, 0.001)

                  du = t_kelvin + (s1*log(eu) - bolton_con1)/(bolton_con2-log(eu))
                  dd = t_kelvin + (s1*log(ed) - bolton_con1)/(bolton_con2-log(ed))
                  td_pl(i,kp,j) = ( du * (pm-pd) + dd * (pu-pm) ) / (pu-pd)
                  
                  !  7. Relative humidity (%)

                  qm = ( qu * (pm-pd) + qd * (pu-pm) ) / (pu-pd)                           ! qvapor at the pressure level.
                  es = s4 * exp(s5 * (1.0 / 273.0 - 1.0 / t_pl(i,kp,j)))
                  qs = eps * es / (pm - es)
                  rh_pl(i,kp,j)   = qm / qs * 100.                     

                  !es = s4 * exp(s5 * (1.0 / 273.0 - 1.0 / t_pl(i,kp,j)))
                  !qs = eps * es / (pm - es)
                  !rh_pl(i,kp,j)   = q_pl(i,kp,j) / qs * 100.

                  !em = qm * pm * 0.01 / ( eps + qm )                                       ! water vapor pressure at the level.
                  !es = s3 * exp( s2 * (t_pl(i,kp,j) - t_kelvin)/(t_pl(i,kp,j) - s4) )      ! sat vapor pressure over liquid water in mb.
                  !rh_pl(i,kp,j) = 100. * em * ( pm * 0.01 - es ) / ( es * ( pm * 0.01 - em ) )

                  !  8. Theta-e

                  pmmb = pm*.01
                  ev   = (qm*pmmb)/(eps+qm)
                  tlcl = TLCLC1/(log(t_pl(i,kp,j)**TLCLC2/ev)-TLCLC3)+TLCLC4
                  thtae_pl(i,kp,j) = t_pl(i,kp,j)*(1000./(pmmb))**( GAMMA_RIP*(1.+GAMMAMD*qm) )*     &
                                  exp( (THTECON1/tlcl-THTECON2)*qm*(1.+THTECON3*qm) ) 

                  !  9. Condensate and fractions: CAWO

                  tcondwu = MAX(qs_curr(i,ke+1,j),0.)+MAX(qg_curr(i,ke+1,j),0.)+MAX(qr_curr(i,ke+1,j),0.) + &
                            MAX(qc_curr(i,ke+1,j),0.)+MAX(qi_curr(i,ke+1,j),0.)
                            
                  tcondwd = MAX(qs_curr(i,ke,j),0.)+MAX(qg_curr(i,ke,j),0.)+MAX(qr_curr(i,ke,j),0.) + &
                            MAX(qc_curr(i,ke,j),0.)+MAX(qi_curr(i,ke,j),0.)
                            
                  TCONDW  = ( tcondwu * (pm-pd) + tcondwd * (pu-pm) ) / (pu-pd)
                  
                  clconu  = MAX(qc_curr(i,ke+1,j),0.)+MAX(qi_curr(i,ke+1,j),0.)                   
                  clcond  = MAX(qc_curr(i,ke,j),0.)+MAX(qi_curr(i,ke,j),0.)     
                  clconp  = ( clconu * (pm-pd) + clcond * (pu-pm) ) / (pu-pd)
                   
                  rnwatu  = MAX(qr_curr(i,ke+1,j),0.)                  
                  rnwatd  = MAX(qr_curr(i,ke,j),0.)
                  rnwatp  = ( rnwatu * (pm-pd) + rnwatd * (pu-pm) ) / (pu-pd)
                                      
                  snowu   = MAX(qs_curr(i,ke+1,j),0.)                   
                  snowd   = MAX(qs_curr(i,ke,j),0.)
                  snowp   = ( snowu * (pm-pd) + snowd * (pu-pm) ) / (pu-pd) 
                  
                  grplu  =  MAX(qg_curr(i,ke+1,j),0.)                  
                  grpld  =  MAX(qg_curr(i,ke,j),0.)                              
                  grplp  = ( grplu * (pm-pd) + grpld * (pu-pm) ) / (pu-pd)
                  
                  if( TCONDW .gt. 1.0E-6 ) then
                     cconfr_pl(i,kp,j) = clconp/TCONDW
                     rnwtfr_pl (i,kp,j) = rnwatp/TCONDW
                     snowfr_pl (i,kp,j) = snowp/TCONDW
                     graufr_pl  (i,kp,j) = grplp/TCONDW
                  else
                     cconfr_pl(i,kp,j) = 0.
                     rnwtfr_pl (i,kp,j) = 0.
                     snowfr_pl (i,kp,j) = 0.
                     graufr_pl  (i,kp,j) = 0.                                    
                  endif                                              !! CAWO end
                                              
                  ke_h = ke
                  EXIT ke_loop_half
               END IF
            END DO ke_loop_half

            ke_loop_full : DO ke = ke_f , kte-1

               pwke = p8w(i,ke  ,j)      

               IF ( ( pwke .GE. p_pl(kp) ) .AND. &
                    ( p8w(i,ke+1,j) .LT. p_pl(kp) ) ) THEN

                  !  Found trapping pressure: up, middle, down.  We are doing first order interpolation.

                  ! pu = LOG(p8w(i,ke+1,j))
                  ! pm = LOG(p_pl(kp))
                  ! pd = LOG(p8w(i,ke  ,j))

                  pu = p8w(i,ke+1,j)
                  pm = p_pl(kp)
                  pd = p8w(i,ke  ,j)                

                  !  Now we just put in a list of diagnostics for this level.

                  !  1. Geopotential height (m)

                  zu = ( zp(i,ke+1,j)+zb(i,ke+1,j) ) / g
                  zd = ( zp(i,ke  ,j)+zb(i,ke  ,j) ) / g
                  ght_pl(i,kp,j) = ( zu * (pm-pd) + zd * (pu-pm) ) / (pu-pd)                   

                  ke_f = ke
                  EXIT ke_loop_full
               END IF
            END DO ke_loop_full

         END DO kp_loop

         !! Initial computation: gust speed estimate part 1
         !! Estimate layer-based wind-speeds approx. 50mb and 150mb above surface pressure
         !! for convective contribution
         
         IF( use_tot_or_hyd_p .EQ. 1 ) THEN           !  total pressure
             p1 = pp(i,1  ,j)+pb(i,1  ,j)
         ELSE IF ( use_tot_or_hyd_p .EQ. 2 ) THEN     !  hydrostatic pressure
             p1 = p(i,1  ,j)
         END IF         
         
         p_at_050up = p1 - 5000.
         kl_050 = -1
         k = 1         
         found = .FALSE.         
         
         DO WHILE( (.not. found) .and. (k.le.kte) )                      
           IF( use_tot_or_hyd_p .EQ. 1 ) THEN           !  total pressure
              pd = pp(i,k  ,j)+pb(i,k  ,j)
           ELSE IF ( use_tot_or_hyd_p .EQ. 2 ) THEN     !  hydrostatic pressure
              pd = p(i,k  ,j)
           END IF           
           IF ( pd .LT. p_at_050up ) THEN
             kl_050 = k
             found = .TRUE.
           END IF
           k = k+1         
         END DO

         IF ( kl_050 .EQ. -1 ) THEN
            WRITE (MESG ,'(A,I4,A)' ) 'Troubles finding level 50mb above ground.'
            CALL wrf_error_fatal ( TRIM(MESG) )
         END IF

         p_at_150up = p1 - 15000.         
         kl_150 = -1
         k = 1         
         found = .FALSE.
         
         DO WHILE( (.not. found) .and. (k.le.kte) )                      
           IF( use_tot_or_hyd_p .EQ. 1 ) THEN           !  total pressure
              pd = pp(i,k  ,j)+pb(i,k  ,j)
           ELSE IF ( use_tot_or_hyd_p .EQ. 2 ) THEN     !  hydrostatic pressure
              pd = p(i,k  ,j)
           END IF           
           IF ( pd .LT. p_at_150up ) THEN
             kl_150 = k
             found = .TRUE.
           END IF
           k = k+1         
         END DO         
         
         IF ( kl_150 .EQ. -1 ) THEN
            WRITE (MESG ,'(A,I4,A)' ) 'Troubles finding level 150mb above ground.'
            CALL wrf_error_fatal ( TRIM(MESG) )
         END IF
         
         wspd_850g = 0.5 * SQRT ( ( u(i,kl_150,j)+u(i+1,kl_150,j) )**2 + ( v(i,kl_150,j)+v(i,kl_150,j+1) )**2 )         
         wspd_950g = 0.5 * SQRT ( ( u(i,kl_050,j)+u(i+1,kl_050,j) )**2 + ( v(i,kl_050,j)+v(i,kl_050,j+1) )**2 )
         convshr_gust = MAX( (wspd_850g - wspd_950g), 0.0 )
         
         !! if( ldbg ) then
         !!    print*,'LDBG: convshr_gust =',convshr_gust
         !!    print*,'kl_150,kl_50=',kl_150,kl_050
         !! endif

         !! Other computations ....
         !! (1) Sea-level pressure

         level(i,j) = -1
         k = 1         
         found = .FALSE.
         IF( use_tot_or_hyd_p .EQ. 1 ) THEN           !  total pressure
             p1 = pp(i,1  ,j)+pb(i,1  ,j)
         ELSE IF ( use_tot_or_hyd_p .EQ. 2 ) THEN     !  hydrostatic pressure
             p1 = p(i,1  ,j)
         END IF

         p_at_pconst = p1 - pconst

         t1(i,j) = (t(i,1,j)+t0)*(p1/p1000mb)**rcp

         DO WHILE( (.not. found) .and. (k.le.kte) )                      
           IF( use_tot_or_hyd_p .EQ. 1 ) THEN           !  total pressure
              pd = pp(i,k  ,j)+pb(i,k  ,j)
           ELSE IF ( use_tot_or_hyd_p .EQ. 2 ) THEN     !  hydrostatic pressure
              pd = p(i,k  ,j)
           END IF           

           IF ( pd .LT. p_at_pconst ) THEN
             level(i,j) = k
             found = .TRUE.
           END IF
           k = k+1         
         END DO

         IF ( level(i,j) .EQ. -1 ) THEN
            WRITE (MESG ,'(A,I4,A)' ) 'Troubles finding level ',NINT(PCONST)/100,' above ground.'
            CALL wrf_error_fatal ( TRIM(MESG) )
         END IF

         !  Get temperature PCONST Pa above surface.  Use this to extrapolate
         !  the temperature at the surface and down to sea level.

         klo = MAX ( level(i,j) - 1 , 1      )
         khi = MIN ( klo + 1        , kte - 1 )
         IF ( klo .EQ. khi ) THEN
            WRITE (MESG ,'(A)' ) 'Error_trapping_levels'
            CALL wrf_error_fatal ( TRIM(MESG) )
         END IF

         IF( use_tot_or_hyd_p .EQ. 1 ) THEN
            plo = pp(i,klo  ,j)+pb(i,klo  ,j)
            phi = pp(i,khi  ,j)+pb(i,khi  ,j) 
         ELSE IF ( use_tot_or_hyd_p .EQ. 2 ) THEN
            plo = p(i,klo  ,j)
            phi = p(i,khi  ,j)
         END IF 

         tlo = ((t(i,klo,j)+t0)*(plo/p1000mb)**rcp)*(1. + 0.608 * qv(i,klo,j) )
         thi = ((t(i,khi,j)+t0)*(phi/p1000mb)**rcp)*(1. + 0.608 * qv(i,khi,j) ) 
         zlo = ( zp(i,klo,j)+zb(i,klo,j) ) / g
         zhi = ( zp(i,khi,j)+zb(i,khi,j) ) / g

         t_at_pconst = thi-(thi-tlo)*LOG(p_at_pconst/phi)*LOG(plo/phi)
         z_at_pconst = zhi-(zhi-zlo)*LOG(p_at_pconst/phi)*LOG(plo/phi)

         t_surf = t_at_pconst*(p1/p_at_pconst)**(lapse_dry*r_d/g)
         t_sea_level = t_at_pconst+lapse_dry*z_at_pconst

         !! following traditional computation, there is a correction to the sea level
         !! temperature if both the surface and sea level temperatures are *too* hot. 

         l1 = t_sea_level .LT. TC
         l2 = t_surf      .LE. TC
         l3 = .NOT. l1

         IF ( l2 .AND. l3 ) THEN
            t_sea_level = TC
         ELSE
            t_sea_level = TC - 0.005*(t_surf-TC)**2
         END IF              

         z_half_lowest = 0.5 * ( terht(i,j) + ( zp(i,2,j)+zb(i,2,j) ) / g ) !! "2" at top of lowest model layer

         slprs(i,j) = p1 * exp((2.*G*z_half_lowest)/(r_d*(t_sea_level+t_surf)))         

         !! Done with Sea-level pressure
         !! (2) Extrapolate pressure-level data where needed
         !! Note: terht == zb(i,1,j) / g and zp(i,1,j)=0.0

         DO kp = 1, kk

            if( ght_pl(i,kp,j) .lt. -999990. .and. p_pl(kp) .lt. p8w(i,kts,j) ) then
               WRITE( MESG, '( A,2I10 )' ) 'Height was not found between ground and 1st level at i,j=',i,j
               CALL wrf_error_fatal ( TRIM(MESG) )
            else if( ght_pl(i,kp,j) .lt. -999990. ) then

               ! Absolutely below ground. Algorithm: 
               ! First, find the model level that is closest to a "target" pressure             
               ! level, where the "target" pressure is delta-p less that the local              
               ! value of a horizontally smoothed surface pressure field.  We use               
               ! delta-p = 150 hPa here. A standard lapse rate temperature profile              
               ! passing through the temperature at this model level will be used               
               ! to define the temperature profile below ground.  This is similar               
               ! to the Benjamin and Miller (1990) method, except that for                      
               ! simplicity, they used 700 hPa everywhere for the "target" pressure.            
               ! Code similar to what is implemented in RIP4.                                    

                ptarget = (p8w(i,kts,j)*.01) - 150.  !! p8w in Pa

                dpmin=1.e4
                loop_kin : DO kin=kte-1,1,-1
                   kup = kin                   
                   IF      ( use_tot_or_hyd_p .EQ. 1 ) THEN     !  total pressure
                      prku = pp(i,kup,j)+pb(i,kup,j)
                   ELSE IF ( use_tot_or_hyd_p .EQ. 2 ) THEN     !  hydrostatic pressure
                      prku = p(i,kup,j)
                   END IF                   
                   dp=abs( ( prku *.01 ) - ptarget )
                   IF (dp.gt.dpmin) exit loop_kIN
                   dpmin=min(dpmin,dp)
                ENDDO loop_kin                                

                pbot=max(p1,p8w(i,kts,j))                                
                zbot=min( zb(i,1,j)/g, terht(i,j))              ! need height in meter

                tkel = (t(i,kup,j)+t0)*(prku/p1000mb)**rcp                
                tbotextrap=tkel*(pbot/prku)**expon
                tvbotextrap=tbotextrap*(1. + 0.608 * qv(i,1,j) )

                ! Calculations use height in meters ... 

                zlev = zbot + ( tvbotextrap/lapse_dry ) * (1.- (p_pl(kp)/pbot)**expon)

                if( kp .eq. p1000 ) then
                   pdex=p1000
                   pmmb=1000.
                else if( kp .eq. p850 ) then
                   pdex=p850
                   pmmb=850.                
                else if( kp .eq. p700 ) then                   
                   pdex=p700
                   pmmb=700.
                else if( kp .eq. p500 ) then
                   pdex=p500
                   pmmb=500.
                else
                   pdex = kp
                   pmmb = float( nint(p_pl(kp))/100 )                                      
                   !!MESG = 'Bad pressure index on extrapolation'
                   !!CALL wrf_error_fatal ( TRIM(MESG) ) 
                endif
                ght_pl(i,pdex,j) = zlev
                t_pl(i,pdex,j) = t1(i,j) + (z_half_lowest - zlev) * lapse_dry
                thta_pl(i,pdex,j) = t_pl(i,pdex,j)*(1000./pmmb)**rcp        ! CAWO

                ! Extrapolate for thetae

                qm = MAX(qv(i,1,j),1.0E-25)  !! Assume constant mixing ratio below ground
                ev = (qm*pmmb)/(eps+qm)
                tlcl = TLCLC1/(log(t_pl(i,pdex,j)**TLCLC2/ev)-TLCLC3)+TLCLC4
                thtae_pl(i,kp,j) = t_pl(i,pdex,j)*(1000./(pmmb))**( GAMMA_RIP*(1.+GAMMAMD*qm) )*     &
                                     exp( (THTECON1/tlcl-THTECON2)*qm*(1.+THTECON3*qm) )
                                     
                ! Dewpoint temperature: 
                td_pl(i,kp,j) = t_kelvin + ( s1 / ((s2 / log(ev/s3)) - 1.0) )       !! CAWO                                                

            else if( ght_pl(i,kp,j) .ge. 0.0 .and. t_pl(i,kp,j) .lt. 0.0 ) then

                if( kp .eq. p1000 ) then
                   pdex=p1000
                   pmmb=1000.
                else if( kp .eq. p850 ) then
                   pdex=p850
                   pmmb=850.                
                else if( kp .eq. p700 ) then                   
                   pdex=p700
                   pmmb=700.
                else if( kp .eq. p500 ) then
                   pdex=p500
                   pmmb=500.
                else
                   pdex = kp
                   pmmb = float( nint(p_pl(kp))/100 )
                   !!MESG = 'GHT_PL interped but T_PL not and kp=>Bad pressure index on extrapolation'
                   !!CALL wrf_error_fatal ( TRIM(MESG) ) 
                endif
                zlev = ght_pl(i,pdex,j)
                t_pl(i,pdex,j) = t1(i,j) + (z_half_lowest - zlev) * lapse_dry
                thta_pl(i,pdex,j) = t_pl(i,pdex,j)*(1000./pmmb)**rcp        ! CAWO

                !!if( ldbg ) then
                  !!print*,'i,j,t_pl,t1,z_half_lowest,zlev,lapse_dry=',i,j,t_pl(i,pdex,j),t1(i,j),z_half_lowest,zlev,lapse_dry
                  !!print*,'kp,pdex,pmmb=',kp,pdex,pmmb
                  !!print*,'ght_pl(i,kp,j)=',ght_pl(i,kp,j)
                  !! CALL wrf_error_fatal ( 'DEBUG' ) 
                !!endif

                ! Extrapolate for thetae

                qm = MAX(qv(i,1,j),1.0E-25)  !! Assume constant mixing ratio below ground
                ev = (qm*pmmb)/(eps+qm)
                tlcl = TLCLC1/(log(t_pl(i,pdex,j)**TLCLC2/ev)-TLCLC3)+TLCLC4
                thtae_pl(i,kp,j) = t_pl(i,pdex,j)*(1000./(pmmb))**( GAMMA_RIP*(1.+GAMMAMD*qm) )*     &
                                     exp( (THTECON1/tlcl-THTECON2)*qm*(1.+THTECON3*qm) )

                ! Dewpoint temperature: 
                td_pl(i,kp,j) = t_kelvin + ( s1 / ((s2 / log(ev/s3)) - 1.0) )       !! CAWO  

            else if( ght_pl(i,kp,j) .lt. 0.0 .and. t_pl(i,kp,j) .lt. 0.0 ) then
                                            
                if( kp .eq. p1000 ) then
                   pdex=p1000
                   pmmb=1000.
                else if( kp .eq. p850 ) then
                   pdex=p850
                   pmmb=850.                
                else if( kp .eq. p700 ) then                   
                   pdex=p700
                   pmmb=700.
                else if( kp .eq. p500 ) then
                   pdex=p500
                   pmmb=500.
                else
                   pdex = kp
                   pmmb = float( nint(p_pl(kp))/100 )
                   !! MESG = 'GHT_PL negative (ok) but T_PL not and kp=>Bad pressure index on extrapolation'
                   !! CALL wrf_error_fatal ( TRIM(MESG) ) 
                endif
                zlev = ght_pl(i,pdex,j)
                t_pl(i,pdex,j) = t1(i,j) + (z_half_lowest - zlev) * lapse_dry
                thta_pl(i,pdex,j) = t_pl(i,pdex,j)*(1000./pmmb)**rcp        ! CAWO

                qm = MAX(qv(i,1,j),1.0E-25)  !! Assume constant mixing ratio 
                ev = (qm*pmmb)/(eps+qm)
                tlcl = TLCLC1/(log(t_pl(i,pdex,j)**TLCLC2/ev)-TLCLC3)+TLCLC4
                thtae_pl(i,kp,j) = t_pl(i,pdex,j)*(1000./(pmmb))**( GAMMA_RIP*(1.+GAMMAMD*qm) )*     &
                                     exp( (THTECON1/tlcl-THTECON2)*qm*(1.+THTECON3*qm) )

                !!if( ldbg ) then
                  !!print*,'i,j,t_pl,t1,z_half_lowest,zlev,lapse_dry=',i,j,t_pl(i,pdex,j),t1(i,j),z_half_lowest,zlev,lapse_dry
                  !!print*,'kp,pdex,pmmb=',kp,pdex,pmmb
                  !!print*,'ght_pl(i,kp,j)=',ght_pl(i,kp,j)
                  !! CALL wrf_error_fatal ( 'DEBUG' ) 
                !!endif

                ! Dewpoint temperature: 
                td_pl(i,kp,j) = t_kelvin + ( s1 / ((s2 / log(ev/s3)) - 1.0) )       !! CAWO  

	    endif

         END DO

         !! Done with extrapolation
         !! (3) Layer 1 stuff: dewpoint, rh, speed, CAWO stuff 

         q1 = MAX(qv(i,1,j),1.0E-25)

         IF      ( use_tot_or_hyd_p .EQ. 1 ) THEN     !  total pressure
            p1 = pp(i,1,j)+pb(i,1,j)
         ELSE IF ( use_tot_or_hyd_p .EQ. 2 ) THEN     !  hydrostatic pressure
            p1 = p(i,1,j)
         END IF

         e1 = q1 * p1 * 0.01 / ( eps + q1 )       ! water vapor pressure in mb.
         e1 = max(e1, 0.001)
         td1(i,j) = t_kelvin + ( s1 / ((s2 / log(e1/s3)) - 1.0) )

         es = s4 * exp(s5 * (1.0 / 273.0 - 1.0 / t1(i,j)))
         qs = eps * es / (p1 - es)
         rh1(i,j) = q1 / qs * 100.

         sp1(i,j) = 0.5 * SQRT ( ( u(i,1,j)+u(i+1,1,j) )**2 + ( v(i,1,j)+v(i,1,j+1) )**2 )
         spdshr(i,j) = SQRT ( ( u(i,4,j)-u(i,1,j) )**2 + ( v(i,4,j)-v(i,1,j) )**2 ) !! low-level wind-shear m/sec

         TCONDW = MAX(qs_curr(i,1,j),0.)+MAX(qg_curr(i,1,j),0.)+MAX(qr_curr(i,1,j),0.) + &
                  MAX(qc_curr(i,1,j),0.)+MAX(qi_curr(i,1,j),0.)                               !! CAWO
         
         if( TCONDW .gt. 1.0E-6 ) then
            ccondfrc1(i,j)=(MAX(qc_curr(i,1,j),0.)+MAX(qi_curr(i,1,j),0.))/TCONDW 
            rainfrc1(i,j) = MAX(qr_curr(i,1,j),0.)/TCONDW 
            snowfrc1(i,j) = MAX(qs_curr(i,1,j),0.)/TCONDW 
            icefrc1(i,j)  = MAX(qg_curr(i,1,j),0.)/TCONDW
         else
            ccondfrc1(i,j)= 0.
            rainfrc1(i,j)= 0. 
            snowfrc1(i,j)= 0.
            icefrc1(i,j)= 0.      
         endif                                                                                !! CAWO end 
           
         !! Done with layer 1 stuff
         !! (4) 2-meter dewpoint: use p8w(i,kts,j) for 2-meter pressure

         p2m = p8w(i,kts,j) * 0.01             ! 2-meter pressure in mb
         q2m = MAX(q2(i,j),1.0E-25)
         e2m = q2m * p2m / ( eps + q2m )       ! water vapor pressure in mb.
         e2m = max(e2m, 0.001)
         td2(i,j) = t_kelvin + ( s1 / ((s2 / log(e2m/s3)) - 1.0) )
         td2(i,j) = MIN( td2(i,j), t2(i,j) )
         pressfc(i,j) = p8w(i,kts,j)           ! CAWO
         !! if( ldbg ) print*,'i,j,pressfc=',i,j,pressfc(i,j)

         !! Done with 2-meter dewpoint
         !! 2-meter rh;

         es2 = s4 * exp(s5 * (1.0 / 273.0 - 1.0 / t2(i,j)))  !! in Pa; as p8w in Pa.
         qs2 = eps * es2 / (p8w(i,kts,j) - es2)
         rh2(i,j) = MAX( MIN( q2m / qs2 * 100., 100.), 0.0)

         !! Done with 2-meter RH
         !! (4.1) 2-meter wet-bulb temperature
         !! Utilize formulation consistent with calculation of twh/tw1 below,
         !! which is in mb; whereas the above (es2, qs2, rh2) are in Pascals
         
         ee2 = q2m * p2m/(eps + 0.378*q2m)
         d2m = amax1( s5/(e2p - alog( ee2*d611 ) ), 200. )
         es2 = exp( c15 - c_1*t2(i,j) - c_2/t2(i,j) )
         ed2 = exp( c15 - c_1*td2(i,j)  - c_2/td2(i,j) ) !! mb
         
         if( abs( (es2 - ed2) ) .lt. 1.e-6 ) then
             ss = 0.0
         else
             ss  = MAX( ((es2 - ed2) / (t2(i,j) - td2(i,j))), 0.0 )
         endif         
                  
         tw2m  = (t2(i,j)*fw*p2m + td2(i,j)*ss) / ( fw*p2m + ss)
         
         do  n = 1, 4
             ew  = exp (c15 - c_1*tw2m - c_2/tw2m )
             de  = fw*p2m*(t2(i,j) - tw2m ) - (ew - ed2)
             der = ew*(c_1-c_2/tw2m**2) - fw*p2m
             tw2m  = tw2m    - de/der
         end do
         
         tw2(i,j) = max( min( tw2m,t2(i,j) ), td2(i,j) )
         
         !! if( ldbg ) then
         !!   print*,'final t2,tw2,td2=',t2(i,j),tw2(i,j),td2(i,j)
         !! endif
         
         !! Done with 2-meter wet-bulb         
         !! Compute speed at 10-meters and 2-meter apparent temperature

         wspd10(i,j) = sqrt(u10(i,j)*u10(i,j)+v10(i,j)*v10(i,j))
         uearth = u10(i,j)*cosalpha(i,j)+v10(i,j)*sinalpha(i,j)    !! CAWO
         vearth = v10(i,j)*cosalpha(i,j)+u10(i,j)*sinalpha(i,j)
         wdir = atan2(-uearth,-vearth)*180./3.14159
         if( wdir .lt. 0.0 ) wdir = wdir + 360.
         wdir10(i,j) = wdir                                       
         u10earth(i,j) = uearth
         v10earth(i,j) = vearth                                    !! End CAWO

         tcel2 = t2(i,j) - CTOK         
         tf2(i,j) = 1.8 * tcel2 + 32.0
         layspd = 3.6 * wspd10(i,j)     !! km/hour
         aptemp(i,j) = tf2(i,j)

         if ( tcel2.le.10.0 .and. layspd .gt. 4.8 ) then                  !! Wind chill calc
            resid = 13.12 + 0.6215*tcel2 - 11.37*(layspd**0.16) +     &
                    0.3965*tcel2*(layspd**0.16)
            aptemp(i,j) = resid*1.8 + 32.0          
         else if ( tf2(i,j) .gt. 80.0 ) then
            aptemp(i,j) = c1a + c2a*tf2(i,j) + c3a*rh2(i,j) +      & 
                          c4a*tf2(i,j)*rh2(i,j) + c5a*tf2(i,j)*tf2(i,j) +      & 
                          c6a*rh2(i,j)*rh2(i,j) + c7a*tf2(i,j)*tf2(i,j)*rh2(i,j) +     & 
                          c8a*tf2(i,j)*rh2(i,j)*rh2(i,j) +     & 
                          c9a*tf2(i,j)*tf2(i,j)*rh2(i,j)*rh2(i,j)            
            if( rh2(i,j) .lt. 13.0 .and. tf2(i,j) .lt. 112.0 ) then
               hadj = ( (13.0-rh2(i,j))*0.25 ) * SQRT ( (17.0-abs(tf2(i,j)-95.0)) * INV17 )
               aptemp(i,j) = aptemp(i,j) - hadj               
            else if (rh2(i,j) .gt. 85.0 .and. tf2(i,j) .lt. 87.0 ) then
               hadj = ( (rh2(i,j)-85.0)*0.1 ) * ( (87.0-tf2(i,j))*0.2 )
               aptemp(i,j) = aptemp(i,j) + hadj
            endif
            aptemp(i,j) = min( aptemp(i,j), 150. )
         else if ( tf2(i,j) .gt. 70.0 ) then
            aptemp(i,j) = 0.5 * ( tf2(i,j) + 61.0 + ( (tf2(i,j)-68.0)*1.2 ) + rh2(i,j)*0.094 )
            aptemp(i,j) = 0.5 * ( aptemp(i,j) + tf2(i,j) )
            aptemp(i,j) = min( aptemp(i,j), 150. )
         endif         
         
         !! else if ( tf2(i,j) .ge. 80.0 .and. rh2(i,j) .ge. 40.0 ) then    !! heat index calculation   
         !!    aptemp(i,j) = c1a + c2a*tf2(i,j) + c3a*rh2(i,j) +      & 
         !!                  c4a*tf2(i,j)*rh2(i,j) + c5a*tf2(i,j)*tf2(i,j) +      & 
         !!                  c6a*rh2(i,j)*rh2(i,j) + c7a*tf2(i,j)*tf2(i,j)*rh2(i,j) +     & 
         !!                  c8a*tf2(i,j)*rh2(i,j)*rh2(i,j) +     & 
         !!                  c9a*tf2(i,j)*tf2(i,j)*rh2(i,j)*rh2(i,j)         
         !!    aptemp(i,j) = min( aptemp(i,j), 150. )
         !! endif

         !! Done with apparent temperature

         !! New: Provide apparent temperature in C, 2-meter mixing ratio, and specific humidity at 2-m
         
         apta2c(i,j) = (aptemp(i,j) - 32.0) / 1.8
         qv2(i,j) = q2(i,j)
         spechum(i,j) = qv2(i,j)/(1.0 + qv2(i,j))

         !! New: Also provide new soil-related variables

         soiltlay1(i,j) = tslb(i,1,j)
         smrellay1(i,j) = max( smcrel(i,1,j), 0.0 )

         totthik = 0.0
         smreltot( i,j ) = 0.0
         do kl = 1, lsm_lays
           totthik = totthik + soilthk(kl)
           smreltot(i,j) = smreltot(i,j) + max(smcrel(i,kl,j), 0.0)  * soilthk(kl)         
         enddo
         smreltot(i,j) = smreltot(i,j) / totthik        
   
         !! Compute wet-bulb globe temperature: ln(2.0)/ln(10.0) = 0.30103 (loglaw2)
         !! e2m is 2-meter vapor pressure in mb
         
         wspd2 = loglaw2 * wspd10(i,j)
         wspd2 = max( wspd2, 1.0 )      !! setting a floor consistent w/ NWS Tulsa office practice
         twcel2 = tw2(i,j) - CTOK
         
         tnwb2 = twcel2 + swcoef*swdown(i,j) - w2coef*wspd2 + tnwint  !! natural wet-bulb
         tnwb2 = max( min( tnwb2, tcel2 ), twcel2 )
              
         eps_a = epsa_coef * e2m**one_7th
         grayterm = eps_a * tcel2**4
         cterm = c1term * (wspd2*3600.)**upow
         
         if( coszen(i,j) .ge. coszen_min ) then !! the sun is up; use daytime formula
            bterm = swdown(i,j) * ( bs1term/coszen(i,j) + bs2term ) + grayterm
            tglob2 = ( bterm + cterm*tcel2 + numer_term ) / ( cterm + denom_term )
            twbgt2(i,j) = 0.7*tnwb2 + 0.2*tglob2 + 0.1*tcel2
         else                                                           !! no solar loading
            bterm = grayterm
            tglob2 = ( bterm + cterm*tcel2 + numer_term ) / ( cterm + denom_term )
            twbgt2(i,j) = 0.7*tnwb2 + 0.3*tglob2         
         endif
         
         twbgt2(i,j) =  twbgt2(i,j) + CTOK  

         !! Done with wet-bulb globe temperature
         
         !! (4.5) Set up for forecast icing potential MSL and FLIGHT
         !! level interpolations
         
         !! Acquire and save FLIGHT-Level interpolation
         !! indices for later application to forecast icing
         !! potential (FIP). For layers below 18,000ft MSL,
         !! we use the geopotential and model atmosphere as
         !! a basis  
                        
         kflfnd(:) = .false.
         kfldd(:) = 0
         kfluu(:) = 0
         facfld(:) = 0.00
         facflu(:) = 0.00
         
         !! if( ldbg ) then
            !! print*,'at ldbg, terht=',terht(i,j)
         !! endif
                  
         do kl = 1, ktrans                     
           if( fl_mt(kl) .gt. terht(i,j) ) then        
              
              do k = 1, kte-1            
                if( .not. kflfnd(kl) ) then
                   zmsld = (zp(i,k,j)+zb(i,k,j))/g
                   zmslu = (zp(i,k+1,j)+zb(i,k+1,j))/g           
                   if( zmsld .le. fl_mt(kl) .and. zmslu .gt. fl_mt(kl) ) then
                      kflfnd(kl) = .true.
                      kfldd(kl) = k
                      kfluu(kl) = k+1
                      dlz = zmslu - zmsld                  
                      facfld(kl) = (zmslu - fl_mt(kl))/dlz
                      facflu(kl) = (fl_mt(kl) - zmsld)/dlz
                      
                      !! if( ldbg ) then
                         !! print*,'at klMSL,k,fl_mt=',kl,k,fl_mt(kl)
                         !! print*,'zd,zu=',zmsld,zmslu
                         !! print*,'facd,facu=',facfld(kl),facflu(kl)
                         !! print*                      
                      !! endif
                      
                   endif
                endif  
              enddo
         
           endif
         enddo
         
         !! At/above 18000ft MSL, we use the 1013.25 ICAO standard
         !! atmosphere as a reference, resulting in "FLIGHT LEVELS" 
         
         do kl = ktrans+1, FIPLEVS         
           if( fl_mt(kl) .gt. terht(i,j) ) then

              do k = 1, kte-1                     
                 if( .not. kflfnd(kl) ) then
                    zstdd = ZSTDATM( p8w(i,k,j) )
                    zstdu = ZSTDATM( p8w(i,k+1,j) ) 
                    if( zstdd .le. fl_mt(kl) .and. zstdu .gt. fl_mt(kl) ) then
                      kflfnd(kl) = .true.
                      kfldd(kl) = k
                      kfluu(kl) = k+1
                      dlz = zstdu - zstdd                  
                      facfld(kl) = (zstdu - fl_mt(kl))/dlz
                      facflu(kl) = (fl_mt(kl) - zstdd)/dlz

                         !! if( ldbg ) then
                            !! print*,'at klFLT,k,fl_mt=',kl,k,fl_mt(kl)
                            !! print*,'zd,zu=',zstdd,zstdu
                            !! print*,'facd,facu=',facfld(kl),facflu(kl)
                            !! print*                      
                         !! endif                   

                    endif
                 endif         
               enddo                  

           endif
         enddo

         !! (5) Wet-bulb, "Near-surface," cloud-fraction, and snow/frz level stuff
         !! Add simulated reflectivity calculations into this k-loop; prepare
         !! for CAPE calculations; aviation-related calculations       

         maxk_ze = 0.00
         mxck_ze = 0.00
         rfl_zr  = 0.00         
         maxk_mpscdbz = -35.0 !! as in microphysics package
         kzfnd   = .false.        
         z_e(:)  = 0.00
         dBZ(:)  = -35.0
         dlz     = 0.00
         facd    = 0.00
         facu    = 0.00
         kdd     = 0
         kuu     = 0         
         ethmax  = -1.
         exting  = 1.
         lfog = .false.
         clgfound = .false.
         dz3 = 0.0
         
         if( tw2(i,j) .le. CTOK ) then
            wbzfound = .true.
         else
            wbzfound = .false.   
         endif

         kclo = -1
         kcmi = -1
         kchi = -1

         cl_lowht = clhtlw                                  !! always, for all terrain heights ( 500ft AGL == 152.4-meters )
         if( terht(i,j) .le. chi_breakpt ) then
             cl_hight = clhthi - terht(i,j)
         else
             cl_hight = chig_slope * terht(i,j) + chig_intcp  !! all AGL here in meters (not less than 1000.0)
         endif                                                !! piecewise linear above 15,000ft terht=4572.0 meters
         cl_medht = cfwght_low*(cl_hight-clhtlw) + clhtlw

         do k = 1,kte-1
            qq  = MAX( qv(i,k,j), 1.0E-25 )

            IF      ( use_tot_or_hyd_p .EQ. 1 ) THEN     !  total pressure
               prku = ( pp(i,k,j)+pb(i,k,j) )
            ELSE IF ( use_tot_or_hyd_p .EQ. 2 ) THEN     !  hydrostatic pressure
               prku = p(i,k,j)
            END IF                        
            prmb(k) = 0.01 * prku

            dta(k) = (t(i,k,j)+t0)*(prku/p1000mb)**rcp
            dtc(k) = dta(k)-CTOK

            ee  = qq * prmb(k)/(eps + 0.378*qq)          ! mb
            td  = amax1( s5/(e2p - alog( ee*d611 ) ), 200. )
            es  = exp( c15 - c_1*dta(k) - c_2/dta(k) )
            ed  = exp( c15 - c_1*td  - c_2/td )
            
            if( abs( (es - ed) ) .lt. 1.e-6 ) then
                ss = 0.0
            else
                ss  = MAX( ((es - ed) / (dta(k) - td)), 0.0 )
            endif
            
            tw  = (dta(k)*fw*prmb(k) + td*ss) / ( fw*prmb(k) + ss)    

            do  n = 1, 4
                ew  = exp (c15 - c_1*tw - c_2/tw )
                de  = fw*prmb(k)*(dta(k) - tw ) - (ew - ed)
                der = ew*(c_1-c_2/tw**2) - fw*prmb(k)
                tw  = tw    - de/der
            end do        
                        
            elog = alog( qq*prmb(k) / ( EZERO*( eps + qq ) ) )
            if( dta(k) .gt. tfrzout ) then
               tdsig = ( esl2*elog - escr ) / (elog-esl1)
            else
               tdsig = 6150. / (22.514 - elog)  !!  degrees k
            end if

            tdh = min( tdsig, dta(k) )
            twh(k) = max( min( tw, dta(k) ), tdh )
            tbh(k) = 0.5 * ( dta(k) + twh(k) )
            
            z_at_w(k) = (zp(i,k,j)+zb(i,k,j))/g
            zht(k) = 0.5*( z_at_w(k) + ((zp(i,k+1,j)+zb(i,k+1,j))/g))    !! half-layer heights following phy_prep
            dz8w(k) = (zp(i,k+1,j)+zb(i,k+1,j))/g - z_at_w(k)            !! full-layer thicknesses following phy_prep; with updated zp; like dz8w
            
            if( k .eq. 1 ) then
               z_at_w2 = ((zp(i,k+1,j)+zb(i,k+1,j))/g)                   !! used for FOG diagnostics below          
            endif 

            !! Reminder: terht == zb(i,1,j) / g and zp(i,1,j)=0.0, so z_at_w(1) = terht
            !! zht's are w/rt mean-sea-level
            
            if( .not. wbzfound ) then
              if( twh(k) .le. CTOK ) then
                 wbzfound = .true.
                 if( k .eq. 1 ) then
                    faczk    = (tw2(i,j) - CTOK) / (tw2(i,j) - twh(k))
                    zhtlw = 2.0 + terht(i,j)                            !! 2-meters AGL
                 else
                    faczk    = (twh(k-1) - CTOK) / (twh(k-1) - twh(k))
                    zhtlw = zht(k-1)
                 endif
                 dzht = zht(k) - zhtlw 
                 wbfrzlev( i,j ) = (zhtlw + faczk*dzht) - terht(i,j)
              endif             
            endif            
            
            !! The following rh form is used for cloud fractions

            es = s4 * exp(s5 * (1.0 / 273.0 - 1.0 / dta(k)))
            qs = eps * es / (prku - es)
            rhk(k) = MAX( MIN( qq / qs * 100., 100.), 0.0)

            !! For cloud fraction computation

            !! if( prku .gt. 97000. ) kclo=k
            !! if( prku .gt. 80000. ) kcmi=k
            !! if( prku .gt. 45000. ) kchi=k

            if( zht(k) - terht(i,j) .lt. cl_lowht ) kclo=k            
            if( zht(k) - terht(i,j) .lt. cl_medht ) kcmi=k
            if( zht(k) - terht(i,j) .lt. cl_hight ) kchi=k

            !! For FIP and SLD calculations

            if( prku .gt. 30000. ) k300=k
            if( zht(k) - terht(i,j) .lt. clg2 ) kclg2=k
            if( prku .gt. 25000. ) k250=k
            if( zht(k) - terht(i,j) .lt. clg1 ) kclg1=k            

            !! For MUCAPE calculations: Find parcel with max theta-e in lowest 3 km AGL

            if( ( z_at_w(k) - terht(i,j) ) .lt. 3000. ) THEN 
               ev   = qq * prmb(k)/(eps+qq)
               tlcl = TLCLC1/(log(dta(k)**TLCLC2/ev)-TLCLC3)+TLCLC4
               eth  =     dta(k)*(1000./prmb(k))**( GAMMA_RIP*(1.+GAMMAMD*qq) )*     &
                          exp( (THTECON1/tlcl-THTECON2)*qq*(1.+THTECON3*qq) )            

               IF ( eth .gt. ethmax ) THEN
                 kmxte=k
                 ethmax=eth
                 prmbmx=prmb(k)
                 qqmax=qq
                 tkmax=dta(k)
                 ghtmax=z_at_w(k)  !! JNM comment: In RIP4, it appears that "ght" which is equivalent to
                                   !! our half-layer height field "zht" is used throughout CAPE calculations there;
                                   !! whereas in ARWpost, it appears that ght ends up being at the full-levels
                                   !! since it is computed directly from PH and PHB without interpolation 
               END IF
               !if( ldbg ) then
               !   print*,'k,eth,ethmax=',k,eth,ethmax
               !endif      
            end if

            !! End set-up items for MUCAPE calculations            

            !!
            !! Further set up for aviation-related calculations
            !!  

            qra = MAX( qr_curr(i,k,j), 0.00 ) 
            qsn = MAX( qs_curr(i,k,j), 0.00 ) 
            qgr = MAX( qg_curr(i,k,j), 0.00 )            

            tva    = dta(k) * (eps+qq)/(eps*(1.+qq)) 
            rhoair = prku / ( RD * tva )               !! moist air density in kg/m3
                        
            qcol( i,j ) = qcol( i,j ) + qq * rhoair * dz8w(k) !! kg/m2 = mm
                        
            qclw(k)   = MAX( qc_curr(i,k,j), 0.00 )       !! kg cld-liq-water/kg-dry-air
            qclice(k) = MAX( qi_curr(i,k,j), 0.00 )       !! kg cld-ice/kg-dry-air            
            qctot(k)  = qclw(k)+qclice(k)
            qrlw(k)   = qra
            qsnow     = qsn
            qgraup    = qgr
            qrtot(k)  = qra+qsn+qgr                       !! total ppt mix ratio aloft
            qcond(k)  = qctot(k)+qrtot(k)                 !! total condensate of any kind (cld+ppt)

            !! rhoair -- kg/m3 air moistened by vapor only
            !! qq     -- kg vapor/kg-dry air 

            vovermd=(1.+qq)/rhoair+(qclw(k)+qrlw(k))/rhowat+ &
               (qgraup+qclice(k)+qsnow)/rhoice       !! m**3/kg of air with all moist vbles

            conclc(k) = g_per_kg * qclw(k)/vovermd   !! g/m3 cloud-liquid
            conclp(k) = g_per_kg * qrlw(k)/vovermd   !! 
            concfc(k) = g_per_kg * qclice(k)/vovermd !! g/m3 cloud-ice
            concfp(k) = g_per_kg * qsnow/vovermd     !!
            concfg(k) = g_per_kg * qgraup/vovermd    !! add graupel term per Stan Benjamin....     

            temp_fac = min(1.,max((dta(k)-271.15),0.) )
            coef_snow = coeffp_dry*(1.-temp_fac)                   &
                      + coeffp_wet* temp_fac
            
            bextclw = coeflc * conclc(k) ** exponlc
            bextcli = coeffc * concfc(k) ** exponfc
            
            !! V4.2.1 surface-layer FOG decision tree here.
            
            if( k .eq. 1 ) then
               if( bextclw .ge. bext_lgt ) then                ! possible water fog .. 
                   l_lay2_1_temp_invers = .false.
                   l_lay2_1_statically_stable = .false.
                   l_sfc_lay_stable = .false.
                   l_spd10_vlight = .false.
                   l_spd10_light = .false.
                   l_2m_LCL_in_lay1 = .false.
                   l_eqv_pseudo_stable = .false.
                   lfog_weak = .false.
                   lfog_strong_wind = .false.
                   lfog_light_wind = .false.
                                      
                   IF      ( use_tot_or_hyd_p .EQ. 1 ) THEN
                      prk2 = ( pp(i,2,j)+pb(i,2,j) )
                   ELSE IF ( use_tot_or_hyd_p .EQ. 2 ) THEN     ! hydrostatic pressure
                      prk2 = p(i,2,j)
                   ENDIF               
                   
                   dta2 = (t(i,2,j)+t0)*(prk2/p1000mb)**rcp     ! compare layer 2 with layer 1 temperature     
                   if( dta2 .gt. dta(1) ) then                  ! true thermal inversion; FOG situation likely
                      l_lay2_1_temp_invers = .true.
                      lfog = .true.
                   endif
                   
                   q2m   = MAX(q2(i,j),1.0E-25)
                   p2m = p8w(i,kts,j) * 0.01             ! use p8w(i,kts,j) for 2-meter pressure 2-meter pressure (mb)
                   tht2m  = t2(i,j)*(p1000mb/p2m)**drcp
                   thtv2m = tht2m *( 1.0 + 0.608*q2m )    !! Note t2() is already in Kelvin, whereas t() is perturbation theta
                   thetv1 = (t(i,1,j)+t0)*( 1.0 + 0.608*MAX(qv(i,1,j),0.0) )
                   if( thetv1 .gt. thtv2m ) then
                      l_sfc_lay_stable = .true.
                      if( .not. lfog ) then
                         lfog = .true.
                         !! WRITE( MESG, '( A,2I8 )' ) 'Fog flag set via l_sfc_lay_stable at i,j=',i,j
                         !! CALL wrf_message(MESG)
                      endif   
                   endif                   
                   
                   ! Determine the LCL of the 2-meter parcel. This follows the formulation for computing CAPEs below
                   
                   qvppari = q2m
                   tmkpari = t2(i,j)                   
                   prmbmx  = p2m
                   ghtmax  = z_at_w(1)                   ! z_at_w(1) = terht
                   
                   cpm  = cp*(1.+CPMD*qvppari)
                   ev   = max(1.e-20,qvppari*prmbmx/(EPS+qvppari))
                   tlcl = TLCLC1/(log(tmkpari**TLCLC2/ev)-TLCLC3)+TLCLC4
                   ethpari = tmkpari*(1000./prmbmx)**(GAMMA_RIP*(1.+GAMMAMD*qvppari))*   &
                             exp((THTECON1/tlcl-THTECON2)*qvppari*                       &
                             (1.+THTECON3*qvppari))
                   zlcl = ghtmax+(tmkpari-tlcl)/(G/cpm)
                   if( zlcl .le. z_at_w2 ) then
                      l_2m_LCL_in_lay1 = .true.
                   endif                 
                   
                   !! Now Lift the 2-meter parcel pseudoadiabatically since at least a little saturation is assumed,
                   !! given that there is enough half-layer-1 liquid water to reduce visibility. This follows the
                   !! formulation for computing CAPEs below. We want to determine the "internal" stability of the 2-meter
                   !! to half-layer-1 "layer" 
                   
                   tmklift = tonpsadiabat(ethpari,prmb(1))
                   if( tmklift .lt. -9.0 ) then          !! -9.9999 is set inside the function as "out of table lookup bounds"
                      WRITE (MESG ,'(A,3I8,2X,2F13.6)' ) 'Outside of lookup table bounds at i,j,k; prs,thte=',i,j,k,ethpari,prmb(k)
                      CALL wrf_error_fatal ( TRIM(MESG) )
                   endif
                   eslift  = EZERO*exp(ESL1*(tmklift-CTOK)/    &
                             (tmklift-ESL2))
                   qvplift = EPS*eslift/(prmb(1)-eslift)          
                   tvenv   = virtual(dta(1),qv(i,1,j))
                   tvlift  = virtual(tmklift,qvplift)
                   
                   if( (tvlift-tvenv) .le. 0.0 ) then
                      l_eqv_pseudo_stable = .true.
                   endif                                    

                   !! For Q/A purposes only, determine the stability of layer 1 if lifted to layer 2 
                   !! using virtual potential temperature
                   
                   thetv2 = (t(i,2,j)+t0)*( 1.0 + 0.608*MAX(qv(i,2,j),0.0) )                   
                   thetv1 = (t(i,1,j)+t0)*( 1.0 + 0.608*MAX(qv(i,1,j),0.0) )
                   if( thetv2 .ge. thetv1 ) then
                      l_lay2_1_statically_stable = .true.
                   endif
                   
                   if (thetv2 .lt. thetv1 ) then  !! can occur when lay1/2 temp diffs are very close but mix-ratio is smaller in lay 2
                      !! WRITE( MESG, '( A, 2I8 )' ) 'Actual temperature inversion but not thetav invers at i,j=',i,j
                      !! CALL wrf_message( MESG )
                      !! WRITE( MESG, '( A, 2F15.6 )' ) 'thetv2,thetv1=',thetv2,thetv1
                      !! CALL wrf_message( MESG )
                      !! WRITE( MESG, '( A, 2F15.6 )' ) 't(i,2,j)+t0,t(i,1,j)+t0=',(t(i,2,j)+t0),(t(i,1,j)+t0)
                      !! CALL wrf_message( MESG )
                      !! WRITE( MESG, '( A, 2F15.6 )' ) 'qv(i,2,j),qv(i,1,j)=',qv(i,2,j),qv(i,1,j) 
                      !! CALL wrf_message( MESG )
                   endif
                   
                   if( rh1(i,j) .lt. 0.90 ) then
                      !! WRITE( MESG, '( A,2I8,2F15.6 )' ) 'Layer-1 mean RH < 90% but conclc(1) at i,j=',i,j,rh1(i,j),conclc(1)
                      !! CALL wrf_message( TRIM(MESG) )
                   endif                   
                                                                       
                   if( wspd10(i,j) .le. 3.5 ) then
                      l_spd10_light = .true.
                      if( wspd10(i,j) .le. 1.5 ) then
                         l_spd10_vlight = .true.
                      endif
                   endif    
                   
                   if( lfog ) then
                      if( .not. l_spd10_light ) then
                         lfog_strong_wind = .true.
                         !! WRITE( MESG, '( A,2I8 )' ) 'Fog flag set but wind-speed is strong at i,j=',i,j
                         !! CALL wrf_message( MESG )
                      else if ( .not. l_spd10_vlight ) then
                         lfog_light_wind = .true.
                         !! WRITE( MESG, '( A,2I8 )' ) 'Fog flag set but wind-speed not very light at i,j=',i,j
                         !! CALL wrf_message( MESG )
                      endif
                   endif           
                   
                   if( l_2m_LCL_in_lay1 .and. .not. lfog ) then  !! weaker criteria here....
                       if( l_eqv_pseudo_stable ) then
                          lfog = .true.
                          lfog_weak = .true.
                          !! WRITE( MESG, '( A,2I8 )' ) 'Fog flag set combo of l_2m_LCL_in_lay1 and  l_eqv_pseudo_stable at i,j=',i,j
                          !! CALL wrf_message( MESG )
                       endif                          
                   endif
                   
                   if( l_2m_LCL_in_lay1 .and. .not. lfog ) then  !! weaker criteria here....
                      if( q2m .gt. qv(i,1,j) ) then  
                         if( rh2(i,j) .gt. rh1(i,j) ) then
                            !! WRITE( MESG, '( A,2I8 )' ) 'Q2M greater than QV(1) and RH higher at 2-Meters at i,j = ',i,j
                            !! CALL wrf_message( MESG )
                            lfog = .true.
                            lfog_weak = .true.
                         else
                            delt2 = t2(i,j) - td2(i,j)
                            delt1 = t1(i,j) - td1(i,j)
                            if( delt1 .gt. delt2 ) then
                                !! WRITE( MESG, '( A,2I8 )' ) 'Q2M greater than QV(1) and DP depression smaller at 2-Meters at i,j = ',i,j
                                !! CALL wrf_message( MESG )
                            endif
                            lfog = .true.
                            lfog_weak = .true.
                         endif
                      endif
                   endif
 
                   if( lfog ) then
                   
                      !! if( ldbg ) then
                      !!    WRITE( MESG, '( A,2I8 )' ) 'Debug Fog situation, i,j=',i,j
                      !!    CALL wrf_message( MESG )
                      !!    WRITE( MESG, '( A, 4F15.6 )' ) 'bext: clw/den/10th/max=',bextclw,bext_den,bext10th,bext_max
                      !!    CALL wrf_message( MESG )
                      !!    WRITE( MESG, '( A, 2F15.6 )' ) 'cllwcvden,conclc(k)=',cllwcvden,conclc(k)
                      !!    CALL wrf_message( MESG )
                      !!    WRITE( MESG, '( A, F15.6 )' )  't2 minus td2 =',t2(i,j) - td2(i,j)
                      !!    CALL wrf_message( MESG ) 
                      !!    WRITE( MESG, '( A, 2F15.6 )' ) 'qv(i,1,j),q2m=',qv(i,1,j),q2m
                      !!    CALL wrf_message( MESG )
                      !!    WRITE( MESG, '( A, 2F15.6 )' ) '(t(i,1,j)+t0),t2(i,j)=',(t(i,1,j)+t0),t2(i,j)
                      !!    CALL wrf_message( MESG )
                      !!    WRITE( MESG, '( A, 2F15.6 )' ) 'rh1(i,j),rh2(i,j)=',rh1(i,j),rh2(i,j)
                      !!    CALL wrf_message( MESG )
                      !!    WRITE( MESG, '( A, 2F15.6 )' ) 'thetv1,thtv2m=',thetv1,thtv2m
                      !!    CALL wrf_message( MESG )
                      !!    WRITE( MESG, '( A, 2F15.6 )' ) 'l_2m_LCL_in_lay1=',l_2m_LCL_in_lay1
                      !!    CALL wrf_message( MESG )
                      !!    WRITE( MESG, '( A, 4F15.6 )' ) 'zlcl,z_at_w(2),z_at_w(1),zht(k)=',zlcl,z_at_w2,z_at_w(1),zht(k)
                      !!    CALL wrf_message( MESG )
                      !!    WRITE( MESG, '( A, 2F15.6 )' ) 'tlcl,tmkpari=',tlcl,tmkpari
                      !!    CALL wrf_message( MESG )
                      !!    if( .not. l_eqv_pseudo_stable ) then
                      !!       WRITE( MESG, '( A )' ) '2m_LCL_in_lay1 is TRUE, but l_eqv_pseudo_stable is FALSE'
                      !!       CALL wrf_message( MESG )
                      !!       WRITE( MESG, '( A, 2F15.6 )' ) 'tvlift,tvenv = ',tvlift,tvenv
                      !!       CALL wrf_message( MESG )                            
                      !!    else
                      !!       WRITE( MESG, '( A, L10 )' ) 'l_eqv_pseudo_stable=',l_eqv_pseudo_stable
                      !!       CALL wrf_message( MESG )
                      !!       WRITE( MESG, '( A, 2F15.6 )' ) 'tvlift,tvenv = ',tvlift,tvenv
                      !!       CALL wrf_message( MESG )
                      !!    endif   
                      !!    WRITE( MESG, '( A, L10 )' ) 'l_spd10_light=',l_spd10_light
                      !!    CALL wrf_message( MESG )
                      !!    WRITE( MESG, '( A, L10 )' ) 'l_spd10_vlight=',l_spd10_vlight
                      !!    CALL wrf_message( MESG )
                      !!    WRITE( MESG, '( A, L10 )' ) 'l_sfc_lay_stable=',l_sfc_lay_stable
                      !!    CALL wrf_message( MESG ) 
                      !!    WRITE( MESG, '( A, L10 )' ) 'l_lay2_1_statically_stable=',l_lay2_1_statically_stable
                      !!    CALL wrf_message( MESG )
                      !! endif

                      if( td2(i,j) .lt. t2(i,j) ) then          ! dewpoint indicates sub-saturation at 2-meters
                         delt2 = t2(i,j) - td2(i,j)             ! WRF atmosphere has fog but is sub-saturated at 2-meters                            
                         wgtb = 1.0
                         if( delt2 .lt. 3.0 ) then              ! degrees K 
                             wgtb = ( 1.0 - exp( -delt2 ) ) / exp_denom
                         endif
                         if( bextclw .gt. bext_den ) then
                             bextclw = bext_den * wgtb + bextclw * (1.0 - wgtb )
                         else if( bextclw .gt. bext_mod ) then
                             bextclw = bext_mod * wgtb + bextclw * (1.0 - wgtb )
                         else if( bextclw .gt. bext_lgt ) then
                             bextclw = bext_lgt * wgtb + bextclw * (1.0 - wgtb )
                         endif                         
                      endif                           
                      bextclw = min( bextclw, bext_max )
                      
                      if( bextclw .ge. bext_den ) then        !! assign "standard" fog-type here: dense/moderate/light
                          fogflag( i,j ) = 3.0
                      else if( bextclw .ge. bext_mod ) then
                          fogflag( i,j ) = 2.0
                      else if( bextclw .ge. bext_lgt ) then
                          fogflag( i,j ) = 1.0
                      endif
                      
                      !! Adjust for weaker situations
                      
                      if( lfog_weak ) then
                         if( nint(fogflag( i,j )) .eq. 3 ) fogflag( i,j ) = 2.0                
                      endif
                      
                      if( lfog_strong_wind ) then
                         if( nint(fogflag( i,j )) .eq. 3 ) fogflag( i,j ) = 1.0
                         if( nint(fogflag( i,j )) .eq. 2 ) fogflag( i,j ) = 1.0
                      else if( lfog_light_wind ) then
                         fogflag( i,j ) = MAX( (fogflag( i,j ) - 1.0), 0.0 )
                      endif

                   else                                        ! no fog criteria met, can't define fog situation                      
                      bextclw = min( bextclw, bext10th )       ! limit k=1 bext to equivalent 10th of a mile visibility unless fog                   
                   endif

                else if ( bextclw .ge. 0.85*bext_lgt ) then    !! marginal conditions; enforce strict stability                
                   
                   l_lay2_1_temp_invers = .false.
                   l_sfc_lay_stable = .false.
                   l_spd10_vlight = .false.
                   l_2m_LCL_in_lay1 = .false.
                   l_eqv_pseudo_stable = .false. 

                   IF      ( use_tot_or_hyd_p .EQ. 1 ) THEN
                      prk2 = ( pp(i,2,j)+pb(i,2,j) )
                   ELSE IF ( use_tot_or_hyd_p .EQ. 2 ) THEN     ! hydrostatic pressure
                      prk2 = p(i,2,j)
                   ENDIF
                   dta2 = (t(i,2,j)+t0)*(prk2/p1000mb)**rcp     ! compare layer 2 with layer 1 temperature     
                   if( dta2 .gt. dta(1) ) then                  ! true thermal inversion; FOG situation likely
                      l_lay2_1_temp_invers = .true.
                   endif
                   
                   q2m   = MAX(q2(i,j),1.0E-25)
                   p2m = p8w(i,kts,j) * 0.01             ! 2-meter pressure in mb
                   tht2m  = t2(i,j)*(p1000mb/p2m)**drcp
                   thtv2m = tht2m*( 1.0 + 0.608*q2m )  !! Note t2() is already in Kelvin, whereas t() is perturbation theta
                   thetv1 = (t(i,1,j)+t0)*( 1.0 + 0.608*MAX(qv(i,1,j),0.0) )
                   if( thetv1 .gt. thtv2m ) then
                      l_sfc_lay_stable = .true.
                   endif  
                   if( wspd10(i,j) .le. 1.5 ) then
                      if( wspd10(i,j) .le. 1.5 ) then
                         l_spd10_vlight = .true.
                      endif
                   endif
                   
                   ! Determine the LCL of the 2-meter parcel. This follows the formulation for computing CAPEs below
                   
                   qvppari = q2m
                   tmkpari = t2(i,j)                   
                   prmbmx  = p2m
                   ghtmax  = z_at_w(1)
                   
                   cpm  = cp*(1.+CPMD*qvppari)
                   ev   = max(1.e-20,qvppari*prmbmx/(EPS+qvppari))
                   tlcl = TLCLC1/(log(tmkpari**TLCLC2/ev)-TLCLC3)+TLCLC4
                   ethpari = tmkpari*(1000./prmbmx)**(GAMMA_RIP*(1.+GAMMAMD*qvppari))*   &
                             exp((THTECON1/tlcl-THTECON2)*qvppari*                       &
                             (1.+THTECON3*qvppari))
                   zlcl = ghtmax+(tmkpari-tlcl)/(G/cpm)
                   if( zlcl .le. z_at_w2 ) then
                      l_2m_LCL_in_lay1 = .true.
                   endif 

                   !! Now Lift the 2-meter parcel pseudoadiabatically since at least a little saturation is assumed,
                   !! given that there is enough half-layer-1 liquid water to reduce visibility. This follows the
                   !! formulation for computing CAPEs below. We want to determine the "internal" stability of the 2-meter
                   !! to half-layer-1 "layer"
                   
                   tmklift = tonpsadiabat(ethpari,prmb(1))
                   if( tmklift .lt. -9.0 ) then          !! -9.9999 is set inside the function as "out of table lookup bounds"
                      WRITE (MESG ,'(A,3I8,2X,2F13.6)' ) 'Outside of lookup table bounds at i,j,k; prs,thte=',i,j,k,ethpari,prmb(k)
                      CALL wrf_error_fatal ( TRIM(MESG) )
                   endif
                   eslift  = EZERO*exp(ESL1*(tmklift-CTOK)/    &
                             (tmklift-ESL2))
                   qvplift = EPS*eslift/(prmb(1)-eslift)          
                   tvenv   = virtual(dta(1),qv(i,1,j))
                   tvlift  = virtual(tmklift,qvplift)
                   
                   if( (tvlift-tvenv) .le. 0.0 ) then
                      l_eqv_pseudo_stable = .true.
                   endif 
 
                   if( l_lay2_1_temp_invers .OR. l_sfc_lay_stable ) then
                      if( l_spd10_vlight .and. l_2m_LCL_in_lay1 .and. l_eqv_pseudo_stable ) then
                         if( rh1(i,j) .gt. 0.90 ) then
                            lfog = .true.
                            !! WRITE( MESG, '( A, 2F15.6 )' ) 'Marginal lay-1 bextclw and probabable light fog detected: ',bextclw,bext_lgt 
                            !! CALL wrf_message( MESG )
                         endif   
                      endif
                   endif
                   
                   if( lfog ) then
                   
                      !! if( ldbg ) then
                      !!    WRITE( MESG, '( A,2I8 )' ) 'Debug MARGINAL Fog situation, i,j=',i,j
                      !!    CALL wrf_message( MESG )
                      !!    WRITE( MESG, '( A, 4F15.6 )' ) 'bext: clw/den/10th/max=',bextclw,bext_den,bext10th,bext_max
                      !!    CALL wrf_message( MESG )
                      !!    WRITE( MESG, '( A, 2F15.6 )' ) 'cllwcvden,conclc(k)=',cllwcvden,conclc(k)
                      !!    CALL wrf_message( MESG )
                      !!    WRITE( MESG, '( A, F15.6 )' )  't2 minus td2 =',t2(i,j) - td2(i,j)
                      !!    CALL wrf_message( MESG ) 
                      !!    WRITE( MESG, '( A, 2F15.6 )' ) 'qv(i,1,j),q2m=',qv(i,1,j),q2m
                      !!    CALL wrf_message( MESG )
                      !!    WRITE( MESG, '( A, 2F15.6 )' ) '(t(i,1,j)+t0),t2(i,j)=',(t(i,1,j)+t0),t2(i,j)
                      !!    CALL wrf_message( MESG )
                      !!    WRITE( MESG, '( A, 2F15.6 )' ) 'rh1(i,j),rh2(i,j)=',rh1(i,j),rh2(i,j)
                      !!    CALL wrf_message( MESG )
                      !!    WRITE( MESG, '( A, 2F15.6 )' ) 'thetv1,thtv2m=',thetv1,thtv2m
                      !!    CALL wrf_message( MESG )
                      !!    WRITE( MESG, '( A, 2F15.6 )' ) 'l_2m_LCL_in_lay1=',l_2m_LCL_in_lay1
                      !!    CALL wrf_message( MESG )
                      !!    WRITE( MESG, '( A, 4F15.6 )' ) 'zlcl,z_at_w(2),z_at_w(1),zht(k)=',zlcl,z_at_w2,z_at_w(1),zht(k)
                      !!    CALL wrf_message( MESG )
                      !!    WRITE( MESG, '( A, 2F15.6 )' ) 'tlcl,tmkpari=',tlcl,tmkpari
                      !!    CALL wrf_message( MESG )
                      !!    if( .not. l_eqv_pseudo_stable ) then
                      !!       WRITE( MESG, '( A )' ) '2m_LCL_in_lay1 is TRUE, but l_eqv_pseudo_stable is FALSE'
                      !!       CALL wrf_message( MESG )
                      !!       WRITE( MESG, '( A, 2F15.6 )' ) 'tvlift,tvenv = ',tvlift,tvenv
                      !!       CALL wrf_message( MESG )                            
                      !!    else
                      !!       WRITE( MESG, '( A, L10 )' ) 'l_eqv_pseudo_stable=',l_eqv_pseudo_stable
                      !!       CALL wrf_message( MESG )
                      !!       WRITE( MESG, '( A, 2F15.6 )' ) 'tvlift,tvenv = ',tvlift,tvenv
                      !!       CALL wrf_message( MESG )
                      !!    endif   
                      !!    WRITE( MESG, '( A, L10 )' ) 'l_spd10_light=',l_spd10_light
                      !!    CALL wrf_message( MESG )
                      !!    WRITE( MESG, '( A, L10 )' ) 'l_spd10_vlight=',l_spd10_vlight
                      !!    CALL wrf_message( MESG )
                      !!    WRITE( MESG, '( A, L10 )' ) 'l_sfc_lay_stable=',l_sfc_lay_stable
                      !!    CALL wrf_message( MESG ) 
                      !!    WRITE( MESG, '( A, L10 )' ) 'l_lay2_1_statically_stable=',l_lay2_1_statically_stable
                      !!    CALL wrf_message( MESG )
                      !! endif
                      
                      fogflag( i,j ) = 1.0  !! Always light fog in marginal situations
                   
                   endif  !! End if lfog for MARGINAL conditions 
 
                endif     !! End if bextclw .ge. bext_lgt; or not                                                
            endif         !! End if k=1 for V4.2.1 surface-layer FOG decision tree                        
            
            !! END V4.2.1 surface-layer FOG decision tree.
            
            !! Units of Bext in km**-1

            bext(k) = bextclw + bextcli + coef_SNOW * concfp(k) ** exponfp  &
                              + coeflp * conclp(k) ** exponlp     &
                              + coeffg * concfg(k) ** exponfg + 1.e-10            
            
            !! if( ldbg .and. k.eq.1 ) then
            !!   print*,'i,j,bextclw,bext(k)=',i,j,bextclw,bext(k)
            !!   print*,'bext10th,bext_max  =',i,j,bext10th,bext_max
            !! endif

            !! For ceiling calculation routine integrates upward using layered bext,
            !! until an upward beam of light would be extinguished to .02 times
            !! its original intensity.

            if( .not. clgfound ) then

              dzf = km_per_m *dz8w(k)                    ! m to km             
              extingt=exting*exp(-bext(k)*dzf)
              if( k .le. 3 ) dz3 = dz3 + dzf
                            
              if (extingt.le.thcon) then
                  logterm  = -log(thcon/exting)/bext(k) !! additional "dz" thickness to get to .02 of original light                  
                  clg(i,j) = clg(i,j)+logterm           !! km here; will be converted to meters below
                  optd(i,j) = optd(i,j)+logterm*bext(k) !! total optical depth
                  clgfound = .true.
                  if( lfog .and. clg(i,j) .gt. dz3 ) then  !! this can happen under fog situations; will cause ceiling
                     clg(i,j) = CEILMAX / m_per_km         !! to be calculated via rh / cldfrac method below (DTC)
                  endif
                  goto 22         
              endif
              clg(i,j)=clg(i,j)+dzf
              optd(i,j) = optd(i,j) + bext(k)*dzf
              exting=extingt
              
            endif
22          CONTINUE

            !! End initial set of aviation-related calculations            

            !!
            !! For reflectivity calculations         
            !! Adjust factor for brightband, where snow or graupel particle        
            !! scatters like liquid water (alpha=1.0) because it is assumed to     
            !! have a liquid skin.                                                 

            IF (dta(k) .gt. CTOK) THEN
              factorb_s=factor_s/alpha
              factorb_g=factor_g/alpha
            ELSE
              factorb_s=factor_s
              factorb_g=factor_g
            ENDIF

            !! Calculate variable intercept parameters         

            temp_c = amin1(-0.001, dtc(k))
            sonv   = amin1(2.0e8, 2.0e6*exp(-0.12*temp_c))

            gonv = gon
            if (qgr.gt.r1) then
              gonv = 2.38*(PI*rho_g/(rhoair*qgr))**0.92
              gonv = max(1.e4, min(gonv,gon))
            endif            

            ronv = ron2
            IF (qra.gt. r1) THEN
               ronv = ron_const1r*tanh((ron_qr0-qra)     &
                      /ron_delqr0) + ron_const2r
            endif         

            !! Total equivalent reflectivity factor (z_e, in mm^6 m^-3) is
            !! the sum of z_e for each hydrometeor species:

            !! z_e(k) =   factor_r  * (rhoair*qra)**1.75 /ronv**.75   &
            !!          + factorb_s * (rhoair*qsn)**1.75 /sonv**.75   &
            !!         + factorb_g * (rhoair*qgr)**1.75 /gonv**.75

            FACA   = rhoair * qra / ronv
            FACN   = rhoair * qsn / sonv
            FACR   = rhoair * qgr / gonv
            z_e(k) =   factor_r  * rhoair * qra * SQRT( SQRT( FACA ) )**3  &
                     + factorb_s * rhoair * qsn * SQRT( SQRT( FACN ) )**3  &
                     + factorb_g * rhoair * qgr * SQRT( SQRT( FACR ) )**3                  

            !! Adjust small values of Z_e so that dBZ is no lower than -40

            z_e(k) = max(z_e(k),.0001)
            
            dBZ(k) = 10. * log10(z_e(k))
            refl_htrgen(i,k,j) = MAX(-35., dBZ(k)) !! Generic 3D reflectivity (output; new 3.9.1.1)

            vilh(i,j) =  vilh(i,j) + (qra + qsn + qgr)*dz8w(k)*rhoair
                                    
            !! if( i .eq. 8 .and. j .eq. 32 .and. k .eq. 1 ) then
            !!    print*,'z_e_rain=',k,factor_r  * (rhoair*qra)**1.75 /ronv**.75
            !!    print*,'dbz,dbz6=',10.*log10(z_e(k)),refl_10cm(i,k,j)
            !!    print*,'rhoair,qra=',rhoair,qra
            !!    print*,'factor_r,ronv=',factor_r,ronv            
            !! endif                     

            maxk_ze = max( z_e(k),maxk_ze )
            if( k .lt. kte-16 ) then
               mxck_ze = max( z_e(k),mxck_ze )            
            endif
            
            if( .not. kzfnd ) then
               zagld = z_at_w(k) - terht(i,j)
               zaglu = (zp(i,k+1,j)+zb(i,k+1,j))/g - terht(i,j)            
               if( zagld .le. AGL1KM .and. zaglu .ge. AGL1KM ) then
                  kzfnd = .true.
                  kdd = k
                  kuu = k+1
                  dlz = zaglu - zagld                  
                  facd = (zaglu - AGL1KM)/dlz
                  facu = (AGL1KM - zagld)/dlz
                  if( do_radar_ref .eq. 1 ) then
                     refl_mpsku = refl_10cm(i,kuu,j)
                     refl_mpskd = refl_10cm(i,kdd,j)
                  endif
               endif
            endif      
            
            if( do_radar_ref .eq. 1 ) then
            
                !! Fix up edges due to edge problems in refl_10cm()
                
                refl_mpsk = refl_10cm(i,k,j)

                if( .not. firstime ) then
                    if( i .eq. ids   .or.  &
                        i .eq. ide-1 .or.  & 
                        j .eq. jds   .or.  & 
                        j .eq. jde-1 ) then
                        refl_mpsk = -35.0
                        refl_mpsku = -35.0
                        refl_mpskd = -35.0         
                    endif
                endif            

                maxk_mpscdbz = max( refl_mpsk,maxk_mpscdbz )

                !! add diagnose "radar-derived VIL" from equivalent radar reflectivity
                !! radarVIL = (integral of LW*dz )/1000.0  (in kg/m^2)
                !! LW = 0.00344 * z_e** (4/7)  in g/m^3 [via AFWA]. The factor VILFAC = 3.4e-6 =
                !! 0.00344 coefficient times (1/1000.0) converting grams to kilograms

                ze_scheme = 10.0**(0.1*refl_mpsk)
                radarvilh(i,j) = radarvilh(i,j) + VILFAC * dz8w(k) * ze_scheme**0.57143
            
            endif

            !! Compute average vertical velocity in micro-bars per second
            !! See Bluestein, Vol 1, page 300 for rather arduous units conversion simplifications
            !! rhoair in kg/m3; w0avg in m/sec; g in m/s**2; output vv in microbars/second

            vvubs(k) = -g*10.0*rhoair*w0avg(i,k,j)  !! microbars/second [neg values upward vertical velocity]            

         end do  !! end k-loop (5) Wet-bulb etc.

         !! CAWO: PV surfaces: Geopotential Height of 2.0PVU surface and its potential temperature
         
         if( .not. firstime ) then

            zpvor20( i,j ) = ght_pl(i,p100,j)          !! Initial guess 
            tpvor20( i,j ) = thta_pl(i,p100,j)            
            do k = kte-3,1,-1                          !! kte-3: potvort(kte-1) === potvort(kte-2)          
               if( zht(k+1) .ge. ght_pl(i,p100,j) ) then 
                   CYCLE
               else if( zht(k+1) .lt. ght_pl(i,p100,j) .and. &
                        zht(k+1) .gt. ght_pl(i,p850,j) ) then  !! NOTE ODDITY p_400 index as vble p400 is already declared in Registry
                  if( potvort(i,k+1,j) .gt. vcrit20 .and. potvort(i,k,j) .le. vcrit20 ) then
                     faczk    = (vcrit20-potvort(i,k,j))/( potvort(i,k+1,j)-potvort(i,k,j) )
                     zhtlw = zht(k)
                     dzht = zht(k+1) - zhtlw
                     zpvor20( i,j ) = zhtlw + faczk*dzht
                     td = t(i,k,j)+t0
                     dthz = t(i,k+1,j)+t0 - td
                     tpvor20( i,j ) = td + faczk*dthz
                     kvl = k
                     if( zht(kvl) .gt. ght_pl(i,p250,j) ) then !! Search for a secondary, lower surface
                        do kv = kvl-1,1,-1
                           if( zht(kv+1) .gt. ght_pl(i,p550,j) ) then
                              if( potvort(i,kv+1,j) .gt. vcrit20 .and. potvort(i,kv,j) .le. vcrit20 ) then
                                 faczk    = (vcrit20-potvort(i,kv,j))/( potvort(i,kv+1,j)-potvort(i,kv,j) )
                                 zhtlw = zht(kv)
                                 dzht = zht(kv+1) - zhtlw
                                 zpvor20( i,j ) = zhtlw + faczk*dzht
                                 td = t(i,kv,j)+t0
                                 dthz = t(i,kv+1,j)+t0 - td
                                 tpvor20( i,j ) = td + faczk*dthz
                                 exit 
                              endif
                           else
                              exit
                           endif
                        enddo
                     endif
                     exit                                                  
                  else
                     CYCLE   
                  endif
               else 
                  exit
               endif
            enddo
            zpvor20( i,j ) = max( zpvor20( i,j),ght_pl(i,p600,j))
            tpvor20( i,j ) = max( tpvor20( i,j),  t_pl(i,p600,j))
            
            if( zpvor20( i,j ) .lt. ght_pl(i,p_400,j) ) then !! tropics
               do k = kte-3,1,-1
                  if( zht(k+1) .lt. ght_pl(i,p100,j) .and. &
                      zht(k+1) .gt. ght_pl(i,p_400,j) ) then  !! NOTE ODDITY p_400 index as vble p400 is alr            
                      if( potvort(i,k+1,j) .lt. 0.0 .and. potvort(i,k,j) .lt. 0.0 ) then
                        if( abs(potvort(i,k+1,j)) .gt. vcrit20 .and. abs(potvort(i,k,j)) .le. vcrit20 ) then
                           faczk    = (vcrit20-abs(potvort(i,k,j)))/( abs(potvort(i,k+1,j))-abs(potvort(i,k,j)) )
                           zhtlw = zht(k)
                           dzht = zht(k+1) - zhtlw
                           zpvor20( i,j ) = zhtlw + faczk*dzht
                           td = t(i,k,j)+t0
                           dthz = t(i,k+1,j)+t0 - td
                           tpvor20( i,j ) = td + faczk*dthz            
                           exit
                        endif
                      endif
                  endif 
               enddo            
            endif
          
            zpvor15( i,j ) = ght_pl(i,p150,j)          !! Initial guess 
            tpvor15( i,j ) = thta_pl(i,p150,j)            
            do k = kte-3,1,-1                          !! kte-3: potvort(kte-1) === potvort(kte-2)          
               if( zht(k+1) .ge. ght_pl(i,p150,j) ) then 
                   CYCLE
               else if( zht(k+1) .lt. ght_pl(i,p150,j) .and. &
                        zht(k+1) .gt. ght_pl(i,p850,j) ) then  !! NOTE ODDITY p_400 index as vble p400 is already declared in Registry
                  if( potvort(i,k+1,j) .gt. vcrit15 .and. potvort(i,k,j) .le. vcrit15 ) then
                     faczk    = (vcrit15-potvort(i,k,j))/( potvort(i,k+1,j)-potvort(i,k,j) )
                     zhtlw = zht(k)
                     dzht = zht(k+1) - zhtlw
                     zpvor15( i,j ) = zhtlw + faczk*dzht
                     td = t(i,k,j)+t0
                     dthz = t(i,k+1,j)+t0 - td
                     tpvor15( i,j ) = td + faczk*dthz
                     kvl = k                     
                     if( zht(kvl) .gt. ght_pl(i,p250,j) ) then !! Search for a secondary, lower surface
                        do kv = kvl-1,1,-1
                           if( zht(kv+1) .gt. ght_pl(i,p550,j) ) then
                              if( potvort(i,kv+1,j) .gt. vcrit15 .and. potvort(i,kv,j) .le. vcrit15 ) then
                                 faczk    = (vcrit15-potvort(i,kv,j))/( potvort(i,kv+1,j)-potvort(i,kv,j) )
                                 zhtlw = zht(kv)
                                 dzht = zht(kv+1) - zhtlw
                                 zpvor15( i,j ) = zhtlw + faczk*dzht
                                 td = t(i,kv,j)+t0
                                 dthz = t(i,kv+1,j)+t0 - td
                                 tpvor15( i,j ) = td + faczk*dthz
                                 exit 
                              endif
                           else
                              exit
                           endif
                        enddo
                     endif          
                     exit                                                 
                  else
                     CYCLE   
                  endif
               else 
                  exit
               endif
            enddo         
            zpvor15( i,j ) = max( zpvor15( i,j),ght_pl(i,p650,j))
            tpvor15( i,j ) = max( tpvor15( i,j),  t_pl(i,p650,j))                   

            if( zpvor15( i,j ) .lt. ght_pl(i,p_400,j) ) then !! tropics
               do k = kte-3,1,-1
                  if( zht(k+1) .lt. ght_pl(i,p100,j) .and. &
                      zht(k+1) .gt. ght_pl(i,p_400,j) ) then  !! NOTE ODDITY p_400 index as vble p400 is alr            
                      if( potvort(i,k+1,j) .lt. 0.0 .and. potvort(i,k,j) .lt. 0.0 ) then
                        if( abs(potvort(i,k+1,j)) .gt. vcrit15 .and. abs(potvort(i,k,j)) .le. vcrit15 ) then
                           faczk    = (vcrit15-abs(potvort(i,k,j)))/( abs(potvort(i,k+1,j))-abs(potvort(i,k,j)) )
                           zhtlw = zht(k)
                           dzht = zht(k+1) - zhtlw
                           zpvor15( i,j ) = zhtlw + faczk*dzht
                           td = t(i,k,j)+t0
                           dthz = t(i,k+1,j)+t0 - td
                           tpvor15( i,j ) = td + faczk*dthz            
                           exit
                        endif
                      endif
                  endif 
               enddo            
            endif
            if( zpvor15( i,j ) .gt. zpvor20( i,j) ) then
               zpvor15( i,j ) = zpvor20( i,j)
               tpvor15( i,j ) = tpvor20( i,j)
            endif         
         endif        !! End if firsttime for CAWO
         
         !! CAWO End PV surfaces

         z_at_w(kte) = (zp(i,kte,j)+zb(i,kte,j))/g

         z_e_max(i,j) = maxk_ze
         rfl_maxagl(i,j) = MAX( 10. * log10(z_e_max(i,j)), -35.0 ) !! convert to dbz
         rfl_maxagl(i,j) = MAX( rfl_maxagl(i,j), 0.0 )             !! floor at 0.0 for display
         
         !! "Clamping" approach
         z_e_mxc(i,j) = mxck_ze
         rfl_mxcagl(i,j) = MAX( 10. * log10(z_e_mxc(i,j)), -35.0 ) !! convert to dbz
         rfl_mxcagl(i,j) = MAX( rfl_mxcagl(i,j), 0.0 )             !! floor at 0.0 for display        

         !! if( .not. firstime ) then                                       
         !!    if( rfl_maxagl(i,j) .lt. -34.0 ) then                        
         !!       print*,'i,j,rfl_maxagl(i,j)=',i,j,rfl_maxagl(i,j)         
         !!       print*,'i,j,maxk_ze,z_e_max(i,j)=',maxk_ze,z_e_max(i,j)   
         !!       print*,'log10(z_e_max(i,j))=',log10(z_e_max(i,j))         
         !!       print*,'10.*log10(z_e_max(i,j))=',10.*log10(z_e_max(i,j)) 
         !!       rfl_maxagl(i,j) = -40.000                                 
         !!       ze_scheme = 10.0**(0.1*rfl_maxagl(i,j))                   
         !!       print*,'ze_scheme=',ze_scheme                             
         !!      CALL wrf_error_fatal ( 'debug' )                           
         !!   endif                                                         
         !! endif                                                           

         if( .not. kzfnd ) then
             WRITE( MESG, '( A, 2I8 )' ) '1000-meter height AGL not trapped at i,j=',i,j
             CALL wrf_error_fatal ( TRIM(MESG) )
         endif

         z_e_1km(i,j) = z_e(kdd)*facd + z_e(kuu)*facu
         rfl_1kmagl(i,j) = MAX( 10. * log10(z_e_1km(i,j)), -35.0 ) !! convert to dbz         

         !! Add microphysics package specific explicit internal reflectivities

         if( do_radar_ref .eq. 1 ) then 
            rflmpscmax(i,j) = maxk_mpscdbz
            rflmpsc1km(i,j) = refl_mpskd*facd + refl_mpsku*facu
         endif   
         
         !! Calculate echo-tops for both generic and microphysics package specific reflectivities
         !! Generic Scheme first

         IF( rfl_maxagl(i,j) .gt. echo_top_maxdbz ) THEN
             kecho = 0
             do k = kte-1,1,-1
                 refl_genk = MAX( 10. * log10(z_e(k)), -35.0 ) !! convert to dbz
                 if( refl_genk .gt. echo_top_maxdbz ) then
                     kecho = k
                     if( k .eq. kte-1 ) then
                        echo_htkm = zht(k)
                        exit
                     else
                        refl_genk1 = MAX( 10. * log10(z_e(k+1)), -35.0 ) !! convert to dbz
                        facup = (refl_genk - echo_top_maxdbz)/(refl_genk-refl_genk1)
                        facdn = 1.0-facup
                        echo_htkm = facdn*zht(k) + facup*zht(k+1)
                        if( echo_htkm .le. terht(i,j) ) then
                            WRITE( MESG, '( A, 2I8 )' ) 'Generic Echo top at or below terrain height at i,j=',i,j
                            CALL wrf_error_fatal ( TRIM(MESG) )
                        endif
                        exit
                     endif
                 endif                
             enddo
             if( kecho .lt. 1 ) then
                WRITE( MESG, '( A, 2I8 )' ) 'Could not find MP scheme echo top at i,j=',i,j
                CALL wrf_error_fatal ( TRIM(MESG) )             
             endif             
             etp_18dbz(i,j) = echo_htkm * ft_per_meter
         ENDIF

         !! Now do package specific scheme

         IF( maxk_mpscdbz .gt. echo_top_maxdbz ) THEN
             kecho = 0
             do k = kte-1,1,-1
                 if( refl_10cm(i,k,j) .gt. echo_top_maxdbz ) then
                     kecho = k
                     if( k .eq. kte-1 ) then
                        echo_htkm = zht(k)
                        exit
                     else
                        facup = (refl_10cm(i,k,j) - echo_top_maxdbz)/(refl_10cm(i,k,j)-refl_10cm(i,k+1,j))
                        facdn = 1.0-facup
                        echo_htkm = facdn*zht(k) + facup*zht(k+1)
                        if( echo_htkm .le. terht(i,j) ) then
                            WRITE( MESG, '( A, 2I8 )' ) 'MP Echo top at or below terrain height at i,j=',i,j
                            CALL wrf_error_fatal ( TRIM(MESG) )
                        endif
                        exit
                     endif
                 endif                
             enddo
             if( kecho .lt. 1 ) then
                WRITE( MESG, '( A, 2I8 )' ) 'Could not find MP scheme echo top at i,j=',i,j
                CALL wrf_error_fatal ( TRIM(MESG) )             
             endif             
             etp_mpsc(i,j) = echo_htkm * ft_per_meter
         ENDIF

         !! End reflectivity calculation

         !! Gust: insert wind-speed max in PBL here for convective situations.

         !!hpbl = zht( kpbl(i,j) ) - terht(i,j)  !! height of PBL ..better to use pblh from scheme...
         !!if( ldbg ) then
            !! print*,'i,j,hpbl,pblh=',i,j,hpbl,pblh(i,j)
         !!endif

         !!pblk = 1
         hpbl = min( pblh(i,j), 5000.0 )
         !!do k = 1,kte-1
         !!   if( hpbl .le. (z_at_w(k) - terht(i,j)) ) then
         !!      pblk = k
         !!      exit
         !!   endif         
         !!enddo 
         
         !!wspd_pbl = 0.0
         gspd10(i,j) = 0.0
         !!if( ldbg ) then
         !!   print*,'Vertical diagnostics for PBL gust:'
         !!   print*
         !!endif
         !!do k = 1, pblk
         !!   wspd_pbl = 0.5 * SQRT ( ( u(i,k,j)+u(i+1,k,j) )**2 + ( v(i,k,j)+v(i,k,j+1) )**2 )
         !!   gspd10(i,j) = MAX(gspd10(i,j),wspd_pbl)
         !!   if( ldbg) then
         !!       print*,'k,wspd_pbl=',k,wspd_pbl
         !!   endif
         !!enddo
         !! print*,'pblk,w10,g10:',pblk,wspd10(i,j),gspd10(i,j)
         
         !! gust_delt =  MAX( 0.0, gspd10(i,j)-wspd10(i,j) )
         
         !! if( ldbg ) then
         !!    print*,'LDBG: pblh=',pblh(i,j)
         !!    print*,'Bulk-Richardson number: ',br(i,j)         
         !! endif   
               
         !! Calculate unstable fit function:
         GFU = 2.7545
         if( wspd10(i,j) .ge. 2.0 ) then
             GFU = 0.924 + 3.661/wspd10(i,j)   !! Asymptoptic to 1.225 for wspd10>=2.0 m/s
             GFU = max( GFU, 1.225 )
         endif
         !! else if( wspd10(i,j) .gt. 0.01 ) then  !! piecewise-linear fit between near 0 velocity and 2.0 meters/second
         !!     !! g_slop = ( 0.924 + (3.661/2.0) - 1.01 ) / 2.0
         !!     g_slop = 0.87225
         !!     if (ldbg) then
         !!        print*,'g_slop unstable=',g_slop
         !!     endif             
         !!     GFU = wspd10(i,j) * g_slop + 1.01
         !! endif
            
         !! Calculate stable fit function:
         GFS = 2.987                           !! For wspd10 < 1.0, use: 0.979 + 2.008 fixed factor as if wspd10 == 1.0
         if(  wspd10(i,j) .ge. 1.0 ) then
             GFS = 0.979 + 2.008/wspd10(i,j)   !! Asymptoptic to 1.175 for wspd10>=1.0 m/s
             GFS = max( GFS, 1.175 )
         endif    
         !! else if( wspd10(i,j) .gt. 0.01 ) then !! piecewise-linear fit between near 0 velocity and 1.0 meters/second
         !!    !! g_slop = ( 0.979 + 2.008/1.0 - 1.01 ) / 1.0
         !!    g_slop = 1.977
         !!    if (ldbg) then
         !!       print*,'g_slop stable=',g_slop
         !!    endif 
         !!    GFS = wspd10(i,j) * g_slop + 1.01
         !! endif
            
         if( br(i,j) .lt. 0.3 .and. br(i,j) .gt. -0.1 ) then !! marginal
             rich_fac = (0.3 - br(i,j)) / 0.4
             gustfac = rich_fac * GFU + (1.0-rich_fac) * GFS
         else if( br(i,j) .ge. 0.3 ) then !! stable
             gustfac = GFS
         else if( br(i,j) .le. -0.1 ) then !! unstable
             gustfac = GFU                    
         endif
         
         if( gustfac .lt. 1.01 ) then
            WRITE( MESG, '( A, 2I8, F13.6 )' ) 'GUSTfac TOO small at i,j,gustfac =',i,j,gustfac
            CALL wrf_error_fatal ( MESG ) 
         endif 
         
         gust_delt = wspd10(i,j)*(gustfac-1.0)
         gspd10(i,j) = wspd10(i,j)*gustfac          
         
         !! if( ldbg ) then            
           !!  print*,'After Fit function,gspd10,wspd10,gust_delt=',gspd10(i,j),wspd10(i,j),gust_delt
           !!  print*,'hpbl,gustfac=',hpbl,gustfac
           !!  print*,'br=',br(i,j)
           !!  print*,'GFS,GFU,rich_fac=',GFS,GFU,rich_fac
         !! end if         
         
         !! wfac = 0.0
         !! if( wspd10(i,j) .gt. 10.0 ) then
         !!    wfac = (wspd10(i,j) - 10.0)/10.0
         !!    wfac = min( wfac, 1.0 )
         !! endif
         
         !! if( hpbl .le. 400.0 ) then
         !!    gfac = 0.4
         !!    glim400 = wspd10(i,j) + wfac*gfac*gust_delt
         !!    hfac = 0.0
         !! else if( hpbl .ge. 1000. ) then
         !!    gfac = 0.1
         !!    glim1000 =  MAX( 0.7*gspd10(i,j), (wspd10(i,j)+wfac*gfac*gust_delt) )
         !!    hfac = 1.0
         !! else
         !!    gfac = (1000.-hpbl)/(1000.-400.)
         !!    gfac = 0.1 + gfac*0.3
         !!    hfac = (hpbl-400.)/(1000.-400.)
         !!    glim400 = wspd10(i,j) + wfac*gfac*gust_delt
         !!    glim1000 =  MAX( 0.7*gspd10(i,j), (wspd10(i,j)+wfac*gfac*gust_delt) )
         !! endif
         
         !! gspd10(i,j) = hfac*glim1000 + (1.0-hfac)*glim400
         
         !! if( ldbg ) then
         !!    print*,'LDBG: after hfac, gfac calculation'
         !!    print*,'LDBG: hfac, glim1000,glim400=',hfac, glim1000,glim400
         !!    print*,'LDBG: gspd10=',gspd10(i,j)
         !! end if         

         !!        
         !! Maximum-usable CAPE (MUCAPE): Establish average properties 

         pavg = davg*prmbmx*G / ( RD*virtual(tkmax,qqmax) )
         p22 = min(prmbmx+.5*pavg,p8w(i,kts,j)*0.01)
         p11 = p22-pavg
         totthe = 0.
         totqvp = 0.
         totprs = 0.

         !if( ldbg ) then
         !   print*,'kmxte,ethmax=',kmxte,ethmax
         !   print*,'pavg,p22,p11=',pavg,p22,p11
         !endif          

         do k = 1,kte-1
            pup = p8w(i,k,j)*0.01
            pdn = p8w(i,k+1,j)*0.01            
            if( pup .le. p11 ) goto 35 
            if( pdn .ge. p22 ) goto 34
            qq  = MAX( qv(i,k,j), 1.0E-25 )
            thq = dta(k)*(1000./prmb(k))**(GAMMA_RIP*(1.+GAMMAMD*qq))
            pp1 = max(p11,pdn)
            pp2 = min(p22,pup)
            IF ( pp2 .gt. pp1 ) THEN
              deltap = pp2-pp1
              totqvp = totqvp+qq*deltap
              totthe = totthe+thq*deltap
              totprs = totprs+deltap
            END IF              
34          CONTINUE            
         end do
35       CONTINUE         
         qvppari = totqvp/totprs
         tmkpari = (totthe/totprs)*(prmbmx/1000.)**  &
                   (GAMMA_RIP*(1.+GAMMAMD*qqmax))

         !! End finding parcel-mean properties for MUCAPE;

         !! Now mixed-layer CAPE: Establish "mean" parcel properties in lowest 100mb of atmosphere         
         !! Find the layer containing the pressure surface 50mb lower than psfc:

         do k = 1,kte-1
            dprs = (p8w(i,1,j)-p8w(i,k,j))*.01
            if( dprs .ge. 50.0 ) then
               klmix = k
               qvpmxl = MAX( qv(i,k,j), 1.0E-25 )
               prbmxl = prmb(k)
               tkamxl = dta(k)
               ghtmix = z_at_w(k)
               !! dz = z_at_w(k) - terht(i,j)
               !! print*,'kl and prses=',klmix,p8w(i,k,j)*.01,prmb(k),p8w(i,k+1,j)*.01,dz
               goto 44
            endif
         end do
44       CONTINUE

         !! Mixed-layer CAPE (MLCAPE): Establish average properties 

         pavg = davgml*prbmxl*G /                        &
                ( RD*virtual(tkamxl,qvpmxl) )   
         p22 = min(prbmxl+.5*pavg,p8w(i,kts,j)*0.01)
         p11 = p22-pavg
         totthe = 0.
         totqvp = 0.
         totprs = 0.

         do k = 1,kte-1
            pup = p8w(i,k,j)*0.01
            pdn = p8w(i,k+1,j)*0.01
            if( pup   .le. p11 ) goto 75 
            if( pdn .ge. p22 ) goto 74
            qq  = MAX( qv(i,k,j), 1.0E-25 )
            thq = dta(k)*(1000./prmb(k))**(GAMMA_RIP*(1.+GAMMAMD*qq))
            pp1 = max(p11,pdn)
            pp2 = min(p22,pup)
            IF ( pp2 .gt. pp1 ) THEN
              deltap = pp2-pp1
              totqvp = totqvp+qq*deltap
              totthe = totthe+thq*deltap
              totprs = totprs+deltap
            END IF              
74          CONTINUE            
         end do
75       CONTINUE         
         qvpmix = totqvp/totprs
         tmkmix = (totthe/totprs)*(prbmxl/1000.)**  &
                   (GAMMA_RIP*(1.+GAMMAMD*qvpmxl))

         DO ktyp = 1,3   !! Loop over different types of CAPE: 1-MUCAPE, 2-SBCAPE, 3-MLCAPE

            if( ktyp .eq. 2 ) then !! surface-based CAPE
               qvppari = MAX( qv(i,1,j), 1.0E-25 )
               tmkpari = dta(1)
               prmbmx  = prmb(1)
               ghtmax  = z_at_w(1)
               kmxte   = 1
            else if( ktyp .eq. 3 ) then !! mixed-layer CAPE
               qvppari = qvpmix
               tmkpari = tmkmix
               prmbmx  = prbmxl
               ghtmax  = ghtmix
               kmxte   = klmix                           
            endif

            !! Now calculate temperature and moisture properties of the parcel at hand

            cpm  = cp*(1.+CPMD*qvppari)
            ev   = max(1.e-20,qvppari*prmbmx/(EPS+qvppari))
            tlcl = TLCLC1/(log(tmkpari**TLCLC2/ev)-TLCLC3)+TLCLC4
            ethpari = tmkpari*(1000./prmbmx)**(GAMMA_RIP*(1.+GAMMAMD*qvppari))*   &
                      exp((THTECON1/tlcl-THTECON2)*qvppari*                       &
                      (1.+THTECON3*qvppari))         
            zlcl    = ghtmax+(tmkpari-tlcl)/(G/cpm)

            !  Calculate buoyancy and relative height of lifted parcel at             
            !  all levels, and store in bottom up arrays.  Add a level at the LCL,    
            !  and at all points where buoyancy is zero.                              

            kc = 0                    ! for arrays that go bottom to top
            ilcl = 0
            IF ( ghtmax .ge. zlcl ) THEN
              !! initial parcel already saturated or supersaturated.
              ilcl = 2
              klcl = 1
            END IF         

            do k = kmxte,kte-1
33            kc=kc+1         

              IF ( z_at_w(k) .lt. zlcl ) THEN ! model level is below LCL
                qvplift = qvppari
                tmklift = tmkpari-G/cpm*(z_at_w(k)-ghtmax)
                tvenv   = virtual(dta(k),qv(i,k,j))
                tvlift  = virtual(tmklift,qvplift)
                ghtlift = z_at_w(k)
              ELSE IF ( z_at_w(k) .ge. zlcl .AND. ilcl .eq. 0 ) THEN
                !! This model level and previous model level straddle the LCL,
                !! so first create a new level in the bottom-up array, at the LCL.
                tmklift = tlcl
                qvplift = qvppari
                facden  = z_at_w(k)-z_at_w(k-1)
                fac1    = (zlcl-z_at_w(k-1))/facden
                fac2    = (z_at_w(k)-zlcl)/facden
                tmkenv  = dta(k-1)*fac2+dta(k)*fac1
                qvpenv  = qv(i,k-1,j)*fac2+qv(i,k,j)*fac1
                tvenv   = virtual(tmkenv,qvpenv)
                tvlift  = virtual(tmklift,qvplift)
                ghtlift = zlcl
                ilcl    = 1
              ELSE         
                tmklift = tonpsadiabat(ethpari,prmb(k))
                if( tmklift .lt. -9.0 ) then          !! -9.9999 is set inside the function as "out of table lookup bounds"
                   WRITE (MESG ,'(A,3I8,2X,2F13.6)' ) 'Outside of lookup table bounds at i,j,k; prs,thte=',i,j,k,ethpari,prmb(k)
                   CALL wrf_error_fatal ( TRIM(MESG) )
                endif                                
                eslift  = EZERO*exp(ESL1*(tmklift-CTOK)/    &
                          (tmklift-ESL2))
                qvplift = EPS*eslift/(prmb(k)-eslift)
                tvenv   = virtual(dta(k),qv(i,k,j))
                tvlift  = virtual(tmklift,qvplift)
                ghtlift = z_at_w(k)
              END IF            

              buoy(kc) = g*(tvlift-tvenv)/tvenv  ! buoyancy
              zrel(kc) = ghtlift-ghtmax

              if ( kc.gt.1 ) then
                if ( buoy(kc)*buoy(kc-1).lt.0.0 ) then
                  !! parcel ascent curve crosses sounding curve, so create a new level
                  !! in the bottom-up array at the crossing.
                  kc = kc+1
                  buoy(kc)   = buoy(kc-1)
                  zrel(kc)   = zrel(kc-1)
                  buoy(kc-1) = 0.
                  zrel(kc-1) = zrel(kc-2) + buoy(kc-2)/(buoy(kc-2)-buoy(kc))*  &
                               (zrel(kc)-zrel(kc-2))
                end if
              end if

              if (ilcl == 1) then
                klcl = kc
                ilcl = 2
                goto 33
              end if         
            END DO         !! END DO k = kmxte,kte-1               

            kmax = kc
            IF (kmax .gt. 150) THEN
              WRITE( MESG, '( A, I8 )' ) 'in calc_cape: kmax got too big. kmax=',kmax
              CALL wrf_error_fatal ( TRIM(MESG) )
            ENDIF

            !  Get the accumulated buoyant energy from the parcel's starting
            !  point, at all levels up to the top level.

            benaccum(1) = 0.0
            benamin     = 9.0e+9
            DO k = 2,kmax
              dz          = zrel(k)-zrel(k-1)
              benaccum(k) = benaccum(k-1)+.5*dz*(buoy(k-1)+buoy(k))
                IF ( benaccum(k) .lt. benamin ) THEN
                  benamin = benaccum(k)
                END IF
            END DO

            !  Determine equilibrium level (EL), which we define as the highest      
            !  level of non-negative buoyancy above the LCL. Note, this may be       
            !  the top level if the parcel is still buoyant there.                   

            DO k = kmax,klcl,-1
              IF ( buoy(k) .ge. 0. ) THEN
                kel = k   ! k of equilibrium level
                GOTO 50
              END IF
            END DO

            !  If we got through that loop, then there is no non-negative            
            !  buoyancy above the LCL in the sounding.  In these situations,         
            !  both CAPE and CIN will be set to -0.1 J/kg.  Also, where CAPE is      
            !  non-zero, CAPE and CIN will be set to a minimum of +0.1 J/kg, so      
            !  that the zero contour in either the CIN or CAPE fields will           
            !  circumscribe regions of non-zero CAPE. [Negative sign applied below].                                

            if( ktyp .eq. 1 ) then
               mucap(i,j) = 0.1
               mucin(i,j) = 0.1
            else if( ktyp .eq. 2 ) then
               sbcap(i,j) = 0.1
               sbcin(i,j) = 0.1
            else if( ktyp .eq. 3 ) then
               mlcap(i,j) = 0.1
               mlcin(i,j) = 0.1            
            endif   

            klfc = kmax         

            GOTO 102
50          CONTINUE         

            !  If there is an equilibrium level, then CAPE is positive.  We'll         
            !  define the level of free convection (LFC) as the point below the        
            !  EL, but at or above the LCL, where accumulated buoyant energy is a      
            !  minimum.  The net positive area (accumulated buoyant energy) from       
            !  the LFC up to the EL will be defined as the CAPE, and the net           
            !  negative area (negative of accumulated buoyant energy) from the         
            !  parcel starting point to the LFC will be defined as the convective      
            !  inhibition (CIN).                                                       

            !  First get the LFC according to the above definition.                    

            benamin = 9.0e+9
            klfc = kmax
            DO k = klcl,kel
              IF ( benaccum(k) .lt. benamin ) THEN
                benamin = benaccum(k)
                klfc = k
              END IF
            END DO

            !  Now we can assign values to cape and cin

            if( ktyp .eq. 1 ) then
               mucap(i,j) = max(benaccum(kel)-benamin,0.1)
               mucin(i,j) = max(-benamin,0.1)
            else if( ktyp .eq. 2 ) then
               sbcap(i,j) = max(benaccum(kel)-benamin,0.1)
               sbcin(i,j) = max(-benamin,0.1)
            else if( ktyp .eq. 3 ) then
               mlcap(i,j) = max(benaccum(kel)-benamin,0.1)
               mlcin(i,j) = max(-benamin,0.1) 
            endif

            !  CIN is uninteresting when CAPE is small (< 100 J/kg), so set      
            !  CIN to -.1 in that case. [Negative sign applied below].                                         

            if( ktyp .eq. 1 ) then
               IF ( mucap(i,j) .lt. 100. ) mucin(i,j) = 0.1
            endif
            if( ktyp .eq. 2 ) then
               IF ( sbcap(i,j) .lt. 100. ) sbcin(i,j) = 0.1
            endif            
            if( ktyp .eq. 3 ) then
               IF ( mlcap(i,j) .lt. 100. ) mlcin(i,j) = 0.1
            endif            

102         CONTINUE  !! End of MUCAPE calculation

         END DO    !! End do loop over different types of CAPE                

         !! Storm-relative Helicity: 1km and 3km versions

         do k = 1,kte-1
            ucrsk( k ) = 0.5 * ( u(i,k,j)+u(i+1,k,j) )
            vcrsk( k ) = 0.5 * ( v(i,k,j)+v(i,k,j+1) )            
         end do                 

         !! ucrs10 = 0.5 * ( u10(i,j)+u10(i+1,j) ) !! Not staggered !! 
         !! vcrs10 = 0.5 * ( v10(i,j)+v10(i,j+1) ) !! ave. needs halo !!        
         ucrs10 = u10(i,j)
         vcrs10 = v10(i,j)
         
         if( i .eq. MIN(ite,ide-1) .and. j .eq. MIN(jte,jde-1) ) then    
            WRITE( MESG, '( A,2I8,2F15.6 )' ) 'At ite+1,jte+1=',MIN(ite,ide-1)+1,MIN(jte,jde-1)+1,ucrsk(1),vcrsk(1)
            !! CALL wrf_message(MESG)   
         endif
         
         CALL relhl( ldbg, ucrsk,vcrsk,ucrs10,vcrs10,terht(i,j),1000.0,z_at_w,dz8w,   &
                     zht, srh1,   &
                     kds,kde,    &
                     kms,kme,    &
                     kts,kte       )

         srh01(i,j) = srh1
         ehi01(i,j) = (mucap(i,j) * srh1)/160000.

         CALL relhl( ldbg, ucrsk,vcrsk,ucrs10,vcrs10,terht(i,j),3000.0,z_at_w,dz8w,   &
                     zht, srh3,   &
                     kds,kde,    &
                     kms,kme,    &
                     kts,kte       )                     

         srh03(i,j) = srh3
         ehi03(i,j) = (mucap(i,j) * srh3)/160000.            

         !! Cloud fraction

         lkfnd = .false.
         mkfnd = .false.
         hkfnd = .false.

         if( kclo .le. 0 .or. kcmi .le. 0 .or. kchi .le. 0 ) then
            WRITE( MESG, '( A, 2I8 )' ) 'Cloud fraction: kc-index <= 0 at i,j=',i,j
            CALL wrf_error_fatal ( TRIM(MESG) )
         endif
         
         if( (kclo .eq. kcmi) .or. &
             (kcmi .eq. kchi) .or. &
             (kclo .eq. kchi) ) then
             WRITE( MESG, '( A, 2I8 )' ) 'Cloud frac indices kclo,kcmi,kchi not distinguished at i,j=',i,j
             CALL wrf_error_fatal ( TRIM(MESG) )
         endif         

         do k = 1,kte-1
           !! zht(k) = 0.5*(z_at_w(k) +z_at_w(k+1) )     !! half-layer heights following phy_prep           

           IF ( k .ge. kclo .AND. k .lt. kcmi ) then
                cfracl(i,j) = AMAX1(rhk(k),cfracl(i,j))                                
                if( .not. lkfnd .and. rhk(k) .gt. 75.0 ) then
                   lowcht(i,j) = zht(k)-terht(i,j)
                   lkfnd = .true.
                endif 
           ELSE IF ( k .ge. kcmi .AND. k .lt. kchi ) then
                cfracm(i,j) = AMAX1(rhk(k),cfracm(i,j))
                if( .not. mkfnd .and.  rhk(k) .gt. 75.0 ) then
                   midcht(i,j) = zht(k)-terht(i,j)
                   mkfnd = .true.                
                endif                          
           ELSE IF ( k .ge. kchi ) then 
                cfrach(i,j) = AMAX1(rhk(k),cfrach(i,j))
                if( .not. hkfnd .and. rhk(k) .gt. 60.0 ) then
                   higcht(i,j) = zht(k)-terht(i,j)
                   hkfnd = .true.                
                endif                
           ENDIF
         end do

         if( dx .gt. 3500.0 ) then
         
             cfracl(i,j)=4.0*cfracl(i,j)/100.-3.0            !! from RH to actual cloud fraction
             cfracm(i,j)=4.0*cfracm(i,j)/100.-3.0
             cfrach(i,j)=2.5*cfrach(i,j)/100.-1.5
         
         else
         
             !! New cloud fraction functional formulation for 03km model:

             if( cfracl(i,j) .gt. 99.0 ) then
                cfracl(i,j) = 1.0
             else
                cfracl(i,j) = 4.0*(cfracl(i,j)/100.)**2 - 2.9204
             endif

             if( cfracm(i,j) .gt. 99.0 ) then
                cfracm(i,j) = 1.0         
             else
                cfracm(i,j) = 4.0*(cfracm(i,j)/100.)**2 - 2.9204         
             endif

             if( cfrach(i,j) .gt. 99.0 ) then
                cfrach(i,j) = 1.0         
             else
                !! cfrach(i,j) = 2.5*(cfrach(i,j)/100.)**2 - 1.45025
                cfrach(i,j) = 3.44*cfrach(i,j)/100. - 2.4056          
             endif                  

             !! cfrlomax = 0.0
             !! cfrmdmax = 0.0
             !! cfrhimax = 0.0

             !! if( ldbg ) then
             !!    print*,'kclo,kcmi,kchi=',kclo,kcmi,kchi
             !! endif        

             !! do k = 1,kte-1
             !! 
             !!    cld_cond = conclc(k)+concfc(k)               !! g/m3 cloud liquid plus ice
             !!    rhfrac = 0.01*rhk(k)

                !! if( ldbg ) then
                !!    print*,'k,g/m3,kg/kg,rh=',k,cld_cond,qctot(k),rhfrac
                !! endif

                !! if( rhfrac .gt. .99 ) then
                !!  print*,'k,rh,cld_cond=',k,rhfrac,cld_cond
                !! endif

             !!   if( cld_cond .gt. 0.01 ) then
             !!      print*,'k,rh,cld_cond=',k,rhfrac,cld_cond
             !!   endif 

             !!   IF( k .eq. 1 ) THEN
             !!        !! Clouds in layer 1 -- possible fog 
             !!   
             !!        if( fogflag( i,j ) .eq. 3.0 ) then
             !!           cfrlow(k) = 1.0
             !!        else if( cld_cond .ge. 0.01 ) then
             !!           cfrlow(k) =  min( 3.23623591755*cld_cond + .83763764082, 1.0 )                
             !!        else if( rhfrac .ge. .858202128 ) then
             !!           cfrlow(k) =  exp(-9.2419624075*(1.015-rhfrac))                           
             !!        else if( rhfrac .ge. .75 ) then
             !!           cfrlow(k) = 2.16979828472*rhfrac - 1.62734871354                            
             !!        else
             !!           cfrlow(k) = 0.0            
             !!        endif
             !!        cfrlomax = cfrlow(k)

                    !! if( ldbg ) then
                    !!    print*,'k,cfrlow(k)=',k,cfrlow(k)
                    !! endif                                

             !!   ELSE IF( k .lt. kcmi ) THEN
             !!        !! Low clouds above layer 1     
             !!    
             !!        if( cld_cond .ge. 0.05 ) then
             !!           cfrlow(k) = 1.0                
             !!        else if( cld_cond .ge. 0.01 ) then
             !!           cfrlow(k) =  3.23623591755*cld_cond + .83763764082                           
             !!        else if( rhfrac .ge. .858202128 ) then
             !!           cfrlow(k) =  exp(-9.2419624075*(1.015-rhfrac))                           
             !!        else if( rhfrac .ge. .75 ) then
             !!           cfrlow(k) = 2.16979828472*rhfrac - 1.62734871354                            
             !!        else
             !!           cfrlow(k) = 0.0            
             !!        endif
             !!        cfrlomax = max(cfrlomax,cfrlow(k)) 

                     !! if( ldbg ) then
                     !!    print*,'k,cfrlow(k)=',k,cfrlow(k)
                     !! endif

             !!   ELSE IF( k .lt. kchi ) THEN
                     !! Medium clouds  

             !!        if( cld_cond .ge. 0.05 ) then
             !!           cfrmed(k) = 1.0                
             !!        else if( cld_cond .ge. 0.01 ) then
             !!           cfrmed(k) =  3.23623591755*cld_cond + .83763764082                           
             !!        else if( rhfrac .ge. .858202128 ) then
             !!           cfrmed(k) =  exp(-9.2419624075*(1.015-rhfrac))                           
             !!        else if( rhfrac .ge. .75 ) then
             !!           cfrmed(k) = 2.16979828472*rhfrac - 1.62734871354                
             !!        else
             !!           cfrmed(k) = 0.0            
             !!        endif
             !!        cfrmdmax = max(cfrmdmax,cfrmed(k))           

                    !! if( ldbg ) then
                    !!    print*,'k,cfrmed(k)=',k,cfrmed(k)
                    !! endif

             !!   ELSE IF ( k .ge. kchi ) then
                     !! High clouds

             !!        if( cld_cond .ge. 0.005 ) then
             !!           cfrhgh(k) = 1.0
             !!        else if( cld_cond .ge. 0.001 ) then
             !!           cfrhgh(k) =  24.687384328*cld_cond + .87656307835            
             !!        else if( rhfrac .ge. .7442695042 ) then
             !!           cfrhgh(k) =  exp(-6.9314718*(1.015-rhfrac))       
             !!        else if( rhfrac .ge. .60 ) then
             !!           cfrhgh(k) = 1.06159813665 *rhfrac - 0.6369588198          
             !!        else
             !!           cfrhgh(k) = 0.0            
             !!        endif
             !!        cfrhimax = max(cfrhimax,cfrhgh(k))            

                     !! if( ldbg ) then
                     !!    print*,'k,cfrhgh(k)=',k,cfrhgh(k)
                     !! endif

             !!   ENDIF         
             !! enddo

            !! cfracl(i,j) = cfrlomax
            !! cfracm(i,j) = cfrmdmax
            !! cfrach(i,j) = cfrhimax
        
            !! Done with new cloud fraction for 03km model
         
         endif
         
         cfracl(i,j)=amin1(cfracl(i,j),1.0)
         cfracl(i,j)=amax1(cfracl(i,j),0.0)
         cfracm(i,j)=amin1(cfracm(i,j),1.0)
         cfracm(i,j)=amax1(cfracm(i,j),0.0)
         cfrach(i,j)=amin1(cfrach(i,j),1.0)
         cfrach(i,j)=amax1(cfrach(i,j),0.0)
         
         !! if( ldbg ) then
         !!    print*,'i,j,cfracs=',i,j,cfrlomax,cfrmdmax,cfrhimax
         !! endif         
         
         !! Done with cloud fraction       
                  
         !! Snow, mixed, and freezing levels

         tw1(i,j) = twh( 1 )
         twm(i,j) = twh( 1 )
         tns(i,j) = dta( 1 )

         do  l = 2, nearsfc  !!  loop for nearsfc wet-bulb temperature
            twm( i,j ) = twm( i,j ) + twh( l )
            tns( i,j ) = tns( i,j ) + dta( l )
         end do          !!  end c-loop     

         twm( i,j )  = dlay * twm( i,j )              !! TWBAR
         tns( i,j )  = dlay * tns( i,j )
         lfr( i,j )  = dfrz * ( twm( i,j ) - tkice )  !! MASKFRAC

         !!  Compute snow level: 

         do   l = kte-6, 1, -1                        
            if ( tbh( l ) .gt. ctok ) then
               bot = tbh( l ) - ctok
               dt  = tbh( l ) - tbh( l+1 )
               dz  = zht(l+1) - zht(l)
               snl( i,j ) = zht(l) + dz            
               go to  91         
            end if 
         end do

         dz  = ( ctok - tbh( 1 ) ) * dzlap   !!  not found, so below zht(1)
         snl( i,j ) = zht( 1 ) - dz

91       continue        !!  exit snolev-loop

         !!  Compute mixed-phase level

         if ( tbh( 1 ) .le. ctok ) then
            dz  = ( ctok - tbh( 1 ) ) * dzlap
            mxl( i,j ) = zht( 1 ) - dz
         else
            do   l = 2, kte-6
               if ( tbh( l ) .lt. ctok ) then
                  bot = tbh( l-1 ) - ctok
                  dt  = tbh( l-1 ) - tbh( l )
                  dz  = (zht(l) - zht(l-1)) * bot / dt
                  mxl( i,j ) = zht( l-1 ) + dz
                  go to  92
               end if
            end do

            mxl( i,j ) = zht( kte-5 )    

92          continue        !!  exit mxl-loop            
         end if      

         !!  Compute freezing level

         if ( dta( 1 ) .le. ctok ) then

            dz  = ( ctok - dta( 1 ) ) * dzlap
            frl( i,j ) = zht( 1 ) - dz

         else

            do   l = 2, kte-6
               if ( dta( l ) .lt. ctok ) then
                  bot = dta( l-1 ) - ctok
                  dt  = dta( l-1 ) - dta( l )
                  dz  = (zht(l) - zht(l-1)) * bot / dt
                  frl( i,j ) = zht( l-1 ) + dz
                  go to  93
               end if
            end do

            frl( i,j ) = zht( kte-5 )         !!  not found, so above zht(kte-5)
            
93          continue        !!  exit frl-loop

         end if

         !! Additional aviation-related calculations       

         clg(i,j)=min(clg(i,j)*m_per_km,CEILMAX)  ! km to m, max of 9,000m
         
         rhmax = 0.01 * max( rhk(1), rhk(2) ) !! max rh fraction

         !  - vary RH coefficient between 75% up to max at 95%
         !    to give 5.4 km visibility at 95%.

         qrh = max(0.0,min(1.0,(rhmax-0.15)/0.80))
         visrh = 65.79 * exp(-2.5*qrh)

         ! Add term to increase RH vis term for low-level wind shear increasing from 4 to 6 ms-1
         ! following Evan Kuchera's paper as a guideline

         shear_fac = min(1.,max(0.,(spdshr(i,j)-4.)/2.) )
         if (visrh.lt.10.) visrh = visrh + (10.-visrh)*shear_fac
         
         ! -- vis(i,j) = min(32.,visrh)

         ! -- bextmx = max( bext(1), bext(2), bext(3) )
         ! -- vis(i,j) = min(20.,const1/bextmx)  ! max of 20km

         ! -- Dec 2003 - Roy Rasmussen (NCAR) expression for night
         ! -- vs. day vis. 1.609 factor is number of km in mile.

         ! -- vis_night = 1.69 * ((vis(i,j)/1.609)**0.86) * 1.609
         ! -- zen_fac   = min(0.1,max(coszen(i,j),0.))/ 0.1
         ! -- vis(i,j)  = zen_fac * vis(i,j) + (1.-zen_fac)*vis_night
         
         vis(i,j) = min(32.,const1/bext(1))  ! max of 32km const1 == -log(thcon)
         !! if( ldbg ) then
         !!    PRINT*,'Vis found at i,j,vis=',i,j,vis(i,j)
         !!    PRINT*,'Vis calc: const1, bext=',const1,bext(1)
         !! endif
         vis(i,j) = min(vis(i,j),visrh)   !! km                  
         
         !! Flight regulation category -- VFR:  1; MVFR: 2; IFR:  3; LIFR: 4
         !! For situations in which grid-scale hydrometeors including cloud
         !! water do not result in a ceiling, use fractional coverage approach
         !! following DTC. Here, the layered (low, mid, high) fractions must
         !! first exceed 50%, and then the ceiling is determined as the first
         !! model level within those layers that exceeds the threshold for 
         !! cloud existence.... i.e. lowcht
                  
         if( clg(i,j) .gt. CEILBND ) then
            if( cfracl(i,j) .gt. 0.50 .and. lowcht(i,j) .gt. 0.0 ) then
               clg(i,j) = lowcht(i,j)
            else if( cfracm(i,j) .gt. 0.50 .and. midcht(i,j) .gt. 0.0 ) then
               clg(i,j) = midcht(i,j)                             
            else if( cfrach(i,j) .gt. 0.50 .and. higcht(i,j) .gt. 0.0 ) then
               clg(i,j) = higcht(i,j)                               
            endif
         endif

         if( clg(i,j).lt.clg1 .or. vis(i,j).lt.vis1 ) then
            freg(i,j)=4.
         elseif( clg(i,j).lt.clg2 .or. vis(i,j).lt.vis2 ) then
            freg(i,j)=3.
         elseif( clg(i,j).lt.clg3 .or. vis(i,j).lt.vis3 ) then
            freg(i,j)=2.
         endif

         !!
         !! Intersperse precipitation rates/p-types. Needed *prior* to
         !! other FIP related calculations
         !!

         if( firstime .and. .not. lrestart ) then
             if( (rainc(i,j)+rainnc(i,j)) .ne. 0.0 ) THEN
               WRITE( MESG, '( A, 3I8 )' ) 'Non-zero initial total rain at i,j=',i,j               
               CALL wrf_error_fatal ( TRIM(MESG) )
             else
               rain_last(i,j)=0.0
               sn_nc_last(i,j)=0.0               
               gr_nc_last(i,j)=0.0               
               lq_nc_last(i,j)=0.0
               radcv_last(i,j)=0.0
               rascv_last(i,j)=0.0
               rannc_last(i,j)=0.0
               snowcv(i,j)    =0.0  !! accumulators for convective snow, graupel and liquid
               graupelcv(i,j) =0.0
               liquidcv(i,j)  =0.0
               sn_cv_last(i,j)=0.0               
               gr_cv_last(i,j)=0.0               
               lq_cv_last(i,j)=0.0
               cu_act_last(i,j)=0  !! integer
               cu_top_last(i,j)=0.0
               cu_ktop_lst(i,j)=0.0
               cu_bot_last(i,j)=0.0
               cu_kbot_lst(i,j)=0.0
               ishall_last(i,j)=-1  !! integer
               cu_ctt_last(i,j)=0.0
               raliqd(i,j)     =0.0 !! time-step accumulators for iptyp_fip defined p_types
               rafzra(i,j)     =0.0
               rasnow(i,j)     =0.0
               raicep(i,j)     =0.0
               rafrzn(i,j)     =0.0 !! CAWO
               rafrznfr(i,j)   =0.0 !! CAWO
               raliqdcum(i,j)  =0.0 !! full-run accumulators for iptyp_fip defined p_types
               rafzracum(i,j)  =0.0
               rasnowcum(i,j)  =0.0
               dysnowcum(i,j)  =0.0
               dysnratcu(i,j)  =0.0
               raicepcum(i,j)  =0.0
               ratotlcum(i,j)  =0.0 !! full-run accumulator for CAWO
               rafrzncum(i,j)  =0.0 !! full-run accumulator for CAWO
               rafrzncumfr(i,j)=0.0 !! derived full-run accumulator for CAWO
               
               kbot_cnvct = 0
               ktop_cnvct = 0
               ctop_cnvct = 0.0
               ctta_cnvct = 0.0 
               cbot_cnvct = 0.0
               cdpt_cnvct = 0.0
               cshl_cnvct = -1                            
             endif                          
         
         else !! either (firstime and lrestart) or (not firstime)
         
             if( lrestart .and. firstime ) then
                WRITE( MESG, '( A )' ) 'PLD HTR scheme should not be called from start_em on RESTART'
                CALL wrf_error_fatal ( TRIM(MESG) )
                
                if( cu_activ(i,j) .lt. 1.0e-10 ) then  !! A cloud is not active so the scheme is available for activation             
                   cu_act_last(i,j) = 0  !! integer
                   cu_top_last(i,j)=0.0
                   cu_ktop_lst(i,j)=0.0
                   cu_bot_last(i,j)=0.0
                   cu_kbot_lst(i,j)=0.0
                   ishall_last(i,j)=-1  !! integer
                   cu_ctt_last(i,j)=0.0
                endif
             
             else
                if( cu_act_flag(i,j) ) then
                   cu_activ(i,j) = 0.0
                else
                   cu_activ(i,j) = 1.0
                endif             
             endif

             rain_this(i,j)  = rainc(i,j)+rainnc(i,j)-rain_last(i,j)
             rain_rate(i,j)  = rain_this(i,j)*DDT_PLD
             rain_last(i,j)  =  rainc(i,j)+rainnc(i,j)             
             rannc_rate(i,j) = (rainnc(i,j)-rannc_last(i,j))*DDT_PLD
             !! rainc_rate(i,j) = (rainc(i,j)-rainc_last(i,j))*DDT_PLD !! for later implementation
             rannc_last(i,j) = rainnc(i,j)
             sn_nc_rate(i,j) = (snownc(i,j)-sn_nc_last(i,j))*DDT_PLD
             sn_nc_last(i,j) =  snownc(i,j)
             gr_nc_rate(i,j) = (graupelnc(i,j)-gr_nc_last(i,j))*DDT_PLD
             gr_nc_last(i,j) =  graupelnc(i,j)             
             lq_nc_rate(i,j) = ( (rainnc(i,j) - snownc(i,j) - graupelnc(i,j)) - lq_nc_last(i,j) )*DDT_PLD
             lq_nc_last(i,j) =   (rainnc(i,j) - snownc(i,j) - graupelnc(i,j))                                       
             raliqd(i,j)     = 0.0 !! time-step accumulators for iptyp_fip defined p_types
             rafzra(i,j)     = 0.0
             rasnow(i,j)     = 0.0
             raicep(i,j)     = 0.0
             rafrzn(i,j)     = 0.0 !! CAWO
             rafrznfr(i,j)   = 0.0 !! CAWO 
             
             prate_inst(i,j) = MAX( ( rainncv(i,j) + raincv(i,j) ) / dt_advec , 0.0 ) !! Instantaneous total precip rate (mm/s)             
             
             !! Deep and shallow parameterized convection:
             
             if( rainsh(i,j) .gt. 1.0e-20 ) then  !! will ONLY be positive if this ppt rate derives from *module_shallowcu_driver.F* !! 
                WRITE( MESG, '( A, 3I8 )' ) 'Positive rainsh shallow-convective accum rain at i,j=',i,j
                !! CALL wrf_message ( TRIM(MESG) )
             endif
             
             radcv_this      = rainc(i,j)-radcv_last(i,j)
             radcv_rate(i,j) = radcv_this*DDT_PLD
             radcv_last(i,j) = rainc(i,j)                          
             rascv_this      = rainsh(i,j)-rascv_last(i,j)
             rascv_rate(i,j) = rascv_this*DDT_PLD
             rascv_last(i,j) = rainsh(i,j)
             ratotcv_this    = radcv_this + rascv_this               

             kbot_cnvct = 0
             ktop_cnvct = 0
             ctop_cnvct = 0.0
             ctta_cnvct = 0.0 
             cbot_cnvct = 0.0
             cdpt_cnvct = 0.0
             cshl_cnvct = -1
             
             if( cu_activ(i,j) .lt. 1.0e-10 ) then !! A cloud is not active so the scheme is available for activation             
                if( NINT(cutop(i,j)) .ne. kts ) then
                   WRITE( MESG, '( A, 2I8 )' ) 'Cu not active but top above surface at i,j=',i,j
                   CALL wrf_error_fatal ( TRIM(MESG) )
                endif
                if( NINT(cubot(i,j)) .ne. kte ) then
                   WRITE( MESG, '( A, 2I8 )' ) 'Cu not active but bot below model top at i,j=',i,j
                   CALL wrf_error_fatal ( TRIM(MESG) )
                endif                
                if( ishall(i,j) .ne. -1 ) then
                   WRITE( MESG, '( A, 2I8 )' ) 'Cu not active but ishall claims to be active at i,j=',i,j
                   CALL wrf_error_fatal ( TRIM(MESG) )                
                endif                
                if( cu_act_last(i,j) .eq. 1 ) then !! A cloud expired between last call and this call to PLD
                   ctop_cnvct = cu_top_last(i,j)
                   ctta_cnvct = cu_ctt_last(i,j)
                   ktop_cnvct = cu_ktop_lst(i,j)
                   cbot_cnvct = cu_bot_last(i,j)
                   kbot_cnvct = cu_kbot_lst(i,j)
                   cshl_cnvct = ishall_last(i,j)
                   !! if( ldbg ) then
                   !!    print*,'cshl_cnvct:::(1)',i,j,cshl_cnvct,ishall_last(i,j),ktop_cnvct
                   !!    print*,'cu_activ=',cu_activ(i,j)
                   !! endif
                endif    
             else
                if( NINT(cutop(i,j)) .le. kts ) then
                   WRITE( MESG, '( A, 2I8 )' ) 'Cu active but top NOT above surface at i,j=',i,j
                   CALL wrf_error_fatal ( TRIM(MESG) )
                endif
                if( ishall(i,j) .ne. 0 .and. ishall(i,j) .ne. 1 ) then
                   WRITE( MESG, '( A, 2I8 )' ) 'Cu active but ishall has bad value at i,j=',i,j
                   CALL wrf_error_fatal ( TRIM(MESG) )                
                endif                
                cshl_cnvct = ishall(i,j)
                !! if( ldbg ) then
                !!    print*,'cshl_cnvct:::(2)',i,j,cshl_cnvct,ishall_last(i,j)
                !!    print*,'cu_activ=',cu_activ(i,j)
                !!    print*,'ishall=',ishall(i,j)
                !! endif                
             endif
             cu_act_last(i,j)=NINT(cu_activ(i,j))

             if( cshl_cnvct .eq. -1 .and. ( ishall(i,j) .ge. 0 .or. ishall_last(i,j) .ge. 0) ) then
                  WRITE( MESG, '( A, 2I8 )' ) 'Unexpected cshl_cnvct(1) at i,j=',i,j
                  CALL wrf_error_fatal ( TRIM(MESG) )             
             else if( cshl_cnvct .ge. 0 .and. ishall(i,j) .lt. 0 .and. ishall_last(i,j) .lt. 0 ) then
                  !! print*,'ishall,ishall_last=',ishall(i,j),ishall_last(i,j)
                  WRITE( MESG, '( A, 2I8 )' ) 'Unexpected cshl_cnvct(2) at i,j=',i,j
                  CALL wrf_error_fatal ( TRIM(MESG) )             
             endif             
             ishall_last(i,j)=ishall(i,j)
             
             if(cu_physics .eq. 1 .and. cudt .gt. cudt_min ) then                   !! We are running KFETA with a positive CUDT
                 if( NINT(cu_activ(i,j)) .eq. 1 .and. .NOT. cu_exp_flag(i,j) ) THEN !! Thus, re-set IF and only IF cu_activ is TRUE and 
                                                                                    !! cu_expired_flg is FALSE: this indicates
                     cu_act_last(i,j)=NINT(cu_activ(i,j))                           !! a cloud is bridging this PLD output tstep
                     ishall_last(i,j)=ishall(i,j)
                     !! if( ldbg ) then
                     !!  print*,'bridging, cu_activ=',i,j,NINT(cu_activ(i,j))
                     !! endif                 
                 else
                     cu_act_last(i,j) = 0  !! integer
                     ishall_last(i,j) = -1                     
                 endif
             else
                 cu_act_last(i,j) = 0
                 ishall_last(i,j) = -1
             endif

             if( NINT(cutop(i,j)) .lt. kts ) then
                WRITE( MESG, '( A, 2I8 )' ) 'Unexpected cutop at i,j=',i,j,NINT(cutop(i,j))
                CALL wrf_error_fatal ( TRIM(MESG) )
             else if( NINT(cutop(i,j)) .eq. kts ) then
                cu_top_m(i,j) = 0.0
             else
                ktop_cnvct = NINT(cutop(i,j))
                cu_top_m(i,j) = zht( ktop_cnvct ) - terht(i,j)  !! height of convective cloud top (M)
                ctop_cnvct = cu_top_m(i,j)
                ctta_cnvct = cu_top_ta(i,j)   !! ctta_cnvct not currently used; is retrieved from within KF at this time
                !! if( ldbg .and. cu_activ(i,j) .lt. 1.0e-10 ) then
                !!   print*,'ktop_cnvct:::(1)',i,j,ktop_cnvct,NINT(cutop(i,j))
                !! endif  
             endif
             cu_top_last(i,j) = cu_top_m(i,j)
             cu_ktop_lst(i,j) = cutop(i,j)
             cu_ctt_last(i,j) = cu_top_ta(i,j)
             
             if( NINT(cubot(i,j)) .gt. kte ) then
                WRITE( MESG, '( A, 2I8 )' ) 'Unexpected cutop at i,j=',i,j,NINT(cutop(i,j))
                CALL wrf_error_fatal ( TRIM(MESG) )
             else if( NINT(cubot(i,j)) .eq. kte ) then
                cu_bot_m(i,j) = 0.0
             else
                kbot_cnvct = NINT(cubot(i,j))                
                cu_bot_m(i,j) = zht( kbot_cnvct ) - terht(i,j)  !! height of convective cloud base (M)
                cbot_cnvct = cu_bot_m(i,j)
             endif
             cu_bot_last(i,j) = cu_bot_m(i,j)
             cu_kbot_lst(i,j) = cubot(i,j)
             
             !! If ctop_cnvct>0.0 then a convective cloud is active now, or there was one that expired between
             !! the last call and this call to PLD. Use ctop_cnvct further below in FIP determination. 
             
             if( ctop_cnvct .gt. 0.0 ) then
                cdpt_cnvct = ctop_cnvct-cbot_cnvct
             endif
             
             !! partition convective precipitation into snow, "graupel", and liquid. Liquid
             !! may include freezing rain as well as plain old rain. Graupel may include "mixed"
             !! and ice-pellets defined using traditional DTO/BAMS approach
             
             if( (snl(i,j)-terht(i,j)) .le. 0.0 ) then !!
               snowcv(i,j)     = snowcv(i,j) + ratotcv_this
               sn_cv_rate(i,j) = radcv_rate(i,j) + rascv_rate(i,j)
               sn_cv_last(i,j) = snowcv(i,j)
               gr_cv_rate(i,j) = 0.0
               lq_cv_rate(i,j) = 0.0
             else if( (mxl(i,j)-terht(i,j)) .le. 0.0 ) then !!
               graupelcv(i,j)  = graupelcv(i,j) + ratotcv_this
               gr_cv_rate(i,j) = radcv_rate(i,j) + rascv_rate(i,j)
               gr_cv_last(i,j) = graupelcv(i,j)
               sn_cv_rate(i,j) = 0.0
               lq_cv_rate(i,j) = 0.0            
             else
               liquidcv(i,j)   = liquidcv(i,j) + ratotcv_this
               lq_cv_rate(i,j) = radcv_rate(i,j) + rascv_rate(i,j)
               lq_cv_last(i,j) = liquidcv(i,j)
               sn_cv_rate(i,j) = 0.0
               gr_cv_rate(i,j) = 0.0           
             endif                     
         endif     !! End if firstime or not for precipitation rates/p-type rates    
         
         !! Forecast Icing Potential (FIP); Super-cooled liquid (SCL) and super-cooled large droplet (SLD)
         !! calculations
         
         kcibas( : ) = 0
         kcitop( : ) = 0
         cbot_tc( : ) = DIAG_MISSING
         ctop_tc( : ) = DIAG_MISSING
         cbot_ht( : ) = DIAG_MISSING
         ctop_ht( : ) = DIAG_MISSING
         icg_pot1d( : ) = 0.0
         sld_pot1d( : ) = 0.0
         kcitop_rh = CTOP_RH
         kcibot_rh = CBOT_RH
         kcilay_rh = CLAY_RH
         clay_iter = 0
         ncilays = 0
         ifip_situ = 0
         iso_deepr = .FALSE.   !! is an isolated deep param. conv. cld precipitating
         iso_shalr = .FALSE.   !! is an isolated shallow conv. cld precipitating
         mlt_deepr = .FALSE.   !! when mixed with layer clouds, is deep param convection precipitating?
         ires_typ = 0          !! 0,1,2 == no resolved cld; non-ppt resolved cld; ppt-resolved cld
                           
         kcitop_hi = -1
         kcibas_lo = -1
         kconv     = 0         !! cloud-layer "k" index when resolved-scale convection identified
         w0absmax  = 0.0
         cdepk     = 0.0
         lresdcon  = .false.
         lresdtyp  = 0
         kbot_cnvcr = 0
         ktop_cnvcr = 0 
         kbot = 0
         ktop = 0
         lresdshal = .false.  !! will there be RESOLVED deep convection plus parameterized shallow?
         
         lbranch222 = .false.
         lbranch225 = .false. 
         
         if( rannc_rate(i,j) .ge. 0.5*MM_PER_100HRS ) then  !! lower limit at which we consider resolved cld
                                                            !! to be precipitating heavily enough to "care" about        
             ires_typ = 2
             
191          continue
             clay_iter = clay_iter + 1
             
             if( clay_iter .gt. 5 ) then
                WRITE( MESG, '( A, 2I8 )' ) 'Too many iterations for res-cld layer detection at i,j=',i,j
                !! CALL wrf_message (TRIM(mesg))         
             else
                iter_max = MAX( iter_max, clay_iter )
             endif             
             
             do k=k250,kclg1,-1
               if( qctot(k) .ge. 1.0e-10 .or. rhk(k) .ge. kcitop_rh ) then
                  kcitop_hi = k
                  EXIT
               endif
             enddo

             if( kcitop_hi .lt. 0.0 ) then
                 kcitop_rh = kcitop_rh - 5.0
                 kcilay_rh = kcilay_rh - 5.0
                 kcibot_rh = kcibot_rh - 5.0
                 goto 191              
             endif
                          
             if( kcitop_hi .gt. 0 ) then
                do k=kclg1,kcitop_hi
                   if( qcond(k) .ge. 1.0e-10 .or. rhk(k) .ge. kcibot_rh ) then
                      kcibas_lo = k
                      EXIT
                   endif
                enddo
             endif
         
             if( kcibas_lo .lt. 0.0 ) then
                 kcitop_rh = kcitop_rh - 5.0
                 kcilay_rh = kcilay_rh - 5.0
                 kcibot_rh = kcibot_rh - 5.0
                 goto 191             
             endif                 
         
         else   !! too little resolved precipitation for the scheme to care; use total condensate
                !! to detect cloud top and base (if any)

             do k=k250,kclg1,-1
               if( qctot(k) .ge. 1.0e-10 ) then
                  kcitop_hi = k
                  EXIT
               endif
             enddo
        
             if( kcitop_hi .gt. 0.0 ) then  !! found resolved cloud             
                do k=kclg1,kcitop_hi
                   if( qcond(k) .ge. 1.0e-10 ) then
                      kcibas_lo = k
                      EXIT
                   endif
                enddo
                if( kcibas_lo .lt. 0.0 ) then !! error by construction             
                    WRITE( MESG, '( A, 2I8 )' ) 'No none-ppt resolved cloud base found at i,j=',i,j
                    CALL wrf_error_fatal ( TRIM(MESG) )             
                endif             
                ires_typ = 1
             endif        
         endif

         if( kcitop_hi .eq. -1 .and. ires_typ .ne. 0 ) then
              WRITE( MESG, '( A, 2I8 )' ) ' DEBUG kcitop_hi at i,j=',i,j
              CALL wrf_error_fatal ( TRIM(MESG) )         
         endif         
                           
         if( ires_typ .gt. 0 ) then  !! A resolved cloud was just identified, determine layer structure                  
            
             kdry = .FALSE.
             kwet = .TRUE.
             kcibas( 1 ) = kcibas_lo
             cbot_tc( 1 ) = dtc(kcibas_lo)
             cbot_ht( 1 ) = zht(kcibas_lo) - terht(i,j)  !! height of base of lowest cloud layer       
             ncilays = 1

             do k = kcibas_lo,kcitop_hi            
                if( kwet ) then
                   if( rhk(k) .lt. kcitop_rh .and. qcond(k) .lt. 1.0e-12 ) then
                      kdry = .TRUE.
                      kwet = .FALSE.
                      kcitop( ncilays ) = k-1
                      ctop_tc(ncilays ) = dtc(k-1)
                      ctop_ht(ncilays ) = zht(k-1) - terht(i,j) 
                      CYCLE
                  endif
               endif
               if( kdry ) then
                  if( rhk(k) .ge. kcilay_rh .or. qcond(k) .ge. 1.0e-10 ) then
                      kwet = .TRUE.
                      kdry = .FALSE.
                      ncilays = ncilays + 1
                      kcibas( ncilays ) = k
                      cbot_tc(ncilays ) = dtc(k)
                      cbot_ht(ncilays ) = zht(k) - terht(i,j)
                   endif
                endif
                if( k .eq. kcitop_hi ) then
                   kcitop( ncilays ) = kcitop_hi
                   ctop_tc(ncilays ) = dtc(kcitop_hi)
                   ctop_ht(ncilays ) = zht(kcitop_hi) - terht(i,j) 
                endif
             enddo

             if( ncilays .eq. 1 ) then
                if( kcibas( ncilays ) .ne. kcibas_lo .or. kcitop( ncilays ) .ne. kcitop_hi ) then
                   CALL wrf_error_fatal ( 'DEBUG ncilays condition 1' )                         
                endif
             endif

             if( kcitop( ncilays ) .ne. kcitop_hi ) then
                 WRITE (MESG , '(A,3I8)' ) 'ncil,kcit,kcith=',ncilays,kcitop(ncilays),kcitop_hi                
                 CALL wrf_error_fatal ( MESG )            
             endif

             if( ncilays .ge. 10 ) then  
                WRITE (MESG , '(A,3I8)' ) '>=Ten cloud layers at i,j,ncilays=',i,j,ncilays
                CALL wrf_error_fatal ( MESG )
             endif
             
             if( (cu_physics .eq. 0 .and. ires_typ .eq. 2) .or. &
                 (kfshonly          .and. ires_typ .eq. 2) .or. &
                 (gfshonly          .and. ires_typ .eq. 2) .or. &                  
                 (ntshonly          .and. ires_typ .eq. 2)  ) then  !! possible resolved scale convection
                 
                 IF( rfl_maxagl(i,j) .gt. echo_top_maxdbz ) THEN !! then generic max reflectivity at or above 18dbz
                                                                 !! loop through cloud layers and see if any deeper than 4km ...                     
                    kconv = 0
                    do k=1,ncilays
                       cdepk = ctop_ht(k)-cbot_ht(k)             !! meters cloud depth base to top
                       if( cbot_ht( k ) .le. 3500. .and. cdepk .gt. 4000.0 ) then
                           kconv = k                             !! cloud layer at which resolved convection possible
                           EXIT
                       endif
                    enddo                                       !! DO WE NEED A total condensate test?
                                                                                   
                    if( kconv .gt. 0 .and. rannc_rate(i,j) .ge. PRLIMRCONV ) then  !! resolved convection possible; test vertical velocity
                        do k = kcibas(kconv),kcitop(kconv)
                           if( prmb(k) .ge. 300.0 ) then                     !! AFWA limits at 400mb.... seems low to me.... 
                              w0absmax = MAX( w0absmax, abs(w0avg(i,k,j)) )  !! updraft or downdraft ...
                           endif   
                        enddo
                        
                        if( w0absmax .gt. 0.50 ) then   !! somewhat weak criteria, but OK for now: determine resolved convection
                                                        !! note this is a running approx five minute average ...  
                           lresdcon = .true.
                           cnvresmask( i,j ) = 1.0
                           kbot_cnvcr = kcibas(kconv)
                           ktop_cnvcr = kcitop(kconv)                           
                           if( ncilays .eq. 1 ) lresdtyp = 1
                           if( ncilays .ge. 2 ) lresdtyp = 2
                           
                           !! Enhance wind gust estimate with resolved deep convection following ECMWF
                           gspd10(i,j) = gspd10(i,j) + 0.6*convshr_gust
                           !! print*,'Resolved Convective enhancememt gust speed at i,j=',i,j,gspd10(i,j)                           

                        endif
                    endif
             
                 endif
             endif !! possible resolved scale convection
                          
         else !! no resolved cloud; check for isolated parameterized convection
         
             if( lresdcon ) then
                WRITE( MESG, '( A,2I8 )' ) 'Resolv-convec flag TRUE when no-resolved cloud identified at i,j=',i,j
                CALL wrf_error_fatal ( TRIM(MESG) )
             endif            
                          
             if( rain_rate(i,j) .gt. MM_PER_100HRS ) then !! rate in mm/s == .1mm/hour !! Needs to become convective rain rate !! 
         
                if( ctop_cnvct .le. 0.0 ) then
                   WRITE( MESG, '( A, 2I8 )' ) 'WARNING: No cvctv cloud top found (1) despite cnvctv precip at i,j=',i,j
                   !! CALL wrf_message (TRIM(mesg))
                   WRITE( MESG, '( A, F13.8,I8,F13.8 )' ) 'rannc_rate,ires_typ,rain_rate=',rannc_rate(i,j),ires_typ,rain_rate(i,j)
                   !! CALL wrf_message (TRIM(mesg))
                   WRITE( MESG, '( A, 3(F13.8,2X) )' ) 'radcv_rate,ctop_cnvct,rascv_rate=',radcv_rate(i,j),ctop_cnvct,rascv_rate(i,j)
                   !! CALL wrf_message (TRIM(mesg))                   
                   WRITE( MESG, '( A, 2I8,2(F13.8,2X) )' ) 'kbot_cnvct,ktop_cnvct,cbot_cnvct,cdpt_cnvct=',kbot_cnvct,ktop_cnvct,cbot_cnvct,cdpt_cnvct
                   !! CALL wrf_message (TRIM(mesg))                
                   WRITE( MESG, '( A, 2I8 )' ) 'cu_act_last,cu_activ=',cu_act_last(i,j),NINT(cu_activ(i,j))
                   !! CALL wrf_message (TRIM(mesg))                   
                   WRITE( MESG, '( A, 3I8 )' ) 'kcibas_lo,kcitop_hi,ncilays=',kcibas_lo,kcitop_hi,ncilays
                   !! CALL wrf_message (TRIM(mesg))          
                   MESG = 'WARNING: Assuming clear sky'
                   !! CALL wrf_message (TRIM(mesg))
                   ifip_situ = 1
                   lbranch225 = .true.
                   !! goto 225        !! Clear-sky, no icing potential: icg_pot3d and sld_pot3d already == 0.0               
                endif
                
                if( cshl_cnvct .eq. 1 ) then                  
                   if( cdpt_cnvct .gt. 2000.00 ) then          !! since the cloud is raining and a bit deep, treat as isolated deep
                      ifip_situ = 3
                      iso_deepr = .TRUE.              
                      !! write( mesg, '( A, 3I4, F13.5, I4, 2F13.5, I8 )' ) 'islted raining deeper shall:i,j,kb,cb,kt,ct,dpth,ifip_situ=', &
                      !!                               i,j,kbot_cnvct,cbot_cnvct,ktop_cnvct,ctop_cnvct,cdpt_cnvct,ifip_situ                      
                      !! CALL wrf_message (TRIM(mesg))
                      lbranch222 = .true.
                      !! goto 222
                   else
                      iso_shalr = .TRUE.                       !! will be treated like a ppt-ing layer cld excpt using frac cov                                                                                                                                                
                      ncilays = 1
                      kcibas( 1 ) = kbot_cnvct
                      kcitop( 1 ) = ktop_cnvct
                      cbot_tc( 1 ) = dtc(kbot_cnvct)
                      cbot_ht( 1 ) = zht(kbot_cnvct) - terht(i,j)  !! height of base
                      ctop_tc( 1 ) = dtc(ktop_cnvct)
                      ctop_ht( 1 ) = zht(ktop_cnvct) - terht(i,j)                      
                      !! write( mesg, '( A, 3I4, F13.5, I4, 2F13.5, I8 )' ) 'islted raining shall:i,j,kb,cb,kt,ct,dpth,ifip_situ=', &
                      !!                               i,j,kbot_cnvct,cbot_cnvct,ktop_cnvct,ctop_cnvct,cdpt_cnvct,ifip_situ                      
                      !! CALL wrf_message (TRIM(mesg))
                   endif                    
                else if( cshl_cnvct .eq. 0 ) then
                   ifip_situ = 3
                   iso_deepr = .TRUE.
                   lbranch222 = .true.                   
                   !! goto 222            
                else
                   if( lbranch225 ) then
                      CONTINUE
                   else
                      CALL wrf_error_fatal ( 'Bad cshl_cnvct(1)' )                   
                   endif
                endif             
             else                  !! No resolved cloud and very light precip
                if( ctop_cnvct .gt. 0.0 ) then
                   if( cshl_cnvct .eq. 1 ) then 
                      ifip_situ = 2
                      !! write( mesg, '( A, 3I4, F13.5, I4, 2F13.5, I8 )' ) 'islted NON-raining shall:i,j,kb,cb,kt,ct,dpth,ifip_situ=', &
                      !!                                 i,j,kbot_cnvct,cbot_cnvct,ktop_cnvct,ctop_cnvct,cdpt_cnvct,ifip_situ                      
                      !! CALL wrf_message (TRIM(mesg))        
                   else if( cshl_cnvct .eq. 0 ) then
                      ifip_situ = 3
                      lbranch222 = .true.
                      !! goto 222                  
                   else
                      CALL wrf_error_fatal ( 'Bad cshl_cnvct(2)' )
                   endif   
                else 
                   ifip_situ = 1
                   lbranch225 = .true.               
                   !! goto 225        !! Clear-sky, no icing potential: icg_pot3d and sld_pot3d already == 0.0
                endif             
             endif
         endif         !! End if a resolved cloud was just identified, determine layer structure                 

121      CONTINUE

         !! Now determine additional heuristics of the situation .. 
         !! (1) categorical p-types required for the scheme: (RA, SN, PE, FZRA, DZ, FZDZ)
         !! If there is sufficient surface precipitation and snow is the "only" 
         !! p-type present, then the sub-cloud layer need not be considered for icing 

         ALL_SNOW = .false.
         kwarm = .false.
         kfrzr = .false.
         if( rain_rate(i,j) .gt. MM_PER_100HRS ) then !! rate in mm/s == .1mm/hour            
            if( (sn_nc_rate(i,j)+sn_cv_rate(i,j))/rain_rate(i,j) .gt. 0.90 ) then      !! "effectively all snow"
               ptyp_fipi( i,j ) = SNOW
               ptyp_cati( i,j ) = CAT_SNNW                                             !! includes RESOLVED CONVECTION
               ALL_SNOW = .true.
            else if( (radcv_rate(i,j)+rascv_rate(i,j))/rain_rate(i,j) .gt. 0.90 ) then !! "effectively all param convective, not all snow"
               !! assume drizzle not possible; must be either RA, PE, OR FZRA
               
               if( (lq_cv_rate(i,j)/rain_rate(i,j)) .gt. 0.675 ) then   !! "majority convective liquid"                  
                  if( t2(i,j) .le. CTOK ) then
                     ptyp_fipi( i,j ) = FZRA            !! FRZA
                     ptyp_cati( i,j ) = CAT_FZRA
                  else
                     ptyp_fipi( i,j ) = RAAN            !! RA
                     ptyp_cati( i,j ) = CAT_RAAN
                  endif
                  if( sn_cv_rate(i,j) .gt. 1.0e-20 .or. gr_cv_rate(i,j) .gt. 1.0e-20 ) then
                      WRITE( MESG, '( A, 2I8 )' ) 'conv p-type 1 at i,j=',i,j
                      CALL wrf_error_fatal ( TRIM(MESG) )                      
                  endif
               else if( (sn_cv_rate(i,j)/rain_rate(i,j)) .gt. 0.675 ) then !! "majority convective snow"
                  if( t2(i,j) .gt. CTOK+3.0 ) then 
                     ptyp_fipi( i,j ) = RAAN            !! RA
                     ptyp_cati( i,j ) = CAT_RAAN
                  else
                     ptyp_fipi( i,j ) = SNOW            !! SN
                     ptyp_cati( i,j ) = CAT_SNNW
                  endif
                  if( lq_cv_rate(i,j) .gt. 1.0e-20 .or. gr_cv_rate(i,j) .gt. 1.0e-20 ) then
                      WRITE( MESG, '( A, 2I8 )' ) 'conv p-type 2 at i,j=',i,j
                      CALL wrf_error_fatal ( TRIM(MESG) )                      
                  endif                                                              
               else if( (gr_cv_rate(i,j)/rain_rate(i,j)) .gt. 0.675 ) then !! "mixed convective"
                  if( t2(i,j) .gt. CTOK+3.0 ) then 
                     ptyp_fipi( i,j ) = RAAN            !! RA
                     ptyp_cati( i,j ) = CAT_MIXD
                  else if( t2(i,j) .gt. CTOK ) then 
                     ptyp_fipi( i,j ) = PELT            !! PE
                     ptyp_cati( i,j ) = CAT_MIXD
                  else if( lq_nc_rate(i,j) .gt. 0.1 * MM_PER_100HRS ) then
                     ptyp_fipi( i,j ) = FZRA            !! FRZA
                     ptyp_cati( i,j ) = CAT_MIXD             
                  else
                     ptyp_fipi( i,j ) = PELT            !! PE
                     ptyp_cati( i,j ) = CAT_MIXD
                  endif
                  if( sn_cv_rate(i,j) .gt. 1.0e-20 .or. lq_cv_rate(i,j) .gt. 1.0e-20 ) then
                      WRITE( MESG, '( A, 2I8 )' ) 'conv p-type 3 at i,j=',i,j
                      CALL wrf_error_fatal ( TRIM(MESG) )                      
                  endif                                                                                                          
               else
                  WRITE( MESG, '( A, 2I8 )' ) 'Bad convective p-type determination at i,j=',i,j
                  CALL wrf_error_fatal ( TRIM(MESG) )                                                                                 
               endif
                              
            else !! enough explicit to use that to determine p-types (even when a majority of the precip
                 !! may be convective); not all snow; INCLUDES RESOLVED CONVECTION
               
               if( (lq_nc_rate(i,j)/rannc_rate(i,j)) .gt. 0.675 ) then !! "majority" liquid
                   
                   !! determine drizzle first: precip must be "all" explicit                  
              
                   if( (radcv_rate(i,j)+rascv_rate(i,j))/rain_rate(i,j) .lt. 0.01) then  !! precip "all" explicit                    
                     
                     if( cbot_ht(1) .lt. 0.0 ) then                     
                        WRITE( MESG, '( A, 2I8 )' ) 'CBOT_HT missing but all explicit precip at i,j=',i,j
                        CALL wrf_error_fatal ( TRIM(MESG) )
                     endif               
                     
                     if( lq_nc_rate(i,j) .le. MM_PER_2HRS .and.  &       !! precip rate is small enough for drizzle
                              ctop_tc(1) .ge.  -8.0 .and.        &       !! "warm cloud top" drizzle more likely 
                              cbot_ht(1) .le. 500.0 .and.        &       !! low cloud base; drizzle likely
                              cfracl(i,j) .gt. 0.95 ) then               !! "socked-in"; drizzle likely

                        if( t2(i,j) .le. CTOK ) then
                           ptyp_fipi( i,j ) = FZDZ      !! FZDZ
                           ptyp_cati( i,j ) = CAT_FZRA
                        else
                           ptyp_fipi( i,j ) = DZZL      !! DZ
                           ptyp_cati( i,j ) = CAT_RAAN
                        endif
                     else      !! precip rate is too high even though "all" explicit
                        if( t2(i,j) .le. CTOK+1.5 ) then
                           if( gr_nc_rate(i,j)/rannc_rate( i,j ) .gt. 0.10 ) then !! "at least  marginal pellets mixing in
                              ptyp_fipi( i,j ) = PELT            !! PE
                              ptyp_cati( i,j ) = CAT_PELT                           
                           else if( t2(i,j) .le. CTOK ) then
                              ptyp_fipi( i,j ) = FZRA      !! FRZA
                              ptyp_cati( i,j ) = CAT_FZRA
                           else
                              ptyp_fipi( i,j ) = RAAN      !! RA
                              ptyp_cati( i,j ) = CAT_RAAN
                           endif
                        else
                           ptyp_fipi( i,j ) = RAAN      !! RA
                           ptyp_cati( i,j ) = CAT_RAAN
                        endif              
                     endif                   
                   else !! there is sufficient convective precip that drizzle is not likely                      
                      if( t2(i,j) .le. CTOK+1.5 ) then
                         if( gr_nc_rate(i,j)/rannc_rate( i,j ) .gt. 0.10 ) then !! "at least marginal pellets mixing in
                            ptyp_fipi( i,j ) = PELT            !! PE
                            ptyp_cati( i,j ) = CAT_PELT                           
                         else if( t2(i,j) .le. CTOK ) then
                            ptyp_fipi( i,j ) = FZRA      !! FRZA
                            ptyp_cati( i,j ) = CAT_FZRA
                         else
                            ptyp_fipi( i,j ) = RAAN      !! RA
                            ptyp_cati( i,j ) = CAT_RAAN
                         endif
                      else
                         ptyp_fipi( i,j ) = RAAN        !! RA
                         ptyp_cati( i,j ) = CAT_RAAN
                      endif                   
                   endif                
               else if( sn_nc_rate(i,j)/rannc_rate( i,j ) .gt. 0.675 ) then !! "majority snow"
                  if( t2(i,j) .gt. CTOK+2.0 ) then 
                     ptyp_fipi( i,j ) = RAAN            !! RA
                     ptyp_cati( i,j ) = CAT_RAAN
                  else
                     ptyp_fipi( i,j ) = SNOW            !! SN
                     ptyp_cati( i,j ) = CAT_SNNW
                  endif        
               else if( gr_nc_rate(i,j)/rannc_rate( i,j ) .gt. 0.675 ) then !! "majority pellets"  [P52 mod]
                  if( t2(i,j) .gt. CTOK+1.5 ) then 
                     ptyp_fipi( i,j ) = RAAN            !! RA
                     ptyp_cati( i,j ) = CAT_RAAN
                  else
                     ptyp_fipi( i,j ) = PELT            !! PE
                     ptyp_cati( i,j ) = CAT_PELT
                  endif   
               else           !! mixed in some form, including some liquid (full DTO MOD)
                   if( t2(i,j) .gt. CTOK+1.5 ) then !! while mixed, still mostly rain
                      ptyp_fipi( i,j ) = RAAN           !! RA
                      ptyp_cati( i,j ) = CAT_MIXD
                   else if( (snl(i,j)-terht(i,j)) .le. 0.0 ) then !! SNOW
                      ptyp_fipi( i,j ) = SNOW        
                      ptyp_cati( i,j ) = CAT_SNNW                                    
                   else if( (mxl(i,j)-terht(i,j)) .le. 0.0 ) then !! PELLETS
                      if( gr_nc_rate(i,j)/rannc_rate( i,j ) .le. 0.10 ) then !! not enough pellets mixing in
                         if( t2(i,j) .le. CTOK ) then
                            ptyp_fipi( i,j ) = FZRA      !! FRZA
                            ptyp_cati( i,j ) = CAT_FZRA                            
                         else
                            ptyp_fipi( i,j ) = RAAN      !! RA
                            ptyp_cati( i,j ) = CAT_RAAN                         
                         endif                      
                      else
                         ptyp_fipi( i,j ) = PELT            !! PE
                         ptyp_cati( i,j ) = CAT_PELT
                      endif              
                   else                                           !! RAIN 
                      if( gr_nc_rate(i,j)/rannc_rate( i,j ) .gt. 0.10 ) then !! "at least marginal pellets mixing in
                         ptyp_fipi( i,j ) = PELT      !! PE
                         ptyp_cati( i,j ) = CAT_PELT                     
                      else if( t2(i,j) .le. CTOK ) then
                         ptyp_fipi( i,j ) = FZRA      !! FRZA
                         ptyp_cati( i,j ) = CAT_FZRA
                      else
                         ptyp_fipi( i,j ) = RAAN      !! RA
                         ptyp_cati( i,j ) = CAT_RAAN
                      endif
                   endif   !! end if whether too warm for "mixed"                                                                  
               endif       !! end if determining whether all liquid, all snow, or all pellets                           
            endif          !! end if determine whether convective or explicit to determine ptype                         
         
            !! includes RESOLVED CONVECTION
                        
            if( ptyp_fipi( i,j ) .eq. NONE ) then
                WRITE( MESG, '( A, 2I8 )' ) 'Indeterminate p-type at i,j=',i,j
                CALL wrf_error_fatal ( TRIM(MESG) )
            else if( ptyp_fipi( i,j ) .eq. RAAN .or. ptyp_fipi( i,j ) .eq. DZZL ) then
                raliqd(i,j) = rain_this(i,j)
                raliqdcum(i,j) = raliqdcum(i,j) + rain_this(i,j)
                cat_rain(i,j) = 1.0
                ratotlcum(i,j) = ratotlcum(i,j) + rain_this(i,j)
            else if( ptyp_fipi( i,j ) .eq. SNOW ) then
                rasnow(i,j) = rain_this(i,j)
                rasnowcum(i,j) = rasnowcum(i,j) + rain_this(i,j)
                cat_snow(i,j) = 1.0                                
                !! Implement Cobb-based dynamic snow-ratio algorithm
                call cobb_dyn_snow( i,j,kts,kte,ires_typ,ctop_cnvct,cbot_cnvct,cdpt_cnvct,iso_deepr, &
                                    iso_shalr,lbranch225,kbot_cnvct,ktop_cnvct,sn_cv_rate(i,j),rain_rate(i,j), &
                                    w0avg(i,kts:kte,j),prmb,dtc,dz8w,NSRT,snrat_temp,snrat_dens,ncilays,kcibas, &
                                    kcitop,lresdcon,dta,ep_2,qv(i,kts:kte,j),snowratio )
                dyrasnow = rain_this(i,j) * snowratio
                dysnowcum(i,j) =  dysnowcum(i,j) + dyrasnow
                dysnratcu(i,j) =  dysnowcum(i,j) / rasnowcum(i,j)
                rafrzn(i,j) = rain_this(i,j)              
                rafrzncum(i,j) = rafrzncum(i,j) + rain_this(i,j)                      !! CAWO
                ratotlcum(i,j) = ratotlcum(i,j) + rain_this(i,j)        
            else if( ptyp_fipi( i,j ) .eq. PELT ) then
                raicep(i,j) = rain_this(i,j)
                raicepcum(i,j) = raicepcum(i,j) + rain_this(i,j)
                cat_icep(i,j) = 1.0
                rafrzn(i,j) = rain_this(i,j)
                rafrzncum(i,j) = rafrzncum(i,j) + rain_this(i,j)                      !! CAWO
                ratotlcum(i,j) = ratotlcum(i,j) + rain_this(i,j)         
            else if( ptyp_fipi( i,j ) .eq. FZRA .or. ptyp_fipi( i,j ) .eq. FZDZ ) then            
                rafzra(i,j) = rain_this(i,j)
                rafzracum(i,j) = rafzracum(i,j) + rain_this(i,j)
                cat_fzrn(i,j) = 1.0
                rafrzn(i,j) = rain_this(i,j)
                rafrzncum(i,j) = rafrzncum(i,j) + rain_this(i,j)                      !! CAWO
                ratotlcum(i,j) = ratotlcum(i,j) + rain_this(i,j)          
            endif
            
            if( rain_this(i,j) .gt. 0.0 ) then                                            !! CAWO
                rafrznfr(i,j) = rafrzn(i,j) / rain_this(i,j)
            endif
            if( ratotlcum(i,j) .gt. 0.0 ) then
                rafrzncumfr(i,j) = rafrzncum(i,j) / ratotlcum(i,j)
            endif                                                                         !! CAWO
            
            !! if( ldbg ) then
               !! rain_sum = raliqdcum(i,j) + rasnowcum(i,j) + raicepcum(i,j) + rafzracum(i,j)
               !! print*,'at i,j,rain,rain_sum=',rainc(i,j)+rainnc(i,j),rain_sum
               !! CALL wrf_error_fatal ( 'DEBUG' )            
            !! endif
            
            if( ires_typ .gt. 0 .and. .not. ALL_SNOW ) then  !! includes RESOLVED CONVECTION
            
              !! determine if there is a "classic freezing rain" warm nose present..this
              !! could be an aloft structure even if surface temp is "quite high" or "quite low"
              !! irrespective of surface p-type. Note: cloud-top temp may be > 0 under CAD situations
              
              if( t2(i,j) .le. CTOK+3.0 ) then
                 warm_nose : DO k = kcitop( ncilays )+1,1,-1
                    if( .not. kwarm ) then
                       if( dtc(k) .gt. 0.0 ) then
                          ml = k
                          kwarm = .true.
                          CYCLE warm_nose
                       endif
                    endif                   
                    if( kwarm ) then
                       if( dtc(k) .le. 0.0 ) then
                          fl = k
                          kfrzr = .true.
                          EXIT warm_nose                  
                      endif
                    endif            
                 END DO warm_nose
                 if( kfrzr ) then
                   WRITE( MESG, '( A, 3I4, F13.6, I4, F13.6 )' ) 'Warm nose at: i,j,ml,dtc,fl,dtc=',i,j,ml,dtc(ml),fl,dtc(fl) 
                   !! CALL wrf_message (TRIM(mesg))
                   if( ml .eq. 0 .or. fl .eq. 0 .or. (ml .le. fl) ) then
                      WRITE( MESG, '( A, 4I8 )' ) 'Bad warm-nose layers: ',i,j,ml,fl
                      CALL wrf_error_fatal ( MESG ) 
                   endif
                endif
              endif            
            endif                     
         endif       !! end if precipitation rate is high enough to consider surface precip type

         !! Before identifying the situation / sub-situation to calculate icing potential etc., partition
         !! rfl_maxagl into categorical ptypes:
         
         catsum = cat_rain(i,j) + cat_snow(i,j) + cat_icep(i,j) + cat_fzrn(i,j)

         if( ptyp_fipi( i,j ) .ne. NONE ) then               !! categorical p-type is known
             
             if( lresdcon ) then                             !! there is grid-scale convection
                rfl_zr = 300.0*(rain_rate(i,j)*3600.0)**1.4        !! typical convective Z/R              
             else if( ctop_cnvct .gt. 0.0 ) then             !! there is parameterized convection             
                if( cshl_cnvct .eq. 1 ) then
                   if( cdpt_cnvct .gt. 2000.00 ) then        !! shallow param -- treat as isolated deep.
                     rfl_zr = 300.0*(rain_rate(i,j)*3600.0)**1.4   !! typical convective Z/R  
                   else                                      !! shallow param
                     rfl_zr = 250.0*(rain_rate(i,j)*3600.0)**1.2   !! treat as tropical convective                     
                   endif
                else                                         !! deep param           
                   rfl_zr = 300.0*(rain_rate(i,j)*3600.0)**1.4     !! typical convective Z/R

                   !! Enhance wind gust estimate with deep parameterized convection following ECMWF
                   gspd10(i,j) = gspd10(i,j) + 0.6*convshr_gust
                   if( dx .le. 3500.0 ) then
                       PRINT*,'parameterized convection--gust enhancement: ',gspd10(i,j)
                       PRINT*,'ctop_cnvct=',ctop_cnvct
                       PRINT*,'cshl_cnvct=',cshl_cnvct
                       PRINT*,'cbot_cnvct=',cbot_cnvct
                       PRINT*,'lrestart=',lrestart
                       WRITE( MESG, '( A, 4I8 )' ) 'Unexpected deep param at dx=3km: ',i,j
                       CALL wrf_error_fatal ( MESG )
                   endif                                     
                endif                
             else                                            !! stratiform
                rfl_zr = 200.0*(rain_rate(i,j)*3600.0)**1.6        !! Marshall-Palmer
             endif
             
             rfl_zr = 10. * log10(rfl_zr)     
             
             if( rfl_maxagl(i,j) .lt. 5.0 ) then
                rfl_maxagl(i,j) = max( rfl_maxagl(i,j), rfl_zr ) !! artificial!!! There may be areas where it is precipitating heavy
             endif                                               !! enough to care about, but the reflectivies are small. Include
             if( rflmpscmax(i,j) .lt. 5.0 ) then                 !! this "artifact" to improve aesthetics...
                rflmpscmax(i,j) = max( rflmpscmax(i,j), rfl_zr )
             endif
             rfl_mxcagl(i,j) = rfl_maxagl(i,j)
         
             if( catsum .le. 0.5 ) then
                WRITE( MESG, '( A, 2I8 )' ) 'Categorical precip NOT set when it should be at i,j=',i,j
                CALL wrf_error_fatal ( TRIM(MESG) )             
             endif

             if( dx .le. 3500.0 .and. cshl_cnvct .eq. 1 .and. rain_rate(i,j) .le. MM_PER_2HRS ) then  !! limit reflectivity from leaky shallow clouds
                                                                                                      !! when running at convection resolving scales
                 if( rfl_maxagl(i,j) .gt. 0.0 ) then             !! There is positive max refl in the column: treat as VIRGA        
                     if( rfl_maxagl(i,j) .lt. 13.0 ) then        !! floor at 13dbZ for "virga" 
                        rfl_1kmagl(i,j) = 0.0
                        rfl_maxagl(i,j) = 0.0                
                        vilh(i,j) = 0.0             
                     endif
                     if( rflmpscmax(i,j) .lt. 13.0 ) then         !! floor at 13dbZ for "virga"
                        rflmpsc1km(i,j) = 0.0
                        rflmpscmax(i,j) = 0.0
                        radarvilh(i,j) = 0.0 
                     endif        
                 end if

                 if( rfl_mxcagl(i,j) .gt. 0.0 ) then
                    if( rfl_mxcagl(i,j) .lt. 20.0 ) then
                       rfl_mxcagl(i,j) = 0.0
                    else if( rfl_mxcagl(i,j) .le. 25.0 ) then
                       rfl_mxcagl(i,j) = 2.4*rfl_mxcagl(i,j) - 35.0   
                    endif             
                 endif
             
             else if( rain_rate(i,j) .le. MM_PER_10HRS ) then    !! treat as if were Virga using the older rain-rate cutoff...
                 if( rfl_maxagl(i,j) .gt. 0.0 ) then             !! There is positive max refl in the column: treat as VIRGA        
                     if( rfl_maxagl(i,j) .lt. 13.0 ) then        !! floor at 13dbZ for "virga" 
                        rfl_1kmagl(i,j) = 0.0
                        rfl_maxagl(i,j) = 0.0                
                        vilh(i,j) = 0.0             
                     endif
                     if( rflmpscmax(i,j) .lt. 13.0 ) then         !! floor at 13dbZ for "virga"
                        rflmpsc1km(i,j) = 0.0
                        rflmpscmax(i,j) = 0.0
                        radarvilh(i,j) = 0.0 
                     endif        
                 end if

                 if( rfl_mxcagl(i,j) .gt. 0.0 ) then
                    if( rfl_mxcagl(i,j) .lt. 20.0 ) then
                       rfl_mxcagl(i,j) = 0.0
                    else if( rfl_mxcagl(i,j) .le. 25.0 ) then
                       rfl_mxcagl(i,j) = 2.4*rfl_mxcagl(i,j) - 35.0   
                    endif             
                 endif
             endif

         else                                                !! categorical precipitation has not been set: possible VIRGA 
             if( rfl_maxagl(i,j) .gt. 0.0 ) then             !! There is positive max refl in the column: VIRGA        
                 if( rfl_maxagl(i,j) .lt. 13.0 ) then        !! floor at 13dbZ for "virga" 
                    rfl_1kmagl(i,j) = 0.0
                    rfl_maxagl(i,j) = 0.0                
                    vilh(i,j) = 0.0             
                 endif
                 if( rflmpscmax(i,j) .lt. 13.0 ) then         !! floor at 13dbZ for "virga"
                    rflmpsc1km(i,j) = 0.0
                    rflmpscmax(i,j) = 0.0
                    radarvilh(i,j) = 0.0 
                 endif        
             end if
             
             if( rfl_mxcagl(i,j) .gt. 0.0 ) then
                if( rfl_mxcagl(i,j) .lt. 20.0 ) then
                   rfl_mxcagl(i,j) = 0.0
                else if( rfl_mxcagl(i,j) .le. 25.0 ) then
                   rfl_mxcagl(i,j) = 2.4*rfl_mxcagl(i,j) - 35.0   
                endif             
             endif                
              
             if( catsum .gt. 0.5 ) then
                WRITE( MESG, '( A, 2I8 )' ) 'Categorical precip set when it should not be at i,j=',i,j
                CALL wrf_error_fatal ( TRIM(MESG) )             
             else
                if( (snl(i,j)-terht(i,j)) .le. 100.0 ) then !! DTO: Allow a 100m "melting" zone
                   cat_snow(i,j) = 1.0
                else if( (mxl(i,j)-terht(i,j)) .gt. 0.0  ) then
                   cat_rain(i,j) = 1.0
                else
                   if( t2(i,j) .le. CTOK ) then                
                      dzh = ght_pl(i,p850,j)-ght_pl(i,p1000,j)                      
                      if( dzh .ge. 1290.0 ) then                
                         cat_fzrn(i,j) = 1.0
                      else                
                         cat_icep(i,j) = 1.0                
                      endif
                   else
                      cat_icep(i,j) = 1.0
                   endif                                  
                endif
             endif
             catsum = cat_rain(i,j) + cat_snow(i,j) + cat_icep(i,j) + cat_fzrn(i,j)
             if( catsum .le. 0.5 ) then
                WRITE( MESG, '( A, 2I8 )' ) 'Categorical precip NOT set when it should just have been at i,j=',i,j
                CALL wrf_error_fatal ( TRIM(MESG) )             
             endif
             !! Note: this "DTO assignment to categoricals must be made again below because
             !! code above may branch around this section: see goto 225 above.         
         endif
         
         !! We can now identify the situation and sub-situations allowing us to calculate the initial
         !! icing potential icg_pot3d() and final super-cooled large droplet sld_pot3d() potential

         if( lbranch222 ) goto 222
         if( lbranch225 ) goto 225         
         
         if( ctop_cnvct .eq. 0 .and. ncilays .eq. 0 ) then
             WRITE( MESG, '( A, 2I8 )' ) 'Out of place clear-sky condition at i,j=',i,j
             CALL wrf_error_fatal ( 'Clear sky' )              
         endif
         
         if( (ctop_cnvct .eq. 0 .or. iso_shalr) .and. .NOT.lresdcon ) then   !! all resolved layer clouds or isolated ppt-ing shallow convection
            if( kfrzr ) then
               if( ncilays .eq. 1 ) then 
                   ifip_situ = 24 !! Classic FRZA structure, single layer
               else
                   ifip_situ = 25 !! Classic FRZA structure, multiple layers        
               endif    
            else
               if(  ptyp_fipi( i,j ) .eq. NONE .or. &
                  ( ptyp_fipi( i,j ) .eq. RAAN .and. ctop_tc(1) .lt. -12.0 ) .or. &
                  ( ptyp_fipi( i,j ) .eq. DZZL .and. ctop_tc(1) .lt. -12.0 ) ) then
                  if( ncilays .eq. 1 ) then 
                      ifip_situ = 4
                  else
                      ifip_situ = 14                    
                  endif                   
               else if( ALL_SNOW ) then
                  if( ncilays .eq. 1 ) then  
                      ifip_situ = 5
                  else
                      ifip_situ = 15                      
                  endif                
               else if( ( ptyp_fipi( i,j ) .eq. FZDZ .and. ctop_tc(1) .ge. -30.0 ) .or. &
                        ( ptyp_fipi( i,j ) .eq. FZRA .and. ctop_tc(1) .ge. -30.0 ) .or. &
                        ( ptyp_fipi( i,j ) .eq. PELT .and. ctop_tc(1) .ge. -30.0 ) .or. &
                        ( ptyp_fipi( i,j ) .eq. SNOW .and. ctop_tc(1) .ge. -30.0 ) ) then  !! SNOW here is majority but not all
                  if( ncilays .eq. 1 ) then  
                      ifip_situ = 6
                  else
                      ifip_situ = 16                      
                  endif                              
               else if( ( ptyp_fipi( i,j ) .eq. FZDZ .and. ctop_tc(1) .lt. -30.0 ) .or. &
                        ( ptyp_fipi( i,j ) .eq. FZRA .and. ctop_tc(1) .lt. -30.0 ) .or. &
                        ( ptyp_fipi( i,j ) .eq. PELT .and. ctop_tc(1) .lt. -30.0 ) .or. &
                        ( ptyp_fipi( i,j ) .eq. SNOW .and. ctop_tc(1) .lt. -30.0 ) ) then  !! SNOW here is majority but not all                  
                  if( ncilays .eq. 1 ) then  
                      ifip_situ = 7
                  else
                      ifip_situ = 17                      
                  endif                                                      
               else if( ctop_tc(1) .ge. -12.0 ) then
                  if( ptyp_fipi( i,j ) .eq. RAAN .or. ptyp_fipi( i,j ) .eq. DZZL ) then
                     if( t2(i,j) .ge. CTOK-2.0 ) then                        
                        if( ncilays .eq. 1 ) then  
                            ifip_situ = 8
                        else
                            ifip_situ = 18                      
                        endif
                     endif 
                  endif
               endif
            endif
         else if( ctop_cnvct .gt. 0.0 .and. ires_typ .gt. 0 .and. .NOT.lresdcon ) then   !! there is param convection in addition to resolved layer clouds
            
            if( kfrzr ) then
               if( ncilays .eq. 1 ) then 
                   ifip_situ = 26 !! Classic FRZA structure, single layer
               else
                   ifip_situ = 27 !! Classic FRZA structure, multiple layers          
               endif    
            else
               if(  ptyp_fipi( i,j ) .eq. NONE .or. &
                  ( ptyp_fipi( i,j ) .eq. RAAN .and. ctop_tc(1) .lt. -12.0 ) .or. &
                  ( ptyp_fipi( i,j ) .eq. DZZL .and. ctop_tc(1) .lt. -12.0 ) ) then
                  if( ncilays .eq. 1 ) then 
                      ifip_situ = 9
                  else
                      ifip_situ = 19                      
                  endif                   
               else if( ALL_SNOW ) then
                  if( ncilays .eq. 1 ) then  
                      ifip_situ = 10
                  else
                      ifip_situ = 20                      
                  endif                
               else if( ( ptyp_fipi( i,j ) .eq. FZDZ .and. ctop_tc(1) .ge. -30.0 ) .or. &
                        ( ptyp_fipi( i,j ) .eq. FZRA .and. ctop_tc(1) .ge. -30.0 ) .or. &
                        ( ptyp_fipi( i,j ) .eq. PELT .and. ctop_tc(1) .ge. -30.0 ) .or. &
                        ( ptyp_fipi( i,j ) .eq. SNOW .and. ctop_tc(1) .ge. -30.0 ) ) then  !! SNOW here is majority but not all
                  if( ncilays .eq. 1 ) then  
                      ifip_situ = 11
                  else
                      ifip_situ = 21                     
                  endif                              
               else if( ( ptyp_fipi( i,j ) .eq. FZDZ .and. ctop_tc(1) .lt. -30.0 ) .or. &
                        ( ptyp_fipi( i,j ) .eq. FZRA .and. ctop_tc(1) .lt. -30.0 ) .or. &
                        ( ptyp_fipi( i,j ) .eq. PELT .and. ctop_tc(1) .lt. -30.0 ) .or. &
                        ( ptyp_fipi( i,j ) .eq. SNOW .and. ctop_tc(1) .lt. -30.0 ) ) then  !! SNOW here is majority but not all                  
                  if( ncilays .eq. 1 ) then  
                      ifip_situ = 12
                  else
                      ifip_situ = 22                      
                  endif                                                      
               else if( ctop_tc(1) .ge. -12.0 ) then
                  if( ptyp_fipi( i,j ) .eq. RAAN .or. ptyp_fipi( i,j ) .eq. DZZL ) then
                     if( t2(i,j) .ge. CTOK-2.0 ) then                        
                        if( ncilays .eq. 1 ) then  
                            ifip_situ = 13
                        else
                            ifip_situ = 23                  
                        endif
                     endif 
                  endif           
               endif
            endif
            if( rain_rate(i,j) .gt. MM_PER_100HRS ) then
               if( (radcv_rate(i,j)+rascv_rate(i,j))/rain_rate(i,j) .gt. 0.01 ) then !! sufficient param precip to set base at ground
                 mlt_deepr = .TRUE. 
               endif                
            endif
         else if( lresdcon .and. lresdtyp .eq. 1 ) then !! there is isolated RESOLVED CONVECTION 
                                                        !! (may also include parameterized shallow; if so, we ignore its effects for icing purposes) 
            
            if( ifip_situ .ne. 0 ) then
                WRITE( MESG, '( A, 3I8 )' ) 'IFIP_SITU improperly set for resolved precipitating convection(1) at i,j,ifip_situ=',i,j,ifip_situ
                CALL wrf_error_fatal ( TRIM(MESG) )
            endif
            if( ptyp_fipi( i,j ) .eq. NONE ) then
               WRITE( MESG, '( A, 2I8 )' ) 'PTYPE_FIPI unset for resolved precipitating convection(1) at i,j=',i,j
               CALL wrf_error_fatal ( TRIM(MESG) )            
            endif
            ifip_situ = 31                              !! like 3 except for resolved convection                                         
            kbot = 1
            ktop = ktop_cnvcr
            
            if( cshl_cnvct .eq. 1 ) lresdshal = .true.  !! used for paranoia only          
                              
         else if( lresdcon .and. lresdtyp .eq. 2 ) then !! there is resolved convection plus one or more additional layer clouds 
                                                        !! (may also include parameterized shallow; if so, we ignore its effects for icing purposes)
         
            WRITE( MESG, '( A, 3I8 )' ) 'Resolved convection and layer cld(s) at: ',i,j,ptyp_fipi( i,j )
            !! CALL wrf_message (TRIM(mesg))            
            
            if( ifip_situ .ne. 0 ) then
                WRITE( MESG, '( A, 3I8 )' ) 'IFIP_SITU improperly set for resolved precipitating convection(2) at i,j,ifip_situ=',i,j,ifip_situ
                CALL wrf_error_fatal ( TRIM(MESG) )
            endif            
            if( ptyp_fipi( i,j ) .eq. NONE ) then
               WRITE( MESG, '( A, 2I8 )' ) 'PTYPE_FIPI unset for resolved precipitating convection(2) at i,j=',i,j
               CALL wrf_error_fatal ( TRIM(MESG) )            
            endif                                                        
            if( kfrzr ) then
               if( ncilays .eq. 2 ) then 
                   ifip_situ = 261 !! Classic FRZA structure, single layer + RESOLVED CONVECTION like 26
               else
                   ifip_situ = 271 !! Classic FRZA structure, multiple layers + RESOLVED CONVECTION like 27           
               endif            
            else
               if( ( ptyp_fipi( i,j ) .eq. RAAN .and. ctop_tc(1) .lt. -12.0 ) .or. &
                   ( ptyp_fipi( i,j ) .eq. DZZL .and. ctop_tc(1) .lt. -12.0 ) ) then   
                  if( ncilays .eq. 2 ) then 
                      ifip_situ = 91           !! RESOLVED CONVECTION like 9 
                  else
                      ifip_situ = 191          !! RESOLVED CONVECTION like 19                     
                  endif         
               else if( ALL_SNOW ) then
                  if( ncilays .eq. 2 ) then  
                      ifip_situ = 101          !! RESOLVED CONVECTION like 10 
                  else
                      ifip_situ = 201          !! RESOLVED CONVECTION like 20                      
                  endif         
               else if( ( ptyp_fipi( i,j ) .eq. FZDZ .and. ctop_tc(1) .ge. -30.0 ) .or. &
                        ( ptyp_fipi( i,j ) .eq. FZRA .and. ctop_tc(1) .ge. -30.0 ) .or. &
                        ( ptyp_fipi( i,j ) .eq. PELT .and. ctop_tc(1) .ge. -30.0 ) .or. &
                        ( ptyp_fipi( i,j ) .eq. SNOW .and. ctop_tc(1) .ge. -30.0 ) ) then  !! SNOW here is majority but not all
                  if( ncilays .eq. 2 ) then  
                      ifip_situ = 111          !! RESOLVED CONVECTION like 11
                  else
                      ifip_situ = 211          !! RESOLVED CONVECTION like 21                     
                  endif                              
               else if( ( ptyp_fipi( i,j ) .eq. FZDZ .and. ctop_tc(1) .lt. -30.0 ) .or. &
                        ( ptyp_fipi( i,j ) .eq. FZRA .and. ctop_tc(1) .lt. -30.0 ) .or. &
                        ( ptyp_fipi( i,j ) .eq. PELT .and. ctop_tc(1) .lt. -30.0 ) .or. &
                        ( ptyp_fipi( i,j ) .eq. SNOW .and. ctop_tc(1) .lt. -30.0 ) ) then  !! SNOW here is majority but not all                  
                  if( ncilays .eq. 2 ) then  
                      ifip_situ = 121          !! RESOLVED CONVECTION like 12
                  else
                      ifip_situ = 221          !! RESOLVED CONVECTION like 22                      
                  endif                                                      
               else if( ctop_tc(1) .ge. -12.0 ) then
                  if( ptyp_fipi( i,j ) .eq. RAAN .or. ptyp_fipi( i,j ) .eq. DZZL ) then
                     if( t2(i,j) .ge. CTOK-2.0 ) then                        
                        if( ncilays .eq. 2 ) then  
                            if( kconv .eq. 2 ) then
                               ifip_situ = 131    !! RESOLVED CONVECTION like 13
                            else if( kconv .ne. 1 ) then
                               WRITE( MESG, '( A, 2I8 )' ) 'KCONV neither 2 nor 1 for RESOLVED CONVECTION at i,j=',i,j
                               CALL wrf_error_fatal ( TRIM(MESG) )
                            else
                               ifip_situ = 91     !! RESOLVED CONVECTION like 9
                            endif
                        else
                            if( kconv .ge. 2 ) then
                               ifip_situ = 231    !! RESOLVED CONVECTION like 23
                            else
                               ifip_situ = 191    !! RESOLVED CONVECTION like 19
                            endif
                        endif
                     endif 
                  endif           
               endif
            endif         
            
            if( ifip_situ .eq. 0 ) then
                WRITE( MESG, '( A, 4I8 )' ) 'Deep resolved convection with ifip_situ=0 at: ',i,j,ifip_situ,ptyp_fipi( i,j )
                CALL wrf_message( TRIM(MESG) )
                CALL wrf_error_fatal ( 'debug' )
            endif
            
            if( cshl_cnvct .eq. 1 ) lresdshal = .true.  !! used for paranoia only         
         
         else if( ifip_situ .ne. 2 ) then
            WRITE( MESG, '( A, 4I8 )' ) 'Unexpected situation at: ',i,j,ifip_situ,ptyp_fipi( i,j )
            CALL wrf_error_fatal ( 'debug' )                     
         endif        !! end if parameterized convection; or not      
                  
222      CONTINUE  !! Target of branch if isolated convection, except if isolated shallow
                   !! is precipitating (in this case iso_shalr == TRUE).

         if( lresdcon .and. cshl_cnvct .eq. 1 ) then
             if( .not.lresdshal ) then             
                 WRITE( MESG, '( A, 5I8 )' ) 'Deep resolved and KF shallow at: ',i,j,ifip_situ,ptyp_fipi( i,j ),lresdtyp
                 CALL wrf_message( TRIM( MESG ) )
                 if( lresdtyp .eq. 1 ) then
                    WRITE( MESG, '( A, 5I8 )' ) 'kbot_cnvct,ktop_cnvct,kbot_cnvcr,ktop_cnvcr,mlt_deepr =',kbot_cnvct,ktop_cnvct,kbot_cnvcr,ktop_cnvcr,mlt_deepr
                    CALL wrf_message( TRIM( MESG ) )
                    WRITE( MESG, '( A, 2(F15.4,2X) )' ) 'ctop_cnvct,cbot_cnvct =',ctop_cnvct,cbot_cnvct
                    CALL wrf_message( TRIM( MESG ) )
                    WRITE( MESG, '( A, 2(F15.4,2X) )' ) 'ctta_cnvct, cdpt_cnvct =',ctta_cnvct,cdpt_cnvct
                    CALL wrf_message( TRIM( MESG ) )
                 else  !! there is resolved convection plus one or more additional layer clouds
                    WRITE( MESG, '( A,I8 )' ) 'Includes one or more additional layer clouds,ncilays=',ncilays
                    CALL wrf_message( TRIM( MESG ) )
                 endif
                 CALL wrf_error_fatal ( 'debug' )
             endif
          endif   

         !! Now determine appropriate bottom and top levels to use
         !! in computation
         
         if( ( ifip_situ .le. 13 ) .or. &
               ifip_situ .eq. 24 .or.  ifip_situ .eq. 26 ) then
                
             if( ifip_situ .eq. 2 ) then      !! isolated light or non-precipitating shallow param convection                
                 kbot = kbot_cnvct
                 ktop = ktop_cnvct
             else if( ifip_situ .eq. 3 ) then !! isolated deep convection            
                 if( iso_deepr ) then
                    kbot = 1
                    ktop = ktop_cnvct         
                 else
                    kbot = kbot_cnvct
                    ktop = ktop_cnvct          
                 endif             
             else if( ptyp_fipi( i,j ) .eq. NONE .or. &
                      ifip_situ .eq. 5 .or. ifip_situ .eq. 10 ) then
                 kbot=kcibas(1)
                 ktop=kcitop(1)             
             else
                 kbot=1
                 ktop=kcitop(1)
             endif
             
             if( ifip_situ .ge. 9 .and.  ifip_situ .le. 13 ) then
                if( mlt_deepr ) then          !! sufficient param precip to set base at ground
                   kbot_cnvct = 1             
                endif
             endif 
                          
         else if( (ifip_situ .ge. 91 .and. ifip_situ .le. 131 ) .or. &
                   ifip_situ .eq. 261 ) then !! Single-layer clouds plus resolved convection          
         
             if( kbot_cnvcr .eq. 0 .or. ktop_cnvcr .eq. 0 .or. ptyp_fipi( i,j ) .eq. NONE ) then             
                 WRITE( MESG, '( A, 2I8 )' ) 'kbot_cnvcr,ktop_cnvcr: ',kbot_cnvcr,ktop_cnvcr
                 CALL wrf_message (TRIM(mesg))
                 WRITE( MESG, '( A, 4I8 )' ) 'Resolved convection (DEBUG-1) at: ',i,j,ifip_situ,ptyp_fipi( i,j )
                 CALL wrf_error_fatal ( TRIM(MESG) )
             endif

             if( ifip_situ .eq. 101 ) then
                kbot_cnvct = kbot_cnvcr              
             else              
                kbot_cnvct = 1
             endif
             ktop_cnvct = ktop_cnvcr

             if( kconv .eq. 1 ) then
                kbot = kcibas(2)
                ktop = kcitop(2)
             else if( kconv .eq. 2 ) then
                kbot = kcibas(1)
                ktop = kcitop(1)              
             endif
              
         else if( (ifip_situ .ge. 14 .and. ifip_situ .le. 23) .or. &
                   ifip_situ .eq. 25 .or.  ifip_situ .eq. 27 ) then
             
             if( ptyp_fipi( i,j ) .eq. NONE .or. ifip_situ .eq. 15 .or. &
                                                 ifip_situ .eq. 20 ) then 
                kbot=kcibas(1)
                ktop=kcitop(ncilays)       
             else
                kbot=1
                ktop=kcitop(ncilays)                            
             endif                                                           
         
             if( ifip_situ .ge. 19 .and.  ifip_situ .le. 23 ) then
                if( mlt_deepr ) then
                   kbot_cnvct = 1             
                endif
             endif
                                       
         else if( (ifip_situ .ge. 191 .and. ifip_situ .le. 231 ) .or. &
                   ifip_situ .eq. 271 ) then !! Multiple-layer clouds plus resolved convection          
         
             if( kbot_cnvcr .eq. 0 .or. ktop_cnvcr .eq. 0 .or. ptyp_fipi( i,j ) .eq. NONE ) then             
                 WRITE( MESG, '( A, 2I8 )' ) 'kbot_cnvcr,ktop_cnvcr: ',kbot_cnvcr,ktop_cnvcr
                 CALL wrf_message (TRIM(mesg))
                 WRITE( MESG, '( A, 4I8 )' ) 'Resolved convection (DEBUG-1) at: ',i,j,ifip_situ,ptyp_fipi( i,j )
                 CALL wrf_error_fatal ( TRIM(MESG) )
             endif              

             if( ifip_situ .eq. 201 ) then
                kbot_cnvct = kbot_cnvcr              
             else              
                kbot_cnvct = 1
             endif
             ktop_cnvct = ktop_cnvcr              

             if( kconv .eq. 1 ) then
                kbot = kcibas(2)
                ktop = kcitop(ncilays)
             else if( kconv .ge. 2 ) then
                kbot = kcibas(1)
                ktop = kcitop(ncilays)              
             endif

             !!else if( ifip_situ .eq. 91 ) then
             !!   WRITE( MESG, '( A, 4I8 )' ) 'Resolved:91,kbot,ktop,kbot_cnvcr,ktop_cnvcr: ',kbot,ktop,kbot_cnvcr,ktop_cnvcr
             !!   CALL wrf_message (TRIM(mesg))      
             !!else if( ifip_situ .eq. 101 ) then          
             !!   WRITE( MESG, '( A, 4I8 )' ) 'Resolved:101,kbot,ktop,kbot_cnvcr,ktop_cnvcr: ',kbot,ktop,kbot_cnvcr,ktop_cnvcr         
             !!   CALL wrf_message (TRIM(mesg))
             !!else if( ifip_situ .eq. 111 ) then
             !!  WRITE( MESG, '( A, 4I8 )' ) 'Resolved:111,kbot,ktop,kbot_cnvcr,ktop_cnvcr: ',kbot,ktop,kbot_cnvcr,ktop_cnvcr         
             !!   CALL wrf_message (TRIM(mesg))  
             !!else if( ifip_situ .eq. 121 ) then
             !!   WRITE( MESG, '( A, 4I8 )' ) 'Resolved:121,kbot,ktop,kbot_cnvcr,ktop_cnvcr: ',kbot,ktop,kbot_cnvcr,ktop_cnvcr         
             !!   CALL wrf_message (TRIM(mesg))
             !!else if( ifip_situ .eq. 131 ) then          
             !!   WRITE( MESG, '( A, 4I8 )' ) 'Resolved:131,kbot,ktop,kbot_cnvcr,ktop_cnvcr: ',kbot,ktop,kbot_cnvcr,ktop_cnvcr         
             !!   CALL wrf_message (TRIM(mesg))
             !!else if( ifip_situ .eq. 261 ) then         
             !!   WRITE( MESG, '( A, 4I8 )' ) 'Resolved:261,kbot,ktop,kbot_cnvcr,ktop_cnvcr: ',kbot,ktop,kbot_cnvcr,ktop_cnvcr         
             !!   CALL wrf_message (TRIM(mesg))   
             !!else if( ifip_situ .eq. 191 ) then
             !!   WRITE( MESG, '( A, 4I8 )' ) 'Resolved:191,kbot,ktop,kbot_cnvcr,ktop_cnvcr: ',kbot,ktop,kbot_cnvcr,ktop_cnvcr         
             !!   CALL wrf_message (TRIM(mesg))
             !!else if( ifip_situ .eq. 201 ) then          
             !!   WRITE( MESG, '( A, 4I8 )' ) 'Resolved:201,kbot,ktop,kbot_cnvcr,ktop_cnvcr: ',kbot,ktop,kbot_cnvcr,ktop_cnvcr         
             !!   CALL wrf_message (TRIM(mesg)) 
             !!else if( ifip_situ .eq. 211 ) then         
             !!   WRITE( MESG, '( A, 4I8 )' ) 'Resolved:211,kbot,ktop,kbot_cnvcr,ktop_cnvcr: ',kbot,ktop,kbot_cnvcr,ktop_cnvcr         
             !!   CALL wrf_message (TRIM(mesg))
             !!else if( ifip_situ .eq. 221 ) then
             !!   WRITE( MESG, '( A, 4I8 )' ) 'Resolved:221,kbot,ktop,kbot_cnvcr,ktop_cnvcr: ',kbot,ktop,kbot_cnvcr,ktop_cnvcr         
             !!   CALL wrf_message (TRIM(mesg))
             !!else if( ifip_situ .eq. 231 ) then          
             !!   WRITE( MESG, '( A, 4I8 )' ) 'Resolved:231,kbot,ktop,kbot_cnvcr,ktop_cnvcr: ',kbot,ktop,kbot_cnvcr,ktop_cnvcr         
             !!   CALL wrf_message (TRIM(mesg))
             !!else if( ifip_situ .eq. 271 ) then         
             !!   WRITE( MESG, '( A, 4I8 )' ) 'Resolved:271,kbot,ktop,kbot_cnvcr,ktop_cnvcr: ',kbot,ktop,kbot_cnvcr,ktop_cnvcr         
             !!   CALL wrf_message (TRIM(mesg))
         
         endif

         if( kbot .eq. 0 .or. ktop .eq. 0 ) then
             WRITE( MESG, '( A, 4I8 )' ) 'DEBUG at: ',i,j,ifip_situ,ptyp_fipi( i,j )
             CALL wrf_message (TRIM(mesg))
             WRITE( MESG, '( A, 4I8 )' ) 'kbot,ktop,kbot_cnvcr,ktop_cnvcr: ',kbot,ktop,kbot_cnvcr,ktop_cnvcr
             CALL wrf_message (TRIM(mesg))
             CALL wrf_error_fatal ( TRIM(MESG) )
         endif
         
         if( ifip_situ .eq. 3 .or. ifip_situ .eq. 31 ) then    !! isolated deep param or resolved convection
            
            call isltd_deep( ldbg, kbot, ktop, rhk, dtc, vvubs, frc_convect, icg_pot1d, sld_pot1d, &
                             kds, kde, kms, kme, kts, kte )
         
         else if( ifip_situ .eq. 2 .or. (ifip_situ .ge. 4 .and. ifip_situ .le. 8) .or. &
                 ifip_situ .eq. 24 ) then !! Single-layer clouds or isolated shallow param convection
                        
            call single_layclds( ifip_situ, iso_shalr, ml, fl, kbot, ktop, rhk, dta, dtc, vvubs, &
                                 qclw, frc_convect, icg_pot1d, sld_pot1d, &
                                 kds, kde, kms, kme, kts, kte )            
         
         else if( (ifip_situ .ge. 14 .and. ifip_situ .le. 18) .or. &
                   ifip_situ .eq. 25 ) then  !! Multiple-layer clouds, no convection 
                  
            call multpl_layclds( ldbg, ifip_situ, ml, fl, kbot, ktop, ncilays, kcibas, kcitop, ctop_tc, &
                                 cbot_tc, rhk, dta, dtc, vvubs, qclw, frc_convect, icg_pot1d, sld_pot1d, &
                                 kds, kde, kms, kme, kts, kte )           
                                         
         else if( (ifip_situ .ge. 9 .and. ifip_situ .le. 13)   .or. &
                   ifip_situ .eq. 26                           .or. & 
                  (ifip_situ .ge. 91 .and. ifip_situ .le. 131) .or. &
                   ifip_situ .eq. 261 ) then   !! Single-layer clouds plus param. or resolved convection         
          
            call single_laycttg( ldbg, ifip_situ, ml, fl, kbot, ktop, kbot_cnvct, ktop_cnvct, rhk, dta, &
                                 dtc, vvubs, qclw, frc_convect, icg_pot1d, sld_pot1d, &
                                 kds, kde, kms, kme, kts, kte )
                                 
            kbot = MIN( kbot, kbot_cnvct)
            ktop = MAX( ktop, ktop_cnvct)                                                                
         
         else if( (ifip_situ .ge. 19 .and. ifip_situ .le. 23)   .or. &
                   ifip_situ .eq. 27                            .or. &
                  (ifip_situ .ge. 191 .and. ifip_situ .le. 231) .or. &
                   ifip_situ .eq. 271 ) then  !! Multiple-layer clouds plus param or resolved convection
         
           call multpl_laycttg( ldbg, ifip_situ, ml, fl, kbot, ktop, kbot_cnvct, ktop_cnvct, &
                                ncilays, kcibas, kcitop, ctop_tc, cbot_tc, rhk, dta, dtc, &
                                vvubs, qclw, frc_convect, icg_pot1d, sld_pot1d, &
                                kds, kde, kms, kme, kts, kte )
         
           kbot = MIN( kbot, kbot_cnvct)
           ktop = MAX( ktop, ktop_cnvct)             
                 
         endif 
                           
         !v36 call icing_types( ldbg, kbot, ktop, rhk, dta, dtc, vvubs, qrlw, icg_pot1d, sld_pot1d, ictyp_clr1d, ictyp_mxd1d, ictyp_rim1d, &
         !v36                    kds, kde, kms, kme, kts, kte )    
                  
         !! Populate 3D icing-potential fields
         
         do k = kbot,ktop
            icg_pot3d(i,k,j) = icg_pot1d(k)
            sld_pot3d(i,k,j) = sld_pot1d(k)
            !v36 ictyp_clr3d(i,k,j) = ictyp_clr1d(k)
            !v36 ictyp_mxd3d(i,k,j) = ictyp_mxd1d(k)
            !v36 ictyp_rim3d(i,k,j) = ictyp_rim1d(k)
            !if( .not.firstime .and. ldbg ) then
            !   print*,'At k,dta,icg_pot3d=',k,dta(k),icg_pot3d(i,k,j)
            !endif             
         end do
         
         !if( .not.firstime .and. ldbg ) then
         !   print*,'kbot,ktop=',kbot,ktop
         !   print*,'i,j,ifip_situ=',i,j,ifip_situ
         !endif        

         !! End of Forecast Icing Potential (FIP); Super-cooled liquid (SCL) and super-cooled large droplet (SLD)
         !! calculations

225      CONTINUE    !! If we branch to here, "clear-sky" (i.e. low enough rh) is detected through the column
                     !! with essentially no surface precip.
         
         do k = 1,kte-1
            if( icg_pot3d(i,k,j) .gt. 1.0 ) then
               WRITE( MESG, '( A, 4I8, 2X, F15.9 )' ) 'Too large icg_pot3d at i,j,k,ifip_situ=',i,k,j,ifip_situ,icg_pot3d(i,k,j)       
               CALL wrf_message(MESG)
               icg_pot3d(i,k,j) = 1.0
               !! CALL wrf_error_fatal ( TRIM(MESG) )             
            else if ( icg_pot3d(i,k,j) .lt. 0.0 .and. icg_pot3d(i,k,j) .gt. -999990.0 ) then
               WRITE( MESG, '( A, 4I8, 2X, F15.9 )' ) 'Negative icg_pot3d at i,j,k,ifip_situ =',i,k,j,ifip_situ,icg_pot3d(i,k,j)            
               CALL wrf_message(MESG)            
               icg_pot3d(i,k,j) = 0.0
               !! CALL wrf_error_fatal ( TRIM(MESG) )
            endif                 

            if( sld_pot3d(i,k,j) .gt. 1.0 ) then
               WRITE( MESG, '( A, 4I8, 2X, F15.9 )' ) 'Too large sld_pot3d at i,j,k,ifip_situ =',i,k,j,ifip_situ,sld_pot3d(i,k,j)
               CALL wrf_message(MESG)
               sld_pot3d(i,k,j) = 1.0
               !! CALL wrf_error_fatal ( TRIM(MESG) )             
            else if ( sld_pot3d(i,k,j) .lt. 0.0 .and. sld_pot3d(i,k,j) .gt. -999990.0 ) then
               WRITE( MESG, '( A, 4I8, 2X, F15.9 )' ) 'Negative sld_pot3d at i,j,k,ifip_situ =',i,k,j,ifip_situ,sld_pot3d(i,k,j)          
               CALL wrf_message(MESG)
               sld_pot3d(i,k,j) = 0.0
               !! CALL wrf_error_fatal ( TRIM(MESG) )                  
            endif
         end do

         do kl = 1,FIPLEVS
           
           !if( .not.firstime .and. ldbg ) then
           !   print*,'Attempting interp: kl,kflfnd(kl)=',kl,kflfnd(kl)
           !endif            
            
           if( kflfnd(kl) ) then                
               kdd  = kfldd(kl)
               kuu  = kfluu(kl)
               facd = facfld(kl)
               facu = facflu(kl)
               facd_big = ( facd .gt. facu )        
               if( sld_pot3d(i,kdd,j) .lt. 0.0 .and. sld_pot3d(i,kuu,j) .lt. 0.0 ) then
                  sld_potlv(i,kl,j) = -.01  !! Unknown
               else if( sld_pot3d(i,kdd,j) .ge. 0.0 .and. sld_pot3d(i,kuu,j) .lt. 0.0 ) then
                 if( facd_big ) then
                     if( sld_pot3d(i,kdd,j) .gt. 0.0 ) then     !! strictly greater than 0.0
                        sld_potlv(i,kl,j) = sld_pot3d(i,kdd,j)
                     else
                        sld_potlv(i,kl,j) = -.01  !! Unknown
                     endif        
                  else
                     sld_potlv(i,kl,j) = -.01  !! Unknown                 
                  endif
               else if( sld_pot3d(i,kdd,j) .lt. 0.0 .and. sld_pot3d(i,kuu,j) .ge. 0.0 ) then
                  if( facd_big ) then
                     sld_potlv(i,kl,j) = -.01  !! Unknown                   
                  else
                     if( sld_pot3d(i,kuu,j) .gt. 0.0 ) then    !! strictly greater than 0.0                      
                        sld_potlv(i,kl,j) = sld_pot3d(i,kuu,j)
                     else
                        sld_potlv(i,kl,j) = -.01  !! Unknown
                     endif                          
                  endif                
               else
                 sld_potlv(i,kl,j) = sld_pot3d(i,kdd,j)*facd + sld_pot3d(i,kuu,j)*facu
               endif
               icg_potlv(i,kl,j) = icg_pot3d(i,kdd,j)*facd + icg_pot3d(i,kuu,j)*facu                   
               !v36 icg_clrlv(i,kl,j) = ictyp_clr3d(i,kdd,j)*facd + ictyp_clr3d(i,kuu,j)*facu
               !v36 icg_mxdlv(i,kl,j) = ictyp_mxd3d(i,kdd,j)*facd + ictyp_mxd3d(i,kuu,j)*facu
               !v36 icg_rimlv(i,kl,j) = ictyp_rim3d(i,kdd,j)*facd + ictyp_rim3d(i,kuu,j)*facu
               !if( .not.firstime .and. ldbg ) then
               !    print*,'kl,kdd,kuu=',kl,kdd,kuu
               !    print*,'icg-kdd-kuu-kl=',icg_pot3d(i,kdd,j),icg_pot3d(i,kuu,j),icg_potlv(i,kl,j)
               !    print*,'----------------'                    
               !endif          
           endif
         end do    

         !if( .not.firstime .and. ldbg ) then
         !   print*,'At debug i,j=',i,j
         !endif

         !! End aviation calculations             

         catsum = cat_rain(i,j) + cat_snow(i,j) + cat_icep(i,j) + cat_fzrn(i,j)
         if( catsum .le. 0.5 ) then
            if( (snl(i,j)-terht(i,j)) .le. 100.0 ) then !! DTO: Allow a 100m "melting" zone
               cat_snow(i,j) = 1.0
            else if( (mxl(i,j)-terht(i,j)) .gt. 0.0  ) then
               cat_rain(i,j) = 1.0
            else
               if( t2(i,j) .le. CTOK ) then                
                  dzh = ght_pl(i,p850,j)-ght_pl(i,p1000,j)                      
                  if( dzh .ge. 1290.0 ) then                
                     cat_fzrn(i,j) = 1.0
                  else                
                     cat_icep(i,j) = 1.0                
                  endif
               else
                  cat_icep(i,j) = 1.0
               endif                                  
            endif         
         endif

      END DO i_loop
   END DO j_loop

   DO j = jts , MIN(jte,jde-1)
      DO kp = 1 , num_press_levels
         DO i = its , MIN(ite,ide-1)
            if( ght_pl(i,kp,j) .lt. -999990.0 ) then
               WRITE( MESG, '( A, 3I8 )' ) 'Bad extrapolation for ght_pl at i,kp,j=',i,kp,j
               CALL wrf_error_fatal ( TRIM(MESG) )
            else if( t_pl(i,kp,j) .lt. 0.0 ) then
               WRITE( MESG, '( A, 3I8 )' ) 'Bad extrapolation for t_pl at i,kp,j=',i,kp,j
               CALL wrf_error_fatal ( TRIM(MESG) )            
            else if( thta_pl(i,kp,j) .lt. 0.0 ) then
               WRITE( MESG, '( A, 3I8 )' ) 'Bad extrapolation for thta_pl at i,kp,j=',i,kp,j
               CALL wrf_error_fatal ( TRIM(MESG) )
            endif
            if(  u_pl(i,kp,j)   .lt. -999990.0 ) u_pl(i,kp,j)= u(i,1,j)
            if(  v_pl(i,kp,j)   .lt. -999990.0 ) v_pl(i,kp,j)= v(i,1,j)                         
            if( avor_pl(i,kp,j) .lt. -999990.0 ) avor_pl(i,kp,j) = relvort(i,1,j) + f(i,j) 
            if( rh_pl(i,kp,j)   .lt. -999990.0 ) rh_pl(i,kp,j) = rh1(i,j)
            if(  s_pl(i,kp,j)   .lt. -999990.0 ) s_pl(i,kp,j) = sp1(i,j)
            if( wdir_pl(i,kp,j) .lt. -999990.0 ) then
               uearth = u(i,1,j)*cosalpha(i,j)+v(i,1,j)*sinalpha(i,j)
               vearth = v(i,1,j)*cosalpha(i,j)+u(i,1,j)*sinalpha(i,j)
               wdir = atan2(-uearth,-vearth)*180./3.14159
               if( wdir .lt. 0.0 ) wdir = wdir + 360.
               wdir_pl(i,kp,j) = wdir            
            endif
            if( wwnd_pl(i,kp,j)   .lt. -999990.0 ) wwnd_pl(i,kp,j)=0.0
            if(  q_pl(i,kp,j)     .lt. -999990.0 ) q_pl(i,kp,j)= MAX(qv(i,1,j),1.0E-25) !! CAWO
            if( td_pl(i,kp,j)     .lt. -999990.0 ) td_pl(i,kp,j)= td1(i,j)              !! CAWO
            if( pvor_pl(i,kp,j)   .lt. -999990.0 ) pvor_pl(i,kp,j) = potvort(i,1,j)     !! CAWO 
            if( cconfr_pl(i,kp,j) .lt. -999990.0 ) cconfr_pl(i,kp,j)=ccondfrc1(i,j)     !! CAWO            
            if( rnwtfr_pl(i,kp,j) .lt. -999990.0 ) rnwtfr_pl(i,kp,j)=rainfrc1(i,j)      !! CAWO            
            if( snowfr_pl(i,kp,j) .lt. -999990.0 ) snowfr_pl(i,kp,j)=snowfrc1(i,j)      !! CAWO            
            if( graufr_pl(i,kp,j) .lt. -999990.0 ) graufr_pl(i,kp,j)=icefrc1(i,j)       !! CAWO
         END DO
      END DO
   END DO

   !! Now have all pressure level data "filled in" -- when needed using
   !! layer-1 values for pressure surfaces "below ground" -- e.g. for
   !! wind components, rh, and speed.

   !! Finally compute vorticity at pressure levels     

   !! Example indexing here shows no domain decomp and no OMP (full grid only)
   !! For pedagogical purposes only

   !! PRINT*,'its, ite= ',its, ite         !!  1         132 OMP Tile                        
   !! PRINT*,'jts ,jte= ',jts ,jte         !!  1          96 OMP Tile                        
   !! PRINT*,'ims, ime= ',ims, ime         !! -3         136 MPI Patch including halo (patch dimension)      
   !! PRINT*,'jms ,jme= ',jms ,jme         !! -3         100 MPI Patch including halo (patch dimension)         
   !! PRINT*,'ids, ide= ',ids, ide         !!  1         132 domain (staggered)                          
   !! PRINT*,'jds ,jde= ',jds ,jde         !!  1          96 domain (staggered)

   DO j = jts , MIN(jte,jde-1)
      DO i = its , MIN(ite,ide-1)
          twet1(i,j)      = tw1(i,j)              !! Layer-1 wet-bulb air temperature
          twmean(i,j)     = twm( i,j )            !! Integrated near-surface wet-bulb air temperature
          maskfrac(i,j)   = lfr( i,j )            !! Precip frac: freezing <= 0 < mixed < 1 <= liquid
          qclay1(i,j)     = qc_curr(i,1,j) 
          qilay1(i,j)     = qi_curr(i,1,j)  
          qrlay1(i,j)     = qr_curr(i,1,j)
          qslay1(i,j)     = qs_curr(i,1,j)  
          qglay1(i,j)     = qg_curr(i,1,j)
          qnilay1(i,j)    = qs_curr(i,1,j) + qg_curr(i,1,j)
          rain(i,j)       = rain_last(i,j)   
          qv_col(i,j)     = qcol(i,j)
          talay1(i,j)     = t1(i,j)               !! layer-1 half-sigma air temperature
          tnsfc (i,j)     = tns( i,j )            !! mean near-sfc half-sigma air temperature 
          ta2(i,j)        = t2(i,j)
          apta2(i,j)      = aptemp(i,j)
          tdew2(i,j)      = td2(i,j)              !! 2-Meter Dew Point
          rhum2(i,j)      = rh2(i,j)
          tdsig1(i,j)     = td1(i,j)              !! Layer-1 Dew Point    
          ux10(i,j)       = u10(i,j)   
          vx10(i,j)       = v10(i,j)
          !! wspd10(i,j)     = sqrt(u10(i,j)*u10(i,j)+v10(i,j)*v10(i,j)) !! Now loaded above    
          slprs(i,j)      = slprs(i,j)*0.01       !! converting Pa to mb   
          cfraclo(i,j)    = cfracl(i,j)
          cfracmd(i,j)    = cfracm(i,j)  
          cfrachi(i,j)    = cfrach(i,j) 
          z1000(i,j)      = ght_pl(i,p1000,j)     
          snolev(i,j)     = snl( i,j )            !! Elevation above mean sea level of snow level 
          mixlev(i,j)     = mxl( i,j )            !! Elevation above mean sea level at mixed level       
          frzlev(i,j)     = frl( i,j )            !! Elevation above mean sea level at freezing level          
          z850(i,j)       = ght_pl(i,p850,j)          
          uwind850(i,j)   =   u_pl(i,p850,j)  
          vwind850(i,j)   =   v_pl(i,p850,j) 
          wspd850(i,j)    = s_pl(i,p850,j)          
          wdir850(i,j)    = wdir_pl(i,p850,j)          
          rh850(i,j)      =  rh_pl(i,p850,j)     
          ta850(i,j)      =   t_pl(i,p850,j)               
          z700(i,j)       = ght_pl(i,p700,j)           
          uwind700(i,j)   =   u_pl(i,p700,j)   
          vwind700(i,j)   =   v_pl(i,p700,j)
          wwind700(i,j)   = wwnd_pl(i,p700,j)*100. !! Convert from m/s to cm/s  
          wspd700(i,j)    = s_pl(i,p700,j)          
          wdir700(i,j)    = wdir_pl(i,p700,j)          
          rh700(i,j)      =  rh_pl(i,p700,j)     
          ta700(i,j)      =   t_pl(i,p700,j)
          thtae700(i,j)   = thtae_pl(i,p700,j)                    
          z500(i,j)       = ght_pl(i,p500,j)               
          uwind500(i,j)   =   u_pl(i,p500,j)
          vwind500(i,j)   =   v_pl(i,p500,j)   
          wspd500(i,j)    = s_pl(i,p500,j)
          wdir500(i,j)    = wdir_pl(i,p500,j)
          rh500(i,j)      =  rh_pl(i,p500,j)     
          ta500(i,j)      =   t_pl(i,p500,j)          
          z300(i,j)       = ght_pl(i,p300,j)
          uwind300(i,j)   = u_pl(i,p300,j)
          vwind300(i,j)   = v_pl(i,p300,j)
          wspd300(i,j)    = s_pl(i,p300,j)
          wdir300(i,j)    = wdir_pl(i,p300,j)
          rh300(i,j)      = rh_pl(i,p300,j)
          ta300(i,j)      =  t_pl(i,p300,j)                    
          z250(i,j)       = ght_pl(i,p250,j)                                                      
          uwind250(i,j)   =   u_pl(i,p250,j)  
          vwind250(i,j)   =   v_pl(i,p250,j)  
          wspd250(i,j)    = s_pl(i,p250,j)
          wdir250(i,j)    = wdir_pl(i,p250,j)                    
          rh250(i,j)      =  rh_pl(i,p250,j)     
          ta250(i,j)      =   t_pl(i,p250,j)                         
          dz1000_850(i,j) = z850(i,j) - z1000(i,j) !! Thickness btwn 1000mb and 850mb sfcs
          dz1000_500(i,j) = z500(i,j) - z1000(i,j) !! Thickness btwn 1000mb and 500mb sfcs
          dz850_700(i,j)  = z700(i,j) - z850(i,j)
          avor850(i,j)    = avor_pl(i,p850,j)
          avor700(i,j)    = avor_pl(i,p700,j)
          avor500(i,j)    = avor_pl(i,p500,j)
          avor250(i,j)    = avor_pl(i,p250,j)
          snodpt(i,j)     = 1000.*snowh(i,j)       !! convert to mm
                                                   !! reflectivities now loaded above
          cfractot(i,j)   = MAX( cfracl(i,j), cfracm(i,j), cfrach(i,j) )
          mucape(i,j)     = mucap(i,j)
          cinh(i,j)       = -mucin(i,j)
          mlcape(i,j)     = mlcap(i,j)
          cinm(i,j)       = -mlcin(i,j) 
          sbcape(i,j)     = sbcap(i,j)
          cins(i,j)       = -sbcin(i,j)
          srh0_1km(i,j)   = MAX( 0.0, srh01(i,j) ) 
          srh0_3km(i,j)   = MAX( 0.0, srh03(i,j) )  
          ehi_1km(i,j)    = MAX( 0.0, ehi01(i,j) )
          ehi_3km(i,j)    = MAX( 0.0, ehi03(i,j) )
          !! visib(i,j)      = MIN( 20.,vis(i,j) )
          visib(i,j)      = vis(i,j)
          ceiling(i,j)    = clg(i,j)
          flghtreg(i,j)   = freg(i,j)
          ptyp_fip(i,j)   = FLOAT( ptyp_fipi(i,j) )
          ptyp_cat(i,j)   = FLOAT( ptyp_cati(i,j) )                                         
          icgpot35(i,j)   = icg_potlv(i,35,j)          
          sldpot35(i,j)   = sld_potlv(i,35,j)   
          icgpot30(i,j)   = icg_potlv(i,30,j)          
          sldpot30(i,j)   = sld_potlv(i,30,j)         
          icgpot25(i,j)   = icg_potlv(i,25,j)          
          sldpot25(i,j)   = sld_potlv(i,25,j)          
          icgpot20(i,j)   = icg_potlv(i,20,j)          
          sldpot20(i,j)   = sld_potlv(i,20,j)              
          icgpot15(i,j)   = icg_potlv(i,15,j)          
          sldpot15(i,j)   = sld_potlv(i,15,j)          
          icgpot10(i,j)   = icg_potlv(i,10,j)          
          sldpot10(i,j)   = sld_potlv(i,10,j)     
          icgpot05(i,j)   = icg_potlv(i,5,j)          
          sldpot05(i,j)   = sld_potlv(i,5,j)
          icgpot01(i,j)   = icg_potlv(i,1,j)          
          sldpot01(i,j)   = sld_potlv(i,1,j)
          turbdx01(i,j)   = afw_turbdex(i,1,j)
          turbdx02(i,j)   = afw_turbdex(i,2,j)
          turbdx03(i,j)   = afw_turbdex(i,3,j)
          turbdx04(i,j)   = afw_turbdex(i,4,j)
          turbdx05(i,j)   = afw_turbdex(i,5,j)
          turbdx06(i,j)   = afw_turbdex(i,6,j)
          turbdx07(i,j)   = afw_turbdex(i,7,j)
          llturb(i,j)     = afw_llturb(i,j)
          llturblgt(i,j)  = afw_llturblgt(i,j)
          llturbmdt(i,j)  = afw_llturbmdt(i,j)
          llturbsvr(i,j)  = afw_llturbsvr(i,j)
          fits(i,j)       = afw_fits(i,j)
          llws(i,j)       = afw_llws(i,j)          
          uphelmax(i,j)   = uphlmx(i,j)
          wspd10mx(i,j)   = wspdmx10(i,j)
          afw_hail(i,j)   = hail_afwa(i,j)
          afw_torn(i,j)   = torn_afwa(i,j)
          icgpot45(i,j)   = icg_potlv(i,45,j)          
          sldpot45(i,j)   = sld_potlv(i,45,j)   
          icgpot40(i,j)   = icg_potlv(i,40,j)          
          sldpot40(i,j)   = sld_potlv(i,40,j)                             
!! CAWO
          ta050(i,j)      = t_pl(i,p050,j)  
          ta100(i,j)      = t_pl(i,p100,j) 
          ta150(i,j)      = t_pl(i,p150,j) 
          ta200(i,j)      = t_pl(i,p200,j) 
          ta350(i,j)      = t_pl(i,p350,j) 
          ta400(i,j)      = t_pl(i,p_400,j) 
          ta450(i,j)      = t_pl(i,p450,j) 
          ta550(i,j)      = t_pl(i,p550,j) 
          ta600(i,j)      = t_pl(i,p600,j) 
          ta650(i,j)      = t_pl(i,p650,j) 
          ta750(i,j)      = t_pl(i,p750,j) 
          ta800(i,j)      = t_pl(i,p800,j) 
          ta900(i,j)      = t_pl(i,p900,j)
          ta950(i,j)      = t_pl(i,p950,j) 
          ta1000(i,j)     = t_pl(i,p1000,j)
          td050(i,j)      = td_pl(i,p050,j)
          td100(i,j)      = td_pl(i,p100,j)
          td150(i,j)      = td_pl(i,p150,j)
          td200(i,j)      = td_pl(i,p200,j)
          td250(i,j)      = td_pl(i,p250,j)
          td300(i,j)      = td_pl(i,p300,j)
          td350(i,j)      = td_pl(i,p350,j)
          td400(i,j)      = td_pl(i,p_400,j)
          td450(i,j)      = td_pl(i,p450,j)
          td500(i,j)      = td_pl(i,p500,j)
          td550(i,j)      = td_pl(i,p550,j)
          td600(i,j)      = td_pl(i,p600,j)
          td650(i,j)      = td_pl(i,p650,j)
          td700(i,j)      = td_pl(i,p700,j)
          td750(i,j)      = td_pl(i,p750,j)
          td800(i,j)      = td_pl(i,p800,j)
          td850(i,j)      = td_pl(i,p850,j)
          td900(i,j)      = td_pl(i,p900,j)
          td950(i,j)      = td_pl(i,p950,j)
          td1000(i,j)     = td_pl(i,p1000,j) 
          rh050(i,j)      = rh_pl(i,p050,j) 
          rh100(i,j)      = rh_pl(i,p100,j) 
          rh150(i,j)      = rh_pl(i,p150,j) 
          rh200(i,j)      = rh_pl(i,p200,j) 
          rh350(i,j)      = rh_pl(i,p350,j) 
          rh400(i,j)      = rh_pl(i,p_400,j) 
          rh450(i,j)      = rh_pl(i,p450,j) 
          rh550(i,j)      = rh_pl(i,p550,j) 
          rh600(i,j)      = rh_pl(i,p600,j) 
          rh650(i,j)      = rh_pl(i,p650,j) 
          rh750(i,j)      = rh_pl(i,p750,j) 
          rh800(i,j)      = rh_pl(i,p800,j) 
          rh900(i,j)      = rh_pl(i,p900,j)
          rh950(i,j)      = rh_pl(i,p950,j) 
          rh1000(i,j)     = rh_pl(i,p1000,j)
          qv050(i,j)      = q_pl(i,p050,j)
          qv100(i,j)      = q_pl(i,p100,j)
          qv150(i,j)      = q_pl(i,p150,j)
          qv200(i,j)      = q_pl(i,p200,j)
          qv250(i,j)      = q_pl(i,p250,j)
          qv300(i,j)      = q_pl(i,p300,j)
          qv350(i,j)      = q_pl(i,p350,j)
          qv400(i,j)      = q_pl(i,p_400,j)
          qv450(i,j)      = q_pl(i,p450,j)
          qv500(i,j)      = q_pl(i,p500,j)
          qv550(i,j)      = q_pl(i,p550,j)
          qv600(i,j)      = q_pl(i,p600,j)
          qv650(i,j)      = q_pl(i,p650,j)
          qv700(i,j)      = q_pl(i,p700,j)
          qv750(i,j)      = q_pl(i,p750,j)
          qv800(i,j)      = q_pl(i,p800,j)
          qv850(i,j)      = q_pl(i,p850,j)
          qv900(i,j)      = q_pl(i,p900,j)
          qv950(i,j)      = q_pl(i,p950,j)
          qv1000(i,j)     = q_pl(i,p1000,j)
          wspd050(i,j)    = s_pl(i,p050,j) 
          wspd100(i,j)    = s_pl(i,p100,j) 
          wspd150(i,j)    = s_pl(i,p150,j) 
          wspd200(i,j)    = s_pl(i,p200,j) 
          wspd350(i,j)    = s_pl(i,p350,j) 
          wspd400(i,j)    = s_pl(i,p_400,j) 
          wspd450(i,j)    = s_pl(i,p450,j)
          wspd550(i,j)    = s_pl(i,p550,j) 
          wspd600(i,j)    = s_pl(i,p600,j) 
          wspd650(i,j)    = s_pl(i,p650,j) 
          wspd750(i,j)    = s_pl(i,p750,j) 
          wspd800(i,j)    = s_pl(i,p800,j)
          wspd900(i,j)    = s_pl(i,p900,j)
          wspd950(i,j)    = s_pl(i,p950,j)
          wspd1000(i,j)   = s_pl(i,p1000,j)
          w_dir050(i,j)   = wdir_pl(i,p050,j) 
          w_dir100(i,j)   = wdir_pl(i,p100,j) 
          w_dir150(i,j)   = wdir_pl(i,p150,j) 
          w_dir200(i,j)   = wdir_pl(i,p200,j) 
          w_dir350(i,j)   = wdir_pl(i,p350,j) 
          w_dir400(i,j)   = wdir_pl(i,p_400,j) 
          w_dir450(i,j)   = wdir_pl(i,p450,j) 
          w_dir500(i,j)   = wdir_pl(i,p500,j) 
          w_dir550(i,j)   = wdir_pl(i,p550,j) 
          w_dir600(i,j)   = wdir_pl(i,p600,j) 
          w_dir650(i,j)   = wdir_pl(i,p650,j) 
          w_dir750(i,j)   = wdir_pl(i,p750,j) 
          w_dir800(i,j)   = wdir_pl(i,p800,j)
          w_dir850(i,j)   = wdir_pl(i,p850,j) 
          w_dir900(i,j)   = wdir_pl(i,p900,j)
          w_dir950(i,j)   = wdir_pl(i,p950,j)
          w_dir1000(i,j)  = wdir_pl(i,p1000,j)
          theta050(i,j)   = thta_pl(i,p050,j)  
          theta100(i,j)   = thta_pl(i,p100,j)  
          theta150(i,j)   = thta_pl(i,p150,j)  
          theta200(i,j)   = thta_pl(i,p200,j)  
          theta250(i,j)   = thta_pl(i,p250,j)  
          theta300(i,j)   = thta_pl(i,p300,j)  
          theta350(i,j)   = thta_pl(i,p350,j)  
          theta400(i,j)   = thta_pl(i,p_400,j)  
          theta450(i,j)   = thta_pl(i,p450,j)  
          theta500(i,j)   = thta_pl(i,p500,j)  
          theta550(i,j)   = thta_pl(i,p550,j)  
          theta600(i,j)   = thta_pl(i,p600,j)  
          theta650(i,j)   = thta_pl(i,p650,j)  
          theta700(i,j)   = thta_pl(i,p700,j)  
          theta750(i,j)   = thta_pl(i,p750,j)  
          theta800(i,j)   = thta_pl(i,p800,j)  
          theta850(i,j)   = thta_pl(i,p850,j)  
          theta900(i,j)   = thta_pl(i,p900,j)  
          theta950(i,j)   = thta_pl(i,p950,j)  
          theta1000(i,j)  = thta_pl(i,p1000,j)
          z050(i,j)       = ght_pl(i,p050,j)
          z100(i,j)       = ght_pl(i,p100,j)
          z150(i,j)       = ght_pl(i,p150,j)
          z200(i,j)       = ght_pl(i,p200,j)
          z350(i,j)       = ght_pl(i,p350,j)
          z400(i,j)       = ght_pl(i,p_400,j)
          z450(i,j)       = ght_pl(i,p450,j)
          z550(i,j)       = ght_pl(i,p550,j)
          z600(i,j)       = ght_pl(i,p600,j)
          z650(i,j)       = ght_pl(i,p650,j)
          z750(i,j)       = ght_pl(i,p750,j)
          z800(i,j)       = ght_pl(i,p800,j)
          z900(i,j)       = ght_pl(i,p900,j)
          z950(i,j)       = ght_pl(i,p950,j)
          wwind050(i,j)   = wwnd_pl(i,p050,j)*100. !! Convert from m/s to cm/s   
          wwind100(i,j)   = wwnd_pl(i,p100,j)*100. !! Convert from m/s to cm/s   
          wwind150(i,j)   = wwnd_pl(i,p150,j)*100. !! Convert from m/s to cm/s   
          wwind200(i,j)   = wwnd_pl(i,p200,j)*100. !! Convert from m/s to cm/s   
          wwind250(i,j)   = wwnd_pl(i,p200,j)*100. !! Convert from m/s to cm/s  
          wwind300(i,j)   = wwnd_pl(i,p200,j)*100. !! Convert from m/s to cm/s  
          wwind350(i,j)   = wwnd_pl(i,p350,j)*100. !! Convert from m/s to cm/s   
          wwind400(i,j)   = wwnd_pl(i,p_400,j)*100. !! Convert from m/s to cm/s   
          wwind450(i,j)   = wwnd_pl(i,p450,j)*100. !! Convert from m/s to cm/s   
          wwind500(i,j)   = wwnd_pl(i,p500,j)*100. !! Convert from m/s to cm/s   
          wwind550(i,j)   = wwnd_pl(i,p550,j)*100. !! Convert from m/s to cm/s   
          wwind600(i,j)   = wwnd_pl(i,p600,j)*100. !! Convert from m/s to cm/s   
          wwind650(i,j)   = wwnd_pl(i,p650,j)*100. !! Convert from m/s to cm/s   
          wwind750(i,j)   = wwnd_pl(i,p750,j)*100. !! Convert from m/s to cm/s   
          wwind800(i,j)   = wwnd_pl(i,p800,j)*100. !! Convert from m/s to cm/s   
          wwind850(i,j)   = wwnd_pl(i,p850,j)*100. !! Convert from m/s to cm/s   
          wwind900(i,j)   = wwnd_pl(i,p900,j)*100. !! Convert from m/s to cm/s   
          wwind950(i,j)   = wwnd_pl(i,p950,j)*100. !! Convert from m/s to cm/s   
          wwind1000(i,j)  = wwnd_pl(i,p1000,j)*100. !! Convert from m/s to cm/s
          clconfrc050(i,j) =  cconfr_pl(i,p050,j)  
          clconfrc100(i,j) =  cconfr_pl(i,p100,j)  
          clconfrc150(i,j) =  cconfr_pl(i,p150,j)  
          clconfrc200(i,j) =  cconfr_pl(i,p200,j)  
          clconfrc250(i,j) =  cconfr_pl(i,p250,j)  
          clconfrc300(i,j) =  cconfr_pl(i,p300,j)  
          clconfrc350(i,j) =  cconfr_pl(i,p350,j)  
          clconfrc400(i,j) =  cconfr_pl(i,p_400,j)  
          clconfrc450(i,j) =  cconfr_pl(i,p450,j)  
          clconfrc500(i,j) =  cconfr_pl(i,p500,j)  
          clconfrc550(i,j) =  cconfr_pl(i,p550,j)  
          clconfrc600(i,j) =  cconfr_pl(i,p600,j)  
          clconfrc650(i,j) =  cconfr_pl(i,p650,j)  
          clconfrc700(i,j) =  cconfr_pl(i,p700,j)  
          clconfrc750(i,j) =  cconfr_pl(i,p750,j)  
          clconfrc800(i,j) =  cconfr_pl(i,p800,j)  
          clconfrc850(i,j) =  cconfr_pl(i,p850,j)  
          clconfrc900(i,j) =  cconfr_pl(i,p900,j)  
          clconfrc950(i,j) =  cconfr_pl(i,p950,j)  
          clconfrc1000(i,j)=  cconfr_pl(i,p1000,j)
          rnwatfrc050(i,j) = rnwtfr_pl(i,p050,j)   
          rnwatfrc100(i,j) = rnwtfr_pl(i,p100,j)   
          rnwatfrc150(i,j) = rnwtfr_pl(i,p150,j)   
          rnwatfrc200(i,j) = rnwtfr_pl(i,p200,j)   
          rnwatfrc250(i,j) = rnwtfr_pl(i,p250,j)   
          rnwatfrc300(i,j) = rnwtfr_pl(i,p300,j)   
          rnwatfrc350(i,j) = rnwtfr_pl(i,p350,j)   
          rnwatfrc400(i,j) = rnwtfr_pl(i,p_400,j)   
          rnwatfrc450(i,j) = rnwtfr_pl(i,p450,j)   
          rnwatfrc500(i,j) = rnwtfr_pl(i,p500,j)   
          rnwatfrc550(i,j) = rnwtfr_pl(i,p550,j)   
          rnwatfrc600(i,j) = rnwtfr_pl(i,p600,j)   
          rnwatfrc650(i,j) = rnwtfr_pl(i,p650,j)   
          rnwatfrc700(i,j) = rnwtfr_pl(i,p700,j)   
          rnwatfrc750(i,j) = rnwtfr_pl(i,p750,j)   
          rnwatfrc800(i,j) = rnwtfr_pl(i,p800,j)   
          rnwatfrc850(i,j) = rnwtfr_pl(i,p850,j)   
          rnwatfrc900(i,j) = rnwtfr_pl(i,p900,j)   
          rnwatfrc950(i,j) = rnwtfr_pl(i,p950,j)   
          rnwatfrc1000(i,j)= rnwtfr_pl(i,p1000,j)
          snowfrc050(i,j) = snowfr_pl(i,p050,j)      
          snowfrc100(i,j) = snowfr_pl(i,p100,j)      
          snowfrc150(i,j) = snowfr_pl(i,p150,j)      
          snowfrc200(i,j) = snowfr_pl(i,p200,j)      
          snowfrc250(i,j) = snowfr_pl(i,p250,j)      
          snowfrc300(i,j) = snowfr_pl(i,p300,j)      
          snowfrc350(i,j) = snowfr_pl(i,p350,j)      
          snowfrc400(i,j) = snowfr_pl(i,p_400,j)      
          snowfrc450(i,j) = snowfr_pl(i,p450,j)      
          snowfrc500(i,j) = snowfr_pl(i,p500,j)      
          snowfrc550(i,j) = snowfr_pl(i,p550,j)      
          snowfrc600(i,j) = snowfr_pl(i,p600,j)      
          snowfrc650(i,j) = snowfr_pl(i,p650,j)      
          snowfrc700(i,j) = snowfr_pl(i,p700,j)      
          snowfrc750(i,j) = snowfr_pl(i,p750,j)      
          snowfrc800(i,j) = snowfr_pl(i,p800,j)      
          snowfrc850(i,j) = snowfr_pl(i,p850,j)      
          snowfrc900(i,j) = snowfr_pl(i,p900,j)      
          snowfrc950(i,j) = snowfr_pl(i,p950,j)      
          snowfrc1000(i,j)= snowfr_pl(i,p1000,j)
          grplfrc050(i,j) = graufr_pl(i,p050,j)     
          grplfrc100(i,j) = graufr_pl(i,p100,j)     
          grplfrc150(i,j) = graufr_pl(i,p150,j)     
          grplfrc200(i,j) = graufr_pl(i,p200,j)     
          grplfrc250(i,j) = graufr_pl(i,p250,j)     
          grplfrc300(i,j) = graufr_pl(i,p300,j)     
          grplfrc350(i,j) = graufr_pl(i,p350,j)     
          grplfrc400(i,j) = graufr_pl(i,p_400,j)     
          grplfrc450(i,j) = graufr_pl(i,p450,j)     
          grplfrc500(i,j) = graufr_pl(i,p500,j)     
          grplfrc550(i,j) = graufr_pl(i,p550,j)     
          grplfrc600(i,j) = graufr_pl(i,p600,j)     
          grplfrc650(i,j) = graufr_pl(i,p650,j)     
          grplfrc700(i,j) = graufr_pl(i,p700,j)     
          grplfrc750(i,j) = graufr_pl(i,p750,j)     
          grplfrc800(i,j) = graufr_pl(i,p800,j)     
          grplfrc850(i,j) = graufr_pl(i,p850,j)     
          grplfrc900(i,j) = graufr_pl(i,p900,j)     
          grplfrc950(i,j) = graufr_pl(i,p950,j)     
          grplfrc1000(i,j)= graufr_pl(i,p1000,j)  
          avor050(i,j)    = avor_pl(i,p050,j)   
          avor100(i,j)    = avor_pl(i,p100,j)   
          avor150(i,j)    = avor_pl(i,p150,j)   
          avor200(i,j)    = avor_pl(i,p200,j)   
          avor300(i,j)    = avor_pl(i,p300,j)   
          avor350(i,j)    = avor_pl(i,p350,j)   
          avor400(i,j)    = avor_pl(i,p_400,j) 
          avor450(i,j)    = avor_pl(i,p450,j)   
          avor550(i,j)    = avor_pl(i,p550,j)   
          avor600(i,j)    = avor_pl(i,p600,j)   
          avor650(i,j)    = avor_pl(i,p650,j)   
          avor750(i,j)    = avor_pl(i,p750,j)   
          avor800(i,j)    = avor_pl(i,p800,j)  
          avor900(i,j)    = avor_pl(i,p900,j)   
          avor950(i,j)    = avor_pl(i,p950,j)  
          avor1000(i,j)   = avor_pl(i,p1000,j)
          pvor050(i,j)    = pvor_pl(i,p050,j)    
          pvor100(i,j)    = pvor_pl(i,p100,j)    
          pvor150(i,j)    = pvor_pl(i,p150,j)    
          pvor200(i,j)    = pvor_pl(i,p200,j)    
          pvor250(i,j)    = pvor_pl(i,p250,j)    
          pvor300(i,j)    = pvor_pl(i,p300,j)    
          pvor350(i,j)    = pvor_pl(i,p350,j)    
          pvor400(i,j)    = pvor_pl(i,p_400,j)  
          pvor450(i,j)    = pvor_pl(i,p450,j)    
          pvor500(i,j)    = pvor_pl(i,p500,j)    
          pvor550(i,j)    = pvor_pl(i,p550,j)    
          pvor600(i,j)    = pvor_pl(i,p600,j)    
          pvor650(i,j)    = pvor_pl(i,p650,j)    
          pvor700(i,j)    = pvor_pl(i,p700,j)    
          pvor750(i,j)    = pvor_pl(i,p750,j)    
          pvor800(i,j)    = pvor_pl(i,p800,j)          
          pvor850(i,j)    = pvor_pl(i,p850,j)          
          pvor900(i,j)    = pvor_pl(i,p900,j)          
          pvor950(i,j)    = pvor_pl(i,p950,j)          
          pvor1000(i,j)   = pvor_pl(i,p1000,j)         
          zpvor1_5(i,j)   = zpvor15(i,j)               
          zpvor2_0(i,j)   = zpvor20(i,j)               
          tpvor1_5(i,j)   = tpvor15(i,j)               
          tpvor2_0(i,j)   = tpvor20(i,j)               
          wdir_10(i,j)    = wdir10(i,j)                
          u10_earth(i,j)  = u10earth(i,j)
          v10_earth(i,j)  = v10earth(i,j)
          dryairmtot(i,j) = mubase(i,j) + mupert(i,j)  
          !! pressfc(i,j)         !! Now loaded above
!! CAWO end
      END DO
   END DO

   !! if( firstime ) then   
   !!    WRITE (MESG , * ) 'DEBUG PLD'
   !!    CALL wrf_error_fatal ( TRIM(MESG) )      
   !! endif      

END SUBROUTINE pld

!-----------------------------------------------------------------------------------

REAL FUNCTION tonpsadiabat (thte,prs)

    USE module_model_constants                                                  

    IMPLICIT NONE                                                               
   
    REAL   , INTENT(IN   )                                          :: thte,prs 

    REAL, PARAMETER :: RD = 287.04                                              
    REAL, PARAMETER :: GAMMA_RIP = RD/CP 

    INTEGER         :: jtch, jt, ipch, ip
    REAL            :: fracjt, fracjt2, fracip, fracip2 
    
    CHARACTER*256 MESG                                           
   
    !   This function gives the temperature (in K) on a moist adiabat           
    !   (specified by thte in K) given pressure in hPa.  It uses a              
    !   lookup table, with data that was generated by the Bolton (1980)         
    !   formula for theta_e.                                                    

 
    !   First check if pressure is less than min pressure in lookup table.      
    !   If it is, assume parcel is so dry that the given theta-e value can      
    !   be interpretted as theta, and get temperature from the simple dry       
    !   theta formula.                                                          
 
    IF ( prs .le. psadiprs(150) ) THEN                                          
      tonpsadiabat = thte*(prs/1000.)**GAMMA_RIP                                
      RETURN                                                                    
    ENDIF                                                                       

    !   Otherwise, look for the given thte/prs point in the lookup table.
 
    DO jtch = 1,150-1                                                        
      IF ( thte.ge.psadithte(jtch) .AND. thte.lt.psadithte(jtch+1) ) THEN    
        jt = jtch                                                            
        GOTO 213                                                             
      END IF                                                                 
    END DO                                                                   
    jt = -1                                                                  
213 CONTINUE

    DO ipch = 1,150-1                                                  
      if ( prs.le.psadiprs(ipch) .AND. prs.gt.psadiprs(ipch+1) ) THEN  
        ip = ipch                                                      
        GOTO 215                                                       
      END IF                                                           
    ENDDO                                                              
    ip = -1                                                            
215 CONTINUE

    IF ( jt.eq.-1 .OR. ip.eq.-1 ) THEN                                      
      !! WRITE (MESG ,'(A,2F13.6)' ) 'Outside of lookup table bounds. prs,thte=',prs,thte          
      !! CALL wrf_error_fatal ( TRIM(MESG) )
      tonpsadiabat = -9.9999
      RETURN                                                                
    ENDIF                                                                   

    fracjt  = (thte-psadithte(jt))/(psadithte(jt+1)-psadithte(jt))          
    fracjt2 = 1.-fracjt                                                     
    fracip  = (psadiprs(ip)-prs)/(psadiprs(ip)-psadiprs(ip+1))              
    fracip2 = 1.-fracip                                                     

    IF ( psaditmk(ip,jt  ).gt.1e9 .OR. psaditmk(ip+1,jt  ).gt.1e9 .OR.   &  
         psaditmk(ip,jt+1).gt.1e9 .OR. psaditmk(ip+1,jt+1).gt.1e9 ) THEN    
      WRITE (MESG ,'(A,2F13.6)' ) 'Tried to access missing tmp in lookup tab: unreasonable. prs,thte=',prs,thte
      CALL wrf_error_fatal ( TRIM(MESG) )
    ENDIF                                                                   

    tonpsadiabat = fracip2*fracjt2*psaditmk(ip  ,jt  )+   &                 
                   fracip *fracjt2*psaditmk(ip+1,jt  )+   &                 
                   fracip2*fracjt *psaditmk(ip  ,jt+1)+   &                 
                   fracip *fracjt *psaditmk(ip+1,jt+1)                      

END FUNCTION tonpsadiabat

!-----------------------------------------------------------------------------------

REAL FUNCTION POLYSVP (T,TYPE)

!-------------------------------------------

!  COMPUTE SATURATION VAPOR PRESSURE

!  POLYSVP RETURNED IN UNITS OF PA.
!  T IS INPUT IN UNITS OF K.
!  TYPE REFERS TO SATURATION WITH RESPECT TO LIQUID (0) OR ICE (1)

! Borrowed from module_mp_morr_two moment scheme; FROM FLATAU ET AL. 1992, TABLE 4 (RIGHT-HAND COLUMN)
! Cleaned up for readability.

      IMPLICIT NONE

      ! ice
   
      REAL, PARAMETER :: A0i = 6.11147274
      REAL, PARAMETER :: A1i = 0.503160820     
      REAL, PARAMETER :: A2i = 0.188439774e-1
      REAL, PARAMETER :: A3i = 0.420895665e-3
      REAL, PARAMETER :: A4i = 0.615021634e-5      
      REAL, PARAMETER :: A5i = 0.602588177e-7   
      REAL, PARAMETER :: A6i = 0.385852041e-9      
      REAL, PARAMETER :: A7i = 0.146898966e-11   
      REAL, PARAMETER :: A8i = 0.252751365e-14   

      ! liquid (V1.7)   

      REAL, PARAMETER :: A0 = 6.11239921
      REAL, PARAMETER :: A1 = 0.443987641
      REAL, PARAMETER :: A2 = 0.142986287e-1
      REAL, PARAMETER :: A3 = 0.264847430e-3
      REAL, PARAMETER :: A4 = 0.302950461e-5  
      REAL, PARAMETER :: A5 = 0.206739458e-7
      REAL, PARAMETER :: A6 = 0.640689451e-10  
      REAL, PARAMETER :: A7 = -0.952447341e-13
      REAL, PARAMETER :: A8 = -0.976195544e-15 

      REAL DUM
      REAL T, DT
      INTEGER TYPE

! ICE

      IF (TYPE.EQ.1) THEN

           dt = max(-80.,t-273.16)
           polysvp = a0i + dt*(a1i+dt*(a2i+dt*(a3i+dt*(a4i+dt*(a5i+dt*(a6i+dt*(a7i+a8i*dt))))))) 
           polysvp = polysvp*100.

      END IF

! LIQUID

      IF (TYPE.EQ.0) THEN

           dt = max(-80.,t-273.16)
           polysvp = a0 + dt*(a1+dt*(a2+dt*(a3+dt*(a4+dt*(a5+dt*(a6+dt*(a7+a8*dt)))))))
           polysvp = polysvp*100.

      END IF

END FUNCTION POLYSVP

!-----------------------------------------------------------------------------------

SUBROUTINE cobb_dyn_snow( i,j,kts,kte,ires_typ,ctop_cnvct,cbot_cnvct,cdpt_cnvct,iso_deepr, &
                          iso_shalr,lbranch225,kbot_cnvct,ktop_cnvct,sn_cv_rate,rain_rate, &
                          w0avg,prmb,dtc,dz8w,NSRT,snrat_temp,snrat_dens,ncilays,kcibas,   &
                          kcitop,lresdcon,dta,ep_2,qv, &
                                  snowratio )

   IMPLICIT NONE

   REAL   , PARAMETER     :: snowrat_DEFAULT = 10.0

   INTEGER, INTENT(IN   ) :: kts,kte,i,j,NSRT
   INTEGER, INTENT(IN   ) :: ires_typ
   REAL   , INTENT(  OUT) :: snowratio
   REAL   , INTENT(IN   ) :: ctop_cnvct,cbot_cnvct,cdpt_cnvct
   INTEGER, INTENT(IN   ) :: kbot_cnvct,ktop_cnvct
   REAL   , INTENT(IN   ) :: sn_cv_rate,rain_rate
   REAL   , INTENT(IN   ) :: w0avg(kts:kte),prmb(kts:kte),dtc(kts:kte),dz8w(kts:kte)
   REAL   , INTENT(IN   ) :: dta(kts:kte),qv(kts:kte)   
   REAL   , INTENT(IN   ) :: snrat_temp(NSRT),snrat_dens(NSRT)     
   LOGICAL, INTENT(IN   ) :: iso_deepr,iso_shalr,lbranch225,lresdcon
   INTEGER, INTENT(IN   ) :: ncilays, kcibas( 10 ), kcitop( 10 )
   REAL   , INTENT(IN   ) :: ep_2 

   real evs, eis, qvi, prspa, rhice, rhwat, qvs, maxw, dmaxw
   real,    dimension(kts:kte)                                              :: wgtlays
   real,    dimension(kts:kte)                                              :: sndent
   integer, dimension(kts:kte)                                              :: klused
   integer, dimension(kts:kte)                                              :: wwgt_lay
   integer, dimension(kts:kte)                                              :: ncld_lay          
   real wgtsum, dttbl, facup, facdn
   integer jj, ncbl, nvvskip, nclskip, nlvskip, nvvlay, ncldlay
   integer k,kl
   logical l_vvweight
   logical l_convective
   logical l_resolved

   CHARACTER*256 MESG    

   !! Executable Code starts here:

   maxw = 0.0
   wgtlays( : ) = 0.0
   wwgt_lay( : ) = 0.0
   sndent( : ) = 0.0
   ncld_lay( : ) = 0
   klused( : ) = 0

   maxw = 0.0
   wgtsum = 0.0    
   nclskip = 0
   nvvskip = 0
   nlvskip = 0
   nvvlay =  0
   ncldlay = 0
   ncbl = 0
   facup = 0
   facdn = 0
   
   l_resolved = .false.
   l_convective = .false.
   l_vvweight = .true.
   
   snowratio = 0.0   !! Result to be computed    

   if( ires_typ.ne.0 .and. ires_typ.ne.1 .and. ires_typ.ne.2 ) then
       CALL wrf_error_fatal ( 'Bad ires_typ detected in Cobb' )
   endif

   if ( ires_typ .eq. 0 ) then  !! No resolved cloud, must use param
      if( lbranch225 ) then                !! Assuming clear sky--use DEFAULT
         snowratio = snowrat_DEFAULT
         RETURN
      endif
      if( ctop_cnvct .le. 1.0e-6 ) then    !! Assuming clear sky--use DEFAULT
         snowratio = snowrat_DEFAULT
         RETURN                
      endif                   
      if( iso_deepr ) then
         !! print*,'Isolated DEEP parameterized convection detected'
         l_convective = .true.
      else if( iso_shalr ) then
         !! print*,'Isolated SHALLOW parameterized convection detected'
         l_convective = .true.
      else                                 !! Assuming clear sky--use DEFAULT
         !! print*,'NO cloud detected, using default: ires_typ=',ires_typ
         snowratio = snowrat_DEFAULT
         RETURN          
      endif
   endif
    
   !! If ires_typ > 0; Determine whether to use convective or resolved cloud for dynamic
   !! ratio calculation
   
   if( ires_typ .eq. 1 ) then  !! Only light resolved precip; check convective
       if( ctop_cnvct .gt. 1.0e-6 ) then   
          if( kbot_cnvct .lt. 1 .or. ktop_cnvct .lt. 1 ) then                          
              WRITE( MESG, '( A, 2I8,2(F13.8,2X) )' ) 'kbot_cnvct,ktop_cnvct,cbot_cnvct,cdpt_cnvct=',kbot_cnvct,ktop_cnvct,cbot_cnvct,cdpt_cnvct
              CALL wrf_message (TRIM(mesg))
              CALL wrf_error_fatal ( 'Cobb: bad parameterized convective cloud top or bottom' )
          endif
          
          if( (sn_cv_rate/rain_rate) .gt. 0.675 ) then !! "majority convective snow"
             l_convective = .true.             
          else
             !! print*,'Enough non-convective to use resolved'
             l_resolved = .true.
             !! CALL wrf_error_fatal ( 'Cobb: debug needed' )                                    
          endif
       else
          !! print*,'No parameterized cloud detected, will use resolved layers'
          l_resolved = .true.
       endif
   else if( ires_typ .eq. 2 ) then  !! Definitively resolved 
       l_resolved = .true.   
   endif
   
   if( l_convective ) then
      !! print*,'Convective: ,ires_typ= ',ires_typ
      ncbl = ktop_cnvct-kbot_cnvct+1
      do kl = kbot_cnvct, ktop_cnvct
          if( w0avg(kl) .le. 0.0 ) then
              nvvskip = nvvskip+1
          endif
          if( prmb(kl) .ge. 490.00 .and. dtc(kl) .lt. 0.0 .and. dtc(kl) .gt. -32.0 ) then
             if( w0avg(kl) .gt. 0.0 ) then
                wwgt_lay(kl) = kl
                nvvlay =  nvvlay + 1
                maxw = max( maxw, w0avg(kl) )
             else
                ncld_lay(kl) = kl
                ncldlay = ncldlay+1
             endif
          else
             nclskip = nclskip+1
          endif
      enddo
      if( nvvlay .gt. 0 ) then
         !! print*,'maxw=',maxw
         dmaxw = 1.0/maxw
         wgtsum = 0
         !! print*,'Number vv acceptable layers=',nvvlay
      else
         if( ncldlay .gt. 0 ) then                          
             l_vvweight = .false.
         else
             !! print*,'Using default snow-ratio: No param cld layers meet criteria bounds'
             snowratio = snowrat_DEFAULT
             RETURN
         endif    
      endif

      do kl = kbot_cnvct, ktop_cnvct
         if( nvvlay .gt. 0 .and. wwgt_lay(kl) .eq. kl ) then
            wgtlays(kl) = w0avg(kl) * (( w0avg(kl)*dmaxw )**2) * dz8w(kl)
            if( w0avg(kl) .le. 0.0 ) then
              CALL wrf_error_fatal ( 'Bad vertical velocity used in weighting' )
            endif 
         else if(.NOT.l_vvweight .and. ncld_lay(kl) .eq. kl ) then
            wgtlays(kl) = dz8w(kl)
         else
            CYCLE
         endif 

         wgtsum = wgtsum + wgtlays(kl)
         klused(kl) = 1
         !! print*,'layer temp=',dtc(kl)
         do jj = 1,NSRT
           if( snrat_temp(jj) .gt. dtc(kl) ) then
              dttbl = snrat_temp(jj) - snrat_temp(jj-1)
              facup = (abs(dtc(kl)) - abs(snrat_temp(jj)))/dttbl 
              facdn = 1.0 - facup
              !! print*,'At jj=',jj
              !! print*,'snrat_temp(jj-1),dtc(kl),snrat_temp(jj)=',snrat_temp(jj-1),dtc(kl),snrat_temp(jj)
              !! print*,'facup, facdn=',facup, facdn
              sndent(kl) = facup * snrat_dens(jj-1) + facdn * snrat_dens(jj)
              !! print*,'snrat_dens(jj-1),sndent(kl),snrat_dens(jj) = ',snrat_dens(jj-1),sndent(kl),snrat_dens(jj)
              exit 
           endif
         enddo
      enddo                          
      !! print*,'wgtsum=',wgtsum
      do kl = kbot_cnvct, ktop_cnvct
         if( klused(kl) .eq. 1 ) then                  
            snowratio = snowratio + sndent(kl)*wgtlays(kl)/wgtsum
         endif   
      enddo
      !! print*,'Final subroutine CONVECTIVE-based snow-ratio = ',snowratio
      RETURN   
         
   else if( l_resolved ) then  !! Use resolved-scale cloud layer structure
   
      !! print*,'Resolved: ,ires_typ= ',ires_typ
      if( lresdcon ) then
         !! print*,'Resolved-scale deep convection, lresdcon=',lresdcon
      endif
      do k=1,ncilays
         do kl = kcibas(k),kcitop(k)
           if( w0avg(kl) .le. 0.0 ) then
             nvvskip = nvvskip+1
           endif
           if( prmb(kl) .ge. 490.00 .and. dtc(kl) .lt. 0.0 .and. dtc(kl) .gt. -32.0 ) then                   
                !! Calculate RH w/rt to ice .. if > 90%, then assume a
                !! cloudy possibly snow producing layer...assuming the
                !! layer temp is within range. Use formula in Morrison
                !! two moment microphysics                         

                !! print*,'kl,prmb=',kl,prmb(kl)
                prspa  = prmb(kl)*100                      ! pa
                eis = min(0.99*prspa,polysvp(dta(kl),1))   ! (Pa,kelvin); returns Pa --> ice
                evs = min(0.99*prspa,polysvp(dta(kl),0))   ! (Pa,kelvin); returns Pa --> water
                if (eis.gt.evs) eis = evs                  ! make sure ice saturation doesn't exceed water sat. near freezing
                qvi = ep_2*eis/(prspa-eis)
                qvs = ep_2*evs/(prspa-evs)
                rhice = qv(kl)/qvi                     ! rhice will often be > rhwat for given qv
                rhwat = qv(kl)/qvs
                if( rhice .ge. 0.90 ) then
                   !! print*,'rhice>90%:kl,dta=',kl,dta(kl)
                   !! print*,'prspa=',prspa
                   !! print*,'eis=',eis
                   !! print*,'polysvp_ice=',polysvp(dta(kl),1)
                   !! print*,'evs=',evs
                   !! print*,'polysvp_wat=',polysvp(dta(kl),0)
                   !! print*,'rhice,rhwat=',rhice,rhwat
                   if( w0avg(kl) .gt. 0.0 ) then
                       wwgt_lay(kl) = kl
                       nvvlay =  nvvlay + 1
                       maxw = max( maxw, w0avg(kl) )
                    else
                       ncld_lay(kl) = kl
                       ncldlay = ncldlay+1
                    endif                                
                else
                  nclskip = nclskip+1
                endif                                            
           else
                nclskip = nclskip+1                           
           endif                                      
           ncbl = ncbl+1
         enddo
      enddo
      if( nvvlay .gt. 0 ) then
         !! print*,'maxw=',maxw
         dmaxw = 1.0/maxw
         wgtsum = 0
         !! print*,'Number vv acceptable layers=',nvvlay
      else
         !! print*,'No cloudy layers have upwards vertical velocities'
         !! print*,'ncldlay=',ncldlay
         if( ncldlay .gt. 0 ) then                          
             l_vvweight = .false.
         else
             !! print*,'Using default snow-ratio: No resolved cld layers meet criteria bounds'
             snowratio = snowrat_DEFAULT
             RETURN
         endif    
      endif                   

      do k=1,ncilays
         do kl = kcibas(k),kcitop(k)
            if( nvvlay .gt. 0 .and. wwgt_lay(kl) .eq. kl ) then
               wgtlays(kl) = w0avg(kl) * (( w0avg(kl)*dmaxw )**2) * dz8w(kl)
               if( w0avg(kl) .le. 0.0 ) then
                 CALL wrf_error_fatal ( 'Bad vertical velocity used in weighting' )
               endif 
            else if(.NOT.l_vvweight .and. ncld_lay(kl) .eq. kl ) then
               wgtlays(kl) = dz8w(kl)
            else
               CYCLE
            endif

            wgtsum = wgtsum + wgtlays(kl)
            klused(kl) = 1
            !! print*,'layer temp=',dtc(kl)
            do jj = 1,NSRT
              if( snrat_temp(jj) .gt. dtc(kl) ) then
                 dttbl = snrat_temp(jj) - snrat_temp(jj-1)
                 facup = (abs(dtc(kl)) - abs(snrat_temp(jj)))/dttbl 
                 facdn = 1.0 - facup
                 !! print*,'At jj=',jj
                 !! print*,'snrat_temp(jj-1),dtc(kl),snrat_temp(jj)=',snrat_temp(jj-1),dtc(kl),snrat_temp(jj)
                 !! print*,'facup, facdn=',facup, facdn
                 sndent(kl) = facup * snrat_dens(jj-1) + facdn * snrat_dens(jj)
                 !! print*,'snrat_dens(jj-1),sndent(kl),snrat_dens(jj) = ',snrat_dens(jj-1),sndent(kl),snrat_dens(jj)
                 exit 
              endif
            enddo
         enddo
      enddo
      snowratio = 0.0                   
      do k=1,ncilays
         do kl = kcibas(k),kcitop(k)                                      
            if( klused(kl) .eq. 1 ) then                  
               snowratio = snowratio + sndent(kl)*wgtlays(kl)/wgtsum
            endif
         enddo
      enddo
      !! print*,'Final subroutine NON-CONVECTIVE-based snow-ratio = ',snowratio   

   else
      CALL wrf_error_fatal ( 'Cobb: Cloud type not detected' )
   endif  
   
   RETURN                                                                 
          
END SUBROUTINE cobb_dyn_snow

!-----------------------------------------------------------------------------------

SUBROUTINE relhl( ldbg, uk, vk, ucrs10, vcrs10, terij, top, z_at_w, dzf, &
                  zht, srh,                          &
                  kds,kde,                           &
                  kms,kme,                           &
                  kts,kte                                )

   IMPLICIT NONE

   INTEGER, INTENT(IN   )                                          :: kds,kde, &
                                                                      kms,kme, &
                                                                      kts,kte
   REAL   , INTENT(IN   ) , DIMENSION( kts:kte ) :: uk,vk,z_at_w,dzf,zht
   REAL   , INTENT(IN   ) :: terij, top, ucrs10, vcrs10
   LOGICAL, INTENT(IN   ) :: ldbg
   REAL   , INTENT(  OUT) :: srh

   REAL, PARAMETER :: PI = 3.141592653589793
   REAL, PARAMETER :: dtr=pi/180.
   REAL, PARAMETER :: dpr=180./pi
   
   !! Local variables
   
   REAL    :: sdh, su, sv, ua, va, asp, adr, bsp, bdr, cu, cv
   REAL    :: u_at_w_dn, u_at_w_up, v_at_w_dn, v_at_w_up, usr, vsr, dudz, dvdz
   REAL    :: uheadvwsv, vheadvwsv, utailvwsv, vtailvwsv, ushear, vshear
   REAL    :: udrm, vdrm
   REAL    :: ustrm, vstrm
   REAL    :: z1,z3,z2,dz1,dz2,dz,du1,du2,dv1,dv2
   REAL , DIMENSION( kts:kte ) :: u_full, v_full
   INTEGER :: ktop, k, nl
   
   !!REAL    :: uk1, vk1, dzf1, uk2, vk2, dzf2     
   
   !   
   !...............................................................................................
   !      

      !! Bunkers et al (Weather and Forecasting; February 2000) 
      !! method for computing storm motion (right moving); non-mass-weighted !!
      !! UNIPOST method. 

      su = 0
      sv = 0
      nl = 0
      
      do k = 1,kte-1
         if ( (zht(k) - terij) .le. 6000. ) then   !! remember z_at_w(1)==terij !!
            su = su + uk(k)
            sv = sv + vk(k)
            nl = nl + 1   
         else
            exit
         endif                                                                     
      enddo 
                  
      ua = su / float( nl )      !! Mean-wind in 0-6km model layer (Vmean for Bunkers)                                                                         
      va = sv / float( nl )         
                  
      !! if ( ldbg ) then         
      !!    asp = sqrt(ua*ua + va*va)
      !!    if (ua .eq. 0. .and. va .eq. 0.) then
      !!      adr = 0.
      !!    else   
      !!      adr = dpr * (pi + atan2(ua,va))
      !!    endif                   
      !!    print*,'Bunkers non-mass weighted mean-wind: ua,va=',ua,va
      !!    print*,'Bunkers mean direction: adr=',adr
      !!    print*,'Bunkers mean windspeed: asp = ',asp      
      !! endif
         
      !! Estimate Vshear for Bunkers: follow UNIPOST approach
         
      su = 0.
      sv = 0.
      nl = 0         
         
      do k = 1,kte-1
         if ( (zht(k) - terij) .le. 6000. ) then   !! remember z_at_w(1)==terij !!         
           if ( (zht(k) - terij) .ge. 5500. ) then
              nl = nl + 1
              su = su + uk(k)                                                                                            
              sv = sv + vk(k)
           endif
         else
            exit
         endif                                                                                                                 
      enddo                                                                                                                
                                                                                                       
      if( nl .eq. 0 ) then          !! there were no levels with height between 5500 AND 6000      
         do k = 1,kte-1
            if ( (zht(k) - terij) .gt. 6000. ) then   !! remember z_at_w(1)==terij !!
               nl = nl + 1
               su = su + uk(k)                                                                                            
               sv = sv + vk(k)
               goto 30
            endif                                                                                                               
         enddo
      endif
30    continue      
      
      uheadvwsv = su / float( nl )  !! Mean-wind for head of vertical wind-shear vector                                                                       
      vheadvwsv = sv / float( nl )       
                           
      su = 0.
      sv = 0.
      nl = 0  
                  
      do k = 1,kte-1
         if ( (zht(k) - terij) .le. 500. ) then !         
           su = su + uk(k)                                                                                            
           sv = sv + vk(k)                                                                                           
           nl = nl + 1
         else
           exit         
         endif                                                                                           
      enddo                                                                                                                
                                                                                               
      utailvwsv = su / float( nl )   !! Mean-wind for head of vertical wind-shear vector                                                                       
      vtailvwsv = sv / float( nl ) 
         
      ushear = uheadvwsv - utailvwsv
      vshear = vheadvwsv - vtailvwsv         
      asp = sqrt(ushear*ushear + vshear*vshear)
      
      !! if ( ldbg ) then         
      !!   print*,'Bunkers uheadvwsv,vheadvwsv=',uheadvwsv,vheadvwsv
      !!   print*,'Bunkers utailvwsv,vtailvwsv=',utailvwsv,vtailvwsv
      !!   print*,'Bunkers ushear, vshear =',ushear, vshear
      !!   if (ushear .eq. 0. .and. vshear .eq. 0.) then
      !!      adr = 0.
      !!   else   
      !!      adr = dpr * (pi + atan2(ushear,vshear))
      !!   endif
      !!   print*,'Bunkers Shear mean direction: adr=',adr
      !!   print*,'Bunkers Shear magnitude: asp=',asp
      !! endif
               
      !! if( ldbg ) then
      !!   For a right mover, add 90 degrees to the shear vector direction
      !!   bsp = 7.5
      !!   bdr = adr + 90.
      !!   if (bdr .gt. 360.) bdr = bdr-360.
      !!   udrm = -bsp * sin(bdr*dtr)    !! propogation deviation from mean wind                                              
      !!   vdrm = -bsp * cos(bdr*dtr)    !! pi/180.
      !!   print*,'shear: adr,bdr=',adr,bdr
      !!   print*,'sin,cos of shear adr=',sin(adr*dtr),cos(adr*dtr)
      !!   print*,'bdr*dtr=',bdr*dtr
      !!   print*,'sin,cos=',sin(bdr*dtr),cos(bdr*dtr)
      !!   print*,'initial udrm, vdrm=',udrm, vdrm
      !!   print*,'revised udrm, vdrm=',( bsp*vshear/asp ),( bsp*ushear/asp )        
      !! endif         

      bsp = 7.5                          !! m/s: Deviation wind magnitude: best-fit according to Bunkers      
      ustrm = ua + ( bsp*vshear/asp )
      vstrm = va - ( bsp*ushear/asp )
      
      !! if( ldbg ) then
      !!    print*,'ustrm=',ustrm
      !!    print*,'vstrm=',vstrm      
      !! endif
         
      !! if( ldbg ) then      
      !!    cu = ua + udrm      
      !!    cv = va + vdrm      
      !!    print*,'Bunkers Storm u-component: cu=',cu
      !!    print*,'Bunkers Storm v-component: cv=',cv
      !!    asp = sqrt(cu*cu + cv*cv)
      !!    if (cu .eq. 0. .and. cv .eq. 0.) then
      !!      adr = 0.
      !!    else   
      !!      adr = dpr * (pi + atan2(cu,cv))
      !!    endif
      !!    print*,'**Bunkers storm direction: adr=',adr
      !!    print*,'**Bunkers storm speed: asp = ',asp                   
      !! endif

      !! vertical integral of horizontal wind dotted into the *horizontal*
      !! component of the vorticity: use 2nd order vertical-spatial
      !! derivative

      srh = 0.0

      do k = 2,kte-1         
         z2=zht(k)
         z3=zht(k+1)
         z1=zht(k-1)         
         dz1=z2-z1
         dz2=z3-z2         
         dz=z_at_w(k+1)-z_at_w(k)         
         du1=uk(k)-uk(k-1)
         du2=uk(k+1)-uk(k)         
         dv1=vk(k)-vk(k-1)
         dv2=vk(k+1)-vk(k)         
         !! if ( ldbg ) then         
         !!   print*,'z2,z3,z1,dz1=',z2,z3,z1,dz1
         !!   print*,'dz2,dz,du1=',dz2,dz,du1
         !!   print*,'du2,dv1,dv2=',du2,dv1,dv2         
         !! endif                 
          
         if ( (zht(k) - terij) .lt. top ) then   !! remember z_at_w(1)==terij !!         
            srh = srh + ((vk(k)-vstrm)*(dz2*(du1/dz1)+dz1*(du2/dz2))    &
                     -   (uk(k)-ustrm)*(dz2*(dv1/dz1)+dz1*(dv2/dz2)))   &
                         *dz/(dz1+dz2)
         
         !! if ( ldbg ) then
         !!    print*,'srh,vk(k)-vstrm,dz2*(du1/dz1)=',srh,vk(k)-vstrm,dz2*(du1/dz1)
         !!    print*,'dz1*(du2/dz2)=',dz1*(du2/dz2)
         !!    print*,'uk(k)-ustrm,dz2*(dv1/dz1),dz1*(dv2/dz2)=',uk(k)-ustrm,dz2*(dv1/dz1),dz1*(dv2/dz2)
         !!    print*,'dz/(dz1+dz2)=',dz/(dz1+dz2)         
         !! endif          
         
         else
           goto 108
         endif  
      enddo

108   continue

      !! if(ldbg) then
      !!   print*,'top, Bunkers SRH =',top,srh
      !!   do k = 2,kte-1
      !!      print*,'k,zht(k)=',k,zht(k)
      !!   enddo
      !! endif

      !! if( ldbg ) then
      !!  CALL wrf_error_fatal ( 'debug inside relhl' )
      !! endif                                   
                                                                             
      RETURN                                                                 
          
END SUBROUTINE RELHL

SUBROUTINE loc_interp( lodex, hidex, nvals, fintv, fval, flist, ftarg, fintrp )

   IMPLICIT NONE

   INTEGER, INTENT(IN   )                        :: lodex,hidex
   INTEGER, INTENT(IN   )                        :: nvals
   REAL   , INTENT(IN   )                        :: fintv, fval
   REAL   , INTENT(IN   ) , DIMENSION(nvals)     :: flist, ftarg
   REAL   , INTENT(  OUT)                        :: fintrp   
   
   !! Local variables
   
   REAL    :: wght1,wght2
   INTEGER :: m,locatr1,lo,hi
      
   !...............................................................................................
   !      

      LO = LODEX
      HI = HIDEX
      fintrp = 0.0

111   continue

      IF ( LO .GT. HI ) THEN
        LOCATR1 = LO
        goto 112
      END IF              

      M = ( LO + HI ) / 2
      IF ( fval .GT. flist( M ) ) THEN
        LO = M + 1
        GO TO  111
      ELSE IF ( fval .LT. flist( M ) ) THEN
        HI = M - 1
        GO TO  111
      ELSE           !! "exact" find
        LOCATR1 = M
        fintrp = ftarg(M)
        RETURN
      END IF              

112   continue 

      wght1 = ( flist(lo) - fval ) / fintv
      fintrp = wght1*ftarg(lo-1) + (1.0-wght1)*ftarg(lo)
                                                                                                                           
      RETURN                                                                 
          
END SUBROUTINE LOC_INTERP

SUBROUTINE single_layclds( isitu, iso_shalr, ml, fl, kbot, ktop, rhk, dta, dtc, vvubs, lwc, frcmax, icg_pot1d, sld_pot1d,   &
                           kds,kde,                                                                               &
                           kms,kme,                                                                               &
                           kts,kte )

   USE module_model_constants

   IMPLICIT NONE

   !  Input variables

   INTEGER, INTENT(IN   )                                          :: kds,kde, &
                                                                      kms,kme, &
                                                                      kts,kte   
   INTEGER, INTENT(IN   )                                          :: isitu, ml, fl, kbot, ktop
   LOGICAL, INTENT(IN   )                                          :: iso_shalr
   REAL   , INTENT(IN   ) , DIMENSION( kms:kme ) :: dta, dtc, vvubs, rhk
   REAL   , INTENT(IN   ) , DIMENSION( kms:kme ) :: lwc               !! lwc in kg/kg

   !  Output variables   

   REAL   , INTENT(  OUT)                        :: frcmax   
   REAL   , INTENT(  OUT) , DIMENSION( kms:kme ) :: icg_pot1d, sld_pot1d

   !  Local variables

   REAL, PARAMETER :: D14 = 1.0/14.0
   REAL, PARAMETER :: LWC_LOWLIM = 5.0e-5 !! kg/kg  
   INTEGER         :: nlays,k
   REAL            :: ctt, ctk, cnvtmap, slwmap, pirepmap, vvmap, cttmap, tmap, rhmap, pct_radar, sldfac, frcshal 
   
   CHARACTER*256 MESG
   
   !! Statement Functions
   
   REAL FINAL_UP, INIT_INTERST, SLW_MAP, PIREP_MAP, VV_MAP
   FINAL_UP( INIT_INTERST, SLW_MAP, PIREP_MAP, VV_MAP ) = INIT_INTERST + (1.0 - INIT_INTERST)*    &
                                                      (0.4*SLW_MAP + 0.35*PIREP_MAP + 0.25*VV_MAP)
   REAL FINALDWN                                                      
   FINALDWN( INIT_INTERST, SLW_MAP, PIREP_MAP, VV_MAP ) = INIT_INTERST*(1.0 + 0.25*VV_MAP) + (1.0 - INIT_INTERST)*  &                                                  
                                                      (0.4*SLW_MAP + 0.35*PIREP_MAP)
   !...............................................................................................
   !    

      icg_pot1d( : ) = 0.0
      sld_pot1d( : ) = 0.0
      pct_radar      = 1.0          !! code as in original scheme 

      !! For isolated sub-grid convection, assume fractional coverage is not greater than the 
      !! mean grid-scale rh through the cloud depth

      frcmax = 0.0
      nlays = ktop-kbot+1
      do k = kbot,ktop
         frcmax = frcmax+rhk(k)
      enddo
      frcmax = 0.01*frcmax/float(nlays)

      !! In cases where shallow parameterized convection is also precipitating, we have
      !! distiguished isitu (4-8) as if it was a layer cloud, but apply the convective fraction
      
      frcshal = 1.0
      if( iso_shalr ) then
         frcshal = frcmax
      endif
            
      cttmap = 0.0
      ctt = dtc(ktop)
      ctk = dta(ktop)
      if( ctt .gt. fip_normcttcel_temp(1) .and. & 
          ctt .lt. fip_normcttcel_temp(NCTT)  ) then   !! Table lookup and interp
                                                       !! cloudtop-temperature interest map            
          call loc_interp( 1, NCTT, NCTT, 2.0, ctt, fip_normcttcel_temp, fip_normcttcel_indx, cttmap )
      endif 
   
      if( isitu .eq. 2 ) then  !! Weak param. shallow convec
          do k = kbot,ktop            
             tmap = 0.0
             if( dtc(k) .gt. fip_normtcel_temp(1) .and. &
                 dtc(k) .lt. fip_normtcel_temp(NTCL)  ) then !! Table lookup and interp
                                                           !! temperature interest map
                 call loc_interp( 1, NTCL, NTCL, 2.0, dtc(k), fip_normtcel_temp, fip_normtcel_indx, tmap )                                                  
             endif !! Note if dtc(k) is out of the table range, tmap==0

             rhmap = 0.0
             if( rhk(k) .gt. fip_normrh_pct(1) .and. &
                 rhk(k) .lt. fip_normrh_pct(NVRH)  ) then !! Table lookup and interp
                 call loc_interp( 1, NVRH, NVRH, 5.0, rhk(k), fip_normrh_pct, fip_normrh_indx, rhmap ) !! rel. hum. interest map
             endif
             icg_pot1d(k) = cttmap*tmap*rhmap
             sld_pot1d(k) = DIAG_MISSING
             
             !! Now apply "boosting factor(s)": in this case, vertical velocity map to get 
             !! final icg_pot3d. There is no resolved cloud thus no super-cooled liquid interest map. 

             pirepmap = 0.0              
             slwmap   = 0.0
             vvmap = 0.0

             if( dtc(k) .lt. 0.0 ) then
                 if( vvubs(k) .le. -1.0 ) then
                    vvmap = 1.0
                 else if( vvubs(k) .ge. 1.0 ) then
                    vvmap = -0.075                   
                 else
                    call loc_interp( 1, NVRH, NVRH, 0.1, vvubs(k), fip_normvv_ubars, fip_normvv_indx, vvmap )
                 endif
             endif

             if( vvubs(k) .le. 0.0 ) then !! upward or no vertical velocity
                 icg_pot1d(k) = frcshal * final_up( icg_pot1d(k), slwmap, pirepmap, vvmap )
             else
                 icg_pot1d(k) = frcshal * finaldwn( icg_pot1d(k), slwmap, pirepmap, vvmap )
             endif
          enddo
      else if( isitu .eq. 4 ) then  !! single-layer no precip or DZ/RA with CTT < -12C 
          do k = kbot,ktop            
             tmap = 0.0
             if( dtc(k) .gt. fip_normtcel_temp(1) .and. &
                 dtc(k) .lt. fip_normtcel_temp(NTCL)  ) then !! Table lookup and interp
                                                           !! temperature interest map
                 call loc_interp( 1, NTCL, NTCL, 2.0, dtc(k), fip_normtcel_temp, fip_normtcel_indx, tmap )                                                  
             endif !! Note if dtc(k) is out of the table range, tmap==0

             rhmap = 0.0
             if( rhk(k) .gt. fip_normrh_pct(1) .and. &
                 rhk(k) .lt. fip_normrh_pct(NVRH)  ) then !! Table lookup and interp
                 call loc_interp( 1, NVRH, NVRH, 5.0, rhk(k), fip_normrh_pct, fip_normrh_indx, rhmap ) !! rel. hum. interest map
             endif
             icg_pot1d(k) = cttmap*tmap*rhmap
             sld_pot1d(k) = DIAG_MISSING
             
             !! Now apply "boosting factor(s)":  

             pirepmap = 0.0
             slwmap   = 0.0
             vvmap = 0.0              
             
             if( dtc(k) .lt. 0.0 ) then
                 if( lwc(k) .ge. LWC_LOWLIM ) then
                    slwmap = 1.0
                 else
                    slwmap = 0.0
                 endif

                 if( vvubs(k) .le. -1.0 ) then
                    vvmap = 1.0
                 else if( vvubs(k) .ge. 1.0 ) then
                    vvmap = -0.075                   
                 else
                    call loc_interp( 1, NVRH, NVRH, 0.1, vvubs(k), fip_normvv_ubars, fip_normvv_indx, vvmap )
                 endif
             endif

             if( vvubs(k) .le. 0.0 ) then !! upward or no vertical velocity
                 icg_pot1d(k) = frcshal * final_up( icg_pot1d(k), slwmap, pirepmap, vvmap )
             else
                 icg_pot1d(k) = frcshal * finaldwn( icg_pot1d(k), slwmap, pirepmap, vvmap )
             endif
          enddo      
      else if( isitu .eq. 5 ) then  !! single-layer, All snow
          do k = kbot,ktop            
             tmap = 0.0
             if( dtc(k) .gt. fip_normtcel_temp(1) .and. &
                 dtc(k) .lt. fip_normtcel_temp(NTCL)  ) then !! Table lookup and interp
                                                           !! temperature interest map
                 call loc_interp( 1, NTCL, NTCL, 2.0, dtc(k), fip_normtcel_temp, fip_normtcel_indx, tmap )                                                  
             endif !! Note if dtc(k) is out of the table range, tmap==0

             rhmap = 0.0
             if( rhk(k) .gt. fip_normrh_pct(1) .and. &
                 rhk(k) .lt. fip_normrh_pct(NVRH)  ) then !! Table lookup and interp
                 call loc_interp( 1, NVRH, NVRH, 5.0, rhk(k), fip_normrh_pct, fip_normrh_indx, rhmap ) !! rel. hum. interest map
             endif
             icg_pot1d(k) = (0.9-0.6*pct_radar)*cttmap*tmap*rhmap
             sld_pot1d(k) = DIAG_MISSING
             
             !! Now apply "boosting factor(s)":

             pirepmap = 0.0
             slwmap   = 0.0
             vvmap = 0.0              

             if( dtc(k) .lt. 0.0 ) then
                 if( lwc(k) .ge. LWC_LOWLIM ) then
                    slwmap = 1.0
                 else
                    slwmap = 0.0
                 endif

                 if( vvubs(k) .le. -1.0 ) then
                    vvmap = 1.0
                 else if( vvubs(k) .ge. 1.0 ) then
                    vvmap = -0.075                   
                 else
                    call loc_interp( 1, NVRH, NVRH, 0.1, vvubs(k), fip_normvv_ubars, fip_normvv_indx, vvmap )
                 endif
             endif

             if( vvubs(k) .le. 0.0 ) then !! upward or no vertical velocity
                 icg_pot1d(k) = frcshal * final_up( icg_pot1d(k), slwmap, pirepmap, vvmap )
             else
                 icg_pot1d(k) = frcshal * finaldwn( icg_pot1d(k), slwmap, pirepmap, vvmap )
             endif
          enddo
      else if( isitu .eq. 6 ) then !! Single-layer clouds; primarily FZDZ,FZRA,PELT at sfc and CTT > -30C
          do k = kbot,ktop            
             tmap = 0.0
             if( dtc(k) .gt. fip_normtcel_temp(1) .and. &
                 dtc(k) .lt. fip_normtcel_temp(NTCL)  ) then !! Table lookup and interp
                                                           !! temperature interest map
                 call loc_interp( 1, NTCL, NTCL, 2.0, dtc(k), fip_normtcel_temp, fip_normtcel_indx, tmap )                                                  
             endif !! Note if dtc(k) is out of the table range, tmap==0

             rhmap = 0.0
             if( rhk(k) .gt. fip_normrh_pct(1) .and. &
                 rhk(k) .lt. fip_normrh_pct(NVRH)  ) then !! Table lookup and interp
                 call loc_interp( 1, NVRH, NVRH, 5.0, rhk(k), fip_normrh_pct, fip_normrh_indx, rhmap ) !! rel. hum. interest map
             endif
             icg_pot1d(k) = cttmap*tmap*rhmap
             sld_pot1d(k) = (tmap + 0.2*pct_radar)*((cttmap - 0.4)/0.6) + 0.25*tmap
             sld_pot1d(k) = MIN( MAX( sld_pot1d(k), 0.0 ), 1.0 )
             
             !! Now apply "boosting factor(s)": 

             pirepmap = 0.0
             slwmap = 0.0
             vvmap = 0.0              
             
             if( dtc(k) .lt. 0.0 ) then
                 if( lwc(k) .ge. LWC_LOWLIM ) then
                    slwmap = 1.0
                 else
                    slwmap = 0.0
                 endif

                 if( vvubs(k) .le. -1.0 ) then
                    vvmap = 1.0
                 else if( vvubs(k) .ge. 1.0 ) then
                    vvmap = -0.075                   
                 else
                    call loc_interp( 1, NVRH, NVRH, 0.1, vvubs(k), fip_normvv_ubars, fip_normvv_indx, vvmap )
                 endif
             endif

             if( vvubs(k) .le. 0.0 ) then !! upward or no vertical velocity
                 icg_pot1d(k) = frcshal * final_up( icg_pot1d(k), slwmap, pirepmap, vvmap )
             else
                 icg_pot1d(k) = frcshal * finaldwn( icg_pot1d(k), slwmap, pirepmap, vvmap )
             endif
          enddo
      else if( isitu .eq. 7 ) then !! Single-layer clouds; primarily FZDZ,FZRA,PELT at sfc and CTT < -30C
          do k = kbot,ktop            
             tmap = 0.0
             if( dtc(k) .gt. fip_normtcel_temp(1) .and. &
                 dtc(k) .lt. fip_normtcel_temp(NTCL)  ) then !! Table lookup and interp
                                                           !! temperature interest map
                 call loc_interp( 1, NTCL, NTCL, 2.0, dtc(k), fip_normtcel_temp, fip_normtcel_indx, tmap )                                                  
             endif !! Note if dtc(k) is out of the table range, tmap==0

             rhmap = 0.0
             if( rhk(k) .gt. fip_normrh_pct(1) .and. &
                 rhk(k) .lt. fip_normrh_pct(NVRH)  ) then !! Table lookup and interp
                 call loc_interp( 1, NVRH, NVRH, 5.0, rhk(k), fip_normrh_pct, fip_normrh_indx, rhmap ) !! rel. hum. interest map
             endif
             icg_pot1d(k) = cttmap*tmap*rhmap
             sld_pot1d(k) = 0.25*tmap
             
             !! Now apply "boosting factor(s)": 
             
             pirepmap = 0.0
             slwmap = 0.0
             vvmap = 0.0              

             if( dtc(k) .lt. 0.0 ) then
                 if( lwc(k) .ge. LWC_LOWLIM ) then
                    slwmap = 1.0
                 else
                    slwmap = 0.0
                 endif

                 if( vvubs(k) .le. -1.0 ) then
                    vvmap = 1.0
                 else if( vvubs(k) .ge. 1.0 ) then
                    vvmap = -0.075                   
                 else
                    call loc_interp( 1, NVRH, NVRH, 0.1, vvubs(k), fip_normvv_ubars, fip_normvv_indx, vvmap )
                 endif
             endif

             if( vvubs(k) .le. 0.0 ) then !! upward or no vertical velocity
                 icg_pot1d(k) = frcshal * final_up( icg_pot1d(k), slwmap, pirepmap, vvmap )
             else
                 icg_pot1d(k) = frcshal * finaldwn( icg_pot1d(k), slwmap, pirepmap, vvmap )
             endif
          enddo
      else if( isitu .eq. 8 ) then !! Single-layer clouds; CTT >= -12C; DZ/RA with T2 > -2C and no SNOW      
          sldfac = SQRT((ctk - 261.0)*D14)  !! by construction, ctk must be >= 261.15
          do k = kbot,ktop            
             tmap = 0.0
             if( dtc(k) .gt. fip_normtcel_temp(1) .and. &
                 dtc(k) .lt. fip_normtcel_temp(NTCL)  ) then !! Table lookup and interp
                                                           !! temperature interest map
                 call loc_interp( 1, NTCL, NTCL, 2.0, dtc(k), fip_normtcel_temp, fip_normtcel_indx, tmap )                                                  
             endif !! Note if dtc(k) is out of the table range, tmap==0

             rhmap = 0.0
             if( rhk(k) .gt. fip_normrh_pct(1) .and. &
                 rhk(k) .lt. fip_normrh_pct(NVRH)  ) then !! Table lookup and interp
                 call loc_interp( 1, NVRH, NVRH, 5.0, rhk(k), fip_normrh_pct, fip_normrh_indx, rhmap ) !! rel. hum. interest map
             endif
             icg_pot1d(k) = cttmap*tmap*rhmap
             sld_pot1d(k) = MIN( tmap*sldfac, 1.0 )
             
             !! Now apply "boosting factor(s)": 

             pirepmap = 0.0
             slwmap = 0.0
             vvmap = 0.0              

             if( dtc(k) .lt. 0.0 ) then
                 if( lwc(k) .ge. LWC_LOWLIM ) then
                    slwmap = 1.0
                 else
                    slwmap = 0.0
                 endif
                 !! print*,'k,lw,map=',k,lwc(k),rhk(k)

                 if( vvubs(k) .le. -1.0 ) then
                    vvmap = 1.0
                 else if( vvubs(k) .ge. 1.0 ) then
                    vvmap = -0.075                   
                 else
                    call loc_interp( 1, NVRH, NVRH, 0.1, vvubs(k), fip_normvv_ubars, fip_normvv_indx, vvmap )
                 endif
             endif

             if( vvubs(k) .le. 0.0 ) then !! upward or no vertical velocity
                 icg_pot1d(k) = frcshal * final_up( icg_pot1d(k), slwmap, pirepmap, vvmap )
             else
                 icg_pot1d(k) = frcshal * finaldwn( icg_pot1d(k), slwmap, pirepmap, vvmap )
             endif
          enddo
      else if( isitu .eq. 24 ) then !! Single-layer clouds; classic warm-nose/FRZRA structure
          sldfac = MAX( (cttmap - 0.7)/0.3, 0.0 )
          do k = kbot,ktop            
             tmap = 0.0
             if( dtc(k) .gt. fip_normtcel_temp(1) .and. &
                 dtc(k) .lt. fip_normtcel_temp(NTCL)  ) then !! Table lookup and interp
                                                           !! temperature interest map
                 call loc_interp( 1, NTCL, NTCL, 2.0, dtc(k), fip_normtcel_temp, fip_normtcel_indx, tmap )                                                  
             endif !! Note if dtc(k) is out of the table range, tmap==0

             rhmap = 0.0
             if( rhk(k) .gt. fip_normrh_pct(1) .and. &
                 rhk(k) .lt. fip_normrh_pct(NVRH)  ) then !! Table lookup and interp
                 call loc_interp( 1, NVRH, NVRH, 5.0, rhk(k), fip_normrh_pct, fip_normrh_indx, rhmap ) !! rel. hum. interest map
             endif
             
             if( k .le. fl+1 ) then  !! at/below the first k-lev with T>0.
                icg_pot1d(k) = MIN( tmap + 0.2*pct_radar*tmap, 1.0 )
                sld_pot1d(k) = icg_pot1d(k)             
             else                    !! above the warm nose
                icg_pot1d(k) = cttmap*tmap*rhmap
                sld_pot1d(k) = MIN( sldfac*(tmap + 0.2*pct_radar), 1.0 )    
             endif

             !! Now apply "boosting factor(s)": 

             pirepmap = 0.0
             slwmap   = 0.0                           
             vvmap = 0.0  

             if( dtc(k) .lt. 0.0 ) then
                 if( lwc(k) .ge. LWC_LOWLIM ) then
                    slwmap = 1.0
                 else
                    slwmap = 0.0
                 endif

                 if( vvubs(k) .le. -1.0 ) then
                    vvmap = 1.0
                 else if( vvubs(k) .ge. 1.0 ) then
                    vvmap = -0.075                   
                 else
                    call loc_interp( 1, NVRH, NVRH, 0.1, vvubs(k), fip_normvv_ubars, fip_normvv_indx, vvmap )
                 endif
             endif

             if( vvubs(k) .le. 0.0 ) then !! upward or no vertical velocity
                 icg_pot1d(k) = final_up( icg_pot1d(k), slwmap, pirepmap, vvmap )
             else
                 icg_pot1d(k) = finaldwn( icg_pot1d(k), slwmap, pirepmap, vvmap )
             endif
          enddo     
      
      endif       
        
      RETURN 
   
END SUBROUTINE SINGLE_LAYCLDS

SUBROUTINE isltd_deep( ldbg, kbot, ktop, rhk, dtc, vvubs, frcmax, icg_pot1d, sld_pot1d,  &
                       kds,kde,                                                    &
                       kms,kme,                                                    &
                       kts,kte )
   
   USE module_model_constants

   IMPLICIT NONE

   !  Input variables

   INTEGER, INTENT(IN   )                                          :: kds,kde, &
                                                                      kms,kme, &
                                                                      kts,kte   
   INTEGER, INTENT(IN   )                                          :: kbot, ktop
   REAL   , INTENT(IN   ) , DIMENSION( kms:kme ) :: dtc, vvubs, rhk
   LOGICAL, INTENT(IN   )                                          :: LDBG       
   
   !  Output variables   
   
   REAL   , INTENT(  OUT)                        :: frcmax
   REAL   , INTENT(  OUT) , DIMENSION( kms:kme ) :: icg_pot1d, sld_pot1d

   !  Local variables
   
   INTEGER :: nlays,k
   REAL    :: cnvtmap, slwmap, pirepmap, vvmap 

   !! Statement Functions
   
   REAL FINAL_UP, INIT_INTERST, SLW_MAP, PIREP_MAP, VV_MAP
   FINAL_UP( INIT_INTERST, SLW_MAP, PIREP_MAP, VV_MAP ) = INIT_INTERST + (1.0 - INIT_INTERST)*    &
                                                      (0.4*SLW_MAP + 0.35*PIREP_MAP + 0.25*VV_MAP)
   REAL FINALDWN                                                      
   FINALDWN( INIT_INTERST, SLW_MAP, PIREP_MAP, VV_MAP ) = INIT_INTERST*(1.0 + 0.25*VV_MAP) + (1.0 - INIT_INTERST)*  &                                                  
                                                      (0.4*SLW_MAP + 0.35*PIREP_MAP)
   !...............................................................................................
   !    

      icg_pot1d( : ) = 0.0
      sld_pot1d( : ) = 0.0

      !! For isolated sub-grid convection, assume fractional coverage is not greater than the 
      !! mean grid-scale rh through the cloud depth      

      frcmax = 0.0
      nlays = ktop-kbot+1
      do k = kbot,ktop
         frcmax = frcmax+rhk(k)
      enddo
      frcmax = 0.01*frcmax/float(nlays)
      
      do k = kbot,ktop
         cnvtmap = 0.0
         if( dtc(k) .gt. fip_normtcel_convtemp(1) .and. &
             dtc(k) .lt. fip_normtcel_convtemp(NTCL)  ) then !! Table lookup and interp
                                                           !! convective temperature interest map
             call loc_interp( 1, NTCL, NTCL, 2.0, dtc(k), fip_normtcel_convtemp, fip_normtcel_convindx, cnvtmap )
             icg_pot1d(k) = cnvtmap
             sld_pot1d(k) = cnvtmap                   
         endif !! Note if dtc(k) is out of the table range, cnvtmap==0      

         !! Now apply "boosting factor(s)": in this case, vertical velocity map to get 
         !! final icg_pot3d. There is no resolved cloud thus no super-cooled liquid interest map. 

         slwmap = 0.0
         pirepmap = 0.0
         vvmap = 0.0               
         
         if( dtc(k) .lt. 0.0 ) then
             if( vvubs(k) .le. -1.0 ) then
                vvmap = 1.0
             else if( vvubs(k) .ge. 1.0 ) then
                vvmap = -0.075                   
             else
                call loc_interp( 1, NVRH, NVRH, 0.1, vvubs(k), fip_normvv_ubars, fip_normvv_indx, vvmap )
             endif
         endif

         if( vvubs(k) .le. 0.0 ) then !! upward or no vertical velocity
             icg_pot1d(k) = frcmax * final_up( icg_pot1d(k), slwmap, pirepmap, vvmap )
         else
             icg_pot1d(k) = frcmax * finaldwn( icg_pot1d(k), slwmap, pirepmap, vvmap )
         endif                                  
      enddo       

      RETURN    

END SUBROUTINE ISLTD_DEEP

SUBROUTINE multpl_layclds( ldbg, isitu,  ml, fl, kbot, ktop, ncilays, kcibas, kcitop, ctop_tc, cbot_tc,           &
                           rhk, dta, dtc, vvubs, lwc, frcmax, icg_pot1d, sld_pot1d,                               &
                           kds,kde,                                                                               &
                           kms,kme,                                                                               &
                           kts,kte )

   USE module_model_constants

   IMPLICIT NONE

   !  Input variables

   INTEGER, INTENT(IN   )                                          :: kds,kde, &
                                                                      kms,kme, &
                                                                      kts,kte   
   INTEGER, INTENT(IN   )                                          :: isitu, ml, fl, kbot, ktop, ncilays
   LOGICAL, INTENT(IN   )                                          :: LDBG   
   INTEGER, INTENT(IN   ) , DIMENSION( ncilays ) :: kcibas, kcitop
   REAL   , INTENT(IN   ) , DIMENSION( ncilays ) :: ctop_tc, cbot_tc
   REAL   , INTENT(IN   ) , DIMENSION( kms:kme ) :: dta, dtc, vvubs, rhk
   REAL   , INTENT(IN   ) , DIMENSION( kms:kme ) :: lwc               !! lwc in kg/kg

   !  Output variables   

   REAL   , INTENT(  OUT)                        :: frcmax   
   REAL   , INTENT(  OUT) , DIMENSION( kms:kme ) :: icg_pot1d, sld_pot1d

   !  Local variables

   REAL, PARAMETER :: D14 = 1.0/14.0
   REAL, PARAMETER :: LWC_LOWLIM = 5.0e-5 !! kg/kg  
   INTEGER         :: nlays,k,l
   REAL            :: ctt, ctk, cnvtmap, slwmap, pirepmap, vvmap, cttmap, tmap, rhmap, pct_radar, sldfac 
   
   CHARACTER*256 MESG
   
   !! Statement Functions
   
   REAL FINAL_UP, INIT_INTERST, SLW_MAP, PIREP_MAP, VV_MAP
   FINAL_UP( INIT_INTERST, SLW_MAP, PIREP_MAP, VV_MAP ) = INIT_INTERST + (1.0 - INIT_INTERST)*    &
                                                      (0.4*SLW_MAP + 0.35*PIREP_MAP + 0.25*VV_MAP)
   REAL FINALDWN                                                      
   FINALDWN( INIT_INTERST, SLW_MAP, PIREP_MAP, VV_MAP ) = INIT_INTERST*(1.0 + 0.25*VV_MAP) + (1.0 - INIT_INTERST)*  &                                                  
                                                      (0.4*SLW_MAP + 0.35*PIREP_MAP)
   !...............................................................................................
   !    

      icg_pot1d( : ) = 0.0
      sld_pot1d( : ) = 0.0
      pct_radar      = 1.0          !! code as in original scheme
      
      cttmap = 0.0
      ctt = ctop_tc(1)
      ctk = dta(kcitop(1))
      if( ctt .gt. fip_normcttcel_temp(1) .and. & 
          ctt .lt. fip_normcttcel_temp(NCTT)  ) then   !! Table lookup and interp
                                                       !! cloudtop-temperature interest map            
          call loc_interp( 1, NCTT, NCTT, 2.0, ctt, fip_normcttcel_temp, fip_normcttcel_indx, cttmap )
      endif 
   
      l = 1

      if( isitu .eq. 14 ) then  !! Multpl-layer clouds lowest deck with no precip or DZ/RA with CTT < -12C
          do k = kbot,ktop
             if( k .gt. kcitop(l) ) then 
                l = l + 1
                cttmap = 0.0
                ctt = ctop_tc(l)
                ctk = dta(kcitop(l))
                if( ctt .gt. fip_normcttcel_temp(1) .and. & 
                    ctt .lt. fip_normcttcel_temp(NCTT)  ) then   !! Table lookup and interp
                                                                !! cloudtop-temperature interest map            
                    call loc_interp( 1, NCTT, NCTT, 2.0, ctt, fip_normcttcel_temp, fip_normcttcel_indx, cttmap )
                endif
             endif
             tmap = 0.0
             if( dtc(k) .gt. fip_normtcel_temp(1) .and. &
                 dtc(k) .lt. fip_normtcel_temp(NTCL)  ) then !! Table lookup and interp
                                                           !! temperature interest map
                 call loc_interp( 1, NTCL, NTCL, 2.0, dtc(k), fip_normtcel_temp, fip_normtcel_indx, tmap )                                                  
             endif !! Note if dtc(k) is out of the table range, tmap==0

             rhmap = 0.0
             if( rhk(k) .gt. fip_normrh_pct(1) .and. &
                 rhk(k) .lt. fip_normrh_pct(NVRH)  ) then !! Table lookup and interp
                 call loc_interp( 1, NVRH, NVRH, 5.0, rhk(k), fip_normrh_pct, fip_normrh_indx, rhmap ) !! rel. hum. interest map
             endif
             icg_pot1d(k) = cttmap*tmap*rhmap
             sld_pot1d(k) = DIAG_MISSING
             
             !! Now apply "boosting factor(s)":  

             pirepmap = 0.0              
             slwmap   = 0.0
             vvmap = 0.0
             
             if( dtc(k) .lt. 0.0 ) then             
                 if( lwc(k) .ge. LWC_LOWLIM ) then
                    slwmap = 1.0
                 else
                    slwmap = 0.0
                 endif

                 if( vvubs(k) .le. -1.0 ) then
                    vvmap = 1.0
                 else if( vvubs(k) .ge. 1.0 ) then
                    vvmap = -0.075                   
                 else
                    call loc_interp( 1, NVRH, NVRH, 0.1, vvubs(k), fip_normvv_ubars, fip_normvv_indx, vvmap )
                 endif
             endif

             if( vvubs(k) .le. 0.0 ) then !! upward or no vertical velocity
                 icg_pot1d(k) = final_up( icg_pot1d(k), slwmap, pirepmap, vvmap )
             else
                 icg_pot1d(k) = finaldwn( icg_pot1d(k), slwmap, pirepmap, vvmap )
             endif
          enddo
      else if(  isitu .eq. 15 ) then !! Multpl-layer clouds lowest deck; SNOW-only at the surface
          do k = kbot,ktop            
             if( k .gt. kcitop(l) ) then 
                l = l + 1
                cttmap = 0.0
                ctt = ctop_tc(l)
                ctk = dta(kcitop(l))
                if( ctt .gt. fip_normcttcel_temp(1) .and. & 
                    ctt .lt. fip_normcttcel_temp(NCTT)  ) then   !! Table lookup and interp
                                                                !! cloudtop-temperature interest map            
                    call loc_interp( 1, NCTT, NCTT, 2.0, ctt, fip_normcttcel_temp, fip_normcttcel_indx, cttmap )
                endif
             endif             
             tmap = 0.0
             if( dtc(k) .gt. fip_normtcel_temp(1) .and. &
                 dtc(k) .lt. fip_normtcel_temp(NTCL)  ) then !! Table lookup and interp
                                                           !! temperature interest map
                 call loc_interp( 1, NTCL, NTCL, 2.0, dtc(k), fip_normtcel_temp, fip_normtcel_indx, tmap )                                                  
             endif !! Note if dtc(k) is out of the table range, tmap==0

             rhmap = 0.0
             if( rhk(k) .gt. fip_normrh_pct(1) .and. &
                 rhk(k) .lt. fip_normrh_pct(NVRH)  ) then !! Table lookup and interp
                 call loc_interp( 1, NVRH, NVRH, 5.0, rhk(k), fip_normrh_pct, fip_normrh_indx, rhmap ) !! rel. hum. interest map
             endif
             icg_pot1d(k) = (0.9-0.6*pct_radar)*cttmap*tmap*rhmap
             sld_pot1d(k) = DIAG_MISSING
             
             !! Now apply "boosting factor(s)":

             pirepmap = 0.0              
             slwmap   = 0.0
             vvmap = 0.0              
             
             if( dtc(k) .lt. 0.0 ) then
                 if( lwc(k) .ge. LWC_LOWLIM ) then
                    slwmap = 1.0
                 else
                    slwmap = 0.0
                 endif

                 if( vvubs(k) .le. -1.0 ) then
                    vvmap = 1.0
                 else if( vvubs(k) .ge. 1.0 ) then
                    vvmap = -0.075                   
                 else
                    call loc_interp( 1, NVRH, NVRH, 0.1, vvubs(k), fip_normvv_ubars, fip_normvv_indx, vvmap )
                 endif
             endif

             if( vvubs(k) .le. 0.0 ) then !! upward or no vertical velocity
                 icg_pot1d(k) = final_up( icg_pot1d(k), slwmap, pirepmap, vvmap )
             else
                 icg_pot1d(k) = finaldwn( icg_pot1d(k), slwmap, pirepmap, vvmap )
             endif
          enddo      
      else if(  isitu .eq. 16 ) then !! Multpl-layer clouds lowest deck; primarily FZDZ,FZRA,PELT at sfc and CTT > -30C      
          do k = kbot,ktop            
             if( k .gt. kcitop(l) ) then 
                l = l + 1
                cttmap = 0.0
                ctt = ctop_tc(l)
                ctk = dta(kcitop(l))
                if( ctt .gt. fip_normcttcel_temp(1) .and. & 
                    ctt .lt. fip_normcttcel_temp(NCTT)  ) then   !! Table lookup and interp
                                                                !! cloudtop-temperature interest map            
                    call loc_interp( 1, NCTT, NCTT, 2.0, ctt, fip_normcttcel_temp, fip_normcttcel_indx, cttmap )
                endif
             endif       
             tmap = 0.0
             if( dtc(k) .gt. fip_normtcel_temp(1) .and. &
                 dtc(k) .lt. fip_normtcel_temp(NTCL)  ) then !! Table lookup and interp
                                                           !! temperature interest map
                 call loc_interp( 1, NTCL, NTCL, 2.0, dtc(k), fip_normtcel_temp, fip_normtcel_indx, tmap )                                                  
             endif !! Note if dtc(k) is out of the table range, tmap==0

             rhmap = 0.0
             if( rhk(k) .gt. fip_normrh_pct(1) .and. &
                 rhk(k) .lt. fip_normrh_pct(NVRH)  ) then !! Table lookup and interp
                 call loc_interp( 1, NVRH, NVRH, 5.0, rhk(k), fip_normrh_pct, fip_normrh_indx, rhmap ) !! rel. hum. interest map
             endif
             icg_pot1d(k) = cttmap*tmap*rhmap

             if( k .gt. kcitop(1) ) then
                sld_pot1d(k) = DIAG_MISSING                     
             else
                sld_pot1d(k) = (tmap + 0.2*pct_radar)*((cttmap - 0.4)/0.6) + 0.25*tmap
                sld_pot1d(k) = MIN( MAX( sld_pot1d(k), 0.0 ), 1.0 )                  
             endif
             
             !! Now apply "boosting factor(s)": 

             pirepmap = 0.0              
             slwmap   = 0.0
             vvmap = 0.0              
             
             if( dtc(k) .lt. 0.0 ) then
                 if( lwc(k) .ge. LWC_LOWLIM ) then
                    slwmap = 1.0
                 else
                    slwmap = 0.0
                 endif

                 if( vvubs(k) .le. -1.0 ) then
                    vvmap = 1.0
                 else if( vvubs(k) .ge. 1.0 ) then
                    vvmap = -0.075                   
                 else
                    call loc_interp( 1, NVRH, NVRH, 0.1, vvubs(k), fip_normvv_ubars, fip_normvv_indx, vvmap )
                 endif
             endif

             if( vvubs(k) .le. 0.0 ) then !! upward or no vertical velocity
                 icg_pot1d(k) = final_up( icg_pot1d(k), slwmap, pirepmap, vvmap )
             else
                 icg_pot1d(k) = finaldwn( icg_pot1d(k), slwmap, pirepmap, vvmap )
             endif
          enddo      
      else if(  isitu .eq. 17 ) then !! Multpl-layer clouds lowest deck; primarily FZDZ,FZRA,PELT at sfc and CTT < -30C 
          do k = kbot,ktop            
             if( k .gt. kcitop(l) ) then 
                l = l + 1
                cttmap = 0.0
                ctt = ctop_tc(l)
                ctk = dta(kcitop(l))
                if( ctt .gt. fip_normcttcel_temp(1) .and. & 
                    ctt .lt. fip_normcttcel_temp(NCTT)  ) then   !! Table lookup and interp
                                                                !! cloudtop-temperature interest map            
                    call loc_interp( 1, NCTT, NCTT, 2.0, ctt, fip_normcttcel_temp, fip_normcttcel_indx, cttmap )
                endif
             endif       
             tmap = 0.0
             if( dtc(k) .gt. fip_normtcel_temp(1) .and. &
                 dtc(k) .lt. fip_normtcel_temp(NTCL)  ) then !! Table lookup and interp
                                                           !! temperature interest map
                 call loc_interp( 1, NTCL, NTCL, 2.0, dtc(k), fip_normtcel_temp, fip_normtcel_indx, tmap )                                                  
             endif !! Note if dtc(k) is out of the table range, tmap==0

             rhmap = 0.0
             if( rhk(k) .gt. fip_normrh_pct(1) .and. &
                 rhk(k) .lt. fip_normrh_pct(NVRH)  ) then !! Table lookup and interp
                 call loc_interp( 1, NVRH, NVRH, 5.0, rhk(k), fip_normrh_pct, fip_normrh_indx, rhmap ) !! rel. hum. interest map
             endif
             icg_pot1d(k) = cttmap*tmap*rhmap
             
             if( k .gt. kcitop(1) ) then
                sld_pot1d(k) = DIAG_MISSING                     
             else
                sld_pot1d(k) = 0.25*tmap                 
             endif
             
             !! Now apply "boosting factor(s)": 
             
             pirepmap = 0.0              
             slwmap   = 0.0
             vvmap = 0.0              
             
             if( dtc(k) .lt. 0.0 ) then
                 if( lwc(k) .ge. LWC_LOWLIM ) then
                    slwmap = 1.0
                 else
                    slwmap = 0.0
                 endif

                 if( vvubs(k) .le. -1.0 ) then
                    vvmap = 1.0
                 else if( vvubs(k) .ge. 1.0 ) then
                    vvmap = -0.075                   
                 else
                    call loc_interp( 1, NVRH, NVRH, 0.1, vvubs(k), fip_normvv_ubars, fip_normvv_indx, vvmap )
                 endif
             endif

             if( vvubs(k) .le. 0.0 ) then !! upward or no vertical velocity
                 icg_pot1d(k) = final_up( icg_pot1d(k), slwmap, pirepmap, vvmap )
             else
                 icg_pot1d(k) = finaldwn( icg_pot1d(k), slwmap, pirepmap, vvmap )
             endif
          enddo      
      else if(  isitu .eq. 18 ) then !! Multpl-layer clouds lowest deck; CTT >= -12C; DZ/RA with T2 > -2C and no SNOW           
          sldfac = SQRT((ctk - 261.0)*D14)  !! by construction, ctk must be >= 261.15          
          do k = kbot,ktop            
             if( k .gt. kcitop(l) ) then 
                l = l + 1
                cttmap = 0.0
                ctt = ctop_tc(l)
                ctk = dta(kcitop(l))
                if( ctt .gt. fip_normcttcel_temp(1) .and. & 
                    ctt .lt. fip_normcttcel_temp(NCTT)  ) then   !! Table lookup and interp
                                                                !! cloudtop-temperature interest map            
                    call loc_interp( 1, NCTT, NCTT, 2.0, ctt, fip_normcttcel_temp, fip_normcttcel_indx, cttmap )
                endif
             endif
             tmap = 0.0
             if( dtc(k) .gt. fip_normtcel_temp(1) .and. &
                 dtc(k) .lt. fip_normtcel_temp(NTCL)  ) then !! Table lookup and interp
                                                           !! temperature interest map
                 call loc_interp( 1, NTCL, NTCL, 2.0, dtc(k), fip_normtcel_temp, fip_normtcel_indx, tmap )                                                  
             endif !! Note if dtc(k) is out of the table range, tmap==0

             rhmap = 0.0
             if( rhk(k) .gt. fip_normrh_pct(1) .and. &
                 rhk(k) .lt. fip_normrh_pct(NVRH)  ) then !! Table lookup and interp
                 call loc_interp( 1, NVRH, NVRH, 5.0, rhk(k), fip_normrh_pct, fip_normrh_indx, rhmap ) !! rel. hum. interest map
             endif
             icg_pot1d(k) = cttmap*tmap*rhmap
             
             if( k .gt. kcitop(1) ) then
                sld_pot1d(k) = DIAG_MISSING                     
             else
                sld_pot1d(k) = MIN( tmap*sldfac, 1.0 )                  
             endif
             
             !! Now apply "boosting factor(s)": 

             pirepmap = 0.0              
             slwmap   = 0.0
             vvmap = 0.0     

             if( dtc(k) .lt. 0.0 ) then
                 if( lwc(k) .ge. LWC_LOWLIM ) then
                    slwmap = 1.0
                 else
                    slwmap = 0.0
                 endif

                 if( vvubs(k) .le. -1.0 ) then
                    vvmap = 1.0
                 else if( vvubs(k) .ge. 1.0 ) then
                    vvmap = -0.075                   
                 else
                    call loc_interp( 1, NVRH, NVRH, 0.1, vvubs(k), fip_normvv_ubars, fip_normvv_indx, vvmap )
                 endif
             endif

             if( vvubs(k) .le. 0.0 ) then !! upward or no vertical velocity
                 icg_pot1d(k) = final_up( icg_pot1d(k), slwmap, pirepmap, vvmap )
             else
                 icg_pot1d(k) = finaldwn( icg_pot1d(k), slwmap, pirepmap, vvmap )
             endif             
          enddo
      else if(  isitu .eq. 25 ) then !! Multiple layer clouds; classic warm-nose/FRZRA structure          
          sldfac = MAX( (cttmap - 0.7)/0.3, 0.0 ) 
          do k = kbot,ktop 
             if( k .gt. kcitop(l) ) then 
                l = l + 1
                cttmap = 0.0
                ctt = ctop_tc(l)
                ctk = dta(kcitop(l))
                if( ctt .gt. fip_normcttcel_temp(1) .and. & 
                    ctt .lt. fip_normcttcel_temp(NCTT)  ) then   !! Table lookup and interp
                                                                !! cloudtop-temperature interest map            
                    call loc_interp( 1, NCTT, NCTT, 2.0, ctt, fip_normcttcel_temp, fip_normcttcel_indx, cttmap )
                endif
                sldfac = MAX( (cttmap - 0.7)/0.3, 0.0 )
             endif      
             tmap = 0.0
             if( dtc(k) .gt. fip_normtcel_temp(1) .and. &
                 dtc(k) .lt. fip_normtcel_temp(NTCL)  ) then !! Table lookup and interp
                                                           !! temperature interest map
                 call loc_interp( 1, NTCL, NTCL, 2.0, dtc(k), fip_normtcel_temp, fip_normtcel_indx, tmap )                                                  
             endif !! Note if dtc(k) is out of the table range, tmap==0

             rhmap = 0.0
             if( rhk(k) .gt. fip_normrh_pct(1) .and. &
                 rhk(k) .lt. fip_normrh_pct(NVRH)  ) then !! Table lookup and interp
                 call loc_interp( 1, NVRH, NVRH, 5.0, rhk(k), fip_normrh_pct, fip_normrh_indx, rhmap )  !! rel. hum. interest map
             endif
             
             if( k .le. fl+1 ) then  !! at/below the first k-lev with T>0.
                icg_pot1d(k) = MIN( tmap + 0.2*pct_radar*tmap, 1.0 )
                sld_pot1d(k) = icg_pot1d(k)          
             else                    !! above the warm nose
                icg_pot1d(k) = cttmap*tmap*rhmap
                sld_pot1d(k) = MIN( sldfac*(tmap + 0.2*pct_radar), 1.0 )
             endif

             !! Now apply "boosting factor(s)": 

             pirepmap = 0.0              
             slwmap   = 0.0
             vvmap = 0.0              
             
             if( dtc(k) .lt. 0.0 ) then
                 if( lwc(k) .ge. LWC_LOWLIM ) then
                    slwmap = 1.0
                 else
                    slwmap = 0.0
                 endif

                 if( vvubs(k) .le. -1.0 ) then
                    vvmap = 1.0
                 else if( vvubs(k) .ge. 1.0 ) then
                    vvmap = -0.075                   
                 else
                    call loc_interp( 1, NVRH, NVRH, 0.1, vvubs(k), fip_normvv_ubars, fip_normvv_indx, vvmap )
                 endif
             endif

             if( vvubs(k) .le. 0.0 ) then !! upward or no vertical velocity
                 icg_pot1d(k) = final_up( icg_pot1d(k), slwmap, pirepmap, vvmap )
             else
                 icg_pot1d(k) = finaldwn( icg_pot1d(k), slwmap, pirepmap, vvmap )
             endif
          enddo      
                   
      endif       
        
      RETURN 
   
END SUBROUTINE MULTPL_LAYCLDS

SUBROUTINE single_laycttg( ldbg, isitu, ml, fl, kbot, ktop, kbot_cnvct, ktop_cnvct, rhk, dta, dtc, vvubs,    & 
                           lwc, frcmax, icg_pot1d, sld_pot1d,                                          &
                           kds,kde,                                                                    &
                           kms,kme,                                                                    &
                           kts,kte )

   USE module_model_constants

   IMPLICIT NONE

   !  Input variables

   INTEGER, INTENT(IN   )                                          :: kds,kde, &
                                                                      kms,kme, &
                                                                      kts,kte   
   INTEGER, INTENT(IN   )                                          :: isitu, ml, fl, kbot, ktop
   INTEGER, INTENT(IN   )                                          :: kbot_cnvct, ktop_cnvct
   LOGICAL, INTENT(IN   )                                          :: LDBG
   REAL   , INTENT(IN   ) , DIMENSION( kms:kme ) :: dta, dtc, vvubs, rhk
   REAL   , INTENT(IN   ) , DIMENSION( kms:kme ) :: lwc               !! lwc in kg/kg

   !  Output variables   

   REAL   , INTENT(  OUT)                        :: frcmax   
   REAL   , INTENT(  OUT) , DIMENSION( kms:kme ) :: icg_pot1d, sld_pot1d

   !  Local variables

   REAL, PARAMETER :: D14 = 1.0/14.0
   REAL, PARAMETER :: LWC_LOWLIM = 5.0e-5 !! kg/kg  
   INTEGER         :: nlays,k
   REAL            :: ctt, ctk, cnvtmap, slwmap, pirepmap, vvmap, cttmap, tmap, rhmap, pct_radar, sldfac, frcshal 
   REAL, DIMENSION( kms:kme ) :: icg_pot1d_cnvct, sld_pot1d_cnvct    
   
   CHARACTER*256 MESG
   
   !! Statement Functions
   
   REAL FINAL_UP, INIT_INTERST, SLW_MAP, PIREP_MAP, VV_MAP
   FINAL_UP( INIT_INTERST, SLW_MAP, PIREP_MAP, VV_MAP ) = INIT_INTERST + (1.0 - INIT_INTERST)*    &
                                                      (0.4*SLW_MAP + 0.35*PIREP_MAP + 0.25*VV_MAP)
   REAL FINALDWN                                                      
   FINALDWN( INIT_INTERST, SLW_MAP, PIREP_MAP, VV_MAP ) = INIT_INTERST*(1.0 + 0.25*VV_MAP) + (1.0 - INIT_INTERST)*  &                                                  
                                                      (0.4*SLW_MAP + 0.35*PIREP_MAP)
   !...............................................................................................
   !    

      icg_pot1d( : ) = 0.0
      sld_pot1d( : ) = 0.0
      icg_pot1d_cnvct( : ) = 0.0
      sld_pot1d_cnvct( : ) = 0.0      
      pct_radar      = 1.0          !! code as in original scheme 

      !! For sub-grid convection, assume fractional coverage is not greater than the 
      !! mean grid-scale rh through the convective cloud depth

      frcmax = 0.0
      nlays = ktop_cnvct-kbot_cnvct+1
      do k = kbot_cnvct,ktop_cnvct
         frcmax = frcmax+rhk(k)
      enddo
      frcmax = 0.01*frcmax/float(nlays)
            
      !! Compute the convective contribution first     
      
      do k = kbot_cnvct,ktop_cnvct
         cnvtmap = 0.0
         if( dtc(k) .gt. fip_normtcel_convtemp(1) .and. &
             dtc(k) .lt. fip_normtcel_convtemp(NTCL)  ) then !! Table lookup and interp
                                                           !! convective temperature interest map
             call loc_interp( 1, NTCL, NTCL, 2.0, dtc(k), fip_normtcel_convtemp, fip_normtcel_convindx, cnvtmap )
             icg_pot1d_cnvct(k) = cnvtmap
             sld_pot1d_cnvct(k) = cnvtmap       
         
         endif !! Note if dtc(k) is out of the table range, cnvtmap==0      

         !! Now apply "boosting factor(s)": 

         pirepmap = 0.0              
         slwmap   = 0.0
         vvmap = 0.0
         
         if( dtc(k) .lt. 0.0 ) then
             if( lwc(k) .ge. LWC_LOWLIM ) then
                slwmap = 1.0
             else
                slwmap = 0.0
             endif             

             if( vvubs(k) .le. -1.0 ) then
                vvmap = 1.0
             else if( vvubs(k) .ge. 1.0 ) then
                vvmap = -0.075                   
             else
                call loc_interp( 1, NVRH, NVRH, 0.1, vvubs(k), fip_normvv_ubars, fip_normvv_indx, vvmap )
             endif
         endif
         
         if( vvubs(k) .le. 0.0 ) then !! upward or no vertical velocity
             icg_pot1d_cnvct(k) = frcmax * final_up( icg_pot1d_cnvct(k), slwmap, pirepmap, vvmap )              
         else 
             icg_pot1d_cnvct(k) = frcmax * finaldwn( icg_pot1d_cnvct(k), slwmap, pirepmap, vvmap )                
         endif
      
      enddo             
           
      !! Now compute the resolved scale contribution
      
      cttmap = 0.0
      ctt = dtc(ktop)
      ctk = dta(ktop)
      if( ctt .gt. fip_normcttcel_temp(1) .and. & 
          ctt .lt. fip_normcttcel_temp(NCTT)  ) then   !! Table lookup and interp
                                                       !! cloudtop-temperature interest map            
          call loc_interp( 1, NCTT, NCTT, 2.0, ctt, fip_normcttcel_temp, fip_normcttcel_indx, cttmap )
      endif 
   
      if( isitu .eq. 9 .or. isitu .eq. 91 ) then  !! single-layer no precip or DZ/RA with CTT < -12C

          do k = kbot,ktop            
             tmap = 0.0
             if( dtc(k) .gt. fip_normtcel_temp(1) .and. &
                 dtc(k) .lt. fip_normtcel_temp(NTCL)  ) then !! Table lookup and interp
                                                           !! temperature interest map
                 call loc_interp( 1, NTCL, NTCL, 2.0, dtc(k), fip_normtcel_temp, fip_normtcel_indx, tmap )                                                  
             endif !! Note if dtc(k) is out of the table range, tmap==0      
             
             rhmap = 0.0
             if( rhk(k) .gt. fip_normrh_pct(1) .and. &
                 rhk(k) .lt. fip_normrh_pct(NVRH)  ) then !! Table lookup and interp
                 call loc_interp( 1, NVRH, NVRH, 5.0, rhk(k), fip_normrh_pct, fip_normrh_indx, rhmap ) !! rel. hum. interest map
             endif
             icg_pot1d(k) = cttmap*tmap*rhmap
             sld_pot1d(k) = DIAG_MISSING
             
             !! Now apply "boosting factor(s)":  

             pirepmap = 0.0              
             slwmap   = 0.0
             vvmap = 0.0              
             
             if( dtc(k) .lt. 0.0 ) then
                 if( lwc(k) .ge. LWC_LOWLIM ) then
                    slwmap = 1.0
                 else
                    slwmap = 0.0
                 endif

                 if( vvubs(k) .le. -1.0 ) then
                    vvmap = 1.0
                 else if( vvubs(k) .ge. 1.0 ) then
                    vvmap = -0.075                   
                 else
                    call loc_interp( 1, NVRH, NVRH, 0.1, vvubs(k), fip_normvv_ubars, fip_normvv_indx, vvmap )
                 endif
             endif
                         
             if( vvubs(k) .le. 0.0 ) then !! upward or no vertical velocity
                 icg_pot1d(k) = final_up( icg_pot1d(k), slwmap, pirepmap, vvmap )
             else
                 icg_pot1d(k) = finaldwn( icg_pot1d(k), slwmap, pirepmap, vvmap )
             endif
                                   
          enddo      
      else if( isitu .eq. 10 .or. isitu .eq. 101 ) then  !! single-layer, All snow
          do k = kbot,ktop            
             tmap = 0.0
             if( dtc(k) .gt. fip_normtcel_temp(1) .and. &
                 dtc(k) .lt. fip_normtcel_temp(NTCL)  ) then !! Table lookup and interp
                                                           !! temperature interest map
                 call loc_interp( 1, NTCL, NTCL, 2.0, dtc(k), fip_normtcel_temp, fip_normtcel_indx, tmap )                                                  
             endif !! Note if dtc(k) is out of the table range, tmap==0

             rhmap = 0.0
             if( rhk(k) .gt. fip_normrh_pct(1) .and. &
                 rhk(k) .lt. fip_normrh_pct(NVRH)  ) then !! Table lookup and interp
                 call loc_interp( 1, NVRH, NVRH, 5.0, rhk(k), fip_normrh_pct, fip_normrh_indx, rhmap ) !! rel. hum. interest map
             endif
             icg_pot1d(k) = (0.9-0.6*pct_radar)*cttmap*tmap*rhmap
             sld_pot1d(k) = DIAG_MISSING
             
             !! Now apply "boosting factor(s)":

             pirepmap = 0.0              
             slwmap   = 0.0
             vvmap = 0.0              
             
             if( dtc(k) .lt. 0.0 ) then
                 if( lwc(k) .ge. LWC_LOWLIM ) then
                    slwmap = 1.0
                 else
                    slwmap = 0.0
                 endif

                 if( vvubs(k) .le. -1.0 ) then
                    vvmap = 1.0
                 else if( vvubs(k) .ge. 1.0 ) then
                    vvmap = -0.075                   
                 else
                    call loc_interp( 1, NVRH, NVRH, 0.1, vvubs(k), fip_normvv_ubars, fip_normvv_indx, vvmap )
                 endif
             endif

             if( vvubs(k) .le. 0.0 ) then !! upward or no vertical velocity
                 icg_pot1d(k) = final_up( icg_pot1d(k), slwmap, pirepmap, vvmap )
             else
                 icg_pot1d(k) = finaldwn( icg_pot1d(k), slwmap, pirepmap, vvmap )
             endif
          enddo
      else if( isitu .eq. 11 .or. isitu .eq. 111 ) then !! Single-layer clouds; primarily FZDZ,FZRA,PELT at sfc and CTT > -30C
          do k = kbot,ktop            
             tmap = 0.0
             if( dtc(k) .gt. fip_normtcel_temp(1) .and. &
                 dtc(k) .lt. fip_normtcel_temp(NTCL)  ) then !! Table lookup and interp
                                                           !! temperature interest map
                 call loc_interp( 1, NTCL, NTCL, 2.0, dtc(k), fip_normtcel_temp, fip_normtcel_indx, tmap )                                                  
             endif !! Note if dtc(k) is out of the table range, tmap==0

             rhmap = 0.0
             if( rhk(k) .gt. fip_normrh_pct(1) .and. &
                 rhk(k) .lt. fip_normrh_pct(NVRH)  ) then !! Table lookup and interp
                 call loc_interp( 1, NVRH, NVRH, 5.0, rhk(k), fip_normrh_pct, fip_normrh_indx, rhmap ) !! rel. hum. interest map
             endif
             icg_pot1d(k) = cttmap*tmap*rhmap
             sld_pot1d(k) = (tmap + 0.2*pct_radar)*((cttmap - 0.4)/0.6) + 0.25*tmap
             sld_pot1d(k) = MIN( MAX( sld_pot1d(k), 0.0 ), 1.0 )
             
             !! Now apply "boosting factor(s)": 

             pirepmap = 0.0              
             slwmap   = 0.0
             vvmap = 0.0              
             
             if( dtc(k) .lt. 0.0 ) then
                 if( lwc(k) .ge. LWC_LOWLIM ) then
                    slwmap = 1.0
                 else
                    slwmap = 0.0
                 endif

                 if( vvubs(k) .le. -1.0 ) then
                    vvmap = 1.0
                 else if( vvubs(k) .ge. 1.0 ) then
                    vvmap = -0.075                   
                 else
                    call loc_interp( 1, NVRH, NVRH, 0.1, vvubs(k), fip_normvv_ubars, fip_normvv_indx, vvmap )
                 endif
             endif

             if( vvubs(k) .le. 0.0 ) then !! upward or no vertical velocity
                 icg_pot1d(k) = final_up( icg_pot1d(k), slwmap, pirepmap, vvmap )
             else
                 icg_pot1d(k) = finaldwn( icg_pot1d(k), slwmap, pirepmap, vvmap )
             endif
          enddo
      else if( isitu .eq. 12 .or. isitu .eq. 121 ) then !! Single-layer clouds; primarily FZDZ,FZRA,PELT at sfc and CTT < -30C
          do k = kbot,ktop            
             tmap = 0.0
             if( dtc(k) .gt. fip_normtcel_temp(1) .and. &
                 dtc(k) .lt. fip_normtcel_temp(NTCL)  ) then !! Table lookup and interp
                                                           !! temperature interest map
                 call loc_interp( 1, NTCL, NTCL, 2.0, dtc(k), fip_normtcel_temp, fip_normtcel_indx, tmap )                                                  
             endif !! Note if dtc(k) is out of the table range, tmap==0

             rhmap = 0.0
             if( rhk(k) .gt. fip_normrh_pct(1) .and. &
                 rhk(k) .lt. fip_normrh_pct(NVRH)  ) then !! Table lookup and interp
                 call loc_interp( 1, NVRH, NVRH, 5.0, rhk(k), fip_normrh_pct, fip_normrh_indx, rhmap ) !! rel. hum. interest map
             endif
             icg_pot1d(k) = cttmap*tmap*rhmap
             sld_pot1d(k) = 0.25*tmap
             
             !! Now apply "boosting factor(s)": 
             
             pirepmap = 0.0              
             slwmap   = 0.0
             vvmap = 0.0
             
             if( dtc(k) .lt. 0.0 ) then   
                 if( lwc(k) .ge. LWC_LOWLIM ) then
                    slwmap = 1.0
                 else
                    slwmap = 0.0
                 endif

                 if( vvubs(k) .le. -1.0 ) then
                    vvmap = 1.0
                 else if( vvubs(k) .ge. 1.0 ) then
                    vvmap = -0.075                   
                 else
                    call loc_interp( 1, NVRH, NVRH, 0.1, vvubs(k), fip_normvv_ubars, fip_normvv_indx, vvmap )
                 endif
             endif

             if( vvubs(k) .le. 0.0 ) then !! upward or no vertical velocity
                 icg_pot1d(k) = final_up( icg_pot1d(k), slwmap, pirepmap, vvmap )
             else
                 icg_pot1d(k) = finaldwn( icg_pot1d(k), slwmap, pirepmap, vvmap )
             endif
          enddo
      else if( isitu .eq. 13 .or. isitu .eq. 131 ) then !! Single-layer clouds; CTT >= -12C; DZ/RA with T2 > -2C and no SNOW      
          sldfac = SQRT((ctk - 261.0)*D14)  !! by construction, ctk must be >= 261.15
          do k = kbot,ktop            
             tmap = 0.0
             if( dtc(k) .gt. fip_normtcel_temp(1) .and. &
                 dtc(k) .lt. fip_normtcel_temp(NTCL)  ) then !! Table lookup and interp
                                                           !! temperature interest map
                 call loc_interp( 1, NTCL, NTCL, 2.0, dtc(k), fip_normtcel_temp, fip_normtcel_indx, tmap )                                                  
             endif !! Note if dtc(k) is out of the table range, tmap==0

             rhmap = 0.0
             if( rhk(k) .gt. fip_normrh_pct(1) .and. &
                 rhk(k) .lt. fip_normrh_pct(NVRH)  ) then !! Table lookup and interp
                 call loc_interp( 1, NVRH, NVRH, 5.0, rhk(k), fip_normrh_pct, fip_normrh_indx, rhmap ) !! rel. hum. interest map
             endif
             icg_pot1d(k) = cttmap*tmap*rhmap
             sld_pot1d(k) = MIN( tmap*sldfac, 1.0 )
             
             !! Now apply "boosting factor(s)": 

             pirepmap = 0.0              
             slwmap   = 0.0
             vvmap = 0.0              
             
             if( dtc(k) .lt. 0.0 ) then
                 if( lwc(k) .ge. LWC_LOWLIM ) then
                    slwmap = 1.0
                 else
                    slwmap = 0.0
                 endif
                 !! print*,'k,lw,map=',k,lwc(k),rhk(k)

                 if( vvubs(k) .le. -1.0 ) then
                    vvmap = 1.0
                 else if( vvubs(k) .ge. 1.0 ) then
                    vvmap = -0.075                   
                 else
                    call loc_interp( 1, NVRH, NVRH, 0.1, vvubs(k), fip_normvv_ubars, fip_normvv_indx, vvmap )
                 endif
             endif

             if( vvubs(k) .le. 0.0 ) then !! upward or no vertical velocity
                 icg_pot1d(k) = final_up( icg_pot1d(k), slwmap, pirepmap, vvmap )
             else
                 icg_pot1d(k) = finaldwn( icg_pot1d(k), slwmap, pirepmap, vvmap )
             endif
          enddo
      else if( isitu .eq. 26 .or. isitu .eq. 261 ) then !! Single-layer clouds; classic warm-nose/FRZRA structure
          sldfac = MAX( (cttmap - 0.7)/0.3, 0.0 )
          do k = kbot,ktop            
             tmap = 0.0
             if( dtc(k) .gt. fip_normtcel_temp(1) .and. &
                 dtc(k) .lt. fip_normtcel_temp(NTCL)  ) then !! Table lookup and interp
                                                           !! temperature interest map
                 call loc_interp( 1, NTCL, NTCL, 2.0, dtc(k), fip_normtcel_temp, fip_normtcel_indx, tmap )                                                  
             endif !! Note if dtc(k) is out of the table range, tmap==0

             rhmap = 0.0
             if( rhk(k) .gt. fip_normrh_pct(1) .and. &
                 rhk(k) .lt. fip_normrh_pct(NVRH)  ) then !! Table lookup and interp
                 call loc_interp( 1, NVRH, NVRH, 5.0, rhk(k), fip_normrh_pct, fip_normrh_indx, rhmap ) !! rel. hum. interest map
             endif
             
             if( k .le. fl+1 ) then  !! at/below the first k-lev with T>0.
                icg_pot1d(k) = MIN( tmap + 0.2*pct_radar*tmap, 1.0 )
                sld_pot1d(k) = icg_pot1d(k)           
             else                    !! above the warm nose
                icg_pot1d(k) = cttmap*tmap*rhmap
                sld_pot1d(k) = MIN( sldfac*(tmap + 0.2*pct_radar), 1.0 )                   
             endif

             !! Now apply "boosting factor(s)": 

             pirepmap = 0.0              
             slwmap   = 0.0
             vvmap = 0.0              
             
             if( dtc(k) .lt. 0.0 ) then
                 if( lwc(k) .ge. LWC_LOWLIM ) then
                    slwmap = 1.0
                 else
                    slwmap = 0.0
                 endif

                 if( vvubs(k) .le. -1.0 ) then
                    vvmap = 1.0
                 else if( vvubs(k) .ge. 1.0 ) then
                    vvmap = -0.075                   
                 else
                    call loc_interp( 1, NVRH, NVRH, 0.1, vvubs(k), fip_normvv_ubars, fip_normvv_indx, vvmap )
                 endif
             endif

             if( vvubs(k) .le. 0.0 ) then !! upward or no vertical velocity
                 icg_pot1d(k) = final_up( icg_pot1d(k), slwmap, pirepmap, vvmap )
             else
                 icg_pot1d(k) = finaldwn( icg_pot1d(k), slwmap, pirepmap, vvmap )
             endif
          enddo     
      endif       
        
      !! Combine the two estimates
            
      do k = MIN( kbot,kbot_cnvct ), MAX( ktop,ktop_cnvct )  
         icg_pot1d(k) = MAX( icg_pot1d(k),icg_pot1d_cnvct(k) )
         if( sld_pot1d_cnvct(k) .gt. 0.0 ) then
             sld_pot1d(k) = MAX( sld_pot1d(k),sld_pot1d_cnvct(k) )
         endif
      enddo
      
      RETURN 
   
END SUBROUTINE SINGLE_LAYCTTG

SUBROUTINE multpl_laycttg( ldbg, isitu,  ml, fl, kbot, ktop, kbot_cnvct, ktop_cnvct, ncilays, kcibas, kcitop,           &
                           ctop_tc, cbot_tc, rhk, dta, dtc, vvubs, lwc, frcmax, icg_pot1d, sld_pot1d,             &
                           kds,kde,                                                                               &
                           kms,kme,                                                                               &
                           kts,kte )

   USE module_model_constants

   IMPLICIT NONE

   !  Input variables

   INTEGER, INTENT(IN   )                                          :: kds,kde, &
                                                                      kms,kme, &
                                                                      kts,kte   
   INTEGER, INTENT(IN   )                                          :: isitu, ml, fl, kbot, ktop, ncilays
   INTEGER, INTENT(IN   )                                          :: kbot_cnvct, ktop_cnvct   
   LOGICAL, INTENT(IN   )                                          :: LDBG
   INTEGER, INTENT(IN   ) , DIMENSION( ncilays ) :: kcibas, kcitop
   REAL   , INTENT(IN   ) , DIMENSION( ncilays ) :: ctop_tc, cbot_tc
   REAL   , INTENT(IN   ) , DIMENSION( kms:kme ) :: dta, dtc, vvubs, rhk
   REAL   , INTENT(IN   ) , DIMENSION( kms:kme ) :: lwc               !! lwc in kg/kg

   !  Output variables   

   REAL   , INTENT(  OUT)                        :: frcmax   
   REAL   , INTENT(  OUT) , DIMENSION( kms:kme ) :: icg_pot1d, sld_pot1d

   !  Local variables

   REAL, PARAMETER :: D14 = 1.0/14.0
   REAL, PARAMETER :: LWC_LOWLIM = 5.0e-5 !! kg/kg  
   INTEGER         :: nlays,k,l
   REAL            :: ctt, ctk, cnvtmap, slwmap, pirepmap, vvmap, cttmap, tmap, rhmap, pct_radar, sldfac 
   REAL, DIMENSION( kms:kme ) :: icg_pot1d_cnvct, sld_pot1d_cnvct   
   
   CHARACTER*256 MESG
   
   !! Statement Functions
   
   REAL FINAL_UP, INIT_INTERST, SLW_MAP, PIREP_MAP, VV_MAP
   FINAL_UP( INIT_INTERST, SLW_MAP, PIREP_MAP, VV_MAP ) = INIT_INTERST + (1.0 - INIT_INTERST)*    &
                                                      (0.4*SLW_MAP + 0.35*PIREP_MAP + 0.25*VV_MAP)
   REAL FINALDWN                                                      
   FINALDWN( INIT_INTERST, SLW_MAP, PIREP_MAP, VV_MAP ) = INIT_INTERST*(1.0 + 0.25*VV_MAP) + (1.0 - INIT_INTERST)*  &                                                  
                                                      (0.4*SLW_MAP + 0.35*PIREP_MAP)
   !...............................................................................................
   !    

      icg_pot1d( : ) = 0.0
      sld_pot1d( : ) = 0.0
      icg_pot1d_cnvct( : ) = 0.0
      sld_pot1d_cnvct( : ) = 0.0      
      pct_radar      = 1.0          !! code as in original scheme
      
      !! For sub-grid convection, assume fractional coverage is not greater than the 
      !! mean grid-scale rh through the cloud depth

      frcmax = 0.0
      nlays = ktop_cnvct-kbot_cnvct+1
      do k = kbot_cnvct,ktop_cnvct
         frcmax = frcmax+rhk(k)
      enddo
      frcmax = 0.01*frcmax/float(nlays)
            
      !! Compute the convective contribution first     
     
      do k = kbot_cnvct,ktop_cnvct
         cnvtmap = 0.0
         if( dtc(k) .gt. fip_normtcel_convtemp(1) .and. &
             dtc(k) .lt. fip_normtcel_convtemp(NTCL)  ) then !! Table lookup and interp
                                                             !! convective temperature interest map
             call loc_interp( 1, NTCL, NTCL, 2.0, dtc(k), fip_normtcel_convtemp, fip_normtcel_convindx, cnvtmap )
             icg_pot1d_cnvct(k) = cnvtmap
             sld_pot1d_cnvct(k) = cnvtmap                  
         endif !! Note if dtc(k) is out of the table range, cnvtmap==0      

         !! Now apply "boosting factor(s)": 

         pirepmap = 0.0              
         slwmap   = 0.0
         vvmap = 0.0              
             
         if( dtc(k) .lt. 0.0 ) then
             if( lwc(k) .ge. LWC_LOWLIM ) then
                slwmap = 1.0
             else
                slwmap = 0.0
             endif             

             if( vvubs(k) .le. -1.0 ) then
                vvmap = 1.0
             else if( vvubs(k) .ge. 1.0 ) then
                vvmap = -0.075                   
             else
                call loc_interp( 1, NVRH, NVRH, 0.1, vvubs(k), fip_normvv_ubars, fip_normvv_indx, vvmap )
             endif
         endif

         if( vvubs(k) .le. 0.0 ) then !! upward or no vertical velocity
             icg_pot1d_cnvct(k) = frcmax * final_up( icg_pot1d_cnvct(k), slwmap, pirepmap, vvmap )
         else
             icg_pot1d_cnvct(k) = frcmax * finaldwn( icg_pot1d_cnvct(k), slwmap, pirepmap, vvmap )
         endif                         
      enddo             
           
      !! Now compute the resolved scale contribution      
            
      cttmap = 0.0
      ctt = ctop_tc(1)
      ctk = dta(kcitop(1))
      if( ctt .gt. fip_normcttcel_temp(1) .and. & 
          ctt .lt. fip_normcttcel_temp(NCTT)  ) then   !! Table lookup and interp
                                                       !! cloudtop-temperature interest map            
          call loc_interp( 1, NCTT, NCTT, 2.0, ctt, fip_normcttcel_temp, fip_normcttcel_indx, cttmap )
      endif 
   
      l = 1
      
      if( isitu .eq. 19 .or. isitu .eq. 191 ) then  !! Multpl-layer clouds lowest deck with no precip or DZ/RA with CTT < -12C
          do k = kbot,ktop
             if( k .gt. kcitop(l) ) then 
                l = l + 1
                cttmap = 0.0
                ctt = ctop_tc(l)
                ctk = dta(kcitop(l))
                if( ctt .gt. fip_normcttcel_temp(1) .and. & 
                    ctt .lt. fip_normcttcel_temp(NCTT)  ) then   !! Table lookup and interp
                                                                !! cloudtop-temperature interest map            
                    call loc_interp( 1, NCTT, NCTT, 2.0, ctt, fip_normcttcel_temp, fip_normcttcel_indx, cttmap )
                endif
             endif
             tmap = 0.0
             if( dtc(k) .gt. fip_normtcel_temp(1) .and. &
                 dtc(k) .lt. fip_normtcel_temp(NTCL)  ) then !! Table lookup and interp
                                                           !! temperature interest map
                 call loc_interp( 1, NTCL, NTCL, 2.0, dtc(k), fip_normtcel_temp, fip_normtcel_indx, tmap )                                                  
             endif !! Note if dtc(k) is out of the table range, tmap==0

             rhmap = 0.0
             if( rhk(k) .gt. fip_normrh_pct(1) .and. &
                 rhk(k) .lt. fip_normrh_pct(NVRH)  ) then !! Table lookup and interp
                 call loc_interp( 1, NVRH, NVRH, 5.0, rhk(k), fip_normrh_pct, fip_normrh_indx, rhmap ) !! rel. hum. interest map
             endif
             icg_pot1d(k) = cttmap*tmap*rhmap
             sld_pot1d(k) = DIAG_MISSING
             
             !! Now apply "boosting factor(s)":  

             pirepmap = 0.0              
             slwmap   = 0.0
             vvmap = 0.0              
             
             if( dtc(k) .lt. 0.0 ) then
                 if( lwc(k) .ge. LWC_LOWLIM ) then
                    slwmap = 1.0
                 else
                    slwmap = 0.0
                 endif

                 if( vvubs(k) .le. -1.0 ) then
                    vvmap = 1.0
                 else if( vvubs(k) .ge. 1.0 ) then
                    vvmap = -0.075                   
                 else
                    call loc_interp( 1, NVRH, NVRH, 0.1, vvubs(k), fip_normvv_ubars, fip_normvv_indx, vvmap )
                 endif
             endif

             if( vvubs(k) .le. 0.0 ) then !! upward or no vertical velocity
                 icg_pot1d(k) = final_up( icg_pot1d(k), slwmap, pirepmap, vvmap )
             else
                 icg_pot1d(k) = finaldwn( icg_pot1d(k), slwmap, pirepmap, vvmap )
             endif
          enddo
      else if(  isitu .eq. 20 .or. isitu .eq. 201 ) then !! Multpl-layer clouds lowest deck; SNOW-only at the surface
          do k = kbot,ktop            
             if( k .gt. kcitop(l) ) then 
                l = l + 1
                cttmap = 0.0
                ctt = ctop_tc(l)
                ctk = dta(kcitop(l))
                if( ctt .gt. fip_normcttcel_temp(1) .and. & 
                    ctt .lt. fip_normcttcel_temp(NCTT)  ) then   !! Table lookup and interp
                                                                !! cloudtop-temperature interest map            
                    call loc_interp( 1, NCTT, NCTT, 2.0, ctt, fip_normcttcel_temp, fip_normcttcel_indx, cttmap )
                endif
             endif             
             tmap = 0.0
             if( dtc(k) .gt. fip_normtcel_temp(1) .and. &
                 dtc(k) .lt. fip_normtcel_temp(NTCL)  ) then !! Table lookup and interp
                                                           !! temperature interest map
                 call loc_interp( 1, NTCL, NTCL, 2.0, dtc(k), fip_normtcel_temp, fip_normtcel_indx, tmap )                                                  
             endif !! Note if dtc(k) is out of the table range, tmap==0

             rhmap = 0.0
             if( rhk(k) .gt. fip_normrh_pct(1) .and. &
                 rhk(k) .lt. fip_normrh_pct(NVRH)  ) then !! Table lookup and interp
                 call loc_interp( 1, NVRH, NVRH, 5.0, rhk(k), fip_normrh_pct, fip_normrh_indx, rhmap ) !! rel. hum. interest map
             endif
             icg_pot1d(k) = (0.9-0.6*pct_radar)*cttmap*tmap*rhmap
             sld_pot1d(k) = DIAG_MISSING
             
             !! Now apply "boosting factor(s)":

             pirepmap = 0.0              
             slwmap   = 0.0
             vvmap = 0.0              
             
             if( dtc(k) .lt. 0.0 ) then
                 if( lwc(k) .ge. LWC_LOWLIM ) then
                    slwmap = 1.0
                 else
                    slwmap = 0.0
                 endif

                 if( vvubs(k) .le. -1.0 ) then
                    vvmap = 1.0
                 else if( vvubs(k) .ge. 1.0 ) then
                    vvmap = -0.075                   
                 else
                    call loc_interp( 1, NVRH, NVRH, 0.1, vvubs(k), fip_normvv_ubars, fip_normvv_indx, vvmap )
                 endif
             endif

             if( vvubs(k) .le. 0.0 ) then !! upward or no vertical velocity
                 icg_pot1d(k) = final_up( icg_pot1d(k), slwmap, pirepmap, vvmap )
             else
                 icg_pot1d(k) = finaldwn( icg_pot1d(k), slwmap, pirepmap, vvmap )
             endif
          enddo      
      else if(  isitu .eq. 21 .or. isitu .eq. 211 ) then !! Multpl-layer clouds lowest deck; primarily FZDZ,FZRA,PELT at sfc and CTT > -30C
          do k = kbot,ktop            
             if( k .gt. kcitop(l) ) then 
                l = l + 1
                cttmap = 0.0
                ctt = ctop_tc(l)
                ctk = dta(kcitop(l))
                if( ctt .gt. fip_normcttcel_temp(1) .and. & 
                    ctt .lt. fip_normcttcel_temp(NCTT)  ) then   !! Table lookup and interp
                                                                !! cloudtop-temperature interest map            
                    call loc_interp( 1, NCTT, NCTT, 2.0, ctt, fip_normcttcel_temp, fip_normcttcel_indx, cttmap )
                endif
             endif       
             tmap = 0.0
             if( dtc(k) .gt. fip_normtcel_temp(1) .and. &
                 dtc(k) .lt. fip_normtcel_temp(NTCL)  ) then !! Table lookup and interp
                                                           !! temperature interest map
                 call loc_interp( 1, NTCL, NTCL, 2.0, dtc(k), fip_normtcel_temp, fip_normtcel_indx, tmap )                                                  
             endif !! Note if dtc(k) is out of the table range, tmap==0

             rhmap = 0.0
             if( rhk(k) .gt. fip_normrh_pct(1) .and. &
                 rhk(k) .lt. fip_normrh_pct(NVRH)  ) then !! Table lookup and interp
                 call loc_interp( 1, NVRH, NVRH, 5.0, rhk(k), fip_normrh_pct, fip_normrh_indx, rhmap ) !! rel. hum. interest map
             endif
             icg_pot1d(k) = cttmap*tmap*rhmap

             if( k .gt. kcitop(1) ) then
                sld_pot1d(k) = DIAG_MISSING          
             else
                sld_pot1d(k) = (tmap + 0.2*pct_radar)*((cttmap - 0.4)/0.6) + 0.25*tmap
                sld_pot1d(k) = MIN( MAX( sld_pot1d(k), 0.0 ), 1.0 )                  
             endif
             
             !! Now apply "boosting factor(s)": 

             pirepmap = 0.0              
             slwmap   = 0.0
             vvmap = 0.0              
             
             if( dtc(k) .lt. 0.0 ) then
                 if( lwc(k) .ge. LWC_LOWLIM ) then
                    slwmap = 1.0
                 else
                    slwmap = 0.0
                 endif

                 if( vvubs(k) .le. -1.0 ) then
                    vvmap = 1.0
                 else if( vvubs(k) .ge. 1.0 ) then
                    vvmap = -0.075                   
                 else
                    call loc_interp( 1, NVRH, NVRH, 0.1, vvubs(k), fip_normvv_ubars, fip_normvv_indx, vvmap )
                 endif
             endif

             if( vvubs(k) .le. 0.0 ) then !! upward or no vertical velocity
                 icg_pot1d(k) = final_up( icg_pot1d(k), slwmap, pirepmap, vvmap )
             else
                 icg_pot1d(k) = finaldwn( icg_pot1d(k), slwmap, pirepmap, vvmap )
             endif
          enddo      
      else if(  isitu .eq. 22 .or. isitu .eq. 221 ) then !! Multpl-layer clouds lowest deck; primarily FZDZ,FZRA,PELT at sfc and CTT < -30C 
          do k = kbot,ktop            
             if( k .gt. kcitop(l) ) then 
                l = l + 1
                cttmap = 0.0
                ctt = ctop_tc(l)
                ctk = dta(kcitop(l))
                if( ctt .gt. fip_normcttcel_temp(1) .and. & 
                    ctt .lt. fip_normcttcel_temp(NCTT)  ) then   !! Table lookup and interp
                                                                !! cloudtop-temperature interest map            
                    call loc_interp( 1, NCTT, NCTT, 2.0, ctt, fip_normcttcel_temp, fip_normcttcel_indx, cttmap )
                endif
             endif       
             tmap = 0.0
             if( dtc(k) .gt. fip_normtcel_temp(1) .and. &
                 dtc(k) .lt. fip_normtcel_temp(NTCL)  ) then !! Table lookup and interp
                                                           !! temperature interest map
                 call loc_interp( 1, NTCL, NTCL, 2.0, dtc(k), fip_normtcel_temp, fip_normtcel_indx, tmap )                                                  
             endif !! Note if dtc(k) is out of the table range, tmap==0

             rhmap = 0.0
             if( rhk(k) .gt. fip_normrh_pct(1) .and. &
                 rhk(k) .lt. fip_normrh_pct(NVRH)  ) then !! Table lookup and interp
                 call loc_interp( 1, NVRH, NVRH, 5.0, rhk(k), fip_normrh_pct, fip_normrh_indx, rhmap ) !! rel. hum. interest map
             endif
             icg_pot1d(k) = cttmap*tmap*rhmap
             
             if( k .gt. kcitop(1) ) then
                sld_pot1d(k) = DIAG_MISSING                     
             else
                sld_pot1d(k) = 0.25*tmap                 
             endif
             
             !! Now apply "boosting factor(s)": 
             
             pirepmap = 0.0              
             slwmap   = 0.0
             vvmap = 0.0              
             
             if( dtc(k) .lt. 0.0 ) then
                 if( lwc(k) .ge. LWC_LOWLIM ) then
                    slwmap = 1.0
                 else
                    slwmap = 0.0
                 endif

                 if( vvubs(k) .le. -1.0 ) then
                    vvmap = 1.0
                 else if( vvubs(k) .ge. 1.0 ) then
                    vvmap = -0.075                   
                 else
                    call loc_interp( 1, NVRH, NVRH, 0.1, vvubs(k), fip_normvv_ubars, fip_normvv_indx, vvmap )
                 endif
             endif

             if( vvubs(k) .le. 0.0 ) then !! upward or no vertical velocity
                 icg_pot1d(k) = final_up( icg_pot1d(k), slwmap, pirepmap, vvmap )
             else
                 icg_pot1d(k) = finaldwn( icg_pot1d(k), slwmap, pirepmap, vvmap )
             endif
          enddo      
      else if(  isitu .eq. 23 .or. isitu .eq. 231 ) then !! Multpl-layer clouds lowest deck; CTT >= -12C; DZ/RA with T2 > -2C and no SNOW           
          sldfac = SQRT((ctk - 261.0)*D14)  !! by construction, ctk must be >= 261.15          
          do k = kbot,ktop            
             if( k .gt. kcitop(l) ) then 
                l = l + 1
                cttmap = 0.0
                ctt = ctop_tc(l)
                ctk = dta(kcitop(l))
                if( ctt .gt. fip_normcttcel_temp(1) .and. & 
                    ctt .lt. fip_normcttcel_temp(NCTT)  ) then   !! Table lookup and interp
                                                                !! cloudtop-temperature interest map            
                    call loc_interp( 1, NCTT, NCTT, 2.0, ctt, fip_normcttcel_temp, fip_normcttcel_indx, cttmap )
                endif
             endif
             tmap = 0.0
             if( dtc(k) .gt. fip_normtcel_temp(1) .and. &
                 dtc(k) .lt. fip_normtcel_temp(NTCL)  ) then !! Table lookup and interp
                                                           !! temperature interest map
                 call loc_interp( 1, NTCL, NTCL, 2.0, dtc(k), fip_normtcel_temp, fip_normtcel_indx, tmap )                                                  
             endif !! Note if dtc(k) is out of the table range, tmap==0

             rhmap = 0.0
             if( rhk(k) .gt. fip_normrh_pct(1) .and. &
                 rhk(k) .lt. fip_normrh_pct(NVRH)  ) then !! Table lookup and interp
                 call loc_interp( 1, NVRH, NVRH, 5.0, rhk(k), fip_normrh_pct, fip_normrh_indx, rhmap ) !! rel. hum. interest map
             endif
             icg_pot1d(k) = cttmap*tmap*rhmap
             
             if( k .gt. kcitop(1) ) then
                sld_pot1d(k) = DIAG_MISSING                     
             else
                sld_pot1d(k) = MIN( tmap*sldfac, 1.0 )                  
             endif
             
             !! Now apply "boosting factor(s)": 

             pirepmap = 0.0              
             slwmap   = 0.0
             vvmap = 0.0

             if( dtc(k) .lt. 0.0 ) then              
                 if( lwc(k) .ge. LWC_LOWLIM ) then
                    slwmap = 1.0
                 else
                    slwmap = 0.0
                 endif
                 !! print*,'k,lw,map=',k,lwc(k),rhk(k)

                 if( vvubs(k) .le. -1.0 ) then
                    vvmap = 1.0
                 else if( vvubs(k) .ge. 1.0 ) then
                    vvmap = -0.075                   
                 else
                    call loc_interp( 1, NVRH, NVRH, 0.1, vvubs(k), fip_normvv_ubars, fip_normvv_indx, vvmap )
                 endif
             endif

             if( vvubs(k) .le. 0.0 ) then !! upward or no vertical velocity
                 icg_pot1d(k) = final_up( icg_pot1d(k), slwmap, pirepmap, vvmap )
             else
                 icg_pot1d(k) = finaldwn( icg_pot1d(k), slwmap, pirepmap, vvmap )
             endif             
          enddo
      else if(  isitu .eq. 27 .or. isitu .eq. 271 ) then !! Multiple layer clouds; classic warm-nose/FRZRA structure
          sldfac = MAX( (cttmap - 0.7)/0.3, 0.0 )          
          do k = kbot,ktop 
             if( k .gt. kcitop(l) ) then 
                l = l + 1
                cttmap = 0.0
                ctt = ctop_tc(l)
                ctk = dta(kcitop(l))
                if( ctt .gt. fip_normcttcel_temp(1) .and. & 
                    ctt .lt. fip_normcttcel_temp(NCTT)  ) then   !! Table lookup and interp
                                                                !! cloudtop-temperature interest map            
                    call loc_interp( 1, NCTT, NCTT, 2.0, ctt, fip_normcttcel_temp, fip_normcttcel_indx, cttmap )
                endif
                sldfac = MAX( (cttmap - 0.7)/0.3, 0.0 )
             endif      
             tmap = 0.0
             if( dtc(k) .gt. fip_normtcel_temp(1) .and. &
                 dtc(k) .lt. fip_normtcel_temp(NTCL)  ) then !! Table lookup and interp
                                                           !! temperature interest map
                 call loc_interp( 1, NTCL, NTCL, 2.0, dtc(k), fip_normtcel_temp, fip_normtcel_indx, tmap )                                                  
             endif !! Note if dtc(k) is out of the table range, tmap==0

             rhmap = 0.0
             if( rhk(k) .gt. fip_normrh_pct(1) .and. &
                 rhk(k) .lt. fip_normrh_pct(NVRH)  ) then !! Table lookup and interp
                 call loc_interp( 1, NVRH, NVRH, 5.0, rhk(k), fip_normrh_pct, fip_normrh_indx, rhmap ) !! rel. hum. interest map
             endif
             
             if( k .le. fl+1 ) then  !! at/below the first k-lev with T>0.
                icg_pot1d(k) = MIN( tmap + 0.2*pct_radar*tmap, 1.0 )
                sld_pot1d(k) = icg_pot1d(k)             
             else                    !! above the warm nose
                icg_pot1d(k) = cttmap*tmap*rhmap
                sld_pot1d(k) = MIN( sldfac*(tmap + 0.2*pct_radar), 1.0 )    
             endif

             !! Now apply "boosting factor(s)": 

             pirepmap = 0.0              
             slwmap   = 0.0
             vvmap = 0.0              
             
             if( dtc(k) .lt. 0.0 ) then
                 if( lwc(k) .ge. LWC_LOWLIM ) then
                    slwmap = 1.0
                 else
                    slwmap = 0.0
                 endif

                 if( vvubs(k) .le. -1.0 ) then
                    vvmap = 1.0
                 else if( vvubs(k) .ge. 1.0 ) then
                    vvmap = -0.075                   
                 else
                    call loc_interp( 1, NVRH, NVRH, 0.1, vvubs(k), fip_normvv_ubars, fip_normvv_indx, vvmap )
                 endif
             endif

             if( vvubs(k) .le. 0.0 ) then !! upward or no vertical velocity
                 icg_pot1d(k) = final_up( icg_pot1d(k), slwmap, pirepmap, vvmap )
             else
                 icg_pot1d(k) = finaldwn( icg_pot1d(k), slwmap, pirepmap, vvmap )
             endif
          enddo      
                   
      endif       
        
      !! Combine the two estimates
      
      do k = MIN( kbot,kbot_cnvct ), MAX( ktop,ktop_cnvct ) 
         icg_pot1d(k) = MAX( icg_pot1d(k),icg_pot1d_cnvct(k) )
         if( sld_pot1d_cnvct(k) .gt. 0.0 ) then
             sld_pot1d(k) = MAX( sld_pot1d(k),sld_pot1d_cnvct(k) )
         endif 
      enddo
     
      RETURN 
   
END SUBROUTINE MULTPL_LAYCTTG

SUBROUTINE icing_types( ldbg, kbot, ktop, rhk, dta, dtc, vvubs, qrlw, icg_pot1d, sld_pot1d, ictyp_clr1d, ictyp_mxd1d, ictyp_rim1d,  &
                        kds,kde,                                                    &
                        kms,kme,                                                    &
                        kts,kte )
   
   USE module_model_constants

   IMPLICIT NONE

   !  Input variables

   INTEGER, INTENT(IN   )                                          :: kds,kde, &
                                                                      kms,kme, &
                                                                      kts,kte   
   INTEGER, INTENT(IN   )                                          :: kbot, ktop
   LOGICAL, INTENT(IN   )                                          :: LDBG 
   
   REAL   , INTENT(IN   ) , DIMENSION( kms:kme ) ::  rhk, dta, dtc, vvubs, qrlw
   REAL   , INTENT(INOUT) , DIMENSION( kms:kme ) ::  icg_pot1d, sld_pot1d   
   
   
   !  Output variables   
   
   REAL   , INTENT(  OUT) , DIMENSION( kms:kme ) :: ictyp_clr1d, ictyp_mxd1d, ictyp_rim1d

   !  Local variables
   
   REAL, PARAMETER :: LWC_LOWLIM = 5.0e-5 !! kg/kg     
   INTEGER :: k
   REAL clrval, mixval, rimval, frc_convect, icg_potk, sld_potk
   REAL sldboost, factemp, facconc, slpboost, sum_clear, sum_total

   CHARACTER*256 MESG   
   
   !...............................................................................................
   !    

      ictyp_clr1d( : ) = 0.0
      ictyp_mxd1d( : ) = 0.0
      ictyp_rim1d( : ) = 0.0

      if( ktop .eq. 0 ) then
         WRITE( MESG, '( A )' ) 'Unexpected ktop=0 in icing_types calculation'
         CALL wrf_error_fatal ( TRIM(MESG) )      
      endif
            
      do k = kbot,ktop
         if( icg_pot1d(k) .ge. 0.01 ) then
                  
            call loc_interp( 1, NVIT, NVIT, 1.0, dtc(k), ict_normtcel_temp, ict_normcler_indx, clrval )
            call loc_interp( 1, NVIT, NVIT, 1.0, dtc(k), ict_normtcel_temp, ict_normmixd_indx, mixval )
            call loc_interp( 1, NVIT, NVIT, 1.0, dtc(k), ict_normtcel_temp, ict_normrime_indx, rimval )
            
            if( sld_pot1d(k) .le. 0.10 ) then
               sldboost = 0.0
            else if( sld_pot1d(k) .ge. 0.75 ) then 
               sldboost = 0.020
            else
               sldboost = ((sld_pot1d(k)-0.10)/(.75-.10))*0.020            
            endif
         
            factemp = clrval/0.033216   !! 0.033216 max table value
            
            if( qrlw(k) .le. LWC_LOWLIM ) then
               facconc = 0.0
            else if( qrlw(k) .ge. 0.00012 ) then
               facconc = 1.0            
            else
               facconc = ((qrlw(k)-LWC_LOWLIM)/(0.00012-LWC_LOWLIM))*0.020 
            endif
         
            slpboost = factemp*facconc*0.020
            
            sum_clear = clrval+sldboost+slpboost
            sum_total = sum_clear+mixval+rimval
            
            ictyp_clr1d(k) = sum_clear/sum_total
            ictyp_mxd1d(k) = mixval/sum_total
            ictyp_rim1d(k) = rimval/sum_total
         
         endif
      enddo       

      if( kbot .gt. 1 ) then
        do k = 1,kbot-1 
           
           if( icg_pot1d(k) .ge. 1.0e-6 ) then
              WRITE( MESG, '( A, 2I8 )' ) 'Non-zero icg_pot1d below kbot,k=',kbot,k
              CALL wrf_error_fatal ( TRIM(MESG) )
           endif
           
           if( dtc(k) .le. 0.5 .and. qrlw(k) .gt. LWC_LOWLIM ) then !! there is light below cloud freezing rain not
                                                                    !! elsewhere detected
                                                                                            
              !! First need to handle filling in icg_pot1d and possibly sld_pot1d under
              !! these circumstances; treat each FZRA k-level as an independent single cloud
              !! layer for which ifip_situ = 24 !! Classic FRZA structure, single layer

              call single_layfzra( 24, .FALSE., 1, 1, k, k, rhk, dta, dtc, vvubs, &
                                   qrlw, frc_convect, icg_potk, sld_potk, &
                                   kds, kde, kms, kme, kts, kte )              
              
              sld_pot1d(k) = icg_potk
              icg_pot1d(k) = sld_potk
              
              if( icg_pot1d(k) .ge. 0.01 ) then

                 call loc_interp( 1, NVIT, NVIT, 1.0, dtc(k), ict_normtcel_temp, ict_normcler_indx, clrval )
                 call loc_interp( 1, NVIT, NVIT, 1.0, dtc(k), ict_normtcel_temp, ict_normmixd_indx, mixval )
                 call loc_interp( 1, NVIT, NVIT, 1.0, dtc(k), ict_normtcel_temp, ict_normrime_indx, rimval )

                 if( sld_pot1d(k) .le. 0.10 ) then
                    sldboost = 0.0
                 else if( sld_pot1d(k) .ge. 0.75 ) then 
                    sldboost = 0.020
                 else
                    sldboost = ((sld_pot1d(k)-0.10)/(.75-.10))*0.020            
                 endif

                 factemp = clrval/0.033216   !! 0.033216 max table value

                 if( qrlw(k) .ge. 0.00012 ) then
                    facconc = 1.0            
                 else
                    facconc = ((qrlw(k)-LWC_LOWLIM)/(0.00012-LWC_LOWLIM))*0.020 
                 endif

                 slpboost = factemp*facconc*0.020

                 sum_clear = clrval+sldboost+slpboost
                 sum_total = sum_clear+mixval+rimval

                 ictyp_clr1d(k) = sum_clear/sum_total
                 ictyp_mxd1d(k) = mixval/sum_total
                 ictyp_rim1d(k) = rimval/sum_total              

              endif              
                          
           endif
             
        enddo
      endif
            
      RETURN    

END SUBROUTINE ICING_TYPES

SUBROUTINE single_layfzra( isitu, iso_shalr, ml, fl, kbot, ktop, rhk, dta, dtc, vvubs, lwc, frcmax, icg_potk, sld_potk,   &
                           kds,kde,                                                                               &
                           kms,kme,                                                                               &
                           kts,kte )

   USE module_model_constants

   IMPLICIT NONE

   !  Input variables

   INTEGER, INTENT(IN   )                                          :: kds,kde, &
                                                                      kms,kme, &
                                                                      kts,kte   
   INTEGER, INTENT(IN   )                                          :: isitu, ml, fl, kbot, ktop
   LOGICAL, INTENT(IN   )                                          :: iso_shalr
   REAL   , INTENT(IN   ) , DIMENSION( kms:kme ) :: dta, dtc, vvubs, rhk
   REAL   , INTENT(IN   ) , DIMENSION( kms:kme ) :: lwc               !! lwc in kg/kg

   !  Output variables   

   REAL   , INTENT(  OUT)                        :: frcmax   
   REAL   , INTENT(  OUT)                        :: icg_potk, sld_potk

   !  Local variables

   REAL, PARAMETER :: D14 = 1.0/14.0
   REAL, PARAMETER :: LWC_LOWLIM = 5.0e-5 !! kg/kg  
   INTEGER         :: nlays,k
   REAL            :: ctt, ctk, cnvtmap, slwmap, pirepmap, vvmap, cttmap, tmap, rhmap, pct_radar, sldfac, frcshal 
   
   CHARACTER*256 MESG
   
   !! Statement Functions
   
   REAL FINAL_UP, INIT_INTERST, SLW_MAP, PIREP_MAP, VV_MAP
   FINAL_UP( INIT_INTERST, SLW_MAP, PIREP_MAP, VV_MAP ) = INIT_INTERST + (1.0 - INIT_INTERST)*    &
                                                      (0.4*SLW_MAP + 0.35*PIREP_MAP + 0.25*VV_MAP)
   REAL FINALDWN                                                      
   FINALDWN( INIT_INTERST, SLW_MAP, PIREP_MAP, VV_MAP ) = INIT_INTERST*(1.0 + 0.25*VV_MAP) + (1.0 - INIT_INTERST)*  &                                                  
                                                      (0.4*SLW_MAP + 0.35*PIREP_MAP)
   !...............................................................................................
   !    

      icg_potk = 0.0
      sld_potk = 0.0
      pct_radar = 1.0          !! code as in original scheme 

      if( isitu .ne. 24 ) then
          WRITE( MESG, '( A )' ) 'Subroutine must only be called for isitu=24'
          CALL wrf_error_fatal ( TRIM(MESG) )      
      endif 
            
      if( kbot .ne. ktop ) then
          WRITE( MESG, '( A )' ) 'kbot /= ktop: Subroutine must only be called when kbot=ktop'
          CALL wrf_error_fatal ( TRIM(MESG) )      
      else
          k = kbot
      endif
                  
      frcmax = 0.01*rhk(k)
            
      cttmap = 0.0
      ctt = dtc(ktop)
      ctk = dta(ktop)
      if( ctt .gt. fip_normcttcel_temp(1) .and. & 
          ctt .lt. fip_normcttcel_temp(NCTT)  ) then   !! Table lookup and interp
                                                       !! cloudtop-temperature interest map            
          call loc_interp( 1, NCTT, NCTT, 2.0, ctt, fip_normcttcel_temp, fip_normcttcel_indx, cttmap )
      endif 
   
      !! Single-layer; FRZRA structure
          
      sldfac = MAX( (cttmap - 0.7)/0.3, 0.0 )
      
      tmap = 0.0
      if( dtc(k) .gt. fip_normtcel_temp(1) .and. &
          dtc(k) .lt. fip_normtcel_temp(NTCL)  ) then !! Table lookup and interp
                                                    !! temperature interest map
          call loc_interp( 1, NTCL, NTCL, 2.0, dtc(k), fip_normtcel_temp, fip_normtcel_indx, tmap )                                                  
      endif !! Note if dtc(k) is out of the table range, tmap==0

      rhmap = 0.0
      if( rhk(k) .gt. fip_normrh_pct(1) .and. &
          rhk(k) .lt. fip_normrh_pct(NVRH)  ) then !! Table lookup and interp
          call loc_interp( 1, NVRH, NVRH, 5.0, rhk(k), fip_normrh_pct, fip_normrh_indx, rhmap ) !! rel. hum. interest map
      endif

      !! at/below the first k-lev with T>0.
      icg_potk = MIN( tmap + 0.2*pct_radar*tmap, 1.0 )
      sld_potk = icg_potk

      !! Now apply "boosting factor(s)": 

      pirepmap = 0.0              
      slwmap   = 0.0
      vvmap = 0.0 
      
      if( dtc(k) .lt. 0.0 ) then      
          if( lwc(k) .ge. LWC_LOWLIM ) then
             slwmap = 1.0
          else
             slwmap = 0.0
          endif

          if( vvubs(k) .le. -1.0 ) then
             vvmap = 1.0
          else if( vvubs(k) .ge. 1.0 ) then
             vvmap = -0.075                   
          else
             call loc_interp( 1, NVRH, NVRH, 0.1, vvubs(k), fip_normvv_ubars, fip_normvv_indx, vvmap )
          endif
      endif

      if( vvubs(k) .le. 0.0 ) then !! upward or no vertical velocity
          icg_potk = final_up( icg_potk, slwmap, pirepmap, vvmap )
      else
          icg_potk = finaldwn( icg_potk, slwmap, pirepmap, vvmap )
      endif
               
      RETURN 
   
END SUBROUTINE SINGLE_LAYFZRA

END MODULE module_diag_pld
#endif
