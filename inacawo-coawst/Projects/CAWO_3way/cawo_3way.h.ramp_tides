/*
** svn $Id: sandy.h 25 2007-04-09 23:43:58Z jcwarner $
*******************************************************************************
** Copyright (c) 2002-2007 The ROMS/TOMS Group                               **
**   Licensed under a MIT/X style license                                    **
**   See License_ROMS.txt                                                    **
*******************************************************************************
**
** Options for Sandy Test.
**
** Application flag:   CAWO_3WAY
*/

#define ROMS_MODEL
#define WRF_MODEL
#define SWAN_MODEL
#undef  WW3_MODEL
#define MCT_LIB
#define MCT_INTERP_OC2AT
#define MCT_INTERP_WV2AT
#define MCT_INTERP_OC2WV

#ifdef WRF_MODEL && (defined SWAN_MODEL || defined WW3_MODEL)
# define DRAGLIM_DAVIS
# define COARE_TAYLOR_YELLAND
#endif

#ifdef ROMS_MODEL
/* Physics + numerics */
#ifdef WW3_MODEL || defined SWAN_MODEL
#  define WEC_VF
#  define WDISS_WAVEMOD
#  define UV_KIRBY
# endif
# endif

/* Forcing */
# ifdef WRF_MODEL
#  undef  BULK_FLUXES
#  define ATM2OCN_FLUXES
#  undef  ANA_SSFLUX
#  undef  LONGWAVE_OUT
# else
#  define BULK_FLUXES
# endif
# define ATM_PRESS
# define COOL_SKIN 
# define EMINUSP
# define SOLAR_SOURCE

#define OMEGA_IMPLICIT
#define SINGLE_PRECISION

/* MPI optimization*/
#define COLLECT_ALLREDUCE
#define REDUCE_ALLGATHER

/*NETCDF4*/
/*#define HDF5
#define DEFLATE*/
/* Grid */
#define CURVGRID
#define MASKING

/* model output options */
#define AVERAGES
#define PERFECT_RESTART

/* Equation of State */
# define SALINITY
# define NONLIN_EOS

/* Options for solving momentum equations. The default horizontal advection is 3rd-order upstream bias 3D momentum and 4th-order centered for 2D momentum */
#define UV_ADV
#define UV_COR
#define UV_QDRAG
#define DJ_GRADPS
#define MIX_S_UV
#define UV_VIS2
#define VISC_GRID /*have different horizontal viscocity regarding cell size*/
#define SOLVE3D

/* Options for soloving tracer equations. The default horizontal and vertical advection is 4th-order centered */
# define TS_DIF2
# define DIFF_GRID
# define MIX_GEO_TS

/*** splines***/
# define RI_SPLINES

/* Bottom Boundary Condition*/
# define ANA_BSFLUX
# define ANA_BTFLUX
# define ANA_BPFLUX

/*lateral boundary condition*/
# define RADIATION_2D

/*Tidal Forcing*/
# define SSH_TIDES
# define UV_TIDES
# define RAMP_TIDES

# ifdef SSH_TIDES
# define ADD_FSOBC
# endif

# ifdef UV_TIDES
# define ADD_M2OBC
# endif

/* Vertical Mixing parameterization */
#define MY25_MIXING
#ifdef MY25_MIXING
# define N2S2_HORAVG
# define KANTHA_CLAYSON
#endif

/* Restrict SWAN output files */
#define SWAN_SINGLESPEC
#define WAVE_PARTITION
