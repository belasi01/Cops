/**
 *  @file qaa.c
 *  @brief Quasi-Analytic Algorithm
 *  @author Paul Martinolich
 *  @author Naval Research Laboratory, Stennis Space Center, MS
 *
 *  This code implements version 6 of the QAA algorithm which
 *  was based on the original algorithm  Lee, at al (2002)
 *  "Deriving inherent optical properties from water color:  A
 *  multi-band quasi-analytical algorithm for optically deep water"
 *  <em>Applied Optics, (41) 27, 5755-5772</em>, 2002
 *
 *  The version 4 update was presented as Appendix A in the paper
 *  Lee, et al (2007) "Euphotic zone depth:  Its derivation and
 *  implication to ocean-color remote sensing" <em>Journal of
 *  Geophysical Research, Vol 112</em>, C03009, doi:10,1029/2006JC003802.
 *
 *  The version 5 update is available at IOCCG web site:
 *  http://www.ioccg.org/groups/Software_OCA/QAA_v5.pdf
 *
 *  The version 6 update is available at IOCCG web site:
 *  http://www.ioccg.org/groups/Software_OCA/QAA_v6.pdf
 *
 *  Additional routines are used to initialize indices and other parameters
 *  with the goal to make the code generic for use in SeaWiFS/MODIS processing
 *  as well as hyperspectral data from an ASD or PHILLS.
 *
 *  Likewise, the code duplicates routines for float or double type
 *  variables.  The 'float' routines have an 'f' after the 'qaa'
 *  prefix.
 *
 *  The following code example shows a example of running the QAA
 *  algorithm on SeaWiFS data (with notes on how to run it for other
 *  sensor and using the 670nm path or chl path).
 *
 *  @code
 *  #include "qaa.h"
 *  #define NBANDS 7
 *  // inputs
 *  int   idx410  = 0;
 *  int   idx440  = 1;
 *  int   idx490  = 2;
 *  int   idx555  = 3;
 *  int   idx670  = 4;
 *
 *  // SeaWiFS wavelengths (nm)
 *  double wl[NBANDS] = { 412, 443, 490, 555, 670 };
 *
 *  // SeaWiFS band-averaged pure water absorption
 *  double aw[NBANDS] = { 0.004641, 0.007095, 0.0015267, 0.032599, 0.4456 };
 *
 *  // SeaWiFS band-averaged pure water backscattering coefficient
 *  double bbw[NBANDS] = { 0.003272, 0.002421, 0.001568, 0.0009394, 0.0004261 };
 *
 *  double Rrs[NBANDS];
 *
 *  // temporary space
 *
 *  double u[NBANDS], buf[4*NBANDS];
 *
 *  // outputs
 *
 *  double rrs[NBANDS];
 *  double a[NBANDS], bb[NBANDS], adg[NBANDS], aph[NBANDS];
 *
 *  // read input Rrs
 *
 *  qaa_init( idx410, idx440, idx490, idx555, idx670);
 *
 *  // set the S parameter (default is 0.015)
 *  qaa_set_param( QAA_S_PARAM, 0.011 );
 *
 *  // set the aw coefficients which differ based on sensor (Step 2)
 *  // qaa_set_param( QAA_COEFS_PARAM, -1.204, -1.229, -0.395 ); // MODIS
 *  // qaa_set_param( QAA_COEFS_PARAM, -1.273, -1.163, -0.295 ); // MERIS
 *  // qaa_set_param( QAA_COEFS_PARAM, -1.146, -1.366, -0.469 ); // OCM1
 *  // qaa_set_param( QAA_COEFS_PARAM, -1.318, -1.192, -0.206 ); // OCM2
 *  // NOTE: OCM2 has no 670 band, but 620 can be used.
 *  // qaa_set_param( QAA_COEFS_PARAM, -1.273, -1.366, -0.469 ); // SeaWiFS (default)
 *
 *  // run QAA using chl reference and decompose a into adg and aph components
 *  // NOTE: if quality flags are undesired, a NULL can be passed to ignore them.
 *  qaa_v6( NBANDS, wl, Rrs, rrs, u, buf, a, bb, NULL, NULL, flags );
 *  qaa_decomp( NBANDS, wl, rrs, a, adg, aph, flags );
 *
 *  // write out results
 *
 *  @endcode
 */

#include <stdlib.h>
#include <math.h>
#include <stdarg.h>
#include <assert.h>

#include <R_ext/Arith.h>

/* #include "qaa.h" */
/* qaa.h directly included here */

/**
 *  @file liboc/qaa.h
 *  @brief Compute IOPs from rrs using Quasi-Analytic Algorithm
 *  @author Paul Martinolich
 *  @author Naval Research Laboratory, Stennis Space Center, MS
 */

#ifndef _QAA_H
#define _QAA_H

enum {
    QAA_S_PARAM = 1,
    QAA_COEFS_PARAM = 3,
    QAA_APH_CHECK = 8
};

int qaa_is_initialized(void);

int qaa_init(int i410, int i440, int i490, int i555, int i640);
int qaa_set_param(int param, ...);
int qaa_v6(int nbands, double *wavel, double *Rrs, double *aw, double *bbw,
        double *rrs, double *u, double *a, double *bb,
        unsigned char *flags, double *ETA, double *G0G1);
int qaa_decomp(int nbands, double *wavel, double *rrs, double *a, double *aw,
        double *adg, double *aph, unsigned char *flags, double *SrOUT);
int qaaf_v6(int nbands, float *wavel, float *Rrs, float *aw, float *bbw,
        float *rrs, float *u, float *a, float *bb,
        unsigned char *flags, float *ETA, float *G0G1);
int qaaf_decomp(int nbands, float *wavel, float *rrs, float *a, float *aw,
        float *adg, float *aph, unsigned char *flags, float *SrOUT);

#endif
/* end of qaa.h */

static int idx410 = -1;
static int idx440 = -1;
static int idx490 = -1;
static int idx555 = -1;
static int idx670 = -1;
static int initialized = 0;
static int aph_check = 1;
static double S = 0.015;
static double acoefs[3];

/**
 *  @brief determine if qaa algorithm properly initialized
 *  @returns 1 if qaa_init has been previously called; 0, otherwise
 */

int
qaa_is_initialized(void) {
    return initialized;
}

/**
 *  @brief initalize Quasi-Analytical Algorithm v4
 *  @param[in] i410   0-relative index in spectrum of 410 nm
 *  @param[in] i440   0-relative index in spectrum of 440 nm
 *  @param[in] i490   0-relative index in spectrum of 490 nm
 *  @param[in] i555   0-relative index in spectrum of 555 nm
 *  @param[in] i670   0-relative index in spectrum of 670 nm
 *
 *  This routine should be called to initialize various parameters that may
 *  be adjusted in the QAA algorithm or that must be known apriori.  For example,
 *  the user must supply the indices for various required band numbers and
 *  whether to perform the iteration in the QAA-555 algorithm (normally yes).
 */

int
qaa_init(int i410, int i440, int i490, int i555, int i670) {
    idx410 = i410;
    idx440 = i440;
    idx490 = i490;
    idx555 = i555;
    idx670 = i670;

    //  SeaWiFS coefficients

    acoefs[0] = -1.146;
    acoefs[1] = -1.366;
    acoefs[2] = -0.469;

    initialized = 1;

    return 0;
}

/**
 *  @brief set a parameter for Quasi-Analytical Algorithm
 *  @param[in] param  name of parameter
 *
 *  This routine should be called to initialize various parameters that may
 *  be adjusted in the QAA algorithm or that must be known apriori.  For example,
 *  the user must supply the indices for various required band numbers.  Optionally,
 *  the user may define there own values for some parameters, like S.
 *
 *  Presently the only parameter the user may set is the S value.
 */

int
qaa_set_param(int param, ...) {
    va_list ap;
    va_start(ap, param);
    switch (param) {
    case QAA_S_PARAM:
        S = va_arg(ap, double);
        break;
    case QAA_COEFS_PARAM:
        acoefs[0] = va_arg(ap, double);
        acoefs[1] = va_arg(ap, double);
        acoefs[2] = va_arg(ap, double);
        break;
    case QAA_APH_CHECK:
        aph_check = va_arg(ap, int);
        break;
    }
    va_end(ap);
    return 0;
}

/**
 *  @brief Quasi-Analytical Algorithm v6
 *  @param[in]   nbands   number of bands in spectrum
 *  @param[in]   wavel    wavelength of spectrum (nbands)
 *  @param[in]   Rrs      above-water remote sensing reflectance (nbands)
 *  @param[in]   aw       pure-water absorption (nbands)
 *  @param[in]   bbw      pure-water back scattering (nbands)
 *  @param[out]  rrs      below-water remote sensing reflectance (nbands)
 *  @param[out]  u        ratio (nbands)
 *  @param[out]  a        total absorption (nbands)
 *  @param[out]  bb       backscattering (nbands)
 *  @param[out]  flags    quality flags (will be modified) or NULL     
 *
 *  This implements version 6 of the Quasi-Analytical Algorithm.
 */

int
qaa_v6(int nbands, double *wavel, double *Rrs, double *aw, double *bbw,
        double *rrs, double *u, double *a, double *bb, unsigned char *flags, double *ETA, double *G0G1) {

    const double g0 = 0.08945;
    const double g1 = 0.1247;
    G0G1[0] = g0;
    G0G1[1] = g1;

    int i, idxref;

    double rat, aref;
    double bbpref;
    double Y;
    double rho, numer, denom;

    assert(idx440 >= 0);
    assert(idx490 >= 0);
    assert(idx555 >= 0);
    assert(nbands >= 0);

    /* pre-test 670 */
    if ((Rrs[idx670] > 20.0 * pow(Rrs[idx555], 1.5)) ||
            (Rrs[idx670] < 0.9 * pow(Rrs[idx555], 1.7))) {
        Rrs[idx670] = 1.27 * pow(Rrs[idx555], 1.47) + 0.00018 * pow(Rrs[idx490] / Rrs[idx555], -3.19);
        if (flags) *flags |= 0x02;
    }

    /* Step 0 */
    for (i = 0; i < nbands; i++) {
        rrs[i] = Rrs[i] / (0.52 + 1.7 * Rrs[i]);
        if (Rrs[i] < 0.0)
            if (flags) *flags |= 0x01;
    }

    /* Step 1 */
    for (i = 0; i < nbands; i++)
        u[i] = (sqrt(g0 * g0 + 4.0 * g1 * rrs[i]) - g0) / (2.0 * g1);

    /* Step 2 */
    if (Rrs[idx670] >= 0.0015) {
        aref   = aw[idx670] + 0.39*powf(Rrs[idx670]/(Rrs[idx440]+Rrs[idx490]),1.14);
        idxref = idx670;
    } else {
        numer = Rrs[idx440] + Rrs[idx490];
        denom = Rrs[idx555] + 5 * Rrs[idx670]*(Rrs[idx670] / Rrs[idx490]);
        rho = log10(numer / denom);
        rho = acoefs[0] + acoefs[1] * rho + acoefs[2] * rho*rho;
        aref = aw[idx555] + pow(10.0, rho);
        idxref = idx555;
    }

    /* Step 3 */
    bbpref = ((u[idxref] * aref) / (1.0 - u[idxref])) - bbw[idxref];

    /* Step 4 */
    rat = rrs[idx440] / rrs[idx555];
    Y = 2.0 * (1.0 - 1.2 * exp(-0.9 * rat));
    *ETA = Y;

    /* Step 5 */
    for (i = 0; i < nbands; i++) {
        bb[i] = bbpref * pow((wavel[idxref] / wavel[i]), Y) + bbw[i];
        if (bb[i] < 0.0)
            if (flags) *flags |= 0x04;
    }

    /* Step 6 */
    for (i = 0; i < nbands; i++) {
        a[i] = ((1.0 - u[i]) * bb[i]) / u[i];
        if (a[i] < 0.0)
            if (flags) *flags |= 0x08;
    }

    return 0;
}

/**
 *  @brief Quasi-Analytical Algorithm v4
 *  @see qaa_v6()
 */

int
qaaf_v6(int nbands, float *wavel, float *Rrs, float *aw, float *bbw,
        float *rrs, float *u, float *a, float *bb,
        unsigned char *flags, float *ETA, float *G0G1) {

    const float g0 = 0.08945;
    const float g1 = 0.1247;
    G0G1[0] = g0;
    G0G1[1] = g1;

    float rho, numer, denom;

    float rat, aref;
    float bbpref;
    float Y;

    int i, idxref;

    assert(idx440 >= 0);
    assert(idx490 >= 0);
    assert(idx555 >= 0);
    assert(idx670 >= 0);
    assert(nbands >= 0);

    /* Test for bad Rrs at idx555 */
    if (Rrs[idx555] <= 0.0)
        Rrs[idx555] = 0.001;

    /* pre-test 670 */
    if ((Rrs[idx670] > 20.0 * powf(Rrs[idx555], 1.5)) ||
            (Rrs[idx670] < 0.9 * powf(Rrs[idx555], 1.7))) {
        Rrs[idx670] = 1.27 * powf(Rrs[idx555], 1.47) + 0.00018 * powf(Rrs[idx490] / Rrs[idx555], -3.19);
        if (flags) *flags |= 0x02;
    }

    /* Step 0 */
    for (i = 0; i < nbands; i++) {
        rrs[i] = Rrs[i] / (0.52 + 1.7 * Rrs[i]);
        if (Rrs[i] < 0.0)
            if (flags) *flags |= 0x01;
    }

    /* Step 1 */
    for (i = 0; i < nbands; i++)
        u[i] = (sqrt(g0 * g0 + 4.0 * g1 * rrs[i]) - g0) / (2.0 * g1);

    /* Step 2 */
    if (Rrs[idx670] >= 0.0015) {
        aref   = aw[idx670] + 0.39*powf(Rrs[idx670]/(Rrs[idx440]+Rrs[idx490]),1.14);
        idxref = idx670;
    } else {
        numer = Rrs[idx440] + Rrs[idx490];
        denom = Rrs[idx555] + 5 * Rrs[idx670]*(Rrs[idx670] / Rrs[idx490]);
        rho = log10f(numer / denom);
        rho = acoefs[0] + acoefs[1] * rho + acoefs[2] * rho*rho;
        aref = aw[idx555] + powf(10.0, rho);
        idxref = idx555;
    }

    /* Step 3 */
    bbpref = ((u[idxref] * aref) / (1.0 - u[idxref])) - bbw[idxref];

    /* Step 4 */
    rat = rrs[idx440] / rrs[idx555];
    Y = 2.0 * (1.0 - 1.2 * expf(-0.9 * rat));
    *ETA = Y;

    /* Step 5 */
    for (i = 0; i < nbands; i++) {
        bb[i] = bbpref * pow((wavel[idxref] / wavel[i]), Y) + bbw[i];
        if (bb[i] < 0.0)
            if (flags) *flags |= 0x04;
    }

    /* Step 6 */
    for (i = 0; i < nbands; i++) {
        a[i] = ((1.0 - u[i]) * bb[i]) / u[i];
        if (a[i] < 0.0)
            if (flags) *flags |= 0x08;
    }

    return 0;
}

/**
 *  @brief Quasi-Analytical Algorithm - decomposition of total absorption
 *  @param[in]   nbands   number of bands in spectrum
 *  @param[in]   wavel    wavelength of spectrum (nbands)
 *  @param[in]   rrs      below-water remote sensing reflectance (nbands)
 *  @param[in]   a        total absorption (nbands)
 *  @param[in]   aw       pure-water absorption (nbands)
 *  @param[out]  adg      gelbstuff absorption (nbands)
 *  @param[out]  aph      phytoplankton absorption (nbands)
 *  @param[out]  flags    quality flags (will be modified) or NULL
 *
 *  This implements Table 3 of the Quasi-Analytical Algorithm.
 *  It decomposes the total absorption into phytoplankton absorption and
 *  gelbstuff absorption.
 *
 *  This implementation adds a consistency check based on the
 *  phytoplankton absorption at 443nm to improve the decomposition
 *  of total absorption.
 */

int
qaa_decomp(int nbands, double *wavel, double *rrs, double *a, double *aw,
        double *adg, double *aph, unsigned char *flags, double *SrOUT) {
    int i;
    double symbol, x1, x2;
    double zeta, denom, dif1, dif2;
    double rat, ag440;
    double Sr;

    assert(idx410 >= 0);
    assert(idx440 >= 0);
    assert(idx555 >= 0);
    assert(nbands >= 0);

    /* step 7 */
    rat = rrs[idx440] / rrs[idx555];
    symbol = 0.74 + (0.2 / (0.8 + rat));

    /* step 8 */
    Sr = S + 0.002 / (0.6 + rat);
    *SrOUT = Sr;
    zeta = exp(Sr * (wavel[idx440] - wavel[idx410]));

    /* step 9 */
    denom = zeta - symbol;
    dif1 = a[idx410] - symbol * a[idx440];
    dif2 = aw[idx410] - symbol * aw[idx440];
    ag440 = (dif1 - dif2) / denom;

    for (i = 0; i < nbands; i++) {
        adg[i] = ag440 * exp(Sr * (wavel[idx440] - wavel[i]));
        aph[i] = a[i] - adg[i] - aw[i];
    }

    /* check aph443 range */

    if (aph_check) {

        x1 = aph[idx440] / a[idx440];
        if (x1 < 0.15 || x1 > 0.6) {

            if (flags) *flags |= 0x10;

            x2 = -0.8 + 1.4 * (a[idx440] - aw[idx440]) / (a[idx410] - aw[idx410]);
            if (x2 < 0.15) {
                x2 = 0.15;
                if (flags) *flags |= 0x20;
            }
            if (x2 > 0.6) {
                x2 = 0.6;
                if (flags) *flags |= 0x40;
            }

            aph[idx440] = a[idx440] * x2;
            ag440 = a[idx440] - aph[idx440] - aw[idx440];

            for (i = 0; i < nbands; i++) {
                adg[i] = ag440 * exp(Sr * (wavel[idx440] - wavel[i]));
                aph[i] = a[i] - adg[i] - aw[i];
            }

        }
    }
    return 0;
}

/**
 *  @brief Quasi-Analytical Algorithm - decomposition of total absorption
 *  @see qaa_decomp()
 */

int
qaaf_decomp(int nbands, float *wavel, float *rrs, float *a, float *aw,
        float *adg, float *aph, unsigned char *flags, float *SrOUT) {
    int i;
    float symbol, x1, x2;
    float zeta, denom, dif1, dif2;
    float rat, ag440;
    float Sr;

    assert(idx410 >= 0);
    assert(idx440 >= 0);
    assert(idx555 >= 0);
    assert(nbands >= 0);

    /* step 7 */
    rat = rrs[idx440] / rrs[idx555];
    symbol = 0.74 + (0.2 / (0.8 + rat));

    /* step 8 */
    Sr = S + 0.002 / (0.6 + rat);
    *SrOUT = Sr;
    zeta = expf(Sr * (wavel[idx440] - wavel[idx410]));

    /* step 9 */
    denom = zeta - symbol;
    dif1 = a[idx410] - symbol * a[idx440];
    dif2 = aw[idx410] - symbol * aw[idx440];
    ag440 = (dif1 - dif2) / denom;

    for (i = 0; i < nbands; i++) {
        adg[i] = ag440 * expf(Sr * (wavel[idx440] - wavel[i]));
        aph[i] = a[i] - adg[i] - aw[i];
    }

    /* check aph443 range */

    if (aph_check) {

        x1 = aph[idx440] / a[idx440];
        if (x1 < 0.15 || x1 > 0.6) {

            if (flags) *flags |= 0x10;

            x2 = -0.8 + 1.4 * (a[idx440] - aw[idx440]) / (a[idx410] - aw[idx410]);
            if (x2 < 0.15) {
                x2 = 0.15;
                if (flags) *flags |= 0x20;
            }
            if (x2 > 0.6) {
                x2 = 0.6;
                if (flags) *flags |= 0x40;
            }

            aph[idx440] = a[idx440] * x2;
            ag440 = a[idx440] - aph[idx440] - aw[idx440];

            for (i = 0; i < nbands; i++) {
                adg[i] = ag440 * expf(Sr * (wavel[idx440] - wavel[i]));
                aph[i] = a[i] - adg[i] - aw[i];
            }

        }

    }

    return 0;
}

/* #ifdef TEST_QAA */

#include <stdio.h>

/*
 * to compile:
 * cc -o qaa -g -DTEST_QAA -I. qaa.c -lm
 * ./qaa
 */



static void print_out(int n, float *fwl, float *Rrs, float *rrs, float *u,
        float *a, float *aph, float *adg, float *aw, float *bb, float *bbw) {

    int i;

    printf("lamda ");
    for (i = 0; i < n; i++)
        printf("%9.0f ", fwl[i]);
    printf("\n");

    printf("Rrs : ");
    for (i = 0; i < n; i++)
        printf("%9.6f ", Rrs[i]);
    printf("\n");

    printf("rrs : ");
    for (i = 0; i < n; i++)
        printf("%9.6f ", rrs[i]);
    printf("\n");

    printf("u   : ");
    for (i = 0; i < n; i++)
        printf("%9.6f ", u[i]);
    printf("\n");

    printf("a   : ");
    for (i = 0; i < n; i++)
        printf("%9.6f ", a[i]);
    printf("\n");

    printf("aph : ");
    for (i = 0; i < n; i++)
        printf("%9.6f ", aph[i]);
    printf("\n");

    printf("adg : ");
    for (i = 0; i < n; i++)
        printf("%9.6f ", adg[i]);
    printf("\n");

    printf("aw  : ");
    for (i = 0; i < n; i++)
        printf("%9.6f ", aw[i]);
    printf("\n");

    printf("bb  : ");
    for (i = 0; i < n; i++)
        printf("%9.6f ", bb[i]);
    printf("\n");

    printf("bbw : ");
    for (i = 0; i < n; i++)
        printf("%9.6f ", bbw[i]);
    printf("\n");

}
int qaa(char **infile, char **outfile) {


    float G0G1[2];

    /* the 5th position here will be changed for 640nm later down in code */

/*#define NBANDS      6*/
/*#define NUM_SPECTRA 24*/

    FILE* filein = NULL;
    FILE* fileout = NULL;
    int nUM_SPECTRA, nBANDS;
    int k, l, retscan;

    fileout = fopen(*outfile, "w");
    if (fileout == NULL ) {
        printf( "Cannot open file %s\n", *outfile );
        return 1;
    }
    filein = fopen(*infile, "r");
    if (filein == NULL ) {
        printf( "Cannot open file %s\n", *infile );
        return 1;
    }
    retscan = fscanf(filein, "%d %d", &nUM_SPECTRA, &nBANDS);

    float *fwl = malloc(nBANDS * sizeof(float));
    for (l = 0 ; l < nBANDS ; l++) {
      retscan = fscanf(filein, "%f", &fwl[l]);
      fprintf(fileout, "%f ", fwl[l]);
    }
    fprintf(fileout, "\n");


/*
g0 <- 0.08945
g1 <- 0.1247
*/

    
        /*    412       443       490       510       555       670 */
/*
    float Rrs_insitu[NUM_SPECTRA][NBANDS] = {
        { 0.001919, 0.002297, 0.004420, 0.005547, 0.009138, 0.004110},
        { 0.001753, 0.002657, 0.004348, 0.005212, 0.007641, 0.003950},
        { 0.003968, 0.003374, 0.002932, 0.002142, 0.001214, 0.000136},
        { 0.001332, 0.001414, 0.002116, 0.002464, 0.003891, 0.001669},
        { 0.001572, 0.001537, 0.002132, 0.002504, 0.004157, 0.001909},
        { 0.004830, 0.004084, 0.003540, 0.002648, 0.001584, 0.000194},
        { 0.001145, 0.001457, 0.002501, 0.002945, 0.004180, 0.001524},
        { 0.000921, 0.001004, 0.001465, 0.001456, 0.001363, 0.000252},
        { 0.003120, 0.003226, 0.003530, 0.002857, 0.001882, 0.000196},
        { 0.005170, 0.004682, 0.003794, 0.002535, 0.001343, 0.000131},
        { 0.004718, 0.004378, 0.003763, 0.002627, 0.001472, 0.000151},
        { 0.003503, 0.003394, 0.003358, 0.002510, 0.001507, 0.000181},
        { 0.001005, 0.001131, 0.001879, 0.002219, 0.003316, 0.001100},
        { 0.007704, 0.006917, 0.005540, 0.003721, 0.002129, 0.000389},
        { 0.003311, 0.003071, 0.002920, 0.002315, 0.001508, 0.000285},
        { 0.003476, 0.003285, 0.003073, 0.002347, 0.001436, 0.000200},
        { 0.004661, 0.005739, 0.008028, 0.007809, 0.006632, 0.001035},
        { 0.004212, 0.004859, 0.006455, 0.006117, 0.004895, 0.000676},
        { 0.002090, 0.002280, 0.003091, 0.002780, 0.002177, 0.000444},
        { 0.003237, 0.003042, 0.002947, 0.002317, 0.001440, 0.000166},
        { 0.003125, 0.003444, 0.004119, 0.003777, 0.002875, 0.000413},
        { 0.002637, 0.002896, 0.003477, 0.002987, 0.002085, 0.000251},
        { 0.003472, 0.003596, 0.003834, 0.003067, 0.001969, 0.000331},
        { 0.004554, 0.003772, 0.002074, 0.002082, 0.001338, 0.000120}
    };
*/

/*    float fwl[NBANDS] = {412, 443, 490, 510, 555, 670};*/
/*    float aw[NBANDS] = {0.004994, 0.007512, 0.025010, 0.040020, 0.077080, 0.445600};*/
/*    float bbw[NBANDS] = {0.003271, 0.002421, 0.001568, 0.001339, 0.000939, 0.000426};*/

    printf("Number of Bands %d\n", nBANDS);
    float *aw = malloc(nBANDS * sizeof(float));
    for (l = 0 ; l < nBANDS ; l++) {
      retscan = fscanf(filein, "%f", &aw[l]);
    }


    float *bbw = malloc(nBANDS * sizeof(float));
    for (l = 0 ; l < nBANDS ; l++) {
      retscan = fscanf(filein, "%f", &bbw[l]);
    }

    printf("wavelength     aw      bbw\n");
    for (l = 0 ; l < nBANDS ; l++) {
      printf("%f %f %f\n", fwl[l], aw[l],  bbw[l]);
    }

    float *Rrs = malloc(nBANDS * sizeof(float));
    float *rrs = malloc(nBANDS * sizeof(float));
    float *u = malloc(nBANDS * sizeof(float));
    float *a = malloc(nBANDS * sizeof(float));
    float *bb = malloc(nBANDS * sizeof(float));
    float *aph = malloc(nBANDS * sizeof(float));
    float *adg = malloc(nBANDS * sizeof(float));

    int i, j;
/*
    float Rrs[NBANDS];
    float rrs[NBANDS];
    float u[NBANDS];
    float a[NBANDS];
    float bb[NBANDS];
    float aph[NBANDS];
    float adg[NBANDS];
*/
    float ETA, SrOUT;
    unsigned char flags;

/*
    int idx410 = 0;
    int idx440 = 1;
    int idx490 = 2;
    int idx555 = 4;
    int idx670 = 5;
*/

    int idx410; retscan = fscanf(filein, "%d", &idx410); printf("idx410 wavelength %d : %f\n", idx410 + 1, fwl[idx410]);
    int idx440; retscan = fscanf(filein, "%d", &idx440); printf("idx440 wavelength %d : %f\n", idx440 + 1, fwl[idx440]);
    int idx490; retscan = fscanf(filein, "%d", &idx490); printf("idx490 wavelength %d : %f\n", idx490 + 1, fwl[idx490]);
    int idx555; retscan = fscanf(filein, "%d", &idx555); printf("idx555 wavelength %d : %f\n", idx555 + 1, fwl[idx555]);
    int idx670; retscan = fscanf(filein, "%d", &idx670); printf("idx670 wavelength %d : %f\n", idx670 + 1, fwl[idx670]);

    fprintf(fileout, "%d\n", idx440 + 1);

    char **specname = malloc(nUM_SPECTRA * sizeof(char*));

    float **Rrs_insitu = malloc(nUM_SPECTRA * sizeof(float*));
    for (k = 0 ; k < nUM_SPECTRA ; k++)
    {
        specname[k] = malloc(1024 * sizeof(char));
        Rrs_insitu[k] = malloc(nBANDS * sizeof(float));
    }
    for (k = 0 ; k < nUM_SPECTRA ; k++) {
	retscan = fscanf(filein, "%*c%[^\n]", specname[k]);
        printf("spec %d : %s\n", k + 1, specname[k]);
    }
    for (k = 0 ; k < nUM_SPECTRA ; k++) {
      printf("spec %d (Rrs) : ", k + 1);
      for (l = 0 ; l < nBANDS ; l++) {
        retscan = fscanf(filein, "%f", &Rrs_insitu[k][l]);
        printf("%f ", Rrs_insitu[k][l]);
      }
      printf("\n");
    }

    fclose(filein);


    int nbands;

    // Ping's bbw using 0.0038 * pow((400.0/lambda),4.32);
    //    for ( i = 0; i< NBANDS; i++ )
    //        bbw[i] = 0.0038 * pow(400.0/fwl[i],4.32);

    qaa_init(idx410, idx440, idx490, idx555, idx670);
    qaa_set_param(QAA_APH_CHECK, 0);

    printf("QAA v6\n");

    fprintf(fileout, "NUM_SPECTRA %d\n", nUM_SPECTRA);
    fprintf(fileout, "NUM_BANDS %d\n", nBANDS);
    for (j = 0; j < nUM_SPECTRA; j++) {


        flags = 0;
        nbands = nBANDS;
        /* 412 to 670 */
        for (i = 0; i < nbands; i++)
            Rrs[i] = Rrs_insitu[j][i];

        qaaf_v6(nbands, fwl, Rrs, aw, bbw, rrs, u, a, bb, NULL, &ETA, G0G1);
        qaaf_decomp(nbands, fwl, rrs, a, aw, adg, aph, NULL, &SrOUT);

        for (i = 0; i < 6; i++)
            if (a[i] < aw[i])
                flags |= 0x08;

        for (i = 0; i < 6; i++)
            if (bb[i] < bbw[i])
                flags |= 0x80;


/*
        print_out(nbands, fwl, Rrs, rrs, u, a, aph, adg, aw, bb, bbw);
        printf("\n");
*/

        fprintf(fileout, "%s\n", specname[j]);
        fprintf(fileout, "%f %f %f %f\n", G0G1[0], G0G1[1], ETA, SrOUT);
        fprintf(fileout, "lam   ");
        for ( i = 0; i < nbands; i++ )
	    fprintf(fileout, "%9.0f ", fwl[i]);
        fprintf(fileout, "\n");

        fprintf(fileout, "Rrs   ");
        for ( i = 0; i < nbands; i++ )
	    fprintf(fileout, "%12.8f ", Rrs[i]);
        fprintf(fileout, "\n");

        fprintf(fileout, "rrs   ");
        for ( i = 0; i < nbands; i++ )
	    fprintf(fileout, "%12.8f ", rrs[i]);
        fprintf(fileout, "\n");

        fprintf(fileout, "u     ");
        for ( i = 0; i < nbands; i++ )
	    fprintf(fileout, "%12.8f ", u[i]);
        fprintf(fileout, "\n");

        fprintf(fileout, "a     ");
        for ( i = 0; i < nbands; i++ )
	    fprintf(fileout, "%12.8f ", a[i]);
        fprintf(fileout, "\n");

        fprintf(fileout, "aph   ");
        for ( i = 0; i < nbands; i++ )
	    fprintf(fileout, "%12.8f ", aph[i]);
        fprintf(fileout, "\n");

        fprintf(fileout, "adg   ");
        for ( i = 0; i < nbands; i++ )
	    fprintf(fileout, "%12.8f ", adg[i]);
        fprintf(fileout, "\n");

        fprintf(fileout, "aw    ");
        for ( i = 0; i < nbands; i++ )
	    fprintf(fileout, "%12.8f ", aw[i]);
        fprintf(fileout, "\n");

        fprintf(fileout, "bb    ");
        for ( i = 0; i < nbands; i++ )
	    fprintf(fileout, "%12.8f ", bb[i]);
        fprintf(fileout, "\n");

        fprintf(fileout, "bbw   ");
        for ( i = 0; i < nbands; i++ )
	    fprintf(fileout, "%12.8f ", bbw[i]);
        fprintf(fileout, "\n");

	if ( flags & 0x10 )
	    printf("original aph/a ratio was out of range (0.15 to 0.6)\n");
	if ( flags & 0x20 )
	    printf("    and was forced to minimum (0.15)\n");
	if ( flags & 0x40 )
	    printf("    and was forced to maximum (0.6)\n");

        if (flags & 0x10)
            printf("original aph/a ratio was out of range (0.15 to 0.6)\n");
        if (flags & 0x20)
            printf("    and was forced to minimum (0.15)\n");
        if (flags & 0x40)
            printf("    and was forced to maximum (0.6)\n");
    }

    fclose(fileout);

    return 0;
}
/* #endif */
