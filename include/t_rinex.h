/*------------------------------------------------------------------------------
* rtklib unit test driver : rinex function
*-----------------------------------------------------------------------------*/
#include <stdio.h>
#include "../include/rtklib.h"

EXPORT void dumpobs(obs_t *obs);
static void dumpnav(nav_t *nav);
static void dumpsta(sta_t *sta);

/* readrnx(), sortobs(), uniqnav()  */
EXPORT void utest1(void);
/* readrnxt() */
EXPORT void utest2(void);

EXPORT static rnxopt_t opt1;
EXPORT static rnxopt_t opt2;

/* outrneobsh() */
EXPORT void utest3(void);
/* outrneobsb() */
EXPORT void utest4(void);
/* outrnxnavh() */
EXPORT void utest5(void);
/* outrnxnavb() */
EXPORT void utest6(void);

EXPORT void epoch2time_(gtime_t *time, const double *ep);

EXPORT extern int readrnxt_(const char *file, int rcv, gtime_t *ts, gtime_t *te, double tint, const char *opt, obs_t *obs, nav_t *nav, sta_t *sta);

