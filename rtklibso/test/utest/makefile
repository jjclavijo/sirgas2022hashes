# makefile for rtklib unit test

SRC    = ../../src
#CFLAGS = -Wall -O3 -ansi -pedantic -I$(SRC) -DENAGLO
CFLAGS = -Wall -O3 -ansi -pedantic -I$(SRC) -DTRACE -DENAGLO -DENAQZS
LDLIBS = -lm -llapack -lblas -L ../../ -lrtklib
CC = gcc

BIN    = t_matrix t_time t_coord t_rinex t_lambda t_atmos t_misc t_preceph t_gloeph \
t_geoid t_ppp t_ionex t_tle

all        : $(BIN) utest
t_matrix   : t_matrix.o # rtkcmn.o preceph.o
t_time     : t_time.o # rtkcmn.o preceph.o
t_coord    : t_coord.o # rtkcmn.o geoid.o preceph.o
t_rinex    : t_rinex.o # rtkcmn.o rinex.o preceph.o
t_lambda   : t_lambda.o # rtkcmn.o lambda.o preceph.o
t_atmos    : t_atmos.o # rtkcmn.o preceph.o
t_misc     : t_misc.o # rtkcmn.o preceph.o
t_preceph  : t_preceph.o # rtkcmn.o preceph.o rinex.o ephemeris.o sbas.o qzslex.o
t_gloeph   : t_gloeph.o # rtkcmn.o rinex.o ephemeris.o sbas.o preceph.o qzslex.o
t_geoid    : t_geoid.o # rtkcmn.o preceph.o geoid.o
t_ppp      : t_ppp.o # rtkcmn.o ephemeris.o preceph.o sbas.o ionex.o pntpos.o ppp.o ppp_ar.o qzslex.o
# t_ppp      : lambda.o
t_ionex    : t_ionex.o # rtkcmn.o preceph.o ionex.o
t_tle      : t_tle.o # rtkcmn.o rinex.o ephemeris.o sbas.o preceph.o tle.o

utest : utest1 utest2 utest3 utest4 utest5 utest6 utest7 utest8
utest : utest9 utest10 utest11 utest12 utest14

utest1 :
	-LD_LIBRARY_PATH=../../ ./t_matrix  > utest1.out
utest2 :
	-LD_LIBRARY_PATH=../../ ./t_time    > utest2.out
utest3 :
	-LD_LIBRARY_PATH=../../ ./t_coord   > utest3.out
utest4 :
	-LD_LIBRARY_PATH=../../ ./t_rinex   > utest4.out
utest5 :
	-LD_LIBRARY_PATH=../../ ./t_lambda  > utest5.out
utest6 :
	-LD_LIBRARY_PATH=../../ ./t_atmos   > utest6.out
utest7 :
	-LD_LIBRARY_PATH=../../ ./t_misc    > utest7.out
utest8 :
	-LD_LIBRARY_PATH=../../ ./t_preceph > utest8.out
utest9 :
	-LD_LIBRARY_PATH=../../ ./t_gloeph  > utest9.out
utest10 :
#	./t_geoid   > utest10.out
utest11 :
	-LD_LIBRARY_PATH=../../ ./t_ppp     > utest11.out
utest12 :
	-LD_LIBRARY_PATH=../../ ./t_ionex   > utest12.out
utest14 :
	-LD_LIBRARY_PATH=../../ ./t_tle     > utest14.out

clean :
	rm -f *.o *.out *.exe $(BIN) *.stackdump gmon.out

