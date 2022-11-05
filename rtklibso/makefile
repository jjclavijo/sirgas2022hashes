# 

BINDIR  = /usr/local/bin
SRCDIR  = ./src

SHAREDOPTS = -fPIC #-DWIN_DLL
OPTS    = -DTRACE -DENAGLO -DENAQZS -DENAGAL -DNFREQ=3 ${SHAREDOPTS}

# for no lapack
#CFLAGS  = -Wall -O3 -ansi -pedantic -Wno-unused-but-set-variable -I$(SRCDIR) $(OPTS) -g
#LDLIBS  = -lm -lrt

CFLAGS  = -Wall -O3 -ansi -pedantic -Wno-unused-but-set-variable -I$(SRCDIR) -DLAPACK $(OPTS)
LDLIBS  = -lm -lrt -llapack -lblas

# for gprof
#CFLAGS  = -Wall -O3 -ansi -pedantic -Wno-unused-but-set-variable -I$(SRCDIR) -DLAPACK $(OPTS) -pg
#LDLIBS  = -lm -lrt -llapack -lblas -pg

# for mkl
#MKLDIR  = /opt/intel/mkl
#CFLAGS  = -O3 -ansi -pedantic -Wno-unused-but-set-variable -I$(SRCDIR) $(OPTS) -DMKL
#LDLIBS  = -L$(MKLDIR)/lib/intel64 -lm -lrt -lmkl_core -lmkl_intel_lp64 -lmkl_gnu_thread -liomp5 -lpthread

SRC     = ${wildcard src/*.c} ${wildcard src/rcv/*.c}
OBJ1 		= ${patsubst src/%.c,build/%.o,${SRC}}
OBJ 		= ${patsubst build/rcv/%.o,build/%.o,${OBJ1}}

default: all

build:
	mkdir -p build

VPATH=src/:src/rcv
build/%.o : %.c | build src/rtklib.h
	$(CC) -c $(CFLAGS) $< -o $@

rel/librtklib.so: ${OBJ}
	gcc -shared ${OBJ} ${LDLIBS} -o $@

all: rel/librtklib.so


