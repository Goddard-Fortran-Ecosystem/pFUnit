TOP = ../../..
include ../../../include/base.mk

PFUNIT_MODDIR ?=$(TOP)/source
#PFUNIT_BINDIR ?=$(PFUNIT)/bin

INCLUDE_DIR +=-I$(PFUNIT_MODDIR)






.PHONY: all

all: libf2kex.a

SRCS = $(wildcard *.F90)
OBJS = $(SRCS:%.F90=%.o)

SRCS_SERIAL=$(filter-out MPI%,$(SRCS))
OBJS_SERIAL=$(SRCS_SERIAL:%.F90=%.o)

libf2kex.a: $(OBJS) 
	ar -r $@ $?
	-@ranlib $@



