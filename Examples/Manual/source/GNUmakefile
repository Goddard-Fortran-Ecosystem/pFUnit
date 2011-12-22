TOP = ../..
include ../../include/base.mk
.PHONY: all

all: libf2kex.a

SRCS = $(wildcard *.F90)
OBJS = $(SRCS:%.F90=%.o)

SRCS_SERIAL=$(filter-out MPI%,$(SRCS))
OBJS_SERIAL=$(SRCS_SERIAL:%.F90=%.o)

libf2kex.a: $(OBJS)
	ar -r $@ $?
	-@ranlib $@



