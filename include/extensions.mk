# Decide the file extensions depending on the platform.

ifeq ($(UNAME),)
LOCAL_UNAME ?=$(shell uname)
ifeq ($(LOCAL_UNAME),)
  LOCAL_UNAME =UNKNOWN
else
# If the LOCAL_UNAME is not Linux, then assume a Windows compilation.
ifneq ($(LOCAL_UNAME),Linux)
LOCAL_UNAME =Windows
endif
endif
else
LOCAL_UNAME := $(UNAME)
endif

# Set the file extensions based on the LOCAL_UNAME.
ifneq ($(LOCAL_UNAME),Windows)
# File extensions for non-Windows.
OBJ_EXT ?= .o
LIB_EXT ?= .a
EXE_EXT ?= .x
else
# File extensions for Windows.
OBJ_EXT ?= .obj
LIB_EXT ?= .lib
EXE_EXT ?= .exe
endif
