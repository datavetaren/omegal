ROOT := ..

ifeq ($(OS),Windows_NT)
  OBJ_EXT := .obj
  EXE_EXT := .exe
  LINK := link
  CC := cl
  CCFLAGS := /c /nologo
  CCOUT := /Fo:
  LINKOUT := /out:
  LINKFLAGS := /nologo
else
  OBJ_EXT := .o
  EXE_EXT :=
  LINK := g++
  CC := g++
  CCFLAGS := -c
  CCOUT := -o
  LINKOUT := -o

  green="\033[0;32m"
  yellow="\033[0;33m"
  red="\033[0;31m"
  white="\033[0;37m"
  bold="\033[1m"
  off="\033[0m"
  nonl="\c"

endif

SRC := $(ROOT)/src
OUT := $(ROOT)/out
BIN := $(ROOT)/bin

.PHONY: clean run

define printerr
        if [ -s /tmp/err.log ]; \
        then \
            echo $(red) $(bold) && cat /tmp/err.log && echo $(off) $(white) && exit 1; \
        fi;
endef

CPP_FILES := $(wildcard $(SRC)/*.cpp)
OBJ_FILES := $(addprefix $(OUT)/, $(CPP_FILES:$(SRC)/%.cpp=%$(OBJ_EXT)))

GOAL := $(BIN)/omegal$(EXE_EXT)

$(GOAL) : $(BIN) $(OUT) $(OBJ_FILES)
	@mkdir -p /tmp
	@(echo $(green) $(bold) $@ $(off) $(white))
	@$(LINK) $(LINKFLAGS) $(LINKOUT)$@ $(OBJ_FILES)

$(OUT)/%$(OBJ_EXT) : $(SRC)/%.cpp
	@(echo $(green) $(bold) $(notdir $<) $(off) $(white))
	@mkdir -p /tmp
	@(rm -f /tmp/err.log)
	@($(CC) $(CCFLAGS) $< $(CCOUT) $(SRC)/$@ 2>/tmp/err.log 1>&2) || $(printerr)

$(OUT) :
	@mkdir -p $@

$(BIN) :
	@mkdir -p $@

clean : $(OBJ_FILES) $(GOAL)
	rm -f $^
	rm -f *~

run : $(GOAL)
	$(GOAL)
