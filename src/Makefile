ROOT := ..

ifeq ($(OS),Windows_NT)
  OBJ_EXT := .obj
  EXE_EXT := .exe
  LINK := link
  CC := cl
  CCFLAGS := /c /nologo
  CCINC := /I
  CCOUT := /Fo:
  LINKOUT := /out:
  LINKFLAGS := /nologo
  RM0 := del /Q
  CP := copy /f /q
else
  OBJ_EXT := .o
  EXE_EXT :=
  LINK := g++
  CC := g++
  CCFLAGS := -c -g
  CCINC := -I
  CCOUT := -o
  LINKOUT := -o
  LINKFLAGS := -g
  RM0 := rm -f
  CP := cp -f

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

.PHONY: clean run all test

#
# To fool GNU-Make that something has been done.
#
ifeq ($(OS),Windows_NT)
NOTHING := touch kalle.xxx
else
NOTHING := $(shell pwd >/dev/null)
endif

ifeq ($(OS),Windows_NT)
define echon
	echo xxx >/tmp/nul.txt
	cmd.exe /c set /p dummyName="$1 " </tmp/nul.txt
endef
else
define RM
	echo $1 $(nonl)
endef
endif

ifeq ($(OS),Windows_NT)
define slashfix
	$(subst /,\\,$1)
endef
else
define slashfix
	$1
endef
endif

ifeq ($(OS),Windows_NT)
define RM
	$(foreach F,$1,$(if $(wildcard $F), $(shell cmd.exe /c $(RM0) $(call slashfix, $F))))
endef
else
define RM
	$(RM0) $1
endef
endif

define printerr
        if [ -s /tmp/err.log ]; \
        then \
            echo $(red) $(bold) && cat /tmp/err.log && echo $(off) $(white) && exit 1; \
        fi;
endef

define printtest
	(echo ; while read -r line; do if [[ $$line =~ (.*[Ee][Rr][Rr][Oo][Rr].*|.*[Ff][Aa][Ii][Ll].*) ]]; then echo $(red)$(bold)$$line$(off)$(white); else echo $$line; fi; done </tmp/err.log; exit 1;)
endef

CPP_FILES := $(wildcard $(SRC)/*.cpp)
OBJ_FILES := $(addprefix $(OUT)/, $(CPP_FILES:$(SRC)/%.cpp=%$(OBJ_EXT)))
OBJ_FILES0 := $(filter-out $(OUT)/main$(OBJ_EXT), $(OBJ_FILES))
TEST_CPP_FILES := $(wildcard $(SRC)/test/*.cpp)
TEST_OBJ_FILES := $(addprefix $(OUT)/test/, $(TEST_CPP_FILES:$(SRC)/test/%.cpp=%$(OBJ_EXT)))
TEST_EXE_FILES := $(addprefix $(BIN)/test/, $(TEST_CPP_FILES:$(SRC)/test/%.cpp=%$(EXE_EXT)))
TEST_OK_FILES := $(addprefix $(BIN)/test/, $(TEST_CPP_FILES:$(SRC)/test/%.cpp=%$(EXE_EXT).ok))
TEST_LOG_FILES := $(addprefix $(BIN)/test/, $(TEST_CPP_FILES:$(SRC)/test/%.cpp=%$(EXE_EXT).log))

GOAL := $(BIN)/omegal$(EXE_EXT)

all : $(GOAL)

test : $(TEST_OK_FILES)

.SECONDARY :

$(GOAL) : $(BIN) $(OUT) $(OBJ_FILES)
	@mkdir -p /tmp
	@(echo $(green) $(bold) $@ $(off) $(white))
	@$(LINK) $(LINKFLAGS) $(LINKOUT)$@ $(OBJ_FILES)

$(BIN)/test/%$(EXE_EXT) : $(OUT)/test/%$(OBJ_EXT) $(OBJ_FILES0)
	@mkdir -p $(BIN)/test/
	@$(call RM, /tmp/err.log)
	@($(LINK) $(LINKFLAGS) $(LINKOUT)$@ $< $(OBJ_FILES0) 2>/tmp/err.log 1>&2) || $(printerr)
	@$(call RM, /tmp/err.log)

$(BIN)/test/%$(EXE_EXT).ok : $(BIN)/test/%$(EXE_EXT)
	@$(call echon, $(yellow) $(bold) $<$(off)$(white)) 
	@( $< >$<.log 2>&1) || ($(CP) $<.log /tmp/err.log; exit 1) || $(printtest)
	@touch $@
	@(echo $(green)$(bold) [OK]$(off)$(white))

$(OUT)/test/%$(OBJ_EXT) : $(SRC)/test/%.cpp
	@$(call echon, $(green) $(bold)  $(notdir $<) $(off) $(white)) 
	@mkdir -p /tmp
	@$(call RM, /tmp/err.log)
	@mkdir -p $(OUT)/test/
	@($(CC) $(CCFLAGS) $(CCINC) $(SRC) $< $(CCOUT) $@ 2>/tmp/err.log 1>&2) || $(printerr)

$(OUT)/%$(OBJ_EXT) : $(SRC)/%.cpp
	@(echo $(green) $(bold) $(notdir $<) $(off) $(white))
	@mkdir -p /tmp
	@$(call RM, /tmp/err.log)
	@($(CC) $(CCFLAGS) $< $(CCOUT) $@ 2>/tmp/err.log 1>&2) || $(printerr)

$(OUT) :
	@mkdir -p $@

$(BIN) :
	@mkdir -p $@

clean :
	@touch /tmp/err.log
	@$(call RM, /tmp/err.log)
	@$(call RM, $(OBJ_FILES) $(GOAL) $(TEST_OBJ_FILES) $(TEST_EXE_FILES) $(TEST_OK_FILES) $(TEST_LOG_FILES))

run : $(GOAL)
	$(GOAL)
