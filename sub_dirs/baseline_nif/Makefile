#
 OUTPUT = priv/lib/$(notdir $(PWD)).so

 OS = $(shell uname -s)

#
 CC ?= gcc

 CFLAGS ?= -std=c99 -m64 -Wextra -Wstrict-prototypes -Wno-missing-field-initializers -fno-common
 CFLAGS += -g -Wall -fPIC -MMD
 CFLAGS += -I $(ERLANG_ROOT_DIR)/erts-$(ERLANG_ERTS_VER)/include
 CFLAGS += -I $(ERLANG_LIB_DIR_erl_interface)/include
#CFLAGS += -DUSE_DRIVER_ASYNC_PORT_KEY=1


 CXX ?= g++

 CXXFLAGS = $(CFLAGS)


 LD ?= ld

 LDFLAGS ?= -arch x86_64

ifneq (,$(findstring $(OS), Darwin))
 LDFLAGS += -bundle -flat_namespace -undefined suppress
endif

ifneq (,$(findstring $(OS), Linux FreeBSD))
 LDFLAGS += -shared
endif


 LDLIBS ?=
 LDLIBS += -L $(ERLANG_LIB_DIR_erl_interface)/lib -lerl_interface -lei

#
 SRC = $(shell find -E . -type f -iregex '.*\.(c|cc|cp|cpp|cxx|c\+\+)')
 OBJ = $(addsuffix .o, $(basename $(SRC)))

$(OUTPUT): $(OBJ) dest
	$(LD) -o $(OUTPUT) $(OBJ) $(LDFLAGS) $(LDLIBS)

dest:
	@-mkdir -p $(dir $(OUTPUT))

clean:
	@rm -f $(OUTPUT) $(OBJ)
