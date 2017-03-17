if(WITH_ERLANG)

  find_program(ERL NAMES erl HINTS ${WITH_ERLANG}/bin)

  if(ERL)

    function(_ROOT_DIR OUTPUT)
      execute_process(
        COMMAND ${ERL} -noshell -eval "io:format(\"~s\", [code:root_dir()])" -s erlang halt
        OUTPUT_VARIABLE _OUTPUT
        ERROR_QUIET)
      set(${OUTPUT} ${_OUTPUT} PARENT_SCOPE)
    endfunction()

    _ROOT_DIR(ERL_ROOT_DIRECTORY)

    if (ERL_ROOT_DIRECTORY)
      message(STATUS "Using erlang: ${ERL_ROOT_DIRECTORY}")
      include_directories(${ERL_ROOT_DIRECTORY}/usr/include)
      link_directories(${ERL_ROOT_DIRECTORY}/usr/lib)
      list(APPEND LIBS erl_interface ei)
    endif()

  endif()

endif()
