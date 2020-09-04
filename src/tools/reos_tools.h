#ifndef REOS_TOOLS_H
#define REOS_TOOLS_H


#  if defined _WIN32 || defined __CYGWIN__
#    ifdef tools_EXPORTS
#      ifdef __GNUC__
#        define TOOLS_EXPORT __attribute__ ((dllexport))
#      else
#        define TOOLS_EXPORT __declspec(dllexport) // Note: actually gcc seems to also supports this syntax.
#      endif
#    else
#      ifdef __GNUC__
#        define TOOLS_EXPORT __attribute__ ((dllimport))
#      else
#        define TOOLS_EXPORT __declspec(dllimport) // Note: actually gcc seems to also supports this syntax.
#      endif
#    endif
#  else
#    if __GNUC__ >= 4
#      define TOOLS_EXPORT __attribute__ ((visibility ("default")))
#    else
#      define TOOLS_EXPORT
#    endif
#  endif


#endif // REOS_TOOLS_H
