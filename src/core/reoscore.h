#ifndef REOSCORE_EXPORT_H
#define REOSCORE_EXPORT_H

#  if defined _WIN32 || defined __CYGWIN__
#    ifdef reosCore_EXPORTS
#      ifdef __GNUC__
#        define REOSCORE_EXPORT __attribute__ ((dllexport))
#      else
#        define REOSCORE_EXPORT __declspec(dllexport) // Note: actually gcc seems to also supports this syntax.
#      endif
#    else
#      ifdef __GNUC__
#        define REOSCORE_EXPORT __attribute__ ((dllimport))
#      else
#        define REOSCORE_EXPORT __declspec(dllimport) // Note: actually gcc seems to also supports this syntax.
#      endif
#    endif
#  else
#    if __GNUC__ >= 4
#      define REOSCORE_EXPORT __attribute__ ((visibility ("default")))
#    else
#      define REOSCORE_EXPORT
#    endif
#  endif


#endif
