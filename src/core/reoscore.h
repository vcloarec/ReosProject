#ifndef REOSCORE_EXPORT_H
#define REOSCORE_EXPORT_H

#include "reos_sip.h"

#define SIP_NO_FILE

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

#ifndef REOSEXTERN
#ifdef Q_OS_WIN
#  define REOSEXTERN extern "C" __declspec( dllexport )
#else
#  if defined(__GNUC__) || defined(__clang__)
#    define REOSEXTERN extern "C" __attribute__ ((visibility ("default")))
#  else
#    define REOSEXTERN extern "C"
#  endif
#endif
#endif

#endif
