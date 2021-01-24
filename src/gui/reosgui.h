#ifndef REOSGUI_EXPORT_H
#define REOSGUI_EXPORT_H


#  if defined _WIN32 || defined __CYGWIN__
#    ifdef reosGui_EXPORTS
#      ifdef __GNUC__
#        define REOSGUI_EXPORT __attribute__ ((dllexport))
#      else
#        define REOSGUI_EXPORT __declspec(dllexport) // Note: actually gcc seems to also supports this syntax.
#      endif
#    else
#      ifdef __GNUC__
#        define REOSGUI_EXPORT __attribute__ ((dllimport))
#      else
#        define REOSGUI_EXPORT __declspec(dllimport) // Note: actually gcc seems to also supports this syntax.
#      endif
#    endif
#  else
#    if __GNUC__ >= 4
#      define REOSGUI_EXPORT __attribute__ ((visibility ("default")))
#    else
#      define REOSGUI_EXPORT
#    endif
#  endif


#endif