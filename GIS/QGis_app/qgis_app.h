
#ifndef APP_EXPORT_H
#define APP_EXPORT_H

#ifdef __linux
# define EXPORT_REOS __attribute__((visibility("default")))
# define IMPORT_REOS __attribute__((visibility("default")))
#endif

#ifdef WIN32
# define EXPORT_REOS __declspec(dllexport)
# define IMPORT_REOS __declspec(dllimport)
#endif

#ifdef APP_STATIC_DEFINE
#  define APP_EXPORT
#  define APP_NO_EXPORT
#else
#  ifndef APP_EXPORT
#    ifdef qgis_app_EXPORTS
        /* We are building this library */
#      define APP_EXPORT EXPORT_REOS
#    else
        /* We are using this library */
#      define APP_EXPORT IMPORT_REOS
#    endif
#  endif

#  ifndef APP_NO_EXPORT
#    define APP_NO_EXPORT
#  endif
#endif

#ifndef APP_DEPRECATED
#  define APP_DEPRECATED __declspec(deprecated)
#endif

#ifndef APP_DEPRECATED_EXPORT
#  define APP_DEPRECATED_EXPORT APP_EXPORT APP_DEPRECATED
#endif

#ifndef APP_DEPRECATED_NO_EXPORT
#  define APP_DEPRECATED_NO_EXPORT APP_NO_EXPORT APP_DEPRECATED
#endif

#if 0 /* DEFINE_NO_DEPRECATED */
#  ifndef APP_NO_DEPRECATED
#    define APP_NO_DEPRECATED
#  endif
#endif

#endif /* APP_EXPORT_H */
