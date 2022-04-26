
#ifndef APP_EXPORT_H
#define APP_EXPORT_H

#ifdef APP_STATIC_DEFINE
#  define APP_EXPORT
#  define APP_NO_EXPORT
#else
#ifdef _WIN32
#  ifndef APP_EXPORT
#    ifdef qgis_app_EXPORTS
/* We are building this library */
#      define APP_EXPORT __declspec(dllexport)
#    else
/* We are using this library */
#      define APP_EXPORT __declspec(dllimport)
#    endif
#  endif

#  ifndef APP_NO_EXPORT
#    define APP_NO_EXPORT 
#  endif
#else
#  ifndef APP_EXPORT
#    ifdef qgis_app_EXPORTS
        /* We are building this library */
#      define APP_EXPORT __attribute__((visibility("default")))
#    else
        /* We are using this library */
#      define APP_EXPORT __attribute__((visibility("default")))
#    endif
#  endif

#  ifndef APP_NO_EXPORT
#    define APP_NO_EXPORT __attribute__((visibility("hidden")))
#  endif
#endif
#endif

#ifndef APP_DEPRECATED
#  define APP_DEPRECATED __attribute__ ((__deprecated__))
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
