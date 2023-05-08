/***************************************************************************
  reos_python.h - %{Cpp:License:ClassName}

 ---------------------
 begin                : 12.3.2023
 copyright            : (C) 2023 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef REOS_PYTHON_H
#define REOS_PYTHON_H

#ifdef PYTHON_STATIC_DEFINE
#  define PYTHON_EXPORT
#  define PYTHON_NO_EXPORT
#else
#  ifndef REOS_PYTHON_EXPORT
#    ifdef reosPython_EXPORTS
/* We are building this library */
#      define REOS_PYTHON_EXPORT __attribute__((visibility("default")))
#    else
/* We are using this library */
#      define REOS_PYTHON_EXPORT __attribute__((visibility("default")))
#    endif
#  endif

#  ifndef REOS_PYTHON_NO_EXPORT
#    define REOS_PYTHON_NO_EXPORT __attribute__((visibility("hidden")))
#  endif
#endif

#ifndef PYTHON_DEPRECATED
#  define PYTHON_DEPRECATED __attribute__ ((__deprecated__))
#endif

#ifndef PYTHON_DEPRECATED_EXPORT
#  define PYTHON_DEPRECATED_EXPORT PYTHON_EXPORT PYTHON_DEPRECATED
#endif

#ifndef PYTHON_DEPRECATED_NO_EXPORT
#  define PYTHON_DEPRECATED_NO_EXPORT PYTHON_NO_EXPORT PYTHON_DEPRECATED
#endif


#endif // REOS_PYTHON_H
