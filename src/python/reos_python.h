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

#define SIP_NO_FILE

#  if defined _WIN32 || defined __CYGWIN__
#    ifdef reosPython_EXPORTS
#      ifdef __GNUC__
#        define REOS_PYTHON_EXPORT __attribute__ ((dllexport))
#      else
#        define REOS_PYTHON_EXPORT __declspec(dllexport) // Note: actually gcc seems to also supports this syntax.
#      endif
#    else
#      ifdef __GNUC__
#        define REOS_PYTHON_EXPORT __attribute__ ((dllimport))
#      else
#        define REOS_PYTHON_EXPORT __declspec(dllimport) // Note: actually gcc seems to also supports this syntax.
#      endif
#    endif
#  else
#    if __GNUC__ >= 4
#      define REOS_PYTHON_EXPORT __attribute__ ((visibility ("default")))
#    else
#      define REOS_PYTHON_EXPORT
#    endif
#  endif


#endif // REOS_PYTHON_H
