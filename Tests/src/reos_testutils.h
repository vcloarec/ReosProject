/***************************************************************************
                      reos_testutils.h
                     --------------------------------------
Date                 : 04-09-2020
Copyright            : (C) 2020 by Vincent Cloarec
email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef REOS_TESTUTILS_H
#define REOS_TESTUTILS_H

#include <string>

const char *data_path();

std::string test_file( std::string basename );

std::string tmp_file( std::string basename );

#endif // REOS_TESTUTILS_H
