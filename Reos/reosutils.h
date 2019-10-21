/***************************************************************************
                      reosutils.h
                     --------------------------------------
Date                 : 12-07-2019
Copyright            : (C) 2019 by Vincent Cloarec
email                : vcloarec@gmail.com projetreos@gmail.com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef REOSUTILS_H
#define REOSUTILS_H



#include <vector>
#include <sstream>
#include <iostream>
#include <string>
#include <cmath>
#include <limits>
#include <algorithm>


std::vector<std::string> splitString( const std::string &input, const char &delimiter, bool returnEmpty = false );


bool equality( double d1, double d2, double epsilon );
bool equality( double d1, double d2 );


#endif // REOSUTILS_H
