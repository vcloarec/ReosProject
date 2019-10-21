/***************************************************************************
                      reosutils.cpp
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

#include "reosutils.h"

std::vector<std::string> splitString( const std::string &input, const char &delimiter, bool returnEmpty )
{
  //https://stackoverflow.com/questions/5167625/splitting-a-c-stdstring-using-tokens-e-g
  std::vector<std::string> returnList;
  std::istringstream f( input.c_str() );
  std::string str;
  while ( std::getline( f, str, delimiter ) )
  {
    if ( returnEmpty || str != "" )
      returnList.push_back( str );
  }

  return returnList;

}

bool equality( double d1, double d2, double epsilon )
{
  return fabs( d1 - d2 ) <= fabs( std::max( d1, d2 ) * epsilon );
}

bool equality( double d1, double d2 )
{
  return equality( d1, d2, std::numeric_limits<double>::epsilon() );
}
