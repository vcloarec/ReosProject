/***************************************************************************
  reoshydrograph.cpp
 ---------------------
 begin                : 19.5.2021
 copyright            : (C) 2021 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "reoshydrograph.h"



QColor ReosHydrograph::color() const
{
  return mColor;
}

void ReosHydrograph::setColor( const QColor &color )
{
  mColor = color;
}
