/***************************************************************************
  reospolygonsclassified.cpp - ReosPolygonsClassified

 ---------------------
 begin                : 5.2.2022
 copyright            : (C) 2022 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include "reospolygonsclassified.h"

#include "reospolygonstructure_p.h"

std::unique_ptr<ReosPolygonsClassified> ReosPolygonsClassified::createPolygonStructure( const QString &crs )
{
  return std::make_unique<ReosPolygonsClassified_p>( crs );
}

std::unique_ptr<ReosPolygonsClassified> ReosPolygonsClassified::createPolygonStructure( const ReosEncodedElement &encodedElement )
{
  return std::make_unique<ReosPolygonsClassified_p>( encodedElement );
}
