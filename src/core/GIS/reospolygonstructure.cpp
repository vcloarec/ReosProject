/***************************************************************************
  reospolygonstructure.cpp - ReosPolygonStructure

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
#include "reospolygonstructure.h"

#include "reospolygonstructure_p.h"

std::unique_ptr<ReosPolygonStructure> ReosPolygonStructure::createPolygonStructure( const QString &crs )
{
  return std::make_unique<ReosPolygonStructure_p>( crs );
}

std::unique_ptr<ReosPolygonStructure> ReosPolygonStructure::createPolygonStructure( const ReosEncodedElement &encodedElement )
{
  if ( encodedElement.description() != QStringLiteral( "polygon-structure" ) )
    return nullptr;

  return std::make_unique<ReosPolygonStructure_p>( encodedElement );
}
