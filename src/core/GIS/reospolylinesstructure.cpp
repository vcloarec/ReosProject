/***************************************************************************
  reospolylinesstructures.cpp - ReosPolylinesStructures

 ---------------------
 begin                : 10.1.2022
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
#include "reospolylinesstructure.h"
#include "reospolylinesstructure_p.h"

std::unique_ptr<ReosPolylinesStructure> ReosPolylinesStructure::createPolylineStructure( const QString &crs )
{
  return std::unique_ptr<ReosPolylinesStructure>( new ReosPolylineStructureVectorLayer( crs ) );
}

std::unique_ptr<ReosPolylinesStructure> ReosPolylinesStructure::createPolylineStructure( const QPolygonF &boundary, const QString &crs )
{
  return std::unique_ptr<ReosPolylinesStructure>( new ReosPolylineStructureVectorLayer( boundary, crs ) );
}

std::unique_ptr<ReosPolylinesStructure> ReosPolylinesStructure::createPolylineStructure( const ReosEncodedElement &encodedElement )
{
  return std::unique_ptr<ReosPolylinesStructure>( new ReosPolylineStructureVectorLayer( encodedElement ) );
}

