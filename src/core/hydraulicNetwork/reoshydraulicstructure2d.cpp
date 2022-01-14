/***************************************************************************
  reoshydraulicstructure2d.cpp - ReosHydraulicStructure2D

 ---------------------
 begin                : 9.1.2022
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
#include "reoshydraulicstructure2d.h"

ReosHydraulicStructure2D::ReosHydraulicStructure2D( const QPolygonF &domain, const QString &crs, ReosHydraulicNetwork *parent )
  : ReosHydraulicNetworkElement( parent )
  , mPolylinesStructures( ReosPolylinesStructure::createPolylineStructure( crs ) )
{
  mPolylinesStructures->setBoundary( domain, crs );
}

QPolygonF ReosHydraulicStructure2D::domain( const QString &crs ) const
{
  return mPolylinesStructures->boundary( crs );
}
