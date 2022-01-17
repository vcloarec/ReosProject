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
#include "reosmeshgenerator.h"
#include "reosgmshgenerator.h"

ReosHydraulicStructure2D::ReosHydraulicStructure2D( const QPolygonF &domain, const QString &crs, ReosHydraulicNetwork *parent )
  : ReosHydraulicNetworkElement( parent )
  , mPolylinesStructures( ReosPolylinesStructure::createPolylineStructure( domain, crs ) )
  , mMesh( ReosMesh::createMemoryMesh() )
{
  generateMesh();
}

QPolygonF ReosHydraulicStructure2D::domain( const QString &crs ) const
{
  return mPolylinesStructures->boundary( crs );
}

ReosPolylinesStructure *ReosHydraulicStructure2D::geometryStructure() const
{
  return mPolylinesStructures.get();
}

ReosMesh *ReosHydraulicStructure2D::mesh() const
{
  return mMesh.get();
}

bool ReosHydraulicStructure2D::generateMesh()
{
  ReosGmshGenerator generator;
  generator.setGeometryStructure( mPolylinesStructures.get(), QString() );
  return mMesh->generateMesh( generator );
}
