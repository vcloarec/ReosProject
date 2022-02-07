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
  , mMeshResolutionController( new ReosGmshResolutionController( this, crs ) )
  , mMesh( ReosMesh::createMemoryMesh() )
  , mAutoMeshUpdate( new ReosParameterBoolean( tr( "Auto update mesh" ), false, this ) )
{
  mAutoMeshUpdate->setValue( true );
  if ( mAutoMeshUpdate->value() )
    generateMesh();
}


ReosHydraulicStructure2D::ReosHydraulicStructure2D(
  const ReosEncodedElement &encodedElement,
  ReosHydraulicNetwork *parent )
  : ReosHydraulicNetworkElement( parent )
  , mPolylinesStructures( ReosPolylinesStructure::createPolylineStructure(
                            encodedElement.getEncodedData( QStringLiteral( "structure" ) ) ) )
  , mMeshResolutionController( new ReosGmshResolutionController( this ) )
  , mMesh( ReosMesh::createMemoryMesh() )
  , mAutoMeshUpdate( ReosParameterBoolean::decode(
                       encodedElement.getEncodedData( QStringLiteral( "auto-update" ) ),
                       false,
                       tr( "Auto update mesh" ),
                       this ) )
{
  if ( mAutoMeshUpdate->value() )
    generateMesh();
}

ReosParameterBoolean *ReosHydraulicStructure2D::autoMeshUpdate() const
{
  return mAutoMeshUpdate;
}

QPolygonF ReosHydraulicStructure2D::domain( const QString &crs ) const
{
  return mPolylinesStructures->boundary( crs );
}

ReosPolylinesStructure *ReosHydraulicStructure2D::geometryStructure() const
{
  return mPolylinesStructures.get();
}

ReosMeshResolutionController *ReosHydraulicStructure2D::meshResolutionController() const
{
  return mMeshResolutionController;
}

ReosMesh *ReosHydraulicStructure2D::mesh() const
{
  return mMesh.get();
}

bool ReosHydraulicStructure2D::generateMesh()
{
  ReosGmshGenerator generator;
  generator.setGeometryStructure( mPolylinesStructures.get(), QString() );
  generator.setResolutionController( mMeshResolutionController );
  if ( mMesh )
    return mMesh->generateMesh( generator );
  else
    return false;
}

ReosHydraulicStructure2D *ReosHydraulicStructure2D::create( const ReosEncodedElement &encodedElement, ReosHydraulicNetwork *parent )
{
  if ( encodedElement.description() != ReosHydraulicStructure2D::staticType() )
    return nullptr;

  return new ReosHydraulicStructure2D( encodedElement, parent );

}

void ReosHydraulicStructure2D::encodeData( ReosEncodedElement &element, const ReosHydraulicNetworkContext & ) const
{
  element.addEncodedData( QStringLiteral( "structure" ), mPolylinesStructures->encode() );
  element.addEncodedData( QStringLiteral( "auto-update" ), mAutoMeshUpdate->encode() );
}


ReosHydraulicNetworkElement *ReosHydraulicStructure2dFactory::decodeElement( const ReosEncodedElement &encodedElement, const ReosHydraulicNetworkContext &context ) const
{
  if ( encodedElement.description() != ReosHydraulicStructure2D::staticType() )
    return nullptr;

  return ReosHydraulicStructure2D::create( encodedElement, context.network() );
}
