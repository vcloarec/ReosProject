/***************************************************************************
  reosstructureimporter.cpp - ReosStructureImporter

 ---------------------
 begin                : 19.1.2023
 copyright            : (C) 2023 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include "reosstructureimporter.h"

ReosStructureImporter::ReosStructureImporter( const ReosHydraulicNetworkContext &context ):
  mNetWork( context.network() )
{}

ReosStructureImporterSource *ReosStructureImporterSourceDummy::clone() const
{
  return new ReosStructureImporterSourceDummy( mElement );
}

ReosStructureImporterSourceDummy::ReosStructureImporterSourceDummy( const ReosEncodedElement &element )
  : mElement( element )
{}

ReosStructureImporter *ReosStructureImporterSourceDummy::createImporter() const {return nullptr;}

ReosEncodedElement ReosStructureImporterSourceDummy::encode( const ReosHydraulicNetworkContext & ) const
{
  return mElement;
}

ReosStructureImporterDummy::ReosStructureImporterDummy( const ReosEncodedElement &element, const ReosHydraulicNetworkContext &context )
  : ReosStructureImporter( context )
  , mElement( element )
{
}

QString ReosStructureImporterDummy::importerKey() const {return QString();}

ReosHydraulicStructure2D::Structure2DCapabilities ReosStructureImporterDummy::capabilities() const {return ReosHydraulicStructure2D::Structure2DCapabilities();}

QString ReosStructureImporterDummy::crs() const {return QString();}

QPolygonF ReosStructureImporterDummy::domain() const {return QPolygonF();}

ReosMesh *ReosStructureImporterDummy::mesh( const QString & ) const {return nullptr;}

QList<ReosHydraulicStructureBoundaryCondition *> ReosStructureImporterDummy::createBoundaryConditions( ReosHydraulicStructure2D *, const ReosHydraulicNetworkContext & ) const
{return QList<ReosHydraulicStructureBoundaryCondition *>();}

QList<ReosHydraulicSimulation *> ReosStructureImporterDummy::createSimulations( ReosHydraulicStructure2D * ) const {return QList<ReosHydraulicSimulation *>();}

void ReosStructureImporterDummy::updateBoundaryConditions( const QSet<QString> &, ReosHydraulicStructure2D *, const ReosHydraulicNetworkContext & ) const {}

bool ReosStructureImporterDummy::isValid() const {return false;}

ReosEncodedElement ReosStructureImporterDummy::encode( const ReosHydraulicNetworkContext & ) const {return mElement;}
