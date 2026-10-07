/***************************************************************************
  reostelemacstructureimportersource.h - ReosTelemacStructureImporterSource

 ---------------------
 begin                : 4.10.2026
 copyright            : (C) 2026 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef REOSTELEMACSTRUCTUREIMPORTERSOURCE_H
#define REOSTELEMACSTRUCTUREIMPORTERSOURCE_H

#include "reosstructureimporter.h"
#include "reostelemacsteeringfile.h"
#include "reostelemacboundaries.h"

class ReosHydraulicNetwork;

class ReosTelemacStructureImporterSource : public ReosStructureImporterSource
{
  public:
    ReosTelemacStructureImporterSource( const QString &steeringFile, const ReosHydraulicNetworkContext &context );
    virtual ~ReosTelemacStructureImporterSource() = default;

    virtual ReosStructureImporterSource *clone() const override;
    virtual ReosStructureImporter *createImporter() const override;
    virtual ReosEncodedElement encode( const ReosHydraulicNetworkContext &context ) const;

  private:
    QString mSteeringFilePath;
    ReosHydraulicNetwork *mNetwork = nullptr;
};

class ReosTelemacStructureImporter : public ReosStructureImporter
{
  public:
    ReosTelemacStructureImporter( const QString &steeringFile, const ReosHydraulicNetworkContext &context, const ReosTelemacStructureImporterSource *source );
    virtual ~ReosTelemacStructureImporter() = default;

    virtual QString importerKey() const {};

    virtual ReosHydraulicStructure2D::Structure2DCapabilities capabilities() const;

    virtual QString crs() const;

    virtual QPolygonF domain() const;

    //! Creates and returns a mesh
    virtual ReosMesh *mesh( const QString &destinationCrs ) const;

    //! Creates and returnd a mesh for a specific \a scheme associated to a \a structure
    virtual ReosMesh *mesh( ReosHydraulicStructure2D *structure, ReosHydraulicScheme *scheme, const QString &destinationCrs ) const {};

    virtual QList<ReosHydraulicStructureBoundaryCondition *> createBoundaryConditions( ReosHydraulicStructure2D *structure, const ReosHydraulicNetworkContext &context ) const;;
    virtual QList<ReosHydraulicSimulation *> createSimulations( ReosHydraulicStructure2D *parent ) const {};

    //! Updates the boundary condition, remove not exising add new ones
    virtual void updateBoundaryConditions( const QSet<QString> &currentBoundaryId, ReosHydraulicStructure2D *structure, const ReosHydraulicNetworkContext &context ) const {};

    virtual bool isValid() const;

    virtual const ReosStructureImporterSource *source() const { return mSource; };

  private:
    QDir mDirectory;
    ReosTelemacSteeringFile mSteeringFile;
    mutable std::unique_ptr<ReosMesh> mGeometryMesh;
    mutable ReosTelemacBoundaries mBoundaries;
    const ReosTelemacStructureImporterSource *mSource = nullptr;

    void loadGeometry() const;
};


#endif // REOSTELEMACSTRUCTUREIMPORTERSOURCE_H
