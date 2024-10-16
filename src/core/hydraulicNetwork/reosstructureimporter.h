/***************************************************************************
  reosstructureimporter.h - ReosStructureImporter

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
#ifndef REOSSTRUCTUREIMPORTER_H
#define REOSSTRUCTUREIMPORTER_H

#define SIP_NO_FILE

#include "reoscore.h"
#include "reoshydraulicstructure2d.h"

class ReosHydraulicNetwork;
class ReosEncodedElement;
class ReosHydraulicNetworkContext;
class ReosStructureImporterSource;


/**
 * Base class that is an interface to import hydraulic structure from specific simulation engine
 */
class REOSCORE_EXPORT ReosStructureImporter
{
  public:
    ReosStructureImporter( const ReosHydraulicNetworkContext &context );
    virtual ~ReosStructureImporter() = default;

    virtual QString importerKey() const = 0;

    virtual ReosHydraulicStructure2D::Structure2DCapabilities capabilities() const = 0;

    virtual QString crs() const = 0;
    virtual QPolygonF domain() const = 0;

    //! Creates and returnd a mesh
    virtual ReosMesh *mesh( const QString &destinationCrs ) const = 0;

    //! Creates and returnd a mesh for a specific \a scheme associated to a \a structure
    virtual ReosMesh *mesh( ReosHydraulicStructure2D *structure, ReosHydraulicScheme *scheme, const QString &destinationCrs ) const = 0;

    virtual QList<ReosHydraulicStructureBoundaryCondition *> createBoundaryConditions( ReosHydraulicStructure2D *structure, const ReosHydraulicNetworkContext &context ) const = 0;
    virtual QList<ReosHydraulicSimulation *> createSimulations( ReosHydraulicStructure2D *parent ) const = 0;

    //! Updates the boundary condition, remove not exising add new ones
    virtual void updateBoundaryConditions(
      const QSet<QString> &currentBoundaryId,
      ReosHydraulicStructure2D *structure,
      const ReosHydraulicNetworkContext &context ) const = 0;

    virtual bool isValid() const = 0;

    virtual  const ReosStructureImporterSource *source() const = 0;

  protected:
    ReosHydraulicNetwork *mNetWork = nullptr;
};

/**
 * Base class that is an interface that represent a source to import hydraulic structure from specific simulation engine.
 * An instance of this class can create a ReosStructureImporter that is used to import the hydraulic structure
 */
class ReosStructureImporterSource
{
  public:
    virtual ~ReosStructureImporterSource() = default;
    virtual ReosStructureImporterSource *clone() const = 0;
    virtual ReosStructureImporter *createImporter() const = 0;
    virtual ReosEncodedElement encode( const ReosHydraulicNetworkContext &context ) const = 0;
};

/**
 *  A derived class that is used as a place holder when the engine registery does not have the related simulation factory.
 *  An instance of this class contained only the original encoded data that will be saved if project saving is requested
 */
class ReosStructureImporterDummy : public ReosStructureImporter
{
  public:
    ReosStructureImporterDummy( const ReosEncodedElement &element, const ReosHydraulicNetworkContext &context );

    QString importerKey() const override;

    ReosHydraulicStructure2D::Structure2DCapabilities capabilities() const override;
    QString crs() const override;
    QPolygonF domain() const  override;
    ReosMesh *mesh( const QString & ) const override;
    ReosMesh *mesh( ReosHydraulicStructure2D *structure, ReosHydraulicScheme *scheme, const QString &destinationCrs ) const override;

    QList<ReosHydraulicStructureBoundaryCondition *> createBoundaryConditions( ReosHydraulicStructure2D *, const ReosHydraulicNetworkContext & ) const override;

    QList<ReosHydraulicSimulation *> createSimulations( ReosHydraulicStructure2D * ) const override;

    void updateBoundaryConditions( const QSet<QString> &, ReosHydraulicStructure2D *, const ReosHydraulicNetworkContext & ) const override;;

    bool isValid() const override;

  private:
    ReosEncodedElement mElement;
};

class ReosStructureImporterSourceDummy : public ReosStructureImporterSource
{
  public:
    ReosStructureImporterSource *clone() const override;
    explicit ReosStructureImporterSourceDummy( const ReosEncodedElement &element );
    ReosStructureImporter *createImporter() const override;
    ReosEncodedElement encode( const ReosHydraulicNetworkContext & ) const override;

  private:
    ReosEncodedElement mElement;
};


#endif // REOSSTRUCTUREIMPORTER_H
