/***************************************************************************
  reoshecrassimulation.h - ReosHecRasSimulation

 ---------------------
 begin                : 06.10.2022
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
#ifndef REOSHECRASSIMULATION_H
#define REOSHECRASSIMULATION_H

#include "reoshydraulicsimulation.h"
#include "reoshydraulicstructure2d.h"
#include "reoshecrasproject.h"

class ReosHecRasSimulation
{
  public:
    static QString staticKey() { return QStringLiteral( "hecras" ); }
};


class ReosHecRasSimulationEngineFactory : public ReosSimulationEngineFactory
{
  public:
    ReosHecRasSimulationEngineFactory()
    {
      mCapabilities = SimulationEngineCapability::ImportStructure2D;
    }

    virtual ReosHydraulicSimulation *createSimulation( QObject *parent ) const override
    { return nullptr; }
    virtual ReosHydraulicSimulation *createSimulation( const ReosEncodedElement &element, QObject *parent ) const override
    { return nullptr; }

    virtual QString key() const override { return ReosHecRasSimulation::staticKey(); }
    QString displayName() const override { return QObject::tr( "HEC-RAS Simulation" ); }

    void initializeSettings() override;
};

class ReosHecRasStructureImporter: public ReosStructureImporter
{
  public:
    ReosHecRasStructureImporter( const QString &file );
    ~ReosHecRasStructureImporter();

    ReosHydraulicStructure2D::Structure2DCapabilities capabilities() const override;
    QString crs() const override;
    QPolygonF domain() const override;
    ReosMeshGenerator *meshGenerator() const  override { return nullptr; };
    ReosMeshResolutionController *resolutionController( ReosHydraulicStructure2D *structure ) const override;
    ReosMesh *mesh() const override { return nullptr; };
    ReosRoughnessStructure *roughnessStructure() const override { return nullptr; };

    bool isValid() const override { return mIsValid; }

  private:
    bool mIsValid = false;
    std::unique_ptr<ReosHecRasProject> mProject;
};


#endif //REOSHECRASSIMULATION_H
