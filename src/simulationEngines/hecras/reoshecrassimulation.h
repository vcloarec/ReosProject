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

class ReosHecRasSimulation : public ReosHydraulicSimulation
{
    Q_OBJECT
  public:
    ReosHecRasSimulation( QObject *parent );


    static QString staticKey() { return QStringLiteral( "hecras" ); }

    QString key() const override;
    ReosEncodedElement encode() const {return ReosEncodedElement();}
    QString directoryName() const {return QString();}
    void prepareInput( ReosHydraulicStructure2D *hydraulicStructure, const ReosCalculationContext &calculationContext ) {}
    void prepareInput( ReosHydraulicStructure2D *hydraulicStructure, const ReosCalculationContext &calculationContext, const QDir &directory ) {}
    ReosSimulationProcess *getProcess( ReosHydraulicStructure2D *hydraulicStructure, const ReosCalculationContext &calculationContext ) const {return nullptr;}
    QList<QDateTime> theoricalTimeSteps( ReosHydraulicScheme *scheme ) const {return QList<QDateTime>();}
    ReosDuration representativeTimeStep() const {return ReosDuration();}
    ReosDuration representative2DTimeStep() const {return ReosDuration();}
    void saveSimulationResult( const ReosHydraulicStructure2D *hydraulicStructure, const QString &shemeId, ReosSimulationProcess *process, bool success ) const {}
    ReosHydraulicSimulationResults *loadSimulationResults( ReosHydraulicStructure2D *hydraulicStructure, const QString &shemeId, QObject *parent ) const {return nullptr;}
    bool hasResult( const ReosHydraulicStructure2D *hydraulicStructure, const QString &shemeId ) const {return false;}
    void removeResults( const ReosHydraulicStructure2D *hydraulicStructure, const QString &shemeId ) const {}
    QString engineName() const {return tr( "HECRAS" );}
    void saveConfiguration( ReosHydraulicScheme *scheme ) const {}
    void restoreConfiguration( ReosHydraulicScheme *scheme ) {}

    void setProject( std::shared_ptr<ReosHecRasProject> newProject );
    ReosHecRasProject *project() const;

  private:
    std::shared_ptr<ReosHecRasProject> mProject;
    QString mCurrentPlan;
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
    QStringList boundaryConditionsIds() const override;
    QStringList boundaryConditionsNames() const override;
    QList<QPointF> boundaryConditionMiddlePoint() const override;

    QList<ReosHydraulicSimulation *> createSimulations( QObject *parent ) const override;

    bool isValid() const override { return mIsValid; }

  private:
    bool mIsValid = false;
    std::shared_ptr<ReosHecRasProject> mProject;
};


#endif //REOSHECRASSIMULATION_H
