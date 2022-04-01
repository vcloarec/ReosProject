/***************************************************************************
  reoshydraulicsimulation.h - ReosHydraulicSimulation

 ---------------------
 begin                : 19.3.2022
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
#ifndef REOSHYDRAULICSIMULATION_H
#define REOSHYDRAULICSIMULATION_H

#include "reosdataobject.h"

#include "reosprocess.h"

class QProcess;
class QFile;
class QTextStream;

class ReosHydraulicStructure2D;
class ReosParameterDateTime;
class ReosParameterDuration;
class ReosParameterInteger;
class ReosCalculationContext;
class ReosHydraulicStructureBoundaryCondition;
class ReosSimulationInitialConditions;

class ReosSimulationProcess : public ReosProcess
{
    Q_OBJECT
  public:
    ReosSimulationProcess() = default;

};

class ReosHydraulicSimulation : public ReosDataObject
{
    Q_OBJECT
  public:
    ReosHydraulicSimulation( QObject *parent = nullptr );

    virtual QString directoryName() const = 0;

    virtual void prepareInput( ReosHydraulicStructure2D *hydraulicStructure, const ReosCalculationContext &context ) = 0;

    void launch( ReosHydraulicStructure2D *hydraulicStructure );
    virtual ReosSimulationProcess *getProcess( ReosHydraulicStructure2D *hydraulicStructure, const ReosCalculationContext &calculationContext ) const = 0;

    virtual QString key() const = 0;
    virtual ReosEncodedElement encode() const = 0;

  protected:

    QString mDirName = QStringLiteral( "TELEMAC_simulation" );
    QString mGeomFileName = QStringLiteral( "geom_input.slf" );
    QString mResultFileName = QStringLiteral( "result.slf" );
    QString mBoundaryFileName = QStringLiteral( "boundary.bc" );
    QString mBoundaryConditionFileName = QStringLiteral( "boundaryCondition.sql" );
    QString mSteeringFileName = QStringLiteral( "simulation.cas" );
};


class ReosSimulationEngineFactory
{
  public:
    virtual ReosHydraulicSimulation *createSimulation( QObject *parent ) const = 0;
    virtual ReosHydraulicSimulation *createSimulation( const ReosEncodedElement &element, QObject *parent ) const = 0;
    virtual QString key() const  = 0;
    virtual QString displayName() const = 0;
};

class ReosSimulationEngineRegistery
{
  public:
    ReosSimulationEngineRegistery();

    //! Creates and returns a simuation corresponding to the \a key
    ReosHydraulicSimulation *createSimulation( const QString &key, QObject *parent ) const ;

    //! Creates and returns a simuation corresponding to the encoded \a element
    ReosHydraulicSimulation *createSimulation( const ReosEncodedElement &element, QObject *parent )const;

    //! Returns a pointer to the static instance of this registery
    static ReosSimulationEngineRegistery *instance();

    const QMap<QString, QString> availableEngine();

  private:
#ifdef _MSC_VER
    std::unique_ptr<ReosSimulationEngineFactory> dummy; // workaround for MSVC, if not, the line after create an compilation error if this class is exported (REOSCORE_EXPORT)
#endif
    //! Registers a \a factory
    void registerEngineFactory( ReosSimulationEngineFactory *factory );

    std::map<QString, std::unique_ptr<ReosSimulationEngineFactory>> mFactories;
    static ReosSimulationEngineRegistery *sInstance;
    void loadDynamicLibrary();
};


#endif // REOSHYDRAULICSIMULATION_H
