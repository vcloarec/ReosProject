/***************************************************************************
  reoshydraulicsimulation.cpp - ReosHydraulicSimulation

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
#include "reoshydraulicsimulation.h"

#include <QProcess>

#include <qgsmeshdataset.h>
#include <qgsmeshlayer.h>
#include <qgsproviderregistry.h>
#include <qgsmeshtriangulation.h>

#include "reoshydraulicstructure2d.h"
#include "reoshydraulicstructureboundarycondition.h"
#include "reosparameter.h"
#include "reoscalculationcontext.h"
#include "reoshydrograph.h"
#include "reossimulationinitialcondition.h"

ReosHydraulicSimulation::ReosHydraulicSimulation( QObject *parent ): ReosDataObject( parent )
{
}

QDir ReosHydraulicSimulation::simulationDir( ReosHydraulicStructure2D *hydraulicStructure, const ReosCalculationContext &context ) const
{
  QDir dir = hydraulicStructure->structureDirectory();
  if ( !dir.cd( context.schemeId() ) )
  {
    dir.mkdir( context.schemeId() );
    if ( !dir.cd( context.schemeId() ) )
      return QDir();
  }

  if ( !dir.cd( mDirName ) )
  {
    dir.mkdir( mDirName );
    if ( !dir.cd( mDirName ) )
      return QDir();
  }

  return dir;
}

ReosSimulationEngineRegistery *ReosSimulationEngineRegistery::sInstance = nullptr;

ReosSimulationEngineRegistery::ReosSimulationEngineRegistery()
{

}

void ReosSimulationEngineRegistery::registerEngineFactory( ReosSimulationEngineFactory *factory )
{
  mFactories[factory->key()] = std::unique_ptr<ReosSimulationEngineFactory>( factory );
}

ReosHydraulicSimulation *ReosSimulationEngineRegistery::createSimulation( const QString &key, QObject *parent ) const
{
  auto it = mFactories.find( key );
  if ( it != mFactories.end() )
    return it->second->createSimulation( parent );
  return nullptr;
}

ReosHydraulicSimulation *ReosSimulationEngineRegistery::createSimulation( const ReosEncodedElement &element, QObject *parent ) const
{
  if ( !element.hasEncodedData( QStringLiteral( "key" ) ) )
    return nullptr;

  QString key;
  element.getData( QStringLiteral( "key" ), key );

  auto it = mFactories.find( key );
  if ( it != mFactories.end() )
    return it->second->createSimulation( element, parent );
  return nullptr;
}

ReosSimulationEngineRegistery *ReosSimulationEngineRegistery::instance()
{
  if ( !sInstance )
  {
    sInstance = new ReosSimulationEngineRegistery();
    sInstance->loadDynamicLibrary();
  }

  return sInstance;
}

const QMap<QString, QString> ReosSimulationEngineRegistery::availableEngine()
{
  QMap<QString, QString> ret;
  for ( auto &it : std::as_const( mFactories ) )
    ret.insert( it.first, it.second->displayName() );

  return ret;
}

void ReosSimulationEngineRegistery::loadDynamicLibrary()
{
  QString enginesPath = QCoreApplication::applicationDirPath();
  QDir enginesDir( enginesPath );
  if ( enginesDir.cd( QStringLiteral( REOS_SIMULATION_ENGINES ) ) )
    enginesPath = enginesDir.absolutePath();
  else
  {
    enginesPath = REOS_SIMULATION_ENGINES;
    enginesDir = QDir( enginesPath );
  }

  enginesDir.setSorting( QDir::Name | QDir::IgnoreCase );
  enginesDir.setFilter( QDir::Files | QDir::NoSymLinks );

#if defined(Q_OS_WIN) || defined(__CYGWIN__)
  enginesDir.setNameFilters( QStringList( "*.dll" ) );
#else
  enginesDir.setNameFilters( QStringList( QStringLiteral( "*.so" ) ) );
#endif

  typedef ReosSimulationEngineFactory *factory_function( );

  const QFileInfoList files = enginesDir.entryInfoList();
  for ( const QFileInfo &file : files )
  {
    QLibrary library( file.filePath() );
    if ( library.load() )
    {
      QFunctionPointer fcp = library.resolve( "engineSimulationFactory" );
      factory_function *func = reinterpret_cast<factory_function *>( fcp );

      if ( func )
      {
        ReosSimulationEngineFactory *engineSimulationFactory = func();
        registerEngineFactory( engineSimulationFactory );
      }
    }
  }
}


ReosSimulationPreparationProcess::ReosSimulationPreparationProcess( ReosHydraulicStructure2D *hydraulicStructure, ReosHydraulicSimulation *simulation, const ReosCalculationContext &context )
  : mStructure( hydraulicStructure )
  , mSimulation( simulation )
  , mContext( context )
{}

void ReosSimulationPreparationProcess::start()
{
  emit sendInformation( tr( "Get boundary counditions", nullptr, mWaitedBoundaryId.count() ) );

  if ( mStructure.isNull() || mSimulation.isNull() )
    return;
  QList<ReosHydraulicStructureBoundaryCondition *> boundaries = mStructure->boundaryConditions();
  mBoundaryCount = boundaries.count();
  setMaxProgression( mBoundaryCount );
  setCurrentProgression( 0 );
  for ( ReosHydraulicStructureBoundaryCondition *bc : boundaries )
  {
    if ( bc )
    {
      switch ( bc->conditionType() )
      {
        case ReosHydraulicStructureBoundaryCondition::Type::InputFlow:
          mWaitedBoundaryId.append( bc->id() );
          connect( bc, &ReosHydraulicNetworkElement::calculationIsUpdated, this, &ReosSimulationPreparationProcess::onBoundaryUpdated );
          if ( !bc->updateCalculationContextFromDownstream( mContext ) )
          {
            mWaitedBoundaryId.removeOne( bc->id() );
            disconnect( bc, &ReosHydraulicNetworkElement::calculationIsUpdated, this, &ReosSimulationPreparationProcess::onBoundaryUpdated );
          }
          break;
        case ReosHydraulicStructureBoundaryCondition::Type::OutputLevel:
          bc->updateCalculationContextFromUpstream( mContext, nullptr, true );
          break;
      }
    }
  }

  QEventLoop *eventLoop = new QEventLoop;
  connect( this, &ReosSimulationPreparationProcess::allBoundariesUpdated, eventLoop, &QEventLoop::quit );
  emit sendInformation( tr( "Wait for %n boundary condition", nullptr, mWaitedBoundaryId.count() ) );
  setCurrentProgression( mBoundaryCount - mWaitedBoundaryId.count() );

  if ( !mWaitedBoundaryId.isEmpty() )
    eventLoop->exec();

  eventLoop->deleteLater();

  mSimulation->prepareInput( mStructure, mContext );

}

void ReosSimulationPreparationProcess::onBoundaryUpdated( const QString &id )
{
  if ( mWaitedBoundaryId.contains( id ) )
  {
    mWaitedBoundaryId.removeOne( id );
    emit sendInformation( tr( "Wait for %n boundary condition", nullptr, mWaitedBoundaryId.count() ) );
    setCurrentProgression( mBoundaryCount - mWaitedBoundaryId.count() );

    if ( mWaitedBoundaryId.isEmpty() )
      emit allBoundariesUpdated();
  }
}
