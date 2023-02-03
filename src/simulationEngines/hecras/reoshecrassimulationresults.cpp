/***************************************************************************
  reoshecrassimulationresults.cpp - ReosHecRasSimulationResults

 ---------------------
 begin                : 6.11.2022
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
#include "reoshecrassimulationresults.h"

#include <QDateTime>

#include "reosduration.h"
#include "reosdssfile.h"
#include "reosdssutils.h"
#include "reoshecrassimulation.h"
#include "reoshydrograph.h"


ReosHecRasSimulationResults::ReosHecRasSimulationResults( const ReosHecRasSimulation *simulation, QObject *parent = nullptr )
  : ReosHydraulicSimulationResults( simulation, parent )
  , mProject( *simulation->project() )
  , mPlanId( simulation->currentPlan() )
{
  const ReosHecRasPlan &plan = mProject.plan( mPlanId );
  QString fileName = mProject.directory().filePath( plan.fileName() + QStringLiteral( ".hdf" ) );

  QByteArray curi = fileName.toUtf8();
  mMeshH = MDAL_LoadMesh( curi.constData() );

  if ( !mMeshH )
    return;

  int groupCount = MDAL_M_datasetGroupCount( mMeshH );

  for ( int i = 0; i < groupCount; ++i )
  {
    MDAL_DatasetGroupH group = MDAL_M_datasetGroup( mMeshH, i );
    QString groupName( MDAL_G_name( group ) );
    if ( groupName == QStringLiteral( "Velocity" ) )
      mTypeToSourceGroupIndex[DatasetType::Velocity] = i;
    else if ( groupName == QStringLiteral( "Water Surface" ) )
      mTypeToSourceGroupIndex[DatasetType::WaterLevel] = i;
  }

  registerGroups( mTypeToSourceGroupIndex.keys() );

  mCache.resize( datasetCount( groupIndex( DatasetType::WaterLevel ) ) );
}

ReosHecRasSimulationResults::~ReosHecRasSimulationResults()
{
  if ( mMeshH )
    MDAL_CloseMesh( mMeshH );
}

int ReosHecRasSimulationResults::datasetCount( int groupIndex ) const
{
  if ( groupIndex < 0 || groupIndex >= groupCount() )
    return 0;

  MDAL_DatasetGroupH group = MDAL_M_datasetGroup( mMeshH, groupIndexToSourceIndex( groupIndex ) );

  if ( group )
    return MDAL_G_datasetCount( group );
  else
    return 0;
}

void ReosHecRasSimulationResults::groupMinMax( int groupIndex, double &minimum, double &maximum ) const
{
  if ( groupIndex < 0 || groupIndex >= groupCount() )
    return;

  minimum = std::numeric_limits<double>::quiet_NaN();
  maximum = std::numeric_limits<double>::quiet_NaN();

  MDAL_DatasetGroupH group = MDAL_M_datasetGroup( mMeshH,  groupIndexToSourceIndex( groupIndex ) );

  if ( !group )
    return;

  MDAL_G_minimumMaximum( group, &minimum, &maximum );
}

QDateTime ReosHecRasSimulationResults::groupReferenceTime( int groupIndex ) const
{
  if ( groupIndex < 0 || groupIndex >= groupCount() )
    return QDateTime();

  if ( mReferenceTime.isValid() )
    return mReferenceTime;

  MDAL_DatasetGroupH group = MDAL_M_datasetGroup( mMeshH,  groupIndexToSourceIndex( groupIndex ) );

  if ( !group )
    return QDateTime();

  QString referenceTimeString( MDAL_G_referenceTime( group ) );
  if ( !referenceTimeString.isEmpty() )
    referenceTimeString.append( 'Z' );//For now provider doesn't support time zone and return always in local time, force UTC

  mReferenceTime = QDateTime::fromString( referenceTimeString, Qt::ISODate );
  return mReferenceTime;
}

ReosDuration ReosHecRasSimulationResults::datasetRelativeTime( int groupIndex, int datasetIndex ) const
{
  if ( groupIndex < 0 || groupIndex >= groupCount() )
    return ReosDuration();

  if ( mTimeToTimeStep.isEmpty() )
    populateTimeStep();

  return mTimeSteps.at( datasetIndex );
}

ReosMeshDatasetSource::Location ReosHecRasSimulationResults::groupLocation( int ) const
{
  return ReosMeshDatasetSource::Location::Face;
}

QVector<double> ReosHecRasSimulationResults::datasetValues( int groupIndex, int index ) const
{
  if ( groupIndex < 0 || groupIndex >= groupCount() )
    return QVector<double>();

  MDAL_DatasetGroupH group = MDAL_M_datasetGroup( mMeshH,  groupIndexToSourceIndex( groupIndex ) );

  if ( !group )
    return QVector<double>();

  if ( index >= MDAL_G_datasetCount( group ) )
    return QVector<double>();

  MDAL_DatasetH dataset = MDAL_G_dataset( group, index );
  bool isScalar = MDAL_G_hasScalarData( group );
  int valueCount = MDAL_D_valueCount( dataset );

  QVector<double> ret;
  ret.resize( valueCount * ( isScalar ? 1 : 2 ) );

  int effectiveValueCount = MDAL_D_data( dataset,
                                         0,
                                         valueCount,
                                         isScalar ? MDAL_DataType::SCALAR_DOUBLE : MDAL_DataType::VECTOR_2D_DOUBLE,
                                         ret.data() );

  Q_ASSERT( valueCount == effectiveValueCount );

  return ret;

}

QVector<int> ReosHecRasSimulationResults::activeFaces( int index ) const
{
  if ( index < 0 )
    return QVector<int>();

  QVector<int> &activeFaces = mCache[index].activeFaces;

  if ( activeFaces.isEmpty() )
  {
    const QVector<double> waterLevel = datasetValues( groupIndex( DatasetType::WaterLevel ), index );
    int count = waterLevel.size() ;
    activeFaces.resize( count );
    for ( int i = 0 ; i < count; ++i )
    {
      activeFaces[i] = std::isnan( waterLevel.at( i ) ) ? 0 : 1;
    }
  }

  return mCache.at( index ).activeFaces;
}

int ReosHecRasSimulationResults::datasetIndexClosestBeforeTime( int groupIndex, const QDateTime &time ) const
{
  ReosDuration relativeTime( groupReferenceTime( groupIndex ).msecsTo( time ) );

  if ( mTimeToTimeStep.isEmpty() )
    populateTimeStep();

  auto it = mTimeToTimeStep.upperBound( relativeTime );

  if ( it == mTimeToTimeStep.begin() )
    return -1;

  it--;

  return it.value();
}

int ReosHecRasSimulationResults::datasetValuesCount( int groupIndex, int datasetIndex ) const
{
  MDAL_DatasetGroupH group = MDAL_M_datasetGroup( mMeshH,  groupIndexToSourceIndex( groupIndex ) );
  MDAL_DatasetH ds = MDAL_G_dataset( group, datasetIndex );

  return MDAL_D_valueCount( ds );
}

void ReosHecRasSimulationResults::datasetMinMax( int groupIndex, int datasetIndex, double &min, double &max ) const
{
  if ( groupIndex < 0 || groupIndex >= groupCount() )
    return;

  min = std::numeric_limits<double>::quiet_NaN();
  max = std::numeric_limits<double>::quiet_NaN();

  MDAL_DatasetGroupH group = MDAL_M_datasetGroup( mMeshH,  groupIndexToSourceIndex( groupIndex ) );

  if ( !group )
    return;

  MDAL_DatasetH dataset = MDAL_G_dataset( group, datasetIndex );
  if ( !dataset )
    return;

  MDAL_D_minimumMaximum( dataset, &min, &max );
}

bool ReosHecRasSimulationResults::datasetIsValid( int groupIndex, int datasetIndex ) const
{
  if ( groupIndex < 0 || groupIndex >= groupCount() )
    return false;

  MDAL_DatasetGroupH group = MDAL_M_datasetGroup( mMeshH,  groupIndexToSourceIndex( groupIndex ) );

  if ( !group )
    return false;

  MDAL_DatasetH dataset = MDAL_G_dataset( group, datasetIndex );
  if ( !dataset )
    return false;

  return MDAL_D_isValid( dataset );
}

QDateTime ReosHecRasSimulationResults::runDateTime() const
{
  const ReosHecRasPlan &plan = mProject.plan( mPlanId );
  QString fileName = mProject.directory().filePath( plan.fileName() + QStringLiteral( ".hdf" ) );
  QFileInfo fileInfo( fileName );
  return fileInfo.lastModified();
}

int ReosHecRasSimulationResults::groupIndexToSourceIndex( int groupIndex ) const
{
  DatasetType dt = datasetType( groupIndex );
  return mTypeToSourceGroupIndex.value( dt );
}

void ReosHecRasSimulationResults::populateTimeStep() const
{
  DatasetType dt = DatasetType::WaterLevel;
  int sourceIndex = mTypeToSourceGroupIndex.value( dt );

  MDAL_DatasetGroupH group = MDAL_M_datasetGroup( mMeshH, sourceIndex );

  if ( !group )
    return;

  int dsCount = mCache.count();
  mTimeSteps.resize( dsCount );
  for ( int i = 0; i < dsCount; ++i )
  {
    MDAL_DatasetH dataset = MDAL_G_dataset( group, i );
    if ( !dataset )
      continue;
    ReosDuration relativeTime = ReosDuration( MDAL_D_time( dataset ), ReosDuration::hour );
    mTimeToTimeStep.insert( relativeTime, i );
    mTimeSteps[i] = relativeTime;
  }
}

int ReosHecRasSimulationResults::datasetIndex( int, const QDateTime &time ) const
{
  for ( int i = 0; i < mTimeSteps.count(); ++i )
  {
    if ( time < mReferenceTime.addMSecs( mTimeSteps.at( i ).valueMilliSecond() ) )
      return i - 1;
  }

  return mTimeSteps.count() - 1;
}

QMap<QString, ReosHydrograph *> ReosHecRasSimulationResults::outputHydrographs() const
{
  const QString dssFile = mProject.dssResultFile( mPlanId );
  const ReosHecRasFlow &flow = mProject.flowFromPlan( mPlanId );
  const ReosHecRasPlan &plan = mProject.plan( mPlanId );

  QMap<QString, ReosHydrograph *> ret;

  ReosDssPath path;
  path.setGroup( QStringLiteral( "BCLINE" ) );
  path.setVersion( plan.shortIdentifier() );
  path.setParameter( QStringLiteral( "FLOW" ) );
  path.setTimeInterval( plan.outputInterval() );

  for ( int i = 0; i < flow.boundariesCount(); ++i )
  {
    const ReosHecRasFlow::BoundaryFlow &hbc = flow.boundary( i );
    const QString location = hbc.area() + QStringLiteral( ": " ) + hbc.boundaryConditionLine();
    path.setLocation( location );

    ReosHydrograph *hyd = new ReosHydrograph( const_cast<ReosHecRasSimulationResults *>( this ),
        QStringLiteral( "dss" ),
        ReosDssUtils::uri( dssFile, path, ReosTimeWindow( plan.startTime(), plan.endTime() ) ) );

    ret.insert( hbc.id(), hyd );

  }
  return ret;
}
