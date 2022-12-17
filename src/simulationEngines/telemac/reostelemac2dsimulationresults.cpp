/***************************************************************************
  reostelemac2dsimulationresults.cpp - ReosTelemac2DSimulationResults

 ---------------------
 begin                : 1.4.2022
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
#include "reostelemac2dsimulationresults.h"

#include <qgsmeshlayer.h>

#include <QDateTime>
#include <QFileInfo>
#include <QDir>

#include "reosduration.h"
#include "reostelemac2dsimulation.h"
#include "reosmesh.h"

ReosTelemac2DSimulationResults::ReosTelemac2DSimulationResults( const ReosTelemac2DSimulation *simulation, const ReosMesh *mesh, const QString &fileName, QObject *parent )
  : ReosHydraulicSimulationResults( simulation, parent )
  , mFileName( fileName )
{
  QByteArray curi = fileName.toUtf8();
  mMeshH = MDAL_LoadMesh( curi.constData() );

  if ( !mMeshH )
    return;

  int groupCount = MDAL_M_datasetGroupCount( mMeshH );

  for ( int i = 0; i < groupCount; ++i )
  {
    MDAL_DatasetGroupH group = MDAL_M_datasetGroup( mMeshH, i );
    QString groupName( MDAL_G_name( group ) );
    if ( groupName == QStringLiteral( "velocity      ms" ) )
      mTypeToTelemacGroupIndex[DatasetType::Velocity] = i;
    else if ( groupName == QStringLiteral( "water depth     m" ) )
      mTypeToTelemacGroupIndex[DatasetType::WaterDepth] = i;
    else if ( groupName == QStringLiteral( "free surface    m" ) )
      mTypeToTelemacGroupIndex[DatasetType::WaterLevel] = i;
  }

  mCache.resize( datasetCount( mTypeToTelemacGroupIndex.value( DatasetType::WaterDepth ) ) );

  QgsMeshLayer *meshLayer = qobject_cast<QgsMeshLayer *>( mesh->data() );
  if ( meshLayer )
  {
    mFaces = meshLayer->nativeMesh()->faces;
    const QVector<QgsMeshVertex> &vertices = meshLayer->nativeMesh()->vertices;
    mBottomValues.resize( vertices.size() );
    for ( int i = 0; i < vertices.size(); ++i )
      mBottomValues[i] = vertices.at( i ).z();
  }

  QFileInfo fileInfo( fileName );
  QFile outputHydFile( fileInfo.dir().filePath( QStringLiteral( "outputHydrographs" ) ) );
  if ( outputHydFile.open( QIODevice::ReadOnly ) )
  {
    QMap<QString, QByteArray> encodedHydrographs;
    QDataStream stream( &outputHydFile );
    stream >> encodedHydrographs;

    const QStringList keys = encodedHydrographs.keys();
    for ( const QString &key : keys )
    {
      ReosEncodedElement encodedHyd( encodedHydrographs.value( key ) );
      ReosEncodeContext encodeContext;
      encodeContext.setBaseDir( fileInfo.dir() );
      if ( encodedHyd.description() == QStringLiteral( "hydrograph" ) )
        mOutputHydrographs.insert( key, ReosHydrograph::decode( encodedHyd, encodeContext, this ) );
    }
  }
}

ReosTelemac2DSimulationResults::~ReosTelemac2DSimulationResults()
{
  if ( mMeshH )
    MDAL_CloseMesh( mMeshH );
}

int ReosTelemac2DSimulationResults::groupCount() const
{
  return mTypeToTelemacGroupIndex.count();
}

int ReosTelemac2DSimulationResults::datasetCount( int groupIndex ) const
{
  if ( groupIndex < 0 || groupIndex >= groupCount() )
    return 0;

  MDAL_DatasetGroupH group = MDAL_M_datasetGroup( mMeshH, groupIndexToTelemacIndex( groupIndex ) );

  if ( group )
    return MDAL_G_datasetCount( group );
  else
    return 0;
}

ReosHydraulicSimulationResults::DatasetType ReosTelemac2DSimulationResults::datasetType( int groupIndex ) const
{
  if ( groupIndex < 0 || groupIndex >= groupCount() )
    return DatasetType::WaterLevel;

  return mTypeToTelemacGroupIndex.keys().at( groupIndex );
}

void ReosTelemac2DSimulationResults::groupMinMax( int groupIndex, double &minimum, double &maximum ) const
{
  if ( groupIndex < 0 || groupIndex >= groupCount() )
    return;

  minimum = std::numeric_limits<double>::quiet_NaN();
  maximum = std::numeric_limits<double>::quiet_NaN();

  MDAL_DatasetGroupH group = MDAL_M_datasetGroup( mMeshH,  groupIndexToTelemacIndex( groupIndex ) );

  if ( !group )
    return;

  MDAL_G_minimumMaximum( group, &minimum, &maximum );
}

QDateTime ReosTelemac2DSimulationResults::groupReferenceTime( int groupIndex ) const
{
  if ( groupIndex < 0 || groupIndex >= groupCount() )
    return QDateTime();

  if ( mReferenceTime.isValid() )
    return mReferenceTime;

  MDAL_DatasetGroupH group = MDAL_M_datasetGroup( mMeshH,  groupIndexToTelemacIndex( groupIndex ) );

  if ( !group )
    return QDateTime();

  QString referenceTimeString( MDAL_G_referenceTime( group ) );
  if ( !referenceTimeString.isEmpty() )
    referenceTimeString.append( 'Z' );//For now provider doesn't support time zone and return always in local time, force UTC

  mReferenceTime = QDateTime::fromString( referenceTimeString, Qt::ISODate );
  return mReferenceTime;
}

ReosDuration ReosTelemac2DSimulationResults::datasetRelativeTime( int groupIndex, int datasetIndex ) const
{
  if ( groupIndex < 0 || groupIndex >= groupCount() )
    return ReosDuration();

  if ( mTimeToTimeStep.isEmpty() )
    populateTimeStep();

  return mTimeSteps.at( datasetIndex );
}

bool ReosTelemac2DSimulationResults::datasetIsValid( int groupIndex, int datasetIndex ) const
{
  if ( groupIndex < 0 || groupIndex >= groupCount() )
    return false;

  MDAL_DatasetGroupH group = MDAL_M_datasetGroup( mMeshH,  groupIndexToTelemacIndex( groupIndex ) );

  if ( !group )
    return false;

  MDAL_DatasetH dataset = MDAL_G_dataset( group, datasetIndex );
  if ( !dataset )
    return false;

  return MDAL_D_isValid( dataset );
}

void ReosTelemac2DSimulationResults::datasetMinMax( int groupIndex, int datasetIndex, double &min, double &max ) const
{
  if ( groupIndex < 0 || groupIndex >= groupCount() )
    return;

  min = std::numeric_limits<double>::quiet_NaN();
  max = std::numeric_limits<double>::quiet_NaN();

  MDAL_DatasetGroupH group = MDAL_M_datasetGroup( mMeshH,  groupIndexToTelemacIndex( groupIndex ) );

  if ( !group )
    return;

  MDAL_DatasetH dataset = MDAL_G_dataset( group, datasetIndex );
  if ( !dataset )
    return;

  MDAL_D_minimumMaximum( dataset, &min, &max );
}

int ReosTelemac2DSimulationResults::datasetValuesCount( int groupIndex, int datasetIndex ) const
{
  MDAL_DatasetGroupH group = MDAL_M_datasetGroup( mMeshH,  groupIndexToTelemacIndex( groupIndex ) );
  MDAL_DatasetH ds = MDAL_G_dataset( group, datasetIndex );

  return MDAL_D_valueCount( ds );
}

int ReosTelemac2DSimulationResults::datasetIndex( int, const QDateTime &time ) const
{
  for ( int i = 0; i < mTimeSteps.count(); ++i )
  {
    if ( time < mReferenceTime.addMSecs( mTimeSteps.at( i ).valueMilliSecond() ) )
      return i - 1;
  }

  return mTimeSteps.count() - 1;
}

QVector<double> ReosTelemac2DSimulationResults::datasetValues( int groupIndex, int index ) const
{
  if ( groupIndex < 0 || groupIndex >= groupCount() )
    return QVector<double>();

  DatasetType dt = datasetType( groupIndex );

  switch ( dt )
  {
    case ReosHydraulicSimulationResults::DatasetType::WaterLevel:
      if ( !mCache.at( index ).waterLevel.isEmpty() )
        return mCache.at( index ).waterLevel;
      break;
    case ReosHydraulicSimulationResults::DatasetType::WaterDepth:
      if ( !mCache.at( index ).waterDepth.isEmpty() )
        return mCache.at( index ).waterDepth;
      break;
    case ReosHydraulicSimulationResults::DatasetType::Velocity:
      if ( !mCache.at( index ).velocity.isEmpty() )
        return mCache.at( index ).velocity;
      break;
    case ReosHydraulicSimulationResults::DatasetType::None:
      return QVector<double>();
      break;
  }

  MDAL_DatasetGroupH group = MDAL_M_datasetGroup( mMeshH,  groupIndexToTelemacIndex( groupIndex ) );

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

  switch ( dt )
  {
    case ReosHydraulicSimulationResults::DatasetType::WaterLevel:
      mCache[index].waterLevel = ret;
      dryVertices( index );
      adaptWaterLevel( mCache[index].waterLevel, index );
      return mCache[index].waterLevel;
      break;
    case ReosHydraulicSimulationResults::DatasetType::WaterDepth:
      adaptWaterDepth( ret, index );
      mCache[index].waterDepth = ret;
      break;
    case ReosHydraulicSimulationResults::DatasetType::Velocity:
      adaptVelocity( ret, index );
      mCache[index].velocity = ret;
      break;
    case ReosHydraulicSimulationResults::DatasetType::None:
      return QVector<double>();
      break;
  }

  return ret;
}

QVector<int> ReosTelemac2DSimulationResults::activeFaces( int index ) const
{
  if ( index >= mCache.count() )
    return QVector<int>();

  if ( mCache.at( index ).activeFaces.isEmpty() )
  {
    dryVertices( index );
  }

  return mCache.at( index ).activeFaces;
}

QDateTime ReosTelemac2DSimulationResults::runDateTime() const
{
  const QFileInfo fileInfo( mFileName );
  return fileInfo.lastModified();
}

QMap<QString, ReosHydrograph *> ReosTelemac2DSimulationResults::outputHydrographs() const
{
  return mOutputHydrographs;
}

int ReosTelemac2DSimulationResults::datasetIndexClosestBeforeTime( int groupIndex, const QDateTime &time ) const
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

QString ReosTelemac2DSimulationResults::unitString( ReosHydraulicSimulationResults::DatasetType dataType ) const
{
  switch ( dataType )
  {
    case ReosHydraulicSimulationResults::DatasetType::None:
      return QString();
      break;
    case ReosHydraulicSimulationResults::DatasetType::WaterLevel:
      return tr( "m" );
      break;
    case ReosHydraulicSimulationResults::DatasetType::WaterDepth:
      return tr( "m" );
      break;
    case ReosHydraulicSimulationResults::DatasetType::Velocity:
      return tr( " m/s" );
      break;
  }

  return QString();
}

int ReosTelemac2DSimulationResults::groupIndexToTelemacIndex( int groupIndex ) const
{
  DatasetType dt = datasetType( groupIndex );
  return mTypeToTelemacGroupIndex.value( dt );
}

void ReosTelemac2DSimulationResults::populateTimeStep() const
{
  DatasetType dt = DatasetType::WaterLevel;
  int telemacIndex = mTypeToTelemacGroupIndex.value( dt );

  MDAL_DatasetGroupH group = MDAL_M_datasetGroup( mMeshH, telemacIndex );

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

void ReosTelemac2DSimulationResults::adaptWaterLevel( QVector<double> &waterLevel, int datasetIndex ) const
{
  Q_ASSERT( waterLevel.size() == mBottomValues.size() );

  const QVector<int> facesActive = activeFaces( datasetIndex );

  for ( int fi = 0; fi < facesActive.count(); fi++ )
  {
    if ( facesActive.at( fi ) == 0 )
      continue;

    const QVector<int> &face = mFaces.at( fi );
    QSet<int> vertexToAdapt;
    double value = 0;
    int valueCount = 0;
    for ( int i : face )
    {
      double ws = waterLevel.at( i );
      double bottom = mBottomValues.at( i );
      if ( ws - bottom > mDryDepthValue )
      {
        value += ws;
        valueCount++;
      }
      else
      {
        vertexToAdapt.insert( i );
      }
    }

    if ( valueCount > 0 )
    {
      value = value / valueCount;
      for ( int i : vertexToAdapt )
        waterLevel[i] = value;
    }
  }

}

void ReosTelemac2DSimulationResults::adaptWaterDepth( QVector<double> &waterDepth, int datasetIndex ) const
{
  if ( mCache.at( datasetIndex ).activeVertices.isEmpty() )
    dryVertices( datasetIndex );

  const QVector<int> activeVert = mCache.at( datasetIndex ).activeVertices;

  Q_ASSERT( waterDepth.size() == activeVert.size() );

  for ( int i = 0; i < waterDepth.count(); ++i )
  {
    if ( activeVert.at( i ) == 0 )
      waterDepth[i] = std::numeric_limits<double>::quiet_NaN();
  }
}

void ReosTelemac2DSimulationResults::adaptVelocity( QVector<double> &velocity, int datasetIndex ) const
{
  if ( mCache.at( datasetIndex ).activeVertices.isEmpty() )
    dryVertices( datasetIndex );

  const QVector<int> activeVert = mCache.at( datasetIndex ).activeVertices;

  Q_ASSERT( velocity.size() == activeVert.size() * 2 );

  for ( int i = 0; i < activeVert.count(); ++i )
  {
    if ( activeVert.at( i ) == 0 )
    {
      velocity[2 * i] = std::numeric_limits<double>::quiet_NaN();
      velocity[2 * i + 1] = std::numeric_limits<double>::quiet_NaN();
    }
  }
}

void ReosTelemac2DSimulationResults::dryVertices( int datasetIndex ) const
{
  if ( mCache.at( datasetIndex ).waterLevel.isEmpty() )
    mCache[datasetIndex].waterLevel = datasetValues( groupIndex( DatasetType::WaterLevel ), datasetIndex );

  const QVector<double> &waterLevel = mCache[datasetIndex].waterLevel;
  QVector<int> &active = mCache[datasetIndex].activeFaces;
  active.resize( mFaces.count() );
  QVector<int> &activeVert = mCache[datasetIndex].activeVertices;
  activeVert = QVector<int>( waterLevel.size(), 0 );

  for ( int i = 0; i < active.count(); ++i )
  {
    const QVector<int> &face = mFaces.at( i );
    active[i] = 0;
    for ( int f : face )
    {
      if ( !std::isnan( waterLevel.at( f ) )  && ( waterLevel.at( f ) - mBottomValues.at( f ) > mDryDepthValue ) )
      {
        active[i] = 1;
        break;
      }
    }

    if ( active[i] == 1 )
    {
      for ( int f : face )
        activeVert[f] = 1;
    }
  }
}

int ReosTelemac2DSimulationResults::groupIndex( ReosHydraulicSimulationResults::DatasetType type ) const
{
  const QList<DatasetType> &types = mTypeToTelemacGroupIndex.keys();
  return  types.indexOf( type );
}
