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
    mFaces = meshLayer->nativeMesh()->faces;
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

  DatasetType dt = datasetType( groupIndex );

  MDAL_DatasetGroupH group = MDAL_M_datasetGroup( mMeshH, mTypeToTelemacGroupIndex.value( dt ) );

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

  DatasetType dt = datasetType( groupIndex );
  int telemacIndex = mTypeToTelemacGroupIndex.value( dt );
  minimum = std::numeric_limits<double>::quiet_NaN();
  maximum = std::numeric_limits<double>::quiet_NaN();

  MDAL_DatasetGroupH group = MDAL_M_datasetGroup( mMeshH, telemacIndex );

  if ( !group )
    return;

  MDAL_G_minimumMaximum( group, &minimum, &maximum );
}

QDateTime ReosTelemac2DSimulationResults::groupReferenceTime( int groupIndex ) const
{
  if ( groupIndex < 0 || groupIndex >= groupCount() )
    return QDateTime();

  DatasetType dt = datasetType( groupIndex );
  int telemacIndex = mTypeToTelemacGroupIndex.value( dt );

  MDAL_DatasetGroupH group = MDAL_M_datasetGroup( mMeshH, telemacIndex );

  if ( !group )
    return QDateTime();

  QString referenceTimeString( MDAL_G_referenceTime( group ) );
  if ( !referenceTimeString.isEmpty() )
    referenceTimeString.append( 'Z' );//For now provider doesn't support time zone and return always in local time, force UTC
  return QDateTime::fromString( referenceTimeString, Qt::ISODate );
}

ReosDuration ReosTelemac2DSimulationResults::datasetRelativeTime( int groupIndex, int datasetIndex ) const
{
  if ( groupIndex < 0 || groupIndex >= groupCount() )
    return ReosDuration();

  DatasetType dt = datasetType( groupIndex );
  int telemacIndex = mTypeToTelemacGroupIndex.value( dt );

  MDAL_DatasetGroupH group = MDAL_M_datasetGroup( mMeshH, telemacIndex );

  if ( !group )
    return ReosDuration();

  MDAL_DatasetH dataset = MDAL_G_dataset( group, datasetIndex );
  if ( !dataset )
    return ReosDuration();

  return ReosDuration( MDAL_D_time( dataset ), ReosDuration::hour );
}

bool ReosTelemac2DSimulationResults::datasetIsValid( int groupIndex, int datasetIndex ) const
{
  if ( groupIndex < 0 || groupIndex >= groupCount() )
    return false;

  DatasetType dt = datasetType( groupIndex );
  int telemacIndex = mTypeToTelemacGroupIndex.value( dt );

  MDAL_DatasetGroupH group = MDAL_M_datasetGroup( mMeshH, telemacIndex );

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

  DatasetType dt = datasetType( groupIndex );
  int telemacIndex = mTypeToTelemacGroupIndex.value( dt );
  min = std::numeric_limits<double>::quiet_NaN();
  max = std::numeric_limits<double>::quiet_NaN();

  MDAL_DatasetGroupH group = MDAL_M_datasetGroup( mMeshH, telemacIndex );

  if ( !group )
    return;

  MDAL_DatasetH dataset = MDAL_G_dataset( group, datasetIndex );
  if ( !dataset )
    return;

  MDAL_D_minimumMaximum( dataset, &min, &max );
}

QVector<double> ReosTelemac2DSimulationResults::datasetValues( int groupIndex, int index ) const
{
  if ( groupIndex < 0 || groupIndex >= groupCount() )
    return QVector<double>();

  DatasetType dt = datasetType( groupIndex );

  switch ( dt )
  {
    case ReosHydraulicSimulationResults::DatasetType::WaterLevel:
      break;
    case ReosHydraulicSimulationResults::DatasetType::WaterDepth:
      if ( !mCache.at( index ).waterDepth.isEmpty() )
        return mCache.at( index ).waterDepth;
      break;
    case ReosHydraulicSimulationResults::DatasetType::Velocity:
      break;
  }

  int telemacIndex = mTypeToTelemacGroupIndex.value( dt );

  MDAL_DatasetGroupH group = MDAL_M_datasetGroup( mMeshH, telemacIndex );

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
      break;
    case ReosHydraulicSimulationResults::DatasetType::WaterDepth:
      mCache[index].waterDepth = ret;
      break;
    case ReosHydraulicSimulationResults::DatasetType::Velocity:
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
    if ( mCache.at( index ).waterDepth.isEmpty() )
    {
      mCache[index].waterDepth = datasetValues( mTypeToTelemacGroupIndex.value( DatasetType::WaterDepth ), index );
    }

    const QVector<double> &waterDepth = mCache.at( index ).waterDepth;
    QVector<int> &active = mCache[index].activeFaces;
    active.resize( mFaces.count() );

    for ( int i = 0; i < active.count(); ++i )
    {
      const QVector<int> &face = mFaces.at( i );
      active[i] = 0;
      for ( int f : face )
      {
        if ( waterDepth.at( f ) > mDryDepthValue )
        {
          active[i] = 1;
          break;
        }
      }
    }
  }

  return mCache.at( index ).activeFaces;
}

QDateTime ReosTelemac2DSimulationResults::runDateTime() const
{
  const QFileInfo fileInfo( mFileName );
  return fileInfo.lastModified();
}
