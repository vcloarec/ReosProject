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

#include <QDateTime>

#include "reosduration.h"
#include "reostelemac2dsimulation.h"

ReosTelemac2DSimulationResults::ReosTelemac2DSimulationResults( const ReosTelemac2DSimulation *simulation, const QString &fileName, QObject *parent )
  : ReosHydraulicSimulationResults( simulation, parent )
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
  DatasetType dt = datasetType( groupIndex );

  MDAL_DatasetGroupH group = MDAL_M_datasetGroup( mMeshH, mTypeToTelemacGroupIndex.value( dt ) );

  if ( group )
    return MDAL_G_datasetCount( group );
  else
    return 0;
}

ReosHydraulicSimulationResults::DatasetType ReosTelemac2DSimulationResults::datasetType( int groupIndex ) const
{
  return mTypeToTelemacGroupIndex.keys().at( groupIndex );
}

void ReosTelemac2DSimulationResults::groupMinMax( int groupIndex, double &minimum, double &maximum ) const
{
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
  DatasetType dt = datasetType( groupIndex );
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

  return ret;
}
