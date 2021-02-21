/***************************************************************************
  reosrunoffmodel.cpp - ReosRunoffModel

 ---------------------
 begin                : 17.2.2021
 copyright            : (C) 2021 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include "reosrunoffmodel.h"

#include "reosrainfallitem.h"
#include "reostimeserie.h"
#include "reosparameter.h"

ReosRunoffModel::ReosRunoffModel():
  mName( new ReosParameterString( QObject::tr( "Name" ), false, this ) )
{

}

ReosParameterString *ReosRunoffModel::name() const
{
  return mName;
}

void ReosRunoffModel::connectParameters()
{
  QList<ReosParameter *> params = parameters();
  for ( int i = 1; i < params.count(); ++i ) //do not take the first one that should be the name
    connect( params.at( i ), &ReosParameter::valueChanged, this, &ReosRunoffModel::modelChanged );
}

ReosRunoff::ReosRunoff( ReosRunoffModel *runoffModel, ReosTimeSerieConstantInterval *rainfall, QObject *parent ):
  ReosDataObject( parent )
  , mRainfall( rainfall )
  , mRunoffModel( runoffModel )
{
  connect( mRainfall, &ReosDataObject::dataChanged, this, &ReosRunoff::updateValues );
  connect( runoffModel, &ReosRunoffModel::modelChanged, this, &ReosRunoff::updateValues );
}

int ReosRunoff::valueCount() const
{
  return mData.count();
}

ReosDuration ReosRunoff::timeStep() const
{
  return mRainfall->timeStep()->value();
}

double ReosRunoff::value( int i ) const
{
  return mData.at( i );
}

bool ReosRunoff::updateValues()
{
  if ( !mRainfall.isNull() )
    return mRunoffModel->applyRunoffModel( mRainfall, mData );

  return false;
}

ReosRunoffConstantCoefficientModel::ReosRunoffConstantCoefficientModel(): ReosRunoffModel(),
  mCoefficient( new ReosParameterDouble( QObject::tr( "Coefficient" ), false, this ) )
{
  connectParameters();
}

QList<ReosParameter *> ReosRunoffConstantCoefficientModel::parameters() const
{
  QList<ReosParameter *> ret;
  ret << name();
  ret << mCoefficient;

  return ret;
}

bool ReosRunoffConstantCoefficientModel::applyRunoffModel( ReosTimeSerieConstantInterval *rainfall, QVector<double> &runoffResult )
{
  if ( !rainfall )
    return false;

  runoffResult.clear();
  runoffResult.resize( rainfall->valueCount() );

  double coef = mCoefficient->value();

  for ( int i = 0; i < rainfall->valueCount() ; ++i )
    runoffResult[i] = rainfall->valueAt( i ) * coef;

  return true;
}

ReosParameterDouble *ReosRunoffConstantCoefficientModel::coefficient()
{
  return mCoefficient;
}
