/***************************************************************************
  reosstyleregistery.cpp - ReosStyleRegistery

 ---------------------
 begin                : 26.11.2021
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
#include "reosstyleregistery.h"
#include "reossettings.h"

ReosStyleRegistery *ReosStyleRegistery::sInstance = nullptr;

ReosStyleRegistery::ReosStyleRegistery( ReosModule *parent ): ReosModule( parent )
{
  mCurveColor << QColor( "#00c842" )
              << QColor( "#e48100" )
              << QColor( "#e40098" )
              << QColor( "#007ae4" )
              << QColor( "#5b9800" )
              << QColor( "#986300" )
              << QColor( "#e18dff" )
              << QColor( "#3fecff" )
              << QColor( "#c9d000" )
              << QColor( "#676767" );

  ReosSettings settings;
  if ( settings.contains( QStringLiteral( "StyleRegistery/last-index-curve-color" ) ) )
    mLastCurveColor = settings.value( QStringLiteral( "StyleRegistery/last-index-curve-color" ) ).toInt();
}

ReosStyleRegistery::~ReosStyleRegistery()
{
  ReosSettings settings;
  settings.setValue( QStringLiteral( "StyleRegistery/last-index-curve-color" ), mLastCurveColor );
}

void ReosStyleRegistery::instantiate( ReosModule *parent )
{
  if ( !sInstance )
    sInstance = new ReosStyleRegistery( parent );
}

ReosStyleRegistery *ReosStyleRegistery::instance()
{
  if ( !sInstance )
    sInstance = new ReosStyleRegistery;

  return sInstance;
}

QColor ReosStyleRegistery::curveColor() const
{
  return mCurveColor.at( ( ++mLastCurveColor ) % ( mCurveColor.count() ) );
}
