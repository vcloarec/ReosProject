/***************************************************************************
  reoscoordinatesystemwidget.cpp - ReosCoordinateSystemWidget

 ---------------------
 begin                : 16.1.2023
 copyright            : (C) 2023 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include "reoscoordinatesystemwidget.h"

#include <QHBoxLayout>
#include <qgsprojectionselectionwidget.h>

ReosCoordinateSystemWidget::ReosCoordinateSystemWidget( QWidget *parent )
  : QWidget( parent )
  ,   mQgsWidget( new QgsProjectionSelectionWidget( this ) )
{

  QHBoxLayout *lay = new QHBoxLayout( this );
  setLayout( lay );
  lay->setContentsMargins( 0, 0, 0, 0 );
  lay->addWidget( mQgsWidget );

  connect( mQgsWidget, &QgsProjectionSelectionWidget::crsChanged, this, &ReosCoordinateSystemWidget::crsChanged );
}

QString ReosCoordinateSystemWidget::crs() const
{
  return mQgsWidget->crs().toWkt( QgsCoordinateReferenceSystem::WKT_PREFERRED );
}

void ReosCoordinateSystemWidget::setCrs( const QString &crs )
{
  mQgsWidget->setCrs( QgsCoordinateReferenceSystem::fromWkt( crs ) );
}

int ReosCoordinateSystemWidget::bestDecimalNumber() const
{
  int decimals = 4;
  switch ( mQgsWidget->crs().mapUnits() )
  {
    case QgsUnitTypes::DistanceDegrees:
    case QgsUnitTypes::DistanceUnknownUnit:
      decimals = 9;
      break;
    case QgsUnitTypes::DistanceMeters:
    case QgsUnitTypes::DistanceKilometers:
    case QgsUnitTypes::DistanceFeet:
    case QgsUnitTypes::DistanceNauticalMiles:
    case QgsUnitTypes::DistanceYards:
    case QgsUnitTypes::DistanceMiles:
    case QgsUnitTypes::DistanceCentimeters:
    case QgsUnitTypes::DistanceMillimeters:
      decimals = 4;
      break;
  }

  return decimals;
}
