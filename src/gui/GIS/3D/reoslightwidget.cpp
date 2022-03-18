/***************************************************************************
  reoslightdirectionwidget.cpp - ReosLightDirectionWidget

 ---------------------
 begin                : 3.3.2022
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
#include "reoslightwidget.h"
#include "ui_reoslightwidget.h"

#include <math.h>

#include <QVector3D>

ReosLightWidget::ReosLightWidget( QWidget *parent ) :
  QWidget( parent ),
  ui( new Ui::ReosLightWidget )
{
  ui->setupUi( this );

  connect( ui->mDialAzimuth, &QDial::valueChanged, this, [this]( int value )
  {
    ui->mSpinBoxAzimuth->blockSignals( true );
    ui->mSpinBoxAzimuth->setValue( value );
    ui->mSpinBoxAzimuth->blockSignals( false );
    emit lightChanged();
  } );

  connect( ui->mSpinBoxAzimuth, QOverload<int>::of( &QSpinBox::valueChanged ), this, [this]( int value )
  {
    ui->mDialAzimuth->blockSignals( true );
    ui->mDialAzimuth->setValue( value );
    ui->mDialAzimuth->blockSignals( false );
    emit lightChanged();
  } );


  connect( ui->mSliderAltitude, &QSlider::valueChanged, this, [this]( int value )
  {
    ui->mSpinBoxAltitude->blockSignals( true );
    ui->mSpinBoxAltitude->setValue( value );
    ui->mSpinBoxAltitude->blockSignals( false );
    emit lightChanged();
  } );

  connect( ui->mSpinBoxAltitude, QOverload<int>::of( &QSpinBox::valueChanged ), this, [this]( int value )
  {
    ui->mSliderAltitude->blockSignals( true );
    ui->mSliderAltitude->setValue( value );
    ui->mSliderAltitude->blockSignals( false );
    emit lightChanged();
  } );

  connect( ui->mIntensitySlider,  &QSlider::valueChanged, this, [this]
  {
    emit lightChanged();
  } );
}

ReosLightWidget::~ReosLightWidget()
{
  delete ui;
}

void ReosLightWidget::setDirection( const QVector3D &direction )
{
  const double horizontalVectorMagnitude = std::sqrt( direction.x() * direction.x() + direction.z() * direction.z() );

  double azimuthAngle = 0;
  if ( horizontalVectorMagnitude != 0 )
  {
    azimuthAngle = ( asin( -direction.x() / horizontalVectorMagnitude ) ) / M_PI * 180;
    if ( direction.z() < 0 )
      azimuthAngle = 180 - azimuthAngle;
    azimuthAngle = std::fmod( azimuthAngle + 360.0, 360.0 );
  }

  ui->mDialAzimuth->blockSignals( true );
  ui->mDialAzimuth->setValue( int( azimuthAngle + 180 ) % 360 );
  ui->mDialAzimuth->blockSignals( false );
  ui->mSpinBoxAzimuth->blockSignals( true );
  ui->mSpinBoxAzimuth->setValue( azimuthAngle );
  ui->mSpinBoxAzimuth->blockSignals( false );

  double altitudeAngle = 0;
  if ( horizontalVectorMagnitude != 0 )
    altitudeAngle = -atan( direction.y() / horizontalVectorMagnitude ) / M_PI * 180;

  ui->mSpinBoxAltitude->blockSignals( true );
  ui->mSpinBoxAltitude->setValue( altitudeAngle );
  ui->mSpinBoxAltitude->blockSignals( false );
  ui->mSliderAltitude->blockSignals( true );
  ui->mSliderAltitude->setValue( altitudeAngle );
  ui->mSliderAltitude->blockSignals( false );
}

void ReosLightWidget::setLightIntensity( float intensity )
{
  ui->mIntensitySlider->setValue( int( intensity * 100 ) );
}

float ReosLightWidget::lightIntensity() const
{
  return float( ui->mIntensitySlider->value() / 100.0 );
}

QVector3D ReosLightWidget::direction() const
{
  const double altitudeValue = ui->mSpinBoxAltitude->value();
  const double azimuthValue = ui->mSpinBoxAzimuth->value();

  const double horizontalVectorMagnitude = cos( altitudeValue / 180 * M_PI );
  float directionX = -horizontalVectorMagnitude * sin( azimuthValue / 180 * M_PI );
  float directionZ = horizontalVectorMagnitude * cos( azimuthValue / 180 * M_PI );
  float directionY = -sin( altitudeValue / 180 * M_PI );

  return QVector3D( directionX, directionY, directionZ );
}
