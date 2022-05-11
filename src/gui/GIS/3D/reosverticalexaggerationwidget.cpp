/***************************************************************************
  reosverticalexaggerationwidget.cpp - ReosVerticalExaggerationWidget

 ---------------------
 begin                : 14.3.2022
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
#include "reosverticalexaggerationwidget.h"
#include "ui_reosverticalexaggerationwidget.h"

#include <math.h>

#include "reosparameter.h"

ReosVerticalExaggerationWidget::ReosVerticalExaggerationWidget( QWidget *parent ) :
  QWidget( parent ),
  ui( new Ui::ReosVerticalExaggerationWidget ),
  mExaggerationParameter( new ReosParameterDouble( QString(), false, this ) )
{

  ui->setupUi( this );
  mExaggerationParameter->setValue( 1 );
  ui->mExaggerationParameter->setDouble( mExaggerationParameter );

  connect( ui->mSlider, &QSlider::valueChanged, this, [this]( int value )
  {
    mExaggeration = exagerationFromSliderValue( value );
  } );

  connect( ui->mSlider, &ReosSliderElastic::finalValue, this, [this]( int value )
  {
    mExaggeration = exagerationFromSliderValue( value );
    mPreviousValue = mExaggeration;
  } );


  connect( mExaggerationParameter, &ReosParameter::valueChanged, [this]
  {
    mExaggeration = mExaggerationParameter->value();
    emit valueChanged( mExaggeration );
  } );
}


double ReosVerticalExaggerationWidget::exagerationFromSliderValue( int value ) const
{
  double newValue;

  if ( mPreviousValue > 1 )
  {
    newValue = mPreviousValue + 4.0 * value / 10.0;

    if ( newValue < 1 )
    {
      double remainModif = mPreviousValue - 1 - 4.0 * value / 10.0;
      newValue = 1 / remainModif;
    }
  }
  else
  {
    double old = 1 / mPreviousValue;
    double newDivisor = old - 4.0 * value / 10.0 ;
    if ( newDivisor < 1 )
    {
      double remainDivisor = 1 + 4.0 * value / 10.0 - 1 / mPreviousValue;
      newValue = 1 + remainDivisor;
    }
    else
      newValue = 1 / newDivisor;
  }

  mExaggerationParameter->setValue( newValue );

  return newValue;
}

ReosVerticalExaggerationWidget::~ReosVerticalExaggerationWidget()
{
  delete ui;
}

void ReosVerticalExaggerationWidget::setExageration( double exageration )
{
  mExaggerationParameter->setValue( exageration );
  mExaggeration = exageration;
  mPreviousValue = mExaggeration;
}

double ReosVerticalExaggerationWidget::exageration() const
{
  return mExaggeration;
}

void ReosSliderElastic::mouseReleaseEvent( QMouseEvent *ev )
{
  emit finalValue( value() );
  blockSignals( true );
  setValue( ( minimum() + maximum() ) / 2 );
  blockSignals( false );
  QSlider::mouseReleaseEvent( ev );
}
