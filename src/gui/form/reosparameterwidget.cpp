/***************************************************************************
  reosparameterwidget.cpp - ReosParameterWidget

 ---------------------
 begin                : 22.1.2021
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

#include <QHBoxLayout>
#include <QLineEdit>
#include <QComboBox>
#include <QLabel>
#include <QToolButton>

#include "reosparameterwidget.h"

ReosParameterWidget::ReosParameterWidget( QWidget *parent ):
  QWidget( parent )
{
  setLayout( new QHBoxLayout( this ) );

  mLabelName = new QLabel( this );
  layout()->addWidget( mLabelName );

  mLineEdit = new QLineEdit( this );
  layout()->addWidget( mLineEdit );
  mLineEdit->setAlignment( Qt::AlignRight );

  connect( mLineEdit, &QLineEdit::editingFinished, this, &ReosParameterWidget::applyValue );

  connect( mLineEdit, &QLineEdit::textEdited, this, [this]
  {
    this->mLineEdit->setStyleSheet( "color: black" );
  } );
}

void ReosParameterWidget::setTextValue( double value )
{
  if ( mParameter->isDerived() )
  {
    mLineEdit->setStyleSheet( "color: grey" );
  }
  else
    mLineEdit->setStyleSheet( "color: black" );

  mLineEdit->setText( QString::number( value, 'f', 2 ) );

}

double ReosParameterWidget::value() const
{
  return mLineEdit->text().toDouble();
}

void ReosParameterWidget::setParameter( ReosParameter *param )
{
  if ( mParameter )
    disconnect( mParameter, &ReosParameter::valueChanged, this, &ReosParameterWidget::updateValue );

  mParameter = param;

  if ( mParameter )
  {
    mLabelName->setText( param->name() );
    connect( mParameter, &ReosParameter::valueChanged, this, &ReosParameterWidget::updateValue );
  }
  else
  {
    mLabelName->setText( QString( '-' ) );
  }
}

void ReosParameterWidget::finalizeWidget()
{
  mDerivationButton = new QToolButton( this );
  layout()->addWidget( mDerivationButton );

  connect( mDerivationButton, &QToolButton::clicked, this, &ReosParameterWidget::askDerivation );
}


void ReosParameterWidget::askDerivation()
{
  if ( mParameter )
  {
    mDerivationButton->setFocus(); //to avoid a ficus on the line edit --> that produce a signal textEdited that set the param not derived
    mParameter->askForDerivation();
  }

}


ReosParameterAreaWidget::ReosParameterAreaWidget( QWidget *parent ):
  ReosParameterWidget( parent )
{
  mUnitCombobox = new QComboBox( this );
  layout()->addWidget( mUnitCombobox );

  mUnitCombobox->addItem( QString( 'm' ).append( QChar( 0x00B2 ) ), ReosArea::m2 );
  mUnitCombobox->addItem( tr( "a" ), ReosArea::a );
  mUnitCombobox->addItem( tr( "ha" ), ReosArea::ha );
  mUnitCombobox->addItem( QStringLiteral( "km" ).append( QChar( 0x00B2 ) ), ReosArea::km2 );

  connect( mUnitCombobox, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, &ReosParameterWidget::updateValue );
  connect( mUnitCombobox, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, [this]
  {
    if ( this->areaParameter() )
      this->areaParameter()->changeUnit( static_cast<ReosArea::Unit>( mUnitCombobox->currentData().toInt() ) );
  } );

  finalizeWidget();
}

ReosParameterAreaWidget::ReosParameterAreaWidget( ReosParameterArea *area, QWidget *parent ):
  ReosParameterAreaWidget( parent )
{
  setArea( area );
}

void ReosParameterAreaWidget::setArea( ReosParameterArea *area )
{
  setParameter( area );

  if ( areaParameter() )
  {
    setTextValue( areaParameter()->value().valueInUnit() );
    mUnitCombobox->setCurrentIndex( mUnitCombobox->findData( areaParameter()->value().unit() ) );
    show();
  }
  else
  {
    setTextValue( std::numeric_limits<double>::quiet_NaN() );
    mUnitCombobox->setCurrentIndex( -1 );
    hide();
  }
}

void ReosParameterAreaWidget::updateValue()
{
  if ( areaParameter() )
    setTextValue( areaParameter()->value().valueInUnit( static_cast<ReosArea::Unit>( mUnitCombobox->currentData().toInt() ) ) );
}

void ReosParameterAreaWidget::applyValue()
{
  if ( areaParameter() )
  {
    areaParameter()->setValue( ReosArea( value(), static_cast<ReosArea::Unit>( mUnitCombobox->currentData().toInt() ) ) );
    setTextValue( value() );
  }
}

ReosParameterArea *ReosParameterAreaWidget::areaParameter() const
{
  return static_cast<ReosParameterArea *>( mParameter );
}


ReosParameterSlopeWidget::ReosParameterSlopeWidget( QWidget *parent ):
  ReosParameterWidget( parent )
{
  mLabelSlopeUnit = new QLabel( QString( '%' ) );
  layout()->addWidget( mLabelSlopeUnit );

  finalizeWidget();
}

void ReosParameterSlopeWidget::setSlope( ReosParameterSlope *slope )
{
  setParameter( slope );

  if ( slopeParameter() )
  {
    updateValue();
    show();
  }
  else
  {
    hide();
  }
}

void ReosParameterSlopeWidget::updateValue()
{
  if ( int( slopeParameter()->value() * 1000 ) == 0 )
  {
    mFactor = 1000;
    mLabelSlopeUnit->setText( QChar( 0x2030 ) );
  }
  else
  {
    mFactor = 100;
    mLabelSlopeUnit->setText( QString( '%' ) );
  }

  setTextValue( slopeParameter()->value() * mFactor );
}

void ReosParameterSlopeWidget::applyValue()
{
  if ( slopeParameter() )
  {
    slopeParameter()->setValue( value() / mFactor );
    updateValue();
  }
}

ReosParameterSlope *ReosParameterSlopeWidget::slopeParameter() const
{
  return static_cast<ReosParameterSlope *>( mParameter );
}
