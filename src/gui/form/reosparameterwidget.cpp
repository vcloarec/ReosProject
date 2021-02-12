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
#include <QDateTimeEdit>
#include "reosparameterwidget.h"


ReosParameterWidget::ReosParameterWidget( QWidget *parent ):
  QWidget( parent )
{
  setLayout( new QHBoxLayout( this ) );
  layout()->setContentsMargins( 0, 0, 0, 0 );
  mLabelName = new QLabel( this );
  layout()->addWidget( mLabelName );
}

ReosParameterInLineWidget::ReosParameterInLineWidget( QWidget *parent ):
  ReosParameterWidget( parent )
{
  mLineEdit = new QLineEdit( this );
  layout()->addWidget( mLineEdit );
  mLineEdit->setAlignment( Qt::AlignRight );

  connect( mLineEdit, &QLineEdit::editingFinished, this, &ReosParameterWidget::applyValue );

  connect( mLineEdit, &QLineEdit::textEdited, this, [this]
  {
    this->mLineEdit->setStyleSheet( "color: black" );
  } );
}

void ReosParameterInLineWidget::setFocusOnEdit()
{
  mLineEdit->setFocus();
}

ReosParameterWidget *ReosParameterWidget::createWidget( ReosParameter *parameter, QWidget *parent )
{
  if ( parameter->type() == ReosParameterStringWidget::type() )
    return new ReosParameterStringWidget( static_cast<ReosParameterString *>( parameter ), parent );

  if ( parameter->type() == ReosParameterAreaWidget::type() )
    return new ReosParameterAreaWidget( static_cast<ReosParameterArea *>( parameter ), parent );

  if ( parameter->type() == ReosParameterSlopeWidget::type() )
    return new ReosParameterSlopeWidget( static_cast<ReosParameterSlope *>( parameter ), parent );

  if ( parameter->type() == ReosParameterDurationWidget::type() )
    return new ReosParameterDurationWidget( static_cast<ReosParameterDuration *>( parameter ), parent );

  if ( parameter->type() == ReosParameterDateTimeWidget::type() )
    return new ReosParameterDateTimeWidget( static_cast<ReosParameterDateTime *>( parameter ), parent );

  if ( parameter->type() == ReosParameterDoubleWidget::type() )
    return new ReosParameterDoubleWidget( static_cast<ReosParameterDouble *>( parameter ), parent );

  return nullptr;

}

void ReosParameterInLineWidget::setTextValue( double value )
{
  if ( mParameter->isDerived() )
  {
    mLineEdit->setStyleSheet( "color: grey" );
  }
  else
    mLineEdit->setStyleSheet( "color: black" );

  mLineEdit->setText( QString::number( value, 'f', 2 ) );

}

void ReosParameterInLineWidget::setTextValue( const QString &str )
{
  if ( mParameter->isDerived() )
  {
    mLineEdit->setStyleSheet( "color: grey" );
  }
  else
    mLineEdit->setStyleSheet( "color: black" );

  mLineEdit->setText( str );
}

double ReosParameterInLineWidget::value() const
{
  return mLineEdit->text().toDouble();
}

QString ReosParameterInLineWidget::textValue() const
{
  return mLineEdit->text();
}

void ReosParameterWidget::setParameter( ReosParameter *param )
{
  if ( mParameter )
    disconnect( mParameter, &ReosParameter::valueChanged, this, &ReosParameterWidget::updateValue );

  mParameter = param;

  if ( mParameter )
  {
    mLabelName->setText( param->name() );
    mDerivationButton->setVisible( mParameter->isDerivable() );
    connect( mParameter, &ReosParameter::valueChanged, this, &ReosParameterWidget::updateValue );
  }
  else
  {
    mLabelName->setText( QString( '-' ) );
    mDerivationButton->setVisible( false );
  }
}

void ReosParameterWidget::finalizeWidget()
{
  mDerivationButton = new QToolButton( this );
  layout()->addWidget( mDerivationButton );
  connect( mDerivationButton, &QToolButton::clicked, this, &ReosParameterWidget::askDerivation );
  mDerivationButton->setVisible( mParameter && mParameter->isDerivable() );
}


void ReosParameterWidget::askDerivation()
{
  if ( mParameter && mParameter->isDerivable() )
  {
    mDerivationButton->setFocus(); //to avoid a focus on the line edit --> that produce a signal textEdited that set the param not derived
    mParameter->askForDerivation();
  }
}


ReosParameterAreaWidget::ReosParameterAreaWidget( QWidget *parent ):
  ReosParameterInLineWidget( parent )
{
  mUnitCombobox = new QComboBox( this );
  layout()->addWidget( mUnitCombobox );

  mUnitCombobox->addItem( ReosArea::unitToString( ReosArea::m2 ), ReosArea::m2 );
  mUnitCombobox->addItem( ReosArea::unitToString( ReosArea::a ), ReosArea::a );
  mUnitCombobox->addItem( ReosArea::unitToString( ReosArea::ha ), ReosArea::ha );
  mUnitCombobox->addItem( ReosArea::unitToString( ReosArea::km2 ), ReosArea::km2 );

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
  ReosParameterInLineWidget( parent )
{
  mLabelSlopeUnit = new QLabel( QString( '%' ) );
  layout()->addWidget( mLabelSlopeUnit );

  finalizeWidget();
}

ReosParameterSlopeWidget::ReosParameterSlopeWidget( ReosParameterSlope *slope, QWidget *parent ):
  ReosParameterSlopeWidget( parent )
{
  setSlope( slope );
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

ReosParameterStringWidget::ReosParameterStringWidget( QWidget *parent ):
  ReosParameterInLineWidget( parent )
{
  finalizeWidget();
}

ReosParameterStringWidget::ReosParameterStringWidget( ReosParameterString *string, QWidget *parent ):
  ReosParameterStringWidget( parent )
{
  setString( string );
}

void ReosParameterStringWidget::setString( ReosParameterString *string )
{
  setParameter( string );
}

void ReosParameterStringWidget::updateValue()
{
  if ( stringParameter() )
    setTextValue( stringParameter()->value() );
}

void ReosParameterStringWidget::applyValue()
{
  if ( stringParameter() )
    stringParameter()->setValue( textValue() );
}

ReosParameterString *ReosParameterStringWidget::stringParameter()
{
  if ( mParameter )
    return static_cast<ReosParameterString *>( mParameter );

  return nullptr;
}


ReosParameterDoubleWidget::ReosParameterDoubleWidget( QWidget *parent ):
  ReosParameterInLineWidget( parent )
{
  finalizeWidget();
}

ReosParameterDoubleWidget::ReosParameterDoubleWidget( ReosParameterDouble *value, QWidget *parent ):
  ReosParameterDoubleWidget( parent )
{
  setDouble( value );
}

void ReosParameterDoubleWidget::setDouble( ReosParameterDouble *value )
{
  setParameter( value );
}

void ReosParameterDoubleWidget::updateValue()
{
  if ( doubleParameter() )
    setTextValue( doubleParameter()->toString() );
}

void ReosParameterDoubleWidget::applyValue()
{
  if ( doubleParameter() )
  {
    bool ok = false;
    double v = textValue().toDouble( &ok );
    if ( ok )
      doubleParameter()->setValue( v );
    else
      doubleParameter()->setInvalid();
  }
}

ReosParameterDouble *ReosParameterDoubleWidget::doubleParameter()
{
  if ( mParameter )
    return static_cast<ReosParameterDouble *>( mParameter );

  return nullptr;
}


ReosParameterDurationWidget::ReosParameterDurationWidget( QWidget *parent ):
  ReosParameterInLineWidget( parent )
{
  mUnitCombobox = new QComboBox( this );
  layout()->addWidget( mUnitCombobox );

  mUnitCombobox->addItem( tr( "millisecond" ), ReosDuration::millisecond );
  mUnitCombobox->addItem( tr( "second" ), ReosDuration::second );
  mUnitCombobox->addItem( tr( "minute" ), ReosDuration::minute );
  mUnitCombobox->addItem( tr( "hour" ), ReosDuration::hour );
  mUnitCombobox->addItem( tr( "day" ), ReosDuration::day );
  mUnitCombobox->addItem( tr( "week" ), ReosDuration::week );
  mUnitCombobox->addItem( tr( "month" ), ReosDuration::month );
  mUnitCombobox->addItem( tr( "year" ), ReosDuration::year );


  connect( mUnitCombobox, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, &ReosParameterWidget::updateValue );
  connect( mUnitCombobox, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, [this]
  {
    if ( this->durationParameter() )
      this->durationParameter()->changeUnit( static_cast<ReosDuration::Unit>( mUnitCombobox->currentData().toInt() ) );
  } );

  finalizeWidget();
}

ReosParameterDurationWidget::ReosParameterDurationWidget( ReosParameterDuration *duration, QWidget *parent ):
  ReosParameterDurationWidget( parent )
{
  setDuration( duration );
}

void ReosParameterDurationWidget::setDuration( ReosParameterDuration *duration )
{
  setParameter( duration );

  if ( durationParameter() )
  {
    setTextValue( durationParameter()->value().valueUnit() );
    mUnitCombobox->setCurrentIndex( mUnitCombobox->findData( durationParameter()->value().unit() ) );
    show();
  }
  else
  {
    setTextValue( std::numeric_limits<double>::quiet_NaN() );
    mUnitCombobox->setCurrentIndex( -1 );
    hide();
  }
}

void ReosParameterDurationWidget::updateValue()
{
  if ( durationParameter() )
    setTextValue( durationParameter()->value().valueUnit( static_cast<ReosDuration::Unit>( mUnitCombobox->currentData().toInt() ) ) );
}

void ReosParameterDurationWidget::applyValue()
{
  if ( durationParameter() )
  {
    durationParameter()->setValue( ReosDuration( value(), static_cast<ReosDuration::Unit>( mUnitCombobox->currentData().toInt() ) ) );
    setTextValue( value() );
  }
}

ReosParameterDuration *ReosParameterDurationWidget::durationParameter() const
{
  return static_cast<ReosParameterDuration *>( mParameter );
}


ReosParameterDateTimeWidget::ReosParameterDateTimeWidget( QWidget *parent ):
  ReosParameterWidget( parent )
  , mDateTimeEdit( new QDateTimeEdit( this ) )
{
  layout()->addWidget( mDateTimeEdit );
  connect( mDateTimeEdit, &QDateTimeEdit::dateTimeChanged, this, &ReosParameterWidget::applyValue );
  mDateTimeEdit->setDisplayFormat( QStringLiteral( "yyyy.MM.dd HH:mm:ss" ) );
  mDateTimeEdit->setTimeSpec( Qt::UTC );
  finalizeWidget();
}

ReosParameterDateTimeWidget::ReosParameterDateTimeWidget( ReosParameterDateTime *dateTime, QWidget *parent ):
  ReosParameterDateTimeWidget( parent )
{
  setDateTime( dateTime );
}

void ReosParameterDateTimeWidget::setDateTime( ReosParameterDateTime *dateTime )
{
  setParameter( dateTime );

  if ( dateTimeParameter() )
  {
    updateValue();
    show();
  }
  else
  {
    hide();
  }
}

void ReosParameterDateTimeWidget::updateValue()
{
  if ( dateTimeParameter() && mDateTimeEdit )
    mDateTimeEdit->setDateTime( dateTimeParameter()->value() );
}

void ReosParameterDateTimeWidget::applyValue()
{
  if ( dateTimeParameter() && mDateTimeEdit )
    dateTimeParameter()->setValue( mDateTimeEdit->dateTime() );
}

void ReosParameterDateTimeWidget::setFocusOnEdit()
{
  mDateTimeEdit->setFocus();
}

ReosParameterDateTime *ReosParameterDateTimeWidget::dateTimeParameter() const
{
  return static_cast<ReosParameterDateTime *>( mParameter );
}
