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

#include <QCheckBox>
#include <QHBoxLayout>
#include <QVBoxLayout>
#include <QLineEdit>
#include <QComboBox>
#include <QLabel>
#include <QToolButton>
#include <QDateTimeEdit>
#include <QTextEdit>
#include <QApplication>


#include "reosparameterwidget.h"


ReosParameterWidget::ReosParameterWidget( const QString &defaultName, QWidget *parent, Qt::Orientation orientation ):
  QWidget( parent )
  , mDefaultName( defaultName )
{
  if ( orientation == Qt::Horizontal )
    mLayout =  new QHBoxLayout( this );
  else
    mLayout =  new QVBoxLayout( this );

  setLayout( mLayout );
  layout()->setContentsMargins( 0, 0, 0, 0 );
  mLabelName = new QLabel( defaultName, this );
  layout()->addWidget( mLabelName );
}

ReosParameterInLineWidget::ReosParameterInLineWidget( QWidget *parent, const QString &defaultName ):
  ReosParameterWidget( defaultName, parent )
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

  if ( parameter->type() == ReosParameterIntegerWidget::type() )
    return new ReosParameterIntegerWidget( static_cast<ReosParameterInteger *>( parameter ), parent );

  if ( parameter->type() == ReosParameterBooleanWidget::type() )
    return new ReosParameterBooleanWidget( static_cast<ReosParameterBoolean *>( parameter ), parent );

  if ( parameter->type() == ReosParameterLongStringWidget::type() )
    return new ReosParameterLongStringWidget( static_cast<ReosParameterLongString *>( parameter ), parent );

  return nullptr;
}

void ReosParameterWidget::enableSpacer( SpacerPosition spacerPosition )
{
  if ( mSpacerBefore )
  {
    mLayout->removeItem( mSpacerBefore );
    delete mSpacerBefore;
    mSpacerBefore = nullptr;
  }

  if ( mSpacerMiddle )
  {
    mLayout->removeItem( mSpacerMiddle );
    delete mSpacerMiddle;
    mSpacerMiddle = nullptr;
  }

  if ( mSpacerAfter )
  {
    mLayout->removeItem( mSpacerAfter );
    delete mSpacerAfter;
    mSpacerAfter = nullptr;
  }

  if ( spacerPosition & SpacerPosition::SpacerAfter )
  {
    mSpacerAfter = new QSpacerItem( 0, 0, QSizePolicy::Expanding, QSizePolicy::Ignored );
    mLayout->addItem( mSpacerAfter );
  }

  if ( spacerPosition & SpacerPosition::SpacerInMiddle )
  {
    mSpacerMiddle = new QSpacerItem( 0, 0, QSizePolicy::Expanding, QSizePolicy::Ignored );
    mLayout->insertItem( 1, mSpacerMiddle );
  }

  if ( spacerPosition & SpacerPosition::SpacerBefore )
  {
    mSpacerBefore = new QSpacerItem( 0, 0, QSizePolicy::Expanding, QSizePolicy::Ignored );
    mLayout->insertItem( 0, mSpacerBefore );
  }

}

void ReosParameterInLineWidget::setTextValue( double value )
{
  if ( mParameter && mParameter->isDerived() )
  {
    mLineEdit->setStyleSheet( "color: grey" );
  }
  else
    mLineEdit->setStyleSheet( styleSheet() );

  mLineEdit->setText( ReosParameter::doubleToString( value, 2 ) );
  mCurrentText = mLineEdit->text();

}

void ReosParameterInLineWidget::setTextValue( const QString &str )
{
  if ( mParameter && mParameter->isDerived() )
  {
    mLineEdit->setStyleSheet( "color: grey" );
  }
  else
    mLineEdit->setStyleSheet( styleSheet() );

  mLineEdit->setText( str );
  mCurrentText = mLineEdit->text();
}

double ReosParameterInLineWidget::value() const
{
  bool ok;
  return ReosParameter::stringToDouble( mLineEdit->text(), &ok );
}

QString ReosParameterInLineWidget::textValue() const
{
  mCurrentText = mLineEdit->text();
  return mCurrentText;
}

bool ReosParameterInLineWidget::textHasChanged() const
{
  return mCurrentText != mLineEdit->text();
}

void ReosParameterWidget::setParameter( ReosParameter *param )
{
  if ( mParameter && !mParameter.isNull() )
  {
    disconnect( mParameter, &ReosParameter::valueChanged, this, &ReosParameterWidget::updateValue );
    disconnect( mParameter, &ReosParameter::valueChanged, this, &ReosParameterWidget::valueChanged );
    disconnect( mParameter, &ReosParameter::unitChanged, this, &ReosParameterWidget::updateValue );
    disconnect( mParameter, &ReosParameter::unitChanged, this, &ReosParameterWidget::unitChanged );
  }

  mParameter = param;

  if ( mParameter )
  {
    if ( param->name().isEmpty() )
      mLabelName->setText( mDefaultName );
    else
      mLabelName->setText( param->name() );
    mDerivationButton->setVisible( mParameter->isDerivable() );
    setEnabled( param->isEditable() );
    connect( mParameter, &ReosParameter::valueChanged, this, &ReosParameterWidget::updateValue );
    connect( mParameter, &ReosParameter::unitChanged, this, &ReosParameterWidget::updateValue );
    connect( mParameter, &ReosParameter::valueChanged, this, &ReosParameterWidget::valueChanged );
    connect( mParameter, &ReosParameter::unitChanged, this, &ReosParameterWidget::unitChanged );
  }
  else
  {
    mLabelName->setText( mDefaultName );
    mDerivationButton->setVisible( false );
  }


}

void ReosParameterWidget::setDefaultName( const QString &defaultName )
{
  mDefaultName = defaultName;
  if ( mLabelName->text().isEmpty() )
    mLabelName->setText( defaultName );
}

void ReosParameterWidget::hideWhenVoid( bool b )
{
  mHideWhenVoid = b;
}

void ReosParameterWidget::finalizeWidget()
{
  mDerivationButton = new QToolButton( this );
  layout()->addWidget( mDerivationButton );
  connect( mDerivationButton, &QToolButton::clicked, this, &ReosParameterWidget::askDerivation );
  mDerivationButton->setVisible( mParameter && mParameter->isDerivable() );
  mDerivationButton->setIcon( QPixmap( ":/images/calculation.svg" ) );
}


void ReosParameterWidget::askDerivation()
{
  if ( mParameter && mParameter->isDerivable() )
  {
    mDerivationButton->setFocus(); //to avoid a focus on the line edit --> that produce a signal textEdited that set the param not derived
    QApplication::setOverrideCursor( Qt::WaitCursor );
    mParameter->askForDerivation();
    QApplication::restoreOverrideCursor();
  }
}


ReosParameterAreaWidget::ReosParameterAreaWidget( QWidget *parent, const QString &defaultName ):
  ReosParameterInLineWidget( parent, defaultName )
{
  mUnitCombobox = new QComboBox( this );
  layout()->addWidget( mUnitCombobox );

  mUnitCombobox->addItem( ReosArea::unitToString( ReosArea::m2 ), ReosArea::m2 );
  mUnitCombobox->addItem( ReosArea::unitToString( ReosArea::a ), ReosArea::a );
  mUnitCombobox->addItem( ReosArea::unitToString( ReosArea::ha ), ReosArea::ha );
  mUnitCombobox->addItem( ReosArea::unitToString( ReosArea::km2 ), ReosArea::km2 );

  connect( mUnitCombobox, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, [this]
  {
    if ( this->areaParameter() )
      this->areaParameter()->changeUnit( static_cast<ReosArea::Unit>( mUnitCombobox->currentData().toInt() ) );
    this->updateValue();
  } );

  finalizeWidget();
}

ReosParameterAreaWidget::ReosParameterAreaWidget( ReosParameterArea *area, QWidget *parent ):
  ReosParameterAreaWidget( parent, area ? area->name() : QString() )
{
  setArea( area );
}

void ReosParameterAreaWidget::setArea( ReosParameterArea *area )
{
  setParameter( area );
  updateValue();

}

void ReosParameterAreaWidget::updateValue()
{
  if ( areaParameter() && areaParameter()->isValid() )
  {
    setTextValue( areaParameter()->value().valueInUnit() );
    mUnitCombobox->setCurrentIndex( mUnitCombobox->findData( areaParameter()->value().unit() ) );
    show();
  }
  else
  {
    setTextValue( QString( '-' ) );
    mUnitCombobox->setCurrentIndex( -1 );
    show();
  }

  if ( mHideWhenVoid && !areaParameter() )
    hide();
}

void ReosParameterAreaWidget::applyValue()
{
  if ( textHasChanged() && areaParameter() )
  {
    ReosArea newValue( value(), static_cast<ReosArea::Unit>( mUnitCombobox->currentData().toInt() ) );
    if ( newValue != areaParameter()->value() )
    {
      setTextValue( value() );
      areaParameter()->setValue( newValue );
    }
  }
}

ReosParameterArea *ReosParameterAreaWidget::areaParameter() const
{
  return static_cast<ReosParameterArea *>( mParameter.data() );
}


ReosParameterSlopeWidget::ReosParameterSlopeWidget( QWidget *parent, const QString &defaultName ):
  ReosParameterInLineWidget( parent, defaultName )
{
  mLabelSlopeUnit = new QLabel( QString( '%' ) );
  layout()->addWidget( mLabelSlopeUnit );

  finalizeWidget();
}

ReosParameterSlopeWidget::ReosParameterSlopeWidget( ReosParameterSlope *slope, QWidget *parent ):
  ReosParameterSlopeWidget( parent, slope ? slope->name() : QString() )
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
    if ( mHideWhenVoid )
      hide();
  }
}

void ReosParameterSlopeWidget::updateValue()
{
  if ( slopeParameter() && slopeParameter()->isValid() )
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
    show();
  }
  else
  {
    setTextValue( QString( '-' ) );
    mLabelSlopeUnit->setText( QString( '%' ) );
    show();
  }

  if ( mHideWhenVoid && !slopeParameter() )
    hide();

}

void ReosParameterSlopeWidget::applyValue()
{
  if ( textHasChanged() && slopeParameter() && ( value() / mFactor ) != slopeParameter()->value() )
  {
    slopeParameter()->setValue( value() / mFactor );
    updateValue();
  }
}

ReosParameterSlope *ReosParameterSlopeWidget::slopeParameter() const
{
  return static_cast<ReosParameterSlope *>( mParameter.data() );
}

ReosParameterStringWidget::ReosParameterStringWidget( QWidget *parent, const QString &defaultName ):
  ReosParameterInLineWidget( parent, defaultName )
{
  finalizeWidget();
}

ReosParameterStringWidget::ReosParameterStringWidget( ReosParameterString *string, QWidget *parent ):
  ReosParameterStringWidget( parent, string ? string->name() : QString() )
{
  setString( string );
}

void ReosParameterStringWidget::setString( ReosParameterString *string )
{
  setParameter( string );
  updateValue();
}

void ReosParameterStringWidget::updateValue()
{
  if ( stringParameter() && stringParameter()->isValid() )
  {
    setTextValue( stringParameter()->value() );
    show();
  }
  else
  {
    setTextValue( QString( '-' ) );
    show();
  }

  if ( mHideWhenVoid && !stringParameter() )
    hide();
}

void ReosParameterStringWidget::applyValue()
{
  if ( textHasChanged() && stringParameter() )
    stringParameter()->setValue( textValue() );
}

ReosParameterString *ReosParameterStringWidget::stringParameter()
{
  if ( mParameter )
    return static_cast<ReosParameterString *>( mParameter.data() );

  return nullptr;
}


ReosParameterDoubleWidget::ReosParameterDoubleWidget( QWidget *parent, const QString &defaultName ):
  ReosParameterInLineWidget( parent, defaultName )
{
  finalizeWidget();
}

ReosParameterDoubleWidget::ReosParameterDoubleWidget( ReosParameterDouble *value, QWidget *parent ):
  ReosParameterDoubleWidget( parent, value ? value->name() : QString() )
{
  setDouble( value );
}

void ReosParameterDoubleWidget::setDouble( ReosParameterDouble *value )
{
  setParameter( value );
  updateValue();
}

void ReosParameterDoubleWidget::updateValue()
{
  if ( doubleParameter() && doubleParameter()->isValid() )
  {
    setTextValue( doubleParameter()->toString() );
    show();
  }
  else
  {
    setTextValue( QString( '-' ) );
    show();
  }

  if ( mHideWhenVoid && !doubleParameter() )
    hide();
}

void ReosParameterDoubleWidget::applyValue()
{
  if ( textHasChanged() && doubleParameter() && value() != doubleParameter()->value() )
  {
    bool ok = false;
    double v = ReosParameter::stringToDouble( textValue(), &ok );
    if ( ok )
      doubleParameter()->setValue( v );
    else
      doubleParameter()->setInvalid();
  }
}

ReosParameterDouble *ReosParameterDoubleWidget::doubleParameter()
{
  if ( mParameter )
    return static_cast<ReosParameterDouble *>( mParameter.data() );

  return nullptr;
}


ReosParameterDurationWidget::ReosParameterDurationWidget( QWidget *parent, const QString &defaultName ):
  ReosParameterInLineWidget( parent, defaultName )
{
  mUnitCombobox = new ReosDurationUnitComboBox( this );
  layout()->addWidget( mUnitCombobox );

  connect( mUnitCombobox, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, [this]
  {
    if ( this->durationParameter() )
      this->durationParameter()->changeUnit( mUnitCombobox->currentUnit() );
    this->updateValue();
  } );

  finalizeWidget();
}

ReosParameterDurationWidget::ReosParameterDurationWidget( ReosParameterDuration *duration, QWidget *parent ):
  ReosParameterDurationWidget( parent, duration ? duration->name() : QString() )
{
  setDuration( duration );
}

void ReosParameterDurationWidget::setDuration( ReosParameterDuration *duration )
{
  setParameter( duration );
  updateValue();
}

void ReosParameterDurationWidget::updateValue()
{
  if ( durationParameter() && durationParameter()->isValid() )
  {
    setTextValue( durationParameter()->value().valueUnit() );
    mUnitCombobox->setCurrentUnit( durationParameter()->value().unit() );
    if ( !mCanBeAlwaysHidden )
      show();
  }
  else
  {
    setTextValue( QString( '-' ) );
    mUnitCombobox->setCurrentIndex( -1 );
    if ( !mCanBeAlwaysHidden )
      show();
  }

  if ( mHideWhenVoid && !durationParameter() )
    hide();
}

void ReosParameterDurationWidget::applyValue()
{
  if ( textHasChanged() && durationParameter() && textHasChanged() )
  {
    ReosDuration newValue = ReosDuration( value(), mUnitCombobox->currentUnit() );
    if ( newValue != durationParameter()->value() )
    {
      setTextValue( value() );
      durationParameter()->setValue( newValue );
    }
  }
}

ReosParameterDuration *ReosParameterDurationWidget::durationParameter() const
{
  return static_cast<ReosParameterDuration *>( mParameter.data() );
}


ReosParameterDateTimeWidget::ReosParameterDateTimeWidget( QWidget *parent, const QString &defaultName ):
  ReosParameterWidget( defaultName, parent )
  , mDateTimeEdit( new QDateTimeEdit( this ) )
{
  layout()->addWidget( mDateTimeEdit );
  connect( mDateTimeEdit, &QDateTimeEdit::dateTimeChanged, this, &ReosParameterWidget::applyValue );
  mDateTimeEdit->setDisplayFormat( QLocale().dateTimeFormat( QLocale::ShortFormat ) );
  mDateTimeEdit->setTimeSpec( Qt::UTC );
  finalizeWidget();
}

ReosParameterDateTimeWidget::ReosParameterDateTimeWidget( ReosParameterDateTime *dateTime, QWidget *parent ):
  ReosParameterDateTimeWidget( parent, dateTime ? dateTime->name() : QString() )
{
  setDateTime( dateTime );
}

void ReosParameterDateTimeWidget::setDateTime( ReosParameterDateTime *dateTime )
{
  setParameter( dateTime );
  updateValue();
}

void ReosParameterDateTimeWidget::updateValue()
{
  if ( dateTimeParameter() && dateTimeParameter()->isValid() )
  {
    if ( mDateTimeEdit )
      mDateTimeEdit->setDateTime( dateTimeParameter()->value() );
    show();
  }
  else
  {
    if ( mDateTimeEdit )
      mDateTimeEdit->setDateTime( QDateTime( QDate( QDate::currentDate().year(), 1, 1 ), QTime( 0, 0, 0 ), Qt::UTC ) );
    show();
  }

  if ( mHideWhenVoid && !dateTimeParameter() )
    hide();
}

void ReosParameterDateTimeWidget::applyValue()
{
  if ( dateTimeParameter() &&
       mDateTimeEdit &&
       dateTimeParameter()->value() != mDateTimeEdit->dateTime() )
    dateTimeParameter()->setValue( mDateTimeEdit->dateTime() );
}

void ReosParameterDateTimeWidget::setFocusOnEdit()
{
  mDateTimeEdit->setFocus();
}

ReosParameterDateTime *ReosParameterDateTimeWidget::dateTimeParameter() const
{
  return static_cast<ReosParameterDateTime *>( mParameter.data() );
}

ReosParameterBooleanWidget::ReosParameterBooleanWidget( QWidget *parent, const QString &defaultName ):
  ReosParameterWidget( defaultName, parent )
  , mCheckBox( new QCheckBox( this ) )
{
  layout()->addWidget( mCheckBox );
  connect( mCheckBox, &QCheckBox::stateChanged, this, &ReosParameterWidget::applyValue );
  finalizeWidget();
}

ReosParameterBooleanWidget::ReosParameterBooleanWidget( ReosParameterBoolean *booleanParameter, QWidget *parent ):
  ReosParameterBooleanWidget( parent, booleanParameter ? booleanParameter->name() : QString() )
{
  setBooleanParameter( booleanParameter );
}

void ReosParameterBooleanWidget::setBooleanParameter( ReosParameterBoolean *boolean )
{
  setParameter( boolean );
}

void ReosParameterBooleanWidget::updateValue()
{
  if ( booleanParameter() && booleanParameter()->isValid() )
  {
    if ( mCheckBox )
    {
      mCheckBox->setEnabled( true );
      mCheckBox->setChecked( booleanParameter()->value() );
    }
    show();
  }
  else
  {
    if ( mCheckBox )
      mCheckBox->setEnabled( false );
    show();
  }

  if ( mHideWhenVoid && !booleanParameter() )
    hide();
}

void ReosParameterBooleanWidget::applyValue()
{
  if ( booleanParameter() && mCheckBox && booleanParameter()->value() != mCheckBox->isChecked() )
    booleanParameter()->setValue( mCheckBox->isChecked() );
}

void ReosParameterBooleanWidget::setFocusOnEdit()
{
  mCheckBox->setFocus();
}

ReosParameterBoolean *ReosParameterBooleanWidget::booleanParameter() const
{
  return static_cast<ReosParameterBoolean *>( mParameter.data() );
}

ReosDurationUnitComboBox::ReosDurationUnitComboBox( QWidget *parent, ReosDuration::Unit timeUnit ):
  QComboBox( parent )
{
  addItem( tr( "millisecond" ), ReosDuration::millisecond );
  addItem( tr( "second" ), ReosDuration::second );
  addItem( tr( "minute" ), ReosDuration::minute );
  addItem( tr( "hour" ), ReosDuration::hour );
  addItem( tr( "day" ), ReosDuration::day );
  addItem( tr( "week" ), ReosDuration::week );
  addItem( tr( "month" ), ReosDuration::month );
  addItem( tr( "year" ), ReosDuration::year );;

  setCurrentUnit( timeUnit );
}

ReosDuration::Unit ReosDurationUnitComboBox::currentUnit() const
{
  return static_cast<ReosDuration::Unit>( currentData().toInt() );
}

void ReosDurationUnitComboBox::setCurrentUnit( ReosDuration::Unit unit ) { setCurrentIndex( findData( unit ) );}

ReosParameterLongStringWidget::ReosParameterLongStringWidget( QWidget *parent, const QString &defaultName ):
  ReosParameterWidget( defaultName, parent, Qt::Vertical ),
  mTextEdit( new ReosParameterTextEdit( this ) )
{
  layout()->addWidget( mTextEdit );
  mTextEdit->setMaximumHeight( 30 );
  connect( mTextEdit, &ReosParameterTextEdit::editingFinished, this, &ReosParameterWidget::applyValue );
  finalizeWidget();
}

ReosParameterLongStringWidget::ReosParameterLongStringWidget( ReosParameterLongString *longStringParameter, QWidget *parent ):
  ReosParameterLongStringWidget( parent, longStringParameter ? longStringParameter->name() : QString() )
{
  setParameter( longStringParameter );
}

void ReosParameterLongStringWidget::setString( ReosParameterLongString *string )
{
  setParameter( string );
}

void ReosParameterLongStringWidget::updateValue()
{
  if ( stringParameter() && stringParameter()->isValid() )
  {
    if ( mTextEdit )
      mTextEdit->setText( stringParameter()->value() );
    show();
  }
  else
  {
    if ( mTextEdit )
      mTextEdit->setText( QStringLiteral( "-" ) );
    show();
  }

  if ( mHideWhenVoid && !stringParameter() )
    hide();
}

void ReosParameterLongStringWidget::applyValue()
{
  if ( stringParameter() && mTextEdit && stringParameter()->value() != mTextEdit->toPlainText() )
    stringParameter()->setValue( mTextEdit->toPlainText() );
}

void ReosParameterLongStringWidget::setFocusOnEdit()
{
  mTextEdit->setFocus();
}

ReosParameterLongString *ReosParameterLongStringWidget::stringParameter() const
{
  return static_cast<ReosParameterLongString *>( mParameter.data() );
}

ReosParameterTextEdit::ReosParameterTextEdit( QWidget *parent ): QTextEdit( parent ) {}

void ReosParameterTextEdit::focusOutEvent( QFocusEvent *event )
{
  emit editingFinished();
  QWidget::focusOutEvent( event );
}

ReosParameterIntegerWidget::ReosParameterIntegerWidget( QWidget *parent, const QString &defaultName ):
  ReosParameterInLineWidget( parent, defaultName )
{
  finalizeWidget();
}

ReosParameterIntegerWidget::ReosParameterIntegerWidget( ReosParameterInteger *value, QWidget *parent ):
  ReosParameterIntegerWidget( parent, value ? value->name() : QString() )
{
  setInteger( value );
}

void ReosParameterIntegerWidget::setInteger( ReosParameterInteger *value )
{
  setParameter( value );
  updateValue();
}

void ReosParameterIntegerWidget::updateValue()
{
  if ( integerParameter() && integerParameter()->isValid() )
  {
    setTextValue( integerParameter()->toString() );
    show();
  }
  else
  {
    setTextValue( QString( '-' ) );
    show();
  }

  if ( mHideWhenVoid && !integerParameter() )
    hide();
}

void ReosParameterIntegerWidget::applyValue()
{
  if ( textHasChanged() && integerParameter() && value() != integerParameter()->value() )
  {
    bool ok = false;
    double v = textValue().toInt( &ok );
    if ( ok )
      integerParameter()->setValue( v );
    else
      integerParameter()->setInvalid();
  }
}

ReosParameterInteger *ReosParameterIntegerWidget::integerParameter()
{
  if ( mParameter )
    return static_cast<ReosParameterInteger *>( mParameter.data() );

  return nullptr;
}
