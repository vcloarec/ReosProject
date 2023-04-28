/***************************************************************************
  reoshydraulicstructure2dtimewindowwidget.cpp - Reoshydraulicstructure2dTimeWindowWidget

 ---------------------
 begin                : 2.1.2023
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
#include "reoshydraulicstructure2dtimewindowwidget.h"
#include "ui_reoshydraulicstructure2dtimewindowwidget.h"

#include "reostimewindowsettings.h"

Reoshydraulicstructure2dTimeWindowWidget::Reoshydraulicstructure2dTimeWindowWidget( ReosTimeWindowSettings *timeWindowSettings, QWidget *parent )
  : ReosStackedPageWidget( parent )
  , ui( new Ui::Reoshydraulicstructure2dTimeWindowWidget )
  , mTimeWindowSettings( timeWindowSettings )
{
  ui->setupUi( this );

  ui->mStartOffset->setDuration( timeWindowSettings->startOffset() );
  ui->mStartOffset->hideLabel();
  ui->mEndOffset->setDuration( timeWindowSettings->endOffset() );
  ui->mEndOffset->hideLabel();

  ui->mComboBoxOriginStart->addItem( tr( "From beginning" ), ReosTimeWindowSettings::Begin );
  ui->mComboBoxOriginStart->addItem( tr( "From end" ), ReosTimeWindowSettings::End );
  ui->mComboBoxOriginStart->setCurrentIndex( ui->mComboBoxOriginStart->findData( timeWindowSettings->originStart() ) );
  ui->mComboBoxOriginEnd->addItem( tr( "From beginning" ), ReosTimeWindowSettings::Begin );
  ui->mComboBoxOriginEnd->addItem( tr( "From end" ), ReosTimeWindowSettings::End );
  ui->mComboBoxOriginEnd->setCurrentIndex( ui->mComboBoxOriginEnd->findData( timeWindowSettings->originEnd() ) );
  ui->mCombineCombo->addItem( tr( "Intersection" ), ReosTimeWindowSettings::Intersection );
  ui->mCombineCombo->addItem( tr( "Union" ), ReosTimeWindowSettings::Union );

  ui->mStartTime->setDateTime( timeWindowSettings->userStartTime() );
  ui->mEndTime->setDateTime( timeWindowSettings->userEndTime() );

  ui->mGroupBoxAuto->setChecked( mTimeWindowSettings->automaticallyDefined()->value() );
  ui->mExternallyCheckBox->setChecked( mTimeWindowSettings->useExternalDefinedTimeWindow()->value() );
  syncTimeWindowSettings();
  connect( ui->mExternallyCheckBox, &QCheckBox::toggled, this, &Reoshydraulicstructure2dTimeWindowWidget::onExternallyCheckBoxToogle );
  connect( ui->mGroupBoxAuto, &QGroupBox::toggled, this, &Reoshydraulicstructure2dTimeWindowWidget::onAutomaticGroupBoxToggle );
  connect( ui->mCombineCombo, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, &Reoshydraulicstructure2dTimeWindowWidget::onCombineMethodChange );
  connect( ui->mComboBoxOriginStart, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, &Reoshydraulicstructure2dTimeWindowWidget::onOriginChange );
  connect( ui->mComboBoxOriginEnd, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, &Reoshydraulicstructure2dTimeWindowWidget::onOriginChange );
  connect( mTimeWindowSettings, &ReosDataObject::dataChanged, this, &Reoshydraulicstructure2dTimeWindowWidget::syncTimeWindowSettings );
}

Reoshydraulicstructure2dTimeWindowWidget::~Reoshydraulicstructure2dTimeWindowWidget()
{
  delete ui;
}

void Reoshydraulicstructure2dTimeWindowWidget::setExternallyDefinedEnable( bool enable )
{
  ui->mExternallyCheckBox->setVisible( enable );
  mExternalEnable = enable;
  syncTimeWindowSettings();
}

void Reoshydraulicstructure2dTimeWindowWidget::onExternallyCheckBoxToogle()
{
  bool checked = ui->mExternallyCheckBox->isChecked();
  mTimeWindowSettings->useExternalDefinedTimeWindow()->setValue( checked );
  ui->mGroupBoxAuto->setEnabled( !checked );
  ui->mStartTime->setEnabled( !checked || ( mExternalEnable && !ui->mExternallyCheckBox->isChecked() ) );
  ui->mEndTime->setEnabled( !checked || ( mExternalEnable && !ui->mExternallyCheckBox->isChecked() ) );
}

void Reoshydraulicstructure2dTimeWindowWidget::onAutomaticGroupBoxToggle()
{
  bool checked = ui->mGroupBoxAuto->isChecked();
  mTimeWindowSettings->automaticallyDefined()->setValue( checked );
  ui->mStartTime->setEnabled( !checked || ( mExternalEnable && !ui->mExternallyCheckBox->isChecked() ) );
  ui->mEndTime->setEnabled( !checked || ( mExternalEnable && !ui->mExternallyCheckBox->isChecked() ) );
}

void Reoshydraulicstructure2dTimeWindowWidget::onOriginChange()
{
  ReosTimeWindowSettings::OffsetOrigin ori = static_cast<ReosTimeWindowSettings::OffsetOrigin>( ui->mComboBoxOriginStart->currentData().toInt() );
  if ( ori != mTimeWindowSettings->originStart() )
    mTimeWindowSettings->setOriginStart( ori );

  ori = static_cast<ReosTimeWindowSettings::OffsetOrigin>( ui->mComboBoxOriginEnd->currentData().toInt() );
  if ( ori != mTimeWindowSettings->originEnd() )
    mTimeWindowSettings->setOriginEnd( ori );
}

void Reoshydraulicstructure2dTimeWindowWidget::onCombineMethodChange()
{
  ReosTimeWindowSettings::CombineMethod met = static_cast<ReosTimeWindowSettings::CombineMethod>( ui->mComboBoxOriginStart->currentData().toInt() );
  if ( met != mTimeWindowSettings->combineMethod() )
    mTimeWindowSettings->setCombineMethod( met );
}

void Reoshydraulicstructure2dTimeWindowWidget::syncTimeWindowSettings()
{
  bool externally = false;
  if ( mExternalEnable )
  {
    externally = mTimeWindowSettings->useExternalDefinedTimeWindow()->value();
    ui->mExternallyCheckBox->blockSignals( true );
    ui->mExternallyCheckBox->setChecked( externally );
    ui->mExternallyCheckBox->blockSignals( false );
  }

  ui->mGroupBoxAuto->setEnabled( !externally );

  bool autoChecked = mTimeWindowSettings->automaticallyDefined()->value();
  ui->mGroupBoxAuto->blockSignals( true );
  ui->mGroupBoxAuto->setChecked( autoChecked );
  ui->mGroupBoxAuto->blockSignals( false );
  ui->mStartTime->setEnabled( !autoChecked && !externally );
  ui->mEndTime->setEnabled( !autoChecked && !externally );

  ui->mComboBoxOriginStart->blockSignals( true );
  ui->mComboBoxOriginStart->setCurrentIndex( ui->mComboBoxOriginStart->findData( mTimeWindowSettings->originStart() ) );
  ui->mComboBoxOriginStart->blockSignals( false );

  ui->mComboBoxOriginEnd->blockSignals( true );
  ui->mComboBoxOriginEnd->setCurrentIndex( ui->mComboBoxOriginEnd->findData( mTimeWindowSettings->originEnd() ) );
  ui->mComboBoxOriginEnd->blockSignals( false );

  ui->mCombineCombo->blockSignals( true );
  ui->mCombineCombo->setCurrentIndex( ui->mComboBoxOriginEnd->findData( mTimeWindowSettings->combineMethod() ) );
  ui->mCombineCombo->blockSignals( false );
}
