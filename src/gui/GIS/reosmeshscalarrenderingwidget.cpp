/***************************************************************************
  reosmeshscalarrenderingwidget.cpp - ReosMeshScalarRenderingWidget

 ---------------------
 begin                : 24.2.2022
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
#include "reosmeshscalarrenderingwidget.h"
#include "ui_reosmeshscalarrenderingwidget.h"
#include <QHBoxLayout>
#include <QMessageBox>

#include <qgscolorrampshaderwidget.h>
#include <qgsmeshlayer.h>
#include <qgsmeshrenderersettings.h>

#include "reosstyleregistery.h"
#include "reosrenderersettings.h"

ReosMeshScalarRenderingWidget::ReosMeshScalarRenderingWidget( ReosColorShaderSettings *settings, const ReosGuiContext &guiContext )
  : ReosStackedPageWidget( guiContext.parent() )
  , ui( new Ui::ReosMeshScalarRenderingWidget )
  , mSettings( settings )
  , mMinimumParam( new ReosParameterDouble( tr( "Minimum" ), false, this ) )
  , mMaximumParam( new ReosParameterDouble( tr( "Maximum" ), false, this ) )
{
  ui->setupUi( this );

  ui->mParameterMin->setDouble( mMinimumParam );
  ui->mParameterMax->setDouble( mMaximumParam );

  setLayout( new QHBoxLayout );

  mColorRampShaderWidget = new QgsColorRampShaderWidget( this );
  ui->mColorRampShaderLayout->addWidget( mColorRampShaderWidget );

  syncSettings();

  connect( mMinimumParam, &ReosParameter::valueChanged, this, &ReosMeshScalarRenderingWidget::onMinMaxChanged );
  connect( mMaximumParam, &ReosParameter::valueChanged, this, &ReosMeshScalarRenderingWidget::onMinMaxChanged );

  ui->mBackButton->setIconSize( ReosStyleRegistery::instance()->toolBarIconSize() );
  connect( mColorRampShaderWidget, &QgsColorRampShaderWidget::widgetChanged, this, &ReosMeshScalarRenderingWidget::onColorRampChanged );
  connect( ui->mBackButton, &QPushButton::clicked, this, &ReosStackedPageWidget::backToPreviousPage );

  connect( ui->mOpacitySpinBox, QOverload<int>::of( &QSpinBox::valueChanged ), this, [this]( int value )
  {
    ui->mOpacitySlider->blockSignals( true );
    ui->mOpacitySlider->setValue( value );
    ui->mOpacitySlider->blockSignals( false );
    updateSettings();
  } );

  connect( ui->mOpacitySlider, &QSlider::valueChanged, this, [this]( int value )
  {
    ui->mOpacitySpinBox->blockSignals( true );
    ui->mOpacitySpinBox->setValue( value );
    ui->mOpacitySpinBox->blockSignals( false );
    updateSettings();
  } );

  connect( ui->mReloadButton, &QToolButton::clicked, this, [this]
  {
    double min = 0;
    double max = 0;

    if ( !mSettings.isNull() )
    {
      if ( !mSettings->getDirectSourceMinMax( min, max ) )
      {
        if ( QMessageBox::warning( this,
                                   tr( "Calculate Minimum and Maximum" ),
                                   tr( "Data source does not provide minimum and maximum values.\n"
                                       "To obtain minimum and maximum, it is necessary to calculate them from source.\n"
                                       "Depending of the data, this operation could take some time.\n\n"
                                       "Do you want to proceed?" ), QMessageBox::Yes | QMessageBox::No, QMessageBox::Yes )
             == QMessageBox::No )
          return;

        mSettings->calculateSourceMinMax( min, max );
      }
    }
    if ( min < max )
    {
      mMinimumParam->setValue( min );
      mMaximumParam->setValue( max );
    }
  } );

  connect( mSettings, &ReosColorShaderSettings::settingsChangedFromObject, this, &ReosMeshScalarRenderingWidget::syncSettings );
}

ReosMeshScalarRenderingWidget::~ReosMeshScalarRenderingWidget()
{
  delete ui;
}

void ReosMeshScalarRenderingWidget::hideBackButton()
{
  ui->mBackButton->setVisible( false );
}

void ReosMeshScalarRenderingWidget::onMinMaxChanged()
{
  mColorRampShaderWidget->setMinimumMaximumAndClassify( mMinimumParam->value(), mMaximumParam->value() );
}

void ReosMeshScalarRenderingWidget::onColorRampChanged()
{
  updateSettings();
}

void ReosMeshScalarRenderingWidget::syncSettings()
{
  if ( !mSettings.isNull() )
  {
    mMinimumParam->blockSignals( true );
    mMinimumParam->setValue( mSettings->classificationMinimum() );
    ui->mParameterMin->updateValue();
    mMinimumParam->blockSignals( false );

    mMaximumParam->blockSignals( true );
    mMaximumParam->setValue( mSettings->classificationMaximum() );
    ui->mParameterMax->updateValue();
    mMaximumParam->blockSignals( false );

    mColorRampShaderWidget->blockSignals( true );
    QgsColorRampShader shader;
    mSettings->getShader( &shader );
    mColorRampShaderWidget->setFromShader( shader );
    mColorRampShaderWidget->setMinimumMaximum( mMinimumParam->value(), mMaximumParam->value() );
    mColorRampShaderWidget->blockSignals( false );

    ui->mOpacitySlider->blockSignals( true );
    ui->mOpacitySlider->setValue( mSettings->opacity() * 100 );
    ui->mOpacitySpinBox->setValue( mSettings->opacity() * 100 );
    ui->mOpacitySlider->blockSignals( false );

    bool opacityAvailable = mSettings->opacity() >= 0;
    ui->mOpacitySlider->setVisible( opacityAvailable );
    ui->mOpacitySpinBox->setVisible( opacityAvailable );
    ui->mOpacityLabel->setVisible( opacityAvailable );
  }
}

void ReosMeshScalarRenderingWidget::updateSettings()
{
  if ( !mSettings.isNull() )
  {
    mSettings->setClassificationMinimum( mMinimumParam->value() );
    mSettings->setClassificationMaximum( mMaximumParam->value() );
    mSettings->setOpacity( ui->mOpacitySpinBox->value() / 100.0 );
    QgsColorRampShader shader = mColorRampShaderWidget->shader();
    mSettings->setShader( &shader );
    mSettings->onSettingsUpdated();
  }
}
