/***************************************************************************
  reos3dterrainsettingswidget.cpp - Reos3DTerrainSettingsWidget

 ---------------------
 begin                : 15.3.2022
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
#include "reos3dterrainsettingswidget.h"
#include "ui_reos3dterrainsettingswidget.h"

#include "reos3dmapsettings.h"

Reos3DTerrainSettingsWidget::Reos3DTerrainSettingsWidget( QWidget *parent ) :
  QWidget( parent ),
  ui( new Ui::Reos3DTerrainSettingsWidget )
{
  ui->setupUi( this );

  ui->mRenderingTypeCombo->addItem( tr( "Use unique color" ), int( Reos3DTerrainSettings::UniqueColor ) );
  ui->mRenderingTypeCombo->addItem( tr( "Use terrain color ramp" ), int( Reos3DTerrainSettings::ColorRamp ) );

  connect( ui->mTerrainColorButton, &ReosColorButton::colorChanged, this, &Reos3DTerrainSettingsWidget::terrainSettingsChanged );
  connect( ui->mRenderingTypeCombo,  QOverload<int>::of( &QComboBox::currentIndexChanged ), this, &Reos3DTerrainSettingsWidget::terrainSettingsChanged );
  connect( ui->mWireframeColorButton, &ReosColorButton::colorChanged, this, &Reos3DTerrainSettingsWidget::terrainSettingsChanged );
  connect( ui->mWireframeCheckBox, &QCheckBox::stateChanged, this, &Reos3DTerrainSettingsWidget::terrainSettingsChanged );
  connect( ui->mWireframeWidthSlider, &QSlider::valueChanged, this, &Reos3DTerrainSettingsWidget::terrainSettingsChanged );
  connect( ui->mSmoothedCheckBox, &QCheckBox::stateChanged, this, &Reos3DTerrainSettingsWidget::terrainSettingsChanged );

  ui->mTerrainColorButton->setEnabled( ui->mRenderingTypeCombo->currentData().toInt() == Reos3DTerrainSettings::UniqueColor );
  connect( ui->mRenderingTypeCombo,  QOverload<int>::of( &QComboBox::currentIndexChanged ), this, [this]
  {
    ui->mTerrainColorButton->setEnabled( ui->mRenderingTypeCombo->currentData().toInt() == Reos3DTerrainSettings::UniqueColor );
  } );
  connect( ui->mWireframeCheckBox, &QCheckBox::stateChanged, ui->mWireframeWidthSlider,  &QSlider::setEnabled );
  connect( ui->mWireframeCheckBox, &QCheckBox::stateChanged, ui->mWireframeColorButton,  &QSlider::setEnabled );
}

Reos3DTerrainSettingsWidget::~Reos3DTerrainSettingsWidget()
{
  delete ui;
}

void Reos3DTerrainSettingsWidget::setTerrainSettings( const Reos3DTerrainSettings &settings )
{
  blockSignals( true );
  ui->mTerrainColorButton->setColor( settings.uniqueColor() );
  ui->mRenderingTypeCombo->setCurrentIndex( ui->mRenderingTypeCombo->findData( settings.renderingType() ) );
  ui->mWireframeCheckBox->setChecked( settings.isWireframeEnabled() );
  ui->mWireframeColorButton->setColor( settings.wireframeColor() );
  ui->mWireframeWidthSlider->setValue( settings.wireframeWidth() * 10 );
  ui->mSmoothedCheckBox->setChecked( settings.isSmoothed() );
  blockSignals( false );
}

Reos3DTerrainSettings Reos3DTerrainSettingsWidget::settings() const
{
  Reos3DTerrainSettings settings;

  settings.setRenderingType( static_cast<Reos3DTerrainSettings::RenderingType>( ui->mRenderingTypeCombo->currentData().toInt() ) );
  settings.setUniqueColor( ui->mTerrainColorButton->color() );
  settings.setIsWireframeEnabled( ui->mWireframeCheckBox->isChecked() );
  settings.setWireframeWidth( ui->mWireframeWidthSlider->value() / 10.0 );
  settings.setWireframeColor( ui->mWireframeColorButton->color() );
  settings.setIsSmoothed( ui->mSmoothedCheckBox->isChecked() );

  return settings;
}

