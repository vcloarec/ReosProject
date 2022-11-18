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

#include <qgscolorrampshaderwidget.h>
#include <qgsmeshlayer.h>
#include <qgsmeshrenderersettings.h>

#include "reosstyleregistery.h"

static QgsMeshRendererScalarSettings getScalarSettings( ReosMesh *mesh, const QString &datasetId )
{
  ReosEncodedElement datasetSymbology = mesh->datasetScalarGroupSymbology( datasetId );
  if ( datasetSymbology.description() == QStringLiteral( "dataset-symbology" ) )
  {
    QString docString;
    datasetSymbology.getData( QStringLiteral( "symbology" ), docString );

    QDomDocument doc( QStringLiteral( "dataset-symbology" ) );

    if ( doc.setContent( docString ) )
    {
      QDomElement domElem = doc.firstChildElement( QStringLiteral( "scalar-settings" ) );
      QgsReadWriteContext context;
      QgsMeshRendererScalarSettings scalarSettings;
      scalarSettings.readXml( domElem );

      return scalarSettings;
    }
  }
  return QgsMeshRendererScalarSettings();
}

void setScalarSettings( ReosMesh *mesh, const QString &datasetId, const QgsMeshRendererScalarSettings &scalarSettings )
{
  QDomDocument doc( QStringLiteral( "dataset-symbology" ) );
  doc.appendChild( scalarSettings.writeXml( doc ) ) ;

  ReosEncodedElement encodedElem( QStringLiteral( "dataset-symbology" ) );
  QString docString = doc.toString();
  encodedElem.addData( QStringLiteral( "symbology" ), docString );
  mesh->setDatasetScalarGroupSymbology( encodedElem, datasetId );
}


static QgsMeshRendererVectorSettings getVectorSettings( ReosMesh *mesh, const QString &datasetId )
{
  const ReosEncodedElement &encodedSymbology( mesh->datasetVectorGroupSymbology( datasetId ) );
  QString docString;
  encodedSymbology.getData( QStringLiteral( "symbology" ), docString );
  QDomDocument docFrom( QStringLiteral( "dataset-vector-symbology" ) );

  QgsMeshRendererVectorSettings vectorSettings;
  if ( docFrom.setContent( docString ) )
  {
    QDomElement domElem = docFrom.firstChildElement( QStringLiteral( "vector-settings" ) );
    QgsReadWriteContext context;
    vectorSettings.readXml( domElem );
    return vectorSettings;
  }

  return QgsMeshRendererVectorSettings();
}

void setVectorSettings( ReosMesh *mesh, const QString &datasetId, const QgsMeshRendererVectorSettings &vectorSettings )
{
  QDomDocument docTo( QStringLiteral( "dataset-vector-symbology" ) );
  docTo.appendChild( vectorSettings.writeXml( docTo ) ) ;

  ReosEncodedElement encodedElem( QStringLiteral( "dataset-vector-symbology" ) );
  QString docStringTo = docTo.toString();
  encodedElem.addData( QStringLiteral( "symbology" ), docStringTo );
  mesh->setDatasetVectorGroupSymbology( encodedElem, datasetId );
}


ReosMeshScalarRenderingWidget::ReosMeshScalarRenderingWidget( ReosMesh *mesh, const QString &datasetId, bool isScalar, const ReosGuiContext &guiContext )
  : ReosStackedPageWidget( guiContext.parent() )
  , ui( new Ui::ReosMeshScalarRenderingWidget )
  , mMesh( mesh )
  , mDatasetId( datasetId )
  , mIsScalar( isScalar )
  , mMinimumParam( new ReosParameterDouble( tr( "Minimum" ), false, this ) )
  , mMaximumParam( new ReosParameterDouble( tr( "Maximum" ), false, this ) )
{
  ui->setupUi( this );

  ui->mParameterMin->setDouble( mMinimumParam );
  ui->mParameterMax->setDouble( mMaximumParam );

  setLayout( new QHBoxLayout );

  mColorRampShaderWidget = new QgsColorRampShaderWidget( this );
  ui->mColorRampShaderLayout->addWidget( mColorRampShaderWidget );

  if ( mIsScalar )
  {
    QgsMeshRendererScalarSettings scalarSettings = getScalarSettings( mMesh, mDatasetId );
    mColorRampShaderWidget->setFromShader( scalarSettings.colorRampShader() );
    mMinimumParam->setValue( scalarSettings.classificationMinimum() );
    mMaximumParam->setValue( scalarSettings.classificationMaximum() );
    mColorRampShaderWidget->setMinimumMaximum( mMinimumParam->value(), mMaximumParam->value() );
    ui->mOpacitySlider->setValue( scalarSettings.opacity() * 100 );

  }
  else
  {

    QgsMeshRendererVectorSettings vectorSettings = getVectorSettings( mMesh, mDatasetId );

    mColorRampShaderWidget->setFromShader( vectorSettings.colorRampShader() );
    mMinimumParam->setValue( vectorSettings.colorRampShader().minimumValue() );
    mMaximumParam->setValue( vectorSettings.colorRampShader().maximumValue() );
    mColorRampShaderWidget->setMinimumMaximum( mMinimumParam->value(), mMaximumParam->value() );
    ui->mOpacitySlider->setVisible( false );
    ui->mOpacityLabel->setVisible( false );
    ui->mOpacitySpinBox->setVisible( false );
  }

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
    updateMeshSettings();
  } );

  connect( ui->mOpacitySlider, &QSlider::valueChanged, this, [this]( int value )
  {
    ui->mOpacitySpinBox->blockSignals( true );
    ui->mOpacitySpinBox->setValue( value );
    ui->mOpacitySpinBox->blockSignals( false );
    updateMeshSettings();
  } );

  connect( ui->mReloadButton, &QToolButton::clicked, this, [this]
  {
    double min = 0;
    double max = 0;

    mMesh->datasetGroupMinimumMaximum( mDatasetId, min, max );
    if ( min < max )
    {
      mMinimumParam->setValue( min );
      mMaximumParam->setValue( max );
    }
  } );

}

ReosMeshScalarRenderingWidget::~ReosMeshScalarRenderingWidget()
{
  delete ui;
}

void ReosMeshScalarRenderingWidget::onMinMaxChanged()
{
  mColorRampShaderWidget->setMinimumMaximumAndClassify( mMinimumParam->value(), mMaximumParam->value() );
}

void ReosMeshScalarRenderingWidget::onColorRampChanged()
{
  updateMeshSettings();
}

void ReosMeshScalarRenderingWidget::updateMeshSettings()
{
  if ( mIsScalar )
  {
    QgsMeshRendererScalarSettings scalarSettings = getScalarSettings( mMesh, mDatasetId );
    scalarSettings.setClassificationMinimumMaximum( mMinimumParam->value(), mMaximumParam->value() );
    scalarSettings.setColorRampShader( mColorRampShaderWidget->shader() );
    scalarSettings.setOpacity( ui->mOpacitySpinBox->value() / 100.0 );

    setScalarSettings( mMesh, mDatasetId, scalarSettings );
  }
  else
  {
    QgsMeshRendererVectorSettings vectorSettings = getVectorSettings( mMesh, mDatasetId );
    vectorSettings.setColorRampShader( mColorRampShaderWidget->shader() );

    setVectorSettings( mMesh, mDatasetId, vectorSettings );
  }
}
