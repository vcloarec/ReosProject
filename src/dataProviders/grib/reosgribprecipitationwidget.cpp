/***************************************************************************
  reosgribprecipitationwidget.cpp - ReosGribPrecipitationWidget

 ---------------------
 begin                : 16.11.2022
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
#include "reosgribprecipitationwidget.h"
#include "qdebug.h"
#include "ui_reosgribprecipitationwidget.h"

#include <QFileDialog>
#include "QDockWidget"

#include "reosmap.h"
#include "reosgisengine.h"
#include "reosgriddedrainitem.h"
#include "reossettings.h"


REOSEXTERN ReosDataProviderGuiFactory *providerGuiFactory()
{
  return new ReosGribGuiFactory();
}

ReosGribPrecipitationWidget::ReosGribPrecipitationWidget( QWidget *parent )
  : ReosDataProviderSelectorWidget( parent )
  , ui( new Ui::ReosGribPrecipitationWidget )
  ,  mProvider( new ReosGribGriddedRainfallProvider )
{
  ui->setupUi( this );

  mDataExtent.reset( new ReosMapPolygon( ui->mDataVizMap->map() ) );

  connect( ui->mPathToolButton, &QToolButton::clicked, this, &ReosGribPrecipitationWidget::onPathButtonClicked );
  connect( ui->mPathLineEdit, &QLineEdit::textEdited, this, &ReosGribPrecipitationWidget::onPathChanged );
  connect( ui->mVariablesCombo, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, &ReosGribPrecipitationWidget::updateDataOnMap );

  mDataExtent->setColor( Qt::red );
  mDataExtent->setStyle( Qt::DashLine );
  mDataExtent->setWidth( 3 );
  mDataExtent->setExternalColor( Qt::white );
  mDataExtent->setExternalWidth( 5 );

  ui->mValueTypeCombo->addItem( tr( "Intensity" ), static_cast<int>( ReosGriddedRainfallProvider::ValueType::Intensity ) );
  ui->mValueTypeCombo->addItem( tr( "Height during time step" ), static_cast<int>( ReosGriddedRainfallProvider::ValueType::Height ) );
  ui->mValueTypeCombo->addItem( tr( "Cumulative height from start" ), static_cast<int>( ReosGriddedRainfallProvider::ValueType::CumulativeHeight ) );
  connect( ui->mValueTypeCombo, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, &ReosGribPrecipitationWidget::updateDataOnMap );


  onPathChanged();
}

ReosGribPrecipitationWidget::~ReosGribPrecipitationWidget()
{
  delete ui;
}

QVariantMap ReosGribPrecipitationWidget::selectedMetadata() const
{
  QVariantMap ret;
  /**
   * Returns metadata of the data, must contains map following the Reos convention of the the datatype instead the provider convention:
   *
   * provider-key: provider key
   * station: if the data is associated with a station, the name of the station
   * station-description: a short text desciption of the station
   * x-coord: x coordinate (or longitude)
   * y-coord: y coordinate (or longitude)
   * crs: wkt coordinate system
   * description: a short text desciption of the data
   * start: start date/time of data if temporal
   * end: end date/time of data if temporal
   */

  ret.insert( QStringLiteral( "provider-key" ), GRIB_KEY );
  ret.insert( QStringLiteral( "data-type" ), ReosGriddedRainfall::staticType() );

  if ( mCurrentRainfall )
  {
    QPair<QDateTime, QDateTime> timeExtent = mCurrentRainfall->timeExtent();
    ret.insert( QStringLiteral( "start" ), timeExtent.first );
    ret.insert( QStringLiteral( "end" ), timeExtent.second );
  }

  return ret;
}

ReosDataObject *ReosGribPrecipitationWidget::createData( QObject *parent ) const
{
  std::unique_ptr<ReosGriddedRainfall> ret( new ReosGriddedRainfall( mCurrentDataUri, mProvider->key(), parent ) );
  ret->setName( giveName() );

  return ret.release();
}

ReosDataObject *ReosGribPrecipitationWidget::selectedData() const
{
  mCurrentRainfall->setName( giveName() );
  return mCurrentRainfall.get();
}

void ReosGribPrecipitationWidget::onPathButtonClicked()
{
  ReosSettings settings;
  QString path;
  if ( settings.contains( QStringLiteral( "Rainfall/fileDirectory" ) ) )
    path = settings.value( QStringLiteral( "Rainfall/fileDirectory" ) ).toString();

  QFileDialog *dial = new QFileDialog( this );

  dial->setDirectory( path );

  dial->setOptions( QFileDialog::DontUseNativeDialog );
  dial->setFileMode( QFileDialog::AnyFile );

  connect( dial, &QFileDialog::currentChanged, this, [dial]( const QString & str )
  {
    QStringList fileNames = dial->selectedFiles();
    if ( str.isEmpty() )
      return;
    QFileInfo info( str );
    if ( info.isFile() )
      dial->setFileMode( QFileDialog::ExistingFile );
    else if ( info.isDir() )
      dial->setFileMode( QFileDialog::Directory );

  } );

  dial->exec();

  QStringList fileNames = dial->selectedFiles();

  if ( !fileNames.isEmpty() )
  {
    settings.setValue( QStringLiteral( "Rainfall/fileDirectory" ), dial->directory().path() );
    ui->mPathLineEdit->setText( fileNames.first() );
    onPathChanged();
  }
}

void ReosGribPrecipitationWidget::onPathChanged()
{
  if ( ui->mVariablesCombo->currentIndex() != -1 )
    mCurrentVariable = ui->mVariablesCombo->currentText();

  ui->mVariablesCombo->blockSignals( true );
  ui->mVariablesCombo->clear();
  mCurrentSourceIsValid = false;

  ReosModule::Message message;
  mDetails = mProvider->details( ui->mPathLineEdit->text(), message );
  if ( message.type == ReosModule::Error && !ui->mPathLineEdit->text().isEmpty() )
  {
    ui->mNotificationButton->setMessage( message );
    ui->mNotificationButton->show();
  }

  if ( message.type == ReosModule::Simple || ui->mPathLineEdit->text().isEmpty() )
  {
    ui->mNotificationButton->setMessage( ReosModule::Message() );
    ui->mNotificationButton->hide();

    if ( message.type == ReosModule::Simple )
    {
      QPolygonF extent = mDetails.extent.toPolygon();
      mDataExtent->resetPolygon( ReosGisEngine::transformToCoordinates( mDetails.extent.crs(), extent, ui->mDataVizMap->map()->mapCrs() ) );
      ui->mDataVizMap->setExtent( mDetails.extent );

      ui->mVariablesCombo->addItems( mDetails.availableVariables );
      int currentIndex = ui->mVariablesCombo->findText( mCurrentVariable );
      if ( currentIndex >= 0 )
        ui->mVariablesCombo->setCurrentIndex( currentIndex );
      mCurrentSourceIsValid = true;
    }
  }
  ui->mVariablesCombo->blockSignals( false );
  updateDataOnMap();
  emit dataSelectionChanged( mCurrentSourceIsValid );
}

void ReosGribPrecipitationWidget::updateDataOnMap()
{
  QString source = ui->mPathLineEdit->text();
  QString variable = ui->mVariablesCombo->currentText();
  ReosGriddedRainfallProvider::ValueType valueType = static_cast<ReosGriddedRainfallProvider::ValueType>( ui->mValueTypeCombo->currentData().toInt() );

  QString candidateUri = ReosGribGriddedRainfallProvider::uri( source, variable, valueType );

  if ( mCurrentDataUri == candidateUri )
    return;

  mCurrentDataUri = candidateUri;
  ui->mDataVizMap->removeRenderedObject( mCurrentRainfall.get() );
  mCurrentRainfall.reset();

  if ( mCurrentSourceIsValid )
  {
    mCurrentRainfall.reset( new ReosGriddedRainfall( mCurrentDataUri, mProvider->key() ) );
    QPair<QDateTime, QDateTime> temporalRange = mCurrentRainfall->timeExtent();
    ReosDuration timeStep = mCurrentRainfall->minimumTimeStep();
    ui->mDataVizMap->setTimeExtent( temporalRange.first, temporalRange.second );
    ui->mDataVizMap->setTimeStep( timeStep );
    ui->mDataVizMap->addRenderedDataObject( mCurrentRainfall.get() );
  }
}

QString ReosGribPrecipitationWidget::giveName() const
{
  QString name = ui->mNameLineEdit->text();

  if ( name.isEmpty() )
  {
    auto baseName = []( const QString & string )->QString
    {
      QFileInfo fileInfo( string );
      return fileInfo.baseName();
    };
    const QStringList &files = mDetails.files;

    if ( files.count() == 1 )
      name = baseName( files.first() );
    else
    {
      QString fileName1 = baseName( files.at( 0 ) );
      QString commonPart;
      for ( int i = 0; i < files.count() - 1; ++i )
      {
        commonPart.clear();
        const QString fileName2 = baseName( files.at( i + 1 ) );
        int cp2 = 0;
        QChar c2;
        bool common = false;
        for ( int cp1 = 0; cp1 < fileName1.count(); cp1++ )
        {
          const QChar c1 = fileName1.at( cp1 );
          c2 = fileName2.at( cp2 );

          if ( common && c1 != c2 )
            break;

          while ( c1 != c2 && cp2 < fileName2.count() - 1 )
          {
            cp2++;
            c2 = fileName2.at( cp2 );
          }

          common = c1 == c2;
          if ( !common )
          {
            if ( c2 == fileName2.count() )
            {
              if ( c1 == fileName1.count() )
                break;
              else
                cp2 = 0;
            }
          }
          else
          {
            commonPart.append( c1 );
            cp2++;
          }
          if ( cp2 == fileName2.count() )
            break;
        }
        if ( !commonPart.isEmpty() )
          fileName1 = commonPart;
      }

      if ( fileName1.isEmpty() )
        name = ui->mPathLineEdit->text();
      else
        name = commonPart;
    }
  }

  return name;
}

ReosDataProviderGuiFactory::GuiCapabilities ReosGribGuiFactory::capabilities() const
{
  return GuiCapability::DataSelector;
}

QString ReosGribGuiFactory::key() const
{
  return GRIB_KEY;
}

ReosGribPrecipitationWidget *ReosGribGuiFactory::createProviderSelectorWidget( ReosMap *map, const QString &dataType, QWidget *parent ) const
{
  if ( dataType == ReosGribGriddedRainfallProvider::dataType() )
    return new ReosGribPrecipitationWidget( parent );

  return nullptr;
}

ReosDataProviderSettingsWidget *ReosGribGuiFactory::createProviderSettingsWidget( ReosDataProvider *provider, QWidget *parent ) const
{
  return nullptr;
}

QString ReosGribGuiFactory::dataType() const
{
  return ReosGribGriddedRainfallProvider::dataType();
}

QString ReosGribGuiFactory::displayText() const
{
  return QObject::tr( "GRIB2 format" );
}

