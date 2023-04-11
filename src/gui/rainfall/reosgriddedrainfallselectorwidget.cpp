/***************************************************************************
  reosgriddedrainfallselectorwidget.cpp - ReosGriddedRainfallSelectorWidget

 ---------------------
 begin                : 22.12.2022
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
#include "reosgriddedrainfallselectorwidget.h"
#include "ui_reosgriddedrainfallselectorwidget.h"

#include <QFileDialog>
#include <QDialogButtonBox>

#include "reosgisengine.h"
#include "reossettings.h"
#include "reosgriddedrainitem.h"
#include "reosmeshscalarrenderingwidget.h"
#include "reosrenderersettings.h"

ReosGriddedRainfallSelectorWidget::ReosGriddedRainfallSelectorWidget( const ReosGuiContext &guiContext )
  : ReosDataProviderSelectorWidget( guiContext.parent() )
  , ui( new Ui::ReosGriddedRainfallSelectorWidget )
  , mGuiContext( guiContext )
{
  ui->setupUi( this );

  ui->mShowExtentOnMainButton->setMap( guiContext.map() );

  connect( ui->mPathToolButton, &QToolButton::clicked, this, &ReosGriddedRainfallSelectorWidget::onPathButtonClicked );
  connect( ui->mPathLineEdit, &QLineEdit::textEdited, this, &ReosGriddedRainfallSelectorWidget::onPathChanged );
  connect( ui->mFileRadioButton, &QRadioButton::toggled, this, &ReosGriddedRainfallSelectorWidget::onFileRemoteChanged );

  QObject::connect( ui->mToolButtonColorRamp, &QToolButton::clicked, this, [this]
  {
    if ( mCurrentRainfall )
    {
      QDialog *dial = new QDialog( this );
      dial->setLayout( new QVBoxLayout );
      ReosMeshScalarRenderingWidget *colorShaderWidget = new ReosMeshScalarRenderingWidget( mCurrentRainfall->colorSetting(), ReosGuiContext( mGuiContext, this ) );
      colorShaderWidget->hideBackButton();
      dial->layout()->addWidget( colorShaderWidget );
      QDialogButtonBox *buttonBox = new QDialogButtonBox( QDialogButtonBox::Close, dial );
      dial->layout()->addWidget( buttonBox );
      QObject::connect( buttonBox, &QDialogButtonBox::rejected, dial, &QDialog::close );
      dial->exec();
    }
  } );

  const QStringList remoteProviders = ReosDataProviderRegistery::instance()->withCapabilities( ReosGriddedRainfall::staticType(), ReosDataProvider::Net );

  for ( const QString &remoteKey : remoteProviders )
  {
    const QString text = ReosDataProviderGuiRegistery::instance()->providerDisplayText( remoteKey );
    ui->mRemoteCombobox->addItem( text, remoteKey );
  }

  onPathChanged();
  onFileRemoteChanged();
  onRemoteSourceChanged();
}

ReosGriddedRainfallSelectorWidget::~ReosGriddedRainfallSelectorWidget()
{
  ui->mDataVizMap->map()->deactivateCurrentTool();
  delete ui;
}

ReosDataObject *ReosGriddedRainfallSelectorWidget::createData( QObject *parent ) const
{
  std::unique_ptr< ReosGriddedRainfall> ret;

  if ( mProviderSelectorWidget )
    ret.reset( mProviderSelectorWidget->createData( parent ) );
  else
    ret.reset( new ReosGriddedRainfall( ui->mPathLineEdit->text(), mProvider->key(), parent ) );

  if ( mCurrentRainfall )
    ret->setColorSetting( mCurrentRainfall->colorSetting()->clone() );

  ret->setName( ui->mNameLineEdit->text() );
  return ret.release();
}

QVariantMap ReosGriddedRainfallSelectorWidget::selectedMetadata() const
{
  if ( mProviderSelectorWidget )
    return mProviderSelectorWidget->selectedMetadata();
  else
  {
    QVariantMap ret;
    ret.insert( QStringLiteral( "provider-key" ), mProvider->key() );
    ret.insert( QStringLiteral( "data-type" ), ReosGriddedRainfall::staticType() );

    if ( mCurrentRainfall )
    {
      QPair<QDateTime, QDateTime> timeExtent = mCurrentRainfall->timeExtent();
      ret.insert( QStringLiteral( "start" ), timeExtent.first );
      ret.insert( QStringLiteral( "end" ), timeExtent.second );
    }

    return ret;
  }
}

void ReosGriddedRainfallSelectorWidget::onPathButtonClicked()
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

  if ( dial->exec() )
  {
    QStringList fileNames = dial->selectedFiles();
    if ( !fileNames.isEmpty() )
    {
      settings.setValue( QStringLiteral( "Rainfall/fileDirectory" ), dial->directory().path() );
      ui->mPathLineEdit->setText( fileNames.first() );
      onPathChanged();
    }
  }
}

void ReosGriddedRainfallSelectorWidget::onPathChanged()
{
  if ( !ui->mFileRadioButton->isChecked() )
    return;

  ui->mNotificationButton->setVisible( false );
  mCurrentSourceIsValid = false;

  ReosModule::Message message;
  ui->mNotificationButton->setMessage( message );

  if ( mProviderSelectorWidget )
  {
    delete mProviderSelectorWidget;
    mProviderSelectorWidget = nullptr;
  }

  std::unique_ptr<ReosDataProvider> provider(
    ReosDataProviderRegistery::instance()->createCompatibleProvider( ui->mPathLineEdit->text(), ReosGriddedRainfall::staticType() ) );

  mProvider.reset( qobject_cast<ReosGriddedRainfallProvider *>( provider.release() ) );

  if ( !mProvider )
  {
    if ( !ui->mPathLineEdit->text().isEmpty() )
    {
      if ( message.type != ReosModule::Error )
      {
        message.type = ReosModule::Error;
        message.text = tr( "Unable to find a gridded precipitation with this file path." );
      }
      ui->mNotificationButton->setMessage( message );
      if ( message.type == ReosModule::Error )
        ui->mNotificationButton->show();
    }
    updateRainfall();
    return;
  }

  const QString providerKey = mProvider->key();

  if ( ReosDataProviderGuiRegistery::instance()->hasCapability( providerKey, ReosDataProviderGuiFactory::GuiCapability::DataSelector ) )
  {

    mProviderSelectorWidget =
      qobject_cast<ReosGriddedRainDataProviderSelectorWidget *>(
        ReosDataProviderGuiRegistery::instance()->createProviderSelectorWidget(
          mProvider->key(),
          ReosGriddedRainfall::staticType(),
          ui->mDataVizMap->map(),
          this ) );
  }

  if ( mProviderSelectorWidget )
  {
    mDetails = mProviderSelectorWidget->setSource( ui->mPathLineEdit->text(), message );
    connect( mProviderSelectorWidget, &ReosGriddedRainDataProviderSelectorWidget::dataSelectionChanged,
             this, &ReosGriddedRainfallSelectorWidget::updateRainfall );
    ui->mProviderLayout->addWidget( mProviderSelectorWidget );
  }
  else
  {
    mDetails = mProvider->details( ui->mPathLineEdit->text(), message );
  }

  if ( message.type == ReosModule::Error && !ui->mPathLineEdit->text().isEmpty() )
  {
    ui->mNotificationButton->setMessage( message );
    ui->mNotificationButton->show();
  }

  if ( message.type == ReosModule::Simple || ui->mPathLineEdit->text().isEmpty() )
  {
    ui->mNotificationButton->setMessage( ReosModule::Message() );
    ui->mNotificationButton->hide();
    mCurrentSourceIsValid = message.type == ReosModule::Simple;
  }

  updateRainfall();
  emit dataSelectionChanged( mCurrentSourceIsValid );

  ui->mNameLineEdit->setText( giveName() );
}

void ReosGriddedRainfallSelectorWidget::updateRainfall()
{
  ui->mDataVizMap->removeAllRenderedObjects();
  ui->mDataVizMap->hideExtentOnMap();

  std::unique_ptr<ReosColorShaderSettings> colorSettings;
  if ( mCurrentRainfall )
    colorSettings.reset( mCurrentRainfall->colorSetting()->clone() );;

  mCurrentRainfall.reset();

  if ( mProviderSelectorWidget )
    mCurrentRainfall.reset( qobject_cast<ReosGriddedRainfall *>( mProviderSelectorWidget->createData() ) );
  else if ( mProvider )
    mCurrentRainfall.reset( new ReosGriddedRainfall( ui->mPathLineEdit->text(), mProvider->key() ) );

  if ( mCurrentRainfall && colorSettings )
    mCurrentRainfall->setColorSetting( colorSettings.release() );

  if ( mCurrentRainfall )
  {
    connect( mCurrentRainfall.get(), &ReosDataObject::dataReset, this, &ReosGriddedRainfallSelectorWidget::updateDataOnMap );
  }

  updateDataOnMap();
}

void ReosGriddedRainfallSelectorWidget::updateDataOnMap()
{
  if ( mCurrentRainfall )
  {
    QPair<QDateTime, QDateTime> temporalRange = mCurrentRainfall->timeExtent();
    ReosDuration timeStep = mCurrentRainfall->minimumTimeStep();
    ui->mDataVizMap->setTimeExtent( temporalRange.first, temporalRange.second );
    ui->mDataVizMap->setTimeStep( timeStep );
    ui->mDataVizMap->addRenderedDataObject( mCurrentRainfall.get() );
    ui->mDataVizMap->showExtentOnMap( mCurrentRainfall->rasterExtent() );
    ui->mDataVizMap->setExtent( mCurrentRainfall->rasterExtent() );
    ui->mShowExtentOnMainButton->setExtent( mCurrentRainfall->rasterExtent() );
    ui->mShowExtentOnMainButton->setEnabled( true );
  }
  else
  {
    ui->mDataVizMap->hideExtentOnMap();
    ui->mShowExtentOnMainButton->setExtent( ReosMapExtent() );
    ui->mShowExtentOnMainButton->setEnabled( false );
  }
}

void ReosGriddedRainfallSelectorWidget::onFileRemoteChanged()
{
  bool fileSelected = ui->mFileRadioButton->isChecked();
  ui->mFileLineEditLabel->setVisible( fileSelected );
  ui->mPathToolButton->setVisible( fileSelected );
  ui->mPathLineEdit->setVisible( fileSelected );
  ui->mNotificationButton->setVisible( fileSelected );

  ui->mRemoteSourceLabel->setVisible( !fileSelected );
  ui->mRemoteCombobox->setVisible( !fileSelected );
  ui->mRemoteNotificationButton->setVisible( !fileSelected );

  if ( fileSelected )
    onPathChanged();
  else
    onRemoteSourceChanged();
}

void ReosGriddedRainfallSelectorWidget::onRemoteSourceChanged()
{
  if ( !ui->mRemoteRadioButton->isChecked() )
    return;

  ReosModule::Message message;
  ui->mRemoteNotificationButton->setMessage( message );

  ui->mRemoteNotificationButton->setVisible( false );
  mCurrentSourceIsValid = false;

  if ( mProviderSelectorWidget )
  {
    delete mProviderSelectorWidget;
    mProviderSelectorWidget = nullptr;
  }

  const QString currentKey = ui->mRemoteCombobox->currentData().toString();

  mProviderSelectorWidget = qobject_cast<ReosGriddedRainDataProviderSelectorWidget *>(
                              ReosDataProviderGuiRegistery::instance()->createProviderSelectorWidget(
                                currentKey,
                                ReosGriddedRainfall::staticType(),
                                ui->mDataVizMap->map(),
                                this ) );

  if ( mProviderSelectorWidget )
  {
    mDetails = mProviderSelectorWidget->setSource( ui->mPathLineEdit->text(), message );

    connect( mProviderSelectorWidget, &ReosGriddedRainDataProviderSelectorWidget::dataSelectionChanged,
             this, &ReosGriddedRainfallSelectorWidget::updateRainfall );

    connect( mProviderSelectorWidget, &ReosGriddedRainDataProviderSelectorWidget::dataSelectionChanged,
             this, &ReosGriddedRainfallSelectorWidget::dataSelectionChanged );

    connect( mProviderSelectorWidget, &ReosGriddedRainDataProviderSelectorWidget::dataSelectionChanged, this, [this]
    {
      ui->mNameLineEdit->setText( giveName() );
    } );

    ui->mProviderLayout->addWidget( mProviderSelectorWidget );
  }

  ui->mNameLineEdit->setText( giveName() );
}

QString ReosGriddedRainfallSelectorWidget::giveName() const
{
  QString name = mDetails.deducedName;

  if ( name.isEmpty() && mProviderSelectorWidget )
  {
    name = mProviderSelectorWidget->dataName();
  }

  if ( name.isEmpty() )
  {
    auto baseName = []( const QString & string )->QString
    {
      QFileInfo fileInfo( string );
      return fileInfo.baseName();
    };
    const QStringList &files = mDetails.files;

    if ( files.isEmpty() )
      return QString();

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

ReosGriddedRainfall *ReosGriddedRainDataProviderSelectorWidget::createData( QObject *parent ) const
{
  return qobject_cast<ReosGriddedRainfall *>( ReosDataProviderSelectorWidget::createData( parent ) );
}
