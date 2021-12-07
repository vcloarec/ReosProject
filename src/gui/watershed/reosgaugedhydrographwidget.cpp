/***************************************************************************
  reosgaugedhydrographwidget.cpp - ReosGaugedHydrographWidget

 ---------------------
 begin                : 24.10.2021
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
#include "reosgaugedhydrographwidget.h"
#include "ui_reosgaugedhydrographwidget.h"

#include <QMessageBox>

#include "reosmap.h"
#include "reoswatershed.h"
#include "reoshydrograph.h"
#include "reosformwidget.h"
#include "reoshydrographeditingwidget.h"
#include "reosplottimeconstantinterval.h"
#include "reosdataprovidergui.h"

ReosGaugedHydrographWidget::ReosGaugedHydrographWidget( ReosMap *map, QWidget *parent )
  : ReosActionWidget( parent )
  , ui( new Ui::ReosGaugedHydrographWidget )
  , mMap( map )
{
  ui->setupUi( this );
  setWindowFlag( Qt::Dialog );

  ReosFormWidgetFactories::instance()->addDataWidgetFactory( new ReosHydrographEditingWidgetFactory );

  ui->mWidgetToolBar->setLayout( new QHBoxLayout );
  ui->mWidgetToolBar->layout()->setContentsMargins( 0, 0, 0, 0 );
  QToolBar *toolBar = new QToolBar( ui->mWidgetToolBar );
  toolBar->setIconSize( QSize( 16, 16 ) );
  toolBar->layout()->setContentsMargins( 0, 0, 0, 0 );
  ui->mWidgetToolBar->layout()->addWidget( toolBar );

  mActionAddHydrograph = toolBar->addAction( QPixmap( QStringLiteral( ":/images/add.svg" ) ), tr( "Add Gauged Hydrograph" ), this, &ReosGaugedHydrographWidget::onAddHydrograph );
  mActionDeleteHydrograph = toolBar->addAction( QPixmap( QStringLiteral( ":/images/remove.svg" ) ), tr( "Delete Current Hydrograph" ), this, &ReosGaugedHydrographWidget::onRemoveHydrograph );
  mActionRenameHydrograph = toolBar->addAction( QPixmap( QStringLiteral( ":/images/rename.svg" ) ), tr( "Rename Current Hydrograph" ), this, &ReosGaugedHydrographWidget::onRenameHydrograph );

  ui->mWidgetProviderToolBar->setLayout( new QHBoxLayout );
  ui->mWidgetProviderToolBar->layout()->setContentsMargins( 0, 0, 0, 0 );
  mToolBarProvider = new QToolBar( ui->mWidgetProviderToolBar );
  mToolBarProvider->setIconSize( QSize( 24, 24 ) );
  mToolBarProvider->layout()->setContentsMargins( 0, 0, 0, 0 );
  ui->mWidgetProviderToolBar->layout()->addWidget( mToolBarProvider );

  mHydrographPlot = new ReosPlotTimeSerieVariableStep( tr( "Hydrograph" ) );
  ui->plotWidget->addPlotItem( mHydrographPlot );
  ui->plotWidget->setAxeXType( ReosPlotWidget::temporal );
  ui->plotWidget->enableAutoMinimumSize( true );
  ui->plotWidget->setMagnifierType( ReosPlotWidget::positiveMagnifier );
  ui->plotWidget->setLegendEnabled( false );
  ui->plotWidget->setTitleAxeYLeft( tr( "Flow rate (%1)" ).arg( QString( "m%1/s" ).arg( QChar( 0x00B3 ) ) ) );

  populateProviderActions();

  connect( ui->mComboBoxHydrographName, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, &ReosGaugedHydrographWidget::onCurrentHydrographChanged );

  connect( ui->mButtonBack, &QPushButton::clicked, this, [this]
  {
    backToMainIndex();
  } );

  connect( ui->mButtonAddFromProvider, &QPushButton::clicked, this, [this]
  {
    if ( !mCurrentDataSelectorWidget )
      return;
    std::unique_ptr<ReosHydrograph> hyd;
    hyd.reset( qobject_cast<ReosHydrograph *>( mCurrentDataSelectorWidget->createData() ) );
    if ( !hyd )
      return;
    mHydrographStore->addHydrograph( hyd.release() );
    onStoreChanged();
    backToMainIndex();
    ui->mComboBoxHydrographName->setCurrentIndex( ui->mComboBoxHydrographName->count() - 1 );
  } );

  connect( ui->mButtonCopyFromProvider, &QPushButton::clicked, this, [this]
  {
    if ( !mCurrentDataSelectorWidget )
      return;
    std::unique_ptr<ReosHydrograph> copyHyd = std::make_unique<ReosHydrograph>();

    ReosHydrograph *providerHydrograph = qobject_cast<ReosHydrograph *>( mCurrentDataSelectorWidget->selectedData() );

    if ( providerHydrograph )
    {
      copyHyd->setName( providerHydrograph->name() );
      copyHyd->copyFrom( providerHydrograph );
      copyHyd->setColor( providerHydrograph->color() );
      mHydrographStore->addHydrograph( copyHyd.release() );
      onStoreChanged();
      ui->mComboBoxHydrographName->setCurrentIndex( ui->mComboBoxHydrographName->count() - 1 );
    }

    backToMainIndex();
  } );

  onStoreChanged();
  ui->mStackedWidget->setCurrentIndex( 0 );

}

ReosGaugedHydrographWidget::~ReosGaugedHydrographWidget()
{
  delete ui;
}

void ReosGaugedHydrographWidget::setCurrentWatershed( ReosWatershed *watershed )
{
  mCurrentWatershed = watershed;

  if ( watershed )
  {
    mHydrographStore = watershed->gaugedHydrographs();
    ui->mLabelNoWatershed->setText( "" );
  }
  else
  {
    mHydrographStore = nullptr;
    ui->mLabelNoWatershed->setText( tr( "Select a watershed to add hydrograhs" ) );
    mHydrographPlot->setTimeSerie( nullptr );
  }

  ui->mButtonAddFromProvider->setEnabled( watershed != nullptr && mIsDatasetSelected );
  ui->mButtonCopyFromProvider->setEnabled( watershed != nullptr && mIsDataReady );

  onStoreChanged();
  onCurrentHydrographChanged();
}

void ReosGaugedHydrographWidget::onAddHydrograph()
{
  ReosFormDialog *dial = new ReosFormDialog( this );
  dial->setWindowTitle( tr( "Add Hydrograph" ) );
  dial->addText( tr( "Add a new hydrograph, choose a name:" ) );
  ReosParameterString nameParam( tr( "Hydrograph name" ) );
  dial->addParameter( &nameParam );

  if ( dial->exec() )
  {
    std::unique_ptr<ReosHydrograph> newHydrograph = std::make_unique<ReosHydrograph>();
    newHydrograph->setName( nameParam.value() );
    newHydrograph->setColor( Qt::blue );
    mHydrographStore->addHydrograph( newHydrograph.release() );
    onStoreChanged();
    ui->mComboBoxHydrographName->setCurrentIndex( ui->mComboBoxHydrographName->count() - 1 );
  }

  dial->deleteLater();
}

void ReosGaugedHydrographWidget::onRemoveHydrograph()
{
  if ( !mHydrographStore )
    return;

  int currentIndex = ui->mComboBoxHydrographName->currentIndex();
  ReosHydrograph *hydrographToReMove = mHydrographStore->hydrograph( currentIndex );
  if ( !hydrographToReMove )
    return;

  if ( QMessageBox::warning( this, tr( "Remove Gauged Hydrograph" ),
                             tr( "Do you want to remove the hydrograph '%1'?" ).arg( hydrographToReMove->name() ),
                             QMessageBox::Yes | QMessageBox::No, QMessageBox::No ) == QMessageBox::Yes )
  {
    mHydrographStore->removeHydrograph( currentIndex );
    ui->mComboBoxHydrographName->removeItem( currentIndex );
  }

  onStoreChanged();
}

void ReosGaugedHydrographWidget::onRenameHydrograph()
{
  if ( !mHydrographStore )
    return;

  int currentIndex = ui->mComboBoxHydrographName->currentIndex();
  ReosHydrograph *hydrographToRename = mHydrographStore->hydrograph( currentIndex );
  if ( !hydrographToRename )
    return;

  ReosFormDialog *dial = new ReosFormDialog( this );
  dial->setWindowTitle( tr( "Rename Meteo Model" ) );
  dial->addText( tr( "Rename meteorologic model '%1',\nchoose a new name:" ).arg( hydrographToRename->name() ) );
  ReosParameterString nameParam( tr( "Meteorologic Model name" ) );
  nameParam.setValue( hydrographToRename->name() );
  dial->addParameter( &nameParam );

  if ( dial->exec() )
  {
    hydrographToRename->setName( nameParam.value() );
    ui->mComboBoxHydrographName->setItemText( currentIndex, nameParam.value() );
  }

  dial->deleteLater();
}

void ReosGaugedHydrographWidget::onStoreChanged()
{
  ui->mComboBoxHydrographName->clear();
  bool hasHydropraph = false;

  if ( mHydrographStore )
  {
    const QStringList &names = mHydrographStore->hydrographNames();
    ui->mComboBoxHydrographName->addItems( names );
    hasHydropraph = !names.isEmpty();
    mActionAddHydrograph->setEnabled( true );
  }
  else
  {
    mActionAddHydrograph->setEnabled( false );
  }

  mActionDeleteHydrograph->setEnabled( hasHydropraph );
  mActionRenameHydrograph->setEnabled( hasHydropraph );
}

void ReosGaugedHydrographWidget::onCurrentHydrographChanged()
{
  if ( !mCurrentHydrograph.isNull() )
  {
    disconnect( mCurrentHydrograph, &ReosDataObject::dataChanged, this, &ReosGaugedHydrographWidget::updatePlotExtent );
  }

  if ( mHydrographStore && mHydrographStore->hydrographCount() > 0 )
    mCurrentHydrograph = mHydrographStore->hydrograph( ui->mComboBoxHydrographName->currentIndex() );
  else
    mCurrentHydrograph = nullptr;

  std::unique_ptr<QWidget> newEditingWidget;
  std::unique_ptr<QWidget> newSettingsProviderWidget;

  if ( !mCurrentHydrograph.isNull() )
  {
    newSettingsProviderWidget.reset(
      ReosDataProviderGuiRegistery::instance()->createProviderSettingsWidget( mCurrentHydrograph->dataProvider() ) );
    newEditingWidget.reset( ReosFormWidgetFactories::instance()->createDataFormWidget( mCurrentHydrograph ) );
    connect( mCurrentHydrograph, &ReosDataObject::dataChanged, this, &ReosGaugedHydrographWidget::updatePlotExtent );
  }
  else
  {
    newEditingWidget.reset( new QLabel( tr( "No Hydrograph" ) ) );
  }

  mHydrographPlot->setTimeSerie( mCurrentHydrograph );
  ui->plotWidget->updatePlot();

  if ( ui->mEditingWidgetLayout->count() != 0 )
  {
    ui->mEditingWidgetLayout->removeWidget( mCurrenEditingWidget );
    delete mCurrenEditingWidget;
    ui->mEditingWidgetLayout->removeWidget( mCurrentProviderSettingsWidget );
    delete mCurrentProviderSettingsWidget;
  }

  mCurrentProviderSettingsWidget = newSettingsProviderWidget.release();
  if ( mCurrentProviderSettingsWidget )
    ui->mEditingWidgetLayout->addWidget( mCurrentProviderSettingsWidget );

  mCurrenEditingWidget = newEditingWidget.release();
  ui->mEditingWidgetLayout->addWidget( mCurrenEditingWidget );

  ui->mEditingWidgetLayout->setStretch( ui->mEditingWidgetLayout->count() - 1, 1 );
}

void ReosGaugedHydrographWidget::updatePlotExtent()
{
  if ( mCurrentHydrograph.isNull() )
    return;

  const QPair<QDateTime, QDateTime> timeExtent = mCurrentHydrograph->timeExtent();

  ui->plotWidget->setAxeXExtent( timeExtent.first, timeExtent.second );
}

void ReosGaugedHydrographWidget::populateProviderActions()
{
  const QString dataType = ReosHydrograph::staticType();

  const QStringList providers =
    ReosDataProviderGuiRegistery::instance()->providers( dataType, ReosDataProviderGuiFactory::GuiCapability::DataSelector );

  for ( const QString &provider : providers )
  {
    QAction *action = new QAction(
      ReosDataProviderGuiRegistery::instance()->providerIcon( provider ),
      ReosDataProviderGuiRegistery::instance()->providerDisplayText( provider ), this );
    mProvidersActionToKeys.insert( action, provider );
    mToolBarProvider->addAction( action );

    connect( action, &QAction::triggered, this, [this, provider]
    {
      showProviderSelector( provider );
    } );
  }

}

void ReosGaugedHydrographWidget::showProviderSelector( const QString &providerKey )
{
  const QString dataType = ReosHydrograph::staticType();

  mCurrentDataSelectorWidget =
    ReosDataProviderGuiRegistery::instance()->createProviderSelectorWidget( providerKey, dataType, mMap, this );

  if ( mCurrentDataSelectorWidget )
  {
    ui->mProviderWidget->layout()->addWidget( mCurrentDataSelectorWidget );
    ui->mProviderHeaderSpacer->changeSize( ui->mProviderHeaderSpacer->geometry().width(),
                                           ui->mainHeaderLayout->sizeHint().height(),
                                           QSizePolicy::Expanding );
    ui->mStackedWidget->setCurrentIndex( 1 );

    ui->mButtonCopyFromProvider->setEnabled( false );
    ui->mButtonAddFromProvider->setEnabled( false );
    // connect selection change
    connect( mCurrentDataSelectorWidget, &ReosDataProviderSelectorWidget::dataSelectionChanged, this, [this]( bool isDataSelected )
    {
      mIsDatasetSelected = isDataSelected;
      ui->mButtonAddFromProvider->setEnabled( isDataSelected  && mCurrentWatershed );
      if ( !isDataSelected )
      {
        mIsDataReady = false;
        ui->mButtonAddFromProvider->setEnabled( false );
        ui->mButtonCopyFromProvider->setEnabled( false );
      }
    } );

    // connect loading
    connect( mCurrentDataSelectorWidget, &ReosDataProviderSelectorWidget::dataIsLoading, this, [this]
    {
      mIsDataReady = false;
      ui->mButtonCopyFromProvider->setEnabled( false );
    } );

    // connect data is ready
    connect( mCurrentDataSelectorWidget, &ReosDataProviderSelectorWidget::dataIsReady, this, [this]
    {
      mIsDataReady = true;
      ui->mButtonCopyFromProvider->setEnabled( mCurrentWatershed != nullptr );
    } );

    mCurrentDataSelectorWidget->onOpened();
  }
}

void ReosGaugedHydrographWidget::backToMainIndex()
{
  ui->mStackedWidget->setCurrentIndex( 0 );
  ui->mProviderWidget->layout()->removeWidget( mCurrentDataSelectorWidget );
  mCurrentDataSelectorWidget->deleteLater();
  mCurrentDataSelectorWidget = nullptr;
}
